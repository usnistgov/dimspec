full_import <- function(import_object                  = NULL,
                        file_name                      = NULL,
                        db_conn                        = con,
                        ignore_missing_required        = FALSE,
                        include_if_missing_recommended = FALSE,
                        ignore_extra                   = TRUE,
                        requirements_obj               = "import_requirements",
                        sample_info_in                 = "sample",
                        method_info_in                 = "massspectrometry",
                        chrom_info_in                  = "chromatography",
                        mobile_phases_in               = "chromatography",
                        import_map                     = IMPORT_MAP,
                        log_ns                         = "db") {
  # Check connection
  stopifnot(active_connection(db_conn))
  log_fn("start")
  if (all(is.null(file_name), is.null(obj))) {
    stop('One of either "file_name" or "obj" must be provided.')
  }
  if (is.null(obj)) {
    if (all(file.exists(file_name), grepl(".json", file_name))) {
      import_object <- jsonlite::read_json(file_name)
    }
    import_type <- "file"
  } else {
    import_object <- obj
    import_type <- "object"
  }
  log_it("trace", glue::glue('Verifying import requirements with verify_import_requirements().'), log_ns)
  meets_requirements <- verify_import_requirements(
    obj = import_object,
    requirements_obj = requirements_obj,
    ignore_extra = ignore_extra,
    log_issues_as = "trace"
  )
  if (sample_info_in %in% names(import_object)) import_object <- list(import_object)
  to_ignore <- integer(0)
  if (all(meets_requirements$has_all_required)) {
    log_it("success",
           sprintf("Import %s%s meets all import requirements.",
                   import_type,
                   ifelse(nrow(meets_requirements) > 1, "s", "")),
           log_ns)
  } else {
    missing_required <- meets_requirements %>%
      select(import_object, has_all_required, missing_requirements) %>%
      filter(!has_all_required) %>%
      group_by(import_object) %>%
      unnest(cols = c(missing_requirements)) %>%
      mutate(req_msg = stringr::str_c('Required element',
                                      ifelse(length(missing_requirements) > 1, "s ", " "),
                                      format_list_of_names(missing_requirements, resolve_quotes = TRUE),
                                      ifelse(length(missing_requirements) > 1, " were", " was"),
                                      ' missing from import ',
                                      import_type,
                                      ' "',
                                      import_object,
                                      '". ')) %>%
      distinct(import_object, req_msg)
    for (x in missing_required$req_msg) log_it("error", msg = x, "db")
    if (ignore_missing_required) {
      to_ignore <- c(to_ignore, which(!meets_requirements$has_all_required))
      log_it("warn",
             sprintf('The import will ignore %s %s%s (%s) which did not contain all required information.',
                     nrow(missing_required),
                     import_type,
                     ifelse(nrow(missing_required) > 1, "s", ""),
                     format_list_of_names(missing_required$import_object, resolve_quotes = TRUE)
             ),
             log_ns)
    } else {
      log_it("error",
             sprintf('The import %s must contain all required information defined in "%s" because ignore_missing_required = FALSE.',
                     import_type,
                     requirements_obj),
             log_ns)
      return(invisible(NULL))
    }
  }
  if (all(meets_requirements$has_full_detail)) {
    log_it("success",
           sprintf("Import %s%s contains all expected detail.",
                   import_type,
                   ifelse(nrow(meets_requirements) > 1, "s", "")),
           log_ns)
  } else {
    missing_recommended <- meets_requirements %>%
      select(import_object, has_full_detail, missing_detail) %>%
      filter(!has_full_detail) %>%
      group_by(import_object) %>%
      unnest(cols = c(missing_detail)) %>%
      mutate(recc_msg = stringr::str_c('Recommended element',
                                       ifelse(length(missing_detail) > 1, "s ", " "),
                                       format_list_of_names(missing_detail, resolve_quotes = TRUE),
                                       ifelse(length(missing_detail) > 1, " were", " was"),
                                       ' missing from import ',
                                       import_type,
                                       ' "',
                                       import_object,
                                       '".')) %>%
      distinct(import_object, recc_msg)
    for (x in missing_recommended$recc_msg) log_it("error", msg = x, "db")
    log_it("warn",
           sprintf('The import will %s %s %s%s (%s) which did not contain all recommended information because ignore_missing_recommended = %s.',
                   ifelse(include_if_missing_recommended, "include", "ignore"),
                   nrow(missing_recommended),
                   import_type,
                   ifelse(nrow(missing_recommended) > 1, "s", ""),
                   format_list_of_names(missing_recommended$import_object, resolve_quotes = TRUE),
                   include_if_missing_recommended),
           log_ns)
    if (!include_if_missing_recommended) {
      to_ignore <- c(to_ignore, which(!meets_requirements$has_full_detail))
    }
  }
  if (length(to_ignore) > 0) {
    import_object <- import_object[-unique(to_ignore)]
  }
  # Get all unique relationships to cut down on extraneous database rows
  # ____________________________________________________________________________
  # 2022-04-27: This was more easily accomplished by fixing the import map and
  # relying on the database to find fully qualified matches during INSERT using
  # a combination of build_db_action("get_id", ...) and INSERT OR IGNORE INTO
  # ____________________________________________________________________________
  # import_relationships <- vector("list", length(names(import_requirements)))
  # names(import_relationships) <- names(import_requirements)
  # import_relationships <- import_requirements for (ele in
  # names(import_relationships)) { import_relationships[[ele]] <-
  # get_uniques(to_import, ele) } 
  
  # Resolve contributor and build an internal contributor mapping so they don't
  # have to rely on fuzzy matching as the contributor provided in import files
  # may not end up being a value they use in the eventual contributors table
  map_contributors_to <- data.frame(provided = character(0), resolved = integer(0))
  for (i in 1:length(import_object)) {
    obj <- import_object[[i]]
    contributor <- obj[[sample_info_in]]$data_generator
    if (contributor %in% map_contributors_to$provided) {
      contributor_id <- map_contributors_to$resolved[map_contributors_to$provided == contributor]
    } else {
      contributor_id <- resolve_normalization_value(contributor, "contributors")
      if (is.null(contributor_id)) {
        log_it("error",
               "Unable to resolve contributor. Please adjust and try again.",
               log_ns)
        return(invisible(NULL))
      }
      map_contributors_to <- map_contributors_to %>%
        bind_rows(
          data.frame(provided = contributor, resolved = contributor_id)
        )
    }
    # Method info is optional but heavily encouraged 
    if (method_info_in %in% names(obj)) {
      ms_method_id <- resolve_method(obj,
                                     method_in = method_info_in)
    } else {
      ms_method_id <- NA
    }
    if (!is.na(ms_method_id)) {
      # - instrument properties
      tmp <- map_import(import_obj = obj,
                        aspect = "instrument_properties",
                        import_map = import_map)
      tmp <- tibble(
        ms_method_id = ms_method_id,
        name = names(tmp),
        value = unlist(tmp),
        value_unit = case_when(
          name == "isowidth" ~ "Da",
          name == "msaccuracy" ~ "ppm"
        )
      )
      res <- try(
        build_db_action(action = "insert",
                        table_name = "instrument_properties",
                        db_conn = db_conn,
                        values = tmp,
                        # TODO consider removing in production
                        ignore = TRUE)
      )
      # - ms_description
      if (method_info_in %in% names(obj)) {
        resolve_description_NTAMRT(obj = obj,
                                   method_id = ms_method_id,
                                   mass_spec_in = method_info_in,
                                   type = "massspec")
      }
      # - chromatography description
      if (chrom_info_in %in% names(obj)) {
        resolve_description_NTAMRT(obj = obj,
                                   method_id = ms_method_id,
                                   chrom_spec_in = chrom_info_in,
                                   type = "chromatography")
      }
      # - qc_methods
    }
    # Sample info is required
    # Conveniently carry forward resolved contributor
    if ("data_generator" %in% names(obj[[sample_info_in]]) &&
        !is.null(contributor_id) &&
        !is.na(contributor_id) &&
        is.integer(contributor_id) &&
        length(contributor_id) == 1) {
      obj[[sample_info_in]][["data_generator"]] <- build_db_action(
        action = "select",
        table_name = "contributors",
        column_names = "username",
        match_criteria = list(id = contributor_id)
      )
    }
    sample_id <- resolve_sample(obj,
                                sample_in = sample_info_in,
                                method_id = ms_method_id)
    # - mobile phases
    resolve_mobile_phase_NTAMRT(obj = obj,
                                method_id = ms_method_id,
                                sample_id = sample_id)
    # - qc methods
    resolve_qc_methods(obj = obj, method_id = ms_method_id)
    # Add data
  }
}

#' Utility function to add a record
#'
#' Checks a table in the attached SQL connection for a primary key ID matching
#' the provided `values` and returns the ID. If none exists, adds a record and
#' returns the resulting ID if successful. Values should be provided as a named
#' vector of the values to add. No data coercion is performed, relying almost
#' entirely on the database schema or preprocessing to ensure data integrity.
#'
#' Provided values are checked agaisnt required columns in the table using
#' [verify_import_columns].
#'
#' Operations to add the record and get the resulting ID are both performed with
#' [build_db_action] and are performed virtually back to back with the
#' latest-added ID being given preference in cases where added values may match
#' multiple extant records.
#'
#' @note If this is used in high volume/traffic applications, ID conflicts may
#'   occur if the timing is such that another record containing identical values
#'   is added before the call getting the ID completes.
#'
#' @param db_table CHR scalar name of the database table being modified
#' @param values named vector of the values being added, passed to
#'   [build_db_action]
#' @param db_conn connection object (default: con)
#' @param ensure_unique LGL scalar of whether or not to first check that the
#'   values provided form a new unique record (default: TRUE)
#' @param ignore LGL scalar on whether to treat the insert try as an "INSERT OR
#'   IGNORE" SQL statement (default: FALSE)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return
#' @export
add_or_get_id <- function(db_table, values, db_conn = con, ensure_unique = TRUE, ignore = FALSE, log_ns = "db") {
  log_it("info", glue("Adding or identifying a record to {db_table}."), log_ns)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table      = list(c("mode", "character"), c("length", 1)),
        values        = list(c("n>=", 1)),
        db_conn       = list(c("length", 1)),
        ensure_unique = list(c("mode", "logical"), c("length", 1)),
        ignore        = list(c("mode", "logical"), c("length", 1)),
        log_ns        = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "add_or_get_id"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  # Make sure required values are present
  values <- verify_import_columns(
    db_table   = db_table,
    values     = values,
    names_only = FALSE,
    db_conn    = con
  )
  if (ensure_unique) {
    # Check for an existing match
    res_id <- try(
      build_db_action(
        action         = "get_id",
        table_name     = db_table,
        db_conn        = db_conn,
        match_criteria = values,
        and_or         = "AND"
      )
    )
  } else {
    res_id <- integer(0)
  }
  if (inherits(res_id, "try-error")) {
    tmp <- values
    blanks <- tmp %in% c("", "null", "NULL", "NA", "na", NA)
    tmp[!blanks] <- paste0("= ", tmp[!blanks])
    tmp[blanks] <- "is null"
    msg_text <- paste(names(tmp), tmp)
    log_it("error",
           sprintf('Cannot retrieve ID from table "%s" using values:\n\t%s\n',
                   db_table,
                   paste0(msg_text, collapse = "\n\t")
           ),
           log_ns
    )
    stop()
  } else {
    if (length(res_id) > 1) {
      this_id <- tail(res_id, 1)
      log_it("warn", glue("Multiple IDs ({format_list_of_names(res_id)}) match provided values. Using {this_id}."), log_ns)
    } else if (length(res_id) == 1) {
      this_id <- res_id
      log_it("success", glue("Record found in {db_table} as ID {this_id}."), log_ns)
      return(this_id)
    }
  }
  if (length(res_id) == 0) {
    # Try the insertion
    res <- try(
      build_db_action(
        action     = "insert",
        table_name = db_table,
        db_conn    = db_conn,
        values     = values,
        ignore     = ignore
      )
    )
    if (inherits(res, "try-error")) {
      log_it("error", glue('There was an error adding values to table "{db_table}".'), log_ns)
      return(res)
    }
    
    res_id <- try(
      build_db_action(
        action         = "get_id",
        table_name     = db_table,
        db_conn        = db_conn,
        match_criteria = values,
        and_or         = "AND"
      )
    )
    if (inherits(res_id, "try-error")) {
      tmp <- values
      blanks <- tmp %in% c("", "null", "NULL", "NA", "na", NA)
      tmp[!blanks] <- paste0("= ", tmp[!blanks])
      tmp[blanks] <- "is null"
      msg_text <- paste(names(tmp), tmp)
      log_it("error",
             sprintf('Cannot retrieve ID from table "%s" using values:\n\t%s\n',
                     db_table,
                     paste0(msg_text, collapse = "\n\t")
             ),
             log_ns
      )
      return(res_id)
    } else {
      this_id <- tail(res_id, 1)
      if (length(res_id) > 1) {
        log_it("warn",
               glue("Multiple IDs ({format_list_of_names(res_id)}) match provided values. Using {this_id}."),
               log_ns)
      }
      log_it("success", glue("Record added to {db_table} as ID {this_id}."), log_ns)
      return(this_id)
    }
  }
}

#' Import software settings
#'
#' Part of the standard import pipeline, adding rows to the
#' `conversion_software_settings` table with a given sample id.
#'
#' @param obj CHR vector describing settings or a named LIST with names matching
#'   column names in table conversion_software_settings.
#' @param timestamp CHR scalar of the sample timestamp (e.g. sample$starttime)
#'   to use for linking software conversion settings with peak data, with a call
#'   back to the originating sample. If NULL (the default), the current system
#'   timestamp in UTC will be used from [lubridate::now].
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#' @param ... Other named elements to be appended to records in the
#'   conversion_software_settings as necessary for workflow resolution, can be
#'   used to pass defaults or additional values.
#'
#' @return status of the insertion
#' @export
resolve_software_settings <- function(obj,
                                      sample_timestamp = NULL,
                                      db_conn = con,
                                      software_settings_name = "msconvertsettings",
                                      db_table = "conversion_software_settings",
                                      log_ns = "db",
                                      ...) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, db_conn, software_settings_name, db_table, log_ns),
      conditions = list(
        obj                    = list(c("mode", "list")),
        db_conn                = list(c("length", 1)),
        software_settings_name = list(c("mode", "character"), c("length", 1)),
        db_table               = list(c("mode", "character"), c("length", 1)),
        log_ns                 = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (software_settings_name %in% names(obj)) {
    obj <- obj[[software_settings_name]]
  } else {
    msg <- sprintf('"%s" not found in the namespace of this object. Using obj directly.', software_settings_name)
    log_it("warn", msg)
  }
  obj <- tack_on(obj, ...)
  # Add entries to conversion_software_peaks_linkage
  if (is.null(sample_timestamp)) {
    sample_timestamp <- format(
      lubridate::now("UTC"),
      "%Y-%m-%d %H:%M:%S"
    )
  } else {
    
  }
  link_table <- "conversion_software_peaks_linkage"
  res <- try(
    build_db_action(
      action     = "insert",
      table_name = link_table,
      values     = list(generated_on = sample_timestamp)
    )
  )
  if (inherits(res, "try-error")) {
    log_it("error" , glue::glue("There was a problem adding records to table {link_table}."), log_ns)
    return(res)
  } else {
    linkage_id <- build_db_action(
      action = "get_id",
      table_name = link_table,
      match_criteria = list(generated_on = sample_timestamp)
    )
    if (length(linkage_id) > 1) {
      log_it("info", glue::glue("Links to samples with time stamp {sample_timestamp} already exist."), log_ns)
      linkage_id <- tail(linkage_id, 1)
    }
    log_it("success", glue::glue("Record added to {link_table} as id {linkage_id}."), log_ns)
  }
  # Add entries to conversion_software_settings
  if (is.vector(obj)) {
    values <- lapply(
      obj,
      function(x) {
        setNames(c(linkage_id, x), c("linkage_id", "setting_value"))
      }
    )
  }
  res <- try(
    build_db_action(con,
                    action = "INSERT",
                    table_name = "conversion_software_settings",
                    values = values)
  )
  if (inherits(res, "try-error")) {
    log_it("error" , glue::glue("There was a problem adding records to table {db_table}."), log_ns)
    return(res)
  } else {
    log_it("success", glue::glue("Records added to {db_table}."), log_ns)
  }
  # Feed linkage id to resolve_peaks
  return(linkage_id)
}

#' Add a sample via import
#'
#' Part of the data import routine. Adds a record to the "samples" table with
#' the values provided in the JSON import template. Uses [verify_sample_class]
#' and [verify_contributor] to parse foreign key relationships, [resolve_method]
#' to add a record to ms_methods to get the proper id, and
#' [resolve_software_settings] to insert records into and get the proper
#' conversion software linkage id from tables "conversion_software_settings" and
#' "conversion_software_linkage" if appropriate.
#' 
#' @note This function is called as part of [full_import]
#'
#' @inheritParams add_or_get_id
#' @inheritParams map_import
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_id INT scalar of the associated ms_methods record id
#' @param generation_type CHR scalar of the type of data generated for this
#'   sample (e.g. default: "empirical" or "in silico")
#' @param sample_in CHR scalar of the import object name storing sample data
#'   (default: "sample")
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#' @param ... Other named elements to be appended to samples as necessary for
#'   workflow resolution, can be used to pass defaults or additional values.
#'
#' @return INT scalar if successful, result of the call to [add_or_get_id]
#'   otherwise
resolve_sample <- function(obj,
                           db_conn = con,
                           method_id = NULL,
                           generation_type = "empirical",
                           sample_in = "sample",
                           db_table = "samples",
                           import_map = IMPORT_MAP,
                           ensure_unique = TRUE,
                           log_ns = "db",
                           ...) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, db_conn, sample_in, db_table, ensure_unique, log_ns),
      conditions = list(
        obj                  = list(c("n>=", 1)),
        db_conn              = list(c("length", 1)),
        sample_in            = list(c("mode", "character"), c("length", 1)),
        db_table             = list(c("mode", "character"), c("length", 1)),
        ensure_unique        = list(c("mode", "logical"), c("length", 1)),
        log_ns               = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (!is.null(method_id)) stopifnot(method_id == as.integer(method_id), length(method_id) == 1)
  log_fn("start")
  log_it("info", "Preparing sample import.", log_ns)
  sample_values <- map_import(
    import_obj = obj,
    aspect = db_table,
    import_map = IMPORT_MAP
  ) %>%
    tack_on(
      ms_methods_id = method_id,
      generation_type = resolve_normalization_value(this_value = generation_type,
                                                    db_table = "norm_generation_type",
                                                    db_conn = db_conn,
                                                    log_ns = log_ns)
    ) %>%
    tack_on(...)
  # Ensure that starttime is in the datetime format required by the database
  starttime <- sample_values$generated_on
  if (!any(starttime == "", is.null(starttime), is.na(starttime))) {
    starttime <- starttime %>%
      lubridate::as_datetime() %>%
      format("%Y-%m-%d %H:%M:%S")
    sample_values$generated_on <- starttime
  }
  sample_values <- sample_values %>%
    verify_import_columns(db_table, db_conn = db_conn)
  is_single_record <- all(unlist(lapply(sample_values, length)) < 2)
  log_it("info", glue("Sample value lengths {ifelse(is_single_record, 'are', 'are not')} appropriate."), log_ns)
  if (!is_single_record) {
    stop("Resolve and try again.")
  }
  sample_id <- try(
    add_or_get_id(
      db_table = "samples",
      values   = sample_values,
      db_conn  = db_conn,
      ignore   = FALSE,
      ensure_unique = ensure_unique
    )
  )
  if (inherits(sample_id, "try-error")) {
    log_it("error", "There was an issue resolving the sample record.", log_ns)
    sample_id <- NULL
  }
  return(sample_id)
}

#' Add an ms_method record via import
#'
#' Part of the data import routine. Adds a record to the "ms_methods" table with
#' the values provided in the JSON import template. Makes extensive uses of
#' [resolve_normalization_value] to parse foreign key relationships.
#' 
#' @note This function is called as part of [full_import]
#' 
#' @inheritParams add_or_get_id
#' @inheritParams qc_result
#' @inheritParams map_import
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_in CHR scalar name of the `obj` list containing method
#'   information
#' @param db_table CHR scalar name of the database table containing method
#'   information
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#' @param ... Other named elements to be appended to "ms_methods" as necessary
#'   for workflow resolution, can be used to pass defaults or additional values.
#'
#' @return INT scalar if successful, result of the call to [add_or_get_id]
#'   otherwise
#'   
resolve_method <- function(obj,
                           method_in = "massspectrometry",
                           db_table = "ms_methods",
                           db_conn = con,
                           ensure_unique = TRUE,
                           log_ns = "db",
                           qc_in = "qc",
                           qc_method_in = "qcmethod",
                           search_text_in = "name",
                           search_text = "QC Method Used",
                           value_in = "value",
                           import_map = IMPORT_MAP,
                           ...) {
  # Check connection
  stopifnot(active_connection(db_conn))
  log_fn("start")
  log_it("trace", "Preparing method import.", log_ns)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, method_in, db_table, db_conn, ensure_unique, log_ns),
      conditions = list(
        obj           = list(c("n>=", 1)),
        method_in     = list(c("mode", "character"), c("length", 1)),
        db_table      = list(c("mode", "character"), c("length", 1)),
        db_conn       = list(c("length", 1)),
        ensure_unique = list(c("mode", "logical"), c("length", 1)),
        log_ns        = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  
  obj_method <- get_component(obj, method_in, ...)[[1]]
  
  log_it("trace", "Building methods entry values...", "db")
  # Accelerate this portion
  argument_verification <- VERIFY_ARGUMENTS
  if (argument_verification) {
    assign("VERIFY_ARGUMENTS", FALSE, envir = .GlobalEnv)
  }
  log_it("info", "Preparing sample import.", log_ns)
  ms_method_values <- map_import(
    import_obj = obj,
    aspect = db_table,
    import_map = import_map,
    log_ns = log_ns
  ) %>%
    tack_on(
      has_qc_method = qc_result(
        obj = obj,
        qc_in = qc_in,
        qc_method_in = qc_method_in,
        search_text_in = search_text_in,
        search_text = search_text,
        value_in = value_in)
    ) %>%
    tack_on(...) %>%
    verify_import_columns(db_table = db_table,
                          db_conn = db_conn,
                          log_ns = log_ns)
  is_single_record <- all(unlist(lapply(ms_method_values, length)) < 2)
  log_it("trace", glue("Mass spectromtery value lengths {ifelse(is_single_record, 'are', 'are not')} appropriate."), log_ns)
  if (!is_single_record) {
    stop("Resolve and try again.")
  }
  if (argument_verification) {
    assign("VERIFY_ARGUMENTS", argument_verification, envir = .GlobalEnv)
  }
  # Insert method if appropriate
  ms_method_id <- try(
    add_or_get_id(
      db_table      = "ms_methods",
      values        = ms_method_values,
      db_conn       = db_conn,
      ensure_unique = ensure_unique
    )
  )
  if (inherits(ms_method_id, 'try-error')) {
    resolve_description <- sprintf("%s = %s", names(ms_method_values), ms_method_values)
    log_it("warn", glue('Unable to add or locate values ({format_list_of_names(resolve_description)}) in table "ms_methods".'), "db")
  } else {
    log_it("success", glue::glue("Using ID {ms_method_id} from ms_methods."), "db")
  }
  log_fn("end")
  return(ms_method_id)
}

#' Resolve the method description tables during import
#'
#' Two tables (and their associated normalization tables) exist in the database
#' to store additional information about mass spectrometric and chromatographic
#' methods. These tables are "ms_descriptions" and "chromatography_descriptions"
#' and cannot be easily mapped directly. This function serves to coerce values
#' supplied during import into that required by the database. Primarily, the
#' issue rests in the need to support multiple descriptions of analytical
#' instrumentation (e.g. multiple mass analyzer types, multiple vendors,
#' multiple separation columns, etc.). Tables targeted by this function are
#' "long" tables that may well have `n` records for each mass spectrometric
#' method.
#'
#' @note This function is called as part of [full_import]
#' @note This function is brittle; built specifically for the NIST NTA MRT
#'   import format. If using a different import format, customize to your needs
#'   using this function as a guide.
#'
#' @inheritParams get_component
#' @inheritParams build_db_action
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_id INT scalar of the ms_method.id record to associate
#' @param type CHR scalar, one of "massspec" or "chromatography" depending on
#'   the type of description to add; much of the logic is shared, only details
#'   differ
#'
#' @return
#' @export
#'
resolve_description_NTAMRT <- function(obj,
                                       method_id,
                                       type = c("massspec", "chromatography"),
                                       mass_spec_in = "massspectrometry",
                                       chrom_spec_in = "chromatography",
                                       db_conn = con,
                                       fuzzy = TRUE,
                                       log_ns = "db") {
  # Check connection
  stopifnot(active_connection(db_conn))
  log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, method_id, type, mass_spec_in, chrom_spec_in, db_conn, fuzzy, log_ns),
      conditions = list(
        obj           = list(c("n>=", 1)),
        method_id     = list(c("mode", "numeric"), c("length", 1)),
        type          = list(c("mode", "character"), c("length", 1)),
        mass_spec_in  = list(c("mode", "character"), c("length", 1)),
        chrom_spec_in = list(c("mode", "character"), c("length", 1)),
        db_conn       = list(c("length", 1)),
        fuzzy         = list(c("mode", "logical"), c("length", 1)),
        log_ns        = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  
  type <- match.arg(type)
  method_id_check <- check_for_value(method_id, "ms_methods", "id")$exists
  if (!method_id_check) {
    stop(glue::glue("Method ID {method_id} does not yet exist."))
  }
  if (type == "massspec") {
    if (mass_spec_in %in% names(obj)) {
      obj <- get_component(obj, mass_spec_in)[[1]]
    } else {
      log_it("warn",
             glue::glue("Could not find {mass_spec_in} in the names of this object. Using directly."),
             log_ns)
    }
    table_name <- "ms_descriptions"
    values <- tibble(
      ms_methods_id = method_id,
      ms_types_id = sapply(
        obj[grep("massanalyzer", names(obj), value = TRUE)],
        resolve_normalization_value,
        db_table = "norm_ms_types"
      ),
      vendor_id = sapply(
        obj[grep("vendor", names(obj), value = TRUE)],
        resolve_normalization_value,
        db_table = "norm_vendors"
      ),
      vendor_model = unlist(obj[grep("model", names(obj), value = TRUE)]),
      reference = unlist(obj[grep("source", names(obj), value = TRUE)])
    )
  } else if (type == "chromatography") {
    if (chrom_spec_in %in% names(obj)) {
      obj <- get_component(obj, chrom_spec_in)[[1]]
    } else {
      log_it("warn",
             glue::glue("Could not find {chrom_spec_in} in the names of this object. Using directly."),
             log_ns)
    }
    table_name <- "chromatography_descriptions"
    values <- tibble(
      ms_methods_id = method_id,
      chromatography_types_id = resolve_normalization_value(
        obj$ctype,
        "norm_chromatography_types"
      ),
      system_vendor_id = sapply(
        obj[grep("cvendor", names(obj), value = TRUE)],
        resolve_normalization_value,
        db_table = "norm_vendors"
      ),
      system_vendor_model = unlist(obj[grep("cmodel", names(obj), value = TRUE)]),
      column_chemistry_id = sapply(
        obj[grep("colchemistry", names(obj), value = TRUE)],
        function(x) {
          if (x == "none") {
            ""
          } else {
            resolve_normalization_value(
              x,
              db_table = "norm_column_chemistries",
              fuzzy = fuzzy
            )
          }
        }
      ),
      column_position_id = sapply(c("guard", "analytical"),
                                  resolve_normalization_value,
                                  db_table = "norm_column_positions"),
      column_vendor_id = sapply(
        obj[grep("colvendor", names(obj), value = TRUE)],
        resolve_normalization_value,
        db_table = "norm_vendors"
      ),
      column_vendor_model = unlist(obj[grep("name", names(obj), value = TRUE)]),
      internal_diameter = unlist(obj[grep("colid", names(obj), value = TRUE)]),
      column_length = unlist(obj[grep("collen", names(obj), value = TRUE)]),
      particle_diameter = unlist(obj[grep("coldp", names(obj), value = TRUE)]),
      citation = unlist(obj[grep("source", names(obj), value = TRUE)])
    ) %>%
      filter(!column_chemistry_id == "")
  }
  log_it("info", glue::glue('Adding descriptions to table "{table_name}".'), log_ns)
  res <- try(
    build_db_action(
      action = "insert",
      table_name = table_name,
      db_conn = db_conn,
      values = values,
      # TODO consider removing during production
      ignore = TRUE
    )
  )
  if (inherits(res, "try-error")) {
    stop(glue('There was an issue adding records to table "{table_name}".'))
  } else {
    log_it("success", glue::glue('Added {type} description to table "{table_name}".'), log_ns)
  }
  log_fn("end")
}

#' Resolve the mobile phase node
#'
#' The database node containing chromatographic method information is able to
#' handle any number of descriptive aspects regarding chromatography. It houses
#' normalized and aliased data in a manner that maximizes flexibility, allowing
#' any number of carrier agents (e.g. gasses for GC, solvents for LC) to be
#' described in increasing detail. To accomodate that, the structure itself may
#' be unintuitive and may not map well as records may be heavily nested.
#'
#' The mobile phase node contains one record in table "mobile_phases" for each
#' method id, sample id, and carrier mix collection id with its associated flow
#' rate, normalized flow units, duration, and normalized duration units. Each
#' carrier mix collection has a name and child tables containing: records for
#' each value normalized carrier component and its unit fraction (e.g. in
#' carrier_mixes: Helium 1 would indicate pure Helium as a carrier gas in GC
#' work; Water, 0.9; Methanol, 0.1 to indicate a solvent mixture of 10% methanol
#' in water), as well as value normalized carrier additives, their amount, and
#' the units for that amount (mostly for LC work; e.g. in carrier_additives:
#' ammonium acetate, 5, mMol to indicate an additive to a solvent of 5 mMol
#' ammonium acetate); these are linked through the carrier mix collection id.
#'
#' Call this function to import the results of the NIST Non-Targeted Analysis
#' Method Reporting Tool (NTA MRT), or feed it as `obj` a flat list containing
#' chromatography information.
#'
#' @note This is a brittle function, and should only be used as part of the NTA
#'   MRT import process, or as a template for how to import data.
#' @note Some arguments are complicated by design to keep conceptual information
#'   together. These should be fed a structured list matching expectations. This
#'   applies to `mobile_phase_props`, `carrier_props`, and `additive_props`. See
#'   defaults in documentation for examples.
#' @note Database insertions are done in real time, so failures may result in
#'   hanging or orphaned records. Turn on `clean_up` to roll back by removing
#'   entries from `mix_collection_table` and relying on delete cascades built
#'   into the database. Additional names are provided here to match the schema.
#' @note This function is called as part of [full_import]
#'
#' @inheritParams build_db_action
#' @inheritParams full_import
#' @inheritParams resolve_normalization_value
#'
#' @param method_id INT scalar of the method id (e.g. from the import workflow)
#' @param sample_id INT scalar of the sample id (e.g. from the import workflow)
#' @param carrier_mix_names CHR vector (optional) of carrier mix collection
#'   names to assign, the length of which should equal 1 or the length of
#'   discrete carrier mixtures; the default, NULL, will automatically assign
#'   names as a function of the method and sample id.
#' @param id_mix_by CHR scalar regex to identify the elements of `obj` to use
#'   for the mobile phase node (default "^mp*[0-9]+") grouping of carrier mix
#'   collections, this is the main piece of connectivity pulling together the
#'   descriptions and should only be changed to match different import naming
#'   schemes
#' @param methods_table CHR scalar name of the methods table (default:
#'   "ms_methods")
#' @param samples_table CHR scalar name of the methods table (default:
#'   "samples")
#' @param db_conn existing connection object (e.g. of class "SQLiteConnection")
#' @param mix_collection_table CHR scalar name of the mix collections table
#'   (default: "carrier_mix_collections")
#' @param mobile_phase_props LIST object describing how to import the mobile
#'   phase table containing: in_item: CHR scalar name of the `obj` name
#'   containing chromatographic information (default: "chromatography");
#'   db_table: CHR scalar name of the mobile phases table (default:
#'   "mobile_phases"); props: named CHR vector of name mappings with names equal
#'   to database columns in `mobile_phase_props$db_table` and values matching
#'   regex to match names in `obj[[mobile_phase_props$in_item]]`
#' @param carrier_props LIST object describing how to import the mobile phase
#'   table containing: db_table: CHR scalar name of the mobile phases table
#'   (default: "mobile_phases"); norm_table: CHR scalar name of the table used
#'   to normalize carriers (default: "norm_carriers"); alias_table: CHR scalar
#'   name of the table containing carrier aliases to search (default:
#'   "carrier_aliases"); props: named CHR vector of name mappings with names
#'   equal to database columns in `carrier_props$db_table` and values matching
#'   regex to match names in `obj[[mobile_phase_props$in_item]]`, and an extra
#'   element named `id_by` containing regex used to match names in the import
#'   object indicate a carrier (e.g. "solvent")
#' @param additive_props LIST object describing how to import the mobile phase
#'   table containing: db_table: CHR scalar name of the mobile phases table
#'   (default: "mobile_phases"); norm_table: CHR scalar name of the table used
#'   to normalize carriers (default: "norm_additives"); alias_table: CHR scalar
#'   name of the table containing carrier aliases to search (default:
#'   "additive_aliases"); props: named CHR vector of name mappings with names
#'   equal to database columns in `additive_props$db_table` and values matching
#'   regex to match names in `obj[[mobile_phase_props$in_item]]`
#'   `obj[[mobile_phase_props$in_item]][[mobile_phase_props$db_table]]`, and an
#'   extra element named `id_by` containing regex used to match names in the
#'   import object indicate an additive (e.g. "add$")
#' @param exclude_values CHR vector indicating which values to ignore in `obj`
#'   (default: c("none", "", NA))
#' @param clean_up
#'
#' @return None, executes actions on the database
#' @export
#' 
resolve_mobile_phase_NTAMRT <- function(obj,
                                        method_id,
                                        sample_id,
                                        carrier_mix_names = NULL,
                                        id_mix_by = "^mp*[0-9]+",
                                        methods_table = "ms_methods",
                                        samples_table = "samples",
                                        db_conn = con,
                                        mix_collection_table = "carrier_mix_collections",
                                        mobile_phase_props = list(
                                          in_item = "chromatography",
                                          db_table = "mobile_phases",
                                          props = c(
                                            flow = "flow",
                                            flow_units = "flowunits",
                                            duration = "duration",
                                            duration_units = "durationunits"
                                          )
                                        ),
                                        carrier_props = list(
                                          db_table = "carrier_mixes",
                                          norm_by = "norm_carriers",
                                          alias_in = "carrier_aliases",
                                          props = c(
                                            id_by = "solvent",
                                            fraction_by = "fraction"
                                          )
                                        ),
                                        additive_props = list(
                                          db_table = "carrier_additives",
                                          norm_by = "norm_additives",
                                          alias_in = "additive_aliases",
                                          props = c(
                                            id_by = "add$",
                                            amount_by = "_amount",
                                            units_by = "_units")
                                        ),
                                        exclude_values = c("none", "", NA),
                                        fuzzy = TRUE,
                                        clean_up = TRUE,
                                        log_ns = "db") {
  stopifnot(
    active_connection(db_conn),
    as.integer(method_id) == method_id,
    check_for_value(values = method_id,
                    db_table = methods_table,
                    db_column = "id",
                    db_conn = db_conn)$exists,
    as.integer(sample_id) == sample_id,
    check_for_value(values = sample_id,
                    db_table = samples_table,
                    db_column = "id",
                    db_conn = db_conn)$exists,
    is.list(obj)
  )
  obj <- get_component(obj, mobile_phase_props[["in_item"]])[[1]]
  mixes <- obj[names(obj[grep(id_mix_by, names(obj))])]
  mixes <- mixes[-which(mixes %in% exclude_values)]
  if (length(mixes) == 0) return(NULL)
  mixes <- tibble(
    ref = names(mixes),
    value = mixes %>%
      unname() %>%
      unlist(),
    inferred_order = ref %>%
      str_extract_all("[0-9]+") %>%
      unlist() %>%
      as.integer(),
    inferred_group = ref %>%
      str_extract("[0-9]") %>%
      unlist() %>%
      as.integer(),
    group = case_when(
      str_detect(ref, paste0(mobile_phase_props$props, collapse = "|")) ~ "mobile_phase",
      str_detect(ref, paste0(carrier_props$props, collapse = "|")) ~ "carrier",
      str_detect(ref, paste0(additive_props$props, collapse = "|")) ~ "additive"
    )
  ) %>%
    arrange(inferred_group, inferred_order)
  
  # Separate out solvent mixes and do inserts on carrier mixes, norm_carriers, and carrier_aliases as appropriate
  carrier_mixes <- mixes %>%
    filter(group == "carrier") %>%
    mutate(ref = str_remove_all(ref, id_mix_by)) %>%
    pivot_wider(id_cols = c(inferred_group, inferred_order),
                values_from = c(value),
                names_from = c(ref)) %>%
    mutate(
      across(any_of(!!carrier_props$props[["fraction_by"]]), as.numeric),
      across(any_of(!!carrier_props$props[["fraction_by"]]),
             ~ ifelse(. > 1, . / 100, .)
      )
    ) %>%
    rename(c("component" = !!carrier_props$props[["id_by"]])) %>%
    left_join(
      build_db_action(
        action = "select",
        table_name = carrier_props$alias_in,
        db_conn = db_conn,  
        match_criteria = list(alias = .$component),
        log_ns = log_ns
      ),
      by = c("component" = "alias")
    ) %>%
    rename("alias_id" = "carrier_id") %>%
    mutate(
      carrier_id = ifelse(!is.na(alias_id),
                          alias_id,
                          sapply(component,
                                 function(x) {
                                   resolve_normalization_value(
                                     this_value = x,
                                     db_table = carrier_props$norm_by,
                                     id_column = "id",
                                     db_conn = db_conn,
                                     fuzzy = fuzzy,
                                     log_ns = log_ns)
                                 })
      )
    )
  
  # Insert any missing aliases
  if (any(is.na(carrier_mixes$alias_id))) {
    carrier_mixes %>%
      filter(is.na(alias_id)) %>%
      select(carrier_id, component) %>%
      rename("alias" = "component") %>%
      build_db_action(
        action = "insert",
        table_name = carrier_props$alias_in,
        db_conn = db_conn,
        values = .,
        ignore = TRUE)
  }
  
  # Make carrier mix names, insert carrier mix collections, and get the latest IDs
  if (is.null(carrier_mix_names)) {
    carrier_mix_names <- glue::glue("method_{method_id}_sample_{sample_id}_carrier_{unique(mixes$inferred_group)}")
  } else {
    if (length(carrier_mix_names) != length(carrier_mixes)) {
      log_it("warn", "Incorrect number of carrier names.", log_ns)
      if (length(carrier_mix_names) == 1) {
        msg <- glue::glue("Carrier names will be appended to '{carrier_mix_names}'.")
      } else {
        msg <- glue::glue("Carrier names will be appended to the first carrier name '{carrier_mix_names[1]}'.")
      }
      log_it("info", msg, log_ns)
      carrier_mix_names <- glue::glue("{carrier_mix_names}_carrier_{unique(mixes$inferred_group)}")
    }
  }
  res <- try(
    build_db_action(
      action = "insert",
      table_name = mix_collection_table,
      values = lapply(carrier_mix_names, function(x) list(name = x)),
      db_conn = db_conn,
      log_ns = log_ns
    )
  )
  if (inherits(res, "try-error")) {
    msg <- glue::glue("There was a problem inserting into '{mix_collection_table}'.")
    log_it("error", msg, log_ns)
    stop(msg)
  }
  
  id_col <- "id"
  mix_ids <- dbGetQuery(
    conn = db_conn,
    statement = sqlInterpolate(conn = db_conn,
                               "SELECT * FROM ?table ORDER BY ?id_col DESC LIMIT ?limit",
                               id_col = dbQuoteIdentifier(db_conn, id_col),
                               table = mix_collection_table,
                               limit = n_distinct(carrier_mixes$inferred_group))
  ) %>%
    rename(c("mix_id" = "id")) %>%
    arrange(mix_id)
  carrier_mixes <- carrier_mixes %>%
    mutate(component = carrier_id) %>%
    select(-alias_id, -carrier_id) %>%
    group_by(inferred_group) %>%
    group_split() %>%
    setNames(mix_ids$name)
  carrier_mixes <- lapply(
    mix_ids$name,
    function(x) {
      carrier_mixes[[x]] %>%
        mutate(mix_collection_name = x) %>%
        left_join(mix_ids, by = c("mix_collection_name" = "name"))
    }
  ) %>%
    setNames(mix_ids$name)
  mix_ids <- mix_ids %>%
    left_join(
      carrier_mixes %>%
        unname() %>%
        bind_rows() %>%
        distinct(inferred_group, mix_id)
    )
  mixes <- mixes %>%
    left_join(mix_ids)
  
  # Insert into carrier_mixes table by name match
  res <- lapply(names(carrier_mixes),
                function(x) {
                  try(
                    build_db_action(
                      action = "insert",
                      table_name = carrier_props$db_table,
                      db_conn = db_conn,
                      values = carrier_mixes[[x]] %>%
                        select(-c(inferred_group, inferred_order, mix_collection_name)),
                      log_ns = log_ns
                    )
                  )
                })
  if (any(unlist(lapply(res, inherits, what = "try-error")))) {
    msg <- glue::glue("There was a problem inserting into '{carrier_props$db_table}'.")
    log_it("error", msg, log_ns)
    if (clean_up) {
      build_db_action(
        action = "delete",
        table_name = mix_collection_table,
        match_criteria = list(id = mix_ids$mix_id),
        db_conn = db_conn,
        log_ns = log_ns
      )
    }
    return(carrier_mixes)
  }
  
  # Follow the same basic pipeline for carrier additives
  # Separate out solvent mixes and do inserts on carrier mixes, norm_carriers, and carrier_aliases as appropriate
  carrier_additives <- mixes %>%
    filter(group == "additive") %>%
    mutate(ref = str_remove_all(ref, id_mix_by)) %>%
    pivot_wider(id_cols = c(inferred_group, inferred_order),
                values_from = c(value),
                names_from = c(ref)) %>%
    rename(
      c("amount" = !!grep(additive_props$props[["amount_by"]], names(.), value = TRUE)),
      c("units" = !!grep(additive_props$props[["units_by"]], names(.), value = TRUE)),
      c("component" = !!grep(additive_props$props[["id_by"]], names(.), value = TRUE))
    ) %>%
    mutate(across(any_of("amount"), as.numeric)) %>%
    left_join(
      build_db_action(
        action = "select",
        table_name = additive_props$alias_in,
        db_conn = db_conn,  
        match_criteria = list(alias = .$component),
        log_ns = log_ns
      ),
      by = c("component" = "alias")
    ) %>%
    rename("alias_id" = "additive_id") %>%
    mutate(
      additive_id = ifelse(!is.na(alias_id),
                           alias_id,
                           sapply(component,
                                  function(x) {
                                    resolve_normalization_value(
                                      this_value = x,
                                      db_table = additive_props$norm_by,
                                      id_column = "id",
                                      db_conn = db_conn,
                                      fuzzy = fuzzy,
                                      log_ns = log_ns)
                                  })
      )
    )
  
  # Insert any missing aliases
  if (any(is.na(carrier_additives$alias_id))) {
    carrier_additives %>%
      filter(is.na(alias_id)) %>%
      select(additive_id, component) %>%
      rename("alias" = "component") %>%
      build_db_action(
        action = "insert",
        table_name = additive_props$alias_in,
        db_conn = db_conn,
        values = .,
        ignore = TRUE)
  }
  # Insert into carrier_additives table by name match
  carrier_additives <- carrier_additives %>%
    mutate(component = additive_id) %>%
    select(-alias_id, -additive_id) %>%
    left_join(mix_ids, by = c("inferred_group" = "inferred_group")) %>%
    select(any_of(c("mix_id", "component", "amount", "units", "inferred_group"))) %>%
    mutate(
      across(any_of("units"),
             ~ sapply(
               .,
               function(x) {
                 if (!has_missing_elements(x)) {
                   resolve_normalization_value(
                     this_value = x,
                     db_table = "norm_additive_units",
                     id_column = "id",
                     db_conn = db_conn,
                     fuzzy = fuzzy,
                     log_ns = log_ns) %>%
                     unlist()
                 } else {
                   x
                 }
               }
             ))
    ) %>%
    group_by(inferred_group) %>%
    group_split() %>%
    lapply(
      function(x) {
        build_db_action(
          action = "insert",
          table_name = additive_props$db_table,
          db_conn = db_conn,
          values = x %>%
            select(-inferred_group),
          log_ns = log_ns
        )
      })
  # Finally, shape up and insert into mobile_phases
  mobile_phases <- mixes %>%
    filter(group == "mobile_phase")
  if (nrow(mobile_phases) == 0) {
    mobile_phases <- tibble(mix_id = mix_ids$mix_id)
  } else {
    mobile_phases <- mobile_phases %>%
      mutate(ref = str_remove_all(ref, id_mix_by)) %>%
      left_join(mobile_phase_props$props %>%
                  as.data.frame() %>%
                  rownames_to_column() %>%
                  setNames(c("map_to", "ref"))) %>%
      mutate(ref = ifelse(is.na(map_to), ref, map_to)) %>%
      pivot_wider(id_cols = c(inferred_group, inferred_order, mix_id),
                  values_from = c(value),
                  names_from = c(ref))
  }
  mobile_phases <-  mobile_phases %>%
    rename("carrier_mix_collection_id" = "mix_id") %>%
    mutate(
      ms_methods_id = method_id,
      sample_id = sample_id,
      across(any_of("flow_units"),
             ~ sapply(
               .,
               function(x) {
                 if (!has_missing_elements(x)) {
                   resolve_normalization_value(
                     this_value = x,
                     db_table = "norm_flow_units",
                     id_column = "id",
                     db_conn = db_conn,
                     fuzzy = fuzzy,
                     log_ns = log_ns) %>%
                     unlist()
                 } else {
                   x
                 }
               }
             )
      ),
      across(any_of("duration_units"),
             ~ sapply(
               .,
               function(x) {
                 if (!has_missing_elements(x)) {
                   resolve_normalization_value(
                     this_value = x,
                     db_table = "norm_duration_units",
                     id_column = "id",
                     db_conn = db_conn,
                     fuzzy = fuzzy,
                     log_ns = log_ns) %>%
                     unlist()
                 } else {
                   x
                 }
               }
             )
      )
    ) %>%
    select(any_of(dbListFields(con, mobile_phase_props$db_table)))
  build_db_action(
    action = "insert",
    table_name = mobile_phase_props$db_table,
    values = mobile_phases,
    db_conn = db_conn,
    log_ns = log_ns
  )
}

# TODO
#' @note This function is called as part of [full_import]
resolve_ms_data <- function(obj, log_ns = "db") {
  # Check connection
  stopifnot(active_connection(db_conn))
  
}

# TODO
#' @note This function is called as part of [full_import]
resolve_qc_methods <- function(obj,
                               method_id,
                               sample_id,
                               qc_method_in = "qcmethod",
                               db_table = "qc_methods",
                               reference_in = "source",
                               import_map = IMPORT_MAP,
                               db_conn = con,
                               log_ns = "db") {
  # Check connection
  stopifnot(
    active_connection(db_conn),
    as.integer(method_id) == method_id,
    check_for_value(values = method_id,
                    db_table = "ms_methods",
                    db_column = "id",
                    db_conn = db_conn)$exists,
    check_for_value(values = sample_id,
                    db_table = "samples",
                    db_column = "id",
                    db_conn = db_conn)$exists
  )
  values <- obj[[qc_method_in]] %>%
    rename(c("reference" = !!reference_in)) %>%
    mutate(
      ms_methods_id = method_id,
      sample_id = sample_id,
      name = sapply(
        name,
        function(x) {
          resolve_normalization_value(
            this_value = x,
            db_table = "norm_qc_methods_name",
            db_conn = db_conn,
            log_ns = log_ns
          )
        }
      ) %>%
        unname(),
      reference = ifelse(reference == "", NA, reference),
      reference = ifelse(is.na(reference),
                         NA,
                         sapply(
                           reference,
                           function(x) {
                             resolve_normalization_value(
                               this_value = x,
                               db_table = "norm_qc_methods_reference",
                               db_conn = db_conn,
                               log_ns = log_ns
                             )
                           }
      ))
    )
  res <- try(
    build_db_action(action = "insert",
                    table_name = db_table,
                    db_conn = db_conn,
                    values = values,
                    log_ns = log_ns)
  )
  if (inherits(res, "try-error")) {
    msg <- 'There was an issue adding records to table "qc_methods".'
    log_it("error", msg)
    stop(msg)
  }
}

# TODO
resolve_qc_data <- function(obj, sample_id, db_conn = con, log_ns = "db") {
  # Argument validation relies on verify_args
  if ("qc" %in% names(obj)) obj <- obj$qc
  stopifnot(as.integer(sample_id) == sample_id)
  sample_id <- as.integer(sample_id)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, sample_id, db_conn),
      conditions = list(
        obj       = list(c("mode", "list")),
        sample_id = list(c("mode", "integer"), c("length", 1), "no_na"),
        db_conn   = list(c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  # Sanity check that sample_id exists
  stopifnot(
    active_connection(db_conn),
    check_for_value(sample_id, "samples", "id", db_conn = db_conn)$exists
  )
  values <- lapply(obj,
                   function(x) {
                     x %>%
                       mutate_all("as.character") %>%
                       mutate(parameter = str_c(parameter, if (nrow(x) > 1) paste0("_", seq.int(1, nrow(x), 1)) else "")) %>%
                       pivot_longer(cols = -parameter) %>%
                       mutate(name = ifelse(name == "result", "qc_pass", name))
                   }) %>%
    bind_rows() %>%
    mutate(sample_id = sample_id) %>%
    relocate(sample_id, .before = everything())
  res <- try(
    build_db_action(
      action = "insert",
      table_name = "qc_data",
      db_conn = db_conn,
      values = values
    )
  )
  if (inherits(res, "try-error")) {
    msg <- 'There was an issue adding records to table "qc_data".'
    log_it("error", msg)
    return(res)
  }
}

#' Get unique components of a nested list
#'
#' There are times when the concept of "samples" and "grouped data" may become
#' intertwined and difficult to parse. The import process is one of those times
#' depending on how the import file is generated. This function takes a nested
#' list and compares a specific aspect of it, grouping the output based on that
#' aspect and returning its characteristics.
#'
#' For example, the standard NIST import includes the "sample" aspect, which may
#' be identical for multiple data import files. This provides a unique listing
#' of those sample characteristics to reduce data manipulation and storage, and
#' minimize database "chatter" during read/write. It returns a set of unique
#' characteristics in a list, with appended characteristics "import_object" with
#' the index number and object name of entries matching those characteristics.
#'
#' This is largely superceded by later developments to database operations that
#' first check for a table primary key id given a comprehensive list of column
#' values in those tables where only a single record should contain those values
#' (e.g. a complete unique case, enforced or unenforced).
#'
#' @param objects LIST object
#' @param aspect CHR scalar name of the aspect from which to generate unique
#'   combinations
#'
#' @return Unnamed LIST of length equaling the number of unique combinations
#'   with their values and indices
#' @export
#'
#' @examples
#' tmp <- list(list(a = 1:10, b = 1:10), list(a = 1:5, b = 1:5), list(a = 1:10, b = 1:5))
#' get_uniques(tmp)
get_uniques <- function(objects, aspect) {
  if (!is.list(objects[[1]])) objects <- list(objects)
  out <- lapply(objects, function(x) x[[aspect]]) %>%
    lapply(function(x) x[sort(names(x))])
  out_distinct <- out %>%
    unique() %>%
    lapply(function(x) {
      ind <- unname(which(sapply(out, FUN = identical, x) == TRUE))
      import_object <- list(
        index  = ind,
        object = names(objects)[ind]
      )
      if (is.null(import_object$file)) import_object$file <- "import_file"
      c(data = list(x), import_object = list(import_object))
    })
  return(out_distinct)
}


#' Delete a sample
#'
#' Removes a sample from the database and associated records in ms_methods,
#' conversion_software_settings, and conversion_software_linkage. Associated
#' peak and mass spectrometric signals will also be removed.
#'
#' @param sample_ids INT vector of IDs to remove from the samples table.
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return
#' @export
remove_sample <- function(sample_ids, db_conn = con, log_ns = "db") {
  log_fn("start")
  if (sample_ids != as.integer(sample_ids)) stop('Parameter "sample_ids" cannot be safely coerced to integer.')
  sample_ids <- as.integer(sample_ids)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sample_ids = list(c("mode", "integer"), c("n>=", 1)),
        db_conn    = list(c("length", 1))
      ),
      from_fn = "remove_sample"
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(active_connection(db_conn))
  dat <- tbl(con, "samples")
  target_ms_methods_ids <- dat %>%
    filter(id %in% sample_ids) %>%
    distinct(ms_methods_id) %>%
    pull()
  log_it("info", glue('Removing samples with id{ifelse(length(sample_ids) > 1, "s", "")} {format_list_of_names(sample_ids)}.'), log_ns)
  build_db_action("delete", "samples", match_criteria = list(id = sample_ids))
  remaining_ms_methods_ids <- dat %>%
    filter(ms_methods_id %in% target_ms_methods_ids) %>%
    distinct(ms_methods_id) %>%
    pull()
  remove_methods_ids <- target_ms_methods_ids[!target_ms_methods_ids %in% remaining_ms_methods_ids]
  if (length(remaining_ms_methods_ids) == 0) {
    msg_part <- glue('{ifelse(length(remove_methods_ids) > 1, "s", "")} {format_list_of_names(remove_methods_ids)}')
    log_it("info", glue('No other samples share methods with id{msg_part}.'), log_ns)
  } else {
    msg_part <- glue('{ifelse(length(remaining_ms_methods_ids) > 1, "s", "")} {format_list_of_names(remaining_ms_methods_ids)} {ifelse(length(remaining_ms_methods_ids) > 1, "were", "was")}')
    log_it("info", glue("Method{msg_part} shared by others and will not be removed."), log_ns)
  }
  if (length(remove_methods_ids) > 0) {
    msg_part <- glue('{ifelse(length(remove_methods_ids) > 1, "s", "")} {format_list_of_names(remove_methods_ids)}')
    log_it("info", glue("Removing method{msg_part}."), log_ns)
    build_db_action("delete", "ms_methods", match_criteria = list(id = remove_methods_ids))
  }
  log_fn("end")
}

#' Make import requirements file
#'
#' Importing from the NIST contribution spreadsheet requires a certain format.
#' In order to proceed smoothly, that format must be verified for gross
#' integrity with regard to expectations about shape (i.e. class), names of
#' elements, and whether they are required for import. This function creates a
#' JSON expression of the expected import structure and saves it to the project
#' directory.
#'
#' Either an existing JSON expression or an R list object may be used for
#' `example_import`. If it is a character scalar, it will be assumed to be a
#' file name, which will be loaded based on file extension. That file must be a
#' JSON parseable text file, though raw text is acceptable.
#'
#' An example file is located in the project directory at
#' "example/PFAC30PAR_PFCA1_mzML_cmpd2627.JSON"
#'
#' As with any file manipulation, use care with `file_name`.
#'
#' @param example_import CHR or LIST object containing an example of the
#'   expected import format; this should include only a SINGLE compound
#'   contribution file
#' @param file_name CHR scalar indicating a file name to save the resulting name
#'   or search on any existing file to archive if `archive` = TRUE (default:
#'   "import_requirements.json")
#' @param archive LGL indicating whether or not to archive an existing file
#'   matching `file_name` by suffixing the file name with current date. Only one
#'   archive per date is supported; if a file already exists, it will be
#'   deleted. (default: TRUE)
#' @param retain_in_R LGL indicating whether to retain a local copy of the
#'   requirements file generated (default: TRUE)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return writes a file to the project directory (based on the found location
#'   of `file_name`) with the JSON structure
#' @export
make_requirements <- function(example_import,
                              file_name = "import_requirements.json",
                              archive = TRUE,
                              retain_in_R = TRUE,
                              log_ns = "db") {
  if (class(example_import) == "character") {
    log_it("info", sprintf('Assuming "%s" refers to a file name, and that the file is parseable as JSON.', example_import), log_ns)
    f_name <- ifelse(
      file.exists(example_import),
      example_import,
      list.files(pattern = example_import, recursive = TRUE, full.names = TRUE)
    )
    if (length(f_name) == 0) {
      log_it("error", sprintf('No file matching "%s" was located in the project directory.', example_import), log_ns)
      stop()
    } else if (length(f_name) > 1) {
      log_it("warn", sprintf('Multiple files matching "%s" were located in the project directory.', example_import), log_ns)
      if (interactive()) {
        f_name <- select.list(
          title = "Please select an example file to continue.",
          choices = f_name
        )
      } else {
        log_it("info", format_list_of_names(f_name))
        stop()
      }
    }
    example_import <- try(
      fromJSON(read_file(f_name))
    )
    if (class(example_import) == "try-error") {
      stop(sprintf('The file "%s" was not readable as JSON.', f_name))
    }
  }
  if (!file.exists(f_name)) {
    f_name <- list.files(pattern = file_name, full.names = TRUE, recursive = TRUE)
  }
  if (length(f_name) > 1) {
    f_name <- f_name[1]
    log_it(
      "warn",
      sprintf(
        "Multiple import requirements files located. Only the file at (%s) will be replaced.",
        f_name
      ),
      log_ns
    )
  } else if (length(f_name) == 1 && archive) {
    f_ext      <- paste0(".", tools::file_ext(f_name))
    f_suffix   <- paste0("_", format(Sys.Date(), "%Y%m%d"), f_ext)
    new_f_name <- gsub(
      f_ext,
      f_suffix,
      f_name
    )
    if (file.exists(new_f_name)) file.remove(new_f_name)
    file.rename(f_name, new_f_name)
  } else {
    f_name <- paste0(
      tools::file_path_sans_ext(file_name),
      ".json"
    )
  }
  out <- lapply(
    example_import,
    function(x) {
      list(
        class = class(x),
        names = names(x),
        required = TRUE
      )
    }
  )
  not_required <- c("annotation", "chromatography")
  for (nr in not_required) {
    out[[nr]]$required <- FALSE
  }
  if (retain_in_R) import_requirements <<- out
  out <- toJSON(out, auto_unbox = TRUE, pretty = TRUE)
  write_file(out, f_name)
  cat("Requirements file written to", f_name)
}



#' Verify column names for import
#'
#' This function validates that all required columns are present prior to
#' importing into a database column by examining provided values against the
#' database schema. This is more of a sanity check on other functions than
#' anything, but also strips extraneous columns to meet the needs of an INSERT
#' action. The input to `values` should be either a LIST or named CHR vector of
#' values for insertion or a CHR vector of the column names.
#'
#' @note If columns are defined as required in the schema and are not present,
#'   this will fail with an informative message about which columns were
#'   missing.
#'
#' @note If columns are provided that do not match the schema, they will be
#'   stripped away in the return value.
#'
#' @param values LIST or CHR vector of values to add. If `names_only` is TRUE,
#'   values are directly interpreted as column names. Otherwise, all values
#'   provided must be named.
#' @param db_table CHR scalar of the table name
#' @param names_only LGL scalar of whether to treat entries of `values` as the
#'   column names rather than the column values (default: FALSE)
#' @param require_all LGL scalar of whether to require all columns (except the
#'   assumed primary key column of "id") or only those defined as "NOT NULL"
#'   (default: TRUE requires the presence of all columns in the table)
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return An object of the same type as `values` with extraneous values (i.e.
#'   those not matching a database column header) stripped away.
#' @export
verify_import_columns <- function(values, db_table, names_only = FALSE, require_all = TRUE, db_conn = con, log_ns = "db") {
  log_fn("start")
  log_it("info", glue('Verifying column requirements for table "{db_table}".'), log_ns)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values      = list(c("n>=", 1)),
        db_table    = list(c("mode", "character"), c("length", 1)),
        names_only  = list(c("mode", "logical"), c("length", 1)),
        require_all = list(c("mode", "logical"), c("length", 1)),
        db_conn     = list(c("length", 1)),
        log_ns      = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  if (
    all(
      any(
        is.null(names(values)),
        length(names(values)) != length(values)
      ),
      !names_only
    )
  ) {
    if (!names_only) stop("Values provided to verify_import_columns() must all be named unless 'names_only' is set to TRUE.")
  }
  # Make sure required column names are present
  table_info      <- pragma_table_info(db_table = db_table,db_conn = db_conn)
  table_info      <- table_info %>%
    filter(!name == "id")
  if (require_all) {
    table_needs <- table_info$name
  } else {
    table_needs <- table_info$name[table_info$notnull == 1]
  }
  provided        <- if (names_only) values else names(values)
  columns_present <- table_needs %in% provided
  if (!all(columns_present)) {
    missing_columns <- table_needs[!columns_present]
    stop(glue('Required column{ifelse(length(missing_columns) > 1, "s are", " is")}} missing: {format_list_of_names(missing_columns)}.'))
  } else {
    valid_columns <- provided %in% table_info$name
    if (!all(valid_columns)) {
      extra_columns <- provided[!valid_columns]
      log_it("warn", glue('Extra column{ifelse(length(extra_columns) > 1, "s were", " was")}} provided and will be ignored: {format_list_of_names(extra_columns)}.'), log_ns)
    }
    log_fn("end")
    return(values[valid_columns])
  }
}

#' Verify an import file's properties
#'
#' Checks an import file's characteristics against expectations. This is mostly
#' a sanity check against changing conditions from project to project. Import
#' requirements should be defined at the environment level and enumerated as a
#' JSON object, which can be created by calling [make_requirements] on an
#' example import for simplicity. An example is provided in the 'examples'
#' directory as "NIST_import_requirements.json". If multiple requirements are in
#' use (e.g. pulling from multiple locations), this can be run multiple times
#' with different values of `requirement_obj` or `file_name`.
#'
#' The return from this is a tibble with 6 or 7 columns depending on whether the
#' import file contains data from a single sample or multiple samples. It will
#' parse either way depending on where required names are located in the object
#' provided to `obj`. If it is a single list, the return will contain 6 columns;
#' if it is a nested list of samples, the return will contain 7 columns, with
#' the names of the provided `obj` attached to each row. Other columns contain
#' information related to three checks.
#'
#' 1. has_all_required: Are all required names present in the sample?
#' (TRUE/FALSE)
#'
#' 2. missing_requirements: Character vectors naming any of the missing
#' requirements
#'
#' 3. has_full_detail: Is all expected detail present? (TRUE/FALSE)
#'
#' 4. missing_detail: Character vectors naming any missing value sets
#'
#' 5. has_extra: Are there unexpected values provided ? (TRUE/FALSE)
#'
#' 6. extra_cols: Character vectors naming any has_extra columns; these will be
#' dropped from the import but are provided for information sake
#'
#' All of this is defined by the `requirements_obj` list. Do not provide that
#' list directly, instead pass this function the name of the requirements object
#' for interoperability. If a `requirements_obj` cannot be identified via
#' [base::exists] then the `file_name` will take precedence and be imported.
#' Initial use and set up may be easier in interactive sessions.
#'
#' @note If `file_name` is provided, it need not be fully defined. The value
#'   provided will be used to search the project directory.
#'
#' @param obj LIST of the object to import matching structure expectations,
#'   typically from a JSON file fed through [full_import]
#' @param ignore_extra LGL scalar of whether to ignore extraneous import
#'   elements or stop the import process (default: TRUE)
#' @param requirements_obj CHR scalar of the name of an R object holding import
#'   requirements; this is a convenience shorthand to prevent multiple imports
#'   from parameter `file_name` (default: "import_requirements")
#' @param file_name CHR scalar of the name of a file holding import
#'   requirements; if this has already been added to the calling environment,
#'   `requirements_obj` will be used preferentially as the name of that object
#' @param log_issues_as CHR scalar of the log level to use (default: "warn"),
#'   which must be a valid log level as in [logger::FATAL]; will be ignored if
#'   the [logger] package isn't available
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return A tibble object with 6 columns containing the results of the check,
#'   with one row for each import object identified; if `obj` is a list of
#'   import data, the column "import_object" is added at the left of the data
#'   frame with the names of the list components
#'
#' @export
verify_import_requirements <- function(obj,
                                       ignore_extra = TRUE,
                                       requirements_obj = "import_requirements",
                                       file_name = "import_requirements.json",
                                       log_issues_as = "warn",
                                       log_ns = "db") {
  # Argument validation relies on verify_args
  if (all(exists("verify_args"))) {
    arg_check <- verify_args(
      args       = list(obj, ignore_extra, log_issues_as, log_ns),
      conditions = list(
        obj           = list(c("mode", "list")),
        ignore_extra  = list(c("mode", "logical"), c("length", 1)),
        log_issues_as = list(c("mode", "character"), c("length", 1)),
        log_ns        = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  if (exists(requirements_obj)) {
    reqs <- eval(sym(requirements_obj))
  } else {
    f_name <- list.files(pattern = file_name, recursive = TRUE, full.names = TRUE)
    if (length(f_name) == 1) {
    } else {
      if (interactive()) {
        f_name <- select.list(
          title = "Please select one requirements file.",
          choices = f_name
        )
      } else {
        log_it(
          "error",
          "Multiple files matching the requirements name were identified. Please be more specific.",
          "db"
        )
        stop("Cannot identify a single requirements file.")
      }
    }
    reqs <- fromJSON(read_file(f_name))
    assign(requirements_obj, reqs, envir = .GlobalEnv)
  }
  req_names      <- names(reqs)
  obj_names      <- names(obj)
  hard_reqs      <- req_names[which(lapply(reqs, function(x) x$required) == TRUE)]
  recommended    <- req_names[!req_names %in% hard_reqs]
  all_required   <- all(hard_reqs %in% obj_names)
  detail_reqs    <- hard_reqs[!hard_reqs %in% obj_names]
  full_detail    <- all(recommended %in% obj_names)
  missing_detail <- recommended[!recommended %in% obj_names]
  extra_cols     <- !obj_names %in% req_names
  extra          <- any(extra_cols)
  extra_cols     <- obj_names[extra_cols]
  out    <- list(
    has_all_required     = all_required,
    missing_requirements = list(detail_reqs),
    has_full_detail      = full_detail,
    missing_detail       = list(missing_detail),
    has_extra            = extra,
    extra_cols           = list(extra_cols)
  )
  if (all_required) {
    nested <- FALSE
  } else {
    required <- hard_reqs %in% unique(unlist(lapply(obj, names)))
    nested_with_all_required <- all(required)
    if (nested_with_all_required) {
      log_it("info", "The provided object contains a nested list where required names were located.", "db")
      nested <- TRUE
      # Speed this up since it already passed argument verification
      verify <- VERIFY_ARGUMENTS
      if (verify) {
        log_it("info", "Argument verification will be turned off to speed up the check process.", "db")
        assign("VERIFY_ARGUMENTS", FALSE, envir = .GlobalEnv)
      }
      out <- lapply(obj, verify_import_requirements, ignore_extra = ignore_extra, log_issues_as = log_issues_as) %>%
        setNames(names(obj))
      if (verify) {
        log_it("info", glue::glue("Setting argument verification back to {verify} according to the session settings."), "db")
        assign("VERIFY_ARGUMENTS", verify, envir = .GlobalEnv)
      }
    } else {
      nested <- FALSE
      missing_requirements <- hard_reqs[!hard_reqs %in% names(obj)]
      log_it(log_issues_as,
             sprintf("Required import element%s %s missing from at least one import item.",
                     ifelse(length(missing_requirements) > 1, "s", ""),
                     ifelse(length(missing_requirements) > 1, "were", "was")
             ),
             "db"
      )
    }
  }
  if (all(extra, !nested)) {
    extra_text <- sprintf(" %s will be ignored during import.",
                          ifelse(length(extra_cols) > 1, "These", "It")
    )
    log_it("info",
           sprintf('%sxtraneous element%s named %s %s identified.%s',
                   ifelse(length(extra_cols) > 1, "E", "An e"),
                   ifelse(length(extra_cols) > 1, "s", ""),
                   format_list_of_names(extra_cols, resolve_quotes = TRUE),
                   ifelse(length(extra_cols) > 1, "were", "was"),
                   ifelse(ignore_extra, extra_text, "")
           ),
           "db"
    )
    if (!ignore_extra) stop("Called with ignore_extra = FALSE. Import aborted.")
    obj <- obj[which(names(obj) %in% names(reqs))]
  }
  if (nested) {
    out <- out %>%
      bind_rows() %>%
      mutate(import_object = names(obj)) %>%
      relocate(import_object, .before = )
  } else {
    out <- as_tibble(out)
  }
  return(out)
}

#' Append additional named elements to a list
#'
#' This does nothing more than [base::append] ellipsis arguments to be added
#' directly to the end of an existing list object. This primarily supports
#' additional property assignment during the import process for future
#' development and refinement. Call this as part of any function with additional
#' arguments. This may result in failures or ignoring unrecognized named
#' parameters. If no additional arguments are passed `obj` is returned as
#' provided.
#'
#' @note If duplicate names exists in `obj` and those provided as elipsis
#'   arguments, those provided as part of the elipsis will replace those in
#'   `obj`.
#'
#' @param obj LIST of any length to be appended to
#' @param ... Additional arguments passed to/from the ellipsis parameter of
#'   calling functions. If named, names are preserved.
#'
#' @return LIST object of length equal to `obj` plus additional named arguments
#' @export
#'
#' @examples
#' tack_on(list(a = 1:3), b = letters, c = rnorm(10))
#' tack_on(list(a = 1:3))
tack_on <- function(obj, ...) {
  addl_args <- list(...)
  if (any(names(addl_args) %in% names(obj))) {
    applies_to <- names(addl_args)[names(addl_args) %in% names(obj)]
    for(i in applies_to) {
      obj[[i]] <- NULL
    }
  }
  out <- append(obj, addl_args)
  return(out)
}

#' Resolve components from a list or named vector
#'
#' Call this to pull a component named `obj_component` from a list or named
#' vector provided as `obj` and optionally use [tack_on] to append to it. This
#' is intended to ease the process of pulling specific components from a list
#' for further treatment in the import process by isolating that component.
#'
#' @note If ellipsis arguments are provided, they will be appended to each
#'   identified component via [tack_on]. Use with caution, but this can be
#'   useful for appending common data to an entire list (e.g. a datetime stamp
#'   for logging processing time or a processor name, human or software).
#'
#' @inheritParams tack_on
#'
#' @param obj LIST or NAMED vector in which to find `obj_component`
#' @param obj_component CHR vector of named elements to find in `obj`
#' @param silence LGL scalar indicating whether to silence recursive messages,
#'   which may be the same for each element of `obj` (default: TRUE)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return
#' @export
#'
#' @examples
get_component <- function(obj, obj_component, silence = TRUE, log_ns = "global", ...) {
  stopifnot(is.character(obj_component), length(obj_component) > 0, is.character(log_ns), length(log_ns) == 1)
  logging <- exists("log_it")
  names_present <- obj_component %in% names(obj)
  if (any(names_present)) {
    if (!all(names_present)) {
      msg <- glue::glue("Requested component{ifelse(sum(!names_present) > 1, 's', '')} {format_list_of_names(obj_component[!names_present])} were missing.")
      if (logging) {
        log_it("warn", msg, log_ns)
      } else {
        warning(msg)
      }
      obj_component <- which(names_present)
    }
    out <- obj[obj_component]
  } else if (is.list(obj)) {
    out <- purrr::flatten(
      lapply(obj,
             get_component,
             obj_component = obj_component,
             silence = silence)
    )
  } else {
    if (logging && !silence) log_it("warn", glue::glue('"No components named {gsub(" and ", " or ", format_list_of_names(obj_component))}" found in the namespace of this object. Using directly.'), log_ns)
    return(NULL)
  }
  kwargs <- list(...)
  if (length(kwargs) > 0) {
    if (logging) log_it("info", glue::glue("Tacking on {length(kwargs)} additional argument{ifelse(length(kwargs) > 1, 's', '')} ({format_list_of_names(names(kwargs), add_quotes = TRUE)})."))
    out <- lapply(out, function(x) tack_on(obj = x, ... = ...))
  }
  return(out)
}

#' Map an import file to the database schema
#'
#' This parses an import object and attempts to map it to database fields and
#' tables as defined by an import map stored in an object of class data.frame,
#' typically created during project compliance as "IMPORT_MAP". This object is a
#' list of all columns and their tables in the import file matched with the
#' database table and column to which they should be imported.
#'
#' @note The object used for `import_map` must be of a data.frame object that at
#'   minimum includes names columns that includes import_category,
#'   import_parameter, alias_lookup, and sql_normalization 
#'
#' @param import_obj LIST object of values to import
#' @param aspect CHR scalar of the import aspect (e.g. "sample") to map
#' @param import_map data.frame object of the import map (e.g. from a CSV)
#' @param case_sensitive LGL scalar of whether to match normalization values in
#'   a case sensitive (TRUE, default) or case insensitive manner; passed to
#'   [resolve_normalization_values]
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#'
#' @return LIST of final mapped values
#' @export
#' 
map_import <- function(import_obj,
                       aspect,
                       import_map,
                       case_sensitive = TRUE,
                       db_conn = con,
                       log_ns = "db") {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        import_obj     = list(c("mode", "list")),
        aspect         = list(c("mode", "character"), c("length", 1)),
        import_map     = list(c("mode", "data.frame"), c("n>", 0)),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        db_conn        = list(c("length", 1)),
        log_ns         = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(dbIsValid(db_conn),
            aspect %in% dbListTables(db_conn))
  this_map <- import_map %>%
    filter(sql_table == aspect,
           import_parameter != "")
  out <- vector("list", length = nrow(this_map)) %>%
    setNames(this_map$sql_parameter)
  needed_columns <- c("import_category", "import_parameter", "alias_lookup", "sql_normalization")
  if (!all(needed_columns %in% names(import_map))) {
    stop(glue::glue("The object provided to `import_map` must contain columns {format_list_of_names(needed_columns)} to be a valid import map."))
  }
  this_val <- NA
  i <- 0
  while (i < nrow(this_map)) {
    i <- i + 1
    this_field <- names(out)[i]
    properties <- this_map
    if (!is.na(names(out)[i])) {
      properties <- this_map %>%
        filter(sql_parameter == names(out)[i])
    }
    if (nrow(properties) > 1) {
      # Some expansion tables are listed as "key:value pairs" (e.g. "instrument_properties")
      if (all(properties$note == "key:value pairs")) {
        db_table <- unique(properties$sql_table)
        for (j in 1:nrow(properties)) {
          import_cat  <- properties$import_category[j]
          param_name  <- properties$import_parameter[j]
          param_value <- import_obj[[import_cat]][[param_name]]
          out[j] <- param_value
          names(out)[j] <- param_name
        }
        i <- j
      } else {
        stop(glue::glue("'{this_field}' is unresolvably mapped to multiple places. Please resolve and try again."))
      }
    } else if (nrow(properties) == 0) {
      log_it("warn", glue::glue("'{this_field}' is not mapped. A null value will be used."), log_ns)
    } else {
      this_note <- properties$note
      if (!is.na(this_note) && this_note == "key:value pairs") {
        # Do inserts for straight key:value pair long form tables
      } else {
        category  <- properties$import_category[1]
        parameter <- properties$import_parameter[1]
        alias_in  <- properties$alias_lookup[1]
        norm_by   <- properties$sql_normalization[1]
        this_val  <- import_obj[[category]][[parameter]]
        if (!is.na(this_val) && (is.null(this_val) || this_val == "")) this_val <- NA
        if (!is.na(alias_in) && (is.null(alias_in) || alias_in == "")) alias_in <- NA
        if (!is.na(norm_by) && (is.null(norm_by) || norm_by == "")) norm_by <- NA
        if (!is.na(this_val)) {
          if (!is.na(norm_by)) {
            norm_id <- integer(0)
            column_names <- character(0)
            log_it("info", glue::glue("Attempt resolution of '{this_field}': norm_by = '{norm_by}' and this_val = '{this_val}'."), log_ns)
            if (!is.na(alias_in)) {
              log_it("info", glue::glue("'{this_field}': alias_in = '{alias_in}' and this_val = '{this_val}'; resolving normalization value."), log_ns)
              column_names <- dbListFields(db_conn, alias_in)
              lookup_val <- list(list(values = this_val, like = TRUE)) %>%
                setNames(column_names[2])
              alias_id <- try(
                build_db_action(
                  action = "select",
                  table_name = alias_in,
                  match_criteria = lookup_val,
                  db_conn = db_conn,
                  log_ns = log_ns
                )
              )
              if (inherits(alias_id, "try-error")) {
                msg <- glue::glue("There was a problem resolving the alias for {this_field}.")
                log_it("error", msg, log_ns)
                stop(msg)
                if (!nrow(alias_id) == 1) {
                  msg <- sprintf("%s aliases identified in %s for value '%s'.",
                                 ifelse(nrow(alias_id) == 0, "No", "Multiple"),
                                 this_field,
                                 this_val)
                  log_it("info", msg, log_ns)
                  if (nrow(alias_id) > 1) {
                    alias_id <- resolve_normalization_value(
                      this_value = this_val,
                      db_table = alias_in,
                      id_column = column_names[1],
                      db_conn = db_conn,
                      log_ns = log_ns
                    )
                  }
                }
              } else {
                norm_id <- alias_id %>%
                  select(contains("_id")) %>%
                  pull(1)
                # log_it("info", glue::glue("Resolved alias for '{this_field}' as id = '{norm_id}' ('{this_val}')."), log_ns)
              }
            }
            if (!length(norm_id) == 1) {
              norm_id <- resolve_normalization_value(
                this_value = this_val,
                db_table = norm_by,
                case_sensitive = case_sensitive,
                db_conn = db_conn,
                log_ns = log_ns
              )
              if (!is.na(alias_in)) {
                res <- try(
                  build_db_action(
                    action = "insert",
                    table_name = alias_in,
                    values = list(norm_id, this_val) %>%
                      setNames(column_names),
                    ignore = TRUE,
                    log_ns = log_ns
                  )
                )
                if (inherits(res, "try-error")) {
                  log_it("warn",
                         glue::glue("There was a problem inserting the alias '{this_val}' into table '{alias_in}'."),
                         log_ns)
                }
              }
            }
            
            if (length(norm_id) == 1 && norm_id == as.integer(norm_id)) {
              log_it("info", glue::glue("Resolved normalization value for '{this_field}' as id = '{norm_id}' ('{this_val}')."), log_ns)
              this_val <- norm_id
            } else {
              log_it("error", glue::glue("Could not resolve a normalization value for '{this_field}'."), log_ns)
            }
          }
          out[i] <- this_val
        }
      }
    }
  }
  # Catch date like object and coerce to a specific format
  # date_likes <- lapply(out,
  #                      function(x) {
  #                        if (is.POSIXtst <- suppressWarnings(
  #                          lubridate::as_datetime(x)
  #                        )
  #                      })
  return(out)
}


#' Does an import object have a defined QC value present?
#'
#' This returns a numerical boolean of whether or not the import has a given
#' structure for QC data, indicating whether the analytical method has a QC
#' method attached.
#'
#' @param obj LIST import object of a single import file
#' @param qc_in CHR scalar name of an item in `obj` containing QC information
#'   (default: "qc")
#' @param qc_method_in CHR scalar name of an item in `obj` containing QC method
#'   results as a data.frame object of the form `data.frame(name = character(0),
#'   value = logical(0))` (default: "qcmethod")
#' @param search_text_in CHR scalar column name in `obj[[qc_method_in]]`
#'   containing the QC result names to look for (default: "name")
#' @param search_text CHR scalar of the QC result to look for (default: "QC
#'   Method Used")
#' @param value_in CHR scalar column name in `obj[[qc_method_in]]` containing
#'   the values of QC results in boolean format (default: "name")
#'
#' @return LGL scalar of the value, if present, as numeric, or 0 if not present
#' @export
#'
#' @examples
#' qc_result(list(qc = list(letters), qcmethod = data.frame(name = "QC Method Used", value = TRUE)))
qc_result <- function(obj,
                      qc_in = "qc",
                      qc_method_in = "qcmethod",
                      search_text_in = "name",
                      search_text = "QC Method Used",
                      value_in = "value") {
  stopifnot(
    all(sapply(c(qc_in, qc_method_in, search_text, value_in), is.character)),
    all(sapply(c(qc_in, qc_method_in, search_text, value_in), length) == 1)
  )
  can_parse <- qc_in %in% names(obj) &&
    length(obj[[qc_in]]) > 1 &&
    qc_method_in %in% names(obj)
  if (can_parse) {
    tmp <- obj[[qc_method_in]] %>%
      filter(.[[search_text_in]] == search_text)
    if (nrow(tmp) == 0) {
      out <- FALSE
    } else {
      out <- ifelse(value_in %in% names(tmp),
                    tmp[[value_in]],
                    FALSE)
    }
  } else {
    out <- FALSE
  }
  return(as.numeric(out))
}
