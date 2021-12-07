full_import <- function(obj = NULL, file_name = NULL, conn = con) {
  log_it("info", "Starting full import...")
  if (all(is.null(file_name), is.null(obj))) {
    stop('One of either "file_name" or "obj" must be provided.')
  }
  if (is.null(obj)) {
    if (all(file.exists(file_name), grepl(".json", file_name))) {
      obj <- jsonlite::read_json(file_name)
    }
  }
  get_names <- names(purrr::flatten(IMPORT_HEADERS))
  # Annotation is optional
  get_names <- get_names[!get_names == "annotation"]
  if (all(get_names %in% names(obj))) {
    obj <- list(obj)
  }
  names_present <- lapply(obj,
                          function(x) {
                            get_names %in% names(x)
                          })
  all_present <- lapply(names_present, all) %>%
    unlist()
  if (!all(all_present)) {
    log_it("warn", "Not all required names were universally present in this import.")
    is_missing <- which(!all_present)
    missing_names <- lapply(names_present[is_missing], function(x) unname(get_names[!x]))
    msgs <- sprintf("\t%s is missing %s",
                    names(missing_names),
                    lapply(missing_names, format_list_of_names))
    lapply(msgs, function(x) log_it("warn", x))
    log_it("warn", "Only those entries with all required data will be included.")
    obj <- obj[all_present]
  }
  import_relationships <- lapply(get_names, get_uniques, import_obj = obj) %>%
    setNames(glue("import_{get_names}"))
  tmp <- import_relationships %>%
    setNames(str_remove_all(names(import_relationships), "^import_"))
  for (i in 1:length(tmp)) {
         log_it("info", glue('\t{length(tmp[[i]])} unique entr{ifelse(length(tmp[[i]]) > 1, "ies", "y")} in {names(tmp)[i]}.'))
  }
  tmp <- lapply(import_relationships$import_massspectrometry,
                function(x) {
                  c(x, method_id = add_method(x))
                })
  # Put in methods and append appropriate samples with the method id
  for (i in 1:length(tmp)) {
    method_id <- add_method(obj = tmp[[i]], conn = conn)
    
  }
  # Add samples
  for (i in 1:length(inport_relationships$import_samples)) {
    
  }
  # Add descriptions
  # - ms_description
  # - chromatography description
  # Add data
  # Add QC if appropriate
  return(import_relationships)
}

#' Utility function to add a record
#'
#' Adds a record to a database table and returns the resulting table ID if
#' successful. Values should be provided as a named vector of the values to add.
#' No data coercion is performed, relying on the database schema or
#' preprocessing to ensure data integrity.
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
#' @param values named vector of the values being added
#' @param db_conn connection object (default: con)
#'
#' @return
#' @export
#'
#' @examples
add_and_get_id <- function(db_table, values, db_conn = con, ignore = FALSE) {
  log_it("info", glue("Adding a record to {db_table}."))
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table = list(c("mode", "character"), c("length", 1)),
        # values   = list(c("mode", "vector"), c("n>=", 1)),
        values   = list(c("n>=", 1)),
        db_conn  = list(c("length", 1)),
        ignore   = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn = "add_and_get_id"
    )
    stopifnot(arg_check$valid)
  }
  # Make sure required values are present
  values <- verify_import_columns(
    db_table   = db_table,
    values     = values,
    names_only = FALSE,
    db_conn    = con
  )
  
  # Try the insertion
  res <- try(
    build_db_action(
      action     = "insert",
      table_name = db_table,
      conn       = db_conn,
      values     = values,
      ignore     = ignore
    )
  )
  if (class(res) == "try-error") {
    log_it("error", glue('There was an error adding values to table "{db_table}".'))
    return(res)
  }
  
  res_id <- try(
    build_db_action(
      action         = "get_id",
      table_name     = db_table,
      conn           = db_conn,
      match_criteria = as.list(values),
      and_or         = "AND"
    )
  )
  if (class(res_id) == "try-error") {
    tmp <- values
    blanks <- tmp %in% c("", "null", "NULL", "NA", "na", NA)
    tmp[!blanks] <- paste0("= ", tmp[!blanks])
    tmp[blanks] <- "is null"
    msg_text <- paste(names(tmp), tmp)
    log_it("error",
           sprintf('Cannot retrieve ID from table "%s" using values:\n\t%s\n',
                   db_table,
                   paste0(msg_text, collapse = "\n\t")
           )
    )
    stop()
  } else {
    this_id <- tail(res_id, 1)
    if (length(res_id) > 1) {
      log_it("warn", glue("Multiple IDs ({format_list_of_names(res_id)}) match provided values. Using {this_id}."))
    }
    log_it("success", glue("Record added to {db_table} as ID {this_id}."))
    return(this_id)
  }
}

#' Import software settings
#'
#' Part of the standard import pipeline, this ensures that tables
#' `conversion_software_linkage` and `conversion_software_settings` are updated
#' appropriately and flexibly.
#'
#' @param obj CHR vector describing settings or a named LIST with names matching
#'   column names in table conversion_software_settings.
#' @param conn connection object (default: con)
#'
#' @return status of the insertion
#' @export
#'
#' @examples
add_software_settings <- function(obj, conn = con, software_settings_name = "msconvertsettings") {
  # Argument validation relies on verify_args
  if (software_settings_name %in% names(obj)) {
    obj <- obj[[software_settings_name]]
  } else {
    msg <- sprintf('"%s" not found in the namespace of this object. Using obj directly.', software_settings_name)
    log_it("warn", msg)
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = c(conn, software_settings_name),
      conditions = list(
        conn                   = list(c("length", 1)),
        software_settings_name = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "insert_software_settings"
    )
    stopifnot(arg_check$valid)
  }
  # Add linkage record and get new ID for inclusion in conversion_software_settings
  use_timestamp <- c(ts = as.numeric(Sys.time()) * 1000)
  linkage_id <- add_and_get_id(
    db_table = "conversion_software_linkage",
    values   = use_timestamp,
    db_conn  = conn,
    ignore   = FALSE
  )
  
  # Add entries to conversion_software_settings
  if (is.vector(obj)) {
    values <- lapply(
      obj,
      function(x) {
        c(linkage_id, x) %>%
          setNames(c("linkage_id", "setting_value"))
      }
    )
  }
  res <- try(
    build_db_action(con,
                    action = "INSERT",
                    table_name = "conversion_software_settings",
                    values = values)
  )
  if (class(res) != "try-error") {
    msg <- sprintf("Records added to conversion_software_settings under linkage id %s.", linkage_id)
    log_it("success", msg)
  }
  return(linkage_id)
}

#' Add a sample via import
#'
#' Part of the data import routine. Adds a record to the "samples" table with
#' the values provided in the JSON import template. Uses [verify_sample_class]
#' and [verify_contributor] to parse foreign key relationships, [add_method] to
#' add a record to ms_methods to get the proper id, and [add_software_settings]
#' to insert records into and get the proper conversion software linkage id from
#' tables "conversion_software_settings" and "conversion_software_linkage" if
#' appropriate.
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param conn connection object (default: con)
#'
#' @return INT scalar if successful, result of the call to [add_and_get_id]
#'   otherwise
#'   
add_sample <- function(obj, conn = con, name_is = "sample") {
  log_it("info", "Preparing sample entry with add_sample().")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        obj                    = list(c("n>=", 1)),
        conn                   = list(c("length", 1)),
        name_is = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "add_sample"
    )
    stopifnot(arg_check$valid)
  }
  needed <- unlist(IMPORT_HEADERS[grep("software|samples|method", names(IMPORT_HEADERS))])
  obj_components_present <- needed %in% names(obj)
  if (!all(obj_components_present)) {
    stop(
      sprintf("Object is missing%s required component%s named %s.",
              ifelse(sum(!obj_components_present) == 1, " a", ""),
              ifelse(sum(!obj_components_present) > 1, "s", ""),
              format_list_of_names(needed[!obj_components_present])
      )
    )
  }
  
  obj_sample           <- obj[[name_is]]
  
  this_sample_class_id <- verify_sample_class(obj_sample$sample_class)
  this_contributor_id  <- verify_contributor(obj_sample$data_generator)
  if ("generation_type" %in% names(obj_sample)) {
    this_generation_type <- obj_sample$generation_type
  } else {
    if (interactive()) {
      this_generation_type <- select.list(
        title = "No generation type was provided. Please select one.",
        choices = tbl(con, "norm_generation_type") %>%
          pull(name)
      )
      this_generation_type <- build_db_action(
        action = "get_id",
        table_name = "norm_generation_type",
        match_criteria = list(name = this_generation_type)
      )
    } else {
      stop("No generation type (in silico or empirical) provided.")
    }
  }
  this_software_id     <- add_software_settings(obj)
  
  log_it("info", "Building sample entry values...")
  sample_values <- c(
    name                            = obj_sample$name,
    description                     = obj_sample$description,
    sample_class_id                 = this_sample_class_id,
    source_citation                 = obj_sample$source,
    sample_contributor              = this_contributor_id,
    generation_type                 = this_generation_type,
    generated_on                    = obj_sample$starttime,
    software_conversion_settings_id = this_software_id,
    ms_methods_id                   = if ('method_id' %in% names(obj_sample)) {
        obj_sample$method_id
      } else {
        add_method(obj)
      }
  )
  is_single_record <- all(unlist(lapply(sample_values, length)) == 1)
  log_it("info", glue("Sample value lengths {ifelse(is_single_record, 'are', 'are not')} appropriate."))
  sample_id <- add_and_get_id(
    db_table = "samples",
    values   = sample_values,
    db_conn  = conn,
    ignore   = FALSE
  )
  return(sample_id)
}

#' Add an ms_method record via import
#'
#' Part of the data import routine. Adds a record to the "ms_methods" table with
#' the values provided in the JSON import template. Makes extensive uses of
#' [resolve_normalization_value] to parse foreign key relationships.
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param conn connection object (default: con)
#'
#' @return INT scalar if successful, result of the call to [add_and_get_id]
#'   otherwise
#'   

add_method <- function(obj, conn = con, name_is = "massspectrometry") {
  log_it("info", "Preparing method entry with add_method().")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        obj  = list(c("n>=", 1)),
        conn = list(c("length", 1)),
        name_is = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "add_method"
    )
    stopifnot(arg_check$valid)
  }
  needed <- IMPORT_HEADERS$method
  if (!all(needed %in% names(obj))) {
    log_it("error",
           sprintf("Object does not have children with all defined names (%s).",
                   paste0(needed, collapse = ", "))
    )
    return(NULL)
  }
  
  if (name_is %in% names(obj)) {
    obj_method <- obj[[name_is]]
  } else {
    log_it("warn", glue('"\t{name_is}" not found in the namespace of this object. Using directly.'))
    # needed <- dbListFields(con, "ms_methods")[-1]
    needed <- c("ionization", "voltage", "voltage_units", "polarity")
    if (!all(needed %in% names(obj))) {
      stop("Not all needed names are present.")
    }
    obj_method <- obj
  }
  
  log_it("info", "Building methods entry values...")
  ms_method_values <- c(
    ionization    = resolve_normalization_value(
      obj_method$ionization,
      ref_table_from_map("ms_methods", "ionization")),
    voltage       = obj_method$voltage,
    voltage_units = resolve_normalization_value(
      obj_method$vunits,
      ref_table_from_map("ms_methods", "voltage_units")),
    polarity      = resolve_normalization_value(
      obj_method$polarity,
      ref_table_from_map("ms_methods", "polarity")),
    ce_value      = obj_method$ce_value,
    ce_units      = resolve_normalization_value(
      obj_method$ce_units,
      ref_table_from_map("ms_methods", "ce_units")),
    ce_desc       = resolve_normalization_value(
      obj_method$ce_desc,
      ref_table_from_map("ms_methods", "ce_desc")),
    fragmentation = resolve_normalization_value(
      obj_method$fragmode,
      ref_table_from_map("ms_methods", "fragmentation")),
    ms2_type      = resolve_normalization_value(
      obj_method$ms2exp,
      ref_table_from_map("ms_methods", "ms2_type")),
    has_qc_method = as.numeric("qcmethod" %in% names(obj)),
    citation      = obj_method$source
  )
  # Insert method if appropriate
  added <- add_and_get_id(
    db_table = "ms_methods",
    values   = ms_method_values,
    db_conn  = conn
  )
  if (class(add_description) == 'try-error') {
    log_it("warn", glue('Unable to add values ({format_list_of_names(add_description)}) to table "ms_descriptions".'))
  }
  return(added)
}

add_description <- function(obj, type) {
  type <- match.arg(type, c("ms", "chromatography"))
  type <- paste0(type, "_descriptions")
  log_it("trace", glue('Adding descriptions to table "{type}".'))
  res <- try(
    build_db_action(
      action = "insert",
      table_name = type,
      conn = con,
      values = obj
    )
  )
  if (class(res) == "try-error") {
    stop(glue('There was an issue adding records to table "{type}".'))
  }
}

add_ms_data <- function(obj) {
  
}

add_qc_methods <- function(obj, ms_method_id, name_is = "qcmethod", required = c("name", "value", "source")) {
  if ("data.frame" %in% class(obj)) {
    tmp <- obj
  } else if (class(obj) == "list") {
    if (name_is %in% names(obj)) {
      tmp <- obj[[name_is]]
    } else if (length(obj) > 1) {
      log_it("warn", "The object provided is longer than length 1; only the first element will be used.")
      if (name_is %in% names(obj[[1]])) {
        tmp <- obj[[1]][[name_is]]
      } else {
        stop(sprintf('The name "%s" was not found in the first element of "obj" and is required.', name_is))
      }
    }
  }
  if (!all(required %in% names(tmp))) {
    log_it("error", sprintf('Required column%s %s %s not present.',
                            ifelse(length(required > 1), "s", ""),
                            format_list_of_names(required),
                            ifelse(length(required > 1), "are", "is")))
  }
  tmp <- tmp %>%
    filter(value == TRUE) %>%
    select(all_of(required))
  return(tmp)
}

add_qc_data <- function(obj, sample_id) {
  values <- lapply(obj,
                   function(x) {
                     x %>%
                       mutate_all("as.character") %>%
                       pivot_longer(cols = -parameter)
                   }) %>%
    bind_rows() %>%
    mutate(sample_id = sample_id) %>%
    relocate(ms_data_id)
  res <- try(
    build_db_action(
      action = "insert",
      table_name = "qc_data",
      values = values
    )
  )
  if (class(res) == "try-error") {
    stop('There was an issue adding records to table "qc_data".')
  }
}

get_uniques <- function(import_obj, aspect) {
  out <- lapply(import_obj, function(x) x[[aspect]])
  out_distinct <- out %>%
    unique() %>%
    lapply(function(x) {
      c(x,
        applies_to = list(unname(which(sapply(out, FUN = identical, x) == TRUE))))
  })
  return(out_distinct)
}


#' Delete a sample
#'
#' Removes a sample from the database and associated records in ms_methods,
#' conversion_software_settings, and conversion_software_linkage.
#'
#' @param sample_ids INT vector of IDs to remove from the samples table.
#' @param db_conn connection object (default: con)
#'
#' @return
#' @export
#'
remove_sample <- function(sample_ids, db_conn = con) {
  log_it("info", glue('Removing samples with ids "format_list_of_names{sample_ids}" with remove_sample().'))
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
  dat <- tbl(con, "samples") %>% filter(id %in% sample_ids) %>% collect()
  build_db_action('delete', 'samples', match_criteria = list(id = sample_ids))
  build_db_action('delete', 'ms_methods', match_criteria = list(id = unique(dat$ms_methods_id)))
  build_db_action('delete', 'conversion_software_settings', match_criteria = list(linkage_id = unique(dat$software_conversion_settings_id)))
  build_db_action('delete', 'conversion_software_linkage', match_criteria = list(id = unique(dat$software_conversion_settings_id)))
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
#'
#' @return writes a file to the project directory (based on the found location
#'   of `file_name`) with the JSON structure
#' @export
#' 
make_requirements <- function(example_import,
                              file_name = "import_requirements.json",
                              archive = TRUE,
                              retain_in_R = TRUE) {
  if (class(example_import) == "character") {
    log_it("info", sprintf('Assuming "%s" refers to a file name, and that the file is parseable as JSON.', example_import))
    f_name <- list.files(pattern = example_import, recursive = TRUE, full.names = TRUE)
    if (length(f_name) == 0) {
      log_it("error", sprintf('No file matching "%s" was located in the project directory.', example_import))
      stop()
    } else if (length(f_name) > 1) {
      log_it("warn", sprintf('Multiple files matching "%s" were located in the project directory.', example_import))
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
  f_name <- list.files(pattern = file_name, full.names = TRUE, recursive = TRUE)
  if (length(f_name) > 1) {
    f_name <- f_name[1]
    log_it(
      "warn",
      sprintf(
        "Multiple import requirements files located. Only the file at (%s) will be replaced.",
        f_name
      )
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
#' @note If columns are defined as required in the schema are not present, this
#'   will fail with an informative message about which columns were missing.
#'
#' @note If columns are provided that do not match the schema, they will be
#'   stripped away in the return value.
#'
#' @param db_table CHR scalar of the table name
#' @param values LIST or CHR vector of values to add. If `names_only` is TRUE,
#'   values are directly interpreted as column names. Otherwise, all values
#'   provided must be named.
#' @param names_only LGL scalar of whether to treat entries of `values` as the
#'   column names rather than the column values (default: FALSE)
#' @param require_all LGL scalar of whether to require all columns (except the
#'   assumed primary key column of "id") or only those defined as "NOT NULL"
#'   (default: FALSE)
#' @param db_conn connection object (default: con)
#'
#' @return An object of the same type as `values` with extraneous values (i.e.
#'   those not matching a database column header) stripped away.
#' @export
#'
#' @examples
verify_import_columns <- function(db_table, values, names_only = FALSE, require_all = FALSE, db_conn = con) {
  log_it("info", glue('Verifying column requirements for table "{db_table}" with verify_import_columns().'))
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table    = list(c("mode", "character"), c("length", 1)),
        values      = list(c("n>=", 1)),
        names_only  = list(c("mode", "logical"), c("length", 1)),
        require_all = list(c("mode", "logical"), c("length", 1)),
        db_conn     = list(c("length", 1))
      ),
      from_fn = "verify_import_columns"
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
    stop(glue('Required column{ifelse(length(missing_columns) > 1, "s", "")} {ifelse(length(missing_columns) > 1, "are", "is")} missing: {format_list_of_names(missing_columns)}.'))
  } else {
    valid_columns <- provided %in% table_info$name
    if (!all(valid_columns)) {
      extra_columns <- provided[!valid_columns]
      log_it("warn", glue('Extra column{ifelse(length(extra_columns) > 1, "s", "")} {ifelse(length(extra_columns) > 1, "were", "was")} provided and will be ignored: {format_list_of_names(extra_columns)}.'))
      stop()
    }
    return(values[valid_columns])
  }
}

#' Verify an import file's properties
#'
#' @param obj 
#' @param requirements_obj CHR scalar of the 
#' @param file_name CHR scalar of the name of a file holding 
#' @param nested 
#'
#' @return
#' @export
#'
#' @examples
verify_import_requirements <- function(obj, requirements_obj = "requirements", file_name = "import_requirements.json", nested = FALSE) {
  # Argument validation relies on verify_args
  if (all(exists("verify_args"), !nested)) {
    arg_check <- verify_args(
      args       = list(obj, file_name, nested),
      conditions = list(
        obj       = list(c("mode", "list")),
        file_name = list(c("mode", "character"), c("length", 1)),
        nested    = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn = "verify_import_requirements"
    )
    stopifnot(arg_check$valid)
  }
  # TODO fix this to allow direct use of requirements object as well as name reference
  if (exists(requirements_obj)) {
    reqs <- eval(sym(requirements_obj))
  } else {
    f_name <- list.files(pattern = file_name, recursive = TRUE, full.names = TRUE)
    if (length(f_name) == 1) {
      reqs <- fromJSON(read_file(f_name))
      requirements <<- reqs
    } else {
      if (interactive()) {
        f_name <- select.list(
          title = "Please select one requirements file.",
          choices = f_name
        )
      } else {
        log_it("error", "Multiple files matching the requirements name were identified. Please be more specific.")
        stop()
      }
    }
  }
  hard_reqs    <- names(reqs)[which(lapply(reqs, function(x) x$required) == TRUE)]
  all_required <- all(hard_reqs %in% names(obj))
  full_detail  <- all(names(reqs) %in% names(obj))
  extra_cols   <- !names(obj) %in% names(reqs)
  extra        <- any(extra_cols)
  extra_cols   <- names(obj)[extra_cols]
  if (all_required) {
    out    <- list(all_required = all_required, full_detail = full_detail, extra = extra, extra_cols = list(extra_cols))
    nested <- FALSE
  } else {
    all_required <- all(hard_reqs %in% unique(unlist(lapply(obj, names))))
    if (all_required) {
      log_it("info", "The provided object contains a nested list where required names were located.")
      nested <- TRUE
      out <- lapply(obj, verify_import_requirements, nested = TRUE)
    } else {
      log_it("error", "Not all required columns were present.")
      stop()
    }
  }
  if (all(extra, !nested)) {
    log_it("warn", "Extraneous elements identified; these will be removed from the import file.")
    obj <- obj[which(names(obj) %in% names(reqs))]
  }
  if (nested) {
    out <- out %>%
      bind_rows() %>%
      mutate(applies_to = names(obj)) %>%
      relocate(applies_to)
  } else {
    out <- as_tibble(out)
  }
  return(out)
}
