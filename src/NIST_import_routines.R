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
  this_software_id     <- add_software_settings(obj)
  
  log_it("info", "Building sample entry values...")
  sample_values <- c(
    name                            = obj_sample$name,
    description                     = obj_sample$description,
    sample_class_id                 = this_sample_class_id,
    source_citation                 = obj_sample$source,
    sample_contributor              = this_contributor_id,
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
  # needed <- IMPORT_HEADERS$method
  # if (!all(needed %in% names(obj))) {
  #   log_it("error",
  #          sprintf("Object does not have children with all defined names (%s).",
  #                  paste0(needed, collapse = ", "))
  #   )
  #   return(NULL)
  # }
  
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
    log_it("warn", glue('Unable to add values ({format_list_names(add_description}) to table "ms_descriptions".'))
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

add_qc_data <- function(obj) {
  values <- lapply(obj,
                   function(x) {
                     x %>%
                       mutate_all("as.character") %>%
                       pivot_longer(cols = -parameter)
                   }) %>%
    bind_rows() %>%
    purrr::transpose()
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

#' Verify column names for import
#'
#' This function validates that all required columns are present prior to
#' importing into a database column by examining provided values against the
#' database schema. This is more of a sanity check on other functions than
#' anything, but also strips extraneous columns to meet the needs of an INSERT
#' action. The input to `values` should be either a LIST or named CHR vector of
#' values for insertion or a CHR vector of the column names.
#'
#' @note If columns are defined as required in the schema are not present, this will
#' fail with an informative message about which columns were missing.
#'
#' @note If columns are provided that do not match the schema, they will be stripped
#' away in the return value.
#'
#' @param db_table CHR scalar of the table name
#' @param values LIST or CHR vector of values to add. If `names_only` is TRUE,
#'   values are directly interpreted as column names. Otherwise, all values
#'   provided must be named.
#' @param names_only LGL scalar of whether to treat entries of `values` as the
#'   column names rather than the column values (default: FALSE)
#' @param db_conn connection object (default: con)
#'
#' @return An object of the same type as `values` with extraneous values (i.e.
#'   those not matching a database column header) stripped away.
#' @export
#'
#' @examples
verify_import_columns <- function(db_table, values, names_only = FALSE, db_conn = con) {
  log_it("info", glue('Verifying column requirements for table "{db_table}" with verify_import_columns().'))
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table = list(c("mode", "character"), c("length", 1)),
        values   = list(c("n>=", 1)),
        names_only = list(c("mode", "logical"), c("length", 1)),
        db_conn  = list(c("length", 1))
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
  table_info      <- pragma_table_info(db_table, db_conn = db_conn)
  table_columns   <- table_info$name[-which(table_info$name == "id")]
  table_needs     <- table_info$name[which(table_info$notnull == 1)]
  provided        <- if (names_only) values else names(values)
  columns_present <- table_needs %in% provided
  if (!all(columns_present)) {
    missing_columns <- table_needs[!columns_present]
    stop(glue('Required {ifelse(length(missing_columns) > 1, "fields", "field")} {format_list_of_names(missing_columns)} {ifelse(length(missing_columns) > 1, "are", "is")} missing.'))
  } else {
    valid_columns <- provided %in% table_columns
    if (!all(valid_columns)) {
      extra_columns <- provided[!valid_columns]
      log_it("warn", glue('Extra {ifelse(length(extra_columns) > 1, "fields", "field")} {format_list_of_names(extra_columns)} {ifelse(length(extra_columns) > 1, "were", "was")} provided and will be ignored.'))
    }
    return(values[valid_columns])
  }
}
