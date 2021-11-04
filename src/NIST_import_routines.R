import_from_json <- function(obj, conn = con, ...) {
  
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
  use_timestamp <- as.numeric(Sys.time()) * 1000
  res <- try(
    build_db_action(action       = "INSERT",
                    table_name   = "conversion_software_linkage",
                    values       = list(ts = use_timestamp),
                    conn         = conn)
  )
  linkage_id <- build_db_action(
    conn,
    action = "GET_ID",
    table_name = "conversion_software_linkage",
    match_criteria = list(ts = use_timestamp)
  )
  if (class(res) != "try-error") {
    msg <- sprintf("Record added to conversion_software_linkage as linkage id %s.", linkage_id)
    log_it("success", msg)
  }
  
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
    msg <- sprintf("Record added to conversion_software_settings under linkage id %s.", linkage_id)
    log_it("success", msg)
  }
  return(linkage_id)
}

add_sample <- function(obj, conn = con) {
  # Argument validation relies on verify_args
  log_it("info", "Preparing sample entry with add_sample().")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        obj                    = list(c("n>=", 1)),
        conn                   = list(c("length", 1))
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
  
  obj_sample           <- obj$sample
  
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
    ms_methods_id                   = add_method(obj)
  )
  is_single_record <- all(unlist(lapply(sample_values, length)) == 1)
  log_it("info", glue("Sample value lengths {ifelse(is_single_record, 'are', 'are not')} appropriate."))
  res <- try(
    build_db_action(
      "INSERT",
      "samples",
      values = sample_values
    )
  )
  if (class(res) == "try-error") {
    return(res)
  } else {
    return(sample_values)
  }
}

add_method <- function(obj, conn = con) {
  # Argument validation relies on verify_args
  log_it("info", "Preparing method entry with add_method().")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        obj                    = list(c("n>=", 1)),
        conn                   = list(c("length", 1))
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
  
  obj_method <- obj$massspectrometry
  
  
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
      obj_method$fragmode,
      ref_table_from_map("ms_methods", "fragmentation")),
    has_qc_method = as.numeric("qcmethod" %in% names(obj)),
    citation      = obj_method$source
  )
  
  res <- try(
    build_db_action(
      action     = "insert",
      table_name = "ms_methods",
      values     = ms_method_values
    )
  )
  if (class(res) == "try-error") {
    log_it("error", 'There was an error adding values to table "ms_methods".')
    return(res)
  }
  
  res_id <- try(
    build_db_action(
      action         = "get_id",
      table_name     = "ms_methods",
      match_criteria = as.list(ms_method_values),
      and_or         = "AND"
    )
  )
  res_id <- tail(res_id, 1)
  if (class(res_id) == "try-error") {
    tmp <- ms_method_values
    blanks <- tmp %in% c("", "null", "NULL", "NA", "na", NA)
    tmp[!blanks] <- paste0("= ", tmp[!blanks])
    tmp[blanks] <- "is null"
    msg_text <- paste(names(tmp), tmp) 
    log_it("error",
           sprintf('Cannot retrieve "ms_methods.id" from table "ms_methods" using values:\n\t%s\n',
                   paste0(msg_text, collapse = "\n\t")
           )
    )
    stop()
  } else {
    log_it("success", sprintf("Record added to ms_methods as method id %s", res_id))
    return(res_id)
  }
}
