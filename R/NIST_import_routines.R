#' Import one or more files from the NIST Method Reporting Tool for NTA
#'
#' This function serves as a single entry point for data imports. It is
#' predicated upon the NIST import routine defined here and relies on several
#' assumptions. It is intended ONLY as an interactive manner of importing n data
#' files from the NIST Method Reporting Tool for NTA (MRT NTA).
#'
#' Import files should be in JSON format as created by the MRT NTA. Examples are
#' provided in the "example" directory of the project.
#'
#' Defaults for this release are set throughout as of the latest database
#' schema, but left here as arguments in case those should change, or slight
#' changes are made to column and table names.
#'
#' @note Many calls within this function are executed as do.call with a filtered
#'   argument list based on the names of formals for the called function.
#'   Several arguments to those functions are also left as the defaults set
#'   there; names must match exactly to be passed in this manner. See the list
#'   of inherited parameters.
#'
#' @inheritParams verify_import_requirements
#' @inheritParams resolve_method
#' @inheritParams resolve_description_NTAMRT
#' @inheritParams resolve_sample
#' @inheritParams resolve_mobile_phase_NTAMRT
#' @inheritParams resolve_qc_methods_NTAMRT
#' @inheritParams resolve_peaks
#' @inheritParams resolve_compounds
#' @inheritParams resolve_fragments_NTAMRT
#'
#' @param import_object nested LIST object of JSON data to import; this import
#'   routine was built around output from the NTA MRT (default: NULL) - note you
#'   may supply either import object or file_name
#' @param file_name external file in JSON format of data to import;  this import
#'   routine was built around output from the NTA MRT (default: NULL) - note you
#'   may supply either import object or file_name
#' @param exclude_missing_required LGL scalar of whether or not to skip imports
#'   missing required information (default: FALSE); if set to TRUE, this will
#'   override the setting for `stop_if_missing_required` and the import will
#'   continue with logging messages for which files were incomplete
#' @param stop_if_missing_required LGL scalar of whether or not to to stop the
#'   import routine if a file is missing required information (default: TRUE)
#' @param include_if_missing_recommended LGL scalar of whether or not to include
#'   imports missing recommended information (default: FALSE)
#' @param stop_if_missing_recommended LGL scalar of whether or not to to stop
#'   the import routine if a file is missing recommended information (default:
#'   TRUE)
#' @param ignore_insert_conflicts LGL scalar of whether to ignore insert
#'   conflicts during the qc methods and qc data import steps (default: TRUE)
#' @param instrument_properties_table CHR scalar name of the database table
#'   holding instrument property information for a given method (default:
#'   "instrument_properties")
#' @param sample_info_in CHR scalar name of the element within `import_object`
#'   containing samples information
#' @param contributor_in CHR scalar name of the element within
#'   `import_object[[sample_info_in]]` containing contributor information
#'   (default: "data_generator")
#' @param contributors_table CHR scalar name of the database table holding
#'   contributor information (default: "contributors")
#' @param sample_aliases named CHR vector of aliases with names matching the
#'   alias, and values of the alias reference e.g. c("ACU1234" = "NIST
#'   Biorepository GUAID") which can be virutally any reference text; it is
#'   recommended that the reference be to a resolver service if connecting with
#'   external data sources (default: NULL)
#' @param mobile_phases_in CHR scalar name of the database table holding mobile
#'   phase and chromatographic information (default: "chromatography")
#' @param id_mix_by regex CHR to identify mobile phase mixtures (default:
#'   "^mp*[0-9]+" matches the generated mixture names)
#' @param peaks_in CHR scalar name of the element within `import_object`
#'   containing peak information
#' @param fragment_alias_type_norm_table CHR scalar name of the alias reference
#'   normalization table, by default the return of
#'   \code{ref_table_from_map(fragment_aliases_table, "alias_type")}
#' @param type The type of chemical structure notation (default: SMILES)
#' @param compound_aliases_in CHR scalar name of where compound aliases are
#'   located within the import (default: "compound_aliases"), passed to
#'   [resolve_compounds] as "norm_alias_table"
#' @param compound_aliases_table CHR scalar name of the alias reference table to
#'   use when assigning compound aliases (default: "compound_aliases") passed to
#'   [resolve_compounds] as "compounds_table"
#' @param compound_alias_type_norm_table CHR scalar name of the alias reference
#'   normalization table, by default the return of
#'   \code{ref_table_from_map(compound_aliases_table, "alias_type")}
#'
#' @return Console logging if enabled and interactive prompts when user
#'   intervention is required. There is no formal return as it executes database
#'   actions.
#' @export
#' 
full_import <- function(import_object                  = NULL,
                        file_name                      = NULL,
                        db_conn                        = con,
                        exclude_missing_required       = FALSE,
                        stop_if_missing_required       = TRUE,
                        include_if_missing_recommended = FALSE,
                        stop_if_missing_recommended    = TRUE,
                        ignore_extra                   = TRUE,
                        ignore_insert_conflicts        = TRUE,
                        requirements_obj               = "import_requirements",
                        method_in                      = "massspectrometry",
                        ms_methods_table               = "ms_methods",
                        instrument_properties_table    = "instrument_properties",
                        sample_info_in                 = "sample",
                        sample_table                   = "samples",
                        contributor_in                 = "data_generator",
                        contributors_table             = "contributors",
                        sample_aliases                 = NULL,
                        generation_type                = NULL,
                        generation_type_norm_table     = ref_table_from_map(sample_table, "generation_type"),
                        mass_spec_in                   = "massspectrometry",
                        chrom_spec_in                  = "chromatography",
                        mobile_phases_in               = "chromatography",
                        qc_method_in                   = "qcmethod",
                        qc_method_table                = "qc_methods",
                        qc_method_norm_table           = ref_table_from_map(qc_method_table, "name"),
                        qc_references_in               = "source",
                        qc_data_in                     = "qc",
                        qc_data_table                  = "qc_data",
                        carrier_mix_names              = NULL,
                        id_mix_by                      = "^mp*[0-9]+",
                        mix_collection_table           = "carrier_mix_collections",
                        mobile_phase_props             = list(
                          in_item = "chromatography",
                          db_table = "mobile_phases",
                          props = c(
                            flow = "flow",
                            flow_units = "flowunits",
                            duration = "duration",
                            duration_units = "durationunits"
                          )
                        ),
                        carrier_props                  = list(
                          db_table = "carrier_mixes",
                          norm_by  = ref_table_from_map("carrier_mixes", "component"),
                          alias_in = "carrier_aliases",
                          props    = c(
                            id_by       = "solvent",
                            fraction_by = "fraction"
                          )
                        ),
                        additive_props                 = list(
                          db_table = "carrier_additives",
                          norm_by  = ref_table_from_map("carrier_additives", "component"),
                          alias_in = "additive_aliases",
                          props    = c(
                            id_by     = "add$",
                            amount_by = "_amount",
                            units_by  = "_units")
                        ),
                        exclude_values                 = c("none", "", NA),
                        peaks_in                       = "peak",
                        peaks_table                    = "peaks",
                        software_timestamp             = NULL,
                        software_settings_in           = "msconvertsettings",
                        ms_data_in                     = "msdata",
                        ms_data_table                  = "ms_data",
                        unpack_spectra                 = FALSE,
                        unpack_format                  = c("separated", "zipped"),
                        ms_spectra_table               = "ms_spectra",
                        linkage_table                  = "conversion_software_peaks_linkage",
                        settings_table                 = "conversion_software_settings",
                        as_date_format                 = "%Y-%m-%d %H:%M:%S",
                        format_checks                  = c("ymd_HMS", "ydm_HMS", "mdy_HMS", "dmy_HMS"),
                        min_datetime                   = "2000-01-01 00:00:00",
                        fragments_in                   = "annotation",
                        fragments_table                = "annotated_fragments",
                        fragments_sources_table        = "fragment_sources",
                        fragments_norm_table           = "norm_fragments",
                        citation_info_in               = "fragment_citation",
                        inspection_info_in             = "fragment_inspections",
                        inspection_table               = "fragment_inspections",
                        generate_missing_aliases       = TRUE,
                        fragment_aliases_in            = "fragment_aliases",
                        fragment_aliases_table         = "fragment_aliases",
                        fragment_alias_type_norm_table = ref_table_from_map(fragment_aliases_table, "alias_type"),
                        inchi_prefix                   = "InChI=1S/",
                        rdkit_ref                      = ifelse(exists("PYENV_REF"), PYENV_REF, "rdk"),
                        rdkit_ns                       = "rdk",
                        rdkit_make_if_not              = TRUE,
                        rdkit_aliases                  = c("inchi", "inchikey"),
                        mol_to_prefix                  = "MolTo",
                        mol_from_prefix                = "MolFrom",
                        type                           = "smiles",
                        compounds_in                   = "compounddata",
                        compounds_table                = "compounds",
                        compound_category              = NULL,
                        compound_category_table        = "compound_categories",
                        compound_aliases_in            = "compound_aliases",
                        compound_aliases_table         = "compound_aliases",
                        compound_alias_type_norm_table = ref_table_from_map(compound_aliases_table, "alias_type"),
                        fuzzy                          = FALSE,
                        case_sensitive                 = TRUE,
                        ensure_unique                  = TRUE,
                        require_all                    = FALSE,
                        import_map                     = IMPORT_MAP,
                        log_ns                         = "db") {
  # Check connection and requirements ----
  stopifnot(active_connection(db_conn))
  log_fn("start")
  if (all(is.null(file_name), is.null(import_object))) {
    stop('One of either "import_obj" or "file_name" must be provided.')
  }
  if (is.null(import_object)) {
    if (all(file.exists(file_name), grepl(".json", tolower(file_name)))) {
      import_object <- jsonlite::read_json(file_name)
    } else {
      if (!file.exists(file_name)) stop("No file with that name exists. Did you need to run `file.choose()`?")
      stop("The file extension must be '.json' or '.JSON' to import.")
    }
    import_type <- "file"
  } else {
    import_object <- import_object
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
                                      format_list_of_names(missing_requirements, add_quotes = TRUE),
                                      ifelse(length(missing_requirements) > 1, " were", " was"),
                                      ' missing from import ',
                                      import_type,
                                      ifelse(import_object == "import object",
                                             '',
                                             sprintf(' "%s"', import_object)),
                                      '. ')) %>%
      distinct(import_object, req_msg)
    for (x in missing_required$req_msg) log_it("error", msg = x, "db")
    if (exclude_missing_required) {
      to_ignore <- c(to_ignore, which(!meets_requirements$has_all_required))
      log_it("warn",
             sprintf('The import will exclude %s of %s %s%s (%s) which did not contain all required information.',
                     nrow(missing_required),
                     length(import_object),
                     import_type,
                     ifelse(nrow(missing_required) > 1, "s", ""),
                     format_list_of_names(missing_required$import_object, add_quotes = TRUE)
             ),
             log_ns)
    } else {
      stop_if_missing_required <- TRUE
    }
    if (stop_if_missing_required) {
      log_it("error",
             sprintf("The import %s must contain all required information defined in '%s'; call with 'exclude_missing_required = TRUE' and 'stop_if_missing_required = FALSE' to exclude these items.",
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
                                       format_list_of_names(missing_detail, add_quotes = TRUE),
                                       ifelse(length(missing_detail) > 1, " were", " was"),
                                       ' missing from import ',
                                       import_type,
                                       ' "',
                                       import_object,
                                       '".')) %>%
      distinct(import_object, recc_msg)
    for (x in missing_recommended$recc_msg) log_it("warn", msg = x, "db")
    log_it("warn",
           sprintf('The import will %s %s of %s %s%s (%s) which did not contain all recommended information because include_if_missing_recommended = %s.',
                   ifelse(include_if_missing_recommended, "include", "ignore"),
                   nrow(missing_recommended),
                   length(import_object),
                   import_type,
                   ifelse(nrow(missing_recommended) > 1, "s", ""),
                   format_list_of_names(missing_recommended$import_object, add_quotes = TRUE),
                   include_if_missing_recommended),
           log_ns)
    if (!include_if_missing_recommended) {
      to_ignore <- c(to_ignore, which(!meets_requirements$has_full_detail))
    }
    if (stop_if_missing_recommended) {
      log_it("error", "Include missing recommended data and try again, or call again with 'stop_if_missing_recommended = FALSE'.", log_ns)
      return(invisible(NULL))
    }
  }
  if (length(to_ignore) > 0) {
    import_object <- import_object[-unique(to_ignore)]
  }
  # Ensure the database map is present
  if (!exists("db_map")) {
    log_it("warn", "No database map present, generating in the session environment as object `db_map` using `er_map()`.", log_ns)
    db_map <<- er_map(db_conn)
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
  # map contributors ----
  map_contributors_to <- data.frame(provided = character(0), resolved = integer(0))
  # software timestamps ----
  # Grab software timestamps to cut down on import chatter
  unique_software_settings <- get_uniques(import_object, software_settings_in)
  software_timestamps <- vector("list", length(import_object))
  if (is.null(software_timestamp)) {
    generated_timestamp <- lubridate::now(tzone = "UTC") - 1
    generated_timestamp <- generated_timestamp + seq_along(generated_timestamp)
    for (i in 1:length(unique_software_settings)) {
      software_timestamps[unique_software_settings[[i]]$import_object$index] <- generated_timestamp[i]
    }
  } else {
    if (!(length(unique_software_settings) == 1 ||
          length(unique_software_settings) == length(import_object)
    )) {
      log_it("warn", "Conversion software timestamp must either be of length 1 or of length matching that of the import object.")
    } else {
      software_timestamps[1:length(import_object)] <- software_timestamp
    }
  }
  software_timestamps <- software_timestamps %>%
    unlist() %>%
    lubridate::as_datetime() %>%
    lubridate::ymd_hms() %>%
    format(as_date_format)
  func_env <- as.list(environment())
  # loop through import object elements ----
  for (i in 1:length(import_object)) {
    if (length(names(import_object)) > 0) {
      log_it("info", glue::glue("Importing object #{i} of {length(import_object)} with name {names(import_object)[i]}..."), log_ns)
    } else {
      log_it("info", "Importing single object...", log_ns)
    }
    obj <- import_object[[i]]
    # _ID contributor ----
    log_it("info", "Resolving contributors.", log_ns)
    contributor <- obj[[sample_info_in]][[contributor_in]]
    if (contributor %in% map_contributors_to$provided) {
      contributor_id <- map_contributors_to$resolved[map_contributors_to$provided == contributor]
    } else {
      contributor_id <- resolve_normalization_value(contributor, contributors_table)
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
    # _Methods information ----
    # Method info is optional but heavily encouraged 
    # __resolve method node ----
    log_it("info", "Resolving methods node.", log_ns)
    if (method_in %in% names(obj)) {
      ms_method_id <- do.call(
        resolve_method,
        args = append(list(obj = obj),
                      func_env[names(func_env) %in% names(formals(resolve_method))]
        )
      )
    } else {
      ms_method_id <- NA
    }
    if (!is.na(ms_method_id)) {
      # ___mass spec descriptions ----
      if (mass_spec_in %in% names(obj)) {
        log_it("info", "Resolving mass spectrometry descriptions.", log_ns)
        resolve_description_NTAMRT(obj = obj,
                                   method_id = ms_method_id,
                                   mass_spec_in = mass_spec_in,
                                   type = "massspec",
                                   fuzzy = fuzzy,
                                   db_conn = db_conn,
                                   log_ns = log_ns)
      }
      # ___chromatography descriptions ----
      if (chrom_spec_in %in% names(obj)) {
        log_it("info", "Resolving chromatography descriptions.", log_ns)
        resolve_description_NTAMRT(obj = obj,
                                   method_id = ms_method_id,
                                   chrom_spec_in = chrom_spec_in,
                                   type = "chromatography",
                                   fuzzy = TRUE,
                                   db_conn = db_conn,
                                   log_ns = log_ns)
      }
    }
    # _Sample node ----
    # Sample info is required
    # Conveniently carry forward resolved contributor
    if (contributor_in %in% names(obj[[sample_info_in]]) &&
        !is.null(contributor_id) &&
        !is.na(contributor_id) &&
        is.integer(contributor_id) &&
        length(contributor_id) == 1) {
      obj[[sample_info_in]][[contributor_in]] <- build_db_action(
        action = "select",
        table_name = contributors_table,
        column_names = "username",
        match_criteria = list(id = contributor_id)
      )
    }
    # __resolve sample ----
    log_it("info", "Resolving sample node.", log_ns)
    sample_id <- do.call(
      resolve_sample,
      args = append(list(obj = obj,
                         method_id = ms_method_id),
                    func_env[names(func_env) %in% names(formals(resolve_sample))]
      )
    )
    # __import sample aliases ----
    log_it("info", "Resolving sample aliases.", log_ns)
    if (!is.null(sample_aliases)) {
      if (is.list(sample_aliases)) {
        if (length(names(sample_aliases)) == length(sample_aliases)) {
          resolve_sample_aliases(sample_id = sample_id,
                                 values = sample_aliases)
        } else {
          log_it("warn", "Sample aliases were not properly formatted; add them later with resolve_sample_aliases()", log_ns)
        }
      } else if (is.character(sample_aliases)) {
        resolve_sample_aliases(sample_id = sample_id,
                               obj = obj,
                               aliases_in = sample_aliases,
                               db_conn = db_conn,
                               log_ns = log_ns)
      }
    }
    # _Peaks node (includes data) ----
    log_it("info", "Resolving peaks.", log_ns)
    peaks <- do.call(
      resolve_peaks,
      args = append(list(obj = obj,
                         sample_id = sample_id),
                    func_env[names(func_env) %in% names(formals(resolve_peaks))]
      )
    )
    # __instrument properties ----
    # This was previously tacked into the methods section, but was moved to the data node to better tie instrument performance properties to the measurements made.
    log_it("info", "Resolving instrument QC properties.", log_ns)
    if (!is.null(peaks)) {
      tmp <- map_import(import_obj = obj,
                        aspect = instrument_properties_table,
                        import_map = import_map)
      tmp <- tibble(
        peak_id = list(peaks),
        name = names(tmp),
        value = unlist(tmp),
        value_unit = case_when(
          name == "isowidth" ~ "Da",
          name == "msaccuracy" ~ "ppm"
        )
      ) %>%
        unnest(c(peak_id)) %>%
        arrange(peak_id, name)
      res <- try(
        build_db_action(action = "insert",
                        table_name = instrument_properties_table,
                        values = tmp,
                        db_conn = db_conn,
                        log_ns = log_ns,
                        # TODO consider removing in production
                        ignore = TRUE)
      )
    }
    # __QC methods ----
    log_it("info", "Resolving QC methods.", log_ns)
    do.call(
      resolve_qc_methods_NTAMRT,
      args = append(list(obj = obj,
                         peak_id = peaks,
                         ignore = ignore_insert_conflicts),
                    func_env[names(func_env) %in% names(formals(resolve_qc_methods_NTAMRT))]
      )
    )
    # __QC data ----
    log_it("info", "Resolving QC data.", log_ns)
    do.call(
      resolve_qc_data_NTAMRT,
      args = append(list(obj = obj,
                         peak_id = peaks,
                         ignore = ignore_insert_conflicts),
                    func_env[names(func_env) %in% names(formals(resolve_qc_data_NTAMRT))]
      )
    )
    # _Mobile phase node ----
    log_it("info", "Resolving mobile phase node.", log_ns)
    do.call(
      resolve_mobile_phase_NTAMRT,
      args = append(list(obj = obj,
                         method_id = ms_method_id,
                         sample_id = sample_id,
                         peak_id = peaks),
                    func_env[names(func_env) %in% names(formals(resolve_mobile_phase_NTAMRT))]
      )
    )
    # _Compounds node ----
    log_it("info", "Resolving compounds.", log_ns)
    compounds <- do.call(
      resolve_compounds,
      args = append(list(obj = obj,
                         compound_alias_table = compound_aliases_table,
                         norm_alias_table = compound_alias_type_norm_table),
                    func_env[names(func_env) %in% names(formals(resolve_compounds))]
      )
    )
    # _Fragments node ----
    log_it("info", "Resolving fragments.", log_ns)
    fragments <- do.call(
      resolve_fragments_NTAMRT,
      args = append(list(obj = obj,
                         sample_id = sample_id,
                         alias_type_norm_table = fragment_alias_type_norm_table,
                         strip_na = TRUE),
                    func_env[names(func_env) %in% names(formals(resolve_fragments_NTAMRT))]
      )
    )
    # _Peak/fragment/compound connection ----
    log_it("info", "Linking compounds, peaks, and fragments.", log_ns)
    resolve_compound_fragments(peak_id = peaks,
                               annotated_fragment_id = fragments,
                               compound_id = compounds)
    if (is.null(names(import_object))) {
      log_it("info", "Finished single object import.", log_ns)
    } else {
      log_it("info", glue::glue("Finished importing object #{i} with name {names(import_object)[i]}..."), log_ns)
    }
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
#' @inheritParams build_db_action
#' @inheritParams verify_import_columns
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
#' @return INT scalar of the record identifier
#' @export
#' 
add_or_get_id <- function(db_table,
                          values,
                          db_conn = con,
                          ensure_unique = TRUE,
                          require_all = TRUE,
                          ignore = FALSE,
                          log_ns = "db") {
  log_it("info", glue("Adding to or identifying a record in table '{db_table}'."), log_ns)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table      = list(c("mode", "character"), c("length", 1)),
        values        = list(c("n>=", 1)),
        db_conn       = list(c("length", 1)),
        ensure_unique = list(c("mode", "logical"), c("length", 1)),
        require_all   = list(c("mode", "logical"), c("length", 1)),
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
    db_table    = db_table,
    values      = values,
    names_only  = FALSE,
    require_all = require_all,
    db_conn     = db_conn
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
    return(NULL)
  } else {
    if (length(res_id) > 1) {
      res_id <- tail(res_id, 1)
      log_it("warn", glue("Multiple IDs ({format_list_of_names(res_id)}) match provided values. Using {this_id}."), log_ns)
    }
    if (length(res_id) == 1) {
      log_it("success", glue("Record found in table '{db_table}' as ID {res_id}."), log_ns)
      return(res_id)
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
    if (inherits(res_id, "try-error") || length(res_id) == 0) {
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
      if (length(res_id) > 1) {
        log_it("warn",
               glue("Multiple IDs ({format_list_of_names(res_id)}) match provided values. Using {this_id}."),
               log_ns)
        this_id <- tail(res_id, 1)
      } else if (length(res_id) == 1) {
        this_id <- res_id
      }
      log_it("success", glue("Record added to table '{db_table}' as ID {this_id}."), log_ns)
      return(this_id)
    }
  }
}

#' Resolve the compounds node during bulk import
#' 
#' Call this function as part of an import routine to resolve the compounds node.
#' 
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams add_or_get_id
#' @inheritParams map_import
#' @inheritParams resolve_compound_aliases
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param compounds_in CHR scalar name in `obj` holding compound data (default:
#'   "compounddata")
#' @param compounds_table CHR scalar name the database table holding compound
#'   data (default: "compounds")
#' @param compound_category CHR or INT scalar of the compound category (either a
#'   direct ID or a matching category label in `compound_category_table`)
#'   (default: NULL)
#' @param compound_category_table CHR scalar name the database table holding
#'   normalized compound categories (default: "compound_categories")
#'
#' @return INT scalar if successful, result of the call to [add_or_get_id]
#'   otherwise
#' @export
#'
resolve_compounds <- function(obj,
                              compounds_in = "compounddata",
                              compounds_table = "compounds",
                              compound_category = NULL,
                              compound_category_table = "compound_categories",
                              compound_alias_table = "compound_aliases",
                              norm_alias_table = "norm_analyte_alias_references",
                              norm_alias_name_column = "name",
                              NIST_id_in = "id",
                              require_all = FALSE,
                              import_map = IMPORT_MAP,
                              ensure_unique = TRUE,
                              db_conn = con,
                              log_ns = "db") {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, compounds_in, compounds_table, 
                        compound_category_table, compound_alias_table,
                        NIST_id_in, import_map, require_all, ensure_unique,
                        db_conn, log_ns),
      conditions = list(
        obj                     = list(c("mode", "list")),
        compounds_in            = list(c("mode", "character"), c("length", 1)),
        compounds_table         = list(c("mode", "character"), c("length", 1)),
        compound_category_table = list(c("mode", "character"), c("length", 1)),
        compound_alias_table    = list(c("mode", "character"), c("length", 1)),
        NIST_id_in              = list(c("mode", "character"), c("length", 1)),
        import_map              = list(c("mode", "data.frame")),
        ensure_unique           = list(c("mode", "logical"), c("length", 1)),
        require_all             = list(c("mode", "logical"), c("length", 1)),
        db_conn                 = list(c("length", 1)),
        log_ns                  = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  obj_values <- get_component(obj = obj,
                              obj_component = compounds_in,
                              log_ns = log_ns)[[1]]
  if (compounds_in %in% names(obj_values)) obj_values <- obj_values[[1]]
  if (is.null(names(obj_values)) && any(c(NIST_id_in, "name") %in% names(obj_values[[1]]))) {
    obj_values <- obj_values[[1]]
  }
  if (NIST_id_in %in% names(obj_values)) {
    check_value <- obj_values[[NIST_id_in]] %>%
      as.character() %>%
      str_remove_all("NISTPFAS0*")
    check_value <- paste0("NISTPFAS", str_pad(check_value, 6, "left", "0"))
      
  } else {
    check_value <- obj_values[["name"]]
    if (length(check_value) > 0 && str_detect(check_value, ";")) {
      check_value <- str_split(check_value, ";") %>%
        unlist()
      log_it("warn", glue::glue("{length(check_value)} unique compound names detected as potential aliases."), log_ns)
    }
  }
  if (length(check_value) == 0) {
    msg <- "Could not identify a NIST Suspect List ID or a 'name' column for this compound. These are necessary to proceed."
    log_it("error", msg, log_ns)
    stop(msg)
  }
  exists <- check_for_value(
    values = check_value,
    db_table = compound_alias_table,
    db_column = "alias",
    case_sensitive = FALSE,
    db_conn = db_conn,
    fuzzy = FALSE
  )
  if (exists$exists) {
    compound_ids <- exists$values$compound_id
    if (length(compound_ids) > 1) {
      log_it("warn", glue::glue("Aliases potentially matched to compound_ids {format_list_of_names(unique(compound_ids))}."), log_ns)
      likelihood <- table(compound_ids)
      if (any(likelihood > 1)) {
        compound_ids <- compound_ids[rev(order(table(compound_ids)))][1]
        log_it("info", glue::glue("{max(likelihood)} of {length(check_value)} were in agreement to use compound_id {compound_ids} as the most likely alias."), log_ns)
      } else {
        log_it("warn", glue::glue("Unresolvable multiple alias matches for {format_list_of_names(check_value, add_quotes = TRUE)}. Please examine and try again."), log_ns)
        return(NULL)
      }
    }
    alias_type <- build_db_action(
      action = "select",
      table_name = "norm_analyte_alias_references",
      column_name = "name",
      match_criteria = list(id = exists$values$alias_type),
      and_or = "OR",
      db_conn = db_conn,
      log_ns = log_ns
    )
    log_it("info", glue::glue("Found {alias_type} alias {format_list_of_names(check_value, add_quotes = TRUE)} for compound_id = {compound_ids}."), log_ns)
  } else {
    compound_values <- map_import(
      import_obj = obj,
      aspect = compounds_table,
      import_map = import_map
    )
    if (str_detect(compound_values$name, ";")) {
      all_names <- str_split(compound_values$name, ";")
      first_names <- lapply(all_names, function(x) x[[1]]) %>%
        purrr::flatten_chr()
      compound_values$name <- first_names
    }
    compound_ids <- add_or_get_id(
      db_table = compounds_table,
      values = compound_values,
      db_conn = db_conn,
      ensure_unique = ensure_unique,
      require_all = require_all,
      log_ns = log_ns
    )
  }
  resolve_compound_aliases(obj = obj,
                           compound_id = compound_ids,
                           compounds_in = compounds_in,
                           compound_alias_table = compound_alias_table,
                           norm_alias_table = norm_alias_table,
                           norm_alias_name_column = norm_alias_name_column,
                           db_conn = db_conn,
                           log_ns = log_ns)
  return(compound_ids)
}

#' Resolve compound aliases provided as part of the import routine
#'
#' Call this to add any aliases for a given `compound_id` that may not be
#' present in the database. Only those identifiable as part of the accepted
#' types defined in `norm_alias_table` will be mapped. If multiple items are
#' provided in the import NAME, ADDITIONAL, or other items matching names in
#' `norm_alias_table`.name column, indicate the split character in
#' `split_multiples_by` and any separator between names and values (e.g.
#' CLASS:example) in `identify_property_by`.
#'
#' @note Existing aliases, and aliases for which there is no `compound_id` will
#'   be ignored and not imported.
#' @note Compound IDs provided in `compound_id` must be present in the compounds
#'   table and must be provided explicitly on a 1:1 basis for each element
#'   extracted from `obj`. If you provide an import object with 10 components
#'   for compound data, you must provide tying `compound_id` identifiers for
#'   each. If all extracted components represent aliases for the same
#'   `compound_id` then one may be provided.
#' @note Alias types (e.g. "InChI" are case insensitive)
#'
#' @inheritParams resolve_compounds
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param compound_id INT scalar of the compound_id to use for these aliases
#' @param norm_alias_table CHR scalar name of the table normalizing analyte
#'   alias references (default: "norm_analyte_alias_references")
#' @param norm_alias_name_column CHR scalar name of the column in
#'   `norm_alias_table` containing the human-readable expression of alias type
#'   classes (default: "name")
#' @param ... Named list of any additional aliases to tack on that are not found
#'   in the import object, with names matching those found in
#'   `norm_alias_table`.`norm_alias_name_column`
#'
#' @return None, though if unclassifiable aliases (those with alias types not
#'   present in the normalization table) are found, they will be written to a
#'   file (`out_file`) in the project directory
#' @export
#'
resolve_compound_aliases <- function(obj,
                                     compound_id,
                                     compounds_in = "compounddata",
                                     compound_alias_table = "compound_aliases",
                                     norm_alias_table = "norm_analyte_alias_references",
                                     norm_alias_name_column = "name",
                                     headers_to_examine = c("ADDITIONAL", "NAME"),
                                     split_multiples_by = ";",
                                     identify_property_by = ":",
                                     out_file = "unknown_compound_aliases.csv",
                                     db_conn = con,
                                     log_ns = "db",
                                     ...) {
  stopifnot(compound_id == as.integer(compound_id))
  compound_id <- as.integer(compound_id)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, compound_id, compounds_in, compound_alias_table, 
                        norm_alias_table, norm_alias_name_column,
                        headers_to_examine, split_multiples_by,
                        identify_property_by, out_file,
                        db_conn, log_ns),
      conditions = list(
        obj                     = list(c("mode", "list")),
        compound_id             = list(c("mode", "integer"), c("n>=", 1)),
        compounds_in            = list(c("mode", "character"), c("length", 1)),
        compound_alias_table    = list(c("mode", "character"), c("length", 1)),
        norm_alias_table        = list(c("mode", "character"), c("length", 1)),
        norm_alias_name_column  = list(c("mode", "character"), c("length", 1)),
        headers_to_examine      = list(c("mode", "character"), c("n>=", 1)),
        split_multiples_by      = list(c("mode", "character"), c("length", 1)),
        identify_property_by    = list(c("mode", "character"), c("length", 1)),
        out_file                = list(c("mode", "character"), c("length", 1)),
        db_conn                 = list(c("length", 1)),
        log_ns                  = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  compound_data <- get_component(obj = obj,
                                 obj_component = compounds_in,
                                 log_ns = log_ns)
  if (!length(compound_id) == 1 && !length(obj) == length(compound_id)) {
    log_it("warn", "Lengths of `compound_id` and `obj` must be equal to accept multiple `compound_id` entries.", log_ns)
    return(invisible(NULL))
  }
  while (!compounds_in %in% names(compound_data)) {
    compound_data <- purrr::flatten(compound_data)
  }
  if (identical(names(compound_data), compounds_in)) compound_data <- compound_data[[1]]
  compound_data <- compound_data %>%
    bind_rows() %>%
    mutate(compound_id = compound_id) %>%
    rename_with(tolower)
  alias_refs <- tbl(db_conn, norm_alias_table) %>%
    collect()
  alias_types <- alias_refs[[norm_alias_name_column]] %>%
    tolower()
  kwargs <- list(...)
  if (length(kwargs) > 0) {
    names(kwargs) <- tolower(names(kwargs))
    kwarg_aliases <- try(
      tibble(
        compound_id = compound_id,
        alias_type = names(kwargs),
        alias = lapply(kwargs, as.character)
      ) %>%
        unnest(c(alias)) %>%
        mutate(alias = ifelse(alias_type %in% alias_types,
                              alias,
                              str_c(toupper(alias_type), alias, sep = ":")))
    )
  }
  need_headers <- c("NAME",
                    "compound_id",
                    alias_types,
                    names(list(...))
  ) %>%
    tolower()
  out <- compound_data %>%
    select(any_of(need_headers)) %>%
    rename(alias = name) %>%
    pivot_longer(cols = -compound_id) %>%
    set_names(dbListFields(db_conn, compound_alias_table)) %>%
    mutate(alias = str_split(alias, pattern = ";"),
           alias_type = ifelse(tolower(alias_type) %in% alias_types,
                               tolower(alias_type),
                               alias_type)) %>%
    unnest(c(alias)) %>%
    filter(!alias == "")
  if (length(kwargs) > 0) {
    out <- bind_rows(
      out,
      kwarg_aliases
    )
  }
  out <- bind_rows(
    out %>%
      filter(alias_type %in% alias_types),
    out %>%
      filter(!alias_type %in% alias_types) %>%
      mutate(alias = ifelse(!alias_type %in% alias_types,
                            alias,
                            paste0(toupper(alias_type), ":", alias))) %>%
      select(compound_id, alias) %>%
      separate(alias, sep = ":", into = c("alias_type", "alias"), extra = "merge")
  ) %>%
    distinct() %>%
    mutate(is_alias_type = tolower(alias_type) %in% alias_types)
  out <- list(
    "known" = out %>%
      filter(is_alias_type),
    "unknown" = out %>%
      filter(!is_alias_type)
  ) %>%
    lapply(function(x) x %>% select(-is_alias_type))
  if (nrow(out$known) > 0) {
    alias_refs[[norm_alias_name_column]] <- tolower(alias_refs[[norm_alias_name_column]])
    out$known <- out$known %>%
      mutate(alias_type = tolower(alias_type)) %>%
      left_join(alias_refs,
                by = c("alias_type" = "name")) %>%
      mutate(alias_type = id) %>%
      select(any_of(dbListFields(con, compound_alias_table)))
    build_db_action(
      action = "insert",
      table_name = compound_alias_table,
      values = out$known,
      # TODO consider removing in production
      ignore = TRUE,
      db_conn = db_conn,
      log_ns = log_ns
    )
  }
  if (nrow(out$unknown) > 0) {
    out$unknown$alias_type <- toupper(out$unknown$alias_type)
    write_csv(out$unknown, out_file, append = file.exists(out_file), quote = "all")
    log_it("info", glue::glue("Unrecognizeable compound identifiers were provided. See '{out_file}' for more information."), log_ns)
  }
}

#' Link together peaks, fragments, and compounds
#'
#' This function links together the peaks, annotated_fragments, and compounds
#' table. This serves as the main connection table conceptually tying together
#' peaks, the fragments annotated within those peaks, and the compound
#' identification associated with the peaks. The database supports flexible
#' assignment wherein compounds may be related to either peaks or annotated
#' fragments, or both, and vice versa. At least two IDs are required for
#' linkage; i.e. compounds may not have an acciated peak in the database, but
#' are known to produce fragments at a particular m/z value. Ideally, all three
#' are provided to provide traceback from compounds, a complete list of their
#' annotated fragments, and association with a peak object with data containing
#' unannotated fragments, which can be traced back to the sample from which it
#' was drawn and the associated metrological method information.
#'
#' @inheritParams add_or_get_id
#'
#' @param values LIST item containing items for `peak_id`,
#'   `annotated_fragment_id`, and `compound_id` (default: NULL); used
#'   preferentially if provided
#' @param peak_id INT vector (ideally of length 1) of the peak ID(s) to link;
#'   ignored if `values` is provided (default: NA)
#' @param annotated_fragment_id INT vector of fragment ID(s) to link; ignored if
#'   `values` is provided (default: NA)
#' @param compound_id INT vector of compound ID(s) to link; ignored if `values`
#'   is provided (default: NA)
#' @param linkage_table CHR scalar name of the database table containing
#'   linkages between peaks, fragments, and compounds (default:
#'   "compound_fragments")
#' @param peaks_table CHR scalar name of the database table containing peaks for
#'   look up (default: "peaks")
#' @param fragments_table CHR scalar name of the table holding annotated
#'   fragment information
#' @param compounds_table CHR scalar name of the table holding compound
#'   information
#'
#' @return None, value checks entries and executes database actions
#' @export
#' 
resolve_compound_fragments <- function(values = NULL,
                                       peak_id = NA,
                                       annotated_fragment_id = NA,
                                       compound_id = NA,
                                       linkage_table = "compound_fragments",
                                       peaks_table = "peaks",
                                       annotated_fragments_table = "annotated_fragments",
                                       compounds_table = "compounds",
                                       db_conn = con,
                                       log_ns = "db") {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(linkage_table, peaks_table, compounds_table, db_conn, log_ns),
      conditions = list(
        linkage_table   = list(c("mode", "character"), c("length", 1)),
        peaks_table     = list(c("mode", "character"), c("length", 1)),
        compounds_table = list(c("mode", "character"), c("length", 1)),
        db_conn         = list(c("length", 1)),
        log_ns          = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (is.null(values)) {
    # Pre check coercion to integer
    for (check_arg in c("peak_id", "annotated_fragment_id", "compound_id")) {
      arg <- environment()[[check_arg]]
      not_na <- which(!is.na(arg))
      if (length(not_na) > 0) {
        check <- suppressWarnings(as.integer(arg[not_na]))
        if (any(is.na(check))) {
          check <- arg[not_na][which(is.na(check))]
          log_it("error",
                 sprintf("'%s%s' = %s%s%s could not be safely coerced to an integer.",
                         check_arg,
                         ifelse(length(check) > 1,
                                "s",
                                ""),
                         ifelse(length(check) > 1,
                                "c(",
                                ""),
                         format_list_of_names(check, add_quotes = TRUE),
                         ifelse(length(check) > 1,
                                ")",
                                "")),
                 log_ns)
          return(NULL)
        }
      }
    }
  }
  # Check presence of IDs
  for (check_arg in c("peak_id", "annotated_fragment_id", "compound_id")) {
    ids <- environment()[[check_arg]]
    ids <- ids[!is.na(ids)]
    if (length(ids) > 0) {
      associated_table <- paste0(gsub("_id", "", check_arg), "s_table")
      check_table <- environment()[[associated_table]]
      check_val <- ids %in%
        build_db_action(
          action = "select",
          table_name = check_table,
          column_names = "id",
          db_conn = db_conn,
          log_ns = log_ns
        )
      if (!all(check_val)) {
        id_check <- ids[!check_val]
        log_it("error",
               sprintf("`%s` id%s = %s%s%s %s not present in the '%s' table. Please address and try again.",
                       check_arg,
                       ifelse(length(id_check) > 1,
                              "s",
                              ""),
                       ifelse(length(id_check) > 1,
                              "c(",
                              ""),
                       format_list_of_names(id_check, add_quotes = TRUE),
                       ifelse(length(id_check) > 1,
                              ")",
                              ""),
                       ifelse(length(id_check) > 1,
                              "were",
                              "was"),
                       check_table),
               log_ns
        )
        legit_ids <- ids[check_val]
        if (length(legit_ids) == 0) {
          legit_ids <- NA
        }
        assign(x = check_arg, value = legit_ids)
      }
    }
  }
  values <- list(peak_id = as.integer(peak_id),
                 annotated_fragment_id = as.integer(annotated_fragment_id),
                 compound_id = as.integer(compound_id))
  values <- try(bind_cols(values))
  if (inherits(values, 'try-error')) {
    log_it("error",
           glue::glue("Lengths of 'compound_id' ({length(compound_id)}), 'peak_id' (length = {length(peak_id)}), and 'annotated_fragment_id' (length = {length(annotated_fragment_id)}) must be compatible for combination into a data frame."),
           log_ns)
    return(NULL)
  }
  # Ensure there is at least one relationship remaining to draw
  at_least_two <- values %>%
    mutate(across(everything(), ~ !is.na(.))) %>%
    rowSums() > 1
  if (any(!at_least_two)) {
    n_affected <- length(at_least_two[!at_least_two])
    log_it("warning",
           glue::glue("At least two of peak_id, compound_id, or annotated_fragment_id must evaluate to integer values present in the database for every record being added. This affected {n_affected} record{ifelse(n_affected > 1, 's', '')}."),
           log_ns
    )
    values <- values[at_least_two, ]
  }
  peak_id_check <- peak_id %in% dbGetQuery(con, glue::glue("select id from {peaks_table}"))
  res <- try(
    build_db_action(
      action = "insert",
      table_name = linkage_table,
      values = values,
      db_conn = db_conn,
      log_ns = log_ns
    )
  )
  if (inherits(res, "try-error")) {
    log_it("error", glue::glue("There was an issue adding records to table '{linkage_table}'."), log_ns)
    stop()
  }
}

#' Resolve the fragments node during database import
#'
#' Call this function as part of an import routine to resolve the fragments node
#' including fragment inspections and aliases. If the python connection to RDKit
#' is available and no aliases are provided, aliases as defined in
#' `rdkit_aliases` will be generated and stored if `generate_missing_aliases` is
#' set to TRUE. Components of the import file will be collated, have their
#' values normalized, and any new fragment identifiers will be added to the
#' database.
#'
#' Fragments missing structure annotation are supported (e.g. those with a
#' formula but no SMILES notation provided).
#'
#' For new fragments, the calculated molecular mass is generated by
#' [calculate.monoisotope] from exact masses of each constituent atom. If RDKit
#' is available and a SMILES notation is provided, the formal molecular net
#' charge is also calculated via rdkit.Chem.GetFormalCharge.
#'
#' Database tables affected by resolving the fragments node include:
#' annotated_fragments, norm_fragments, fragment_inspections, fragment_aliases,
#' and fragment_sources.
#'
#' @note This function is called as part of [full_import()]
#' @note If components named in `citation_info_in` and `inspection_info_in` do
#'   not exist, that information will not be appended to the resulting database
#'   records.
#' @note Typical usage as part of the import workflow involves simply passing
#'   the import object and associated sample id: resolve_fragments_NTAMRT(obj =
#'   import_object, sample_id = 1), though wrapper functions like [full_import]
#'   also contain name-matched arguments to be passed in a [do.call] context.
#'
#' @inheritParams map_import
#' @inheritParams add_or_get_id
#' @inheritParams rdkit_mol_aliases
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param sample_id INT scalar matching a sample ID to which to tie these
#'   fragments (optional, default: NULL)
#' @param generation_type CHR scalar containing the generation type as defined
#'   in the "norm_generation_type" table (default: NULL will obtain the
#'   generation type attached to the `sample_id` by database lookup)
#' @param fragments_in CHR scalar name of the `obj` component holding annotated
#'   fragment information (default: "annotation")
#' @param fragments_table CHR scalar name of the database table holding
#'   annotated fragment information (default: "annotated_fragments")
#' @param fragments_norm_table CHR scalar name of the database table holding
#'   normalized fragment identities (default: obtains this from the result of a
#'   call to [er_map] with the table name from `fragments_table`)
#' @param fragments_sources_table CHR scalar name of the database table holding
#'   fragment source (e.g. generation) information (default: "fragment_sources")
#' @param citation_info_in CHR scalar name of the `obj` component holding
#'   fragment citation information (default: "fragment_citation")
#' @param inspection_info_in CHR scalar name of the `obj` component holding
#'   fragment inspection information (default: "fragment_inspections")
#' @param inspection_table CHR scalar name of the database table holding
#'   fragment inspection information (default: "fragment_inspections")
#' @param generate_missing_aliases LGL scalar determining whether or not to
#'   generate machine readable expressions (e.g. InChI) for fragment aliases
#'   from RDKit (requires RDKit activation; default: FALSE); see formals list
#'   for [add_rdkit_aliases]
#' @param fragment_aliases_in CHR scalar name of the `obj` component holding
#'   fragment aliases (default: "fragment_aliases")
#' @param fragment_aliases_table CHR scalar name of the database table holding
#'   fragment aliases (default: "fragment_aliases")
#' @param fragment_alias_type_norm_table CHR scalar name of the database table
#'   holding normalized fragment alias type identities (default: obtains this
#'   from the result of a call to [er_map] with the table name from
#'   `fragment_aliases_table`)
#'
#' @return INT vector of resolved annotated fragment IDs; executes database
#'   actions
#'   
resolve_fragments_NTAMRT <- function(obj,
                                     sample_id = NULL,
                                     generation_type = NULL,
                                     fragments_in = "annotation",
                                     fragments_table = "annotated_fragments",
                                     fragments_norm_table = ref_table_from_map(fragments_table, "fragment_id"),
                                     fragments_sources_table = "fragment_sources",
                                     citation_info_in = "fragment_citation",
                                     inspection_info_in = "fragment_inspections",
                                     inspection_table = "fragment_inspections",
                                     generate_missing_aliases = FALSE,
                                     fragment_aliases_in = "fragment_aliases",
                                     fragment_aliases_table = "fragment_aliases",
                                     alias_type_norm_table = ref_table_from_map(fragment_aliases_table, "alias_type"),
                                     inchi_prefix = "InChI=1S/",
                                     rdkit_name = ifelse(exists("PYENV_NAME"), PYENV_NAME, "rdkit"),
                                     rdkit_ref = ifelse(exists("PYENV_REF"), PYENV_REF, "rdk"),
                                     rdkit_ns = "rdk",
                                     rdkit_make_if_not = TRUE,
                                     rdkit_aliases = c("Inchi", "InchiKey"),
                                     mol_to_prefix = "MolTo",
                                     mol_from_prefix = "MolFrom",
                                     type = "smiles",
                                     import_map = IMPORT_MAP,
                                     case_sensitive = FALSE,
                                     fuzzy = FALSE,
                                     strip_na = TRUE,
                                     db_conn = con,
                                     log_ns = "db") {
  log_fn("start")
  if (any(is.na(sample_id))) {
    log_it("error", glue::glue("Could not safely coerce 'sample_id' = c({format_list_of_names(sample_id, add_quotes = TRUE)}) to an integer."), log_ns)
    stop()
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, 
                        fragments_in, fragments_table, fragments_norm_table, fragments_sources_table,
                        citation_info_in, inspection_info_in,
                        generate_missing_aliases, fragment_aliases_in, fragment_aliases_table, alias_type_norm_table,
                        rdkit_name, rdkit_ref, rdkit_ns, rdkit_make_if_not, mol_to_prefix, mol_from_prefix, type,
                        strip_na, db_conn, log_ns),
      conditions = list(
        obj                      = list(c("mode", "list")),
        fragments_in             = list(c("mode", "character"), c("length", 1)),
        fragments_table          = list(c("mode", "character"), c("length", 1)),
        fragments_norm_table     = list(c("mode", "character"), c("length", 1)),
        fragments_sources_table  = list(c("mode", "character"), c("length", 1)),
        citation_info_in         = list(c("mode", "character"), c("length", 1)),
        inspection_info_in       = list(c("mode", "character"), c("length", 1)),
        generate_missing_aliases = list(c("mode", "logical"), c("length", 1)),
        fragment_aliases_in      = list(c("mode", "character"), c("length", 1)),
        fragment_aliases_table   = list(c("mode", "character"), c("length", 1)),
        alias_type_norm_table    = list(c("mode", "character"), c("length", 1)),
        rdkit_name               = list(c("mode", "character"), c("length", 1)),
        rdkit_ref                = list(c("mode", "character"), c("length", 1)),
        rdkit_ns                 = list(c("mode", "character"), c("length", 1)),
        rdkit_make_if_not        = list(c("mode", "logical"), c("length", 1)),
        mol_to_prefix            = list(c("mode", "character"), c("length", 1)),
        mol_from_prefix          = list(c("mode", "character"), c("length", 1)),
        type                     = list(c("mode", "character"), c("length", 1)),
        strip_na                 = list(c("mode", "logical"), c("length", 1)),
        db_conn                  = list(c("length", 1)),
        log_ns                   = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(active_connection(db_conn = db_conn))
  obj <- get_component(obj, fragments_in)
  if (length(obj) == 0) return(NA)
  while (!fragments_in %in% names(obj)) {
    obj <- obj[[1]]
  }
  # Check for zero length - added 2024-06-18: This should only be a final
  # convenience check. By this point imports should be well formatted.
  for (i in 1:length(obj)) {
    log_it("info", glue::glue("Checking for zero length components from '{fragments_in}' in this import object."), log_ns)
    zero_length <- which(sapply(obj[[i]], length) == 0)
    if (any(zero_length)) {
      log_it("info", glue::glue("Removing {length(zero_length[zero_length])} zero length entries at {str_flatten_comma(names(obj[[i]]), last = ', and ')}."), log_ns)
    }
    obj[[i]][zero_length] <- NULL
  }
  obj <- obj[sapply(obj, length) > 1]
  if (length(obj) == 0) return(NA)
  # Resolve direct aliases first - this will make all SMILES aliases resolve
  # automatically during import mapping of the fragment values
  fragment_identifiers <- map_import(
    import_obj = purrr::flatten_df(obj),
    aspect = fragments_norm_table,
    import_map = import_map,
    resolve_normalization = FALSE,
    strip_na = strip_na,
    db_conn = db_conn,
    log_ns = log_ns
  )
  if (nrow(bind_cols(fragment_identifiers)) == 0) {
    log_it("info", glue::glue("No annotated fragments were located at '{fragments_in}' in this import object."), log_ns)
    return(NA)
  }
  if (!is.null(rdkit_aliases)) {
    stopifnot(is.character(rdkit_aliases), length(rdkit_aliases) > 0)
  }
  using_rdkit <- exists("INFORMATICS") &&
    INFORMATICS &&
    exists("USE_RDKIT") &&
    USE_RDKIT &&
    exists("rdkit_active") &&
    rdkit_active(rdkit_ref = rdkit_ref,
                 rdkit_name = rdkit_name,
                 log_ns = log_ns)
  if (using_rdkit) {
    rdk <- eval(rlang::sym(rdkit_ref))
  }
  if (!"smiles" %in% names(fragment_identifiers)) fragment_identifiers$smiles <- NA
  fragment_identifiers <- fragment_identifiers %>%
    bind_cols() %>%
    mutate(smiles = ifelse(smiles == "", NA, smiles),
           radical = as.logical(radical))
  if (nrow(fragment_identifiers) == 0) {
    log_it("info", "No identifying aliases were located.", log_ns)
    return(NULL)
  }
  
  fragment_identifiers <- fragment_identifiers %>%
    left_join(
      dataframe_match(
        match_criteria = fragment_identifiers,
        table_names = fragments_norm_table,
        and_or = "AND",
        db_conn = db_conn,
        log_ns = log_ns
      )
    ) %>% select(
      any_of(dbListFields(con, fragments_norm_table))
    )
  existing_fragment_identifiers <- fragment_identifiers %>%
    filter(!is.na(id))
  new_fragment_identifiers <- fragment_identifiers %>%
    filter(is.na(id)) %>%
    select(-id)
  if (nrow(new_fragment_identifiers) > 0) {
    fixed_masses <- new_fragment_identifiers$fixedmass
    fixedmass_missing <- which(is.na(fixed_masses) | fixed_masses == 0)
    if (length(fixedmass_missing) > 0) {
      new_fragment_identifiers$fixedmass[fixedmass_missing] <- calculate.monoisotope(new_fragment_identifiers$formula[fixedmass_missing])
    }
    netcharge_missing <- with(new_fragment_identifiers,
                              which(is.na(netcharge) & !is.na(smiles) & !smiles == "")
    )
    if (length(netcharge_missing) > 0) {
      if (using_rdkit) {
        new_fragment_identifiers$netcharge[netcharge_missing] <- sapply(
          new_fragment_identifiers$smiles[netcharge_missing],
          function(x) {
            mol <- rdk$Chem$MolFromSmiles(x)
            if (is.null(mol)) {
              fc <- NA
              log_it("warn", sprintf("Could not generate a mol object from smiles string `%s` to calculate the formal charge", x), log_ns)
            } else{
              fc <- rdk$Chem$GetFormalCharge(mol)
            }
            return(fc)
          }
        )
      }
    }
    build_db_action(
      action = "insert",
      values = new_fragment_identifiers,
      table_name = fragments_norm_table,
      ignore = TRUE,
      db_conn = db_conn,
      log_ns = log_ns
    )
    new_fragment_identifiers <- dataframe_match(
      match_criteria = new_fragment_identifiers,
      table_names = fragments_norm_table,
      and_or = "AND",
      db_conn = db_conn,
      log_ns = log_ns
    )
    fragment_identifiers <- bind_rows(
      existing_fragment_identifiers,
      new_fragment_identifiers
    )
  }
  
  # Originally tried to map_import here, but inconsistent presence of SMILES
  # strings proved this to be too strict. Instead, map it directly, which means
  # this is now an NTAMRT function only.
  fragment_values <- get_component(obj, fragments_in) %>%
    purrr::flatten_df() %>%
    setNames(tolower(str_remove_all(names(.), "fragment_"))) %>%
    mutate(radical = as.numeric(as.logical(radical)),
           smiles = if ("smiles" %in% names(.)) ifelse(smiles == "", NA, smiles) else NA) %>%
    left_join(fragment_identifiers) %>%
    rename("fragment_id" = "id") %>%
    select(any_of(dbListFields(db_conn, fragments_table)))
  
  if (is.null(generation_type) && !is.null(sample_id)) {
    stopifnot(sample_id == as.integer(sample_id))
    generation_type <- build_db_action(
      action = "select",
      table_name = "samples",
      column_names = "generation_type",
      match_criteria = list(id = sample_id)
    )
  } else if (is.null(generation_type) && is.null(sample_id)) {
    log_it("warn", "No sample id was provided to obtain a generation type. It will be left NULL.", log_ns)
  } else {
    norm_gen_type_table <- ref_table_from_map(fragments_sources_table, "generation_type")
    if (length(generation_type) > 1 && !length(generation_type) == nrow(fragment_values)) {
      log_it("error",
             glue::glue("The length of 'generation_type' (length = {length(generation_type)}) must be either a single value or match the length of input values (length = {length(fragment_values[[1]])}). No generation types will be recorded for "),
             log_ns
      )
      generation_type <- NA
    } else if (all(generation_type %in% build_db_action(action = "select",
                                                        table_name = norm_gen_type_table,
                                                        db_conn = db_conn,
                                                        column_names = "id"))) {
      generation_type <- generation_type
    } else {
      generation_type <- sapply(
        generation_type,
        function(x) {
          resolve_normalization_value(
            this_value = x,
            db_table = norm_gen_type_table,
            db_conn = db_conn,
            log_ns = log_ns
          )
        }
      )
    }
  }
  existing_fragments <- dataframe_match(
    match_criteria = fragment_values,
    table_names = fragments_table,
    and_or = "AND",
    db_conn = db_conn,
    log_ns = log_ns
  )
  new_fragments <- fragment_values %>%
    anti_join(existing_fragments)
  if (nrow(new_fragments) > 0) {
    res <- try(
      build_db_action(
        action = "insert",
        table_name = fragments_table,
        values = new_fragments %>%
          select(any_of(dbListFields(con, fragments_table))),
        ignore = TRUE,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
    if (inherits(res, "try-error")) {
      log_it("error", glue::glue("There was an issue adding records to table '{fragments_table}'."), log_ns)
      stop()
    } else {
      fragment_values <- dataframe_match(
        match_criteria = fragment_values,
        table_names = fragments_table,
        and_or = "AND",
        db_conn = db_conn,
        log_ns = log_ns
      )
    }
  } else {
    fragment_values <- existing_fragments
  }
  
  # Add fragment source information
  fragment_sources <- get_component(obj, fragments_in)
  if (length(fragment_sources) > 0) {
    fragment_sources <- fragment_sources %>%
      purrr::flatten_df() %>%
      rename_with(~ tolower(gsub("fragment_", "", .x))) %>%
      left_join(fragment_values %>%
                  rename("annotated_fragments_id" = "id")
      ) %>%
      mutate(generation_type = generation_type) %>%
      select(matches(dbListFields(db_conn, fragments_sources_table)))
    res <- try(
      build_db_action(
        action = "insert",
        table_name = fragments_sources_table,
        values = fragment_sources,
        ignore = TRUE,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
  }
  
  # Add inspection information Inspection information should be coercible to a
  # data frame (that is, list objects must be of the same length) and have at
  # least one identifier name that matches a column in fragment_values (ideally:
  # "smiles") in order for the join to succeed.
  inspection_info <- get_component(obj, inspection_info_in)
  if (length(inspection_info) > 0) {
    inspection_values <- inspection_info %>%
      bind_rows() %>%
      left_join(fragment_values) %>%
      select(any_of(dbListFields(db_conn, inspection_table)))
    res <- try(
      build_db_action(
        action = "insert",
        table_name = inspection_table,
        values = inspection_values,
        ignore = TRUE,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
    if (inherits(res, "try-error")) {
      log_it("warn", glue::glue("There was an issue adding records to table '{inspection_table}'"), log_ns)
    }
  }
  
  # Add additional fragment aliases, if any. Assume the import object containing
  # aliases is in the form of a nested list or a dataframe that can be coerced
  # to a list, named for the SMILES string with components named for the type of
  # alias as (e.g. list(list("SMILES" = "F[C-](F)C(F)(F)F", "INCHI" =
  # "1S/C2F5/c3-1(4)2(5,6)7/q-1", "INCHIKEY" = "ADQIVSCGHADPRK-UHFFFAOYSA-N"))
  # Alias types listed in table norm_analyte_alias_references are supported;
  # others will be ignored.
  fragment_aliases <- get_component(obj, fragment_aliases_in)
  if (length(fragment_aliases) == 0) {
    fragment_alias_values <- list(
      smiles = NA,
      fragment_id = NA
    )
    if (generate_missing_aliases) {
      if (!using_rdkit) {
        log_it("warn", "Alias generation was requested but rdkit was not available.", log_ns)
        fragment_alias_values <- data.frame(a = NULL)
      } else {
        these_args <- as.list(environment())[names(formals(add_rdkit_aliases))]
        these_args <- these_args[!is.na(names(these_args))]
        identifiers <- fragment_identifiers %>%
          filter(!smiles == "" | !is.na(smiles)) %>%
          select(id, smiles)
        if (nrow(identifiers) == 0) {
          fragment_alias_values <- data.frame(a = NULL)
        } else {
          fragment_alias_values <- do.call(
            add_rdkit_aliases,
            args = append(
              list(
                identifiers = identifiers,
                alias_category = "fragments"
              ),
              these_args
            )
          )
        }
      }
    } else {
      fragment_alias_values <- fragment_alias_values %>%
        bind_rows() %>%
        filter(!is.na(fragment_id))
    }
  } else {
    fragment_alias_values <- fragment_aliases[[1]]
    if (!any(names(fragment_values)) %in% names(fragment_alias_values)) {
      log_it("warn", "Joining aliases requires matching names between supplied fragment aliases and a column in supplied fragments. No aliases will be added.", log_ns)
    } else {
      fragment_alias_values <- fragment_alias_values %>%
        bind_rows() %>%
        mutate(across(matches("^inchi$", ignore.case = TRUE),
                      ~ ifelse(test = grepl(inchi_prefix, .x),
                               yes = .x,
                               no = paste0(inchi_prefix, .x))),
               across(matches("^inchi$", ignore.case = TRUE),
                      ~ str_replace_all(
                        string = .x,
                        pattern = sprintf("(%s){2}", gsub("InChI=", "", inchi_prefix)),
                        replacement = gsub("InChI=", "", inchi_prefix)))
        ) %>%
        left_join(fragment_values) %>%
        filter(!is.na(fragment_id)) %>%
        select(1:fragment_id) %>%
        pivot_longer(col = -fragment_id) %>%
        setNames(dbListFields(db_conn, fragment_aliases_table)) %>%
        mutate(alias_type = sapply(alias_type,
                                   function(x) {
                                     resolve_normalization_value(x, fragment_alias_type_norm_table)
                                   }))
    }
  }
  if (nrow(fragment_alias_values) > 0) {
    fragment_alias_values <- fragment_alias_values %>%
      filter(complete.cases(.), !alias == "")
    res <- try(
      build_db_action(
        action = "insert",
        table_name = fragment_aliases_table,
        values = fragment_alias_values,
        # TODO consider removing in production and restrict to unique at the table level
        ignore = TRUE,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
    if (inherits(res, "try-error")) {
      log_it("warn", glue::glue("There was an issue adding records to table '{fragment_aliases_table}'"), log_ns)
    }
  }
  return(fragment_values$id)
}

#' Import software settings
#'
#' Part of the standard import pipeline, adding rows to the
#' `conversion_software_settings` table with a given sample id. Some argument
#' names are shared with other import functions, specifically `obj` but are
#' formed differently to resolve the node complexity correctly.
#'
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams add_or_get_id
#' @inheritParams build_db_action
#'
#' @param obj CHR vector describing settings or a named LIST with names matching
#'   column names in table conversion_software_settings.
#' @param software_timestamp CHR scalar of the sample timestamp (e.g.
#'   sample$starttime) to use for linking software conversion settings with peak
#'   data, with a call back to the originating sample. If NULL (the default),
#'   the current system timestamp in UTC will be used from [lubridate::now()].
#' @param software_settings_in CHR scalar name of the component in `obj`
#'   containing software settings (default: "msconvertsettings")
#' @param settings_table CHR scalar name of the database table containing the
#'   software settings used for an imported data file (default:
#'   "conversion_software_settings")
#' @param linkage_table CHR scalar name of the database table containing the
#'   linkage between peaks and their software settings (default:
#'   "conversion_software_peaks_linkage")
#' @param as_date_format CHR scalar the format to use when storing timestamps
#'   that matches database column expectations (default: "\%Y-\%m-\%d \%H:\%M:\%S")
#' @param format_checks CHR vector of the [lubridate::parse_date_time()] format
#'   checks to execute in order of priority; these must match a lubridate
#'   function of the same name (default: c("ymd_HMS", "ydm_HMS", "mdy_HMS",
#'   "dmy_HMS"))
#' @param min_datetime CHR scalar of the minimum reasonable timestamp used as a
#'   sanity check (default: "2000-01-01 00:00:00")
#'
#' @return NULL on errors, INT scalar of the inserted software linkage id if
#'   successful
#' @export
#' 
resolve_software_settings_NTAMRT <- function(obj,
                                             software_timestamp = NULL,
                                             db_conn = con,
                                             software_settings_in = "msconvertsettings",
                                             settings_table = "conversion_software_settings",
                                             linkage_table = "conversion_software_peaks_linkage",
                                             as_date_format = "%Y-%m-%d %H:%M:%S",
                                             format_checks = c("ymd_HMS", "ydm_HMS", "mdy_HMS", "dmy_HMS"),
                                             min_datetime = "2000-01-01 00:00:00",
                                             log_ns = "db") {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, db_conn, software_settings_in, settings_table, linkage_table, as_date_format, format_checks, min_datetime, log_ns),
      conditions = list(
        obj                  = list(c("mode", "list")),
        db_conn              = list(c("length", 1)),
        software_settings_in = list(c("mode", "character"), c("length", 1)),
        settings_table       = list(c("mode", "character"), c("length", 1)),
        linkage_table        = list(c("mode", "character"), c("length", 1)),
        as_date_format       = list(c("mode", "character"), c("length", 1)),
        format_checks        = list(c("mode", "character")),
        min_datetime         = list(c("mode", "character"), c("length", 1)),
        log_ns               = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (software_settings_in %in% names(obj)) {
    obj <- get_component(obj, software_settings_in)[[1]] %>%
      purrr::flatten_chr()
  } else {
    msg <- sprintf('"%s" not found in the namespace of this object. Using obj directly.', software_settings_in)
    log_it("warn", msg)
  }
  if (length(obj) == 0) {
    log_it("info", "The object provided was empty.", log_ns)
    return(invisible(NULL))
  }
  # Add entries to conversion_software_peaks_linkage
  if (is.null(software_timestamp)) {
    software_timestamp <- lubridate::now("UTC")
  } else {
    log_it("info", glue::glue("Checking that '{software_timestamp}' can be coerced to the '{as_date_format}' format using one of {format_list_of_names(format_checks, add_quotes = TRUE)}."), log_ns)
    if (is.character(software_timestamp)) {
      software_timestamp_coerced <- try(
        lubridate::parse_date_time(
          software_timestamp,
          orders = format_checks
        )
      )
    } else if (is.numeric(software_timestamp)) {
      software_timestamp_coerced <- try(
        lubridate::ymd_hms(
          lubridate::as_datetime(
            software_timestamp
          )
        )
      )
      if (software_timestamp_coerced < as_datetime(min_datetime)) {
        log_it("error", glue::glue("Parsed numerical software_timestamp of '{software_timestamp}' yielded '{software_timestamp_coerced}' which was before the provided minimum timeframe of '{min_datetime}'."), log_ns)
        stop()
      }
    }
    if (inherits(software_timestamp_coerced, "try-error") || is.na(software_timestamp_coerced)) {
      log_it("error", glue::glue("Could not parse '{software_timestamp}' as a datetime object."), log_ns)
      return(invisible(NULL))
    } else {
      software_timestamp <- software_timestamp_coerced
    }
  }
  software_timestamp <- format(software_timestamp, as_date_format)
  linkage_id <- add_or_get_id(
    db_table = linkage_table,
    values = list(generated_on = software_timestamp),
    ensure_unique = TRUE,
    db_conn = db_conn,
    log_ns = log_ns
  )
  # Add entries to conversion_software_settings
  if (!is.list(obj)) {
    if (is.character(obj) && length(obj) > 0) {
      values <- tibble(linkage_id, obj) %>%
        setNames(dbListFields(db_conn, settings_table))
    } else {
      log_it("error", "Could not resolve conversion_software_settings; these should be a character vector of length > 0 or a list coercible to one.", log_ns)
      return(invisible(NULL))
    }
  }
  res <- try(
    build_db_action(action = "insert",
                    db_conn = db_conn,
                    table_name = settings_table,
                    values = values,
                    # TODO consider removing this in production; ignoring here
                    # will ensure that additional entries are not duplicated for
                    # a given timestamp, and requires the software settings
                    # table to have a unique constraint
                    ignore = TRUE,
                    log_ns = log_ns)
  )
  if (inherits(res, "try-error")) {
    log_it("error" , glue::glue("There was a problem resolving records in table {settings_table}."), log_ns)
    return(res)
  } else {
    log_it("success", glue::glue("Records resolved in {settings_table}."), log_ns)
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
#' [resolve_software_settings_NTAMRT] to insert records into and get the proper
#' conversion software linkage id from tables "conversion_software_settings" and
#' "conversion_software_linkage" if appropriate.
#'
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams add_or_get_id
#' @inheritParams map_import
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_id INT scalar of the associated ms_methods record id
#' @param sample_in CHR scalar of the import object name storing sample data
#'   (default: "sample")
#' @param sample_table CHR scalar name of the database table holding sample
#'   information (default: "samples")
#' @param generation_type CHR scalar of the type of data generated for this
#'   sample (e.g. "empirical" or "in silico"). The default (NULL) will assign
#'   based on `generation_type_default`; any other value will override the
#'   default value and be checked against values in `geneation_type_norm_table`
#' @param generation_type_default CHR scalar naming the default data generation
#'   type (default: "empirical")
#' @param generation_type_norm_table CHR scalar name of the database table
#'   normalizing sample generation type (default: "empirical")
#' @param ... Other named elements to be appended to samples as necessary for
#'   workflow resolution, can be used to pass defaults or additional values.
#'
#' @return INT scalar if successful, result of the call to [add_or_get_id]
#'   otherwise
#'   
#' @export
#' 
resolve_sample <- function(obj,
                           db_conn = con,
                           method_id = NULL,
                           sample_in = "sample",
                           sample_table = "samples",
                           generation_type = NULL,
                           generation_type_default = "empirical",
                           generation_type_norm_table = "norm_generation_type",
                           import_map = IMPORT_MAP,
                           ensure_unique = TRUE,
                           require_all = TRUE,
                           fuzzy = FALSE,
                           case_sensitive = TRUE,
                           log_ns = "db",
                           ...) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, db_conn, sample_in, sample_table, generation_type_norm_table, ensure_unique, log_ns),
      conditions = list(
        obj                        = list(c("n>=", 1)),
        db_conn                    = list(c("length", 1)),
        sample_in                  = list(c("mode", "character"), c("length", 1)),
        sample_table               = list(c("mode", "character"), c("length", 1)),
        generation_type_norm_table = list(c("mode", "character"), c("length", 1)),
        ensure_unique              = list(c("mode", "logical"), c("length", 1)),
        log_ns                     = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (!is.null(method_id)) stopifnot(method_id == as.integer(method_id), length(method_id) == 1)
  if (is.null(generation_type)) {
    generation_type <- generation_type_default
  }
  log_fn("start")
  log_it("info", "Preparing sample import.", log_ns)
  sample_values <- map_import(
    import_obj = obj,
    aspect = sample_table,
    fuzzy = fuzzy,
    case_sensitive = case_sensitive,
    import_map = IMPORT_MAP
  ) %>%
    tack_on(
      ms_methods_id = method_id,
      generation_type = resolve_normalization_value(this_value = generation_type,
                                                    db_table = generation_type_norm_table,
                                                    case_sensitive = case_sensitive,
                                                    fuzzy = fuzzy,
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
    verify_import_columns(
      db_table = sample_table,
      db_conn = db_conn
    )
  is_single_record <- all(unlist(lapply(sample_values, length)) < 2)
  log_it("info", glue("Sample value lengths {ifelse(is_single_record, 'are', 'are not')} appropriate."), log_ns)
  if (!is_single_record) {
    stop("Resolve and try again.")
  }
  sample_id <- try(
    add_or_get_id(
      db_table = sample_table,
      values   = sample_values,
      db_conn  = db_conn,
      require_all = require_all,
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

#' Resolve and import sample aliases
#'
#' Call this function to attach sample aliases to a sample record in the
#' database. This can be done either through the import object with a name
#' reference or directly by assigning additional values.
#'
#' @note This function is called as part of [full_import()]
#' @note One of `values` or both of `obj` and `aliases_in` must be provided to
#'   add new sample aliases.
#'
#' @inheritParams build_db_action
#'
#' @param sample_id  INT scalar of the sample id (e.g. from the import workflow)
#' @param obj (optional) LIST object containing data formatted from the import
#'   generator (default: NULL)
#' @param aliases_in (optional) CHR scalar of the name in `obj` containing the
#'   sample aliases in list format (default: NULL)
#' @param values (optional) LIST containing the sample aliases with names as the
#'   alias name and values containing the reference (e.g. URI, link to a
#'   containing repository, or reference to the owner or project from which a
#'   sample is drawn) to that alias
#' @param db_table CHR scalar name of the database table containing sample
#'   aliases (default: "sample_aliases")
#'
#' @return None, executes actions on the database
#'
#' @export
#' 
resolve_sample_aliases <- function(sample_id,
                                   obj = NULL,
                                   aliases_in = NULL,
                                   values = NULL,
                                   db_table = "sample_aliases",
                                   db_conn = con,
                                   log_ns = "db") {
  stopifnot(sample_id == as.integer(sample_id),
            active_connection(db_conn))
  if (all(is.null(aliases_in), is.null(values))) {
    return(NULL)
  }
  if (!is.null(aliases_in)) obj <- get_component(obj, aliases_in)
  if (is.null(values)) {
    values <- obj
  } else {
    values <- tack_on(values, obj)
  }
  values <- tibble(
    sample_id = sample_id,
    name = names(values),
    reference = unname(values)
  ) %>%
    unnest(c(reference, name))
  if (nrow(values) > 0) {
    res <- try(
      build_db_action(action = 'insert',
                      table_name = db_table,
                      values = values,
                      db_conn = db_conn,
                      log_ns = log_ns)
    )
    if (inherits(res, "try-error")) {
      log_it("error",
             glue::glue("Could not insert records into the '{db_table}' table."),
             log_ns)
    }
  }
}

#' Add an ms_method record via import
#'
#' Part of the data import routine. Adds a record to the "ms_methods" table with
#' the values provided in the JSON import template. Makes extensive uses of
#' [resolve_normalization_value] to parse foreign key relationships.
#'
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams add_or_get_id
#' @inheritParams map_import
#' @inheritParams resolve_qc_methods_NTAMRT
#' @inheritParams resolve_qc_data_NTAMRT
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_in CHR scalar name of the `obj` list containing method
#'   information
#' @param ms_methods_table CHR scalar name of the database table containing
#'   method information
#' @param qc_method_in CHR scalar name of the import object element containing
#'   QC method information (default: "qcmethod")
#' @param qc_search_text CHR scalar name of an element in the import object in
#'   part `qc_method_in` identifying whether or not a QC method was used
#'   (default: "QC Method Used")
#' @param qc_value_in CHR scalar name of an element in the import object
#'   corresponding to `qc_method_in` where the value of the metric named for
#'   `qc_search_text` is located (default: "value")
#' @param ... Other named elements to be appended to "ms_methods" as necessary
#'   for workflow resolution, can be used to pass defaults or additional values.
#'
#' @return INT scalar if successful, result of the call to [add_or_get_id]
#'   otherwise
#'
#' @export
#' 
resolve_method <- function(obj,
                           method_in = "massspectrometry",
                           ms_methods_table = "ms_methods",
                           db_conn = con,
                           ensure_unique = TRUE,
                           log_ns = "db",
                           qc_method_in = "qcmethod",
                           qc_search_text = "QC Method Used",
                           qc_value_in = "value",
                           require_all = TRUE,
                           import_map = IMPORT_MAP,
                           ...) {
  # Check connection
  stopifnot(active_connection(db_conn))
  log_fn("start")
  log_it("trace", "Preparing method import.", log_ns)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, method_in, ms_methods_table, db_conn, ensure_unique, require_all, log_ns, qc_method_in, qc_search_text, qc_value_in),
      conditions = list(
        obj           = list(c("n>=", 1)),
        method_in     = list(c("mode", "character"), c("length", 1)),
        ms_methods_table      = list(c("mode", "character"), c("length", 1)),
        db_conn       = list(c("length", 1)),
        ensure_unique = list(c("mode", "logical"), c("length", 1)),
        require_all   = list(c("mode", "logical"), c("length", 1)),
        log_ns        = list(c("mode", "character"), c("length", 1)),
        qc_method_in  = list(c("mode", "character"), c("length", 1)),
        qc_search_text= list(c("mode", "character"), c("length", 1)),
        qc_value_in   = list(c("mode", "character"), c("length", 1))
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
  log_it("info", "Preparing method import.", log_ns)
  used_qc_method <- obj[[qc_method_in]] %>%
    .[[grep(qc_search_text, .)]] %>%
    .[[qc_value_in]] %>%
    as.numeric()
  if (length(used_qc_method) > 1) {
    log_it("warning", sprintf("More than one entry for %s detected; only the first will be used.", search_text), log_ns)
    used_qc_method <- used_qc_method[[1]]
  }
  ms_method_values <- map_import(
    import_obj = obj,
    aspect = ms_methods_table,
    import_map = import_map,
    log_ns = log_ns
  ) %>%
    tack_on(has_qc_method = used_qc_method) %>%
    tack_on(...) %>%
    verify_import_columns(db_table = ms_methods_table,
                          db_conn = db_conn,
                          require_all = require_all,
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
      db_table      = ms_methods_table,
      values        = ms_method_values,
      db_conn       = db_conn,
      ensure_unique = ensure_unique,
      log_ns        = log_ns
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
#' @note This function is called as part of [full_import()]
#' @note This function is brittle; built specifically for the NIST NTA MRT
#'   import format. If using a different import format, customize to your needs
#'   using this function as a guide.
#'
#' @inheritParams build_db_action
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_id INT scalar of the ms_method.id record to associate
#' @param type CHR scalar, one of "massspec" or "chromatography" depending on
#'   the type of description to add; much of the logic is shared, only details
#'   differ
#' @param mass_spec_in CHR scalar name of the element in `obj` holding mass
#'   spectrometry information (default: "massspectrometry")
#' @param chrom_spec_in CHR scalar name of the element in `obj` holding
#'   chromatographic information (default: "chromatography")
#'
#' @return None, executes actions on the database
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
  stopifnot(as.integer(method_id) == method_id,
            active_connection(db_conn))
  method_id <- as.integer(method_id)
  log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, method_id, type, mass_spec_in, chrom_spec_in, db_conn, fuzzy, log_ns),
      conditions = list(
        obj           = list(c("n>=", 1)),
        method_id     = list(c("mode", "integer"), c("length", 1)),
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
#' described in increasing detail. To accommodate that, the structure itself may
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
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams build_db_action
#' @inheritParams resolve_normalization_value
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param method_id INT scalar of the method id (e.g. from the import workflow)
#' @param sample_id INT scalar of the sample id (e.g. from the import workflow)
#' @param peak_id INT scalar of the peak id (e.g. from the import workflow)
#' @param carrier_mix_names CHR vector (optional) of carrier mix collection
#'   names to assign, the length of which should equal 1 or the length of
#'   discrete carrier mixtures; the default, NULL, will automatically assign
#'   names as a function of the method and sample id.
#' @param id_mix_by CHR scalar regex to identify the elements of `obj` to use
#'   for the mobile phase node (default "^mp*[0-9]+") grouping of carrier mix
#'   collections, this is the main piece of connectivity pulling together the
#'   descriptions and should only be changed to match different import naming
#'   schemes
#' @param ms_methods_table CHR scalar name of the methods table (default:
#'   "ms_methods")
#' @param sample_table CHR scalar name of the samples table (default: "samples")
#' @param peak_table CHR scalar name of the peaks table (default: "peaks")
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
#'   import object indicate an additive (e.g. names terminating in "add")
#' @param exclude_values CHR vector indicating which values to ignore in `obj`
#'   (default: c("none", "", NA))
#' @param clean_up LGL scalar determining whether or not to clean up the
#'   `mix_collection_table` by removing just-added records if there are errors
#'   adding to `carrier_props$db_table` (default: TRUE)
#'
#' @return None, executes actions on the database
#' @export
#' 
resolve_mobile_phase_NTAMRT <- function(obj,
                                        method_id,
                                        sample_id,
                                        peak_id,
                                        carrier_mix_names = NULL,
                                        id_mix_by = "^mp*[0-9]+",
                                        ms_methods_table = "ms_methods",
                                        sample_table = "samples",
                                        peak_table = "peaks",
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
                    db_table = ms_methods_table,
                    db_column = "id",
                    db_conn = db_conn)$exists,
    as.integer(sample_id) == sample_id,
    check_for_value(values = sample_id,
                    db_table = sample_table,
                    db_column = "id",
                    db_conn = db_conn)$exists,
    as.integer(peak_id) == peak_id,
    check_for_value(values = peak_id,
                    db_table = peak_table,
                    db_column = "id",
                    db_conn = db_conn)$exists,
    is.list(obj)
  )
  method_id <- as.integer(method_id)
  sample_id <- as.integer(sample_id)
  peak_id <- as.integer(peak_id)
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
    filter(group == "carrier")
  # Catch case of carrier not being provided
  if (nrow(carrier_mixes) == 0) {
    log_it("warn", "No carrier mixes provided.", log_ns)
  }
  carrier_mixes <- carrier_mixes %>%
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
    mutate(additive_id = NA)
  # Catch case of no additives
  if (nrow(carrier_additives) == 0) {
    log_it("info", "No carrier additives provided.", log_ns)
  } else {
    carrier_additives <- carrier_additives %>%
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
  }
  # Finally, shape up and insert into mobile_phases
  mobile_phases <- mixes %>%
    filter(group == "mobile_phase")
  if (nrow(mobile_phases) == 0) {
    log_it("info", "No mobile phase information provided.", log_ns)
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
      peak_id = peak_id,
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

#' Resolve and store mass spectral data during import
#'
#' Use peak IDs generated by the import workflow to assign and store mass
#' spectral data (if coming from the NIST NTA Method Reporting Tool, these will
#' all be in the "separated" format). Optionally also calls [resolve_ms_spectra]
#' if unpack_spectra = TRUE. Mass spectral data are stored in either one
#' ("zipped")
#'
#' @note This function is called as part of [full_import()] during the call to
#'   [resolve_peaks]
#'
#' @inheritParams map_import
#' @inheritParams resolve_ms_spectra
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param ms_data_in CHR scalar of the named component of `obj` holding mass
#'   spectral data (default: "msdata")
#' @param as_object LGL scalar indicating whether or not to return the result to
#'   the session as an object (TRUE) or to add it to the database (default:
#'   FALSE)
#' @param unpack_spectra LGL scalar indicating whether or not to unpack spectral
#'   data to a long format (i.e. all masses and intensities will become a single
#'   record) in the table defined by `ms_spectra_table` (default: FALSE)
#'
#' @return If `as_object` == TRUE, a data.frame object containing either packed
#'   (if `unpack_spectra` == FALSE) or unpacked (if `unpack_spectra` == TRUE)
#'   spectra, otherwise adds spectra to the database
#'
#' @export
#' 
resolve_ms_data <- function(obj,
                            peak_id = NULL,
                            peaks_table = "peaks",
                            ms_data_in = "msdata",
                            ms_data_table = "ms_data",
                            unpack_spectra = FALSE,
                            ms_spectra_table = "ms_spectra",
                            unpack_format = c("separated", "zipped"),
                            as_object = FALSE,
                            import_map = IMPORT_MAP,
                            db_conn = con,
                            log_ns = "db") {
  # Check connection
  stopifnot(as.integer(peak_id) == peak_id,
            active_connection(db_conn))
  if (is.null(peak_id)) {
    if (as_object) {
      peak_id <- -1
    } else {
      log_it("error", "A value for `peak_id` is required if `as_object` == FALSE.", log_ns)
      stop()
    }
  }
  peak_id <- as.integer(peak_id)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, peak_id, peaks_table, ms_data_in, ms_data_table, unpack_spectra, log_ns),
      conditions = list(
        obj                  = list(c("mode", "list")),
        peak_id              = list(c("mode", "integer"), c("length", 1), "no_na"),
        peaks_table          = list(c("mode", "character"), c("length", 1)),
        ms_data_in           = list(c("mode", "character"), c("length", 1)),
        ms_data_table        = list(c("mode", "character"), c("length", 1)),
        unpack_spectra       = list(c("mode", "logical"), c("length", 1)),
        log_ns               = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  obj <- get_component(obj, ms_data_in) %>%
    purrr::flatten_df()
  ms_data <- map_import(
    import_obj = obj,
    aspect = ms_data_table,
    import_map = import_map,
    db_conn = db_conn,
    log_ns = log_ns
  ) %>%
    bind_rows() %>%
    mutate(peak_id = peak_id) %>%
    filter(!measured_mz == "",
           !measured_intensity == "") %>%
    select(any_of(dbListFields(con, ms_data_table)))
  if (!as_object) {
    res <- try(
      build_db_action(
        action = "insert",
        table_name = ms_data_table,
        values = ms_data,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
    if (inherits(res, "try-error")) {
      msg <- glue::glue('There was an issue adding records to table "{ms_data_table}".')
      log_it("error", msg, log_ns)
      stop(msg)
    } else {
      log_it("success", glue::glue("Mass spectral data added to table '{ms_data_table}'."), log_ns)
    }
  }
  if (unpack_spectra) {
    unpack_format <- match.arg(unpack_format)
    if (as_object) {
      spectra <- tidy_spectra(target = ms_data,
                              is_file = FALSE,
                              is_JSON = FALSE,
                              is_format = unpack_format)
    } else {
      spectra <- resolve_ms_spectra(peak_id = peak_id,
                                    ms_data_table = ms_data_table,
                                    ms_spectra_table = ms_spectra_table,
                                    unpack_format = unpack_format,
                                    as_object = as_object,
                                    db_conn = db_conn,
                                    log_ns = log_ns
      )
    }
  }
  if (as_object) {
    if (unpack_spectra) {
      if (peak_id == -1) spectra <- select(spectra, -peak_id)
      return(spectra)
    } else {
      if (peak_id == -1) ms_data <- select(ms_data, -peak_id)
      return(ms_data)
    }
  }
}

#' Unpack mass spectral data in compressed format
#'
#' For some spectra, searching in a long form is much more performant. Use this
#' function to unpack data already present in the `ms_data` table into the
#' `ms_spectra` table. Data should be packed in one of two ways, either two
#' columns for mass-to-charge ratio and intensity ("separated" - see
#' [ms_spectra_separated]) or in a single column with interleaved data ("zipped"
#' - see [ms_spectra_zipped]).
#'
#' @references ms_spectra_separated
#' @references ms_spectra_zipped
#'
#' @note This function may be slow, especially with peaks containing a large
#'   number of scans or a large amount of data
#'
#' @param peak_id INT scalar of the peak ID in question, which must be present
#' @param spectra_data data.frame object containing spectral data 
#' @param peaks_table CHR scalar name of the peaks table in the database
#'   (default: "peaks")
#' @param ms_data_table CHR scalar name of the table holding packed spectra in
#'   the database (default: "ms_data")
#' @param ms_spectra_table CHR scalar name of the table holding long form
#'   spectra in the database (default: "ms_spectra")
#' @param unpack_format CHR scalar of the type of data packing for the spectra,
#'   one of "separated" (default) or "zipped"
#' @param as_object LGL scalar of whether to return the unpacked spectra to the
#'   session (default: TRUE) or to insert into the database (FALSE)
#' @param db_conn database connection object (default: con)
#' @param log_ns CHR scalar name of the logging namespace to use
#'
#' @return If `as_object` == TRUE, a data.frame of unpacked spectra, otherwise
#'   no return and a database insertion will be performed
#' @export
#' 
resolve_ms_spectra <- function(peak_id,
                               spectra_data = NULL,
                               peaks_table = "peaks",
                               ms_data_table = "ms_data",
                               ms_spectra_table = "ms_spectra",
                               unpack_format = c("separated", "zipped"),
                               as_object = FALSE,
                               db_conn = con,
                               log_ns = "db") {
  # Check connection
  stopifnot(as.integer(peak_id) == peak_id,
            active_connection(db_conn))
  peak_id <- as.integer(peak_id)
  peak_id_check <- check_for_value(peak_id, peaks_table, "id")
  if (!peak_id_check$exists) {
    log_it("error",
           glue::glue("There is no peak record for peak_id {format_list_of_names(peak_id)}."),
           log_ns)
    return(invisible(NULL))
  } else {
    if (!nrow(peak_id_check$values) == length(peak_id)) {
      log_it("warn",
             glue::glue("The following peak_ids were not found in '{peaks_table}': {format_list_of_names(peak_id[!peak_id %in% peak_id_check$values$id])} and will not be included here. Add to '{peak_table}' first and then"),
             log_ns)
      peak_id <- peak_id_check$values$id
    }
  }
  peak_id_check <- check_for_value(peak_id, ms_data_table, "peak_id")
  if (!peak_id_check$exists) {
    log_it("error",
           glue::glue("Mass spectra data have not yet been added in table '{ms_data_table}' for peak_id {format_list_of_names(peak_id)}. Call 'resolve_ms_data' with 'unpack_spectra = TRUE' to add and unpack in one step."),
           log_ns)
    return(invisible(NULL))
  } else {
    if (!all(peak_id %in% peak_id_check$values$peak_id)) {
      log_it("warn",
             glue::glue("The following peak_ids were not found in '{ms_data_table}': {format_list_of_names(peak_id[!peak_id %in% peak_id_check$values$peak_id])}; these will not be included. Call 'resolve_ms_data' with 'unpack_spectra = TRUE' to add and unpack in one step."),
             log_ns)
      peak_id <- peak_id_check$values$id
    }
  }
  rm(peak_id_check)
  unpack_format <- match.arg(unpack_format)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment())[-which(names(environment()) == "spectra_data")],
      conditions = list(
        peak_id              = list(c("mode", "integer"), c("n>=", 1), "no_na"),
        peaks_table          = list(c("mode", "character"), c("length", 1)),
        ms_data_table        = list(c("mode", "character"), c("length", 1)),
        ms_spectra_table     = list(c("mode", "character"), c("length", 1)),
        unpack_format        = list(c("mode", "character"), c("length", 1)),
        as_object            = list(c("mode", "logical"), c("length", 1)),
        db_conn              = list(c("length", 1)),
        log_ns               = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  unpack_format <- switch(
    unpack_format,
    "separated" = ms_spectra_separated,
    "zipped" = ms_spectra_zipped
  )
  ms_spectra <- build_db_action(
    action = 'select',
    table_name = ms_data_table,
    match_criteria = list(peak_id = peak_id)
  ) %>%
    select(id, measured_mz, measured_intensity) %>%
    rename(c("ms_data_id" = "id")) %>%
    unpack_format() %>%
    tidy_ms_spectra()
  if (as_object) {
    return(ms_spectra)
  } else {
    res <- try(
      build_db_action(
        action = "insert",
        table_name = ms_spectra_table,
        values = ms_spectra,
        db_conn = db_conn,
        log_ns = log_ns
      )
    )
    if (inherits(res, "try-error")) {
      msg <- glue::glue('There was an issue adding records to table "{ms_data_table}".')
      log_it("error", msg, log_ns)
      stop(msg)
    } else {
      log_it("success", glue::glue("Mass spectra for peak_id {format_list_of_names(peak_id)} unpacked into table '{ms_spectra_table}'."), log_ns)
    }
  }
}

#' Resolve the peaks node during import
#'
#' Call this function to resolve and insert information for the "peaks" node in
#' the database including software conversion settings (via
#' [resolve_software_settings_NTAMRT]) and mass spectra data (via
#' [resolve_ms_data] and, optionally, [resolve_ms_spectra]). This function
#' relies on the import object being formatted appropriately.
#'
#' @note This function is called as part of [full_import()]
#'
#' @note This function relies on an import map
#'
#' @inheritParams resolve_ms_data
#' @inheritParams resolve_ms_spectra
#' @inheritParams resolve_peak_ums_params
#'
#' @param obj CHR vector describing settings or a named LIST with names matching
#'   column names in table conversion_software_settings.
#' @param sample_id INT scalar of the sample id (e.g. from the import workflow)
#' @param peaks_table CHR scalar of the database table name holding QC method check
#'   information (default: "peaks")
#' @param db_conn Connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return INT scalar of the newly inserted or identified peak ID(s)
#' @export
#' 
resolve_peaks <- function(obj,
                          sample_id,
                          peaks_table = "peaks",
                          software_timestamp = NULL,
                          software_settings_in = "msconvertsettings",
                          ms_data_in = "msdata",
                          ms_data_table = "ms_data",
                          unpack_spectra = FALSE,
                          unpack_format = c("separated", "zipped"),
                          ms_spectra_table = "ms_spectra",
                          linkage_table = "conversion_software_peaks_linkage",
                          settings_table = "conversion_software_settings",
                          as_date_format = "%Y-%m-%d %H:%M:%S",
                          format_checks = c("ymd_HMS", "ydm_HMS", "mdy_HMS", "dmy_HMS"),
                          min_datetime = "2000-01-01 00:00:00",
                          import_map = IMPORT_MAP,
                          ums_params_in = "opt_ums_params",
                          ums_params_table = "opt_ums_params",
                          db_conn = con,
                          log_ns = "db") {
  stopifnot(as.integer(sample_id) == sample_id,
            active_connection(db_conn))
  unpack_format <- match.arg(unpack_format)
  sample_id <- as.integer(sample_id)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, sample_id, peaks_table, software_settings_in,
                        ms_data_in, ms_data_table, unpack_spectra,
                        unpack_format, ms_spectra_table, linkage_table,
                        settings_table, as_date_format, format_checks,
                        import_map, ums_params_in, ums_params_table, db_conn,
                        log_ns),
      conditions = list(
        obj                  = list(c("mode", "list")),
        sample_id            = list(c("mode", "integer"), c("length", 1), "no_na"),
        peaks_table          = list(c("mode", "character"), c("length", 1), "no_na"),
        software_settings_in = list(c("mode", "character"), c("length", 1), "no_na"),
        ms_data_in           = list(c("mode", "character"), c("length", 1), "no_na"),
        ms_data_table        = list(c("mode", "character"), c("length", 1), "no_na"),
        unpack_spectra       = list(c("mode", "logical"), c("length", 1), "no_na"),
        unpack_format        = list(c("mode", "character"), c("length", 1), "no_na"),
        ms_spectra_table     = list(c("mode", "character"), c("length", 1), "no_na"),
        linkage_table        = list(c("mode", "character"), c("length", 1), "no_na"),
        settings_table       = list(c("mode", "character"), c("length", 1), "no_na"),
        as_date_format       = list(c("mode", "character"), c("length", 1), "no_na"),
        format_checks        = list(c("mode", "character")),
        import_map           = list(c("mode", "list"), c("n>", 1)),
        ums_params_in        = list(c("mode", "character"), c("length", 1), "no_na"),
        ums_params_table     = list(c("mode", "character"), c("length", 1), "no_na"),
        db_conn              = list(c("length", 1)),
        log_ns               = list(c("mode", "character"), c("length", 1), "no_na")
      )
    )
    stopifnot(arg_check$valid)
  }
  # Map peaks values
  peaks_values <- map_import(
    import_obj = obj,
    aspect = peaks_table,
    fuzzy = TRUE,
    import_map = import_map
  )
  peaks_values <- tack_on(
    peaks_values,
    num_points = bind_rows(obj$msdata) %>%
      filter(ms_n == 1) %>%
      nrow()
  )
  # Insert or identify software settings
  software_settings_id <- resolve_software_settings_NTAMRT(
    obj = obj,
    software_settings_in = software_settings_in,
    software_timestamp = software_timestamp,
    settings_table = settings_table,
    linkage_table = linkage_table,
    as_date_format = as_date_format,
    format_checks = format_checks,
    min_datetime = min_datetime,
    db_conn = db_conn,
    log_ns = log_ns
  )
  # Add values to peaks
  peaks_values <- peaks_values %>%
    tack_on(
      sample_id = sample_id,
      conversion_software_peaks_linkage_id = software_settings_id
    )
  # Insert or identify peaks
  res <- try(
    add_or_get_id(
      db_table = peaks_table,
      values = peaks_values,
      db_conn = db_conn,
      log_ns = log_ns
    )
  )
  if (inherits(res, "try-error")) {
    msg <- glue::glue('There was an issue adding records to table "{peaks_table}".')
    log_it("error", msg)
    stop(msg)
  }
  peak_id <- res
  # Map ms_data
  resolve_ms_data(
    obj = obj,
    peak_id = peak_id,
    ms_data_in = ms_data_in,
    ms_data_table = ms_data_table,
    unpack_spectra = unpack_spectra,
    ms_spectra_table = ms_spectra_table,
    unpack_format = unpack_format,
    import_map = import_map,
    db_conn = db_conn,
    log_ns = log_ns
  )
  # Resolve UMS parameters (optional)
  if (ums_params_in %in% names(obj)) {
    resolve_peak_ums_params(
      obj = obj,
      peak_id = peak_id,
      ums_params_in = ums_params_in,
      ums_params_table = ums_params_table,
      db_conn = db_conn,
      log_ns = log_ns
    )
  }
  return(peak_id)
}

#' Resolve and import optimal uncertain mass spectrum parameters
#'
#' This imports the defined object component containing parameters for the
#' optimized uncertainty mass spectrum used to compare with new data. This
#' function may be called at any time to add data for a given peak, but there is
#' no row unique restriction on the underlying table and is best used in a "one
#' pass" method during the import routine. These parameters are calculated as
#' part of NIST QA procedures and are added to the output of the NTA MRT after
#' those JSONs have been created.
#'
#' @note This function is called as part of [resolve_peaks()]
#'
#' @inheritParams build_db_action
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param peak_id INT scalar of the peak ID in question, which must be present
#'   (e.g. from the import workflow)
#' @param ums_params_in CHR scalar name of the item in `obj` containing
#'   optimized uncertainty mass spectrum parameters
#' @param ums_params_table CHR scalar name of the database table holding
#'   optimized uncertainty mass spectrum parameters
#'
#' @return Nothing if successful, a data frame object of the extracted
#'   parameters otherwise.
#' @export
#' 
resolve_peak_ums_params <- function(obj,
                                    peak_id,
                                    ums_params_in = "opt_ums_params",
                                    ums_params_table = "opt_ums_params",
                                    db_conn = con,
                                    log_ns = "db") {
  stopifnot(peak_id == as.integer(peak_id))
  peak_id <- as.integer(peak_id)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        obj              = list(c("mode", "list")),
        peak_id          = list(c("mode", "integer"), c("length", 1), "no_na"),
        ums_params_in    = list(c("mode", "character"), c("length", 1), "no_na"),
        ums_params_table = list(c("mode", "character"), c("length", 1), "no_na"),
        db_conn          = list(c("length", 1)),
        log_ns           = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  ums_params <- obj %>%
    get_component(ums_params_in) %>%
    purrr::list_flatten() %>%
    lapply(function(x) {
      as_tibble(x) %>%
        mutate(across(.cols = -c(1, ncol(.)), .fns = as.numeric))
    }) %>%
    bind_rows() %>%
    mutate(peak_id = peak_id) %>%
    select(all_of(dbListFields(db_conn, ums_params_table)))
  res <- try(
    build_db_action("insert", ums_params_table, values = ums_params)
  )
  if (inherits(res, "try-error")) {
    log_it("error", glue::glue('There was a problem inserting records into "{ums_params_table}."'), log_ns)
    return(ums_params)
  } else {
    return(NULL)
  }
}

#' Resolve and import quality control method information
#'
#' This imports the defined object component containing QC method information
#' (i.e. a data frame of multiple quality control checks) from the NIST
#' Non-Targeted Analysis Method Reporting Tool (NTA MRT).
#'
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams build_db_action
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param peak_id INT vector of the peak ids (e.g. from the import workflow)
#' @param qc_method_in CHR scalar of the name in `obj` that contains QC method
#'   check information (default: "qcmethod")
#' @param qc_method_table CHR scalar of the database table name holding QC
#'   method check information (default: "qc_methods")
#' @param qc_method_norm_table CHR scalar name of the database table normalizing
#'   QC methods type (default: "norm_qc_methods_name")
#' @param qc_method_norm_reference CHR scalar name of the database table
#'   normalizing QC methods reference type (default:
#'   "norm_qc_methods_reference")
#' @param qc_references_in CHR scalar of the name in `obj[[qc_method_in]]` that
#'   contains the reference or citation for the QC protocol (default: "source")
#' @param peaks_table CHR scalar name of the database table holding sample
#'   information (default: "samples")
#'
#' @return None, executes actions on the database
#' @export
#' 
resolve_qc_methods_NTAMRT <- function(obj,
                                      peak_id,
                                      qc_method_in = "qcmethod",
                                      qc_method_table = "qc_methods",
                                      qc_method_norm_table = "norm_qc_methods_name",
                                      qc_method_norm_reference = "norm_qc_methods_reference",
                                      qc_references_in = "source",
                                      peaks_table = "peaks",
                                      ignore = FALSE,
                                      db_conn = con,
                                      log_ns = "db") {
  # Check connection
  stopifnot(
    active_connection(db_conn),
    as.integer(peak_id) == peak_id,
    check_for_value(values = peak_id,
                    db_table = peaks_table,
                    db_column = "id",
                    db_conn = db_conn)$exists
  )
  values <- get_component(obj, qc_method_in)[[1]] %>%
    bind_rows() %>%
    mutate(
      peak_id = list(peak_id),
      name = sapply(
        name,
        function(x) {
          resolve_normalization_value(
            this_value = x,
            db_table = qc_method_norm_table,
            db_conn = db_conn,
            log_ns = log_ns
          )
        }
      ) %>%
        unname()
      ) %>%
    rename(c("reference" = !!qc_references_in)) %>%
    mutate(
      reference = ifelse(reference == "", NA, reference),
      reference = ifelse(is.na(reference),
                         NA,
                         sapply(
                           reference,
                           function(x) {
                             resolve_normalization_value(
                               this_value = x,
                               db_table = qc_method_norm_reference,
                               db_conn = db_conn,
                               log_ns = log_ns
                             )
                           }
                         ))
    ) %>%
    unnest(c(peak_id))
  res <- try(
    build_db_action(action = "insert",
                    table_name = qc_method_table,
                    db_conn = db_conn,
                    values = values,
                    ignore = ignore,
                    log_ns = log_ns)
  )
  if (inherits(res, "try-error")) {
    msg <- glue::glue('There was an issue adding records to table "{qc_method_table}".')
    log_it("error", msg)
    stop(msg)
  }
}


#' Resolve and import quality control data for import
#'
#' This imports the defined object component containing QC data (i.e. a nested
#' list of multiple quality control checks) from the NIST Non-Targeted Analysis
#' Method Reporting Tool (NTA MRT).
#'
#' @note This function is called as part of [full_import()]
#'
#' @inheritParams build_db_action
#'
#' @param obj LIST object containing data formatted from the import generator
#' @param peak_id INT vector of the peak ids (e.g. from the import workflow)
#' @param qc_data_in CHR scalar name of the component in `obj` containing QC
#'   data (default: "qc")
#' @param qc_data_table CHR scalar name of the database table holding QC data
#'   (default: "qc_data")
#' @param peaks_table CHR scalar name of the database table holding peaks data
#'   (default: "peaks")
#'
#' @return None, executes actions on the database
#' @export
#' 
resolve_qc_data_NTAMRT <- function(obj,
                                   peak_id,
                                   qc_data_in = "qc",
                                   qc_data_table = "qc_data",
                                   peaks_table = "peaks",
                                   ignore = FALSE,
                                   db_conn = con,
                                   log_ns = "db") {
  # Argument validation relies on verify_args
  stopifnot(as.integer(peak_id) == peak_id)
  peak_id <- as.integer(peak_id)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(obj, peak_id, ignore, db_conn, log_ns),
      conditions = list(
        obj       = list(c("mode", "list")),
        peak_id   = list(c("mode", "integer"), c("length", 1), "no_na"),
        ignore    = list(c("mode", "logical"), c("length", 1)),
        db_conn   = list(c("length", 1)),
        log_ns    = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  # Sanity check that sample_id exists
  stopifnot(
    active_connection(db_conn),
    check_for_value(peak_id, peaks_table, "id", db_conn = db_conn)$exists
  )
  # obj <- get_component(obj, qc_data_in) %>%
    # list_flatten() %>%
    # list_flatten()
  # obj <- obj[which(lapply(obj, function(x) length(x) > 1) == TRUE)]
  values <- obj %>%
    get_component(qc_data_in) %>%
    flatten_df() %>%
    mutate_all(as.character) %>%
    arrange(parameter, measuredmz) %>%
    group_by(parameter) %>%
    mutate(
      applies_to = case_when(
        n() == 1 ~ "peak",
        TRUE ~ paste0("fragment_", 1:n())
      )
    ) %>%
    pivot_longer(cols = c(-parameter, -applies_to)) %>%
    filter(!is.na(value)) %>%
    mutate(
      name = ifelse(name == "result", "qc_pass", name),
      peak_id = peak_id
    ) %>%
    ungroup() %>%
    arrange(peak_id, applies_to, parameter) %>%
    select(all_of(dbListFields(db_conn, qc_data_table)))
  res <- try(
    build_db_action(
      action = "insert",
      table_name = qc_data_table,
      ignore = ignore,
      db_conn = db_conn,
      values = values
    )
  )
  if (inherits(res, "try-error")) {
    msg <- glue::glue('There was an issue adding records to table "{qc_data_table}".')
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
#' @return None, executes actions on the database
#' @export
remove_sample <- function(sample_ids, db_conn = con, log_ns = "db") {
  log_fn("start")
  if (any(sample_ids != as.integer(sample_ids))) stop('Parameter "sample_ids" cannot be safely coerced to integer.')
  sample_ids <- as.integer(sample_ids)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sample_ids = list(c("mode", "integer"), c("n>=", 1)),
        db_conn    = list(c("length", 1)),
        log_ns     = list(c("mode", "character"), c("n>=", 1))
      )
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
#' @param not_required CHR vector matching element names of `example_import`
#'   which are not required; all others will be assumed to be required
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
#' 
make_requirements <- function(example_import,
                              file_name = "import_requirements.json",
                              not_required = c("annotation", "chromatography", "opt_ums_params"),
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
    example_import <- try(
      fromJSON(read_file(f_name))
    )
    if (class(example_import) == "try-error") {
      stop(sprintf('The file "%s" was not readable as JSON.', f_name))
    }
  } else {
    f_name <- file_name
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
  for (nr in not_required) {
    out[[nr]]$required <- FALSE
  }
  if (retain_in_R) import_requirements <<- out
  out <- toJSON(out, auto_unbox = TRUE, pretty = TRUE)
  write_file(out, file.path(here::here("config", f_name)))
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
#' 
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
    stop(glue('Required column{ifelse(length(missing_columns) > 1, "s are", " is")} missing: {format_list_of_names(missing_columns)}.'))
  } else {
    valid_columns <- provided %in% table_info$name
    if (!all(valid_columns)) {
      extra_columns <- provided[!valid_columns]
      log_it("warn", glue('Extra column{ifelse(length(extra_columns) > 1, "s were", " was")} provided and will be ignored: {format_list_of_names(extra_columns)}.'), log_ns)
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
#' The return from this is a tibble with 9 columns. The first is the name of the
#' import object member, typically the file name. If a single, unnested import
#' object is provided this will be "import object". The other columns include
#' the following verification checks:
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
#' 5. has_extra: Are there unexpected values provided? (TRUE/FALSE)
#'
#' 6. extra_cols: Character vectors naming any has_extra columns; these will be
#' dropped from the import but are provided for information sake
#'
#' 7. has_name_mismatches: Are there name differences between the import
#' requirement elements and the import object? (TRUE/FALSE)
#'
#' 8. mismatched_names: Named lists enumerating which named elements (if any)
#' from the import object did not match name expectations in the requirements
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
#' @return A tibble object with 9 columns containing the results of the checks.
#'
#' @export
#' 
verify_import_requirements <- function(obj,
                                       ignore_extra = TRUE,
                                       requirements_obj = "import_requirements",
                                       file_name = "import_requirements",
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
  f_name <- sprintf("%s.json", file_name)
  if (exists(requirements_obj)) {
    reqs <- eval(rlang::sym(requirements_obj))
  } else {
    f_name <- list.files(pattern = f_name, recursive = TRUE, full.names = TRUE)
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
  element_names  <- map(reqs, ~ .$names)
  element_names  <- element_names[-which(sapply(element_names, is.null))]
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
    mismatched_names <- sapply(
      req_names,
      function(x) {
        setdiff(element_names[[x]], names(obj[[x]]))
      }
    )
    mismatched_names <- mismatched_names[sapply(mismatched_names, length) > 0]
    has_name_mismatches <- length(mismatched_names) > 0
    out$has_name_mismatches <- unlist(has_name_mismatches)
    out$mismatched_names <- list(mismatched_names)
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
                   format_list_of_names(extra_cols, add_quotes = TRUE),
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
      mutate(import_object = names(obj))
  } else {
    out <- as_tibble(out) %>%
      mutate(import_object = "import object")
  }
  out <- relocate(out, import_object, .before = everything())
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
#' @inheritParams resolve_normalization_value
#'
#' @param import_obj LIST object of values to import
#' @param aspect CHR scalar of the import aspect (e.g. "sample") to map
#' @param import_map data.frame object of the import map (e.g. from a CSV)
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
                       fuzzy = FALSE,
                       ignore = TRUE,
                       id_column = "_*id$",
                       alias_column = "^alias$",
                       resolve_normalization = TRUE,
                       strip_na = FALSE,
                       db_conn = con,
                       log_ns = "db") {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        import_obj             = list(c("mode", "list")),
        aspect                 = list(c("mode", "character"), c("length", 1)),
        import_map             = list(c("mode", "data.frame"), c("n>", 0)),
        case_sensitive         = list(c("mode", "logical"), c("length", 1)),
        fuzzy                  = list(c("mode", "logical"), c("length", 1)),
        ignore                 = list(c("mode", "logical"), c("length", 1)),
        id_column              = list(c("mode", "character"), c("length", 1)),
        alias_column           = list(c("mode", "character"), c("length", 1)),
        resolve_normalization  = list(c("mode", "logical"), c("length", 1)),
        strip_na               = list(c("mode", "logical"), c("length", 1)),
        db_conn                = list(c("length", 1)),
        log_ns                 = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(dbIsValid(db_conn),
            aspect %in% dbListTables(db_conn))
  this_map <- import_map %>%
    filter(sql_table == aspect,
           import_parameter != "")
  if (!any(names(import_obj) %in% unique(this_map$import_category)) &&
      any(names(import_obj) %in% unique(this_map$import_parameter))) {
    log_it("trace", "Import object is not nested but contains referenced names. Recasting.", log_ns)
    this_map$import_category <- "import"
    import_obj <- list(import_obj) %>%
      setNames(unique(this_map$import_category))
  }
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
          if (is.null(param_value)) param_value <- NA
          out[[j]] <- param_value
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
        # TODO inserts for straight key:value pair long form tables
      } else {
        category  <- properties$import_category[1]
        parameter <- properties$import_parameter[1]
        alias_in  <- properties$alias_lookup[1]
        norm_by   <- properties$sql_normalization[1]
        # At some point an extra layer was introduced to import files, creating a gap space for mapping in this manner.
        if (!parameter %in% names(import_obj[[category]])) {
          import_obj[[category]] <- purrr::flatten(import_obj[[category]])
        }
        this_val  <- import_obj[[category]][[parameter]]
        if (has_missing_elements(this_val) && length(this_val) < 2) this_val <- NA
        if (has_missing_elements(alias_in) && length(alias_in) < 2) alias_in <- NA
        if (has_missing_elements(norm_by) && length(norm_by) < 2) norm_by <- NA
        if (!all(is.na(this_val))) {
          if (!is.na(norm_by)) {
            norm_id <- integer(0)
            alias_id <- integer(0)
            column_names <- character(0)
            this_val <- this_val %>%
              sapply(
                function(this_val) {
                  if (this_val == "" || is.na(this_val)) return(NA)
                  log_it("info", glue::glue("Attempt resolution of '{this_field}': norm_by = '{norm_by}' and this_val = '{this_val}'."), log_ns)
                  if (!is.na(alias_in)) {
                    log_it("info", glue::glue("Resolving aliases for '{this_field}' using aliases in '{alias_in}'."), log_ns)
                    column_names <- dbListFields(db_conn, alias_in)
                    alias_column <- grep(alias_column, column_names, value = TRUE)[1]
                    id_column <- grep(id_column, column_names, value = TRUE)[1]
                    lookup_val <- list(list(values = this_val, like = TRUE)) %>%
                      setNames(alias_column)
                    alias_id <- try(
                      build_db_action(
                        action = "select",
                        table_name = alias_in,
                        match_criteria = lookup_val,
                        ignore = ignore,
                        db_conn = db_conn,
                        log_ns = log_ns
                      )
                    )
                    if (inherits(alias_id, "try-error")) {
                      msg <- glue::glue("There was a problem resolving the alias for {this_field}.")
                      log_it("error", msg, log_ns)
                      stop(msg)
                    }
                    if (!nrow(alias_id) == 1) {
                      msg <- sprintf("%s aliases identified in table '%s' for value '%s'.",
                                     ifelse(nrow(alias_id) == 0, "No", "Multiple"),
                                     alias_in,
                                     this_val)
                      log_it("info", msg, log_ns)
                      if (nrow(alias_id) > 1 && resolve_normalization) {
                        alias_id <- resolve_normalization_value(
                          this_value = this_val,
                          db_table = alias_in,
                          case_sensitive = case_sensitive,
                          fuzzy = fuzzy,
                          id_column = id_column,
                          db_conn = db_conn,
                          log_ns = log_ns
                        )
                        norm_id <- alias_id
                      }
                    } else {
                      norm_id <- alias_id %>%
                        slice(1) %>%
                        pull(id_column)
                      log_it("success", glue::glue("Resolved alias for '{this_field}' as id = '{norm_id}'."), log_ns)
                    }
                  }
                  if (!length(norm_id) == 1 && resolve_normalization) {
                    norm_id <- resolve_normalization_value(
                      this_value = this_val,
                      db_table = norm_by,
                      case_sensitive = case_sensitive,
                      fuzzy = fuzzy,
                      db_conn = db_conn,
                      log_ns = log_ns
                    )
                    if (!is.na(alias_in)) {
                      res <- try(
                        build_db_action(
                          action = "insert",
                          table_name = alias_in,
                          values = list(norm_id, this_val) %>%
                            setNames(column_names[1:2]),
                          ignore = ignore,
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
                    if (length(alias_id) == 0) {
                      log_it("success", glue::glue("Resolved normalization value for '{this_field}' as id = '{norm_id}'."), log_ns)
                    }
                    this_val <- norm_id
                  } else {
                    log_it("error", glue::glue("Could not resolve a normalization value for '{this_field}'."), log_ns)
                  }
                })
          }
          
        }
        out[[i]] <- unname(this_val)
      }
    }
  }
  if (all(lapply(out, length) == 1)) {
    if (strip_na) {
      na_only <- which(lapply(out, is.na) == TRUE)
      if (length(na_only)) {
        out <- out[-na_only]
      }
    }
  }
  return(out)
}

#' Add fragment or compound aliases generated by RDKit functions
#'
#' Aliases are stored for both compounds and fragments within the database to
#' facilitate search and unambiguous identification. Given one molecular
#' structure notation (SMILES is preferred), other machine-readable expressions
#' can be generated quickly. Requested aliases as provided to `rdkit_aliases`
#' will be prefixed by `mol_to_prefix` and checked against the namespace of
#' available functions in RDKit and the correct functions automatically
#' assigned.
#'
#' @note It is not recommended to change the defaults here unless you are
#'   familiar with the naming conventions of RDKit.
#' @note Requires both INFORMATICS and USE_RDKIT set to TRUE in the session and
#'   a valid installation of the RDKIT python environment to function.
#' @note See the [RDKit Documentation](https://rdkit.org/docs/index.html) for
#'   more details.
#'
#' @param identifiers CHR vector of machine readable notations in `type` format
#' @param alias_category CHR scalar, one of "compounds" or "fragments" to
#'   determine where in the database to store the resulting aliases (default:
#'   "compounds")
#' @param compound_aliases_table CHR scalar name of the database table holding
#'   compound aliases (default: "compound_aliases")
#' @param fragment_aliases_table CHR scalar name of the database table holding
#'   fragment aliases (default: "fragment_aliases")
#' @param inchi_prefix CHR scalar prefix for the InChI code to use, if InChI is
#'   requested as part of `rdkit_aliases`
#' @param rdkit_name CHR scalar name of the python environment at which RDKit is
#'   installed (default: is the session variable PYENV_NAME or "rdkit")
#' @param rdkit_ref CHR scalar name of the R pointer object to RDKit (default:
#'   is the session variable PYENV_REF or "rdk")
#' @param rdkit_ns CHR scalar name of the logging namespace to use (default:
#'   "rdk"); will be ignored if logging is off
#' @param rdkit_make_if_not LGL scalar of whether to create an RDKit environment
#'   if it does not exist (default: TRUE)
#' @param rdkit_aliases CHR vector of machine-readable aliases to generate,
#'   which must be recognizeable as names in the RDKit namespace when prefixed
#'   by `mol_to_prefix` (default: c("inchi", "inchikey")); these are not case
#'   sensitive
#' @param mol_to_prefix CHR scalar of the prefix identifying alias creation
#'   functions, which must be recognizeable as names in the RDKit namespace when
#'   suffixed by `rdkit_aliases` (default: "MolTo"); this is not case sensitive
#' @param mol_from_prefix CHR scalar of the prefix identifying molecule
#'   expression creation functions, which must be recognizeable as names in the
#'   RDKit namespace when suffixed by `type` (default: "MolFrom"); this is not
#'   case sensitive
#' @param type CHR scalar indicating the type of `identifiers` to be converted
#'   to molecule notation (default: "smiles"); this is not case sensitive
#' @param as_object LGL scalar indicating whether to return the alias list to
#'   the session as an object (default: TRUE) or write aliases to the database
#'   (FALSE)
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return If `as_object` == TRUE, a data.frame of unpacked spectra, otherwise
#'   no return and a database insertion will be performed
#' @export
#'
add_rdkit_aliases <- function(identifiers,
                              alias_category = c("compounds", "fragments"),
                              compound_aliases_table = "compound_aliases",
                              fragment_aliases_table = "fragment_aliases",
                              inchi_prefix = "InChI=1S/",
                              rdkit_name = ifelse(exists("PYENV_NAME"), PYENV_NAME, "rdkit"),
                              rdkit_ref = ifelse(exists("PYENV_REF"), PYENV_REF, "rdk"),
                              rdkit_ns = "rdk",
                              rdkit_make_if_not = TRUE,
                              rdkit_aliases = c("inchi", "inchikey"),
                              mol_to_prefix = "MolTo",
                              mol_from_prefix = "MolFrom",
                              type = "smiles",
                              as_object = TRUE,
                              db_conn = con,
                              log_ns = "rdk") {
  alias_table <- switch(alias_category,
                        "compounds" = compound_aliases_table,
                        "fragments" = fragment_aliases_table)
  alias_type_norm_table <- ref_table_from_map(alias_table, "alias_type")
  alias_values <- list(
    smiles = NA,
    fragment_id = NA
  )
  if (exists("INFORMATICS") && INFORMATICS && USE_RDKIT) {
    if (!tolower(type) %in% tolower(names(identifiers))) {
      log_it("error", glue::glue("Provided data do not include a column named '{type}'."), log_ns)
    } else {
      log_it("info", glue::glue("Generating aliases for {alias_category} using `rdkit_mol_aliases`."), log_ns)
      alias_values <- rdkit_mol_aliases(identifiers = identifiers,
                                        type = type,
                                        get_aliases = rdkit_aliases,
                                        rdkit_ref = rdkit_ref,
                                        log_ns = rdkit_ns,
                                        make_if_not = rdkit_make_if_not)
    }
  } else {
    settings <- paste(ifelse(INFORMATICS, "", "INFORMATICS"),
                      ifelse(!INFORMATICS & !USE_RDKIT, "and", ""),
                      ifelse(USE_RDKIT, "", "RDKit integration"),
                      ifelse(!INFORMATICS & !USE_RDKIT, "are", "is"),
                      "turned off."
    ) %>%
      stringr::str_squish()
    log_it("warn", settings, log_ns)
  }
  id_col <- grep("id$", names(identifiers), value = TRUE)
  # TODO check alias generation for compounds
  alias_values <- alias_values %>%
    bind_rows() %>%
    mutate(across(matches("^inchi$", ignore.case = TRUE),
                  ~ ifelse(test = grepl(inchi_prefix, .x),
                           yes = .x,
                           no = paste0(inchi_prefix, .x))),
           across(matches("^inchi$", ignore.case = TRUE),
                  ~ str_replace_all(
                    string = .x,
                    pattern = sprintf("(%s){2}", gsub("InChI=", "", inchi_prefix)),
                    replacement = gsub("InChI=", "", inchi_prefix)))
    ) %>%
    left_join(identifiers) %>%
    filter(!is.na(.data[[id_col]])) %>%
    select(1:.data[[id_col]]) %>%
    pivot_longer(col = -.data[[id_col]]) %>%
    setNames(dbListFields(db_conn, alias_table)) %>%
    mutate(alias_type = sapply(alias_type,
                               function(x) {
                                 resolve_normalization_value(x, alias_type_norm_table)
                               }))
  if (as_object) {
    return(alias_values)
  } else {
    build_db_action(
      action = "insert",
      table_name = alias_table,
      values = alias_values,
      ignore = TRUE,
      db_conn = db_conn,
      log_ns = log_ns
    )
  }
}
