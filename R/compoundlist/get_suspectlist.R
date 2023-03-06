#' Get the current NIST PFAS suspect list.
#'
#' Downloads the current NIST suspect list of PFAS from the NIST Public Data
#' Repository to the current project directory.
#'
#' @param destfile CHR scalar file.path of location to save the downloaded file
#' @param url_file CHR scalar file.path of where the text file containing the
#'   download URL for the NIST PFAS Suspect List
#' @param save_local LGL scalar of whether to retain an R expression in the
#'   current environment after download
#'
#' @return none
#' @export
#'
#' @examples
#' get_suspectlist()
get_suspectlist <- function(destfile = file.path("R", "compoundlist", "suspectlist.xlsx"),
                            url_file = file.path("config", "suspectlist_url.txt"),
                            default_url = SUS_LIST_URL,
                            save_local = FALSE) {
  if (!file.exists(url_file)) {
    log_it("warn", glue('Could not locate the "{url_file}" file in the expected location. Defaulting to {default_url}.'))
    url <- default_url
  } else {
    url <- readLines(url_file, warn = FALSE) #this file must be configured correctly
    if (length(url) > 1) {
      log_it("warn", "Multiple lines were in this file. The first will be treated as the URL.")
      url <- url[1]
    }
  }
  res <- try(download.file(url, destfile, mode = "wb"))
  if (class(res) == "try-error")
    if (file.exists(destfile)) {
      print(paste("The suspect list has been successfully downloaded to", destfile))
      if (save_local) {
        readxl::read_excel(destfile, sheet = 1)
      }
    }
}

#' Open the NIST PDR entry for the current NIST PFAS suspect list
#'
#' This simply points your browser to the NIST public data repository for the
#' current NIST suspect list, where you can find additional information. Click
#' the download button in the left column of any file to download it.
#'s
#' Requires the file "suspectlist_url.txt" to be present in the `config`
#' subdirectory of the current working directory.
#'
#' @return none
#' @export
#'
#' @examples
#' suspectlist_at_NIST()
suspectlist_at_NIST <- function(url_file = file.path('config', 'suspectlist_url.txt')) {
  url_paths <- list.files(pattern = url_file, recursive = TRUE, full.names = TRUE)
  if (length(url_file) > 1) {
    warning(sprintf("Multiple files found matching %s; using only the first one.", url_file, url_paths[1]))
    url_file <- url_paths[1]
  }
  if (!file.exists(url_file)) stop(sprintf('Could not locate the "%s" file in the current directory.', url_file))
  url <- readLines(url_file, warn = FALSE)
  url <- gsub("\\/ds\\/", "\\/id\\/", url)
  url <- dirname(url)
  browseURL(url)
}

#' Extend the compounds and aliases tables
#'
#' Suspect lists are occasionally updated. To keep the current database up to
#' date, run this function by pointing it to the updated or current suspect
#' list. That suspect list should be one of (1) a file in either
#' comma-separated-value (CSV) or a Microsoft Excel format (XLS or XLSX), (2) a
#' data frame containing the new compounds in the standard format of the suspect
#' list, or (3) a URL pointing to the suspect list.
#'
#' If `suspect_list` does not contain one of the expected file extensions, it
#' will be assumed to be a URL pointing to a Microsoft Excel file with the
#' suspect list in the first spreadsheet. The file for that URL will be
#' downloaded temporarily, read in as a data frame, and then removed.
#'
#' Required columns for the compounds table are first pulled and all other
#' columns are treated as aliases. If `retain_current` is TRUE, entries in the
#' "name" column will be matched against current aliases and the compound id
#' will be persisted for that compound.
#'
#' @param suspect_list CHR scalar pointing either to a file (CSV, XLS, or XLSX)
#'   or URL pointing to an XLSX file.
#' @param db_conn connection object (default: con)
#' @param retain_current LGL scalar of whether to retain the current list by
#'   attempting to match new entries to older ones, or to append all entries
#'   (default: TRUE)
#'
#' @return None
#' @export
#' 
extend_suspect_list <- function(suspect_list, db_conn = con, retain_current = TRUE) {
  # Get the current suspect list
  if (is.character(suspect_list)) {
    slist_ext <- tools::file_ext(suspect_list)
    if (slist_ext %in% c("csv", "xls", "xlsx")) {
      if (!file.exists(suspect_list)) {
        suspect_list_files <- list.files(pattern = suspect_list, full.names = TRUE, recursive = TRUE)
        if (length(suspect_list_files) > 1) {
          log_it(
            "warn",
            sprintf(
              'Multiple files identified matching pattern "%s".\n%s\nBy default the first, "%s", will be used.',
              suspect_list,
              paste0(suspect_list_files, collapse = "\n"),
              suspect_list_files[1]
            )
          )
        }
        suspect_list <- suspect_list_files[1]
      }
    }
    if (slist_ext == "csv") {
      slist <- read.csv(suspect_list)
    } else if (slist_ext == "xls") {
      slist <- readxl::read_xls(suspect_list, sheet = 1)
      
    } else if (slist_ext == "xlsx") {
      slist <- readxl::read_xlsx(suspect_list, sheet = 1)
    } else {
      log_it("info", glue("Downloading a new copy from {suspect_list}..."))
      slist_download <- try(download.file(suspect_list, "tmp.xlsx"))
      if (class(slist_download) == "try-error") {
        stop(sprintf('Unable to download from "%s".', suspect_list))
      }
      slist <- readxl::read_excel("tmp.xlsx", sheet = 1)
      file.remove("tmp.xlsx")
    }
  } else if (is.data.frame(suspect_list)) {
    log_it("info", "Assuming the full suspect list was provided.")
    slist <- suspect_list
  } else {
    stop('Only character scalars pointing to a file path or a download URL are supported for parameter "suspect_list".')
  }
  
  # Catch burying of name descriptors in the ADDITIONAL column
  needed <- tbl(con, "compound_alias_references") %>%
    filter(!name %in% c("Alias", "NIST Suspect List")) %>%
    pull(name)
  present <- tolower(needed) %in% tolower(names(slist))
  if (!all(present) && "additional" %in% tolower(names(slist))) {
    for (lack in needed[!present]) {
      i <- grep("additional", tolower(names(slist)))
      indicator <- glue("{lack}:.[^;]+;*")
      slist[[lack]] <- slist[[i]] %>%
        str_extract(indicator) %>%
        str_remove(glue(";$")) %>%
        str_remove(glue("{lack}:"))
      slist[[i]] <-  str_remove_all(slist[[i]], indicator)
      slist[[i]][slist[[i]] == ""] <- NA
    }
  }
  # Catch inspected_by == "not_inspected()"
  i <- grep("inspectedby", gsub("_", "", tolower(names(slist))))
  slist[[i]][grep("not inspected", slist[[i]])] <- NA
  
  # Catch local_pos and local_neg instead of local_positive and local_negative
  i <- grep("local_pos", gsub("_", "", tolower(names(slist))))
  if (grepl("local_pos", tolower(names(slist)))) {
    names(slist)[grep("local_pos", tolower(names(slist)))] <- "LOCAL_POSITIVE"
  }
  i <- grep("inspectedby", gsub("_", "", tolower(names(slist))))
  if (grepl("local_neg", tolower(names(slist)))) {
    names(slist)[grep("local_neg", tolower(names(slist)))] <- "LOCAL_NEGATIVE"
  }
  
  # Catch MSXL aggressive automatic date forwarding, which will damage any CAS registry number remotely formattable as a date.
  #  - e.g. CAS RN 3869-06-05 -> 719320 (value on windows) -> 6/5/3869 (displayed) coming in to R as the integer expression
  if ("casrn" %in% tolower(names(slist))) {
    i <- which(tolower(names(slist)) == "casrn")
    slist[[i]] <- slist[[i]] %>%
      repair_xl_casrn_forced_to_date() %>%
      validate_casrns()
  }
  
  names(slist)[which(tolower(names(slist)) == "id")] <- "NIST Suspect List"
  
  # Build alias list
  log_it("info", "Identifying aliases from this list...")
  alias_references <- tbl(con, "compound_alias_references")
  all_aliases <- slist %>%
    mutate(row_id = 1:nrow(slist)) %>%
    rename_with(tolower) %>%
    rename("Alias" = "name") %>%
    select(any_of(c("row_id", "Alias",
                    alias_references %>%
                      pull(name) %>%
                      tolower()))) %>%
    mutate(across(.cols = !row_id,
                  .fns = as.character),
           Alias = str_squish(Alias))
  if (any(grepl(";", all_aliases$Alias))) {
    multiple_names <- all_aliases %>%
      select(row_id, Alias) %>%
      mutate(alias_type = "Alias",
             Alias = sapply(Alias, str_split, ";")) %>%
      unnest(c(Alias)) %>%
      rename("alias" = "Alias")
  } else {
    multiple_names <- tibble(
      row_id = integer(0),
      alias_type = character(0),
      alias = character(0)
    )
  }
  all_aliases <- all_aliases %>%
    mutate(Alias = str_split(Alias, ";") %>%
             lapply(function(x) x[[1]]) %>%
             unlist()) %>%
    pivot_longer(cols = -row_id,
                 names_to = "alias_type",
                 values_to = "alias") %>%
    filter(!alias == "NA", !is.na(alias), !is.null(alias))
  all_aliases <- bind_rows(
    all_aliases,
    filter(multiple_names, !alias %in% all_aliases$alias)
  )
  new_alias_types <- distinct(all_aliases, alias_type)[[1]]
  new_alias_types <- new_alias_types[!tolower(new_alias_types) %in% (alias_references %>% pull(name) %>% tolower())]
  if (length(new_alias_types) > 0) {
    if (length(new_alias_types) == 1 && new_alias_types == "Alias") {
      add_normalization_value(db_table = "compound_alias_references",
                              name        = new_alias_types,
                              description = "Human readable compound name, typically in IUPAC or other accepted format.",
                              reference   = "NA")
    } else {
      if (interactive()) {
        add_normalization_value(db_table = "compound_alias_references", name = new_alias_types)
      } else {
        log_it("warn",
               sprintf("New alias categor%s %s can only be added in an interactive session.",
                       ifelse(length(new_alias_types) == 1, "y", "ies"),
                       format_list_of_names(new_alias_types)
               )
        )
        all_aliases <- all_aliases %>%
          filter(!alias_type %in% new_alias_types)
      }
    }
  }
  all_aliases <- all_aliases %>%
    mutate(alias_type = tolower(alias_type)) %>%
    left_join(alias_references %>%
                select(id, name) %>%
                collect() %>%
                mutate(name = tolower(name)),
              by = c("alias_type" = "name")) %>%
    mutate(alias_type = id) %>%
    select(-id)
  log_it("info",
         sprintf("\tA total of %s aliases were identified.",
                 scales::comma(nrow(all_aliases))))
  existing_aliases <- tbl(con, "compound_aliases") %>%
    collect()
  # Retain only new aliases if retaining current compounds list
  if (retain_current) {
    new_aliases <- anti_join(
      all_aliases,
      existing_aliases
    )
  }
  row_comp_id_match <- existing_aliases %>%
    full_join(all_aliases, by = c("alias_type", "alias")) %>%
    group_by(row_id)
  rows_with_existing_compound <- row_comp_id_match %>%
    summarise(is_new = all(is.na(compound_id))) %>%
    filter(!is_new, !is.na(row_id)) %>%
    pull(row_id) %>%
    unique()
  if (retain_current) {
    log_it("info", "\tRetaining current compound matches.")
    new_comps <- slist %>%
      mutate(row_id = 1:nrow(slist)) %>%
      slice(-rows_with_existing_compound)
  } else {
    log_it("warn", "\tAll compounds will be treated as new")
    new_comps <- slist %>%
      mutate(row_id = 1:nrow(slist))
  }
  if (nrow(new_aliases) > 0) {
    log_it("info",
           sprintf("\t%s aliases were not previously recorded and will be added.",
                   scales::comma(nrow(new_aliases))))
    if (length(new_aliases$alias) < 50) {
      log_it("trace", sprintf("%s", format_list_of_names(new_aliases$alias)))
    } else {
      log_it("trace", "List of new aliases is too long to print.")
    }
  } else {
    log_it("info", "\tNo new aliases were identified.")
  }
  if (nrow(new_comps) > 0) {
    log_it("info",
           sprintf("\t%s compounds were not previously recorded and will be added.",
                   scales::comma(nrow(new_comps))))
  } else {
    log_it("info", "No new compounds found.")
  }
  if (all(nrow(new_aliases) == 0, nrow(new_comps) == 0)) {
    log_it("success", "Compound list and aliases already up to date.")
    return(NULL)
  }
  
  # Shape up new compounds list
  needed <- dbListFields(db_conn, "compounds") %>%
    tolower()
  needed <- c("row_id", needed[!needed == "id"], "source")
  new_comps <- new_comps %>%
    rename_with(tolower) %>%
    select(any_of(needed))
  fill_blanks <- c("inspected_by", "inspected_on")
  names(slist)[
    which(tolower(names(slist)) %in% 
            gsub("_", "", fill_blanks))
  ] <- fill_blanks[
    which(gsub("_", "", fill_blanks) %in% 
            tolower(names(slist)))
  ]
  if (!"obtained_from" %in% names(new_comps)) {
    if("source" %in% names(new_comps)) {
      new_comps <- rename(new_comps, "obtained_from" = "source")
      needed <- needed[needed != "source"]
      names(slist)[tolower(names(slist)) == "source"] <- "obtained_from"
    } else {
      fill_blanks <- c(fill_blanks, "obtained_from")
    }
  }
  for (tmp in fill_blanks) {
    if (!tmp %in% names(new_comps)) {
      log_it("warn", glue('No values for "{tmp}"; these will be left blank.'))
      new_comps[[tmp]] = ""
    }
  }
  if (!"category" %in% names(new_comps)) {
    fil <- "PFAS"
    tmp <- tbl(src = db_conn, "compound_categories") %>% filter(name == fil) %>% pull(id)
    log_it("warn", glue('No values provided for "category"; these are assumed to be {fil} and will receive category {tmp}.'))
    new_comps$category = tmp
  }
  if (!"ion_state" %in% names(new_comps)) {
    fil <- "[M]+"
    tmp <- tbl(src = db_conn, "norm_ion_states") %>% filter(name == fil) %>% pull(id)
    log_it("warn", glue('No values provided for "ion_state"; these are assumed to be {fil} and will receive category {tmp}.'))
    new_comps$ion_state = tmp
  }
  all_present <- all(needed %in% names(new_comps))
  if (!all_present) stop(glue('Columns ({format_list_of_names(needed[!needed %in% names(new_comps)])}) are not present.'))
  new_comps <- select(new_comps, any_of(needed))
  all_source_types <- tbl(con, "norm_source_types") %>% collect()
  st_uniques <- unique(new_comps$source_type)
  if (is.character(new_comps$source_type)) {
    sanity_check <- all(st_uniques %in% all_source_types$name)
    if (!sanity_check) {
      sanity_check <- all(st_uniques %in% all_source_types$acronym)
      if (!sanity_check) {
        stop('Not all values provided for "source_types" were found in table "norm_source_types"; values must match either names or acronyms.')
      } else {
        use_col <- "acronym"
      }
    } else {
      use_col <- "name"
    }
    new_comps <- new_comps %>%
      mutate(
        source_type = new_comps %>%
          select(source_type) %>%
          left_join(all_source_types, by = c("source_type" = use_col)) %>%
          pull(id)
      )
  } else if (is.numeric(new_comps$source_type)) {
    sanity_check <- all(st_uniques %in% all_source_types$id)
    if (!sanity_check) {
      stop('Not all values provided for "source_types" match IDs in table "norm_source_types".')
    }
  } else {
    stop('Column "source_types" must be either character or numeric.')
  }
  
  sqlite_available <- Sys.which(SQLITE_CLI) != ""
  
  # Write new compounds with aliases
  if (nrow(new_comps) > 0) {
    max_compound_id <- build_db_action("nrow", "compounds")
    compound_ids <- seq(
      from = max_compound_id + 1,
      to = max_compound_id + nrow(new_comps),
      by = 1
    )
    needed <- c("row_id", dbListFields(con, "compounds"))
    new_comps <- new_comps %>%
      mutate(id = compound_ids) %>%
      select(all_of(needed))
    
    needed <- dbListFields(con, "compound_aliases")
    alias_ref <- build_db_action("get_id", "compound_alias_references", match_criteria = list(name = "Alias"))
    if (length(alias_ref) == 0) {
      alias_ref <- add_or_get_id(
        db_table = "compound_alias_references",
        values   = list(
          name        = "Alias",
          description = "Human readable compound name, typically in IUPAC or other accepted format.",
          reference   = "NA"
        )
      )
    }
    new_comps_aliases <- new_comps %>%
      select(id, name) %>%
      rename("alias" = "name",
             "compound_id" = "id") %>%
      mutate(alias_type = alias_ref) %>%
      select(all_of(needed)) %>%
      anti_join(existing_aliases)
    log_it("trace", glue("There are {scales::comma(nrow(new_comps_aliases))} new aliases for {scales::comma(nrow(new_comps))} new compounds."))
    log_it("trace", glue("Complete new alias list includes {scales::comma(nrow(new_aliases))} aliases."))
    new_aliases <- new_aliases %>%
      anti_join(new_comps_aliases,
                by = c("alias_type", "alias"))
    log_it("trace", glue("Filtered new alias list includes {scales::comma(nrow(new_aliases))} aliases."))
    
    if (sqlite_available) {
      f_name <- "temp_compounds.csv"
      log_it("trace", glue("Creating temporary compounds file at ./{f_name}"))
      if (file.exists(f_name)) file.remove(f_name)
      try_write <- try(write.csv(
        x = new_comps %>%
          select(-row_id),
        file = f_name, row.names = FALSE)
      )
      if (class(try_write) == "try-error") {
        stop(glue('There was a problem writing temporary file "{f_name}".'))
      } else {
        log_it("info", glue("There were previously {scales::comma(build_db_action('nrow', 'compounds'))} compounds."))
        sqlite_call <- glue('{SQLITE_CLI} {DB_NAME} -cmd ".import --csv --skip 1 {f_name} compounds" -cmd ".exit"')
        log_it("trace", glue('Issuing shell command {sqlite_call}'))
        if (.Platform$OS.type == "windows") {
          shell(sqlite_call)
        } else {
          system(sqlite_call)
        }
        log_it("info", glue("There are now {scales::comma(build_db_action('nrow', 'compounds'))} compounds."))
        log_it("trace", glue("Removing temporary compounds file at ./{f_name}"))
        file.remove(f_name)
      }
      f_name <- "temp_cmpd_aliases.csv"
      log_it("trace", glue("Creating temporary aliases file at ./{f_name}"))
      if (file.exists(f_name)) file.remove(f_name)
      try_write <- try(write.csv(x = new_comps_aliases, file = f_name, row.names = FALSE))
      if (class(try_write) == "try-error") {
        stop(glue('There was a problem writing temporary file "{f_name}".'))
      } else {
        log_it("info", glue("There were previously {scales::comma(build_db_action('nrow', 'compound_aliases'))} aliases."))
        sqlite_call <- glue('{SQLITE_CLI} {DB_NAME} -cmd ".import --csv --skip 1 {f_name} compound_aliases" -cmd ".exit"')
        log_it("trace", glue('Issuing shell command {sqlite_call}'))
        if (.Platform$OS.type == "windows") {
          shell(sqlite_call)
        } else {
          system(sqlite_call)
        }
        log_it("info", glue("There are now {scales::comma(build_db_action('nrow', 'compound_aliases'))} aliases."))
        log_it("trace", glue("Removing temporary aliases file at ./{f_name}"))
        file.remove(f_name)
      }
    } else {
      log_it("info", 'Beginning bulk insertion to table "compounds" through R. This will take a while.')
      res <- try(
        build_db_action(
          action = "insert",
          table_name = "compounds",
          values = new_comps %>% select(-row_id)
        )
      )
      if (class(res) == "try-error") {
        stop('There was a problem inserting new values into table "compounds".')
      } else {
        log_it("success", sprintf("%s new compounds added.", nrow(new_comps)))
      }
      log_it("info", 'Beginning bulk insertion to table "compound_aliases" through R. This will take a while.')
      res <- try(
        build_db_action(
          action = "insert",
          table_name = "compound_aliases",
          values = new_aliases
        )
      )
      if (class(res) == "try-error") {
        stop('There was a problem inserting values into table "compound_aliases".')
      } else {
        log_it("success", sprintf("Provided names for %s new compounds added as aliases.", nrow(new_comps)))
      }
    }
  }
  
  # With all compound IDs in place, update aliases for existing compounds accordingly
  log_it("trace", "Mapping between import row_id and compound_id.")
  row_comp_id_matches <- row_comp_id_match %>%
    filter(!is.na(row_id)) %>%
    left_join(new_comps %>%
                select(id, row_id)) %>%
    mutate(compound_id = ifelse(is.na(compound_id), id, compound_id)) %>%
    select(row_id, compound_id) %>%
    group_by(row_id) %>%
    fill(compound_id) %>%
    distinct()
  n_alias_prior <- nrow(new_aliases)
  log_it("trace", glue("new_aliases has {scales::comma(n_alias_prior)} entries prior to mapping."))
  new_aliases <- new_aliases %>%
    left_join(row_comp_id_matches) %>%
    select(all_of(needed))
  n_alias_post <- nrow(new_aliases)
  log_it("trace", glue("new_aliases has {scales::comma(n_alias_post)} entries after mapping."))
  if (!n_alias_prior == n_alias_post) {
    log_it("warn", "Alias list no longer has the same number of entries.")
  }
  if (nrow(new_aliases %>% filter(is.na(compound_id))) > 0) {
    new_aliases <- filter(new_aliases, is.na(compound_id))
    log_it("error", glue("Compound ids could not be rectified for import row IDs {format_list_of_names(new_aliases$row_id)}."))
  } else {
    log_it("info", "Aliases rectified.")
  }
  
  # Write compound aliases
  if (nrow(new_aliases) > 0) {
    if (sqlite_available) {
      f_name <- "temp_cmpd_aliases.csv"
      log_it("trace", glue("Creating temporary aliases file at ./{f_name}"))
      if (file.exists(f_name)) file.remove(f_name)
      try_write <- try(write.csv(x = new_aliases, file = f_name, row.names = FALSE))
      if (class(try_write) == "try-error") {
        stop(glue('There was a problem writing temporary file "{f_name}".'))
      } else {
        sqlite_call <- glue('{SQLITE_CLI} {DB_NAME} -cmd ".import --csv --skip 1 {f_name} compound_aliases" -cmd ".exit"')
        log_it("trace", glue('Issuing shell command {sqlite_call}'))
        if (.Platform$OS.type == "windows") {
          shell(sqlite_call)
        } else {
          system(sqlite_call)
        }
        file.remove(f_name)
      }
    } else {
      log_it("info", 'Beginning bulk insertion to table "compound_aliases" through R. This will take a while.')
      res <- try(
        build_db_action(
          action = "insert",
          table_name = "compound_aliases",
          values = new_aliases
        )
      )
      if (class(res) == "try-error") {
        stop('There was a problem inserting values into table "compound_aliases".')
      }
    }
  }
}


#' Repair CAS RNs forced to a date numeric by MSXL
#'
#' If a file is opened in Microsoft Excel(R), Chemical Abstract Service (CAS)
#' Registry Numbers (RNs) can occasionally be read as a pseudodate (e.g.
#' "1903-02-8"). Without tight controls over column formatting, this can result
#' in CAS RNs that are not real entering a processing pipeline. This convenience
#' function attempts to undo that automatic formatting by forcing vector members
#' whose values when coerced to numeric are equal to those provided to a
#' properly formatted date with an origin depending on operating system platform
#' (as read by `.Platform$OS.type`); Windows operating systems use the Windows
#' MSXL origin date of "1899-12-30" while others use "1904-01-01". Text entries
#' of "NA" are coerced to NA.
#'
#' @param casrn_vec CHR or NUM vector of what should be valid CAS RNs
#' @param output_format CHR scalar of the output format, which
#'
#' @return CHR vector of length equal to that of `casrn_vec` where numeric
#'   entries have been coerced to the assumed date
#'
#' @export
#'
#' @examples
#' repair_xl_casrn_forced_to_date(c("64324-08-3", "12332"))
repair_xl_casrn_forced_to_date <- function(casrn_vec, output_format = "%Y-%m-%d") {
  stopifnot(length(output_format) == 1)
  casrn_vec[casrn_vec == "NA"] <- NA
  unformat  <- which(!is.na(casrn_vec) & suppressWarnings(as.numeric(casrn_vec)) == casrn_vec)
  casrn_vec[unformat] <- format(
    as.Date(
      x = as.integer(casrn_vec[unformat]),
      origin = ifelse(.Platform$OS.type == "windows", "1899-12-30", "1904-01-01")
    ),
    output_format
  )
  casrn_vec[unformat] <- gsub("-0([0-9])$", "-\\1", casrn_vec[unformat])
  return(casrn_vec)
}

#' Validate a CAS RN
#'
#' Chemical Abstract Service (CAS) Registry Numbers (RNs) follow a standard
#' creation format. From
#' [https://www.cas.org/support/documentation/chemical-substances/faqs], a CAS
#' RN is a "numeric identifier that can contain up to 10 digits, divided by
#' hyphens into three parts. The right-most digit is a check digit used to
#' verify the validity and uniqueness of the entire number. For example, 58-08-2
#' is the CAS Registry Number for caffeine."
#'
#' Provided CAS RNs in `casrn_vec` are validated for format and their checksum
#' digit. Those failing will be printed to the console by default, and users
#' have the option of stripping unverified entries from the return vector.
#'
#' This only validates that a CAS RN is properly constructed; it does not
#' indicate that the registry number exists in the CAS Registry.
#' 
#' See [repair_xl_casrn_forced_to_date] as one possible pre-processing step.
#'
#' @param casrn_vec CHR vector of what CAS RNs to validate
#' @param strip_bad_cas LGL scalar of whether to strip out invalid CAS RNs
#'   (default: TRUE)
#'
#' @return CHR vector of length equal to that of `casrn_vec`
#' @export
#'
#' @examples
#' validate_casrns(c("64324-08-9", "64324-08-5", "12332"))
#' validate_casrns(c("64324-08-9", "64324-08-5", "12332"), strip_bad_cas = FALSE)
validate_casrns <- function(casrn_vec, strip_bad_cas = TRUE) {
  parts <- casrn_vec
  parts[parts == "NA"] <- NA
  na_part <- parts == "NA" | is.na(parts)
  parseable <- parts %>%
    str_split("-") %>%
    map(~ length(.x) == 3) %>%
    unlist()
  parts[parseable] <- parts[parseable] %>%
    str_pad(width = 13, pad = "0", side = "left")
  if (any(!parseable)) {
    bad_cas <- parts[!na_part & !parseable]
    log_it("warn", 'Not all entries were parseable to the CAS RN format.')
    log_it("info", sprintf("\tApplies to entr%s %s",
                           ifelse(length(bad_cas) > 1, "ies", "y"),
                           format_list_of_names(bad_cas)))
  }
  valid_casrn <- lapply(parts,
                        function(x) {
                          if (any(!parseable[which(parts == x)], is.na(x))) return(FALSE)
                          x <- x %>% str_split("-") %>% purrr::flatten_chr()
                          y <- str_split_fixed(c(paste0(x[1], x[2])),
                                               pattern = "",
                                               n = 10) %>%
                            as.integer()
                          z <- y * 10:1
                          total <- sum(z)
                          check_sum <- total %% 10
                          check_val <- as.integer(x[[3]])
                          return(check_sum == check_val)
                        }) %>%
    unlist()
  parts <- parts %>%
    str_remove_all("^0*")
  if (!all(valid_casrn[!is.na(valid_casrn)])) {
    log_it("warn", 'Not all CAS RNs were valid.')
    bad_cas <- parts[parseable & !valid_casrn & !na_part]
    log_it("info", sprintf("\tApplies to entr%s %s",
                           ifelse(length(bad_cas) > 1, "ies", "y"),
                           format_list_of_names(bad_cas)))
  }
  if (strip_bad_cas) {
    parts[!valid_casrn] <- NA
  }
  return(unlist(parts))
}

#' Dump current database contents
#'
#' Perform one or both of two main tasks for backing up the NTA database.
#'
#' The main task is to update CSV files in the config/data directory with the
#' current contents of the database. This is done on a table by table basis and
#' results in flat files whose structures no longer interrelate except
#' numerically. Primarily this would be used to migrate database contents to
#' other systems or for further manipulation. Please specify a `project` that
#' project-specific information can be maintained.
#'
#' Backups created with this function are placed in a "backups" subdirectory of
#' the directory defined by parameter `data_dir`. If `dump_sql = TRUE` SQL dump
#' files will be written to "backups/sqlite" with file names equal to the
#' current database name prefixed by date.
#'
#' @param project CHR scalar of the directory containing project specific data
#'   (required, no default)
#' @param data_dir CHR scalar of the directory containing project independent
#'   data sources used for population (default: `file.path("config", "data")`)
#' @param create_backups LGL scalar indicating whether to create backups prior
#'   to writing updated data files (default: TRUE)
#' @param dump_tables LGL scalar indicating whether to dump contents of database
#'   tables as comma-separated-value files (default: TRUE)
#' @param dump_sql LGL scalar indicating whether to create an SQL dump file
#'   containing both schema and data as a backup (default: TRUE)
#' @param db_conn connection object (default: con)
#' @param SQLITE_CLI CHR scalar system reference to your installation of the
#'   sqlite command line interface
#'
#' @return None, copies database information to the local file system
#'
update_data_sources <- function(project,
                                data_dir = file.path("config", "data"),
                                create_backups = TRUE,
                                dump_tables = TRUE,
                                dump_sql = TRUE,
                                db_conn = con,
                                sqlite_cli = ifelse(exists("SQLITE_CLI"), SQLITE_CLI, NULL),
                                db_name = ifelse(exists("DB_NAME"), DB_NAME, NULL) 
) {
  if (all(!create_backups, !dump_tables, !dump_sql)) {
    stop("Arguments 'create_backups', 'dump_tables', and 'dump_sql' were all set to FALSE. Choose at least one action to execute.")
  }
  if (!is.null(sqlite_cli)) {
    if (Sys.which(sqlite_cli) == "") {
      stop(glue('Could not identify the SQLITE CLI under "{sqlite_cli}" on this system; please check your PATH.'))
    }
  } else if (dump_sql) {
    stop("SQL .dump backups are only available if an SQLITE CLI is provided.")
  }
  # Argument validation relies on verify_args
  log_it("trace", sprintf('Updating data sources from %s', data_dir))
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        project        = list(c("mode", "character"), c("length", 1)),
        data_dir       = list(c("mode", "character"), c("length", 1)),
        create_backups = list(c("mode", "logical"), c("length", 1)),
        dump_tables    = list(c("mode", "logical"), c("length", 1)),
        dump_sql       = list(c("mode", "logical"), c("length", 1)),
        db_conn        = list(c("length", 1)),
        sqlite_cli     = list(c("mode", "character"), c("length", 1)),
        db_name        = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "update_data_sources"
    )
    stopifnot(arg_check$valid)
  }
  backup_dirname <- "backups"
  this_date <- format(Sys.time(), '%Y%m%d%H%M')
  if (dump_sql) {
    db_file <- list.files(pattern = glue("{DB_NAME}$"), full.names = TRUE, recursive = TRUE)
    db_file <- db_file[!str_detect(db_file, backup_dirname)]
    if (length(db_file) > 1) {
      log_it("error", glue("Multiple files matching '{db_name}' were identified. SQLite dump operation aborted."))
    } else {
      f_name <- glue("{this_date}_{file_path_sans_ext(basename(db_file))}.sql")
      f_name <- str_replace(
        db_file,
        basename(db_file),
        glue("{data_dir}", "{backup_dirname}", "sqlite", "{f_name}",
             .sep = .Platform$file.sep))
      if (!dir.exists(dirname(f_name))) {
        dir.create(dirname(f_name), recursive = TRUE)
      }
      log_it("info", "Dumping sqlite file from '{db_file}' to '{f_name}' via command line.")
      sql_cmd <- glue("{sqlite_cli} {db_name}", '".output {f_name}"', '".dump"', '".exit"',
                      .sep = " -cmd ")
      log_it("trace", glue("Issuing shell command\n{sql_cmd}"))
      if (.Platform$OS.type == "windows") {
        shell(sql_cmd)
      } else {
        system(sql_cmd)
      }
    }
  }
  project_dir <- file.path(data_dir, project)
  if (!dir.exists(data_dir)) stop(glue("Directory '{data_dir}' does not exist."))
  if (!dir.exists(data_dir)) stop(glue("Project directory '{project_dir}' does not exist."))
  
  data_files  <- c(
    list.files(path = data_dir,
               pattern = ".csv",
               full.names = TRUE,
               include.dirs = FALSE),
    list.files(path = project_dir,
               pattern = ".csv",
               full.names = TRUE,
               include.dirs = FALSE)
  )
  
  if (create_backups){
    backup_dir  <- file.path(data_dir, backup_dirname, "data", this_date)
    for (this_dir in c(backup_dir, file.path(backup_dir, project))) {
      if (!dir.exists(this_dir)) {
        log_it("info", glue("Creating backup directory '{this_dir}'."))
        res <- try(dir.create(this_dir, recursive = TRUE))
        if (class(res) == "try-error") stop(glue("Could not create directory '{this_dir}'."))
      }
    }
    backup_files <- str_replace_all(data_files, data_dir, backup_dir)
    log_it("info", glue("Backing up data files to '{backup_dir}'."))
    for (data_file in data_files) {
      backup_file <- backup_files[which(data_files == data_file)]
      log_it("trace", glue("Backing up '{data_file}' to '{backup_file}'."))
      file.rename(data_file, backup_file)
    }
  }
  
  if (dump_tables) {
    # Should really do a better job here of splitting out which tables belong to
    # project files and which are universal. Maybe store that itself as a table
    # in the database?
    db_tables <- dbGetQuery(con, "select name from sqlite_master where type = 'table'") %>%
      filter(!name == "sqlite_sequence")
    log_it("info", glue("Cloning data files to '{data_dir}'."))
    for (db_table in db_tables$name) {
      data_file <- data_files[file_path_sans_ext(basename(data_files)) == db_table]
      if (length(data_file) > 1) {
        log_it("warn", glue("Multiple files identified for {db_table}: {format_list_of_names(data_file)}"))
      } else if (length(data_file) == 1) {
        if (file.exists(data_file)) file.remove(data_file)
      } else if (length(data_file) == 0) {
        data_file <- file.path(data_dir, db_table)
      }
      log_it("info", glue("Cloning table '{db_table}' to '{data_file}'."))
      tbl(db_conn, db_table) %>%
        collect() %>%
        write_csv(data_file)
    }
  }
}
