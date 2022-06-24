#* @apiTitle API Guide: NIST HRMS Database
#* @apiDescription This plumber API requires a current copy of the NIST high-resolution accurate-mass spectrometry database.
#* @apiVersion 0.1
#* @apiLicense NIST-developed software is provided by NIST as a public service. You may use, copy and distribute copies of the software in any medium, provided that you keep intact this entire notice. You may improve, modify and create derivative works of the software or any portion of the software, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the software and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the software. NIST-developed software is expressly provided "AS IS." NIST MAKES NO WARRANTY OF ANY KIND, EXPRESS, IMPLIED, IN FACT OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF, INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR USEFULNESS OF THE SOFTWARE. You are solely responsible for determining the appropriateness of using and distributing the software and you assume all risks associated with its use, including but not limited to the risks and costs of program errors, compliance with applicable laws, damage to or loss of data, programs or equipment, and the unavailability or interruption of operation. This software is not intended to be used in any situation where a failure could cause risk of injury or damage to property. The software developed by NIST employees is not subject to copyright protection within the United States. This software was developed at the National Institute of Standards and Technology by employees of the Federal Government in the course of their official duties. Pursuant to title 17 Section 105 of the United States Code this software is not subject to copyright protection and is in the public domain. It is an experimental system. NIST assumes no responsibility whatsoever for its use by other parties, and makes no guarantees, expressed or implied, about its quality, reliability, or any other characteristic. We would appreciate acknowledgement if the software is used. This software can be redistributed and/or modified freely provided that any derivative works bear some notice that they are derived from it, and any modified versions bear some notice that they have been modified.

#* Log information about each request
#* @filter logger
function(req) {
  if (!stringr::str_detect(req$PATH_INFO, "__docs__|openapi.json|favicon")) {
    if (exists("LOGGING_ON") && LOGGING_ON && exists("LOGGING") && "API" %in% names(LOGGING) && exists("log_it")) {
      log_it("info", glue::glue("{req$REQUEST_METHOD}{req$PATH_INFO} - {req$HTTP_USER_AGENT} @ {req$REMOTE_ADDR}"), "api")
    }
  }
  plumber::forward()
}

#* Ensure database is connected prior to issuing requests
#* @filter active
function(req, res) {
  if (!DBI::dbIsValid(con)) {
    res$status <- 401
    return(list(error="Database connection is not currently available."))
  }
  plumber::forward()
}

#* Return data from tables or views as JSON expressions.
#* @param table_name:character The name of a single table to which this query applies.
#* @param column_names:character A comma-separated list of column names to include (leave blank to include all columns)
#* @param match_criteria:character An R LIST expression of matching criteria to be passed to {clause_where} with names matching columns against which to apply. In the simplest case, a direct value is given to the name (e.g. `list(last_name = "Smith")`) for single matches. All match criteria must be their own list item. Values can also be provided as a nested list for more complicated WHERE clauses with names `values`, `exclude`, and `like` that will be recognized. `values` should be the actual search criteria, and if a vector of length greater than one is specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar) determines whether to apply the NOT operator. `like` (LGL scalar) determines whether this is an equality, list, or similarity. To reverse the example above by issuing a NOT statement, use `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name = list(values = "Smith", exclude = FALSE, like = TRUE))` (default: "" goes to NULL)
#* @param match_crit_json:logical A single logical value indicating whether `match_criteria` are provided in JSON format.
#* @param case_sensitive:logical A single logical value indicating whether to match on a case sensitive basis (the default TRUE searches for values as-provided) or whether to search for value matches by upper, lower, sentence, and title case matches; passed directly to [clause_where] (default: TRUE)
#* @param and_or:character One of "AND" or "OR" to be applied to the match criteria (default "OR")
#* @param limit:int A single integer value of the maximum number of rows to return  (default 15)
#* @param distinct:logical A single logical value indicating whether or not to apply the DISTINCT clause to all match criteria (default FALSE)
#* @param get_all_columns:logical A single logical value indicating whether to force return all columns rather than those identified in column_names; will be set to TRUE automatically if no column names are provided (default FALSE)
#* @param execute:logical A single logical value indicating whether or not to immediately execute the build query statement (default TRUE, FALSE will instead return the SQL statement to be executed)
#* @param single_column_as_vector:logical A single logical value indicating whether to collapse results to a vector if they consist of only a single column (default TRUE)
#* @get /table_search
function(table_name      = "contributors",
         column_names    = "",
         match_criteria  = "",
         match_crit_json = FALSE,
         case_sensitive  = TRUE,
         and_or          = "AND",
         distinct        = FALSE,
         limit           = 15L,
         get_all_columns = FALSE,
         execute         = TRUE,
         single_column_as_vector = TRUE) {
  action <- "select"
  params <- as.list(environment())
  params$match_crit_json <- NULL
  need_logical <- c("case_sensitive", "distinct", "get_all_columns", "execute", "single_column_as_vector")
  for (nl in need_logical) {
    params[[nl]] <- as.logical(params[[nl]])
  }
  if (!match_criteria == "") {
    if (match_crit_json) {
      match_criteria <- jsonlite::fromJSON(match_criteria)
    } else {
      match_criteria <- utils::URLdecode(match_criteria)
      if (!stringr::str_detect(match_criteria, "^list\\(")) {
        match_criteria <- glue::glue("list({match_criteria})")
      }
      match_eval <- try(lazyeval::lazy_eval(match_criteria))
      if (inherits(match_eval, "try-error")) {
        msg <- glue::glue("Cannot evaluate match_criteria as '{match_criteria}'. Did you forget to quote strings or close parentheses?")
        return(msg)
      } else {
        match_criteria <- match_eval
      }
    }
    params$match_criteria <- match_criteria
  }
  params <- params %>%
    lapply(
      function(x) {
        if (any(x == "", length(x) == 0)) x <- NULL else x
      }) %>%
    lapply(
      function(x) {
        if (is.character(x)) {
          x %>%
            str_split(",") %>%
            lapply(str_trim) %>%
            unlist()
        } else {
          x
        }
      })
  res <- try(do.call("build_db_action", params))
  if (inherits(res, "try-error")) {
    msg <- paste0(names(params), ' = ' , unlist(unname(params)), collapse = ',')
    res <- glue::glue("Malformed call to build_db_action with params {msg}.")
  }
  return(res)
}

#* Return mass spectral data for a compound by its internal ID number.
#* @param compound_id:int A single integer value of the peak ID for which to retrieve mass spectral data.
#* @param tidy_spectra:logical Whether spectra should be made "tidy" or remain packed.
#* @param test:logical
#* @get /compound_data
function(compound_id = 1L,
         tidy_spectra = TRUE,
         test = TRUE) {
  if (test) return(TRUE)
  tidy_spectra <- as.logical(tidy_spectra)
  compound_id <- as.integer(compound_id)
  ms_data <- dbGetQuery(
    con,
    DBI::sqlInterpolate(con,
                        "select distinct cf.compound_id, cf.peak_id, pd.precursor_mz, pd.base_int, pd.scantime, pd.ms_n, pd.mz, pd.intensity from compound_fragments cf left join peak_data pd on cf.peak_id = pd.peak_id where cf.compound_id = ?compound_id and not cf.peak_id is null",
                        compound_id = compound_id)
  )
  if (tidy_spectra && nrow(ms_data) > 0) {
    ms_data <- tidy_spectra(ms_data)
  }
  return(ms_data)
}

#* Return mass spectral data for a peak by its internal ID number.
#* @param peak_id:int A single integer value of the peak ID for which to retrieve mass spectral data.
#* @param tidy_spectra:logical Whether spectra should be made "tidy" or remain packed.
#* @get /peak_data
function(peak_id = 1L,
         tidy_spectra = TRUE) {
  tidy_spectra <- as.logical(tidy_spectra)
  peak_id <- as.integer(peak_id)
  ms_data <- dbGetQuery(
    con,
    DBI::sqlInterpolate(con,
                        "select * from peak_data where peak_id = ?peak_id",
                        peak_id = peak_id)
  )
  if (tidy_spectra && nrow(ms_data) > 0) {
    ms_data <- tidy_spectra(ms_data)
  }
  return(ms_data)
}

#* Get the list of tables in the database.
#* @get /list_tables
function() {
  out <- dbListTables(con)
  out <- out[-grep("^view_|sqlite_sequence", out)]
  return(out)
}

#* Get the list of stored views in the database.
#* @get /list_views
function() {
  out <- dbListTables(con)
  out <- out[grep("^view_", out)]
  return(out)
}

#* Plumber ping, a health check endpoint to give outside observers an indication of whether or not this API is running.
#* @get /_ping
function(req, res) {
  res$setHeader("Content-Type", "application/json")
  res$status <- 200L
  res$body <- "API is live"
  return(res)
}

#* Plumber version, a health check endpoint showing which version of the API is currently running.
#* @get /version
function() {
  if (exists("PLUMBER_VERSION")) {
    return(PLUMBER_VERSION)
  } else {
    "No version recorded."
  }
}

#* Project support information, a health check endpoint. This has less information than running support_info() directly in R but should suffice for most troubleshooting and checking.
#* @get /support_info
function() {
  out <- support_info()
  to_replace <- c("otherPkgs", "loadedOnly")
  replacements <- lapply(to_replace,
                         function(x) {
                           out[["system"]][[x]] <- lapply(out[["system"]][[x]],
                                                          function(y) {
                                                            sprintf("%s_%s", y[["Package"]], y[["Version"]])
                                                          }) %>%
                             unlist() %>%
                             unname()
                         }) %>%
    setNames(to_replace)
  out$system[to_replace] <- replacements
  class(out$system) <- NULL
  return(out)
}

#* Is the connection valid
#* @get /db_active
function() return(dbIsValid(con))

#* Is RDKit available?
#* @get /rdkit_active
function() return(rdkit_active())


#* Search for matches in a mass spectrum
#* @param search_on:character The type of search to run, either "precursor" or "all". A precursor search limits your search to only the chosen precursor, while searching the entire database may give more results, but will be much slower.
#* @param search_ms:character String or JSON expression of the mass spectrum to be searched.
#* @param norm_fn:character The normalization function to use as part of the search, one of "sum" or "mean".
#* @param cor_method:character The correlation function to use as part of the search; the default is "pearson".
#* @get /search_ms
function(search_on = "precursor",
         search_ms = "",
         norm_fn = "sum",
         cor_method = "pearson") {
  search_on <- match.arg(search_on)
  norm_fn <- match.arg(norm_fn)
  search_fn <- switch(
    search_on,
    "precursor" = search_precursor,
    "all" = search_all
  )
  out <- search_fn(con, search_ms, norm_fn, cor_method)
  return(out)
}

#* Returns a file path to a molecular ball-and-stick plot of a compound or fragment in portable-network-graphics (png) format. Requires RDKit integration. If notation is provided, it must 
#* @param type:character The type of notation provided, whether it should be treated as direct notation, a compound id, or a fragment id. One of "notation", "compound", or "fragment"
#* @param molecular_notation:character Character string of the machine readable notation to visualize.
#* @param compound_id:int An integer value indicating the compound id (required if type is "compound").
#* @param fragment_id:int An integer value indicating the fragment id (required if type is "fragment").
#* @param notation_type:character The type of notation to use to generate the visualization. One of "smiles" or "InChI"
#* @param as_graphic:logical Whether to return a graphic (default) or a file path reference.
#* @get /molecular_model
function(type = "notation",
         molecular_notation = "C[n]1cnc2N(C)C(=O)N(C)C(=O)c12",
         compound_id = 1L,
         fragment_id = 1L,
         notation_type = "smiles",
         as_graphic = TRUE) {
  stopifnot(rdkit_active())
  
}

#* Returns a file path to a molecular ball-and-stick plot of a compound or fragment in portable-network-graphics (png) format. Requires RDKit integration. If notation is provided, it must 
#* @param type:character The type of notation provided, whether it should be treated as direct notation, a compound id, or a fragment id. One of "notation", "compound", or "fragment"
#* @param molecular_notation:character Character string of the machine readable notation to visualize.
#* @param compound_id:int An integer value indicating the compound id (required if type is "compound").
#* @param fragment_id:int An integer value indicating the fragment id (required if type is "fragment").
#* @param notation_type:character The type of notation to use to generate the visualization. One of "smiles" or "InChI"
#* @get /molecular_model/file
function(type = "notation",
         molecular_notation = "C[n]1cnc2N(C)C(=O)N(C)C(=O)c12",
         compound_id = 1L,
         fragment_id = 1L,
         notation_type = "smiles") {
  notation_type <- tolower(notation_type)
  type <- match.arg(arg = tolower(type), choices = c("notation", "compound", "fragment"))
  notation_type <- match.arg(arg = notation_type, choices = c("smiles", "inchi"))
  compound_id <- as.integer(compound_id)
  fragment_id <- as.integer(fragment_id)
  id_present <- switch(
    type,
    "notation" = TRUE,
    "compound" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select count(id) from compounds where id = ?compound_id",
        compound_id = compound_id
      )
    ) %>% as.logical(),
    "fragment" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select count(id) from norm_fragments where id = ?fragment_id",
        fragment_id = fragment_id
      )
    ) %>% as.logical()
  )
  if (!id_present) return(glue::glue("The {type} ID {switch(type, 'compound' = compound_id, 'fragment' = fragment_id)} does not exist."))
  molecular_notation <- switch(
    type,
    "notation" = molecular_notation,
    "compound" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select distinct alias from view_compound_aliases where compound_id = ?compound_id and alias_type = ?alias_type",
        compound_id = compound_id,
        alias_type = toupper(notation_type)))$alias,
    "fragment" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select distinct alias from view_fragment_aliases where fragment_id = ?fragment_id and alias_type = ?alias_type",
        fragment_id = fragment_id,
        alias_type = toupper(notation_type)))$alias
  )
  if (notation_type == "inchi" && !str_detect(molecular_notation, "^InChI=")) {
    molecular_notation <- paste0("InChI=", molcular_notation, collapse = "")
    if (!str_detect(molecular_notation, "^InChI=1S/")) {
      molcular_notation <- str_replace(molecular_notation, "^InChI=", "InChI=1S/")
    }
  }
  out <- try(molecule_picture(mol = molecular_notation,
                              mol_type = notation_type,
                              rdkit_name = "rdk",
                              file_name = NULL,
                              open_file = FALSE,
                              show = FALSE)
  )
  if (inherits(out, 'try-error')) {
    return(glue::glue("Could not process {notation} as {notation_type}."))
  } else {
    return(out$file)
  }
}

#* Returns a molecular ball-and-stick graphic for a compound or fragment in portable-network-graphics (png) format. Requires RDKit integration. If notation is provided, it must 
#* @param type:character The type of notation provided, whether it should be treated as direct notation, a compound id, or a fragment id. One of "notation", "compound", or "fragment"
#* @param molecular_notation:character Character string of the machine readable notation to visualize.
#* @param compound_id:int An integer value indicating the compound id (required if type is "compound").
#* @param fragment_id:int An integer value indicating the fragment id (required if type is "fragment").
#* @param notation_type:character The type of notation to use to generate the visualization. One of "smiles" or "InChI"
#* @serializer png
#* @get /molecular_model/png
function(type = "notation",
         molecular_notation = "C[n]1cnc2N(C)C(=O)N(C)C(=O)c12",
         compound_id = 1L,
         fragment_id = 1L,
         notation_type = "smiles") {
  notation_type <- tolower(notation_type)
  type <- match.arg(arg = tolower(type), choices = c("notation", "compound", "fragment"))
  notation_type <- match.arg(arg = notation_type, choices = c("smiles", "inchi"))
  compound_id <- as.integer(compound_id)
  fragment_id <- as.integer(fragment_id)
  id_present <- switch(
    type,
    "notation" = TRUE,
    "compound" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select count(id) from compounds where id = ?compound_id",
        compound_id = compound_id
      )
    ) %>% as.logical(),
    "fragment" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select count(id) from fragments where id = ?fragment_id",
        fragment_id = fragment_id
      )
    ) %>% as.logical()
  )
  if (!id_present) return(glue::glue("The {type} ID {switch(type, 'compound' = compound_iddoes not exist."))
  molecular_notation <- switch(
    type,
    "notation" = molecular_notation,
    "compound" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select distinct alias from view_compound_aliases where compound_id = ?compound_id and alias_type = ?alias_type",
        compound_id = compound_id,
        alias_type = toupper(notation_type)))$alias,
    "fragment" = DBI::dbGetQuery(
      con,
      DBI::sqlInterpolate(
        con,
        "select distinct alias from view_fragment_aliases where fragment_id = ?compound_id and alias_type = ?alias_type",
        compound_id = compound_id,
        alias_type = toupper(notation_type)))$alias
  )
  if (notation_type == "inchi" && !str_detect(molecular_notation, "^InChI=")) {
    molecular_notation <- paste0("InChI=", molcular_notation, collapse = "")
    if (!str_detect(molecular_notation, "^InChI=1S/")) {
      molcular_notation <- str_replace(molecular_notation, "^InChI=", "InChI=1S/")
    }
  }
  out <- try(molecule_picture(mol = molecular_notation,
                              mol_type = notation_type,
                              rdkit_name = "rdk",
                              file_name = NULL,
                              open_file = FALSE,
                              show = TRUE)
  )
  if (inherits(out, 'try-error')) {
    return(glue::glue("Could not process {notation} as {notation_type}."))
  } else {
    return(grid::grid.raster(out))
  }
}

#* Search a mass spectrum in json format. This does no preprocessing of the `search_ms` item and only executes the defined search on the database.
#* @param type:character The type of search to perform, one of either "precursor" to search depending on the defined precursor ion, or "all" to search all possible matches regardless of precursor ion.
#* @param search_ms:character Search object generated by other calls, primarily those created by the R function `create_search_ms` which relies on other paths and should be processed outside this server.
#* @param correlation_max:numeric
#* @param correlation_bin:numeric
#* @param peak_height_max:numeric
#* @param peak_height_bin:numeric 
#* @param frequency_max:numeric 
#* @param frequency_bin:numeric 
#* @param min_n_peaks:numeric Minimum number of observations required (3 - 15, set to 15 to unrestrict) 
#* @get /search_compound/<type>
function(type, search_ms, correlation_max = 0.5, correlation_bin = 0.1, peak_height_max = 10, peak_height_bin = 1, frequency_max = 10, frequency_bin = 1, min_n_peaks = 4, res) {
  search_ms <- try(jsonlite::fromJSON(search_ms))
  if (inherits(search_ms, "try-error")) {
    res$status <- 500
    res$body <- "Object provided to argument search_ms could not be converted from JSON to an R list."
    return(res)
  }
  type <- match.arg(type, c("precursor", "all"))
  search_fn <- switch(type,
                      "precursor" = search_precursor,
                      "all" = search_all)
  out <- search_fn(con = con,
                   searchms = search_ms,
                   max_correl = correlation_max,
                   correl_bin = correlation_bin,
                   max_ph = peak_height_max,
                   ph_bin = peak_height_bin,
                   max_freq = frequency_max,
                   freq_bin = frequency_bin,
                   min_n_peaks = min_n_peaks
  )
  return(out)
}