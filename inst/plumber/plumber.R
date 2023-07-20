#* @apiTitle Interactive API Guide for DIMSpec
#* @apiDescription This plumber API provides programmatic access to an SQLite database conforming to the <a href="https://github.com/usnistgov/dimspec" target="_blank">DIMSpec project</a>. See the <a href="https://pages.nist.gov/dimspec/docs" target="_blank">User Guide</a> describing the project, database, and this API for details. Endpoint definitions may be modified as needed to suit your project; this version supports generic endpoints and those necessary to run the web applications included with the full project. This version was created using <a href="https://www.rplumber.io" target="_blank">plumber</a> v1.1.0 and was last updated 2023-07-13.
#* @apiVersion 1.0.1-202307
#* @apiLicense NIST-developed software is provided by NIST as a public service. You may use, copy and distribute copies of the software in any medium, provided that you keep intact this entire notice. You may improve, modify and create derivative works of the software or any portion of the software, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the software and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the software. NIST-developed software is expressly provided "AS IS." NIST MAKES NO WARRANTY OF ANY KIND, EXPRESS, IMPLIED, IN FACT OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF, INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR USEFULNESS OF THE SOFTWARE. You are solely responsible for determining the appropriateness of using and distributing the software and you assume all risks associated with its use, including but not limited to the risks and costs of program errors, compliance with applicable laws, damage to or loss of data, programs or equipment, and the unavailability or interruption of operation. This software is not intended to be used in any situation where a failure could cause risk of injury or damage to property. The software developed by NIST employees is not subject to copyright protection within the United States. This software was developed at the National Institute of Standards and Technology by employees of the Federal Government in the course of their official duties. Pursuant to title 17 Section 105 of the United States Code this software is not subject to copyright protection and is in the public domain. It is an experimental system. NIST assumes no responsibility whatsoever for its use by other parties, and makes no guarantees, expressed or implied, about its quality, reliability, or any other characteristic. We would appreciate acknowledgement if the software is used. This software can be redistributed and/or modified freely provided that any derivative works bear some notice that they are derived from it, and any modified versions bear some notice that they have been modified.

# Filters ----
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
#* @param db_conn:character The name of the database connection object
#* @filter active
function(db_conn = "con", req, res) {
  if (!exists("testing_api") || !testing_api) {
    if (!exists(db_conn)) {
      res$status <- 401
      return(list(error = sprintf("No database connection object named %s is currently available to this plumber instance.", db_conn)))
    } else {
      if (!DBI::dbIsValid(eval(sym(db_conn)))) {
        res$status <- 401
        return(list(error = sprintf("The database connection at %s is not active in this plumber instance.", db_conn)))
      }
    }
  }
  plumber::forward()
}

# Endpoints ----
# /table_search ----
#* Return data from tables or views as JSON expressions.
#* @param table_name:character The name of a single table to which this query applies.
#* @param column_names:character A comma-separated list of column names to include (leave blank to include all columns)
#* @param match_criteria:character An R LIST expression of matching criteria to be passed to {clause_where} with names matching columns against which to apply. In the simplest case, a direct value is given to the name (e.g. `list(last_name = "Smith")`) for single matches. All match criteria must be their own list item. Values can also be provided as a nested list for more complicated WHERE clauses with names `values`, `exclude`, and `like` that will be recognized. `values` should be the actual search criteria, and if a vector of length greater than one is specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar) determines whether to apply the NOT operator. `like` (LGL scalar) determines whether this is an equality, list, or similarity. To reverse the example above by issuing a NOT statement, use `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name = list(values = "Smith", exclude = FALSE, like = TRUE))` (default: "" goes to NULL)
#* @param match_crit_json:logical A single logical value indicating whether `match_criteria` are provided in JSON format.
#* @param case_sensitive:logical A single logical value indicating whether to match on a case sensitive basis (the default TRUE searches for values as-provided) or whether to search for value matches by upper, lower, sentence, and title case matches; passed directly to [clause_where] (default: TRUE)
#* @param and_or:character One of "AND" or "OR" to be applied to the match criteria (default "OR")
#* @param limit:int A single integer value of the maximum number of rows to return  (default NULL)
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
         limit           = NULL,
         get_all_columns = FALSE,
         execute         = TRUE,
         single_column_as_vector = TRUE) {
  action <- "select"
  params <- as.list(environment())
  params$match_crit_json <- NULL
  need_logical <- c("case_sensitive", "distinct", "get_all_columns", "execute", "single_column_as_vector")
  if (!exists("build_db_action")) {
    return(list(error = "Function build_db_action is not available."))
  }
  if (!table_name %in% dbListTables(con)) {
    return(list(error = sprintf("Table %s is not present in this database. Check the spelling and try again.", table_name)))
  }
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
    res$status <- 500
    return(list(error = glue::glue("Malformed call to build_db_action with params {msg}.")))
  }
  return(res)
}

# /compound_data ----
#* Return mass spectral data for a compound by its internal ID number. This endpoint operates from the compound_data view.
#* @param compound_id:int A single integer value of the peak ID for which to retrieve mass spectral data.
#* @param tidy_spectra:logical Whether spectra should be made "tidy" or remain packed.
#* @get /compound_data
function(compound_id = 2627L,
         tidy_spectra = TRUE,
         format = "separated",
         column_flags = c("measured_mz", "measured_intensity")) {
  tidy_spectra <- as.logical(tidy_spectra)
  compound_id <- as.integer(compound_id)
  ms_data <- dbGetQuery(
    con,
    DBI::sqlInterpolate(con,
                        "select * from compound_data where compound_id = ?compound_id",
                        compound_id = compound_id)
  )
  if (tidy_spectra && nrow(ms_data) > 0) {
    ms_data <- tidy_spectra(target = ms_data, is_format = "separated", ms_col_sep = c("measured_mz", "measured_intensity"))
  }
  return(ms_data)
}

# /peak_data ----
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
    ms_data <- tidy_spectra(target = ms_data, is_format = "separated", ms_col_sep = c("measured_mz", "measured_intensity"))
  }
  return(ms_data)
}

# /list_tables ----
#* Get the list of tables in the database.
#* @get /list_tables
function() {
  out <- dbListTables(con)
  out <- out[-grep("^view_|sqlite_sequence", out)]
  return(out)
}

# /list_views  ----
#* Get the list of stored views in the database.
#* @get /list_views
function() {
  out <- dbListTables(con)
  out <- out[grep("^view_", out)]
  return(out)
}

# /_ping ----
#* Plumber ping, a health check endpoint to give outside observers an indication of whether or not this API is running.
#* @get /_ping
#* @serializer unboxedJSON
function(req, res) {
  list(status = "OK")
}

# /version ----
#* Plumber version, a health check endpoint showing which version of the API is currently running.
#* @get /version
#* @serializer unboxedJSON
function() {
  if (exists("PLUMBER_VERSION")) {
    return(PLUMBER_VERSION)
  } else {
    "No version recorded."
  }
}

# /support_info ----
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

# /db_active ----
#* Is the connection valid
#* @param db_conn:character The name of the database connection object (default: "con")
#* @get /db_active
function(db_conn = "con") {
  if (exists("con")) {
    return(dbIsValid(con))
  } else {
    return(sprintf("No connection object named %s is available.", db_conn))
  }
}

# /rdkit_active ----
#* Is RDKit available?
#* @get /rdkit_active
function() return(exists("rdkit_active") && rdkit_active())

# /molecular_model/file ----
#* Returns a file path to a molecular ball-and-stick plot of a compound or fragment in portable-network-graphics (png) format. Requires RDKit integration. If notation is provided, it must match the "notation_type" provided.
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
  if (!exists("RENV_ESTABLISHED_RDKIT") || !RENV_ESTABLISHED_RDKIT || !rdkit_active()) {
    return("RDKit is not available.")
  }
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

# /molecular_model/png ----
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
  if (!exists("RENV_ESTABLISHED_RDKIT") || !RENV_ESTABLISHED_RDKIT || !rdkit_active()) {
    return("RDKit is not available.")
  }
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

# /exists ----
#* Run R function "exists" on an arbitrary function or variable name. (A plumber health endpoint.)
#* @param env_name:character THe name of an R object that should be present in the plumber environment.
#* @get /exists/<env_name>
function(env_name) {
  return(exists(env_name))
}

# /formals ----
#* Run R function "formals" on an arbitrary function to check its formally declared arguments. (A plumber health endpoint.) Accessing this by using `api_endpoint` may give unexpected results as formals without defaults are truncated in vectors.
#* @param fn_name:character The name of an R object that should be present in the plumber environment.
#* @get /formals/<fn_name>
function(fn_name) {
  if (!exists(fn_name)) {
    return("No function by that name is available.")
  } else {
    out <- lapply(
      formals(fn_name),
      function(x) {
        if (inherits(x, "name")) {
          NA_character_
        } else if (is.language(x)) {
          eval(x)
        } else {
          x
        }
      }
    )
  }
  return(out)
}

# /search_compound ----
#* Search for compounds matching a processed mass spectrum object in JSON notation. This does no preprocessing of the `search_ms` item and only executes the defined search on the database. The serialized version of the object created from `create_search_ms` is much smaller than that of serializing the entire mzML object.  
#* @param type:character The type of search to perform, one of either "precursor" to search depending on the defined precursor ion, or "all" to search all possible matches regardless of precursor ion.
#* @param search_ms:character Search object generated by other calls, primarily those created by the R function `create_search_ms` which relies on other paths and should be processed outside this server.
#* @param norm_function:character Normalization function to use during the search, one of "sum" or "mean"
#* @param correlation_method:character Correlation function to use during the search, must be one of "pearson", "kendall", or "spearman"
#* @param optimized_params:logical Whether or not to use the optimized search parameters present in `search_ms` (default TRUE) or to use an unconstrained search space (FALSE) which may improve matches under certain conditions.
#* @get /search_compound
function(type, search_ms, norm_function = "sum", correlation_method = "pearson", optimized_params = TRUE) {
  type <- match.arg(type, c("precursor", "all"))
  norm_function <- match.arg(norm_function, choices = c("sum", "mean"))
  correlation_method <- match.arg(correlation_method, eval(formals(cor)$method))
  search_ms <- utils::URLdecode(search_ms)
  search_ms <- jsonlite::fromJSON(search_ms)
  search_fn <- switch(type,
                      "precursor" = search_precursor,
                      "all" = search_all)
  out <- search_fn(
    con = con,
    searchms = search_ms,
    normfn = norm_function,
    cormethod = correlation_method,
    optimized_params = optimized_params
  )
  return(out)
}

# /method_narrative ----
#* Get the mass spectroscopic method narrative for a peak, sample, or method by database ID.
#* @param type:character The type of `id` to search, must be one of "peak", "sample", or "method"
#* @param table_pk:integer An integer primary key, which must exist
#* @get /method_narrative
function(type = "peak", table_pk) {
  type <- match.arg(type, c("peak", "sample", "method"))
  type <- switch(
    type,
    "peak" = "peaks",
    "sample" = "samples",
    "method" = "ms_methods"
  )
  table_pk <- as.integer(table_pk)
  if (!table_pk %in% (tbl(con, type) %>% pull(id))) {
      out <- glue::glue("The ID {table_pk} does not exist in table {type}.")
  } else {
    db_table <- tbl(con, type) %>%
      filter(id == table_pk)
    if (type == "peaks") {
      db_table <- db_table %>%
        left_join(tbl(con, "samples"),
                  by = c("sample_id" = "id")) %>%
        left_join(tbl(con, "ms_methods"),
                  by = c("ms_methods_id" = "id"))
    } else if (type == "samples") {
      db_table <- db_table %>%
        left_join(tbl(con, "ms_methods"),
                  by = c("ms_methods_id" = "id"))
    } else if (type == "ms_methods") {
      db_table <- db_table %>%
        rename("ms_methods_id" = "id")
    }
    out <- db_table %>%
      left_join(tbl(con, "view_method_narrative"),
                by = c("ms_methods_id" = "Method ID")) %>%
      pull(Narrative)
  }
  return(out)
}

# /sample_narrative ----
#* Get the plain text narrative for sample by database ID.
#* @param sample_id:integer An integer primary key, which must exist
#* @get /sample_narrative/<sample_id>
function(sample_id) {
  narrative_table <- "view_sample_narrative"
  id_col <- "Sample ID"
  sample_id <- as.integer(sample_id)
  if (!sample_id %in% (tbl(con, narrative_table) %>% pull(id_col))) {
    if (sample_id %in% (tbl(con, "samples") %>% pull(id_col))) {
      out <- glue::glue("The ID {sample_id} is in the samples table but does not have a narrative.")
    } else {
      out <- glue::glue("The ID {sample_id} does not exist.")
    }
  } else {
    out <- tbl(con, narrative_table) %>%
      filter(.data[[id_col]] == sample_id) %>%
      pull(Narrative)
  }
  return(out)
}

# /search_fragments ----
#* Get matching fragments from the database for a list of fragment mass-to-charge ratios
#* @param fragment_ions:character JSON list of mass-to-charge ratios observed for fragments, usually a member of the /search_compound at "[[search_object]][[ums2]][[mz]]"
#* @param mass_error:numeric Scalar observed mass error from the instrument run
#* @param min_error:numeric Scalar minimum error allowed
#* @get /search_fragments
function(fragment_ions, mass_error = 5, min_error = 0.002) {
  mass_error <- as.numeric(mass_error)
  min_error <- as.numeric(min_error)
  fragment_ions <- jsonlite::fromJSON(utils::URLdecode(fragment_ions)) %>%
    as.numeric()
  # TODO offload the join logic to get_compound_fragments so the console result is the same as the API result
  out <- get_compound_fragments(
    con = con,
    fragmentions = fragment_ions,
    masserror = mass_error,
    minerror = min_error
  ) %>%
    left_join(
      DBI::dbGetQuery(
        conn = con,
        statement = glue::glue("select id, name from compounds where id in ({paste0(unique(.$compound_id), collapse = ',')})")
      ) %>%
        rename("compound_name" = "name",
               "compound_id" = "id")
    ) %>%
    left_join(
      DBI::dbGetQuery(
        conn = con,
        statement = glue::glue("select annotated_fragment_id, mz, ppm_error from view_annotated_fragments where annotated_fragment_id in ({paste0(unique(.$annotated_fragment_id), collapse = ',')})")
      )
    ) %>%
    group_by(norm_fragment_id) %>%
    mutate(peak_id = if ("peak_id" %in% names(.)) peak_id else NA_integer_) %>%
    nest(compounds = starts_with("compound"),
         peak_ids = c(peak_id),
         annotated_fragments = c(annotated_fragment_id, mz, ppm_error)) %>%
    mutate(compounds = lapply(compounds, distinct),
           n_compounds = sapply(compounds, nrow, simplify = TRUE),
           peak_ids = lapply(peak_ids, function(x) pull(unique(x))),
           n_peaks = sapply(peak_ids, length),
           annotated_fragments = lapply(
             annotated_fragments,
             function(x)
               x %>%
               group_by(annotated_fragment_id) %>%
               add_count(name = "measured_n_times") %>%
               distinct()
           ),
           n_annotations = sapply(annotated_fragments, function(x) sum(x$measured_n_times), simplify = TRUE)
    ) %>%
    mutate(has_smiles = !is.na(smiles)) %>%
    select(norm_fragment_id, compounds, peak_ids, annotated_fragments, formula, fixedmass, netcharge, radical, has_smiles, smiles, n_compounds, n_peaks, n_annotations)
  return(out)
}

# # /peak_plot ----
# #* @param peak_id:integer
# #* @get peak_plot/<peak_id>
# function(peak_id) {
#   peak_id <- as.integer(peak_id)
#   peak_data <- tbl(con, "ms_data") %>%
#     filter(peak_id == peak_id) %>%
#     tidy_spectra(is_format = "separated")
#   
# }
