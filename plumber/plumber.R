#* @apiTitle API Guide: NIST HRMS Database
#* @apiDescription This plumber API requires a current copy of the NIST high-resolution accurate-mass spectrometry database.

#* Return data from tables or views as JSON expressions.
#* @param table_name The name of a single table to which this query applies.
#* @param column_names A comma-separated list of column names to include (leave blank to include all columns)
#* @param match_criteria An R LIST expression of matching criteria to be passed to {clause_where} with names matching columns against which to apply. In the simplest case, a direct value is given to the name (e.g. `list(last_name = "Smith")`) for single matches. All match criteria must be their own list item. Values can also be provided as a nested list for more complicated WHERE clauses with names `values`, `exclude`, and `like` that will be recognized. `values` should be the actual search criteria, and if a vector of length greater than one is specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar) determines whether to apply the NOT operator. `like` (LGL scalar) determines whether this is an equality, list, or similarity. To reverse the example above by issuing a NOT statement, use `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name = list(values = "Smith", exclude = FALSE, like = TRUE))` (default: "" goes to NULL)
#* @param match_crit_json A single logical value indicating whether `match_criteria` are provided in JSON format.
#* @param case_sensitive A single logical value indicating whether to match on a case sensitive basis (the default TRUE searches for values as-provided) or whether to search for value matches by upper, lower, sentence, and title case matches; passed directly to [clause_where] (default: TRUE)
#* @param and_or One of "AND" or "OR" to be applied to the match criteria (default "OR")
#* @param limit A single integer value of the maximum number of rows to return  (default 15)
#* @param distinct A single logical value indicating whether or not to apply the DISTINCT clause to all match criteria (default FALSE)
#* @param get_all_columns A single logical value indicating whether to force return all columns rather than those identified in column_names; will be set to TRUE automatically if no column names are provided (default FALSE)
#* @param execute A single logical value indicating whether or not to immediately execute the build query statement (default TRUE, FALSE will instead return the SQL statement to be executed)
#* @param single_column_as_vector A single logical value indicating whether to collapse results to a vector if they consist of only a single column (default TRUE)
#* @get /table_search
function(table_name      = as.character("contributors"),
         column_names    = as.character(""),
         match_criteria  = as.character(""),
         match_crit_json = FALSE,
         case_sensitive  = as.logical("true"),
         and_or          = as.character("AND"),
         distinct        = as.logical("false"),
         limit           = as.integer(15),
         get_all_columns = as.logical("false"),
         execute         = as.logical("true"),
         single_column_as_vector = as.logical("true")) {
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
  msg <- paste0(names(params), ' = ' , unlist(unname(params)), collapse = ',')
  if (inherits(res, "try-error")) {
    res <- glue::glue("Malformed call to build_db_action with params {msg}.")
  }
  return(res)
}

#* Return mass spectral data for a peak by its internal ID number.
#* @param peak_id A single integer value of the peak ID for which to retrieve mass spectral data.
#* @get /peak_data
function(peak_id = integer(), tidy_spectra = as.logical("true")) {
  ms_data <- dbGetQuery(
    con,
    DBI::sqlInterpolate(con,
                        "select * from peak_data where peak_id = ?peak_id",
                        peak_id = peak_id)
  )
  if (tidy_spectra) {
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

#* Is the connection valid
#* @get /active
function() return(dbIsValid(con))
