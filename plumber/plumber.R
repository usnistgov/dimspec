#* @apiTitle API Guide: NIST HRMS Database
#* @apiDescription This plumber API requires a current copy of the NIST high-resolution accurate-mass spectrometry database.

#* Return data from tables or views as JSON expressions.
#* @param table_name CHR scalar of the table name to which this query applies
#* @param column_names CHR vector of column names to include (leave blank to include all columns)
#* @param match_criteria LIST of matching criteria to be passed to {clause_where} with names matching columns against which to apply. In the simplest case, a direct value is given to the name (e.g. `list(last_name = "Smith")`) for single matches. All match criteria must be their own list item. Values can also be provided as a nested list for more complicated WHERE clauses with names `values`, `exclude`, and `like` that will be recognized. `values` should be the actual search criteria, and if a vector of length greater than one is specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar) determines whether to apply the NOT operator. `like` (LGL scalar) determines whether this is an equality, list, or similarity. To reverse the example above by issuing a NOT statement, use `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name = list(values = "Smith", exclude = FALSE, like = TRUE))` (default: "" goes to NULL)
#* @param case_sensitive LGL scalar of whether to match on a case sensitive basis (the default TRUE searches for values as-provided) or whether to search for value matches by upper, lower, sentence, and title case matches; passed directly to [clause_where] (default: TRUE)
#* @param and_or CHR scalar one of "AND" or "OR" to be applied to the match criteria (default "OR")
#* @param limit INT scalar of the maximum number of rows to return  (default 15)
#* @param distinct LGL scalar of whether or not to apply the DISTINCT clause to all match criteria (default FALSE)
#* @param get_all_columns LGL scalar of whether to force return all columns rather than those identified in column_names; will be set to TRUE automatically if no column names are provided (default FALSE)
#* @param execute LGL scalar of whether or not to immediately execute the build query statement (default TRUE, FALSE will instead return the SQL statement to be executed)
#* @param single_column_as_vector LGL scalar of whether to return results as a vector if they consist of only a single column (default TRUE)
#* @get /table_search
function(table_name      = as.character("contributors"),
         column_names    = as.character(""),
         match_criteria  = as.character(""),
         case_sensitive  = as.logical("true"),
         and_or          = as.character("AND"),
         distinct        = as.logical("false"),
         limit           = as.integer(15),
         get_all_columns = as.logical("false"),
         execute         = as.logical("true"),
         single_column_as_vector = as.logical("true")) {
  action <- "select"
  params <- as.list(environment())
  need_logical <- c("case_sensitive", "distinct", "get_all_columns", "execute", "single_column_as_vector")
  for (nl in need_logical) {
    params[[nl]] <- as.logical(params[[nl]])
  }
  if (!match_criteria == "") {
    params[["match_criteria"]] <- utils::URLdecode(match_criteria)
  # browser()
    params$match_criteria <- params$match_criteria %>%
      utils::URLdecode() %>%
      str_remove_all('"') %>%
      str_split(",") %>%
      purrr::flatten() %>%
      lapply(str_split, "=") %>%
      lapply(str_trim) %>%
      lapply(function(x) {
        setNames(x[2], x[1])
      }) %>%
      unlist() %>%
      as.list()
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
  do.call("build_db_action", params)
}