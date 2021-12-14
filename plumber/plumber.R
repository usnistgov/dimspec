if (!exists("con")) manage_connection()

#* @apiTitle Application Programming Interface: NIST HRMS Database
#* @apiDescription This plumber API requires a current copy of the NIST high-resolution accurate-mass spectrometry database.

#* Build a DB Action
#* @param action CHR scalar, of one "INSERT", "UPDATE", "SELECT", "GET_ID", or "DELETE"
#* @param table_name CHR scalar of the table name to which this query applies
#* @param column_names CHR vector of column names to include (default: "" goes to NULL)
#* @param values LIST of CHR vectors with values to INSERT or UPDATE (default: "" goes to NULL)
#* @param match_criteria LIST of matching criteria to be passed to {clause_where} with names matching columns against which to apply. In the simplest case, a direct value is given to the name (e.g. `list(last_name = "Smith")`) for single matches. All match criteria must be their own list item. Values can also be provided as a nested list for more complicated WHERE clauses with names `values`, `exclude`, and `like` that will be recognized. `values` should be the actual search criteria, and if a vector of length greater than one is specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar) determines whether to apply the NOT operator. `like` (LGL scalar) determines whether this is an equality, list, or similarity. To reverse the example above by issuing a NOT statement, use `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name = list(values = "Smith", exclude = FALSE, like = TRUE))` (default: "" goes to NULL)
#* @param case_sensitive LGL scalar of whether to match on a case sensitive basis (the default TRUE searches for values as-provided) or whether to coerce value matches by upper, lower, sentence, and title case matches; passed directly to [clause_where] (default: TRUE)
#* @param and_or CHR scalar one of "AND" or "OR" to be applied to the match criteria (default "OR")
#* @param limit INT scalar of the maximum number of rows to return  (default 15)
#* @param group_by CHR vector of columns by which to group (default "" goes to NULL)
#* @param order_by named CHR vector of columns by which to order, with names matching columns and values indicating whether to sort ascending (default "" goes to NULL)
#* @param distinct LGL scalar of whether or not to apply the DISTINCT clause to all match criteria (default FALSE)
#* @param get_all_columns LGL scalar of whether to return all columns; will be set to TRUE automatically if no column names are provided (default FALSE)
#* @param execute LGL scalar of whether or not to immediately execute the build query statement (default TRUE)
#* @param single_column_as_vector LGL scalar of whether to return results as a vector if they consist of only a single column (default TRUE)
#* @get /build_db_action
function(action          = "select",
         table_name      = "contributors",
         column_names    = "",
         values          = "",
         match_criteria  = "",
         case_sensitive  = TRUE,
         and_or          = "OR",
         limit           = 15,
         group_by        = "",
         order_by        = "",
         distinct        = FALSE,
         get_all_columns = TRUE,
         ignore          = FALSE,
         execute         = TRUE,
         single_column_as_vector = TRUE) {
  params <- as.list(environment())
  need_logical <- c("case_sensitive", "distinct", "get_all_columns", "ignore", "execute", "single_column_as_vector")
  for (nl in need_logical) {
    params[[nl]] <- as.logical(params[[nl]])
  }
  check_null <- c("column_names", "values", "match_criteria", "group_by", "order_by")
  for (cn in check_null) {
    if (params[[cn]] == "") params[[cn]] <- NULL
  }
  do.call("build_db_action", params)
}