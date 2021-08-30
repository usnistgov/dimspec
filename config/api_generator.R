
queries <- list(
  INSERT = "INSERT INTO ?table_name (?column_names) VALUES ?values",
  UPDATE = "UPDATE ?table_name SET ?values",
  SELECT = "SELECT ?column_names FROM ?table_name",
  GET_ID = "SELECT id FROM ?table_name",
  DELETE = "DELETE FROM ?table_name"
)

#' Ensure database table presence
#'
#' When working with SQL databases, this convenience function validates any
#' number of table names by comparing against the list of those present.
#' Typically it is called transparently inline to cause execution failure when
#' tables are not present during build of SQL queries.
#'
#' @param con connection object (e.g. of class "SQLiteConnection")
#' @param table_names CHR vector name of tables to ensure are present
#'
#' @return
#' @export
#'
#' @examples
validate_tables <- function(con, table_names) {
  table_list   <- unique(dbListTables(con))
  table_exists <- unique(table_names) %in% table_list
  if (!all(table_exists)) {
    bad_tables <- table_names[!table_exists]
    stop(
      sprintf("Did not recognize %s '%s'",
              ifelse(length(bad_tables) > 1, "tables", "table"),
              paste0(bad_tables, collapse = "', '")
      )
    )
  }
}

#' Ensure database column presence
#'
#' When working with SQL databases, this convenience function validates any
#' number of column names by comparing against the list of column names in any
#' number of tables. Typically it is called transparently inline to cause
#' execution failure when column names are not present in referenced tables
#' during build of SQL queries.
#'
#' @param con connection object (e.g. of class "SQLiteConnection")
#' @param table_names CHR vector of tables to search
#' @param column_names CHR vector of column names to validate
#'
#' @return None
#' @export
#'
#' @examples
validate_column_names <- function(con, table_names, column_names) {
  valid_fields <- lapply(table_names, function(x) dbListFields(con, x)) %>%
    unlist() %>% unique()
  valid_columns <- unique(column_names) %in% valid_fields
  if (!all(valid_columns)) {
    stop(
      sprintf("Did not recognize %s '%s' in %s '%s'",
              ifelse(length(column_names) > 1, "columns", "column"),
              format_list_of_names(column_names[!valid_columns]),
              ifelse(length(table_names) > 1, "tables", "table"),
              paste0(table_names, collapse = "', '")
      )
    )
  }
}

#' Build a WHERE clause for SQL statements
#'
#' Properly escaping SQL to prevent injection attacks can be difficult with more
#' complicated queries. This clause constructor is intended to be specific to
#' the WHERE clause of SELECT to UPDATE statements. The majority of construction
#' is achieved with the `match_criteria` parameter, which should always be a
#' list with names for the columns to appear in the WHERE clause. A variety of
#' convenience is built in, from simple comparisons to more complicated ones
#' including negation and similarity (see the description for argument
#' `match_criteria`).
#'
#' @param con existing connection object (e.g. of class "SQLiteConnection")
#' @param table_names CHR vector of tables to search
#' @param match_criteria LIST of matching criteria with names matching columns
#'   against which to apply. In the simplest case, a direct value is given to
#'   the name (e.g. `list(last_name = "Smith")`) for single matches. All match
#'   criteria must be their own list item. Values can also be provided as a
#'   nested list for more complicated WHERE clauses with names `values`,
#'   `exclude`, and `like` that will be recognized. `values` should be the
#'   actual search criteria, and if a vector of length greater than one is
#'   specified, the WHERE clause becomes an IN clause. `exclude` (LGL scalar)
#'   determines whether to apply the NOT operator. `like` (LGL scalar)
#'   determines whether this is an equality, list, or similarity. To reverse the
#'   example above by issuing a NOT statement, use `list(last_name = list(values
#'   = "Smith", exclude = TRUE))`, or to look for all records LIKE (or NOT LIKE)
#'   "Smith", set this as `list(last_name = list(values = "Smith", exclude =
#'   FALSE, like = TRUE))`
#' @param and_or LGL scalar of whether to use "AND" or "OR" for multiple
#'   criteria, which will be used to combine them all. More complicated WHERE
#'   clauses (including a mixture of AND and OR usage) should be built directly.
#'   (default: "OR")
#'
#' @return CHR scalar of the constructed where clause for an SQL statement
#' @export
#'
#' @examples
#' clause_where(ANSI(), "example", list("foo" = "bar", "cat" = "dog"))
#' clause_where(ANSI(), "example", list("foo" = list(values = "bar", like = TRUE)))
#' clause_where(ANSI(), "example", list("foo" = list(values = "bar", exclude = TRUE)))
clause_where <- function(con, table_names, match_criteria, and_or = "OR") {
  and_or <- toupper(and_or)
  and_or <- match.arg(and_or, c("AND", "OR"))
  and_or <- paste0(" ", and_or, " ")
  out    <- match_criteria
  is_ansi <- identical(con, ANSI())
  # Ensure column names exist
  if (!is_ansi) {
    validate_column_names(con, table_names, names(match_criteria))
  }
  keywords <- c("values", "exclude", "like")
  # Parse match criteria
  for (m in names(out)) {
    column <- dbQuoteIdentifier(con, m)
    modifiers <- names(out[[m]])
    if (all(is.null(modifiers), !is.list(out[[m]]))) {
      out[[m]] <- list(
        values  = out[[m]],
        exclude = NULL,
        like    = NULL
      )
    } else if (any(modifiers %in% keywords)) {
      discarded <- names(out[[m]])[!modifiers %in% keywords]
      out[[m]]  <- out[[m]][which(modifiers %in% keywords)]
      if (length(discarded) > 0) {
        warning(
          sprintf("The extra %s provided ('%s') %s ignored.",
                  ifelse(length(discarded) == 1, "property", "properties"),
                  paste0(discarded, collapse = "', '"),
                  ifelse(length(discarded) == 1, "was", "were")
          )
        )
      }
    } else {
      stop(
        sprintf("Did not recognize any keyword value (must be one of: '%s') in the names of 'match_criteria' for table '%s'.",
                paste0(keywords, collapse = "', '"),
                m)
      )
    }
    modifiers <- names(out[[m]])
    checks    <- out[[m]]$values
    if (length(checks) == 1) {
      out[[m]]$query <- sqlInterpolate(con, "? = ?", column, checks)
    } else {
      out[[m]]$query <- paste0(column, " IN (",
                         paste0(
                           lapply(checks,
                                  function(x) {
                                    if (is.character(x)) {
                                      dbQuoteString(con, x)
                                    } else {
                                      dbQuoteLiteral(con, x)
                                    }
                                    
                                  }) %>%
                             unlist(),
                           collapse = ", "),
                         ")")
    }
    
    # Negate clause
    if (!is.null(out[[m]]$exclude)) {
      if (out[[m]]$exclude) {
        out[[m]]$query <- paste("NOT", out[[m]]$query)
      }
    }
    
    # Shift to "LIKE" operator
    if (!is.null(out[[m]]$like)) {
      if (length(out[[m]]$values) > 1) {
        warning("Like operator is only available for single values.")
      } else {
        out[[m]]$query <- str_replace(out[[m]]$query,
                                      " IN | = ",
                                      " LIKE ") %>%
          str_replace_all("[%]+", "%")
        if (!grepl("%", out[[m]]$query)) {
          out[[m]]$query <- str_replace(out[[m]]$query,
                                        "LIKE '(.*)'",
                                        "LIKE '%\\1%'")
        }
      }
    }
  }
  out <- lapply(out, function(x) x$query) %>%
    unlist() %>%
    unname()
  if (length(match_criteria) > 1) {
    out <- paste0(out, collapse = and_or)
  }
  return(out)
}

#' Build a basic escaped SQL query
#'
#' @param con existing connection object (e.g. of class "SQLiteConnection")
#' @param action CHR scalar, of one "INSERT", "UPDATE", "SELECT", "GET_ID", or
#'   "DELETE"
#' @param table_name CHR scalar of the table name to which this query applies
#' @param column_names CHR vector of column names to include (default NULL)
#' @param values LIST of CHR vectors with values to INSERT or UPDATE (default
#'   NULL)
#' @param match_criteria LIST of matching criteria to be passed to
#'   {clause_where} with names matching columns against which to apply. In the
#'   simplest case, a direct value is given to the name (e.g. `list(last_name =
#'   "Smith")`) for single matches. All match criteria must be their own list
#'   item. Values can also be provided as a nested list for more complicated
#'   WHERE clauses with names `values`, `exclude`, and `like` that will be
#'   recognized. `values` should be the actual search criteria, and if a vector
#'   of length greater than one is specified, the WHERE clause becomes an IN
#'   clause. `exclude` (LGL scalar) determines whether to apply the NOT
#'   operator. `like` (LGL scalar) determines whether this is an equality, list,
#'   or similarity. To reverse the example above by issuing a NOT statement, use
#'   `list(last_name = list(values = "Smith", exclude = TRUE))`, or to look for
#'   all records LIKE (or NOT LIKE) "Smith", set this as `list(last_name =
#'   list(values = "Smith", exclude = FALSE, like = TRUE))`
#' @param and_or CHR scalar one of "AND" or "OR" to be applied to the match
#'   criteria (default "OR")
#' @param limit INT scalar of the maximum number of rows to return  (default
#'   NULL)
#' @param group_by CHR vector of columns by which to group (default NULL)
#' @param order_by named CHR vector of columns by which to order, with names
#'   matching columns and values indicating whether to sort ascending (default
#'   NULL)
#' @param distinct LGL scalar of whether or not to apply the DISTINCT clause to
#'   all match criteria (default FALSE)
#' @param get_all_columns LGL scalar of whether to return all columns; will be
#'   set to TRUE automatically if no column names are provided (default FALSE)
#'
#' @return CHR scalar of the constructed query
#' @export
#'
#' @examples
build_db_action <- function(con,
                            action,
                            table_name,
                            column_names    = NULL,
                            values          = NULL,
                            match_criteria  = NULL,
                            and_or          = "OR",
                            limit           = NULL,
                            group_by        = NULL,
                            order_by        = NULL,
                            distinct        = FALSE,
                            get_all_columns = FALSE) {
  # Argument validation
  action       <- toupper(action)
  table_name   <- tolower(table_name)
  and_or       <- toupper(and_or)
  is_ansi      <- identical(con, ANSI())
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(action, table_name, and_or, distinct, get_all_columns),
      conditions = list(
        action          = list(c("choices", list(toupper(names(queries)))),
                               c("mode", "character")),
        table_name      = if (is_ansi) {
          list(c("mode", "character"),
               c("length", 1))
        } else {
          list(c("choices", list(dbListTables(con))),
               c("mode", "character"),
               c("length", 1))
        },
        and_or          = list(c("choices", list(c("AND", "OR"))),
                               c("mode", "character"),
                               c("length", 1)),
        distinct        = list(c("mode", "logical"),
                               c("length", 1)),
        get_all_columns = list(c("mode", "logical"),
                               c("length", 1))
      )
    )
    if (!arg_check$valid) {
      if ("logger" %in% (.packages())) {
        stop()
      } else {
        stop(cat(paste0(arg_check$messages, collapse = "\n")))
      }
    }
  }
  if (!is_ansi) {
    validate_tables(con, table_name)
  }
  
  query <- queries[[action]]
  if (all(action == "SELECT", distinct)) {
    query <- str_replace(query, "SELECT", "SELECT DISTINCT")
  }
  
  # Ensure columns exist or is a select all query
  if (is.null(column_names)) {
    get_all_columns <- TRUE
  } else {
    check_fields <- c(column_names, names(match_criteria), group_by, names(order_by)) %>%
      tolower()
    if (!is_ansi) {
        validate_column_names(con = con, table_names = table_name, column_names = check_fields)
    }
  }
  
  if (get_all_columns) {
    if (action == "INSERT") {
      query <- gsub("\\(\\?column_names\\) ", "", query)
    } else {
      query <- gsub("\\?column_names", "*", query)
    }
  }
  
  # Safely escape column names
  query <- str_replace(query,
                       "\\?column_names",
                       paste0(lapply(column_names,
                                     function(x) {
                                       dbQuoteIdentifier(con, x)
                                     }),
                              collapse = ", "
                       )
  )
  
  # Safely escape values
  values_formatted <- switch(
    action,
    "INSERT" = paste0("(",
                      lapply(values,
                      function(x) {
                        lapply(x,
                               function(x) {
                                 dbQuoteLiteral(con, x)
                               }) %>%
                          unlist() %>%
                          paste0(collapse = ", ")
                      }),
                      ")",
                      collapse = ", "),
    "UPDATE" = paste0(lapply(names(values),
                      function(x) {
                        sprintf("%s = %s",
                                dbQuoteIdentifier(con, x),
                                dbQuoteLiteral(con, values[[x]]))
                      }),
                      collapse = ", ")
  )
  query <- str_replace(query, "\\?values", values_formatted)
  
  # Safely escape table names
  query <- str_replace(query,
                       "\\?table_name",
                       dbQuoteIdentifier(con, table_name)
  )
  
  # Safely escape where clauses
  if (all(!is.null(match_criteria), action != "INSERT")) {
    query <- paste(query, "WHERE",
                   clause_where(con            = con,
                                table_names    = table_name,
                                match_criteria = match_criteria,
                                and_or         = and_or))
  }
  
  if (action == "SELECT") {
    # Safely escape limit clause
    if (!is.null(limit)) {
      if (!is.numeric(limit)) {
        limit <- as.integer(limit)
        limit <- limit[!is.na(limit)]
      }
      if (any(length(limit) == 0, length(limit) > 1)) {
        stop('Exactly one value must be provided for the "limit" parameter.')
      } else {
        query <- paste(query, "LIMIT", dbQuoteLiteral(con, used_args$limit))
      }
    }
    
    # Safely escape group by clause
    if (!is.null(group_by)) {
      query <- paste(query, "GROUP BY", dbQuoteIdentifier(con, group_by))
    }
    
    # Safely escape order by clause
    if (!is.null(order_by)) {
      ordering <- paste0(
        lapply(names(order_by),
               function(x) {
                 paste0(dbQuoteIdentifier(con, x),
                        " ",
                        order_by[[x]])
               }),
        collapse = ", "
      )
      query <- paste(query, "ORDER BY", ordering) %>%
        str_trim()
    }
  }
  
  if (action == "DELETE") {
    select_version <- str_replace(query, "^DELETE FROM", "SELECT * FROM")
    rows_affected <- nrow(dbGetQuery(con, select_version))
    cat(sprintf("Your constructed action\n'%s'\n%s\n\n",
                query,
                ifelse(rows_affected > 0,
                       sprintf("will delete %s %s.",
                               rows_affected,
                               ifelse(rows_affected == 1, "row", "rows")),
                       "does not match any rows")))
    if (rows_affected > 0) {
      confirm <- select.list(choices   = c("CONFIRM", "abort"),
                             preselect = "abort",
                             multiple  = FALSE,
                             title     = "Please confirm.")
      if (!confirm == "CONFIRM") {
        cat("Query construction aborted.\n")
        query <- NA
      }
    }
  }
  
  return(query)
}
