# TODO Add logging statements rather than stop/warning/etc. throughout this file.

queries <- list(
  NROW   = "SELECT COUNT(*) FROM ?table_name",
  INSERT = "INSERT INTO ?table_name (?column_names) VALUES ?values",
  UPDATE = "UPDATE ?table_name SET ?values",
  SELECT = "SELECT ?column_names FROM ?table_name",
  GET_ID = "SELECT `id` FROM ?table_name",
  DELETE = "DELETE FROM ?table_name"
)

#' Ensure database table presence
#'
#' When working with SQL databases, this convenience function validates any
#' number of table names by comparing against the list of those present.
#' Typically it is called transparently inline to cause execution failure when
#' tables are not present during build of SQL queries.
#'
#' @param db_conn connection object (e.g. of class "SQLiteConnection")
#' @param table_names CHR vector name of tables to ensure are present
#'
#' @return Failure if the table doesn't exist, none if it does.
#' @export
#'
#' @usage validate_tables(con, "peaks")
validate_tables <- function(db_conn, table_names) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_conn     = list(c("length", 1)),
        table_names = list(c("mode", "character"), c("n>=", 1))
      ),
      from_fn = "validate_tables"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  
  table_list   <- unique(dbListTables(db_conn))
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
#' @param db_conn connection object (e.g. of class "SQLiteConnection")
#' @param table_names CHR vector of tables to search
#' @param column_names CHR vector of column names to validate
#'
#' @return None
#' @export
#'
#' @usage validate_column_names(con, "peaks", "id")
validate_column_names <- function(db_conn, table_names, column_names) {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_conn          = list(c("length", 1)),
        table_names  = list(c("mode", "character"), c("n>=", 1)),
        column_names = list(c("mode", "character"), c("n>=", 1))
      ),
      from_fn = "validate_column_names"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  row_id <- "rowid"
  row_id_refs <- c("row_id", "rowid", "ROWID", "ROW_ID")
  if (any(row_id_refs %in% column_names)) {
    column_names[which(column_names %in% row_id_refs)] <- row_id
    column_names <- unique(column_names)
  }
  valid_fields <- c(
    table_names %>%
      lapply(function(x) dbListFields(db_conn, x)) %>%
    unlist() %>%
      unique(),
    row_id
  )
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
#' @inheritParams build_db_action
#'
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
#' @param case_sensitive LGL scalar of whether to match on a case sensitive
#'   basis (the default TRUE searches for values as-provided) or whether to
#'   coerce value matches by upper, lower, sentence, and title case matches
#' @param and_or LGL scalar of whether to use "AND" or "OR" for multiple
#'   criteria, which will be used to combine them all. More complicated WHERE
#'   clauses (including a mixture of AND and OR usage) should be built directly.
#'   (default: "OR")
#'
#' @return CHR scalar of the constructed where clause for an SQL statement
#' @export
#'
#' @usage
#' clause_where(ANSI(), "example", list(foo = "bar", cat = "dog"))
#' clause_where(ANSI(), "example", list(foo = list(values = "bar", like = TRUE)))
#' clause_where(ANSI(), "example", list(foo = list(values = "bar", exclude = TRUE)))
clause_where <- function(db_conn, table_names, match_criteria, case_sensitive = TRUE, fuzzy = FALSE, and_or = "OR") {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_conn        = list(c("length", 1)),
        table_names    = list(c("mode", "character"), c("n>=", 1)),
        match_criteria = list(c("mode", "list")),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        fuzzy          = list(c("mode", "logical"), c("length", 1)),
        and_or         = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "clause_where"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Check connection
  and_or <- toupper(str_trim(and_or))
  and_or <- match.arg(and_or, c("AND", "OR"))
  and_or <- paste0(" ", and_or, " ")
  out    <- match_criteria
  if (!length(names(out)) == length(out)) stop("All match criteria must be named.")
  is_ansi <- identical(db_conn, ANSI())
  # Ensure column names exist
  if (!is_ansi) {
    stopifnot(active_connection(db_conn))
    validate_column_names(db_conn, table_names, names(match_criteria))
  }
  keywords <- c("values", "exclude", "like", "query")
  # Parse match criteria
  for (m in names(out)) {
    column <- dbQuoteIdentifier(db_conn, m)
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
    if (fuzzy) {
      out[[m]]$values <- checks
      out[[m]]$like   <- TRUE
    } else {
      if (!case_sensitive) {
        checks <- unique(
          c(
            checks,
            str_to_lower(checks),
            str_to_title(checks),
            str_to_sentence(checks),
            str_to_upper(checks)
          )
        )
        out[[m]]$values <- checks
        out[[m]]$like   <- FALSE
      }
    }
    if (length(checks) == 1) {
      out[[m]]$query <- sqlInterpolate(db_conn, "? = ?", column, checks)
    } else if (length(checks) > 1) {
      out[[m]]$query <- paste0(column, " IN (",
                               paste0(
                                 lapply(checks,
                                        function(x) {
                                          if (is.character(x)) {
                                            dbQuoteString(db_conn, x)
                                          } else {
                                            dbQuoteLiteral(db_conn, x)
                                          }
                                          
                                        }) %>%
                                   unlist(),
                                 collapse = ", "),
                               ")")
    } else {
      out[[m]]$query <- sqlInterpolate(db_conn, "? IS NULL", column)
    }
    
    # Negate clause
    if (!is.null(out[[m]]$exclude)) {
      if (out[[m]]$exclude) {
        out[[m]]$query <- paste("NOT", out[[m]]$query)
      }
    }
    
    # Shift to "LIKE" operator
    if (!is.null(out[[m]]$like) && out[[m]]$like) {
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
  catch_null <- " = '*(NULL|null|NA|na)'*| = ''"
  if (str_detect(out, catch_null)) {
    out <- out %>%
      str_replace_all(catch_null, " IS NULL")
  }
  return(out)
}

#' Build an escaped SQL query
#'
#' In most cases, issuing basic SQL queries is made easy by tidyverse compliant
#' functions such as [dplyr::tbl]. Full interaction with an SQLite database is a
#' bit more complicated and typically requires [DBI::dbExecute] and writing SQL
#' directly; several helpers exist for that (e.g. [glue::glue_sql]) but aren't
#' as friendly or straight forward when writing more complicated actions, and
#' still require directly writing SQL equivalents, routing through
#' [DBI::dbQuoteIdentifier] and [DBI::dbQuoteLiteral] to prevent SQL injection
#' attacks.
#'
#' This function is intended to ease that by taking care of most of the
#' associated logic and enabling routing through other functions, or picking up
#' arguments from within other function calls.
#' 
#' @inheritParams clause_where
#' 
#' @param action CHR scalar, of one "INSERT", "UPDATE", "SELECT", "GET_ID", or
#'   "DELETE"
#' @param table_name CHR scalar of the table name to which this query applies
#' @param column_names CHR vector of column names to include (default NULL)
#' @param values LIST of CHR vectors with values to INSERT or UPDATE (default
#'   NULL)
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
#' @param execute LGL scalar of whether or not to immediately execute the build
#'   query statement (default TRUE)
#' @param single_column_as_vector LGL scalar of whether to return results as a
#'   vector if they consist of only a single column (default TRUE)
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#'
#' @return CHR scalar of the constructed query
#' @export
#'
#' @usage build_db_action("insert", "table", values = list(col1 = "a", col2 = 2,
#'   col3 = "describe"), execute = FALSE) build_db_action("insert", "table",
#'   values = list(col1 = "a", col2 = 2, col3 = "describe"))
#'   
#'   build_db_action("get_id", "table", match_criteria = list(id = 2))
#'   
#'   build_db_action("delete", "table", match_criteria = list(id = 2))
#'   
#'   build_db_action("select", "table", columns = c("col1", "col2", "col3"),
#'   match_criteria = list(id = 2)) build_db_action("select", "table",
#'   match_criteria = list(sample_name = "sample 123"))
#'   
#'   build_db_action("select", "table", match_criteria = list(sample_name =
#'   list(value = "sample 123", exclude = TRUE)) build_db_action("select",
#'   "table", match_criteria = list(sample_name = "sample 123",
#'   sample_contributor = "Smith"), and_or = "AND", limit = 5)
build_db_action <- function(action,
                            table_name,
                            db_conn         = con,
                            column_names    = NULL,
                            values          = NULL,
                            match_criteria  = NULL,
                            case_sensitive  = TRUE,
                            fuzzy           = FALSE,
                            and_or          = "OR",
                            limit           = NULL,
                            group_by        = NULL,
                            order_by        = NULL,
                            distinct        = FALSE,
                            get_all_columns = FALSE,
                            ignore          = FALSE,
                            execute         = TRUE,
                            single_column_as_vector = TRUE,
                            log_ns          = "db") {
  # Argument validation
  action       <- toupper(action)
  table_name   <- tolower(table_name)
  and_or       <- toupper(and_or)
  is_ansi      <- identical(db_conn, ANSI())
  logging      <- exists("LOGGING_ON") && LOGGING_ON && exists("log_it")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(action, table_name, db_conn, case_sensitive, fuzzy,
                        and_or, distinct, get_all_columns, ignore, execute,
                        single_column_as_vector, log_ns),
      conditions = list(
        action          = list(c("choices", list(toupper(names(queries)))),
                               c("mode", "character")),
        table_name      = if (is_ansi) {
          list(c("mode", "character"),
               c("length", 1))
        } else {
          list(c("choices", list(dbListTables(db_conn))),
               c("mode", "character"),
               c("length", 1))
        },
        db_conn         = list(c("length", 1)),
        case_sensitive  = list(c("mode", "logical"), c("length", 1)),
        fuzzy           = list(c("mode", "logical"), c("length", 1)),
        and_or          = list(c("choices", list(c("AND", "OR"))),
                               c("mode", "character"),
                               c("length", 1)),
        distinct        = list(c("mode", "logical"),
                               c("length", 1)),
        get_all_columns = list(c("mode", "logical"),
                               c("length", 1)),
        ignore          = list(c("mode", "logical"),
                               c("length", 1)),
        execute         = list(c("mode", "logical"),
                               c("length", 1)),
        single_column_as_vector = list(c("mode", "logical"),
                                       c("length", 1)),
        log_ns          = list(c("mode", "character"),
                               c("length", 1))
      ),
      from_fn = "build_db_action"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (!is_ansi) {
    validate_tables(db_conn, table_name)
  }
  
  if (is.data.frame(values)) values <- purrr::transpose(values)
  
  query <- queries[[action]]
  if (all(action == "SELECT", distinct)) {
    query <- str_replace(query, "SELECT", "SELECT DISTINCT")
  }
  
  if (action == "INSERT") {
    if (ignore) {
      query <- str_replace(query, "INSERT", "INSERT OR IGNORE")
    }
    column_names <- names(values)
  }
  
  # Ensure columns exist or is a select all query
  if (is.null(column_names)) {
    get_all_columns <- TRUE
  } else {
    check_fields <- c(column_names, names(match_criteria), group_by, names(order_by), names(values)) %>%
      tolower()
    if (!is_ansi) {
      validate_column_names(db_conn = db_conn, table_names = table_name, column_names = check_fields)
    }
  }
  
  # Check that values is formatted properly as a nested list
  if (!is.null(values)) {
    if (!is.list(values[[1]])) {
      values <- list(values)
    }
    value_names <- names(values[[1]])
  } else {
    value_names <- NULL
  }
  
  if (get_all_columns) {
    if (action == "INSERT") {
      req_names <- dbListFields(db_conn, table_name)
      if (length(value_names) == length(req_names) && all(order(value_names) == order(req_names))) {
        query <- gsub("\\(\\?column_names\\) ", "", query)
      }
    } else {
      query <- gsub("\\?column_names", "*", query)
    }
  }
  
  # Safely escape values
  if (all(!is.null(value_names))) {
    if (all.equal(length(value_names),
                  max(sapply(values, length)),
                  min(sapply(values, length)))) {
      column_names <- value_names
    }
  }
  values_formatted <- switch(
    action,
    "INSERT" = paste0("(",
                      lapply(values,
                             function(x) {
                               lapply(x,
                                      function(y) {
                                        if (any(tolower(y) == "null", y == "", is.na(y), is.null(y))) {
                                          "null"
                                        } else if (str_detect(y, "\\\\")) {
                                          dbQuoteString(db_conn, str_replace_all(y, "\\\\", "\\\\\\\\"))
                                        } else {
                                          dbQuoteLiteral(db_conn, y)
                                        }
                                      }) %>%
                                 unlist() %>%
                                 paste0(collapse = ", ")
                             }),
                      ")",
                      collapse = ", "),
    "UPDATE" = paste0(lapply(values,
                             function(x) {
                               lapply(names(x),
                                      function(y) {
                                        val <- x[[y]]
                                        if (is.na(val) || is.null(val) || tolower(val) == "null" || val == "") {
                                          val <- "null"
                                        }
                                        sprintf("%s = %s",
                                                dbQuoteIdentifier(db_conn, y),
                                                dbQuoteLiteral(db_conn, val)
                                        )
                                      }) %>%
                                 unlist() %>%
                                 paste0(collapse = ", ")
                             }),
                      collapse = ", "),
    ""
  )
  query <- str_replace(query, "\\?values", values_formatted)
  
  # Safely escape column names
  if (length(column_names) > 0) {
    query <- str_replace(query,
                         "\\?column_names",
                         paste0(lapply(column_names,
                                       function(x) {
                                         dbQuoteIdentifier(db_conn, x)
                                       }),
                                collapse = ", "
                         )
    )
  } else {
    query <- str_remove(query, "\\(\\?column_names\\) ")
  }
  
  # Safely escape table names
  query <- str_replace(query,
                       "\\?table_name",
                       dbQuoteIdentifier(db_conn, table_name)
  )
  
  # Safely escape where clauses
  if (all(!is.null(match_criteria), !action == "INSERT")) {
    if (fuzzy && case_sensitive) {
      if (logging) {
        log_it("warn",
               "Fuzzy and case sensitive queries cannot be combined...defaulting to fuzzy, case insensitive query.",
               log_ns)
      }
      case_sensitive <- FALSE
    }
    if (!is.list(match_criteria)) match_criteria <- as.list(match_criteria)
    query <- paste(query, "WHERE",
                   clause_where(db_conn        = db_conn,
                                table_names    = table_name,
                                case_sensitive = case_sensitive,
                                fuzzy          = fuzzy,
                                match_criteria = match_criteria,
                                and_or         = and_or))
  } else if (all(is.null(match_criteria), !action %in% c("NROW", "INSERT", "SELECT"))) {
    stop('Only "NROW", "INSERT", and "SELECT" actions are valid when argument "match_criteria" is not provided.')
  }
  
  if (action == "SELECT") {
    # Safely escape group by clause
    if (!is.null(group_by)) {
      query <- paste(query, "GROUP BY", dbQuoteIdentifier(db_conn, group_by))
    }
    
    # Safely escape order by clause
    if (!is.null(order_by)) {
      ordering <- paste0(
        lapply(names(order_by),
               function(x) {
                 paste0(dbQuoteIdentifier(db_conn, x),
                        " ",
                        order_by[[x]])
               }),
        collapse = ", "
      )
      query <- paste(query, "ORDER BY", ordering) %>%
        str_trim()
    }
    
    # Safely escape limit clause
    if (!is.null(limit)) {
      if (!is.numeric(limit)) {
        limit <- as.integer(limit)
        limit <- limit[!is.na(limit)]
      }
      if (any(length(limit) == 0, length(limit) > 1)) {
        stop('Exactly one value must be provided for the "limit" parameter.')
      } else {
        query <- paste(query, "LIMIT", dbQuoteLiteral(db_conn, limit))
      }
    }
  }
  
  if (action == "DELETE") {
    select_version <- str_replace(query, "^DELETE FROM", "SELECT * FROM")
    if (execute) {
      rows_affected <- nrow(dbGetQuery(db_conn, select_version))
      msg <- sprintf("Your constructed action '%s' %s.",
                  query,
                  ifelse(rows_affected > 0,
                         sprintf("will delete %s %s",
                                 rows_affected,
                                 ifelse(rows_affected == 1, "row", "rows")),
                         "does not match any rows"))
      if (logging) log_it("info", msg, log_ns)
      if (interactive() && exists("LOGGING_ON") && !LOGGING_ON) {
        cat(msg, "\n")
      }
      if (rows_affected > 0) {
        if (interactive()) {
          confirm <- select.list(choices   = c("CONFIRM", "abort"),
                                 preselect = "abort",
                                 multiple  = FALSE,
                                 title     = "Please confirm.")
          if (!confirm == "CONFIRM") {
            if (logging) log_it("info", "Delete query construction aborted.", log_ns)
            query <- NA
          } else {
            if (logging) log_it("info", "Delete query confirmed by user.", log_ns)
          }
        } else {
          if (logging) log_it("warn", "Delete statement issued from non-interactive session.", log_ns)
        }
      }
    }
  }
  
  if (is.na(query)) return(NULL)

  catch_null <- " = '*(NULL|null|NA|na)'*| = ''"
  if (str_detect(query, catch_null)) {
    query <- query %>%
      str_replace_all(catch_null, " is null")
  }
  query <- sql(query)
  if (execute) {
    if (logging) log_it("trace", glue::glue("Executing: {query}."), log_ns)
    if (grepl("^SELECT", query)) {
      tmp <- dbGetQuery(db_conn, query)
      if (all(ncol(tmp) == 1, single_column_as_vector)) tmp <- tmp[, 1]
    } else {
      tmp <- dbSendStatement(db_conn, query)
      dbClearResult(tmp)
    }
    return(tmp)
  } else {
    return(query)
  }
}


#' Match multiple values in a database table
#'
#' Complex queries are sometimes necessary to match against multiple varied
#' conditions across multiple items in a list or data frame. Call this function
#' to apply vectorization to all items in `match_criteria` and create a fully
#' qualified SQL expression using [clause_where] and execute that query against
#' the database connection in `db_conn`. Speed is not optimized during the call
#' to clause where as each clause is built independently and joined together
#' with "OR" statements.
#'
#' This is intended for use with a data frame object
#'
#' @inheritParams clause_where
#'
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#'
#' @return data.frame of the matching database rows
#' @export
#' 
dataframe_match <- function(match_criteria,
                            table_names,
                            and_or = "AND",
                            db_conn = con,
                            log_ns = "db") {
  if (is.data.frame(match_criteria)) {
    match_criteria <- purrr::transpose(match_criteria)
  }
  sprintf("select * from %s where (%s)",
          table_names,
          match_criteria %>%
            lapply(function(x) {
              clause_where(
                db_conn = db_conn,
                table_names = table_names,
                match_criteria = x,
                and_or = and_or
              )
            }) %>%
            paste0(collapse = ") OR (")
  ) %>%
    dbGetQuery(db_conn, .)
}
