# WIP
db_table_mod <- function(con, type, table, properties) {
  # Argument validation
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        con        = list(c("class", "SQLiteConnection")),
        type       = list(c("mode", "character"), c("length", 1)),
        table      = list(c("mode", "character"), c("length", 1)),
        properties = list(c("mode", "data.frame"))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  type <- arg.match("insert", "update")
  if (!table %in% dbListTables(con)) {
    stop(glue::glue('Table "{table}" does not exist.'))
  }
  needed   <- dbGetQuery(con, glue::glue("PRAGMA table_info({table})"))
  provided <- names(properties)
  if (!all(provided %in% needed)) {
    stop(glue::glue('Cannot rectify all names in argument "properties" with database headers for table "{table}".
                     Provided {paste0(provided, collapse = ", ")}.
                     Needed {paste0(needed, collapse = ", ")}.'))
  }
}

#' Get table definition from SQLite
#'
#' Given a database connection (`con`). Get more information about the
#' properties of (a) database table(s) directly from `PRAGMA table_info()`
#' rather than e.g. `DBI::dbListFields`. Set `get_sql` to `TRUE` to include the
#' direct schema using sqlite_master; depending on formatting this may or may
#' not be directly usable though some effort has been made to remove formatting
#' characters (e.g. line feeds, tabs, etc) if stringr is available.
#'
#' Note that the package `stringr` is required for formatting returns that
#' include either `get_sql` or `pretty` as TRUE.
#'
#' @param con connection object, specifically of class "SQLiteConnection" but
#'   not enforced
#' @param table CHR vector name of the table(s) to inspect
#' @param get_sql BOOL scalar of whether or not to return the schema sql
#'   (default FALSE)
#' @param pretty BOOL scalar for whether to return "pretty" SQL that includes
#'   human readability enhancements; if this is set to TRUE (the default), it is
#'   recommended that the output is fed through `cat` and, in the case of multiple tables
#'
#' @return data.frame object representing the SQL PRAGMA expression
#' @export
#'
#' @examples
pragma_table_def <- function(con, table, get_sql = FALSE, pretty = TRUE) {
  require(dplyr)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        con     = list(c("length", 1)),
        table   = list(c("mode", "character"), c("n>=", 1)),
        get_sql = list(c("mode", "logical"), c("length", 1)),
        pretty  = list(c("mode", "logical", c("length", 1)))
      ),
      from_fn   = "pragma_table_def"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Define function scope
  func <- ifelse(get_sql,
                 "select name, type, sql from sqlite_master where name = '%s'",
                 "PRAGMA table_info('%s')"
  )
  # Basic return
  out <- lapply(table,
                function(x) {
                  tmp <- dbGetQuery(con, sprintf(func, x))
                  tmp$table <- x
                  return(tmp)
                }
  )
  # Shape up the return
  if (get_sql) {
    out <- dplyr::bind_rows(out) %>%
      select(table, type, sql)
    if ("stringr" %in% installed.packages()) {
      out$sql <- out$sql %>%
        stringr::str_replace_all(" {2,5}", " ")
        # stringr::str_replace_all("\\s+", " ")
      if (!pretty) {
        out$sql <- stringr::str_replace_all(out$sql, "\n|\t", " ")
      }
    } else {
      warning("Package 'stringr' is required for better formatting of SQL returns.")
    }
  } else {
    out <- dplyr::bind_rows(out) %>%
      select(cid, table, name:pk)
  }
  # Return data frame object representing SQLite table schema(s)
  return(out)
}

#' Explore properties of an SQLite table
#'
#' Add functionality to `pragma_table_def` by filtering on column properties
#' such as required and primary key fields. This provides some flexibility to
#' searching table properties without sacrificing the full details of table
#' schema. Parameter `get_sql` is forced to FALSE; only information available
#' via PRAGMA is searched by this function.
#'
#' This is intended to support validation during database communications with an
#' SQLite connection, especially for application (e.g. `shiny` development) by
#' allowing for programmatic inspection of datbase columns by name and property.
#'
#' @param db_conn connection object, specifically of class "SQLiteConnection"
#'   but not strictly enforced
#' @param table CHR vector name of the table(s) to inspect
#' @param condition CHR vector matching specific checks, must be one of
#'   c("required", "has_default", "is_PK") for constraints where a field must
#'   not be null, has a default value defined, and is a primary key field,
#'   respectively.
#' @param name_like CHR vector of character patterns to match against column
#'   names via grep. If length > 1, will be collapsed to a basic OR regex (e.g.
#'   c("a", "b") becomes "a|b"). As regex, abbreviations and wildcards will
#'   typically work, but care should be used in that case.
#' @param data_type CHR vector of character patterns to match against column
#'   data types via grep. If length > 1 will be collapsed to a basic OR regex
#'   (e.g. c("int", "real") becomes "int|real"). As regex, abbreviations and
#'   wildcards will typically work, but care should be used in that case.
#' @param all_columns LGL scalar
#' @param names_only
#'
#' @return
#' @export
#'
#' @examples
pragma_table_info <- function(db_conn,
                              table,
                              condition   = NULL,
                              name_like   = NULL,
                              data_type   = NULL,
                              all_columns = TRUE,
                              names_only  = FALSE) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_conn, table, all_columns, names_only),
      conditions = list(
        db_conn     = list(c("length", 1)),
        table       = list(c("mode", "character"), c("n>=", 1)),
        all_columns = list(c("mode", "logical"), c("length", 1)),
        names_only  = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn   = "pragma_table_def"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Ensure table exists
  if (!table %in% dbListTables(db_conn)) stop(sprintf('No table named "%s" was found in this schema.', table))
  # Get table properties
  out <- pragma_table_def(db_conn, table, get_sql = FALSE)
  # Set up condition checks
  valid_conditions <- c("required", "has_default", "is_PK")
  if (!is.null(condition)) {
    condition <- match.arg(condition, valid_conditions, several.ok = TRUE)
  }
  fns <- list(
    function(out) which(out$notnull == 1),
    function(out) which(!is.na(out$dflt_value)),
    function(out) which(out$pk == 1)
  )
  names(fns) <- valid_conditions
  check_columns <- c("notnull", "dflt_value", "pk")
  # Define which rows and columns to return
  out_rows <- 1:nrow(out)
  out_check <- integer(0)
  if (all_columns) {
    check_columns <- names(out)
  } else {
    check_columns <- check_columns[which(names(fns) %in% c(check_columns, condition))]
  }
  # Limit column outputs?
  limit <- FALSE
  # Do condition checks
  if (!is.null(condition)) {
    for (check in condition) {
      out_check <- c(out_check, fns[[check]](out))
    }
    limit <- TRUE
  }
  if (!is.null(name_like)) {
    name_like <- paste0(name_like, collapse = "|")
    out_check <- c(out_check, grep(name_like, out$name))
    check_columns <- c("name", check_columns)
    limit <- TRUE
  }
  if (!is.null(data_type)) {
    data_type <- paste0(data_type, collapse = "|")
    out_check <- c(out_check, grep(toupper(data_type), out$type))
    check_columns <- c("type", check_columns)
    limit <- TRUE
  }
  # Shape up the return
  if (limit) out_rows <- unique(out_check)
  out_cols <- unique(c("table", "name", check_columns))
  out <- out[out_rows, ]
  if (names_only) {
    out <- out$name
  } else {
    out <- out[, out_cols]
  }
  # Return data frame object representing PRAGMA table_info columns matching the
  # requested properties
  return(out)
}
