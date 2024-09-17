# Database operations and inspection -------------------------------------------

#' Get table definition from SQLite
#'
#' Given a database connection (`con`). Get more information about the
#' properties of (a) database table(s) directly from `PRAGMA table_info()`
#' rather than e.g. [DBI::dbListFields()]. Set `get_sql` to `TRUE` to include
#' the direct schema using sqlite_master; depending on formatting this may or
#' may not be directly usable though some effort has been made to remove
#' formatting characters (e.g. line feeds, tabs, etc) if stringr is available.
#'
#' Note that the package `stringr` is required for formatting returns that
#' include either `get_sql` or `pretty` as TRUE.
#'
#' @param db_table CHR vector name of the table(s) to inspect
#' @param db_conn connection object (default: con)
#' @param get_sql BOOL scalar of whether or not to return the schema sql
#'   (default FALSE)
#' @param pretty BOOL scalar for whether to return "pretty" SQL that includes
#'   human readability enhancements; if this is set to TRUE (the default), it is
#'   recommended that the output is fed through `cat` and, in the case of
#'   multiple tables
#'
#' @return data.frame object representing the SQL PRAGMA expression
#' @export
#'
pragma_table_def <- function(db_table, db_conn = con, get_sql = FALSE, pretty = TRUE) {
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace",
           sprintf('Getting table definition for "%s".', format_list_of_names(db_table)),
           log_ns = "db")
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table = list(c("mode", "character"), c("n>=", 1)),
        db_conn  = list(c("length", 1)),
        get_sql  = list(c("mode", "logical"), c("length", 1)),
        pretty   = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "pragma_table_def"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  # Ensure table exists
  db_table <- resolve_table_name(db_table, db_conn)
  
  # Define function scope
  func <- ifelse(get_sql,
                 "select name, type, sql from sqlite_master where name = '%s'",
                 "PRAGMA table_xinfo('%s')"
  )
  # Basic return
  out <- lapply(db_table,
                function(x) {
                  tmp <- dbGetQuery(db_conn, sprintf(func, x))
                  tmp$table_name <- x
                  unique_constraints <- dbGetQuery(con, sprintf("pragma index_list('%s')", x)) %>%
                    filter(unique == 1) %>%
                    pull(name)
                  if (length(unique_constraints) > 0) {
                    unique_constraints <- lapply(unique_constraints,
                                                 function(x) {
                                                   dbGetQuery(con, sprintf("pragma index_info('%s')", x))
                                                 }) %>%
                      bind_rows() %>%
                      pull(name)
                  }
                  tmp <- tmp %>%
                    mutate(unique = name %in% unique_constraints)
                  return(tmp)
                }
  )
  # Shape up the return
  if (get_sql) {
    out <- dplyr::bind_rows(out) %>%
      dplyr::select(table_name, type, sql)
    if ("stringr" %in% installed.packages()) {
      if (!pretty) {
        out$sql <- stringr::str_replace_all(out$sql, "\n|\t", " ")
      }
      out$sql <- out$sql %>%
        stringr::str_replace_all(" {2,5}", " ")
    } else {
      warning("Package 'stringr' is required for better formatting of SQL returns.")
    }
  } else {
    out <- dplyr::bind_rows(out) %>%
      dplyr::select(cid, table_name, name:unique)
  }
  if (exists("log_it")) log_fn("end")
  # Return data frame object representing SQLite db_table schema(s)
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
#' @param db_table CHR vector name of the table(s) to inspect
#' @param db_conn connection object (default: con)
#' @param condition CHR vector matching specific checks, must be one of
#'   c("required", "has_default", "is_PK") for constraints where a field must
#'   not be null, has a default value defined, and is a primary key field,
#'   respectively. (default: NULL)
#' @param name_like CHR vector of character patterns to match against column
#'   names via grep. If length > 1, will be collapsed to a basic OR regex (e.g.
#'   c("a", "b") becomes "a|b"). As regex, abbreviations and wildcards will
#'   typically work, but care should be used in that case. (default: NULL)
#' @param data_type CHR vector of character patterns to match against column
#'   data types via grep. If length > 1 will be collapsed to a basic "OR" regex
#'   (e.g. c("int", "real") becomes "int|real"). As regex, abbreviations and
#'   wildcards will typically work, but care should be used in that case.
#'   (default: NULL)
#' @param include_comments LGL scalar of whether to include comments in the
#'   return data frame (default: FALSE)
#' @param names_only LGL scalar of whether to include names meeting defined
#'   criteria as a vector return value  (default: FALSE)
#'
#' @return data.frame object describing the database entity
#' @export
#'
#' @usage pragma_table_info("compounds")
pragma_table_info <- function(db_table,
                              db_conn          = con,
                              condition        = NULL,
                              name_like        = NULL,
                              data_type        = NULL,
                              include_comments = FALSE,
                              names_only       = FALSE) {
  # Argument validation relies on verify_args
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace", glue('Getting table definition for "{db_table}".'), "db")
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_table, db_conn, include_comments, names_only),
      conditions = list(
        db_table         = list(c("mode", "character"), c("n>=", 1)),
        db_conn          = list(c("length", 1)),
        include_comments = list(c("mode", "logical"), c("length", 1)),
        names_only       = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn   = "pragma_table_info"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  # Ensure table exists
  db_table <- resolve_table_name(db_table, db_conn)
  # Bail early for performance if names only
  if (names_only) {
    return(DBI::dbListFields(db_conn, db_table))
  }
  # Get table properties
  out <- pragma_table_def(db_table = db_table, db_conn = db_conn, get_sql = FALSE)
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
  # Limit column outputs?
  limit <- lapply(c(condition, name_like, data_type),
                  function(x) !is.null(x)) %>%
    unlist() %>%
    any()
  # Get comments if any
  if (include_comments) {
    tmp <- pragma_table_def(db_table = db_table, db_conn = db_conn, get_sql = TRUE) %>%
      tidy_comments() %>%
      bind_rows()
    out <- out %>%
      bind_cols(tmp) %>%
      relocate(table_comment, .after = table_name)
  }
  # Do condition checks
  if (!is.null(condition)) {
    for (check in condition) {
      out_check <- c(out_check, fns[[check]](out))
    }
  }
  if (!is.null(name_like)) {
    name_like <- paste0(name_like, collapse = "|")
    out_check <- c(out_check, grep(name_like, out$name))
  }
  if (!is.null(data_type)) {
    data_type <- paste0(data_type, collapse = "|")
    out_check <- c(out_check, grep(toupper(data_type), out$type))
  }
  # Shape up the return
  if (limit) out_rows <- out_check %>% unique() %>% sort()
  out <- out[out_rows, ]
  # Return data frame object representing PRAGMA table_info columns matching the
  # requested properties
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Build or rebuild the database from scratch
#'
#' This function will build or rebuild the NIST HRAMS database structure from
#' scratch, removing the existing instance. By default, most parameters are set
#' in the environment (at "./config/env_glob.txt") but any values can be passed
#' directly. This can be used to quickly spin up multiple copies with a clean
#' slate using different build files, data files, or return to the last stable
#' release.
#'
#' If sqlite3 and its command line interface are available on your platform,
#' that will be used (preferred method) but, if not, this function will read in
#' all the necessary files to directly create it using shell commands. The shell
#' method may not be universally applicable to certain compute environments or
#' may require elevated permissions.
#'
#' @param db CHR scalar of the database name (default: session value DB_NAME)
#' @param build_from CHR scalar of a SQL build script to use (default:
#'   environment value DB_BUILD_FILE)
#' @param populate LGL scalar of whether to populate with data from the file in
#'   `populate_with` (default: TRUE)
#' @param populate_with CHR scalar for the populate script (e.g.
#'   "populate_demo.sql") to during after the build is complete; (default:
#'   session value DB_DATA); ignored if `populate = FALSE`
#' @param archive LGL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default: FALSE), passed to [`remove_db()`]
#' @param sqlite_cli CHR scalar to use to look for installed sqlite3 CLI tools
#'   in the current system environment (default: session value SQLITE_CLI)
#' @param connect LGL scalar of whether or not to connect to the rebuilt
#'   database in the global environment as object `con`` (default: FALSE)
#'
#' @return None, check console for details
#' @export
#'
#' @usage build_db(db = "test_db.sqlite", db_conn_name = "test_conn")
build_db <- function(db            = DB_NAME,
                     build_from    = DB_BUILD_FILE,
                     populate      = TRUE,
                     populate_with = DB_DATA,
                     archive       = FALSE,
                     sqlite_cli    = SQLITE_CLI,
                     db_conn_name  = DB_CONN_NAME,
                     connect       = FALSE) {
  logger <- exists("log_it")
  if (logger) {
    log_fn("start")
    log_it("trace", glue::glue('Starting build of "{db}".'), "db")
  }
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db            = list(c("mode", "character"), c("length", 1), "not_empty"),
        build_from    = list(c("mode", "character"), c("length", 1), "not_empty"),
        populate      = list(c("mode", "logical"), c("length", 1), "not_empty"),
        populate_with = list(c("mode", "character"), c("length", 1)),
        archive       = list(c("mode", "logical"), c("length", 1), "not_empty"),
        sqlite_cli    = list(c("mode", "character"), c("length", 1)),
        connect       = list(c("mode", "logical"), c("length", 1), "not_empty")
      ),
      from_fn    = "build_db"
    )
    stopifnot(arg_check$valid)
  }
  if (tools::file_ext(build_from) != "sql") {
    stop("Only files with the .sql extension are accepted.")
  } else {
    build_file <- ifelse(file.exists(build_from),
                         build_file,
                         list.files(pattern = build_from,
                                    full.names = TRUE,
                                    recursive = TRUE)
    )
  }
  if (length(build_file) == 0) {
    stop(sprintf('Could not find build file "%s" in this directory.',
                 build_from))
  } else if (length(build_file) > 1) {
    msg <- sprintf('Multiple files match "%s" in this directory.')
    if (interactive()) {
      if (logger) log_it("warn", glue::glue("{msg} Querying user to choose..."))
      use_file <- select.list(
        choices = c("(Abort)", build_file),
        title = sprintf("Please choose a single build file or abort.")
      )
      if (use_file == "(Abort)") {
        if (logger) log_it("warn", "Database build aborted by user.")
        stop("Database build aborted by user (build only supported from a single sql file).")
      } else {
        build_file <- use_file
      }
    } else {
      stop(sprintf('Multiple files match "%s" in this directory (%s). Argument "build_from" must apply only to a single file.',
                   build_from,
                   format_list_of_names(build_file)))
    }
  }
  if (logger) log_it("trace", glue('Attempting to connect to "{db}".'))
  manage_connection(db = db, conn_name = db_conn_name, reconnect = FALSE, disconnect = TRUE)
  # Populate with all data sources
  # -- RSQLite does not allow for the CLI [.DOT] commands
  # -- If sqlite3 CLI is installed, use that by preference
  sqlite_available <- length(Sys.which(sqlite_cli) > 1)
  # Do it the short way (with CLI)
  if (sqlite_available) {
    if (logger) log_it("trace", glue::glue('SQLite CLI under alias "{sqlite_cli}" is available. Building directly...'), "db")
    sqlite_call <- glue::glue('{sqlite_cli} {db}')
    if (!file.exists(build_file)) {
      stop(glue::glue('Cannot locate file "{build_file}" in this directory.'))
    }
    build_cmd   <- glue::glue('{sqlite_call} -cmd ".read {build_file}" -cmd ".exit"')
    if (file.exists(db)) remove_db(db = db, archive = archive)
    if (logger) log_it("info", glue::glue('Building database "{db}".'), "db")
    if (logger) log_it("trace", glue::glue('Issuing shell command to build the database as\n{build_cmd}'), "db")
    if (.Platform$OS.type == "windows") {
      shell(build_cmd)
    } else {
      system(build_cmd)
    }
    if (populate) {
      populate_file <- ifelse(file.exists(populate_with),
                              populate_with,
                              list.files(pattern = populate_with, full.names = TRUE, recursive = TRUE)
      )
      if (!file.exists(populate_file)) {
        if (logger) log_it("warn", glue::glue('Cannot locate file "{populate_file}" in this directory; "{db}" will be created but not populated.'), "db")
        populate_cmd <- ""
      } else {
        if (logger) log_it("info", glue::glue('Populating from "{populate_file}".'), "db")
        populate_cmd <- glue::glue('{sqlite_call} -cmd ".read {populate_file}" -cmd ".exit"')
        if (logger) log_it("trace", glue::glue('Issuing shell command to populate the database as\n{populate_cmd}'), "db")
        if (.Platform$OS.type == "windows") {
          shell(populate_cmd)
        } else {
          system(populate_cmd)
        }
      }
    }
    if (logger) log_it("info", glue::glue('Finished attempted build of "{db}" as specified. Check console for any failure details.'), "db")
  } else {
    # Do it the long way
    if (logger) log_it("trace", glue::glue('SQLite CLI under alias "{sqlite_cli}" is not available. Building through R...'), "db")
    # -- Ensure packages are available
    reqs <- c("stringr", "magrittr", "readr", "DBI")
    packs_available <- reqs %in% installed.packages()
    if (!all(packs_available)) {
      stop("Some packages were not available. Please install packages:", paste0(reqs[!packs_available]))
    }
    invisible(lapply(reqs, require, character.only = TRUE))
    # -- Remove the existing database
    if (file.exists(db)) {
      if (logger) log_it("trace", glue::glue('Removing "{db}".'))
      remove_db(db = db, archive = archive)
    }
    # -- Create the build commands in R to pass through RSQLite::SQLite()
    if (logger) log_it("trace", "Creating build statements.", "db")
    build_statement <- create_fallback_build(build_file)
    build_statement <- build_statement[nchar(build_statement) > 1]
    # -- Create the database and read in the build statements
    if (logger) log_it("trace", glue::glue('Creating new database as "{db}".'), "db")
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    if (logger) log_it("trace", glue('Building "{db}"...'), "db")
    invisible(
      lapply(build_statement,
             function(x) DBI::dbSendStatement(con, str_trim(x, "both")))
    )
    # Disconnect
    DBI::dbDisconnect(con)
  }
  if (connect) {
    if (logger) log_it("trace", glue::glue('Connecting to "{db}".'), "db")
    manage_connection(db = db, conn_name = db_conn_name, reconnect = TRUE)
  }
  if (logger) log_fn("end")
}

#' Remove an existing database
#'
#' This is limited to only the current working directory and includes its
#' subdirectories. If you wish to retain a copy of the prior database, ensure
#' argument `archive = TRUE` (note the default is FALSE) to create a copy of the
#' requested database prior to rebuild; this is created in the same directory as
#' the found database and appends
#'
#' @param db CHR scalar name of the database to build (default: session value
#'   DB_NAME)
#' @param archive LGL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default: FALSE)
#'
#' @return None, check console for details
#' @export
#'
#' @usage remove_db("test.sqlite", archive = TRUE)
remove_db <- function(db = DB_NAME, archive = FALSE) {
  require(tools)
  logger <- exists("log_it")
  if (logger) {
    log_fn("start")
    log_it("trace", glue::glue('Starting removal of "{db}"...'), "db")
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db, archive),
      conditions = list(
        db      = list(c("mode", "character"), c("length", 1)),
        archive = list(c("mode", "logical"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Resolve database file location
  if (logger) log_it("trace", glue::glue('Finding database file "{db}"'), "db")
  db_path  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
  db_path  <- db_path[basename(db_path) == db]
  if (length(db_path) == 0) {
    if (logger) log_it("error", sprintf('Database "%s" does not exist in this directory tree.', db), "db")
    return(NULL)
  } else if (length(db_path) > 1) {
    if (logger) log_it("warn", glue('Multiple files found for "{db}" in this directory.'), "db")
    correct_path <- resolve_multiple_values(db_path, db)
  }
  if (length(db_path) == 1) {
    # Ensure no current connection from R
    # check <- try(manage_connection(db = db, reconnect = FALSE, disconnect = TRUE))
    # if (class(check) == "try-error") stop("Unable to automatically stop the connection to '", db, "'.")
    close_up_shop()
    if (archive) {
      now       <- format(Sys.time(), "%Y%m%d%H%M%S%Z")
      fname     <- file_path_sans_ext(db)
      new_fname <- gsub(fname,
                        sprintf("%s_archive_%s",fname, now),
                        db)
      file.copy(db_path, new_fname)
      if (logger) log_it("success", sprintf('Archive created as "%s"', new_fname), "db")
    }
    result <- try(file.remove(db_path))
    if (class(result) == "try-error") {
      if (logger) log_it("error", sprintf('Database "%s" could not be removed; another connection is likely open.', db), "db")
    } else {
      if (logger) log_it("success", sprintf('Database "%s" removed.', db), "db")
    }
  }
  if (logger) log_fn("end")
}

#' Check presence of a database table
#'
#' This convenience function checks for the existence of one or more `db_table`
#' objects in a database.
#'
#' @param db_table CHR vector of table names to check
#' @param db_conn connection object (default: con)
#' @param log_ns CHR scalar of the namespace (if any) to use for logging
#'   (default: "db")
#'
#' @return CHR vector of existing tables
#' @export
#'
#' @usage resolve_table_name(db_table = "compounds", db_conn = "test_con")
resolve_table_name <- function(db_table, db_conn = con, log_ns = "db") {
  can_log <- exists("log_it")
  if (can_log) log_fn("start")
  stopifnot(active_connection(db_conn))
  tables_exist <- db_table %in% dbListTables(db_conn)
  if (!all(tables_exist)) {
    missing <- db_table[!tables_exist]
    if (can_log) {
      log_it("warn", glue::glue('No table{ifelse(length(missing) > 1, "s", "")} named {gsub("and", "or", format_list_of_names(missing, add_quotes = TRUE))} {ifelse(length(missing) > 1, "were", "was")} found in this schema.'), log_ns)
    }
    db_table <- db_table[tables_exist]
  }
  if (length(db_table) == 0) {
    return(NULL)
  }
  if (exists("log_it")) log_fn("end")
  return(db_table)
}

#' Create a data dictionary
#'
#' Get a list of tables and their defined columns with properties, including
#' comments, suitable as a data dictionary from a connection object amenable to
#' [odbc::dbListTables]. This function relies on [pragma_table_info].
#'
#' @param db_conn connection object (default:con)
#'
#' @return LIST of length equal to the number of tables in `con` with attributes
#'   identifying which tables, if any, failed to render into the dictionary.
#' @export
#'
#' @usage data_dictionary(db_conn = con)
data_dictionary <- function(db_conn = con) {
  if (exists("log_it")) log_fn("start")
  # Check connection
  stopifnot(active_connection(db_conn))
  tabls <- dbListTables(db_conn)
  tabls <- tabls[-grep("sqlite_", tabls)]
  out <- vector('list', length(tabls))
  names(out) <- tabls
  failures <- character(0)
  if (exists("log_it")) log_it("info", "Adding tables to the dictionary...one moment please...", "db")
  for (tabl in tabls) {
    tmp <- try(pragma_table_info(db_table = tabl,
                                 db_conn = con,
                                 include_comments = TRUE))
    if (class(tmp) == "try-error") {
      if (exists("log_it")) log_it("warn", sprintf('Dictionary failure on "%s"', tabl), "db")
      failures <- c(failures, tabl)
    } else {
      if (exists("log_it")) log_it("trace", sprintf('"%s" added to dictionary', tabl), "db")
    }
    out[[tabl]] <- tmp
  }
  if (length(failures) > 0) {
    has_failures <- TRUE
    msg <- sprintf("Dictionary was not available for %s: %s",
                   ifelse(length(failures) > 1, "tables", "table"),
                   format_list_of_names(failures))
    if (exists("log_it")) {
      log_it("warn", msg, "db")
    } else {
      cat("WARN", has_failues, "\n")
    }
  } else {
    has_failures <- FALSE
    failures <- "Dictionary available for all tables."
    if (exists("log_it")) log_it("success", failures, "db")
  }
  attr(out, "has_failures") <- has_failures
  attr(out, "failures")     <- failures
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Tidy up table and field comments
#'
#' Creates more human-readable outputs after extracting the raw SQL used to
#' build entities and parsing out the comments as identified with the /* ... */
#' multi-line comment flag pair. Single line comments are not extracted. The
#' first comment is assumed to be the table comment. See examples in the
#' `config/sql_nodes` directory.
#'
#' @param obj result of calling [pragma_table_def] with `get_sql` = TRUE
#'
#' @return LIST of length equal to `obj` containing extracted comments
#' @export
#'
#' @usage tidy_comments(pragma_table_def("compounds", get_sql = TRUE))
tidy_comments <- function(obj) {
  if (exists("log_it")) log_fn("start")
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = list(obj),
      conditions = list(
        obj      = list(c("mode", "data.frame"))
      )
    )
    stopifnot(arg_check$valid)
  }
  if (!all(c("sql", "table_name") %in% names(obj))) {
    if (exists("log_it")) log_it("error", 'Object must contain both "sql" and "table_name" entries. Returning as provided.', "db")
    out <- obj
  } else {
    comments <- obj$sql %>%
      str_remove_all("/\\* (Check constraints|Foreign key relationships) \\*/") %>%
      str_extract_all("/\\* [[:alnum:][:punct:]\\[\\]\\+ ]+[[:alnum:][:punct:] ]\\*/") %>%
      lapply(str_remove_all, "/\\* *| *\\*/")
    table_comments <- lapply(comments, function(x) x[1])
    field_comments <- lapply(comments, function(x) x[-1])
    out <- vector('list', nrow(obj))
    for (i in 1:length(out)) {
      out[[i]]$table_comment  <- table_comments[[i]]
      out[[i]]$field_comments <- field_comments[[i]]
    }
    names(out) <- obj$db_table
  }
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Save the current data dictionary to disk
#'
#' Executes [data_dictionary()] and saves the output to a local file. If \code{output_format}
#' is one of "data.frame" or "list", the resulting file will be saved as an RDS.
#' Parameter \code{output_file} will be used during the save process; relative paths
#' will be identified by the current working directory.
#'
#' @param db_conn connection object (default: con)
#' @param output_format CHR scalar, one of (capitalization insensitive) "json",
#'   "csv", "data.frame", or "list" (default "json")
#' @param output_file CHR scalar indicating where to save the resulting file; an
#'   appropriate file name will be constructed if left NULL (default: NULL)
#' @param overwrite_existing LGL scalar indicating whether to overwrite an
#'   existing file whose name matches that determined from `output_file`
#'   (default: TRUE); file names will be appended with "(x)" sequentially if
#'   this is FALSE and a file with matching name exists.
#'
#' @return None, saves a file to the current working directory
#' @export
#'
#' @usage save_data_dictionary(db_conn = con)
save_data_dictionary <- function(db_conn            = con,
                                 output_format      = "json",
                                 output_file        = NULL,
                                 overwrite_existing = TRUE) {
  if (exists("log_it")) log_fn("start")
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = list(output_format, overwrite_existing),
      conditions = list(
        output_format      = list(c("mode", "character"), c("length", 1)),
        overwrite_existing = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "build_db"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  output_format <- tolower(output_format)
  output_format <- match.arg(output_format,
                             c("json", "csv", "data.frame", "list"))
  if (!output_format %in% c("json", "csv")) {
    f_ext <- "RDS"
  } else {
    f_ext <- output_format
  }
  if (is.null(output_file)) {
    can_construct <- sapply(c("DB_TITLE", "DB_VERSION", "DICT_FILE_NAME"), exists)
    if (!all(can_construct)) {
      if (exists("log_it")) log_it("warn", "Not enough values provided to construct a file name. Using defaults.", "db")
      if (!exists("DB_TITLE"))       DB_TITLE <- "hrams_database"
      if (!exists("DB_VERSION"))     DB_VERSION <- NULL
      if (!exists("DICT_FILE_NAME")) DICT_FILE_NAME <- "dictionary"
    }
    f_name <- file.path(getwd(),
                        sprintf("%s.%s",
                                paste0(c(DB_TITLE,
                                         DB_VERSION,
                                         DICT_FILE_NAME), collapse = "_"),
                                f_ext)
    )
  } else {
    f_name <- output_file
  }
  out <- data_dictionary(db_conn)
  if (attr(out, "has_failures")) {
    if (exists("log_it")) {
      log_it("error",
             sprintf('This dictionary failed on %s. No file will be saved.',
                     format_list_of_names(attr(out, "failures"))),
             "db")
    }
  } else {
    if (is.null(output_file)) {
      if (exists("log_it")) {
        log_it("warn",
               sprintf('No file name provided to "output_file", saving as "%s".',
                       f_name),
               "db")
      }
    }
    i <- 0
    if (all(file.exists(f_name), overwrite_existing)) {
      file.remove(f_name)
    }
    while (file.exists(f_name)) {
      i <- i + 1
      f_name <- str_replace(f_name,
                            sprintf("%s%s.%s", 
                                    DICT_FILE_NAME,
                                    ifelse(i == 1, "",
                                           sprintf(" (%s)", i - 1)),
                                    f_ext),
                            sprintf("%s (%s).%s",
                                    DICT_FILE_NAME,
                                    i,
                                    f_ext)
      )
    }
    switch(output_format,
           "json"       = out %>%
             jsonlite::toJSON() %>%
             readr::write_file(f_name),
           "csv"        = out %>%
             dplyr::bind_rows() %>%
             readr::write_csv(f_name),
           "data.frame" = out %>%
             dplyr::bind_rows() %>%
             readr::write_rds(f_name),
           "list"       = out %>%
             readr::write_rds(f_name)
    )
    if (exists("log_it")) {
      log_it("success", "Dictionary created.", "db")
      log_fn("end")
    }
  }
}

#' Create a simple entity relationship map
#'
#' This will poll the database connection and create an entity relationship map
#' as a list directly from defined SQL statements used to build the table or
#' view. For each table object it returns a list of length three containing the
#' entity names that the table (1) `references` (i.e. has a foreign key to), (2)
#' is `referenced_by` (i.e. is a foreign key for), and (3) views where it is
#' `used_in_view`. These are names. This is intended for use as a mapping
#' shortcut when ER Diagrams are unavailable, or for quick reference within a
#' project, similarly to a dictionary relationship reference.
#'
#' SQL is generated from [pragma_table_def()] with argument `get_sql` = TRUE and
#' ignores entities whose names start with "sqlite".
#'
#' @param db_conn connection object, specifically of class "SQLiteConnection" but
#'   not strictly enforced
#'
#' @return nested LIST object describing the database entity connections
#' @export
#'
#' @usage er_map(db_conn = con)
er_map <- function(db_conn = con) {
  if (exists("log_it")) log_fn("start")
  # Verify arguments
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_conn  = list(c("length", 1))
      ),
      from_fn    = "er_map"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  
  if (exists("log_it")) log_it("debug", "Parsing build statements...", "db")
  build_statements <- pragma_table_def(db_conn = db_conn, db_table = dbListTables(db_conn), get_sql = TRUE)
  build_statements <- build_statements[-grep("^sqlite", build_statements$table_name), ]
  n_tables <- nrow(build_statements)
  t_names  <- build_statements$table_name
  er_mask  <- matrix(rep(0, n_tables ^2), nrow = n_tables)
  rownames(er_mask) <- t_names
  colnames(er_mask) <- t_names
  view_mask <- er_mask
  er_type  <- str_extract_all(build_statements$sql, "CREATE [:word:]+") %>%
    lapply(str_remove_all, "CREATE ")
  tables_and_views <- which(unlist(er_type) %in% c("TABLE", "VIEW"))
  build_statements <- build_statements[tables_and_views, ]
  ref_tables <- stringr::str_extract_all(build_statements$sql, "REFERENCES [:word:]+") %>%
    lapply(stringr::str_remove_all, "REFERENCES ")
  refs     <- stringr::str_extract_all(build_statements$sql, "(FOREIGN KEY \\([:word:]+\\) )?REFERENCES [:word:]+\\([:word:]+\\)") %>%
    lapply(stringr::str_remove_all, "FOREIGN KEY \\(") %>%
    lapply(stringr::str_replace_all, "\\) ", " ")
  used_in  <- stringr::str_extract_all(build_statements$sql, "(JOIN|FROM) [:word:]+") %>%
    lapply(stringr::str_remove_all, "(JOIN|FROM) ")
  if (exists("log_it")) log_it("debug", "Reading table and view relationships...", "db")
  for (i in 1:n_tables) {
    z <- which(t_names %in% ref_tables[[i]])
    if (length(z) > 0) {
      er_mask[i, z] <- 1
    }
    z <- which(t_names %in% used_in[[i]])
    if (length(z) > 0) {
      view_mask[z, i] <- 1
    }
  }
  er_map   <- vector("list", n_tables) %>%
    setNames(t_names)
  for (i in 1:n_tables) {
    er_map[[i]] <- list(
      object_name         = t_names[i],
      object_type         = er_type[[i]],
      references_tables   = if (er_type[[i]] == "TABLE") {
        t_names[as.logical(er_mask[i, ])]
      } else {
        t_names[as.logical(view_mask[, i])]
      },
      references          = refs[[i]],
      normalizes_tables   = t_names[as.logical(er_mask[, i])],
      used_in_view        = t_names[as.logical(view_mask[i, ])]
    )
  }
  direct_add <- names(
    er_map[
      unlist(map(er_map, function(x) all(
        x$object_type == "TABLE",
        length(x$references) == 0)))
    ]
  )
  dependent_add <- names(
    er_map[
      unlist(map(er_map, function(x) all(
        x$object_type == "TABLE",
        length(x$references) > 0)))
    ]
  )
  all_tables <- sort(c(direct_add, dependent_add))
  all_views  <- names(er_map)[!names(er_map) %in% all_tables]
  er_map$tables_without_dependency <- direct_add
  er_map$tables_with_normalization_dependency <- dependent_add
  er_map$is_table <- all_tables
  er_map$is_view  <- all_views
  if (exists("log_it")) log_fn("end")
  return(er_map)
}

#' Check for, and optionally remove, a database connection object
#'
#' This function seeks to abstract connection management objects to a degree. It
#' seeks to streamline the process of connecting and disconnecting existing
#' connections as defined by function parameters. This release has not been
#' tested extensively with drivers other than SQLite.
#'
#' @note If you want to disconnect everything but retain tibble pointers to your
#'   data source as tibbles in this session, use [close_up_shop] instead.
#'
#' @param db CHR scalar name of the database to check, defaults to the name
#'   supplied in config/env.R (default: session variable DB_NAME)
#' @param drv_pack CHR scalar of the package used to connect to this database
#'   (default: session variable DB_DRIVER)
#' @param conn_class CHR vector of connection object classes to check against.
#'   Note this may depend heavily on connection packages and must be present in
#'   the class names of the driver used. (default session variable DB_CLASS)
#' @param conn_name CHR scalar of the R environment object name to use for this
#'   connection (default: "con")
#' @param is_local LGL scalar indicating whether or not the referenced database
#'   is a local file, if not it will be treated as though it is either a DSN or
#'   a database name on your host server, connecting as otherwise defined
#' @param rm_objects LGL scalar indicating whether or not to remove objects
#'   identifiably connected to the database from the current environment. This
#'   is particularly useful if there are outstanding connections that need to be
#'   closed (default: TRUE)
#' @param reconnect LGL scalar indicating whether or not to connect if a
#'   connection does not exist; if both this and `disconnect` are true, it will
#'   first be disconnected before reconnecting. (default: TRUE)
#' @param disconnect LGL scalar indicating whether or not to terminate and
#'   remove the connection from the current global environment (default: TRUE)
#' @param log_ns CHR scalar of the namespace (if any) to use for logging
#' @param .environ environment within which to place this connection object
#' @param ... named list of any other connection parameters required for your
#'   database driver (e.g. postgres username/password)
#'
#' @return None
#' @export
#'
#' @note For more complicated setups, it may be easier to use this function by
#'   storing parameters in a list and calling with [base::do.call()]
#'
#' @usage manage_connection("test.sqlite", conn_name = "test_con")
manage_connection <- function(db          = DB_NAME,
                              drv_pack    = DB_PACKAGE,
                              drv         = DB_DRIVER,
                              conn_class  = DB_CLASS,
                              conn_name   = DB_CONN_NAME,
                              is_local    = TRUE,
                              rm_objects  = TRUE,
                              reconnect   = TRUE,
                              disconnect  = TRUE,
                              log_ns      = "db",
                              .environ    = .GlobalEnv,
                              ...) {
  logger <- exists("log_it")
  if (is.null(log_ns) || is.na(log_ns)) log_ns <- NA_character_
  if (logger) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db, drv_pack, drv, conn_class, conn_name, is_local, rm_objects, reconnect, disconnect, log_ns),
      conditions = list(
        db         = list(c("mode", "character"), c("length", 1)),
        drv_pack   = list(c("mode", "character"), c("length", 1)),
        drv        = list(c("mode", "character"), c("length", 1)),
        conn_class = list(c("mode", "character"), c("length", 1)),
        conn_name  = list(c("mode", "character"), c("length", 1)),
        is_local   = list(c("mode", "logical"), c("length", 1)),
        rm_objects = list(c("mode", "logical"), c("length", 1)),
        reconnect  = list(c("mode", "logical"), c("length", 1)),
        disconnect = list(c("mode", "logical"), c("length", 1)),
        log_ns     = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "manage_connection"
    )
    stopifnot(arg_check$valid)
  }
  require(drv_pack, character.only = TRUE)
  global_env       <- as.list(.environ)
  global_env_names <- names(global_env)
  connection_object_classes <- sprintf("%sConnection", conn_class)
  for (env in global_env_names) {
    connected      <- FALSE
    connection     <- ""
    this_obj       <- global_env[[env]]
    this_obj_class <- class(this_obj)
    if (any(grepl(conn_class, this_obj_class))) {
      if (any(grepl(sprintf("^tbl_%s$", connection_object_classes), this_obj_class))) {
        connection <- try(this_obj$src$con@dbname)
      } else if (any(grepl(sprintf("^%sResult$", drv), this_obj_class))) {
        connection <- try(this_obj@db_conn@dbname)
        dbClearResult(this_obj)
      } else if (any(grepl(connection_object_classes, this_obj_class))) {
        connection <- try(this_obj@dbname)
      } else if (class(this_obj) == conn_class) {
        connection <- "other"
      } else {
        connection <- ""
      }
      if (class(connection) == "try-error") {
        if (logger) log_it("warn", 'Could not automatically identify the connection properties for object "{env}". To avoid hanging connections, it should be removed explicitly.', log_ns)
        connection <- ""
      }
      if (
        all(
          basename(connection) == db,
          any(class(this_obj) %in% connection_object_classes)
        )
      ){
        connected <- dbIsValid(this_obj)
        if (connected && logger) log_it("trace", glue::glue('Database "{db}" is currently open.'), log_ns)
        if (all(connected, disconnect, basename(connection) == db)) {
          if (logger) log_it("trace", glue::glue('Disconnecting from "{db}"...'), log_ns)
          check <- try(invisible(dbDisconnect(this_obj)))
          status <- ifelse(inherits(check, "try-error"),
                           "unable to be disconnected",
                           "disconnected")
          log_it("trace", glue::glue('Database "{db}" {status}.'), log_ns)
          connected <- dbIsValid(this_obj)
        }
      }
      if (
        all(
          rm_objects, disconnect,
          any(
            basename(connection) == db,
            connection == "other"
          )
        )
      ) {
        if (logger) log_it("trace", glue::glue('Removing session object "{env}"...'), log_ns)
        rm(list = env, pos = ".GlobalEnv")
      }
    }
  }
  if (reconnect) {
    if (is_local) {
      # Resolve database file location
      if (logger) log_it("trace", glue::glue('Finding local database file "{db}"'), log_ns)
      db_where  <- list.files(path = here::here(), pattern = db, full.names = TRUE, recursive = TRUE)
      if (length(db_where) == 0) {
        stop(sprintf('Unable to locate "%s".', db))
      } else if (length(db_where) > 1) {
        if (length(db_where[basename(db_where) == db]) == 1) {
          db <- db_where[basename(db_where) == db]
        } else {
          stop(sprintf('More than one file named "%s" were located and none matched completely.', db))
        }
      } else {
        db <- db_where
      }
    }
    global_env       <- as.list(.environ)
    global_env_names <- names(global_env)
    if (conn_name %in% global_env_names) {
      if (connected) dbDisconnect(rlang::sym(conn_name))
      rm(list = conn_name, pos = ".GlobalEnv")
    }
    args <- list(db, ...)
    assign(x     = conn_name,
           value = try(do.call("dbConnect", args = c(drv = do.call(drv, list()), args))),
           envir = .GlobalEnv)
    if (drv == "SQLite") {
      invisible(
        res <- do.call(dbExecute, args = list(conn = .GlobalEnv[[conn_name]], statement = "pragma foreign_keys = on"))
      )
    }
    if (class(eval(rlang::sym(conn_name))) == "try-error") {
      stop(sprintf('Unable to connect to "%s".\nCall attempted was "assign(x = %s, value = try(do.call("dbConnect", args = c(drv = do.call(%s, list()), %s)))',
                   db,
                   drv,
                   paste0(args, collapse = ", ")))
    } else {
      if (dbIsValid(eval(rlang::sym(conn_name)))) if (logger) log_it("trace", glue('"{db}" connected as "{conn_name}".'), log_ns)
    }
  }
  if (logger) log_fn("end")
}

#' Parse SQL build statements
#'
#' Reading SQL files directly into R can be problematic. This function is
#' primarily called in [create_fallback_build]. To support multiline,
#' human-readable SQL statements, `sql_statements` must be of length 1.
#'
#' All arguments `magicsplit`, `header`, and `section` provide flexibility in
#' the comment structure of the SQL file and accept regex for character matching
#' purposes.
#'
#' @param sql_statements CHR scalar of SQL build statements from an SQL file.
#' @param magicsplit CHR scalar regex indicating some "magic" split point SQL
#'   comment to simplify the identification of discrete commands; will be used
#'   to split results (optional but highly recommended)
#' @param header CHR scalar regex indicating the format of header comments SQL
#'   comment to remove (optional)
#' @param section CHR scalar regex indicating the format of section comments SQL
#'   comment to remove (optional)
#'
#' @return LIST of parsed complete build commands as CHR vectors containing each
#'   line.
#' @export
#'
#' @usage
#' example_file <- "./config/sql_nodes/reference.sql"
#' if (file.exists(example_file)) {
#'   build_commands <- readr::read_file(example_file)
#'   sqlite_parse_build(build_commands)
#' }
sqlite_parse_build <- function(sql_statements,
                               magicsplit = "/\\*magicsplit\\*/",
                               header     = "/\\*\\=+\\r*\\n[[:print:][:cntrl:]]+\\r*\\n\\=+\\*/",
                               section    = "/\\* -* [[:print:][:cntrl:]]+ -* \\*/",
                               comment    = "/\\* [[:alnum:][:punct:] -,\\.]+ \\*/") {
  if (exists("log_it")) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sql_statements = list(c("mode", "character"), c("length", 1)),
        magicsplit     = list(c("mode", "character"), c("length", 1)),
        header         = list(c("mode", "character"), c("length", 1)),
        section        = list(c("mode", "character"), c("length", 1)),
        comment        = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "sqlite_parse_build"
    )
    stopifnot(arg_check$valid)
  }
  if (exists("log_it")) log_it("trace", 'Parsing sql_statements for build commands.', "db")
  # to_remove <- paste0(c(header, comment, "\\t", "\\r"), collapse = "|")
  to_remove <- paste0(c(header, "\\t", "\\r"), collapse = "|")
  out       <- sql_statements %>%
    str_replace_all("CREATE ", paste0(magicsplit, "CREATE ")) %>%
    str_replace_all("\\.import ", paste0(magicsplit, ".import ")) %>%
    str_replace_all("\\.read ", paste0(magicsplit, ".read ")) %>%
    str_replace_all(paste0("/\\* ", magicsplit, ".read"), paste0(magicsplit, "/* .read")) %>%
    str_replace_all("DELETE FROM ", paste0(magicsplit, "DELETE FROM ")) %>%
    str_replace_all("PRAGMA ", paste0(magicsplit, "PRAGMA ")) %>%
    str_replace_all("\\;\\nINSERT ", paste0("; ", magicsplit, "\nINSERT ")) %>%
    str_replace_all("\\;\\nDROP ", paste0("; ", magicsplit, "\nDROP ")) %>%
    str_remove_all(to_remove) %>%
    str_split(magicsplit) %>%
    .[[1]] %>%
    str_replace_all("\\n{2,10}", "\n") %>%
    str_squish() %>%
    str_split("\\n") %>%
    lapply(str_squish) %>%
    lapply(function(x) x[! x %in% c("", " ")])
  out        <- out[unlist(lapply(out, length)) > 0]
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Parse SQL import statements
#'
#' In the absence of the sqlite command line interface (CLI), the [build_db]
#' process needs a full set of SQL statements to execute directly rather than
#' CLI dot commands. This utility function parses formatted SQL statements
#' containing CLI ".import" commands to create SQL INSERT statements. This
#' function is primarily called in [create_fallback_build].
#'
#' @param build_statements CHR vector of SQL build statements from an SQL file.
#'
#' @return LIST of parsed .import statements as full "INSERT" statements.
#' @export
#'
#' @usage
#' if (file.exists("./config/data/elements.csv")) {
#'   sqlite_parse_import(".import --csv --skip 1 ./config/data/elements.csv elements")
#' }
sqlite_parse_import <- function(build_statements) {
  logger <- exists("log_it")
  if (logger) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(build_statements),
      conditions = list(
        build_statements = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "sqlite_parse_import"
    )
    stopifnot(arg_check$valid)
  }
  regex_import  <- ".import (--[[:alnum:]]+)? "
  regex_skip    <- "(--skip [[:number:]]+) ?"
  regex_prefix  <- paste0(c(regex_import, regex_skip), collapse = "?")
  if (logger) log_it("trace", glue::glue('Parsing \n{build_statements}\n for import modification.'), "db")
  out           <- lapply(build_statements,
                          function(x) {
                            if (grepl("\\.import", x)) {
                              data_type <- x %>%
                                stringr::str_extract(regex_import) %>%
                                stringr::str_remove(".import( --)?") %>%
                                stringr::str_squish()
                              if (data_type == "") {
                                data_type_read <- FALSE
                                if (logger) log_it("warn", 'No data type identified. Assuming a ".csv" extension.', "db")
                                data_type <- "csv"
                              } else {
                                data_type_read <- TRUE
                              }
                              skip_rows <- x %>%
                                stringr::str_extract(regex_skip) %>%
                                stringr::str_remove("--skip ") %>%
                                stringr::str_squish()
                              skip_rows <- try(as.numeric(skip_rows))
                              if (class(skip_rows) == 'try-error') {
                                if (logger) log_it("warn", glue('Could not convert "{skip_rows}" to numeric in "{x}".'), "db")
                                return(x)
                              }
                              tmp <- x %>%
                                stringr::str_remove(regex_prefix) %>%
                                stringr::str_split(" ") %>%
                                .[[1]]
                              if (length(tmp) != 2) {
                                if (logger) log_it("warn", glue::glue('Could not parse "{x}" to identify both a target file and database target table.'), "db")
                                return(x)
                              }
                              data_file <- tmp[1]
                              if (file.exists(data_file)) {
                                if ("readr" %in% installed.packages()) {
                                  require(readr)
                                  read_func <- sprintf("read_%s", data_type)
                                } else {
                                  read_func <- sprintf("read.%s", data_type)
                                }
                                if (exists(read_func)) {
                                  read_data <- try(
                                    suppressMessages(
                                      do.call(read_func,
                                              list(data_file))
                                    )
                                  )
                                  if ("try-error" %in% class(read_data)) {
                                    msg <- sprintf('Error reading file "%s" with function "%s".\n%s
                                                   Read command was "%s"; make sure file extensions match the file format and try again.',
                                                   data_file,
                                                   read_func,
                                                   ifelse(data_type_read,
                                                          "",
                                                          sprintf('Data type was assumed to be "%s".\n',
                                                                  data_type)),
                                                   x
                                    )
                                    if (logger) log_it("warn", msg, "db")
                                  }
                                } else {
                                  if (logger) log_it("warn", glue::glue('Cannot read file "{data_file}". Function "{read_func}" is not available.'), "db")
                                  return(x)
                                }
                              } else {
                                if (logger) log_it("warn", glue::glue('Could not find file "{data_file}".'), "db")
                                return(x)
                              }
                              target    <- tmp[2]
                              if (nrow(read_data) == 0) {
                                return("")
                              }
                              insert_values <- sprintf('("%s")',
                                                       read_data %>%
                                                         tidyr::unite(col = "statement", sep = '", "') %>%
                                                         dplyr::pull(statement) %>%
                                                         paste0(collapse = '"), ("')
                              ) %>%
                                stringr::str_replace_all('"NA"|"NULL"|"null"|""', "NULL")
                              insert_statement <- c(
                                glue::glue("/* Inserts for table '{target}' were read from file '{data_file}' */"),
                                sprintf("INSERT INTO `%s` VALUES %s;",
                                        target,
                                        insert_values)
                              )
                              return(insert_statement)
                            } else {
                              return(x)
                            }
                          })
  if (logger) log_fn("end")
  return(out)
}

#' Create an SQL file for use without the SQLite CLI
#'
#' For cases where the SQLite Command Line Interface is not available, dot
#' commands used to simplify the database build pipeline are not usable. Call
#' this function to create a self-contained SQL build file that can be used in
#' [build_db] to build the database. The self-contained file will include all
#' "CREATE" and "INSERT" statements necessary by parsing lines including ".read"
#' and ".import" commands and directly reading referenced files.
#'
#' @param build_file CHR scalar name SQL build file to use. The default, NULL,
#'   will use the environment variable "DB_BUILD_FILE" if it is available.
#' @param populate LGL scalar of whether to populate data (default: TRUE)
#' @param populate_with CHR scalar name SQL population file to use. The default,
#'   NULL, will use the environment variable "DB_DATA" if it is available.
#' @param driver CHR scalar of the database driver class to use to correctly
#'   interpolate SQL commands (default: "SQLite")
#' @param comments CHR scalar regex identifying SQLite comments
#' @param out_file CHR scalar of the output file name and destination. The
#'   default, NULL, will write to a file named similarly to `build_file`
#'   suffixed with "_full".
#'
#' @return None: a file will be written at `out_file` with the output.
#' @export
#'
#' @usage create_fallback_build(build_file = file.path("config", "build.sql"))
create_fallback_build <- function(build_file    = NULL,
                                  populate      = TRUE,
                                  populate_with = NULL,
                                  driver        = "SQLite",
                                  comments      = "/\\* [[:alnum:][:punct:] -,\\.\\'/_]+ \\*/",
                                  out_file      = NULL) {
  if (exists("log_it")) log_fn("start")
  if (all(is.null(build_file), exists("DB_BUILD_FILE"))) build_file <- DB_BUILD_FILE
  if (all(is.null(populate_with), exists("DB_DATA"))) populate_with <- DB_DATA
  if (!file.exists(build_file)) {
    build_files <- list.files(pattern = build_file,
                              full.names = TRUE,
                              recursive = TRUE)
    if (length(build_files) > 1) {
      if (interactive()) {
        build_file <- resolve_multiple_values(build_files, build_file)
      } else {
        stop("Non-interactive session and multiple build files identified.")
      }
    } else if (length(build_files) == 1) {
      build_file <- build_files
    }
  }
  if (is.null(out_file)) {
    out_file <- gsub(".sql", "_full.sql", build_file)
  } else {
    out_file <- out_file
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(build_file, populate, populate_with, driver, out_file),
      conditions = list(
        build_file    = list(c("mode", "character"), c("length", 1)),
        populate      = list(c("mode", "logical"), c("length", 1)),
        populate_with = list(c("mode", "character"), c("length", 1)),
        driver        = list(c("mode", "character"), c("length", 1)),
        out_file      = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  if (!file.exists(build_file)) {
    stop(sprintf('Could not locate file "%s".', build_file))
  }
  if (exists("log_it")) log_it("info", glue::glue("Creating fallback build schema from {build_file}..."), "db")
  build   <- c(
    glue::glue("/* This fallback creation schema was created {Sys.Date()} from '.{.Platform$file.sep}{build_file}'. */"),
    glue::glue("/* Created under system context {jsonlite::toJSON(R.Version()[c('platform', 'version.string')])}. */"),
    readr::read_file(build_file) %>%
      sqlite_parse_build()
  )
  
  if (populate) {
    if (file.exists(populate_with)) {
      populate_file <- populate_with
    } else {
      populate_files <- list.files(pattern = populate_with,
                                   full.names = TRUE,
                                   recursive = TRUE)
      if (length(populate_files) > 1) {
        if (interactive()) {
          populate_file <- resolve_multiple_values(populate_files, populate_file)
        } else {
          stop("Non-interactive session and multiple population files identified.")
        }
      } else if (length(populate_files) == 1) {
        populate_file <- populate_files
      }
    }
    if (exists("log_it")) log_it("info", glue::glue("Populating data insert reads from '{populate_file}'..."), "db")
    populate <- readr::read_file(populate_file) %>%
      sqlite_parse_build()
    build <- c(
      build,
      glue::glue("/* Data were populated according to '.{.Platform$file.sep}{populate_file}' */"),
      populate)
  }
  
  build   <- as.list(unlist(build))
  
  # Make SQL build statements
  index_read   <- grep("^.read", build)
  while(length(index_read) > 0) {
    if (exists("log_it")) log_it("info", "Expanding .read statements...", "db")
    temp_read    <- lapply(build[index_read],
                           function(x) {
                             node_file <- str_remove(x, ".read ") %>%
                               str_remove(comments) %>%
                               str_trim()
                             read_file(node_file) %>%
                               sqlite_parse_build()
                           })
    source_read  <- unlist(build[index_read]) %>%
      str_remove(".read ") %>%
      str_remove(comments) %>%
      str_trim()
    names(temp_read) <- source_read %>%
      basename() %>%
      tools::file_path_sans_ext()
    for (i in 1:length(temp_read)) {
      source_comment <- glue::glue("/* Sourced from '.{.Platform$file.sep}{source_read[i]}' */")
      temp_read[[i]] <- c(source_comment, temp_read[[i]])
    }
    build[index_read] <- temp_read %>%
      lapply(function(x) {
        lapply(x, function(y) {
          paste0(y, collapse = " ")
        })
      })
    build <- unlist(build) %>% 
      as.list()
    index_read <- grep("^.read", build)
  }
  
  # Make SQL insert statements
  index_import <- grep("^.import", build)
  while (length(index_import) > 0) {
    if (exists("log_it")) log_it("info", "Expanding .import statements...", "db")
    temp_import  <- lapply(build[index_import],
                           function(x) {
                             if (grepl(comments, x)) return(x)
                             lapply(x, sqlite_parse_import)
                           }
    )
    check <- build
    build[index_import] <- temp_import
    build <- unlist(build) %>%
      as.list()
    index_import <- grep("^.import", build)
  }
  
  build <- build %>%
    purrr::flatten() %>%
    .[!. == ""] %>%
    unlist() %>%
    paste0(collapse = "\n")
  readr::write_file(x = build, file = out_file, append = FALSE)
  if (exists("log_it")) {
    log_it("info", sprintf('Fallback build file created as "%s".',
                           out_file), "db")
    log_fn("end")
  }
}

# TODO early sketch for construction of automatic logging triggers
build_db_logging_triggers <- function(db = DB_NAME, connection = "con", log_table_name = "log") {
  if (exists("log_it")) log_fn("start")
  # According to the current environment setup, exclude logging to save space
  if (!exists("DB_LOGGING")) {
    DB_LOGGING <- FALSE
  }
  if (DB_LOGGING) {
    # Require a connection object
    require(glue)
    require(magrittr)
    if (!exists(connection)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
    }
    tables  <- dbListTables(con)
    tables  <- tables[!tables == log_table_name]
    log_cols <- dbListFields(con, log_table_name)
    out <- character(0)
    for (table in tables) {
      table_cols <- dbGetQuery(con, glue("PRAGMA table_info({table})")) %>%
        filter(pk != 1, name != "id") %>%
        pull(name) %>%
        paste0(collapse = ", ")
      out <- c(out,
               glue(
                 "CREATE TRIGGER {table}_track_insert",
                 "  AFTER INSERT ON {table}",
                 "  BEGIN",
                 "    INSERT INTO {log_table_name} (affect_type, affects_table, affects_ids, executed_by)",
                 "    VALUES ('INSERT', '{table}', NEW.id);",
                 "  END;"
               )
      )
    }
  }
  if (exists("log_it")) log_fn("end")
}

#' Convenience function to rebuild all database related files
#'
#' This is a development and deployment function that should be used with
#' caution. It is intended solely to assist with the development process of
#' rebuilding a database schema from source files and producing the supporting
#' data. It will create both the JSON expressin of the data dictionary and the
#' fallback SQL file.
#'
#' !! To preserve data, do not call this with both `rebuild` = TRUE and
#' `archive` = FALSE !!
#'
#' @note This does not recast the views and triggers files created through
#'   [sqlite_autoview] and [sqlite_autotrigger] as the output of those may often
#'   need additional customization. Existing auto-views and -triggers will be
#'   created as defined. To exclude those, first modify the build file
#'   referenced by [build_db].
#'
#' @note This requires references to be in place to the individual functions in
#'   the current environment.
#'
#' @inheritParams build_db
#'
#' @param rebuild LGL scalar indicating whether to first rebuild from
#'   environment settings (default: FALSE for safety)
#' @param api_running LGL scalar of whether or not the API service is currently
#'   running (default: TRUE)
#' @param api_monitor process object pointing to the API service (default: NULL)
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#'
#' @return Files for the new database, fallback build, and data dictionary will
#'   be created in the project directory and objects will be created in the
#'   global environment for the database map (LIST "db_map") and current
#'   dictionary (LIST "db_dict")
#'
#' @usage update_all()
update_all <- function(rebuild       = FALSE,
                       api_running   = TRUE,
                       api_monitor   = NULL,
                       db            = DB_NAME,
                       build_from    = DB_BUILD_FILE,
                       populate      = TRUE,
                       populate_with = DB_DATA,
                       archive       = TRUE,
                       sqlite_cli    = SQLITE_CLI,
                       connect       = FALSE,
                       log_ns = "db") {
  if (exists("log_it")) {
    log_fn("start")
    log_it("debug", "Updating database, fallback build, and data dictionary files.", log_ns)
  }
  if (api_running) {
    if (exists("api_reload")) {
      pr_name <- obj_name_check(api_monitor, default_name = "plumber_service")
      plumber_service_existed <- exists(pr_name)
      if (plumber_service_existed) {
        if (api_monitor$is_alive()) api_stop(pr = pr_name, remove_service_obj = FALSE)
      }
    } else {
      plumber_service_existed <- FALSE
    }
  }
  if (rebuild) {
    manage_connection(db = db, reconnect = FALSE)
    tmp <- try(
      build_db(
        db            = db,
        build_from    = build_from,
        populate      = populate,
        populate_with = populate_with,
        archive       = archive,
        sqlite_cli    = sqlite_cli,
        connect       = connect
      )
    )
    if (inherits(tmp, "try-error")) {
      if (exists("log_it")) log_it("error", "There was a problem building the database. Build process halted.", log_ns)
      return(tmp)
    }
    manage_connection(db = db)
  }
  save_data_dictionary()
  dict_file <- list.files(pattern = "data_dictionary.json",
                          full.names = TRUE) %>%
    file.info() %>%
    arrange(desc(ctime)) %>%
    slice(1) %>%
    rownames()
  if (length(dict_file) == 0) {
    if (exists("log_it")) log_it("warn", "Unable to locate a data dictionary file.", log_ns)
  } else {
    if (exists("log_it")) log_it("info", "Saving data dictionary to this session as object 'db_dict'.", log_ns)
    db_dict <<- dict_file %>%
      jsonlite::read_json() %>%
      lapply(bind_rows)
  }
  tmp <- try(er_map())
  if (inherits(tmp, "try-error")) {
    if (exists("log_it")) log_it("error", "There was a problem generating the entity relationship map. Build process halted.", log_ns)
    return(tmp)
  }
  if (exists("log_it")) log_it("info", "Saving entity relationship map to this session as object 'db_map'.", log_ns)
  db_map <<- tmp
  create_fallback_build(
    build_file = build_from,
    populate = populate,
    populate_with = populate_with
  )
  if (api_running && exists("api_reload")) {
    if (plumber_service_existed) {
      api_reload(pr = pr_name, background = TRUE)
    } else {
      api_reload(background = TRUE)
    }
  }
  if (exists("log_it")) log_fn("end")
}

#' Conveniently close all database connections
#'
#' This closes both the plumber service and all database connections from the
#' current running environment. If outstanding promises exist to database tables
#' or views were created as class `tbl_` (e.g. with `tbl(con, "table")`), set
#' `back_up_connected_tbls` to TRUE to collect data from those and preserve
#' in-place in the current global environment.
#'
#' @param back_up_connected_tbls LGL scalar of whether to clone currently
#'   promised tibble connections to database objects as data frames (default:
#'   FALSE).
#'
#' @return None, modifies the current global environment in place
#' 
#' @export
#' 
#' @usage
#' manage_connection()
#' close_up_shop(TRUE)
close_up_shop <- function(back_up_connected_tbls = FALSE) {
  if (exists("log_it")) {
    log_fn("start")
    log_it("debug",
           sprintf("Closing connections %s backing up connected tibbles.",
                   ifelse(back_up_connected_tbls, "and", "without")),
           "db"
    )
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(back_up_connected_tbls),
      conditions = list(
        back_up_connected_tbls = list(c("mode", "logical"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  
  tmp <- lapply(as.list(.GlobalEnv), class)
  # Kill plumber instances
  api_services <- names(tmp)[
    which(
      lapply(
        tmp,
        function(x) all(c("r_process", "R6", "process") %in% x)
      ) == TRUE
    )
  ]
  for (api in api_services) {
    if (.GlobalEnv[[api]]$is_alive()) {
      api_stop(pr = api, remove_service_obj = TRUE)
    }
  }
  # Kill db connected objects
  tbls <- tmp[
    which(
      unlist(
        lapply(
          tmp,
          function(x) any(str_detect(x, "^tbl_.*Connection$")))
      ) == TRUE
    )
  ]
  if (length(tbls) > 0) tmp <- tmp[-which(names(tmp) %in% names(tbls))]
  connections <- tmp[
    which(
      unlist(
        lapply(
          tmp,
          function(x) any(str_detect(x, "Connection$")))
      ) == TRUE
    )
  ]
  for (tbl in names(tbls)) {
    if (back_up_connected_tbls) {
      assign(
        x     = tbl,
        value = collect(eval(rlang::sym(tbl))),
        envir = .GlobalEnv
      )
    }
  }
  for (connection in names(connections)) {
    manage_connection(
      db = basename(eval(sym(eval(connection)))@dbname),
      conn_name = connection,
      reconnect = FALSE,
      rm_objects = TRUE
    )
  }
  if (exists("log_it")) log_fn("end")
}

# Database utility functions ---------------------------------------------------

#' Is a connection object still available?
#' 
#' This is a thin wrapper for [DBI::dbIsValid] with some error logging.
#'
#' @aliases [DBI::dbIsValid]
#' 
#' @param db_conn connection object (default "con")
#'
#' @return LGL scalar indicating whether the database is available
#' @export
#'
#' @usage active_connection(db_conn = con)
active_connection <- function(db_conn = con) {
  if (exists("log_it")) log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_conn),
      conditions = list(
        db_conn    = list(c("length", 1))
      ),
      from_fn = "active_connection"
    )
    stopifnot(arg_check$valid)
  }
  status <- DBI::dbIsValid(db_conn)
  if (exists("log_it"))  {
    if (!status) log_it("error", "Connection is no longer available.", "db")
    log_fn("end")
  }
  return(status)
}

#' Add value(s) to a normalization table
#'
#' One of the most common database operations is to look up or add a value in a
#' normalization table. This utility function adds a single value and returns
#' its associated id by using [build_db_action]. This is only suitable for a
#' single value. If you need to bulk add multiple new values, use this with
#' something like [lapply].
#'
#' @param db_table CHR scalar of the normalization table's name
#' @param db_conn connection object (default "con")
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#' @param id_column CHR scalar of the column to use as the primary key
#'   identifier for `db_table` (default: "id")
#' @param database_map LIST of the database entity relationship map, typically
#'   from calling [er_map]. If NULL (default) the object "db_map" will be
#'   searched for and used by default, otherwise it will be created with
#'   [er_map]
#' @param ... CHR vector of additional named arguments to be added; names not
#'   appearing in the referenced table will be ignored
#'
#' @return NULL if unable to add the values, INT scalar of the new ID otherwise
#' @export
#'
#' @usage add_normalization_value("norm_table", name = "new value", acronym = "NV")
add_normalization_value <- function(db_table, db_conn = con, log_ns = "db", id_column = "id", database_map = NULL, ...) {
  if (exists("log_it")) log_fn("start")
  new_values <- list(...)
  if (is.null(database_map)) {
    if (exists("db_map")) {
      database_map <- db_map
    } else {
      assign("db_map", er_map(db_conn), envir = .GlobalEnv)
    }
  }
  if (length(names(new_values)) != length(new_values)) {
    stop("All values provided to this function must be named.")
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_table, db_conn, log_ns),
      conditions = list(
        db_table    = list(c("mode", "character"), c("length", 1)),
        db_conn     = list(c("length", 1)),
        log_ns      = list(c("mode", "character"), c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  table_cols <- pragma_table_info(db_table)
  needed     <- table_cols %>%
    filter(name != id_column) %>%
    pull(name)
  all_cols   <- needed
  required   <- table_cols %>%
    filter(notnull == 1, name != id_column) %>%
    pull(name)
  need_unique <- table_cols %>%
    filter(unique) %>%
    pull(name)
  new_values <- new_values[which(names(new_values) %in% needed)]
  needed     <- needed[!needed %in% names(new_values)]
  if (exists("log_it")) log_it("info", glue::glue("Adding normalization value to {db_table}."), log_ns)
  if (length(needed) == 0) {
    if (exists("log_it")) {
      log_it("trace", "Named values were provided for all columns.", log_ns)
    }
  } else if (length(needed) > 0) {
    msg <- sprintf('Not all values for table "%s" were supplied.', db_table)
    if (exists("log_it")) {
      log_it("warn",
             sprintf("%s Please provide the following values to continue.%s",
                     msg,
                     ifelse(length(new_values) > 0,
                            sprintf(" You provided '%s' as the initial value%s.",
                                    format_list_of_names(paste0(names(new_values), " = ", new_values)),
                                    ifelse(length(new_values) > 1, "s", "")
                            ),
                            ""
                     )
             ),
             log_ns
      )
    }
  }
  if (interactive()) {
    for (need_this in all_cols) {
      prompt_field <- ifelse(tolower(need_this) %in% c("orcid", "pid"),
                             toupper(need_this),
                             need_this %>%
                               stringr::str_replace_all("_", " ") %>%
                               stringr::str_to_title()
      )
      value_checks <- c(
        required = ifelse(need_this %in% required, "Required", ""),
        unique   = ifelse(need_this %in% need_unique, "Unique", ""),
        default  = table_cols$dflt_value[table_cols$name == need_this],
        provided = ifelse(need_this %in% names(new_values),
                          new_values[[need_this]],
                          "")
      )
      if (is.na(value_checks[["default"]])) value_checks[["default"]] <- ""
      need_clause <- c(
        required = value_checks[["required"]],
        unique   = value_checks[["unique"]],
        default = ifelse(
          value_checks[["default"]] == "" || is.na(value_checks[["default"]]),
          "",
          sprintf('default value "%s"', value_checks[["default"]])
        ),
        provided = ifelse(
          value_checks[["provided"]] == "",
          "",
          sprintf('provided value "%s"', value_checks[["provided"]])
        )
      )
      if (need_clause[["provided"]] != "") {
        need_clause[["provided"]] <- sprintf("Press enter to use the %s",
                                             need_clause[["provided"]])
      }
      if (need_clause[["default"]] != "" && need_clause[["provided"]] == "") {
        need_clause[["default"]] <- sprintf("Press enter to use the %s",
                                            need_clause[["default"]])
      }
      need_clause <- need_clause[need_clause != ""] %>%
        paste0(collapse = "; ")
      if (nchar(need_clause) > 0) need_clause <- sprintf(" (%s)", need_clause)
      need_prompt <- sprintf("%s%s: ",
                             prompt_field,
                             need_clause)
      new_value <- ""
      if (all(need_this == "acronym", "name" %in% names(new_values))) {
        if ("name" %in% names(new_values)) {
          auto_acro <- make_acronym(new_values$name)
          use_auto_acro <- select.list(
            choices = c("Yes", "No"),
            title = glue::glue('\nUse the automatic acronym "{auto_acro}" for normalization name "{new_values$name}"?')
          )
        } else {
          use_auto_acro <- "NULL"
        }
        if (use_auto_acro == "Yes") {
          new_value <- auto_acro
        } else {
          new_value <- readline(need_prompt)
        }
      } else {
        new_value <- readline(need_prompt)
      }
      if (new_value == "") {
        if (value_checks[["provided"]] != "") {
          # Use the value provided to the function for this column
          new_value <- value_checks[["provided"]]
        } else if (value_checks[["default"]] != "") {
          # Let the database use its default value
          new_value <- ""
        }
      }
      meets_unique <- FALSE
      while (!meets_unique) {
        while (
          all(
            need_this %in% required,
            any(new_value == "",
                is.null(new_value),
                is.na(new_value))
          )
        ) {
          new_value <- readline(need_prompt)
        }
        if (need_this %in% need_unique) {
          meets_unique <- !check_for_value(new_value, db_table, need_this)$exists
          if (!meets_unique) {
            log_it("warn", glue::glue("That {need_this} is already taken. Please provide a unique value."), log_ns)
            new_value <- ""
          }
        } else {
          meets_unique <- TRUE
        }
        while (all(
          need_this == "orcid",
          new_value != "",
          !str_detect(new_value, "[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9X]"))
        ) {
          log_it("error", 'Valid ORCIDs must be of the pattern "0000-0000-0000-000X" where "0" is a number and "X" may be a number or the character "X" (upper case only).', log_ns)
          new_value <- readline('ORCID (0000-0000-0000-000X) (optional but strongly encouraged): ')
        }
      }
      if (!need_this %in% names(new_values)) {
        new_values <- c(
          new_values,
          setNames(new_value, need_this)
        )
      } else {
        if (new_value != new_values[[need_this]]) {
          new_values[[need_this]] <- new_value
        }
      }
      # Resolve additional normalization values (for N>1 tables)
      norm_refs <- database_map[[db_table]]$references
      if (length(norm_refs) > 0) {
        is_normalized <- grepl(need_this, norm_refs)
        if (is_normalized) {
          log_it("info", glue::glue("{prompt_field} is a normalized field and may need additional information."), log_ns)
          normalizes_to <- grep(need_this, database_map[[db_table]]$references, value = TRUE) %>%
            str_remove_all("\\)") %>%
            str_split(" REFERENCES |\\(") %>%
            .[[1]]
          chosen_value <- resolve_normalization_value(
            this_value = new_values[[need_this]],
            db_table = normalizes_to[2],
            case_sensitive = FALSE,
            db_conn = db_conn,
            id_column = id_column,
            log_ns = log_ns
          )
          if (is.null(chosen_value)) {
            return(NULL)
          } else {
            new_values[[need_this]] <- chosen_value
          }
        }
      }
    }
  } else {
    if (exists("log_it")) {
      log_it("error", msg, "db")
    } else {
      cat("ERROR", msg, "\n")
    }
    return(NULL)
  }
  if (exists("log_it")) log_it("trace", glue::glue('Addding normalization values to table "{db_table}"'), log_ns)
  res <- try(
    build_db_action("insert", db_table, values = list(new_values))
  )
  if (inherits(res, "try-error")) {
    msg <- sprintf(
      'Unable to add normalization values (%s) to table "%s": %s',
      lapply(names(new_values),
             function(x) {
               sprintf('%s = "%s"', x, new_values[[x]])
             }) %>%
        paste0(collapse = ", "),
      db_table,
      str_remove_all(res[[1]], "^[[:alpha:]]* : |\n")
    )
    if (exists("log_it")) {
      log_it("error", msg, "db")
    } else {
      cat("ERROR", msg, "\n")
    }
    this_id <- NULL
  } else {
    last_insert_rowid <- dbGetQuery(con,
                                    sprintf("select seq from sqlite_sequence where name = '%s'",
                                            db_table)
    )$seq
    this_id <- build_db_action(action = "get_id",
                               table_name = db_table,
                               match_criteria = list(rowid = last_insert_rowid))
  }
  if (exists("log_it")) log_fn("end")
  return(this_id)
}

#' Check for a value in a database table
#'
#' This convenience function simply checks whether a value exists in the
#' distinct values of a given column. Only one column may be searched at a time;
#' serialize it in other code to check multiple columns. It leverages the
#' flexibility of [build_db_action] to do the searching. The `values` parameter
#' will be fed directly and can accept the nested list structure defined in
#' [clause_where] for exclusions and like clauses.
#'
#' @param values CHR vector of the values to search
#' @param db_table CHR scalar of the database table to search
#' @param db_column CHR scalar of the column to search
#' @param case_sensitive LGL scalar of whether to match on a case sensitive
#'   basis (the default TRUE searches for values as-provided) or whether to
#'   coerce value matches by upper, lower, sentence, and title case matches
#' @param db_conn connection object (default: con)
#' @param fuzzy LGL scalar of whether to do a "fuzzy" match in the sense that
#'   values provided are used in an SQL LIKE clause bookended with wildcards; 
#'   overrides the `case_sensitive` setting if TRUE (default: FALSE).
#'
#' @return LIST of length 1-2 containing "exists" as a LGL scalar for whether
#'   the values were found, and "values" containing the result of the database
#'   call, a data.frame object containing matching rows or NULL if exists ==
#'   FALSE.
#' @export
#'
#' @usage
#' con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' alphabet <- dplyr::tibble(lower = letters, upper = LETTERS)
#' dplyr::copy_to(con2, alphabet)
#' check_for_value("A", "alphabet", "upper", db_conn = con2)
#' check_for_value("A", "alphabet", "lower", db_conn = con2)
#' check_for_value(letters[1:10], "alphabet", "lower", db_conn = con2)
check_for_value <- function(values, db_table, db_column, case_sensitive = TRUE, db_conn = con, fuzzy = FALSE) {
  if (exists("log_it")) log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values         = list(c("n>=", 1)),
        db_table       = list(c("mode", "character"), c("length", 1)),
        db_column      = list(c("mode", "character"), c("length", 1)),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        db_conn        = list(c("length", 1)),
        fuzzy          = list(c("mode", "logical"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  to_match <- setNames(list(values), db_column)
  if (fuzzy) {
    case_sensitive <- FALSE
  }
  existing_values <- build_db_action(action         = "SELECT",
                                     table_name     = db_table,
                                     db_conn        = db_conn,
                                     match_criteria = to_match,
                                     case_sensitive = case_sensitive,
                                     fuzzy          = fuzzy,
                                     distinct       = TRUE)
  found <- nrow(existing_values) > 0
  out   <- list(
    exists = found,
    values = if (found) existing_values else NULL
  )
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Utility function to resolve multiple choices interactively
#'
#' This function is generally not called directly, but rather as a workflow
#' component from within [resolve_normalization_value] during interactive
#' sessions to get feedback from users during the normalization value resolution
#' process.
#'
#' @param values CHR vector of possible values
#' @param search_value CHR scalar of the value to search
#' @param as_regex LGL scalar of whether to treat `search_value` as a regular
#'   expression string (TRUE) or to use it directly (FALSE, default)
#' @param db_table CHR scalar name of the database table to search, used for
#'   printing log messages only (default: "")
#'
#' @return CHR scalar result of the user's choice
#' @export
#'
resolve_multiple_values <- function(values, search_value, as_regex = FALSE, db_table = "") {
  if (exists("log_it")) log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values       = list(c("n>=", 0)),
        search_value = list(c("length", 1)),
        as_regex     = list(c("mode", "logical"), c("length", 1)),
        db_table     = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  present  <- any(grepl(search_value, values, fixed = !as_regex))
  # Added 2024-06-18 to catch multiple similar normalization entries
  if (is.vector(values)) {
    only_one <- sum(values %in% search_value) == 1
    select_from <- NULL
  } else if (is.data.frame(values)) {
    only_one <- values %>%
      filter(if_any(everything(), .fns = ~ . == search_value)) %>%
      nrow() == 1
    if (only_one && nrow(values) > 1) {
      if (ncol(values) > 1) {
        col_index <- unname(which(sapply(values, \(x) search_value %in% x)))
      } else {
        col_index <- 1
        select_from <- names(values)[1]
      }
      chosen_value <- values[[col_index]][which(values[[col_index]] == search_value)]
      select_from <- names(values)[col_index]
    }
  }
  # ---
  if (present && only_one) {
    chosen_value <- search_value
  } else {
    if (is.data.frame(values)) {
      if (nrow(values) > 0) print(values)
      i <- ifelse("id" %in% names(values), which(names(values) == "id") + 1, 1)
      select_from <- names(values)[i]
      select_vals <- values[, i]
    } else {
      select_vals <- values
    }
    select_vals <- c("(Abort)", sprintf("(New) %s", search_value), select_vals)
    msg <- sprintf('\n%s values matching "%s" were identified%s.',
                   ifelse(all(present, !only_one), "Multiple", "No"),
                   search_value,
                   ifelse(db_table == "",
                          "",
                          sprintf(' in table "%s"', db_table))
    )
    if (interactive()) {
      chosen_value <- select.list(
        choices = select_vals,
        title = paste(msg,
                      sprintf("\nPlease select a number from this list:%s (Abort) to abort this operation, or (New) to add this value to the normalization table.",
                              ifelse(is.data.frame(values) && nrow(values) > 0,
                                     sprintf(" to match with a value from the table above in the '%s' column to associate with that record,",
                                             select_from),
                                     "")
                      ))
      )
    } else {
      stop("Non-interactive session. ", msg)
    }
  }
  chosen_value <- setNames(chosen_value, select_from)
  if (exists("log_it")) log_fn("end")
  return(chosen_value)
}

#' Resolve a normalization value against the database
#'
#' Normalized SQL databases often need to resolve primary keys. This function
#' checks for a given value in a given table and either returns the matching
#' index value or, if a value is not found and `interactive()` is TRUE, it will
#' add that value to the table and return the new index value. It will look for
#' the first matching value in all columns of the requested table to support
#' loose finding of identifiers and is meant to operate only on normalization
#' tables (i.e. look up tables).
#'
#' The search itself is done using [check_for_value].
#'
#' @note This is mostly a DRY convenience function to avoid having to write the
#'   loookup and add logic each time.
#' @note Interactive sessions are required to add new values
#' 
#' @inheritParams check_for_value
#'
#' @param this_value CHR (or coercible to) scalar value to look up
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#' @param ... other values to add to the normalization table, where names must
#'   match the table schema
#'
#' @return The database primary key (typically INT) of the normalized value
#' @export
resolve_normalization_value <- function(this_value,
                                        db_table,
                                        id_column = "id",
                                        case_sensitive = FALSE,
                                        fuzzy = FALSE,
                                        db_conn = con,
                                        log_ns = "db",
                                        ...) {
  if (exists("log_it")) log_fn("start")
  # Check connection
  stopifnot(active_connection(db_conn))
  db_table <- resolve_table_name(db_table, db_conn)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(this_value, db_table, case_sensitive, fuzzy, log_ns),
      conditions = list(
        this_value     = list(c("mode", "character"), c("length", 1), "not_empty"),
        db_table       = list(c("mode", "character"), c("length", 1)),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        fuzzy          = list(c("mode", "logical"), c("length", 1)),
        log_ns         = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  this_id <- integer(0)
  if (!case_sensitive || fuzzy) {
    table_fields <- dbListFields(db_conn, db_table)
    # Accelerate this portion
    argument_verification <- VERIFY_ARGUMENTS
    if (argument_verification) {
      assign("VERIFY_ARGUMENTS", FALSE, envir = .GlobalEnv)
    }
    check <- lapply(table_fields,
                    function(x) {
                      if (x != id_column) {
                        tmp <- check_for_value(
                          values = this_value,
                          db_table = db_table,
                          db_column = x,
                          fuzzy = fuzzy,
                          case_sensitive = case_sensitive)
                        if (tmp$exists) {
                          tmp$values
                        } else {
                          NULL
                        }
                      }
                    })
    if (argument_verification) {
      assign("VERIFY_ARGUMENTS", argument_verification, envir = .GlobalEnv)
    }
    check <- check[-which(sapply(check, is.null))] %>%
      bind_rows() %>%
      distinct()
    match_fail <- nrow(check) == 0
  } else {
    check <- dbReadTable(db_conn, db_table) %>%
      filter(if_any(everything(), .fns = ~ .x == this_value)) %>%
      distinct()
    match_fail <- nrow(check) == 0
    if (match_fail) {
      log_it("info",
             glue::glue("No direct match for '{this_value}' found in table '{db_table}'."),
             log_ns)
      if (fuzzy) {
        log_it("info",
               glue::glue("Executing fuzzy match on table '{db_table}'."),
               log_ns)
        check <- dbReadTable(db_conn, db_table) %>%
          filter(if_any(everything(), .fns = ~ grepl(this_value, .x))) %>%
          distinct()
        match_fail <- nrow(check) == 0
        if (match_fail) {
          log_it("info",
                 glue::glue("No fuzzy matches found for '{this_value}' in table '{db_table}'."),
                 log_ns)
        } else {
          possibles <- grep(
            unlist(check),
            pattern = this_value,
            ignore.case = TRUE,
            value = TRUE
          )
          log_it("info",
                 glue::glue("Fuzzy match {format_list_of_names(possibles, add_quotes = TRUE)} found for '{this_value}' in table '{db_table}'."),
                 log_ns)
        }
      }
    }
  }
  if (match_fail) {
    check <- dbListFields(db_conn, db_table)
    names(check) <- check
    msg <- glue::glue("No case {ifelse(case_sensitive, 'sensitive', 'insensitive')}{ifelse(fuzzy, ' fuzzy', '')} matches found.")
    log_it("info", msg, log_ns)
  }
  if (!id_column %in% names(check)) {
    log_it("warn", glue::glue("Expected but could not find an '{id_column}' column. Is '{db_table}' a normalization table containing '{id_column}'?"), log_ns)
    return(NULL)
  } else {
    this_id <- check[[id_column]]
  }
  if (check[[id_column]] == id_column || !length(this_id) == 1) {
    if (interactive()) {
      if (match_fail) {
        tmp <- dbReadTable(db_conn, db_table)
      } else {
        tmp <- check
      }
      if (id_column %in% names(tmp)) {
        tmp <- tmp %>%
          select(-!!id_column)
      } else {
        # Assume the first column is the id
        tmp <- tmp %>%
          select(-1)
      }
      these_choices <- tmp
      if (length(these_choices) == 0) {
        this_selection <- paste("(New)", this_value)
      } else {
        this_selection <- resolve_multiple_values(these_choices, this_value, db_table = db_table)
      }
      if (str_detect(this_selection, "\\(Abort\\)")) {
        msg <- glue::glue("Normalization check for '{this_value}' in table '{db_table}' aborted by user.") 
        if (exists("log_it")) {
          log_it("info", msg, log_ns)
          return(invisible(NULL))
        }
      } else if (str_detect(this_selection, "\\(New\\) ")) {
        this_selection <- sapply(this_selection, str_remove, "\\(New\\) ") %>%
          setNames(names(tmp)[1])
        kwargs <- append(this_selection, list(...))
        to_add <- append(
          kwargs,
          c(db_table = db_table,
            db_conn = db_conn,
            log_ns = log_ns)
        )
        this_id <- do.call(add_normalization_value, to_add)
      } else {
        this_id <- build_db_action("get_id", db_table, match_criteria = this_selection)
      }
    } else {
      msg <- glue('Multiple entries in "{db_table}" match value "{this_value}')
      if (exists("log_it"))  {
        log_it("error", msg, log_ns)
      } else {
        cat("ERROR", msg, "\n")
      }
      this_id <- NULL
    }
  }
  if (exists("log_it")) log_fn("end")
  return(this_id)
}

#' Get the name of a linked normalization table
#'
#' Extract the name of a normalization table from the database given a table and
#' column reference.
#'
#' @note This requires an object of the same shape and properties as those
#'   resulting from [er_map] as `this_map`.
#'
#' @param table_name CHR scalar name of the database table
#' @param table_column CHR scalar name of the foreign key table column
#' @param this_map LIST object containing the schema representation from
#'   `er_map` (default: an object named "db_map" created as part of the package
#'   spin up)
#' @param fk_refs_in CHR scalar name of the item in `this_map` containing the
#'   SQL "REFERENCES" statements extracted from the schema
#'
#' @return CHR scalar name of the table to which a FK column is linked or an
#'   empty character string if no match is located (i.e. `table_column` is not a
#'   defined foreign key).
#' @export
#' 
#' @usage ref_table_from_map("table1", "fk_column1", er_map(con), "references")
#'
ref_table_from_map <- function(table_name, table_column, this_map = db_map, fk_refs_in = "references") {
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace", sprintf('Getting normalization table reference for column  "%s" in table "%s"', table_column, table_name), "db")
  }
  refs <- this_map[[table_name]][[fk_refs_in]]
  table_column <- sprintf("^%s", table_column)
  refers_to <- grep(table_column, refs, value = TRUE)
  if (length(refers_to) == 1) {
    refers_to <- refers_to %>%
      str_remove_all("^[[:alnum:]-_]* REFERENCES |\\([[:alpha:]]*\\)$") %>%
      str_trim()
  }
  if (exists("log_it")) log_fn("end")
  return(refers_to)
}

# TODO - abstract the appropriate ms_n - 1 scan to associate with ms_data
associated_scan <- function(df, scan_time) {
  if (exists("log_it")) log_fn("start")
  scan_msn  <- df$msn[which(df$scantime == scan_time)] - 1
  out <- df$scantime[
    which(all(df$scantime < scan_time,
              df$msn == scan_msn))
  ]
  if (exists("log_it")) log_fn("end")
  return(out)
}

#' Convenience function to set a new installation code
#'
#' @param db_conn connection object (default "con")
#' @param new_name CHR scalar of the human readable name of the installation
#'   (e.g. your project name) (default: NULL)
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: "db")
#'
#' @return None
#' @export
#'
make_install_code <- function(db_conn = con, new_name = NULL, log_ns = "db") {
  stopifnot(dbIsValid(db_conn),
            is.character(new_name),
            length(new_name) == 1)
  old_vals <- dbReadTable(db_conn, "config") %>%
    filter(id == 0)
  old_code <- old_vals$code
  old_name <- old_vals$name
  use_name <- ifelse(is.null(new_name),
                     "",
                     sqlInterpolate(db_conn,
                                    ", name = ?", new_name))
  dbExecute(db_conn,
            glue::glue("update config set code = hex(randomblob(8)){use_name} where id = 0"))
  new_code <- tbl(db_conn, "config") %>%
    filter(id == 0) %>%
    pull(code)
  log_it("info", glue::glue(
    "Updating install code from {old_code} to {new_code}{ifelse(use_name == '', sprintf(' with %s', new_name), '')}."
  ))
  old_vals <- toJSON(list(code = old_code, name = old_name))
  new_vals <- toJSON(list(code = new_code, name = new_name))
  set_cols <- "category, description, bundle, effect, affects_table, executed_from, new_vals, old_vals"
  set_vals <- glue::glue("'build', 'mint new install code', 'install', 2, 'config', 'script', '{new_vals}', '{old_vals}'")
  dbExecute(db_conn,
            glue::glue("insert into logs ({set_cols}) values ({set_vals})"))
}
