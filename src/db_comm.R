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
#' @examples
pragma_table_def <- function(db_table, db_conn = con, get_sql = FALSE, pretty = TRUE) {
  require(dplyr)
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace",
           sprintf('Getting table definition for "%s".', format_list_of_names(db_table)),
           ns = "db")
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
  db_table <- table_exists(db_table, db_conn)
  
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
                  return(tmp)
                }
  )
  # Shape up the return
  if (get_sql) {
    out <- dplyr::bind_rows(out) %>%
      select(table_name, type, sql)
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
      select(cid, table_name, name:pk)
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
#' @return
#' @export
#'
#' @examples
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
  db_table <- table_exists(db_table, db_conn)
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
#' @examples
build_db <- function(db            = DB_NAME,
                     build_from    = DB_BUILD_FILE,
                     populate      = TRUE,
                     populate_with = DB_DATA,
                     archive       = FALSE,
                     sqlite_cli    = SQLITE_CLI,
                     connect       = FALSE) {
  require(glue)
  logger <- exists("log_it")
  if (exists(logger)) {
    log_fn("start")
    log_it("trace", glue('Starting build of "{db}".'), "db")
  }
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db            = list(c("mode", "character"), c("length", 1)),
        build_from    = list(c("mode", "character"), c("length", 1)),
        populate      = list(c("mode", "logical"), c("length", 1)),
        populate_with = list(c("mode", "character"), c("length", 1)),
        archive       = list(c("mode", "logical"), c("length", 1)),
        sqlite_cli    = list(c("mode", "character"), c("length", 1)),
        connect       = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "build_db"
    )
    stopifnot(arg_check$valid)
  }
  if (exists(logger)) log_it("trace", glue('Attempting to connect to "{db_table}".'), "db")
  manage_connection(db = db, reconnect = FALSE, disconnect = TRUE)
  build_file    <- list.files(pattern = build_from,
                              full.names = TRUE,
                              recursive = TRUE)
  # Populate with all data sources
  # -- RSQLite does not allow for the CLI [.DOT] commands
  # -- If sqlite3 CLI is installed, use that by preference
  sqlite_available <- length(Sys.which(sqlite_cli) > 1)
  # Do it the short way (with CLI)
  if (sqlite_available) {
    if (exists(logger)) log_it("trace", glue('SQLite CLI under alias "{sqlite_cli}" is available. Building directly...'), "db")
    sqlite_call <- glue('{sqlite_cli} {db}')
    if (!file.exists(build_file)) {
      stop(glue('Cannot locate file "{build_file}" in this directory.'))
    }
    build_cmd   <- glue('{sqlite_call} -cmd ".read {build_file}" -cmd ".exit"')
    if (file.exists(db)) remove_db(db = db, archive = archive)
    if (exists(logger)) log_it("info", glue('Building database "{db}".'), "db")
    if (exists(logger)) log_it("trace", glue('Issuing shell command to build the database as\n{build_cmd}'), "db")
    shell(build_cmd)
    if (populate) {
      populate_file <- list.files(pattern = populate_with, full.names = TRUE, recursive = TRUE)
      if (!file.exists(populate_file)) {
        if (exists(logger)) log_it("warn", glue('Cannot locate file "{populate_file}" in this directory; "{db}" will be created but not populated.'), "db")
        populate_cmd <- ""
      } else {
        if (exists(logger)) log_it("info", glue('Populating from "{populate_file}".'), "db")
        populate_cmd <- glue('{sqlite_call} -cmd ".read {populate_file}" -cmd ".exit"')
        if (exists(logger)) log_it("trace", glue('Issuing shell command to populate the database as\n{populate_cmd}'), "db")
        shell(populate_cmd)
      }
    }
    if (exists(logger)) log_it("info", glue('Finished attempted build of "{db}" as specified. Check console for any failure details.'), "db")
  } else {
    # Do it the long way
    if (exists(logger)) log_it("trace", glue('SQLite CLI under alias "{sqlite_cli}" is not available. Building through R...'), "db")
    # -- Ensure packages are available
    reqs <- c("stringr", "magrittr", "readr", "DBI")
    packs_available <- reqs %in% installed.packages()
    if (!all(packs_available)) {
      stop("Some packages were not available. Please install packages:", paste0(reqs[!packs_available]))
    }
    invisible(lapply(reqs, require, character.only = TRUE))
    # -- Remove the existing database
    if (file.exists(db)) {
      if (exists(logger)) log_it("trace", glue('Removing "{db}".'))
      remove_db(db = db, archive = archive)
    }
    # -- Create the build commands in R to pass through RSQLite::SQLite()
    if (exists(logger)) log_it("trace", glue('Creating build statements.'), "db")
    build_path <- list.files(pattern = build_file,
                             full.names = TRUE,
                             recursive = TRUE)
    if (!length(build_path) == 1) {
      stop(sprintf('Could not find build file "%s" in this directory.',
                   build_path))
    }
    build_statement <- create_fallback_build(build_path)
    build_statement <- build_statement[nchar(build_statement) > 1]
    # -- Create the database and read in the build statements
    if (exists(logger)) log_it("trace", glue('Creating new database as "{db}".'), "db")
    con <- dbConnect(RSQLite::SQLite(), db)
    if (exists(logger)) log_it("trace", glue('Building "{db}"...'), "db")
    invisible(
      lapply(build_statement,
             function(x) dbSendStatement(con, str_trim(x, "both")))
    )
    # Disconnect
    dbDisconnect(con)
  }
  if (connect) {
    if (exists(logger)) log_it("trace", glue('Connecting to "{db}".'), "db")
    manage_connection(db = db, reconnect = TRUE)
  }
  if (exists(logger)) log_fn("end")
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
#' @examples
remove_db <- function(db = DB_NAME, archive = FALSE) {
  require(tools)
  logger <- exists("log_it")
  if (exists(logger)) {
    log_fn("start")
    log_it("trace", glue('Starting removal of "{db}"...'), "db")
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db      = list(c("mode", "character"), c("length", 1)),
        archive = list(c("mode", "logical"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Resolve database file location
  if (exists(logger)) log_it("trace", glue('Finding database file "{db}"'), "db")
  db_path  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
  db_path  <- db_path[basename(db_path) == db]
  if (length(db_path) == 0) {
    if (exists(logger)) log_it("error", sprintf('Database "%s" does not exist in this directory tree.', db), "db")
    return(NULL)
  } else if (length(db_path) > 1) {
    if (exists(logger)) log_it("warn", glue('Multiple files found for "{db}" in this directory.'), "db")
    # correct_path <- select.list(db_path, title = "Please select one.")
    correct_path <- resolve_multiple_values(db_path, db)
  }
  if (length(db_path) == 1) {
    # Ensure no current connection from R
    check <- try(manage_connection(db = db, reconnect = FALSE, disconnect = TRUE))
    if (class(check) == "try-error") stop("Unable to automatically stop the connection to '", db, "'.")
    if (archive) {
      now       <- format(Sys.time(), "%Y%m%d%H%M%S%Z")
      fname     <- file_path_sans_ext(db)
      new_fname <- gsub(fname,
                        sprintf("%s_archive_%s",fname, now),
                        db)
      file.copy(db_path, new_fname)
      if (exists(logger)) log_it("success", sprintf('Archive created as "%s"', new_fname), "db")
    }
    result <- try(file.remove(db_path))
    if (class(result) == "try-error") {
      if (exists(logger)) log_it("error", sprintf('Database "%s" could not be removed; another connection is likely open.', db), "db")
    } else {
      if (exists(logger)) log_it("success", sprintf('Database "%s" removed.', db), "db")
    }
  }
  if (exists(logger)) log_fn("end")
}

#' Check presence of a database table
#'
#' This convenience function checks for the existence of one or more `db_table`
#' objects in a database.
#'
#' @param db_table CHR vector of table names to check
#' @param db_conn connection object (default: con)
#'
#' @return CHR vector of existing tables
#' @export
#'
#' @examples
table_exists <- function(db_table, db_conn = con) {
  if (exists("log_it")) log_fn("start")
  tables_exist <- db_table %in% dbListTables(db_conn)
  if (!all(tables_exist)) {
    if (exists("log_it")) {
      log_it("warn", sprintf('No table named "%s" was found in this schema.', db_table[!tables_exist]), "db")
    }
    db_table <- db_table[tables_exist]
  }
  if (length(db_table) == 0) stop("No valid tables were found in this schema.")
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
#' @examples
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
#' @examples
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
#' Use [data_dictionary] and save the output to a local file. If `output_format`
#' is one of "data.frame" or "list", the resulting file will be saved as an RDS.
#' Parameter `output_file` will be used during the save process; relative paths
#' are fine and will be identified by the current working directory.
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
#' @return
#' @export
#'
#' @examples
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
    switch(output_format,
           "json"       = out %>%
             jsonlite::toJSON() %>%
             write_file(f_name),
           "csv"        = out %>%
             dplyr::bind_rows() %>%
             readr::write_csv(f_name),
           "data.frame" = out %>%
             dplyr::bind_rows() %>%
             write_rds(f_name),
           "list"       = out %>%
             write_rds(f_name)
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
#' SQL is generated from [pragma_table_def] with argument `get_sql` = TRUE and
#' ignores entities whose names start with "sqlite".
#'
#' @param db_conn connection object, specifically of class "SQLiteConnection" but
#'   not strictly enforced
#'
#' @return nested LIST object
#' @export
#'
#' @examples
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
  ref_tables <- str_extract_all(build_statements$sql, "REFERENCES [:word:]+") %>%
    lapply(str_remove_all, "REFERENCES ")
  refs     <- str_extract_all(build_statements$sql, "(FOREIGN KEY \\([:word:]+\\) )?REFERENCES [:word:]+\\([:word:]+\\)") %>%
    lapply(str_remove_all, "FOREIGN KEY \\(") %>%
    lapply(str_replace_all, "\\) ", " ")
  used_in  <- str_extract_all(build_statements$sql, "(JOIN|FROM) [:word:]+") %>%
    lapply(str_remove_all, "(JOIN|FROM) ")
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
#' tested extensively drivers other than SQLite.
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
#' @param ... named list of any other connection parameters required for your
#'   database driver (e.g. postgres username/password)
#'
#' @return None
#' @export
#'
#' @note For more complicated setups, it may be easier to use this function by
#'   storing parameters in a list and calling with [base::do.call()]
#'
#' @examples
manage_connection <- function(db          = DB_NAME,
                              drv_pack    = DB_PACKAGE,
                              drv         = DB_DRIVER,
                              conn_class  = DB_CLASS,
                              conn_name   = "con",
                              is_local    = TRUE,
                              rm_objects  = TRUE,
                              reconnect   = TRUE,
                              disconnect  = TRUE,
                              .environ    = .GlobalEnv,
                              ...) {
  logger <- exists("log_it")
  if (logger) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db, drv_pack, drv, conn_class, conn_name, is_local, rm_objects, reconnect, disconnect),
      conditions = list(
        db         = list(c("mode", "character"), c("length", 1)),
        drv_pack   = list(c("mode", "character"), c("length", 1)),
        drv        = list(c("mode", "character"), c("length", 1)),
        conn_class = list(c("mode", "character"), c("length", 1)),
        conn_name  = list(c("mode", "character"), c("length", 1)),
        is_local   = list(c("mode", "logical"), c("length", 1)),
        rm_objects = list(c("mode", "logical"), c("length", 1)),
        reconnect  = list(c("mode", "logical"), c("length", 1)),
        disconnect = list(c("mode", "logical"), c("length", 1))
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
        if (logger) log_warn('Could not automatically identify the connection properties for object "{env}". To avoid hanging connections, it should be removed explicitly.', "db")
        connection <- ""
      }
      if (
        any(
          all(basename(connection) == db,
              this_obj_class %in% connection_object_classes),
          class(this_obj) == conn_class
        )
      ){
        connected <- dbIsValid(this_obj)
        if (logger) log_it("trace", glue('Database "{db}" is currently open.'), "db")
        if (all(connected, disconnect)) {
          check <- try(invisible(dbDisconnect(this_obj)))
          status <- ifelse(class(check)[1] == "try-error",
                           "unable to be disconnected",
                           "disconnected")
          log_it("trace", glue('Database "{db}" {status}.'))
          connected <- dbIsValid(this_obj)
        }
      }
      if (logger) log_it("trace", glue('Closing and removing "{env}"...'), "db")
      if (all(rm_objects, disconnect, !connection == "")) {
        rm(list = env, pos = ".GlobalEnv")
      }
    }
  }
  if (reconnect) {
    if (is_local) {
      # Resolve database file location
      if (logger) log_it("trace", glue('Finding local database file "{db}"'), "db")
      db_where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
      if (length(db_where) == 0) {
        stop(sprintf('Unable to locate "%s".', db))
      }
    }
    global_env       <- as.list(.environ)
    global_env_names <- names(global_env)
    if (conn_name %in% global_env_names) {
      if (connected) dbDisconnect(sym(conn_name))
      rm(list = conn_name, pos = ".GlobalEnv")
    }
    args <- list(db, ...)
    assign(x     = conn_name,
           value = try(do.call("dbConnect", args = c(drv = do.call(drv, list()), args))),
           envir = .GlobalEnv)
    if (class(eval(sym(conn_name))) == "try-error") {
      stop(sprintf('Unable to connect to "%s".\nCall attempted was "assign(x = %s, value = try(do.call("dbConnect", args = c(drv = do.call(%s, list()), %s)))',
                   db,
                   drv,
                   paste0(args, collapse = ", ")))
    } else {
      if (dbIsValid(eval(sym(conn_name)))) if (logger) log_it("trace", glue('"{db}" connected as "{conn_name}".'))
    }
  }
  if (logger) log_fn("end")
}

#' Parse SQL build statements
#'
#' Reading SQL files directly into R can be problematic. This function is
#' primarily called in [create_fallback_build].
#'
#' All arguments `magicsplit`, `header`, and `section` provide flexibility in
#' the comment structure of the SQL file and accept regex for character matching
#' purposes.
#'
#' @param sql_statements CHR vector of SQL build statements from an SQL file.
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
#' @examples
#' example_file <- "./config/sql_nodes/reference.sql"
#' if (file.exists(example_file)) {
#'   build_commands <- readr::read_file(example_file)
#'   sqlite_parse_build(build_commands)
#' }
sqlite_parse_build <- function(sql_statements,
                               magicsplit = "/\\*magicsplit\\*/",
                               header     = "/\\*\\=+\\r*\\n[[:print:][:cntrl:]]+\\r*\\n\\=+\\*/",
                               section    = "/\\* -* [[:print:][:cntrl:]]+ -* \\*/") {
  if (exists("log_it")) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sql_statements = list(c("mode", "character"), c("length", 1)),
        magicsplit     = list(c("mode", "character"), c("length", 1)),
        header         = list(c("mode", "character"), c("length", 1)),
        section        = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "sqlite_parse_build"
    )
    stopifnot(arg_check$valid)
  }
  if (exists("log_it")) log_it("trace", glue('Parsing \n{sql_statements}\n\tfor build commands.'), "db")
  to_remove <- paste0(c(header, "\\t", "\\r"), collapse = "|")
  out       <- sql_statements %>%
    str_replace_all("CREATE ", paste0(magicsplit, "CREATE ")) %>%
    str_replace_all("\\.import ", paste0(magicsplit, ".import ")) %>%
    str_replace_all("DELETE FROM ", paste0(magicsplit, "DELETE FROM ")) %>%
    str_replace_all("\\;\\nINSERT ", paste0("; ", magicsplit, "\nINSERT ")) %>%
    str_split(magicsplit) %>%
    .[[1]] %>%
    str_remove_all(to_remove) %>%
    str_replace_all("\\n{2,10}", "\n") %>%
    str_replace_all(" {2,10}", " ") %>%
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
#' @examples
#' if (file.exists("./config/data/elements.csv")) {
#'   sqlite_parse_import(".import --csv --skip 1 ./config/data/elements.csv elements")
#' }
sqlite_parse_import <- function(build_statements) {
  logger <- exists("log_it")
  if (logger) log_fn("start")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
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
  if (logger) log_it("trace", glue('Parsing \n{build_statements}\n for import modification.'), "db")
  out           <- lapply(build_statements,
                          function(x) {
                            if (grepl("\\.import", x)) {
                              data_type <- str_extract(x, regex_import) %>%
                                str_remove(".import( --)?") %>%
                                str_squish()
                              if (data_type == "") {
                                data_type_read <- FALSE
                                if (logger) log_it("warn", 'No data type identified. Assuming a ".csv" extension.', "db")
                                data_type <- "csv"
                              } else {
                                data_type_read <- TRUE
                              }
                              skip_rows <- str_extract(x, regex_skip) %>%
                                str_remove("--skip ") %>%
                                str_squish()
                              skip_rows <- try(as.numeric(skip_rows))
                              if (class(skip_rows) == 'try-error') {
                                if (logger) log_it("warn", glue('Could not convert "{skip_rows}" to numeric in "{x}".'), "db")
                                return(x)
                              }
                              tmp <- str_remove(x, regex_prefix) %>%
                                str_split(" ") %>%
                                .[[1]]
                              if (length(tmp) != 2) {
                                if (logger) log_it("warn", glue('Could not parse "{x}" to identify both a target file and database target table.'), "db")
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
                                  if (logger) log_it("warn", glue('Cannot read file "{data_file}". Function "{read_func}" is not available.'), "db")
                                  return(x)
                                }
                              } else {
                                if (logger) log_it("warn", glue('Could not find file "{data_file}".'), "db")
                                return(x)
                              }
                              target    <- tmp[2]
                              insert_values <- sprintf('("%s")',
                                                       read_data %>%
                                                         unite(col = "statement", sep = '", "') %>%
                                                         pull(statement) %>%
                                                         paste0(collapse = '"), ("')
                              )
                              insert_statement <- sprintf("INSERT INTO `%s` VALUES %s;",
                                                          target,
                                                          insert_values)
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
#' @param out_file CHR scalar of the output file name and destination. The
#'   default, NULL, will write to a file named similarly to `build_file`
#'   suffixed with "_full".
#'
#' @return None: a file will be written at `out_file` with the output.
#' @export
#'
#' @examples
create_fallback_build <- function(build_file    = NULL,
                                  populate      = TRUE,
                                  populate_with = NULL,
                                  driver        = "SQLite",
                                  out_file      = NULL) {
  if (exists("log_it")) log_fn("start")
  if (all(is.null(build_file), exists("DB_BUILD_FILE"))) build_file <- DB_BUILD_FILE
  if (all(is.null(populate_with), exists("DB_DATA"))) populate_with <- DB_DATA
  build_files <- list.files(pattern = build_file,
                            full.names = TRUE,
                            recursive = TRUE)
  if (length(build_files) > 1) {
    if (interactive()) {
      build_file <- resolve_multiple_values(build_files, build_file)
    } else {
      stop(msg)
    }
  } else if (length(build_files) == 1) {
    build_file <- build_files
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
      ),
      from_fn    = "create_fallback_build"
    )
    stopifnot(arg_check$valid)
  }
  comments   <- "^/\\* [[:alnum:]+ \\.\\',/_]+ \\*/$"
  has_import <- FALSE
  if (!file.exists(build_file)) {
    stop(sprintf('Could not locate file "%s".', build_file))
  }
  if (exists("log_it")) log_it("info", "Creating fall back build schema...", "db")
  build   <- read_file(build_file) %>%
    sqlite_parse_build()
  
  if (populate) {
    if (exists("log_it")) log_it("info", "Adding data insert reads...", "db")
    populate_file <- list.files(pattern = populate_with,
                                full.names = TRUE,
                                recursive = TRUE)
    populate <- read_file(populate_file) %>%
      sqlite_parse_build()
    build <- c(build, populate)
  }
  
  # Make SQL build statements
  if (exists("log_it")) log_it("info", "Expanding .build statements...", "db")
  build   <- unlist(build) %>%
    as.list()
  index_read   <- grep("^.read", build)
  temp_read    <- lapply(build[index_read],
                         function(x) {
                           node_file <- str_remove(x, ".read ")
                           read_file(node_file) %>%
                             sqlite_parse_build()
                         })
  source_read  <- str_remove(unlist(build[index_read]), ".read ")
  names(temp_read) <- source_read %>%
    basename() %>%
    file_path_sans_ext()
  for (i in 1:length(temp_read)) {
    source_comment <- sprintf("/* Sourced from ./%s */", source_read[i])
    temp_read[[i]] <- c(source_comment, temp_read[[i]])
  }
  build[index_read] <- temp_read %>%
    lapply(function(x) {
      lapply(x, function(y) {
        paste0(y, collapse = " ")
      })
    })
  build <- unlist(build) %>% as.list()
  index_import <- grep("^.import", build)
  if (exists("log_it")) log_it("info", "Expanding .import statements...", "db")
  temp_import  <- lapply(build[index_import],
                         function(x) {
                           if (grepl(comments, x)) return(x)
                           lapply(x, sqlite_parse_import)
                         }
  ) %>% unlist()
  check <- build
  build[index_import] <- temp_import
  build <- unlist(build) %>%
    paste0(collapse = "\n")
  write_file(build, out_file)
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
      con <- dbConnect(RSQLite::SQLite(), db)
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
#' @note This requires references to be in place to the individual functions in
#'   the current environment.
#'
#' @param api_running LGL scalar of whether or not the API service is currently
#'   running (default: TRUE)
#' @param api_monitor process object pointing to the API service (default:
#'   NULL)
#'
#' @return Files for the new database, fallback build, and data dictionary will
#'   be created in the project directory and objects will be created in the
#'   global environment for the database map (LIST "db_map") and current
#'   dictionary (LIST "db_dict")
#' @export
#'
#' @examples
update_all <- function(api_running = TRUE, api_monitor = NULL) {
  if (exists("log_it")) log_it("debug", "Run update_all().", "db")
  if (api_running) {
    pr_name <- obj_name_check(api_monitor)
    plumber_service_existed <- exists(pr_name)
    if (plumber_service_existed) {
      api_monitor <- eval(sym(pr_name))
      if (api_monitor$is_alive()) api_stop(pr = api_monitor)
    }
  }
  manage_connection(reconnect = FALSE)
  build_db()
  manage_connection()
  create_fallback_build()
  save_data_dictionary()
  db_map <<- er_map()
  dict_file <- list.files(pattern = "data_dictionary.json",
                          full.names = TRUE) %>%
    file.info() %>%
    arrange(desc(ctime)) %>%
    slice(1) %>%
    rownames()
  db_dict <<- dict_file %>%
    jsonlite::read_json() %>%
    lapply(bind_rows)
  if (api_running) {
    if (plumber_service_existed) {
      api_reload(pr = api_monitor, background = TRUE)
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
#' @example 
#' \notrun {
#' manage_connection()
#' close_up_shop(TRUE)
#' }
close_up_shop <- function(back_up_connected_tbls = FALSE) {
  if (exists("log_it")) log_it("debug", "Run close_up_shop().", "db")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(back_up_connected_tbls),
      conditions = list(
        back_up_connected_tbls = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn = "close_up_shop"
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
      api_stop(pr = .GlobalEnv[[api]], remove_service_obj = TRUE)
    }
  }
  # Kill db connected objects
  db_connections <- names(tmp)[
    which(
      unlist(
        lapply(
          tmp,
          function(x) any(str_detect(x, "Connection$")))
      ) == TRUE
    )
  ]
  for (db_conn in db_connections) {
    if (any(str_detect(tmp[[db_conn]], "^tbl_"))) {
      if (back_up_connected_tbls) {
        assign(
          x     = db_conn,
          value = collect(eval(sym(db_conn))),
          envir = .GlobalEnv
        )
      } else {
        rm(list = db_conn, envir = .GlobalEnv)
      }
    } else {
      manage_connection(conn_name = db_conn, reconnect = FALSE)
    }
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
#' @param db_conn 
#'
#' @return
#' @export
#'
#' @examples
active_connection <- function(db_conn = con) {
  if (exists("log_it")) log_it("debug", "Run active_connection().", "db")
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
#' @param ... Values to add to the table. All values must be named.
#' @param db_conn connection object (default "con")
#'
#' @return NULL if unable to add the values, INT scalar of the new ID otherwise
#' @export
#'
#' @examples
add_normalization_value <- function(db_table, ..., db_conn = con) {
  if (exists("log_it")) log_fn("start")
  new_values <- list(...)
  if (length(names(new_values)) != length(new_values)) {
    stop("All values provided to this function must be named.")
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        new_values  = list(c("mode", "list"), c("n>=", 1)),
        db_table    = list(c("mode", "character"), c("length", 1)),
        db_conn     = list(c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (exists("db_dict")) {
    if (db_table %in% names(db_dict)) {
      table_cols <- db_dict[[db_table]]
    } else {
      table_cols <- pragma_table_info(db_table)
    }
  } else {
    table_cols <- pragma_table_info(db_table)
  }
  needed     <- table_cols %>%
    filter(name != "id") %>%
    pull(name)
  required   <- table_cols %>%
    filter(notnull == 1) %>%
    pull(name)
  new_values <- new_values[which(names(new_values) %in% needed)]
  if (!all(needed %in% names(new_values))) {
    msg <- sprintf('Not all values needed for table "%s" were supplied.', db_table)
    if (interactive()) {
      if (exists("log_it")) {
        log_it("warn",
               sprintf("%s Please provide the following values associated with %s to continue.",
                       msg,
                       format_list_of_names(
                         sprintf('"%s = %s"', names(new_values), unlist(new_values))
                       )
               ),
               "db"
        )
      }
      for (need_this in needed) {
        if (!need_this %in% names(new_values)) {
          new_value <- ""
          if (all(need_this == "acronym", "name" %in% names(new_values))) {
            auto_acro <- make_acronym(new_values$name)
            use_auto_acro <- select.list(
              choices = c("Yes", "No"),
              title = sprintf('\nUse the automatic acronym "%s" for normalization name "%s"?',
                              auto_acro,
                              new_values$name)
            )
            if (use_auto_acro == "Yes") {
              new_value <- auto_acro
            } else {
              new_value <- readline(
                sprintf("%s: ", need_this)
              )
            }
          }
          while (
            all(
              need_this %in% required,
              any(need_this == "",
                  is.null(need_this),
                  is.na(need_this))
            )
          ) {
            new_value <- readline(
              sprintf("%s: ", need_this)
            )
          }
          new_values <- c(
            new_values,
            setNames(new_value, need_this)
          )
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
  }
  if (exists("log_it")) log_it("trace", sprintf('Addding normalization values to table "%s"', db_table), "db")
  res <- try(
    build_db_action("insert", db_table, values = list(new_values))
  )
  if (class(res) == "try-error") {
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
      return(NULL)
    }
  }
  this_id <- build_db_action("get_id", db_table, match_criteria = new_values, and_or = "AND")
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
#'
#' @return LIST of length 1-2 containing "exists" as a LGL scalar for whether
#'   the values were found, and "values" containing the result of the database
#'   call, a data.frame object containing matching rows or NULL if exists ==
#'   FALSE.
#' @export
#'
#' @examples
#'
#' ## Not run:
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' alphabet <- dplyr::tibble(lower = letters, upper = LETTERS)
#' dplyr::copy_to(con, alphabet)
#' check_for_value("A", "alphabet", "upper", con)
#' check_for_value("A", "alphabet", "lower", con)
#' check_for_value(letters[1:10], "alphabet", "lower", con)
#'
#' ## End(Not run)
check_for_value <- function(values, db_table, db_column, case_sensitive = TRUE, db_conn = con) {
  if (exists("log_it")) log_it("debug", "Run check_for_value().", "db")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values         = list(c("mode", "character"), c("n>=", 1)),
        db_table       = list(c("mode", "character"), c("length", 1)),
        db_column      = list(c("mode", "character"), c("length", 1)),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        db_conn        = list(c("length", 1))
      ),
      from_fn = "check_for_value"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  existing_values <- build_db_action(con            = db_conn,
                                     action         = "SELECT",
                                     table_name     = db_table,
                                     match_criteria = setNames(list(values), db_column),
                                     case_sensitive = case_sensitive,
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
#' @param values CHR vector of possible values
#' @param search_value CHR scalar of the value to search
#' @param db_table CHR scalar of the table (for printing purposes only)
#'   (default: "")
#'
#' @return CHR scalar result of the user's choice
#' @export
#'
#' @examples
resolve_multiple_values <- function(values, search_value, db_table = "") {
  if (exists("log_it")) log_it("debug", "Run resolve_multiple_values().", "db")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values       = list(c("n>=", 0)),
        search_value = list(c("length", 1)),
        db_table     = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "resolve_multiple_values"
    )
    stopifnot(arg_check$valid)
  }
  present  <- any(grepl(search_value, values))
  only_one <- sum(values %in% search_value) == 1
  if (present && only_one) {
    chosen_value <- search_value
  } else {
    if (length(values) %in% c(0, 1)) {
      values <- c("(Abort)", sprintf("(New) %s", search_value), values)
      msg <- sprintf('\n%s values directly matching "%s" were identified%s.',
                     ifelse(all(present, !only_one), "Multiple", "No"),
                     search_value,
                     ifelse(db_table == "",
                            "",
                            sprintf(' in table "%s"', db_table))
      )
      if (interactive()) {
        chosen_value <- select.list(values, title = paste(msg, "Please select one."))
      } else {
        stop(msg)
      }
    }
  }
  if (exists("log_it")) log_fn("end")
  return(chosen_value)
}

resolve_normalization_value <- function(this_value, db_table, case_sensitive = FALSE, db_conn = con, ...) {
  if (exists("log_it")) log_it("debug", "Run resolve_normalization_value().", "db")
  # Check connection
  stopifnot(active_connection(db_conn))
  fields <- dbListFields(db_conn = db_conn, db_table)
  this_id <- integer(0)
  for (field in fields) {
    if (length(this_id) == 0) {
      tmp <- check_for_value(this_value, db_table, field, case_sensitive, db_conn)
      if (tmp$exists) {
        this_id <- tmp$values$id
      }
    }
  }
  if (length(this_id) == 1) {
    return(this_id)
  } else {
    if (interactive()) {
      if (length(this_id) == 0) {
        tmp$values <- build_db_action("select", db_table)
      }
      if ("id" %in% names(tmp$values)) {
        tmp$values <- tmp$values %>%
          select(-id)
      } else {
        tmp$values <- tmp$values[, -1]
      }
      these_choices <- tmp$values %>%
        unite(choices, everything(), sep = " - ") %>%
        pull(choices)
      if (length(these_choices) == 0) {
        this_selection <- paste("(New)", this_value)
      } else {
        this_selection <- resolve_multiple_values(these_choices, this_value, db_table = db_table)
      }
      if (str_detect(this_selection, "\\(New\\) ")) {
        this_selection <- str_remove(this_selection, "\\(New\\) ")
        to_add <- c(
          db_table = db_table,
          name = this_selection,
          list(...),
          db_conn = db_conn
        )
        this_id <- do.call(add_normalization_value, to_add)
      } else {
        this_id <- build_db_action("get_id", db_table, match_criteria = to_add)
      }
      return(this_id)
    } else {
      msg <- glue('Multiple entries in "{db_table}" match value "{this_value}')
      if (exists("log_it"))  {
        log_it("error", msg, "db")
      } else {
        cat("ERROR", msg, "\n")
      }
      return(NULL)
    }
  }
}

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

# Operation specific functions -------------------------------------------------

#' Add a contributor programmatically or interactively. If values are fed to all
#' parameters then the user will be added automatically.
#'
#' @param user_value CHR scalar primarily used for programmatic use of this
#'   function, indicating the original value provided by a user interface (e.g.
#'   shiny or from another function). This is used only to provide an
#'   interactive prompt to show the user what value was initially fed into this
#'   function. (default "")
#' @param username CHR scalar of the desired username (default "")
#' @param contact CHR scalar of the contributor's contact point (default "")
#' @param first_name CHR scalar of the contributor's first name (default "")
#' @param last_name CHR scalar of the contributor's last name (default "")
#' @param affiliation CHR scalar of the contributor's affiliation (default "")
#' @param orcid CHR scalar of the contributor's ORCID (default ""), which must
#'   match the valid ORCID pattern of four sets of four alphanumeric characters
#'   separated by dashes (e.g. "1111-2222-3333-4444")
#' @param db_conn connection object (default: con)
#'
#' @return
#' @export
#'
#' @examples
add_contributor <- function(user_value  = "",
                            username    = "",
                            contact     = "",
                            first_name  = "",
                            last_name   = "",
                            affiliation = "",
                            orcid       = "",
                            db_conn     = con) {
  if (exists("log_it")) log_it("debug", "Run add_contributor().", "db")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        user_value  = list(c("mode", "character"), c("length", 1)),
        username    = list(c("mode", "character"), c("length", 1)),
        contact     = list(c("mode", "character"), c("length", 1)),
        first_name  = list(c("mode", "character"), c("length", 1)),
        last_name   = list(c("mode", "character"), c("length", 1)),
        affiliation = list(c("mode", "character"), c("length", 1)),
        orcid       = list(c("mode", "character"), c("length", 1)),
        db_conn     = list(c("length", 1))
      ),
      from_fn = "add_contributor"
    )
    stopifnot(arg_check$valid)
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  if (interactive()) {
    cat(
      sprintf(
        'Would you like to add a user now?%s\n',
        ifelse(user_value == "",
               "",
               sprintf(' The value provided was "%s".',
                       user_value)
        )
      )
    )
    if (menu(c("Yes", "No")) != 1) {
      cat("You can always add a user later.\n")
      return(NULL)
    }
    while (username == "") username <- readline("Username (required): ")
    while (check_for_value(username, "contributors", "username")$exists) {
      cat("That username is already taken. Please select another.")
      username <- readline("Username (required): ")
    }
    while (contact == "") {
      contact     <- readline("Contact (email preferred, required): ")
      if (str_detect(contact, ";|delete|select|alter|drop|update")) {
        cat("Contact information may not include semicolons or database verbs (e.g. select, alter, etc.).")
        contact   <- ""
      }
    }
    while (first_name == "")  first_name  <- readline("First Name (required): ")
    while (last_name == "")   last_name   <- readline("Last Name (required): ")
    while (affiliation == "") affiliation <- readline("Affiliation (required): ")
    this_affiliation <- check_for_value(affiliation, "affiliations", "name", case_sensitive = FALSE)
    if (this_affiliation$exists) {
      affiliation <- this_affiliation$values$id
    }
    if (orcid == "")       orcid       <- readline("ORCID (0000-0000-0000-000X) (optional but strongly encouraged): ")
    while (all(!grepl("^([0-9]{4}-){3}[0-9]{3}[0-9X]$", orcid),
               nchar(orcid) > 1)) {
      log_it("error", 'Valid ORCIDs must be of the pattern "0000-0000-0000-000X" where "0" is a number and "X" may be a number or the character "X" (upper case only). Leave blank to skip.', "db")
      orcid <- readline('ORCID (0000-0000-0000-000X) (optional but strongly encouraged): ')
    }
  } else {
    unfilled <- c(
      username    = username,
      contact     = contact,
      first_name  = first_name,
      last_name   = last_name,
      affiliation = affiliation
    )
    if (any(unfilled == "")) {
      msg <- sprintf('Values must be provided for all required fields (%s were not provided).',
                     format_list_of_names(names(unfilled)[unfilled == ""])
      )
      if (exists("log_it")) {
        log_it("error", msg, "db")
      } else {
        cat("ERROR", msg, "\n")
      }
      return(NULL)
    }
  }
  if (
    any(
      is.null(orcid),
      is.na(orcid),
      orcid %in% c("", "null", "NULL", "NA")
    )
  ) {orcid <- "null"}
  build_db_action(
    action     = "INSERT",
    table_name = "contributors",
    db_conn       = db_conn,
    values     = c(
      username    = username,
      contact     = contact,
      first_name  = first_name,
      last_name   = last_name,
      affiliation = affiliation,
      orcid       = orcid
    )
  )
  user_id <- build_db_action(
    action         = "GET_ID",
    table_name     = "contributors",
    db_conn           = db_conn,
    match_criteria = list(username = username)
  )
  if (exists("log_it")) log_fn("end")
  return(user_id)
}

#' Verify a sample class exists, and add it if needed
#'
#' This utility function leverages [add_normalization_value] for the
#' "norm_sample_classes" table to verify and add, if necessary, a sample class
#' name or ID. If an integer is provided, it is verified against existing IDs.
#' If a character scalar is provided it is checked against the sample class
#' names and the associated ID number is returned.
#'
#' If used in an interactive mode, the user has the option of selecting an
#' existing sample class, aborting, or creating a new entry with the value of
#' parameter `sample_class`.
#'
#' If used non-interactively, new classes can be added automatically by setting
#' `auto_add` to TRUE and using the associated sample class ID downstream. Note
#' this may cause disconnections if typos are introduced (e.g. "plasma" vs
#' "plamsa").
#'
#' @param sample_class CHR or NUM scalar of the sample class name or ID
#' @param db_conn connection object (default "con")
#' @param auto_add LGL scalar of whether to automatically add an entry for
#'   `sample_class` as provided if it does not exist (use with caution: default
#'   FALSE)
#'
#' @return NULL if no action is taken, or the new sample class ID
#' @export
#'
#' @examples
verify_sample_class <- function(sample_class, db_conn = con, auto_add = FALSE) {
  logger <- exists("log_it")
  if (logger) log_it("debug", "Run verify_sample_class().", "db")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sample_class = list(c("length", 1)),
        db_conn      = list(c("length", 1)),
        auto_add     = list(c("mode", "logical"), c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Check connection
  stopifnot(active_connection(db_conn))
  sample_classes <- check_for_value(sample_class, "norm_sample_classes", "name", case_sensitive = FALSE)
  if (sample_classes$exists) {
    sample_class <- sample_classes$values$id
    sample_classes <- sample_classes$values
  } else {
    sample_classes <- tbl(db_conn, "norm_sample_classes") %>% collect()
  }
  if (is.numeric(sample_class)) {
    if (sample_class %in% sample_classes$id) {
      sample_class_id <- sample_class
      msg <- sprintf('Sample class "%s" identified from provided integer "%s".',
                     sample_classes$name[sample_classes$id == sample_class],
                     sample_class)
      if (logger) {
        log_it("info", msg, "db")
      } else {
        cat("INFO", msg, "\n")
      }
    } else {
      sample_class_id <- NULL
      msg <- sprintf('No sample class with ID = "%s" currently exists.',
                     sample_class)
      if (logger) {
        log_it("warn", msg, "db")
      } else {
        cat("WARN", msg, "\n")
      }
    }
  } else if (is.character(sample_class)) {
    if (sample_class %in% sample_classes$name) {
      sample_class_id <- sample_classes$id[sample_classes$name == sample_class]
      msg <- sprintf('Sample class id "%s" identified from direct name match to "%s".',
                              sample_class_id,
                              sample_class)
      if (logger) {
        log_it("info", msg, "db")
      } else {
        cat("INFO", msg, "\n")
      }
    } else {
      sample_class_id <- NULL
      msg <- sprintf('No sample class with name "%s" currently exists.',
                             sample_class)
      if (logger) {
        log_it("info", msg, "db")
      } else {
        cat("INFO", msg, "\n")
      }
      if (interactive()) {
        new_flag <- "\\(New\\) "
        msg <- 'Cannot automatically identify a sample class. Selecting a sample class interactively.'
        if (logger) {
          log_it("info", msg, "db")
        } else {
          cat("INFO", msg, "\n")
        }
        create_it  <- resolve_multiple_values(sample_classes$name, sample_class)
        if (str_detect(create_it, new_flag)) {
          create_it <- str_remove(create_it, new_flag)
          if (create_it != tolower(create_it)) {
            enter_as_lower <- select.list(title = "Standardize spelling to lower case?", choices = c("Yes", "No"))
            if (enter_as_lower == "Yes") {
              create_it <- str_to_lower(create_it)
            }
          }
          sample_class_id <- try(
            add_normalization_value("norm_sample_classes", name = create_it)
          )
          if (class(sample_class_id) == "try-error") {
            sample_class_id <- NULL
            msg <- sprintf('Error adding normalization value "%s" to table "norm_sample_classes".',
                           create_it)
            if (logger) {
              log_it("error", msg, "db")
            } else {
              cat("ERROR", msg, "\n")
            }
          } else {
            msg <- sprintf('Added "%s" as a normalization option to table "norm_sample_classes"',
                           create_it)
            if (logger) {
              log_it("success", msg, "db")
            } else {
              cat("SUCCESS", msg, "\n")
            }
          }
        } else if (create_it == "(Abort)") {
          sample_class_id <- NULL
          msg <- 'Sample class selection aborted.'
          if (logger) {
            log_it("info", msg, "db")
          } else {
            cat("INFO", msg, "\n")
          }
        } else {
          sample_class_id <- sample_classes$id[sample_classes$name == sample_class]
        }
      } else {
        if (auto_add) {
          sample_class_id <- try(add_normalization_value("norm_sample_classes", sample_class))
          if (class(sample_class_id) == "try-error") {
            sample_class_id <- NULL
            msg <- sprintf('Error adding normalization value "%s" to table "norm_sample_classes".',
                           sample_class)
            if (logger) {
              log_it("error", msg, "db")
            } else {
              cat("ERROR", msg, "\n")
            }
          } else {
            msg <- sprintf('Added "%s" as a normalization option to table "norm_sample_classes"',
                           sample_class)
            if (logger) {
              log_it("success", msg, "db")
            } else {
              cat("SUCCESS", msg, "\n")
            }
          }
        } else {
          sample_class_id <- NULL
          msg <- 'No new sample class added because "auto_add" is FALSE.'
          if (logger) {
            log_it("info", msg, "db")
          } else {
            cat("INFO", msg, "\n")
          }
        }
      }
    }
  } else {
    msg <- sprintf('Only character or numeric types are accepted for parameter "sample_class".')
    if (logger) {
      log_it("error", msg, "db")
    } else {
      cat("ERROR", msg, "\n")
    }
    sample_class_id <- NULL
  }
  if (logger) log_fn("end")
  return(sample_class_id)
}

#' Title
#'
#' @param contributor_text 
#' @param db_conn 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
verify_contributor <- function(contributor_text, db_conn = con, ...) {
  logger <- exists("log_it")
  if (logger) log_fn("start")
  # Check connection
  stopifnot(active_connection(db_conn))
  # Check contributor - do not check for internal id number
  # Prefer username
  db_contributors  <- tbl(db_conn, "contributors")
  contributor_properties <- c(list(...), user_value = contributor_text)
  possible_matches <- db_contributors %>%
    filter(username %in% contributor_text |
             contact %in% contributor_text |
             first_name %in% contributor_text |
             last_name %in% contributor_text
    ) %>%
    collect()
  contributor_exists <- nrow(possible_matches) > 0
  if (!contributor_exists) {
    msg <- sprintf('No contributor matching "%s" was located.', contributor_text)
    if (logger) {
      log_it("warn", msg, "db")
    } else {
      cat("WARN", msg, "\n")
    }
    if (interactive()) {
      if (exists("log_it")) log_it("trace", "Getting user information from an interactive session.")
      if (db_contributors %>% collect() %>% nrow() > 1) {
        associate_with <- menu(choices = c("Yes", "No"), title = "Associate with a current user?") == 1
      } else {
        associate_with <- FALSE
      }
      new_user <- NULL
      if (associate_with) {
        cat("\nCurrent contributors:\n")
        print(db_contributors)
        cat("\n")
        new_contrib_prompt <- "Make new contributor"
        new_user <- select.list(
          title = sprintf('Select a current user from the list above or select "%s" to continue.', new_contrib_prompt),
          choices = c("Abort", new_contrib_prompt)
        )
        if (new_user == "Abort") stop("Operation aborted.")
        if (new_user == new_contrib_prompt) new_user <- NULL
      }
      if (is.null(new_user)) {
        new_user <- do.call(add_contributor, contributor_properties)
      }
      if (is.null(new_user)) stop("Could not verify this user.")
      return(new_user)
    } else {
      needed <- dbListFields(db_conn, "contributors")
      needed <- needed[needed != "id"]
      properties_present <- needed %in% names(contributor_properties)
      if (all(properties_present)) {
        do.call(add_contributor, contributor_properties)
      } else {
        stop(sprintf("Unable to automatically add contributor. Please provide contributor properties for %s as arguments to this function.",
                     format_list_of_names(needed[!properties_present])))
      }
    }
  } else {
    if (nrow(possible_matches) == 1) {
      return(possible_matches$id[1])
    } else {
      if (interactive()) {
        possibles <- possible_matches %>%
          left_join(tbl(db_conn, "affiliations") %>% collect(),
                    by = c("affiliation" = "id")) %>%
          select(-affiliation) %>%
          rename("affiliation" = "name") %>%
          mutate(Contributor = sprintf("%s %s at %s from %s",
                                       first_name,
                                       last_name,
                                       contact,
                                       affiliation))
        if (logger) {
          log_it("info", "Multiple contributors found.")
        } else {
          cat("INFO", "Multiple contributors found.\n")
        }
        verified_match <- resolve_multiple_values(possibles$Contributor, contributor_text)
        if (str_detect(verified_match, "\\(New\\) ")) {
          make_new <- select.list(choices = c("Yes", "No"), title = "Create a new user now?")
          if (make_new == "Yes") {
            id <- add_contributor(db_contributors, user_value = contributor_text)
            return(id)
          } else {
            return("Create a contributor manually to proceed.")
          }
        } else {
          return(possible_matches$id[which(possibles$Contributor == verified_match)])
        }
      } else {
        msg <- "Non interactive session and multiple matches identified."
        if (logger) {
          log_it("trace", msg, "db")
        } else {
          cat("TRACE", msg, "\n")
        }
        stop("Unable to discretely identify a contributor. Please try again.")
      }
    }
  }
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
