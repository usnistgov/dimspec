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
#' @param conn connection object (default: con)
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
pragma_table_def <- function(db_table, conn = con, get_sql = FALSE, pretty = TRUE) {
  require(dplyr)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table = list(c("mode", "character"), c("n>=", 1)),
        conn     = list(c("length", 1)),
        get_sql  = list(c("mode", "logical"), c("length", 1)),
        pretty   = list(c("mode", "logical", c("length", 1)))
      ),
      from_fn    = "pragma_table_def"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Define function scope
  func <- ifelse(get_sql,
                 "select name, type, sql from sqlite_master where name = '%s'",
                 "PRAGMA table_xinfo('%s')"
  )
  # Basic return
  out <- lapply(db_table,
                function(x) {
                  tmp <- dbGetQuery(conn, sprintf(func, x))
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
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Ensure table exists
  tables_exist <- db_table %in% dbListTables(db_conn)
  if (!all(tables_exist)) {
    warning(sprintf('No table named "%s" was found in this schema.', table[!tables_exist]))
    db_table <- db_table[tables_exist]
  }
  # Get table properties
  out <- pragma_table_def(db_table = db_table, con = db_conn, get_sql = FALSE)
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
    tmp <- pragma_table_def(db_table, db_conn, get_sql = TRUE) %>%
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
  if (names_only) {
    out <- out$name
  }
  # Return data frame object representing PRAGMA table_info columns matching the
  # requested properties
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
  # Argument validation
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
  if (!arg_check$valid) {
    stop(cat(paste0(arg_check$messages, collapse = "\n")))
  }
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
    log_trace('SQLite CLI under alias "{sqlite_cli}" is available. Building directly...')
    require(glue)
    sqlite_call   <- glue('{sqlite_cli} {db}')
    if (!file.exists(build_file)) {
      stop(glue('Cannot locate file "{build_file}" in this directory.'))
    }
    build         <- glue('-cmd ".read {build_file}"')
    if (populate) {
      populate_file <- list.files(pattern = populate_with, full.names = TRUE, recursive = TRUE)
      if (!file.exists(populate_file)) {
        log_warn('Cannot locate file "{populate_file}" in this directory; "{db}" will be created but not populated.')
        populate_cmd <- ""
      } else {
        populate_cmd <- glue(' -cmd ".read {populate_file}"')
      }
    } else {
      populate_cmd <- ""
    }
    full_call     <- glue('{build}{populate_cmd}') %>%
      str_remove_all("\\./")
    full_call     <- glue('{sqlite_call} {full_call} -cmd ".exit"')
    log_trace("Issuing shell command {full_call}")
    if (file.exists(db)) remove_db(db = db, archive = archive)
    shell(full_call)
    log_success('Finished attempted build of "{db}" as specified. Check console for any failure details.')
  } else {
    # Do it the long way
    log_trace('SQLite CLI under alias "{sqlite_cli}" is not available. Building through R...')
    # -- Ensure packages are available
    reqs <- c("stringr", "magrittr", "readr", "DBI")
    packs_available <- reqs %in% installed.packages()
    if (!all(packs_available)) {
      stop("Some packages were not available. Please install packages:", paste0(reqs[!packs_available]))
    }
    invisible(lapply(reqs, require, character.only = TRUE))
    # -- Remove the existing database
    if (file.exists(db)) remove_db(db = db, archive = archive)
    # -- Create the build commands in R to pass through RSQLite::SQLite()
    build_path <- list.files(pattern = build_file,
                             full.names = TRUE,
                             recursive = TRUE)
    build_statement <- create_fallback_build(build_path)
    build_statement <- build_statement[nchar(build_statement) > 1]
    # -- Create the database and read in the build statements
    con <- dbConnect(RSQLite::SQLite(), db)
    invisible(
      lapply(build_statement,
             function(x) dbSendStatement(con, str_trim(x, "both")))
    )
    # Clean up
    dbDisconnect(con)
  }
  if (connect) {
    manage_connection(db = db, reconnect = TRUE)
  }
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
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db      = list(c("mode", "character"), c("length", 1)),
        archive = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "remove_db"
    )
    logger <- "logger" %in% (.packages())
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Resolve database file location
  if (logger) log_trace('Finding database file "{db}"')
  db_path  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
  db_path  <- db_path[basename(db_path) == db]
  if (length(db_path) > 0) {
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
      if (logger) log_success('Archive created as "{new_fname}"')
    }
    result <- try(file.remove(db_path))
    if (class(result) == "try-error") {
      if (logger) log_error('Database "{db}" could not be removed; another connection is likely open elsedb_path.')
    } else {
      if (logger) log_success('Database "{db}" removed.')
    }
  } else {
    if (logger) log_warn('Database "{db}" does not exist in this directory tree.')
  }
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
                              ...) {
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
    logger <- "logger" %in% (.packages())
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  require(drv_pack, character.only = TRUE)
  logger           <- "logger" %in% (.packages())
  global_env       <- as.list(.GlobalEnv)
  global_env_names <- names(global_env)
  connection_object_classes <- sprintf("%sConnection", conn_class)
  for (env in global_env_names) {
    connected      <- FALSE
    this_obj       <- global_env[[env]]
    this_obj_class <- class(this_obj)
    if (any(grepl(conn_class, this_obj_class))) {
      if (any(grepl(connection_object_classes, this_obj_class))) {
        connection <- try(this_obj@dbname)
      } else if (any(grepl(sprintf("^tbl_%s$", conn_class), this_obj_class))) {
        connection <- try(this_obj$src$con@dbname)
      } else if (any(grepl(sprintf("^%sResult$", drv), this_obj_class))) {
        connection <- try(this_obj@conn@dbname)
        dbClearResult(this_obj)
      } else if (class(this_obj) == conn_class) {
        connection <- "other"
      } else {
        connection <- ""
      }
      if (class(connection) == "try-error") {
        if (logger) log_warn('Could not automatically identify the connection properties for object "{env}". To avoid hanging connections, it should be removed explicitly.')
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
        if (logger) log_trace('Database "{db}" is currently open.')
        if (all(connected, disconnect)) {
          check <- try(invisible(dbDisconnect(this_obj)))
          status <- ifelse(class(check)[1] == "try-error",
                           "unable to be disconnected",
                           "disconnected")
          if (logger) log_trace('Database "{db}" {status}.')
          connected <- dbIsValid(this_obj)
        }
      }
      if (logger) log_trace('Closing and removing "{env}"...')
      if (all(rm_objects, disconnect, !connection == "")) {
        rm(list = env, pos = ".GlobalEnv")
      }
    }
  }
  if (reconnect) {
    if (is_local) {
      # Resolve database file location
      if (logger) log_trace('Finding local database file "{db}"')
      db_where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
      if (length(db_where) == 0) {
        stop(sprintf('Unable to locate "%s".', db))
      }
    }
    if (conn_name %in% global_env_names) {
      if (connected) dbDisconnect(sym(conn_name)) else rm(list = eval(sym(conn_name)), pos = ".GlobalEnv")
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
      if (all(logger, dbIsValid(eval(sym(conn_name))))) log_trace('"{db}" connected as "{conn_name}".')
    }
  }
}

# TODO early sketch for construction of automatic logging triggers
build_db_logging_triggers <- function(db = DB_NAME, connection = "con", log_table_name = "log") {
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
  comments <- obj$sql %>%
    str_remove_all("/\\* (Check constraints|Foreign key relationships) \\*/") %>%
    str_extract_all("/\\* [[:alnum:][:punct:]\\[\\]\\+ ]+ \\*/") %>%
    lapply(str_remove_all, "/\\* | \\*/")
  table_comments <- lapply(comments, function(x) x[1])
  field_comments <- lapply(comments, function(x) x[-1])
  out <- vector('list', nrow(obj))
  for (i in 1:length(out)) {
    out[[i]]$table_comment  <- table_comments[[i]]
    out[[i]]$field_comments <- field_comments[[i]]
  }
  names(out) <- obj$db_table
  return(out)
}

#' Create a data dictionary
#'
#' Get a list of tables and their defined columns with properties, including
#' comments, suitable as a data dictionary from a connection object amenable to
#' [odbc::dbListTables]. This function relies on [pragma_table_info].
#'
#' @param conn_obj connection object (default:con)
#'
#' @return LIST of length equal to the number of tables in `con` with attributes
#'   identifying which tables, if any, failed to render into the dictionary.
#' @export
#'
#' @examples
data_dictionary <- function(conn_obj = con) {
  logger <- "logger" %in% (.packages())
  tabls <- dbListTables(con)
  tabls <- tabls[-grep("sqlite_", tabls)]
  out <- vector('list', length(tabls))
  names(out) <- tabls
  failures <- character(0)
  for (tabl in tabls) {
    tmp <- try(pragma_table_info(db_table = tabl,
                                 db_conn = con,
                                 include_comments = TRUE))
    if (class(tmp) == "try-error") {
      if (logger) {
        log_warn('Dictionary failure on table "{tabl}"')
      } else {
        cat(sprintf('Dictionary failure on table "%s"\n', tabl))
      }
      failures <- c(failures, tabl)
    } else {
      if (logger) {
        log_success('{tabl} added to dictionary')
      } else {
        cat(sprintf('"%s" added to dictionary\n', tabl))
      }
    }
    out[[tabl]] <- tmp
  }
  if (length(failures) > 0) {
    has_failures <- TRUE
    cat(sprintf("\nDictionary was not available for %s: %s\n",
                ifelse(length(failures) > 1, "tables", "table"),
                format_list_of_names(failures)),
        "\n")
  } else {
    has_failures <- FALSE
    failures <- "Dictionary available for all tables."
  }
  attr(out, "has_failures") <- has_failures
  attr(out, "failures")     <- failures
  return(out)
}

#' Save the current data dictionary to disk
#'
#' Use [data_dictionary] and save the output to a local file. If `output_format`
#' is one of "data.frame" or "list", the resulting file will be saved as an RDS.
#' Parameter `output_file` will be used during the save process; relative paths
#' are fine and will be identified by the current working directory.
#'
#' @param conn_obj connection object (default: con)
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
save_data_dictionary <- function(conn_obj           = con,
                                 output_format      = "json",
                                 output_file        = NULL,
                                 overwrite_existing = TRUE) {
  # Argument validation
  arg_check <- verify_args(
    args       = list(output_format, overwrite_existing),
    conditions = list(
      output_format      = list(c("mode", "character"), c("length", 1)),
      overwrite_existing = list(c("mode", "logical"), c("length", 1))
    ),
    from_fn    = "build_db"
  )
  if (!arg_check$valid) {
    stop(cat(paste0(arg_check$messages, collapse = "\n")))
  }
  logger <- "logger" %in% (.packages())
  output_format <- tolower(output_format)
  output_format <- match.arg(output_format,
                             c("json", "csv", "data.frame", "list"))
  if (!output_format %in% c("json", "csv")) {
    f_ext <- "RDS"
  } else {
    f_ext <- output_format
  }
  if (is.null(output_file)) {
    f_name <- sprintf("%s/%s_%s_data_dictionary.%s",
                      getwd(),
                      DB_TITLE,
                      DB_VERSION,
                      f_ext)
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
                          sprintf("_data_dictionary%s.%s", 
                                  ifelse(i == 1, "",
                                         sprintf(" (%s)", i - 1)),
                                  f_ext),
                          sprintf("_data_dictionary (%s).%s",
                                  i,
                                  f_ext)
    )
  }
  out <- data_dictionary(conn_obj)
  if (is.null(output_file)) {
    if (logger) {
      log_warn('No file name provided to "output_file", saving as "{f_name}"')
    } else {
      cat(sprintf('No file name provided to "output_file", saving as "%s"\n', f_name))
    }
  }
  if (attr(out, "has_failures")) {
    if (logger) {
      log_error('This dictionary failed on tables {format_list_of_names(out$failures)}')
    } else {
      stop(sprintf('This dictionary failed on tables %s',
                   format_list_of_names(out$failures)))
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
#' @param conn connection object, specifically of class "SQLiteConnection" but
#'   not strictly enforced
#'
#' @return nested LIST object
#' @export
#'
#' @examples
er_map <- function(conn = con) {
  build_statements <- pragma_table_def(conn, dbListTables(conn), get_sql = TRUE)
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
  refs     <- str_extract_all(build_statements$sql, "REFERENCES [:word:]+") %>%
    lapply(str_remove_all, "REFERENCES ")
  used_in  <- str_extract_all(build_statements$sql, "(JOIN|FROM) [:word:]+") %>%
    lapply(str_remove_all, "(JOIN|FROM) ")
  for (i in 1:n_tables) {
    z <- which(t_names %in% refs[[i]])
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
      object_type  = er_type[[i]],
      references   = if (er_type[[i]] == "TABLE") {
        t_names[as.logical(er_mask[i, ])]
      } else {
        t_names[as.logical(view_mask[, i])]
      },
      normalizes   = t_names[as.logical(er_mask[, i])],
      used_in_view = t_names[as.logical(view_mask[i, ])]
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
  return(er_map)
}

# TODO - abstract the appropriate ms_n - 1 scan to associate with ms_data
associated_scan <- function(df, scan_time) {
  scan_msn  <- df$msn[which(df$scantime == scan_time)] - 1
  df$scantime[
    which(all(df$scantime < scan_time,
              df$msn == scan_msn))
  ]
}

verify_contributor <- function(contributor_text, conn = con, ...) {
  # Check contributor - do not check for internal id number
  # Prefer username
  db_contributors  <- tbl(conn, "contributors")
  contributor_properties <- list(...)
  possible_matches <- db_contributors %>%
    filter(username %in% contributor_text |
             contact %in% contributor_text |
             first_name %in% contributor_text |
             last_name %in% contributor_text
    ) %>%
    collect()
  contributor_exists <- nrow(possible_matches) > 0
  if (!contributor_exists) {
    if (LOGGING_ON) {
      log_warn("No contributor matching '{contributor_text}' was located.")
    } else {
      cat(sprintf("No contributor matching '%s' was located.", contributor_text))
    }
    if (interactive()) {
      do.call(add_contributor, contributor_properties)
    } else {
      needed <- dbListFields(conn, "contributors")
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
      cat("Multiple contributors found. Please select one to continue.")
      possibles <- possible_matches %>%
        left_join(tbl(conn, "affiliations") %>% collect(),
                  by = c("affiliation" = "id")) %>%
        select(-affiliation) %>%
        rename("affiliation" = "name") %>%
        mutate(Contributor = sprintf("%s - %s %s (%s)",
                                     affiliation,
                                     first_name,
                                     last_name,
                                     contact))
      verified_match <- select.list(choices = c("(New)", possibles$Contributor))
      if (verified_match == "(New)") {
        # cat("New contributors must be added manually at this time.")
        make_new <- select.list(choices = c("Yes", "No"), title = "Create a new user now?")
        if (make_new == "Yes") {
          id <- add_contributor(db_contributors)
          return(id)
        } else {
          return("Create a contributor manually to proceed.")
        }
      } else {
        return(possible_matches$id[which(possibles$Contributor == verified_match)])
      }
    }
  }
}

#' Add a contributor programmatically or interactively
#'
#' @param conn connection object (default: con)
#' @param username CHR scalar of the desired username (default "")
#' @param contact CHR scalar of the contributor's contact point (default "")
#' @param first_name CHR scalar of the contributor's first name (default "")
#' @param last_name CHR scalar of the contributor's last name (default "")
#' @param affiliation CHR scalar of the contributor's affiliation (default "")
#' @param orcid CHR scalar of the contributor's ORCID (default ""), which must
#'   match the valid ORCID pattern of four sets of four alphanumeric characters
#'   separated by dashes (e.g. "1111-2222-3333-4444")
#'
#' @return
#' @export
#'
#' @examples
add_contributor <- function(conn = con,
                            username = "",
                            contact = "",
                            first_name = "",
                            last_name = "",
                            affiliation = "",
                            orcid = "") {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        conn        = list(c("length", 1)),
        username    = list(c("mode", "character"), c("length", 1)),
        contact     = list(c("mode", "character"), c("length", 1)),
        first_name  = list(c("mode", "character"), c("length", 1)),
        last_name   = list(c("mode", "character"), c("length", 1)),
        affiliation = list(c("mode", "character"), c("length", 1)),
        orcid       = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "add_contributor"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  if (interactive()) {
    while (username == "")    username    <- readline("Username, required: ")
    while (username %in% build_db_action(conn,
                                         action       = "SELECT",
                                         column_names = "username",
                                         table_name   = "contributors")) {
      cat("That username is already taken. Please select another.")
      username <- readline("Username: ")
    }
    while (contact == "")     contact     <- readline("Contact (email preferred, required): ")
    while (first_name == "")  first_name  <- readline("First Name (required): ")
    while (last_name == "")   last_name   <- readline("Last Name (required): ")
    while (affiliation == "") affiliation <- readline("Affiliation (required): ")
    existing_affiliations <- build_db_action(conn,
                                             action       = "SELECT",
                                             column_names = c("id", "name"),
                                             table_name   = "affiliations")
    existing_affiliations <- bind_rows(existing_affiliations,
                                       mutate(existing_affiliations, name = toupper(name)))
    if (toupper(affiliation) %in% existing_affliations$name) {
      affiliation <- existing_affiliations$id[which(existing_affiliations$name == toupper(affiliation))]
    }
    if (orcid == "")       orcid       <- readline("ORCID (XXXX-XXXX-XXXX-XXXX) (optional but strongly encouraged): ")
    # This regex is not right
    while (all(!grepl("[[:alnum:]{4}-]{3}[[:alnum:]]{4}", orcid),
               nchar(orcid) > 1)) {
      cat(orcid)
      orcid <- readline("Valid ORCIDs must be of the pattern XXXX-XXXX-XXXX-XXXX. Leave blank to skip.\nORCID (optional but strongly encouraged): ")
    }
  } else {
    if (any(username == "", contact == "", first_name == "", last_name == "", affiliation == "")) {
      
    }
  }
  if (orcid == "") orcid <- "null"
  build_db_action(conn, "INSERT", "contributors",
                  values = c(username    = username,
                             contact     = contact,
                             first_name  = first_name,
                             last_name   = last_name,
                             affiliation = affiliation,
                             orcid       = orcid))
  return(build_db_action(conn, "GET_ID", "contributors", match_criteria = list(username = username)))
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
#' @param conn connection object (default: con)
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
check_for_value <- function(values, db_table, db_column, conn = con) {
  existing_values <- build_db_action(con            = conn,
                                     action         = "SELECT",
                                     table_name     = db_table,
                                     match_criteria = setNames(list(values), db_column),
                                     distinct       = TRUE)
  found <- nrow(existing_values) > 0
  out   <- list(exists = found,
                values = if (found) existing_values else NULL)
  return(out)
}

sqlite_parse_build <- function(sql_statements) {
  logger_active <- all(exists("LOGGING_ON"), LOGGING_ON)
  if (logger_active) log_trace('Parsing "{sql_statements}" for build commands.')
  magicsplit <- "/\\*magicsplit\\*/"
  header     <- "/\\*\\=+\\r*\\n[[:print:][:cntrl:]]+\\r*\\n\\=+\\*/"
  section    <- "/\\* -* [[:print:][:cntrl:]]+ -* \\*/"
  comments   <- "^/\\* [[:alnum:]+ \\.\\',/_]+ \\*/$"
  to_remove <- paste0(c(header, "\\t", "\\r"), collapse = "|")
  out        <- sql_statements %>%
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
  return(out)
}

sqlite_parse_import <- function(build_statements) {
  regex_import  <- ".import (--[[:alnum:]]+)? "
  regex_skip    <- "(--skip [[:number:]]+) ?"
  regex_prefix  <- paste0(c(regex_import, regex_skip), collapse = "?")
  logger_active <- all(exists("LOGGING_ON"), LOGGING_ON)
  if (logger_active) log_trace('Parsing "{build_statements}" for import modification.')
  out           <- lapply(build_statements,
                          function(x) {
                            if (grepl("\\.import", x)) {
                              data_type <- str_extract(x, regex_import) %>%
                                str_remove(".import( --)?") %>%
                                str_squish()
                              if (data_type == "") {
                                if (logger_active) {
                                  log_warn('No data type identified. Assuming a ".csv" extension.')
                                } else {
                                  warning('No data type identified. Assuming a ".csv" extension.')
                                }
                                data_type <- "csv"
                              }
                              skip_rows <- str_extract(x, regex_skip) %>%
                                str_remove("--skip ") %>%
                                str_squish()
                              skip_rows <- try(as.numeric(skip_rows))
                              if (class(skip_rows) == 'try-error') {
                                if (logger_active) {
                                  log_warn('Could not convert "{skip_rows}" to numeric in "{x}".')
                                } else {
                                  warning(sprintf('Could not convert "%s" to numeric in "%s".',
                                                  skip_rows, x))
                                }
                                return(x)
                              }
                              tmp <- str_remove(x, regex_prefix) %>%
                                str_split(" ") %>%
                                .[[1]]
                              if (length(tmp) != 2) {
                                if (logger_active) {
                                  log_warn('Could not parse "{x}" to identify both a target file and database target table.')
                                } else {
                                  warning(sprintf('Could not parse "%s" to identify both a target file and database target table.',
                                                  x))
                                }
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
                                  read_data <- invisible(
                                    do.call(read_func,
                                            list(data_file))
                                  )
                                } else {
                                  if (logger_active) {
                                    log_warn('Cannot read file "{data_file}". Function "{read_func}" is not available.')
                                  } else {
                                    warning(sprintf('Cannot read file "%s". Function "%s" is not available.',
                                                    data_file,
                                                    read_func))
                                  }
                                  return(x)
                                }
                              } else {
                                if (logger_active) {
                                  log_warn('Could not find file "{data_file}".')
                                } else {
                                  warning(sprintf('Could not find file %s.',
                                                  x))
                                }
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
  return(out)
}

create_fallback_build <- function(build_file    = NULL,
                                  populate_with = NULL,
                                  populate      = TRUE,
                                  driver        = "SQLite") {
  if (all(is.null(build_file), exists("DB_BUILD_FILE"))) build_file <- DB_BUILD_FILE
  if (all(is.null(populate_with), exists("DB_DATA"))) populate_with <- DB_DATA
  comments   <- "^/\\* [[:alnum:]+ \\.\\',/_]+ \\*/$"
  has_import <- FALSE
  logger_active <- all(exists("LOGGING_ON"), LOGGING_ON)
  build_file <- list.files(pattern = build_file,
                           full.names = TRUE,
                           recursive = TRUE)
  outfile <- gsub(".sql", "_full.sql", build_file)
  build   <- read_file(build_file) %>%
    sqlite_parse_build()
  
  if (populate) {
    populate_file <- list.files(pattern = populate_with,
                                full.names = TRUE,
                                recursive = TRUE)
    populate <- read_file(populate_file) %>%
      sqlite_parse_build()
    build <- c(build, populate)
  }
  # Make SQL build statements
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
  write_file(build, outfile)
  if (logger_active) {
    log_info('Fallback build file created as "{outfile}".')
  } else {
    cat(sprintf('Fallback build file created as "%s".',
                outfile))
  }
}