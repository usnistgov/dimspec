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
#' rather than e.g. [DBI::dbListFields()]. Set `get_sql` to `TRUE` to include
#' the direct schema using sqlite_master; depending on formatting this may or
#' may not be directly usable though some effort has been made to remove
#' formatting characters (e.g. line feeds, tabs, etc) if stringr is available.
#'
#' Note that the package `stringr` is required for formatting returns that
#' include either `get_sql` or `pretty` as TRUE.
#'
#' @param con connection object
#' @param db_table CHR vector name of the table(s) to inspect
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
pragma_table_def <- function(con, db_table, get_sql = FALSE, pretty = TRUE) {
  require(dplyr)
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        con      = list(c("length", 1)),
        db_table = list(c("mode", "character"), c("n>=", 1)),
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
                  tmp <- dbGetQuery(con, sprintf(func, x))
                  tmp$db_table <- x
                  return(tmp)
                }
  )
  # Shape up the return
  if (get_sql) {
    out <- dplyr::bind_rows(out) %>%
      select(db_table, type, sql)
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
      select(cid, db_table, name:pk)
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
#' @param include_comments LGL scalar of whether to include comments in the
#'   return data frame
#' @param names_only LGL scalar of whether to include names meeting defined
#'   criteria as a vector return value
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
                              include_comments = FALSE,
                              names_only  = FALSE) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_conn, table, names_only),
      conditions = list(
        db_conn     = list(c("length", 1)),
        table       = list(c("mode", "character"), c("n>=", 1)),
        names_only  = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn   = "pragma_table_info"
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Ensure table exists
  tables_exist <- table %in% dbListTables(db_conn)
  if (!all(tables_exist)) {
    warning(sprintf('No table named "%s" was found in this schema.', table[!tables_exist]))
    table <- table[tables_exist]
  }
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
  # Limit column outputs?
  limit <- lapply(c(condition, name_like, data_type),
                      function(x) !is.null(x)) %>%
    unlist() %>%
    any()
  # Get comments if any
  if (include_comments) {
    tmp <- pragma_table_def(db_conn, table, get_sql = TRUE) %>%
      tidy_comments() %>%
      bind_rows()
    out <- out %>%
      bind_cols(tmp) %>%
      relocate(table_comment, .after = db_table)
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
#' @param db CHR scalar of the database name
#' @param build_from CHR scalar of a SQL build script to use (default
#'   "build.sql")
#' @param populate_with CHR scalar for which data to include in the build; valid
#'   choices are "demo" and "pfas" (default "demo")
#' @param archive BOOL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default FALSE), passed to [`remove_db()`]
#' @param sqlite_cli CHR scalar to use to look for installed sqlite3 CLI tools
#'   in the current system environment (default "sqlite3")
#' @param connect BOOL scalar of whether or not to connect to the rebuilt
#'   database in the global environment as object `con`` (default FALSE)
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
  # Populate with all data sources
  # -- RSQLite does not allow for the CLI [.DOT] commands
  # -- If sqlite3 CLI is installed, use that by preference
  sqlite_available <- length(Sys.which(sqlite_cli) > 1)
  # Do it the short way
  manage_connection(db = db, reconnect = FALSE, disconnect = TRUE)
  if (sqlite_available) {
    log_trace('SQLite CLI under alias "{sqlite_cli}" is available. Building directly...')
    require(glue)
    sqlite_call   <- glue('{sqlite_cli} {db}')
    build_file    <- list.files(pattern = build_from, full.names = TRUE, recursive = TRUE)
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
    log_success('Finished building "{db}"" as specified. Check console for details.')
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
    build_path <- list.files(pattern = build_from,
                             full.names = TRUE,
                             recursive = TRUE)
    header     <- "/\\*\\=+\\n[[:print:][:cntrl:]]+\\n\\=+\\*/"
    section    <- "/\\* -+ [[:print:][:cntrl:]]+ \\*/"
    magicsplit <- "/\\*magicsplit\\*/"
    build_statement <- read_file(build_path) %>%
      str_split(magicsplit) %>%
      .[[1]] %>%
      str_remove_all(header) %>%
      str_remove_all(section) %>%
      str_remove_all("^\\n\\n") %>%
      str_replace_all("\\n\\t", " ") %>%
      str_squish()
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
#' @param db CHR scalar name of the database to build, defaults to the name
#'   supplied in config/env.R
#' @param archive BOOL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default FALSE)
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
#' @param db CHR scalar name of the database to check, defaults to the name
#'   supplied in config/env.R (default session variable DB_NAME)
#' @param drv_pack CHR scalar of the package used to connect to this database
#'   (default session variable DB_DRIVER)
#' @param conn_class CHR vector of connection object classes to check against.
#'   Note this may depend heavily on connection packages and must be present in
#'   the class names of the driver used. (default session variable DB_CLASS)
#' @param conn_name CHR scalar of the R environment object name to use for this
#'   connection (default "con")
#' @param is_local LGL scalar indicating whether or not the referenced database
#'   is a local file, if not it will be treated as though it is either a DSN or
#'   a database name on your host server, connecting as otherwise defined
#' @param rm_objects LGL scalar indicating whether or not to remove objects
#'   identifiably connected to the database from the current environment. This
#'   is particularly useful
#' @param reconnect LGL scalar indicating whether or not to connect if a
#'   connection does not exist (default TRUE); if both this and `disconnect` are
#'   true, it will first be disconnected
#' @param disconnect LGL scalar indicating whether or not to terminate and
#'   remove the connection from the current global environment (default TRUE)
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

db_bulk_load <- function(target, target_type) {
  
}

read_in_json <- function(package, is_file = TRUE) {
  tmp <- fromJSON(package)
}

add_sample <- function() {
  
}

add_peak <- function() {
  
}

add_compound <- function() {
  
}

add_contributor <- function(con) {
  con <- verify_connection()
}

add_method <- function() {
  
}

add_qc_method <- function() {
  
}

add_mobile_phase <- function() {
  # Modify table "mobile_phases" 
}

add_solvent <- function() {
  # Modify table "norm_solvents" if solvent does not have an id. Looks up
  # solvent by common aliases in "solvent_aliases" first to prevent duplication.
}

add_solvent_mix <- function() {
  # Modify table "solvent_mixes" if a mix id does not have an id. These are
  # combinations of solvents in particular ratios (e.g. 10% methanol in water).
  # These ids are used in column "mobile_phases.carrier"
}

add_ms_data <- function() {
  
}

get_id <- function(table, column, value) {
  
}


tidy_comments <- function(obj) {
  comments <- obj$sql %>%
    str_remove_all("/\\* (Check constraints|Foreign key relationships) \\*/") %>%
    str_extract_all("/\\* [[:alnum:][:punct:] ]+ \\*/") %>%
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

data_dictionary <- function(conn_obj, output_format = "list") {
  # logger <- "logger" %in% (.packages())
  logger <- FALSE
  tabls <- dbListTables(con)
  out <- vector('list', length(tabls))
  names(out) <- tabls
  failures <- character(0)
  for (tabl in tabls) {
    tmp <- try(pragma_table_info(con, tabl, include_comments = TRUE))
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
    cat(sprintf("\n\nDictionary was not available for %s: %s",
                ifelse(length(failures) > 1, "tables", "table"),
                format_list_of_names(failures)),
        "\n")
  } else {
    failures <- "Dictionary available for all tables."
  }
  out <- list(failures = failures, dictionary = out)
  return(out)
}

save_data_dictionary <- function(conn_obj, output_format = "json", output_file = NULL) {
  logger <- "logger" %in% (.packages())
  output_format <- match.arg(tolower(output_format),
                             c("json", "csv", "data.frame", "list"))
  if (!output_format %in% c("json", "csv")) {
    f_ext <- "RDS"
  } else {
    f_ext <- output_format
  }
  f_name <- sprintf("%s/%s_%s_data_dictionary.%s",
                    getwd(),
                    DB_TITLE,
                    DB_VERSION,
                    f_ext)
  i <- 0
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
  out <- data_dictionary(conn_obj, output_format)[[2]]
  if (logger) {
    log_warn('No file name provided to "output_file", saving as "{f_name}"')
  } else {
    cat(sprintf('No file name provided to "output_file", saving as "%s"\n', f_name))
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
