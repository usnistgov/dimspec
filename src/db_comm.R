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
#'   (default FALSE), passed to `remove_db()`
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
    con <<- dbConnect(RSQLite::SQLite(), db)
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
  arg_check <- verify_args(
    args       = as.list(environment()),
    conditions = list(
      db      = list(c("mode", "character"), c("length", 1)),
      archive = list(c("mode", "logical"), c("length", 1))
    ),
    from_fn    = "remove_db"
  )
  logger <- "logger" %in% (.packages())
  require(tools)
  if (!arg_check$valid) {
    stop(cat(paste0(arg_check$messages, collapse = "\n")))
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
#'   supplied in config/env.R
#' @param drv CHR vector of connection object classes to check against. Note
#'   this may depend heavily on connection packages and must be present in the
#'   class names of the driver used. (default "SQLiteConnection")
#' @param reconnect LGL scalar of whether or not to connect if a connection does
#'   not exist (default TRUE); if both this and `disconnect` are true, it will
#'   first be disconnected
#' @param disconnect LGL scalar of whether or not to terminate and remove the
#'   connection from the current global environment (default TRUE)
#'
#' @return None
#' @export
#'
#' @examples
manage_connection <- function(db = DB_NAME,
                              drv_pack = "RSQLite",
                              drv = "SQLite",
                              check_class = "SQLiteConnection",
                              reconnect = TRUE,
                              disconnect = TRUE) {
  logger <- "logger" %in% (.packages())
  current_env <- as.list(.GlobalEnv)
  current_env_names <- names(current_env)
  for (env in current_env_names) {
    this_obj <- current_env[[env]]
    if (any(check_class %in% class(this_obj))) {
      if (basename(this_obj@dbname) == db) {
        if (logger) log_trace('Database "{db}" is currently open.')
        if (disconnect) {
          if (logger) log_trace('Closing and removing from environment...')
          invisible(dbDisconnect(this_obj))
          rm(list = env, pos = ".GlobalEnv")
        }
      }
    }
  }
  if (reconnect) {
    # Resolve database file location
    if (logger) log_trace('Finding database file "{db}"')
    db_where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
    if (length(db_where) == 0) {
      stop(sprintf("Unable to locate '%s'.", db))
    }
    require(drv_pack, character.only = TRUE)
    con <<- try(dbConnect(do.call(drv, list()), db))
    if (class(con) == "try-error") {
      stop(sprintf("Unable to connect to '%s'.", db_where))
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