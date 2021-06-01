#' Returns R session information in case of support needs
#' 
#' Several of 
#' 
#' @return LIST of the following values: current package version
#' @export
#' 
#' @example 
#' support_info()
support_info <- function() {
  if (!exists("VERSION")) source(list.files(pattern = "env.R", recursive = TRUE))
  out <- list(
      pack_version    = VERSION,
      system          = sessionInfo(),
      project_dir     = getwd(),
      last_db_schema  = LAST_DB_SCHEMA,
      files_available = list.files(full.names = TRUE, recursive  = TRUE),
      packs_installed = installed.packages()[, 3],
      packs_active    = (.packages())
  )
  return(out)
}

#' WIP Rebuild the database from scratch
#'
#' @param db CHR scalar of the database name
#' @param build_from CHR scalar of a SQL build script to use (default "build.sql")
#' @param populate_with CHR scalar for which data to include in the build; valid
#'   choices are "demo" and "pfas" (default "demo")
#' @param archive BOOL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default FALSE), passed to `remove_db()`
#' @param sqlite_cli CHR scalar to use to look for installed sqlite3 CLI tools
#'   in the current system environment (default "sqlite3")
#'
#' @return None, check console for details
#' @export
#'
#' @examples
rebuild_db <- function(db = DB_NAME, build_from = "build.sql", populate_with = "demo_data.sql", archive = FALSE, sqlite_cli = "sqlite3") {
  # Argument validation
  if (length(db) > 1) stop('Only one database name should be supplied to argument "db".')
  if (!is.character(db)) stop('Argument "db" must be a character scalar.')
  if (length(build_from) > 1) stop('The base name of a single file should be supplied for argument "build_from".')
  if (!is.character(build_from)) stop('Argument "build_from" must be a character scalar.')
  if (length(populate_with) > 1) stop('The base name of a single file should be supplied for argument "populate_with".')
  if (!is.character(populate_with)) stop('Argument "populate_with" must be a character scalar.')
  if (length(archive) > 1) stop('Only one value is accepted for argument "archive".')
  if (!is.logical(archive)) stop('Argument "archive" must be a logical scalar.')
  if (length(sqlite_cli) > 1) stop('The environment alias for your sqlite3 command line interface (CLI) should be supplied for argument "sqlite_cli". Leave this as the default if you do not have sqlite3 CLI installed.')
  if (!is.character(sqlite_cli)) stop('Argument "sqlite_cli" must be a character scalar.')
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
    populate_file <- list.files(pattern = populate_with, full.names = TRUE, recursive = TRUE)
    if (!file.exists(populate_file)) {
      log_warn('Cannot locate file "{populate_file}" in this directory; "{db}" will be created but not populated.')
      populate    <- ""
    } else {
      populate    <- glue(' -cmd ".read {populate_file}"')
    }
    full_call     <- glue('{build}{populate}') %>%
      str_remove_all("\\./")
    full_call     <- glue('{sqlite_call} {full_call} -cmd .exit')
    log_trace("Issuing shell command {full_call}")
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
    remove_db(db = db, archive = archive)
    # -- Create the build commands in R to pass through RSQLite::SQLite()
    build_path <- list.files(pattern = build_from,
                             full.names = TRUE,
                             recursive = TRUE)
    header     <- "/\\*\\=+\\n[[:print:][:cntrl:]]+\\n\\=+\\*/"
    section    <- "/\\* -- [[:print:][:cntrl:]]+ -- \\*/"
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
  log_trace('Checking argument validity...')
  if (length(db) > 1)       stop('Argument "db" must be a single value.')
  if (!is.character(db))    stop('Argument "db" must be a character scalar.')
  if (length(archive) > 1)  stop('Argument "archive" must be a single value.')
  if (!is.logical(archive)) stop('Argument "archive" must be a logical scalar.')
  log_trace('All arguments valid')
  # Resolve database file location
  log_trace('Finding database file "{db}"')
  where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
  if (length(where) > 0) {
    # Ensure no current connection from R
    check <- tryCatch(manage_connection(db = db, disconnect = TRUE))
    if (class(check) == "try-error") stop("Unable to automatically manage the connection to", db, ".")
    if (archive) {
      now       <- format(Sys.time(), "%Y%m%d%H%M%S%Z")
      new_fname <- gsub(file_path_sans_ext(db), sprintf("%s_archive_%s", file_path_sans_ext(db), now))
      file.copy(where, new_fname)
      log_success('Archive created as "{new_fname}"')
    }
    result <- tryCatch(file.remove(where))
    if (class(result) == "try-error") {
      log_error('Database "{db}" could not be removed; another connection is likely open elsewhere.')
    } else {
      log_success('Database "{db}" removed.')
    }
  } else {
    log_warn('Database "{db}" does not exist in this directory tree.')
  }
}

#' Check for, and optionally remove, a database connection object
#'
#' @param db CHR scalar name of the database to check, defaults to the name
#'   supplied in config/env.R
#' @param drv CHR vector of connection object classes to check against. Note
#'   this may depend heavily on connection packages and must be present in the
#'   class names of the driver used. (default "SQLiteConnection")
#' @param disconnect LGL scalar of whether or not to terminate and remove the
#'   connection from the current global environment (default TRUE)
#'
#' @return None
#' @export
#'
#' @examples
manage_connection <- function(db = DB_NAME, drv = "SQLiteConnection", disconnect = TRUE) {
  current_env <- as.list(.GlobalEnv)
  current_env_names <- names(current_env)
  for (env in current_env_names) {
    this_obj <- current_env[[env]]
    if (any(drv %in% class(this_obj))) {
      if (basename(this_obj@dbname) == db) {
        log_trace('Database "{db}" is currently open.')
        if (disconnect) {
          log_trace('Closing and removing from environment...')
          dbDisconnect(this_obj)
          rm(list = env, pos = ".GlobalEnv")
        }
      }
    }
  }
}
