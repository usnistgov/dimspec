#' R session information for support needs
#'
#' Several items of interest for this particular project including:
#' - DB_DATE, DB_VERSION, BUILD_FILE, LAST_DB_SCHEMA, LAST_MODIFIED, DEPENDS_ON,
#'   and EXCLUSIONS as defined in \code{\link{env.R}}
#'
#' @param app_info BOOL scalar on whether to return this application's properties
#'
#' @return LIST of values
#' @export
#'
#' @example support_info()
support_info <- function(app_info = TRUE) {
  arg_check <- verify_args(
    args       = as.list(environment()),
    conditions = list(list(c("mode", "logical"),
                           c("length", 1))),
    from_fn    = "support_info")
  if (!arg_check$valid) {
    stop(cat(paste0(arg_check$messages, collapse = "\n")))
  }
  if (!exists("DB_VERSION")) source(list.files(pattern = "env.R", recursive = TRUE))
  sys_info <- list(
      system          = sessionInfo(),
      project_dir     = getwd(),
      files_available = sort(list.files(full.names = TRUE, recursive  = TRUE)),
      packs_installed = sort(installed.packages()[, 3]),
      packs_active    = sort((.packages()))
  )
  if (app_info) {
    app <- list(
      DB_DATE         = DB_DATE,
      DB_VERSION      = DB_VERSION,
      BUILD_FILE      = BUILD_FILE,
      LAST_DB_SCHEMA  = LAST_DB_SCHEMA,
      LAST_MODIFIED   = LAST_MODIFIED,
      DEPENDS_ON      = DEPENDS_ON,
      EXCLUSIONS      = EXCLUSIONS
    )
    out <- c(app, sys_info)
  } else {
    out <- sys_info
  }
  return(out)
}

#' Get list of available functions
#'
#' Helper function for \code{\link{verify_args()}} that returns all the
#' currently available functions matching a given prefix. This searches the
#' entire library associated with the current R install.
#'
#' Note: argument \code{use_deprecated} is not currently used but serves as a
#' placeholder for future development to avoid or include deprecated functions
#'
#' @param prefix CHR scalar for the function prefix to search (default "is")
#' @param use_deprecated BOOL scalar indicating whether or not to include
#'   functions marked as deprecated (PLACEHOLDER default FALSE)
#'
#' @return CHR vector of functions matching \code{prefix}
#' @export
#'
#' @examples
#' mode_checks()
mode_checks <- function(prefix = "is", use_deprecated = FALSE) {
  funcs        <- unlist(lapply(paste0("package:", (.packages())), ls))
  mode_types   <- grep(paste0("^", prefix, "[\\._]"), funcs, value = TRUE)
  mode_types   <- mode_types[-grep("<-", mode_types)]
  mode_types   <- sort(mode_types)
  return(mode_types)
}

#' Verify arguments for a function
#'
#' This helper function checks arguments against a list of expectations. This
#' was in part inspired by the excellent \code{testthis} package and shares
#' concepts with the \code{Checkmate} package. However, this function performs
#' many of the common checks without additional package dependencies, and can be
#' inserted into other functions for a project easily with:
#'
#' \preformatted{ arg_check <- verify_args( args       = as.list(environment()),
#' conditions = list( list(c("mode", "logical"), c("length", 1)), list( ... )))
#' }
#'
#' and check the return with
#'
#' \preformatted{ if (!arg_check$valid) cat(paste0(arg_check$messages, "\n"))}
#'
#' where argument \code{conditions} describes the tests. This comes at the price
#' of readability as the list items in `\code{conditions} do not have to be
#' named, but can be to improve clarity. See more details below for argument
#' \code{conditions} to view which expectations are currently supported.
#'
#' As this is a nested list condition check, it can also originate from any
#' source coercible to a list (e.g. JSON, XML, etc.) and this feature, along
#' with the return of human-meaningful evaluation strings, is particularly
#' useful for development of shiny applications. Values from other sources MUST
#' be coercible to a full list (e.g. if being parsed from JSON, use
#' \code{jsonlite::fromJSON(simplifyMatrix = FALSE))}
#'
#' If logger is enabled, also provides some additional meaningful feedback.
#'
#' At least one condition check is required for every element passed to `args`.
#'
#' @param args LIST of named arguments and their values, typically passed
#'   directly from a function definition in the form \code{args = list(foo =
#'   1:2, bar = c("a", "b", "c"))} or directly by passing \code{environment()}
#' @param conditions Nested LIST of conditions and values to check, with one
#'   list item for each element in \code{args}. \itemize{\item The first element
#'   of each list should be a character scalar in the supported list. \item The
#'   second element of each list should be the check values themselves and may
#'   be of any type.} Multiple expectation conditions can be set for each
#'   element of \code{args} in the form \itemize{\item \code{conditions =
#'   list(foo = list(c("mode", "numeric"), c("length", 2)), bar = list(c("mode",
#'   "character"), c("n<", 5)))}} Currently supported expectations are:\itemize{
#'   \item \preformatted{"class"    checks strict class expectation by direct
#'   comparison with \code{class} to support object classes not supported with
#'   the \code{is.x} or \code{is_x} family of functions; much stricter than a
#'   "mode" check in that the requested check must be present in the return from
#'   a call to \code{class} e.g. "list" will fail if a "data.frame" object is
#'   passed \item \preformatted{"mode"    checks class expectation by applying
#'   the \code{is.X} or the \code{is_X} family of functions either directly or
#'   flexibly depending on the value provided to \code{conditions} (e.g.
#'   c("mode", "character") and c("mode", "is.character") and c("mode",
#'   "is_character") all work equally well) and will default to the version you
#'   provide explicitly (e.g. if you wish to prioritize "is_character" over
#'   "is.character" simply provide "is_character" as the condition. Only those
#'   modes able to be checked by this family of functions are supported. Run
#'   function \code{mode_checks()} for a complete sorted list for your current
#'   configuration.} \item \preformatted{"length" length of values matches a
#'   pre-determined exact length, typically a single value expectation (e.g.
#'   c("length",#'   1))} \item \preformatted{"no_na" no NA values are present}
#'   \item \preformatted{"n>"      length of values is greater than a given
#'   value - "n<" length of values is lesser than a given value} \item
#'   \preformatted{"n>="     length of values is greater than or equal to a
#'   given value} \item \preformatted{"n<="     length of values is lesser than
#'   or equal to a given value} \item \preformatted{">" numeric or date value is
#'   greater than a given value} \item \preformatted{"<" numeric or date value
#'   is greater than a given value} \item \preformatted{">=" numeric or date
#'   value is greater than or equal to a given value} \item \preformatted{"<="
#'   numeric or date value is lesser than or equal to a given value} \item
#'   \preformatted{"between" numeric or date values are bound within an
#'   INCLUSIVE range (e.g. c("range", 1:5))} \item \preformatted{"choices"
#'   provided values are part of a selected list of expectations (e.g.
#'   \code{c("choices", list(letters[1:3]))})} \item \preformatted{"FUN" apply a
#'   function to the value and check that the result is valid or that the
#'   function can be executed without error; this evaluates the check condition
#'   using \code{tryCatch} via \code{do.call} and so can also accept a full
#'   named list of arg values. This is a strict check in the sense that a
#'   warning will also result in a failed result, passing the warning (or error
#'   if the function fails) message back to the user, but does not halt checks}
#'   }
#' @param from_fn CHR scalar of the function from which this is called, used if
#'   logger is enabled and ignored if not. (default NULL)
#'
#' @return LIST of the resulting values and checks, primarily useful for
#'   $message
#' @export
#'
#' @examples
#' \preformatted{
#' verify_args(args = list(character_length_2 = c("a", "b")),
#'             conditions = list(character_length_2 = list(c("mode", "character"),
#'                                                         c("length", 3))
#' )
#' verify_args(args = list(boolean = c(TRUE, FALSE, TRUE)),
#'             conditions = list(list(c("mode", "logical"),
#'                                    c("length", 1)))
#' )
#' verify_args(args = list(foo = c(letters[1:3]),
#'                         bar = 1:10),
#'             conditions = list(foo = list(c("mode", "numeric"),
#'                                          c("n>", 5)),
#'                               bar = list(c("mode", "logical"),
#'                                          c("length", 5),
#'                                          c(">", 10),
#'                                          c("between", list(100, 200)),
#'                                          c("choices", list("a", "b")))))
#' }
verify_args <- function(args, conditions, from_fn = NULL) {
  if (is.null(from_fn)) {
    from_fn <- ""
  } else {
    from_fn <- glue(' for function "{from_fn}"')
  }
  logger <- "logger" %in% (.packages())
  if (logger) log_trace('Verifying arguments{from_fn}.')
  require(glue)
  if (length(args) != length(conditions)) stop('Each item in "args" needs at least one matching condition.')
  check_types  <- c("class", "mode", "length", "no_na", "n>", "n<", "n>=", "n<=", ">", "<", ">=", "<=", "between", "choices", "FUN")
  supported    <- paste0("'", check_types, "'", collapse = ", ")
  mode_types   <- mode_checks()
  out          <- list()
  out$args     <- args
  out$checked  <- setNames(conditions, names(args))
  out$valid    <- TRUE
  out$results  <- vector("list", length = length(args))
  out$messages <- vector("character", length = 0L)
  for (i in 1:length(args)) {
    arg   <- args[i]
    needs <- conditions[[i]]
    out$results[[i]] <- vector("logical", length = length(needs))
    for(j in 1:length(needs)) {
      rslt  <- TRUE
      msg   <- character(0)
      x     <- needs[[j]]
      val   <- arg[[1]]
      type  <- unlist(x[1])
      check <- unlist(x[2])
      switch(type,
             "class"   = {
               rslt <- check %in% class(val)
               msg  <- glue("'{names(arg)}' must be of class '{check} rather than '{class(val)}'.")
             },
             "mode"    = {
               mode_check <- grep(paste0("is[\\._]", check), mode_types, value = TRUE)
               if (length(mode_check) > 0) {
                 mode_check <- mode_check[1]
               } else {
                 mode_check <- check[[1]]
               }
               if (length(mode_check) == 1) {
                 rslt <- do.call(mode_check, list(val))
                 msg  <- glue("Argument '{names(arg)}' (checked with '{mode_check}') must be of class '{check}' rather than '{class(val)}'.")
               } else {
                 rslt <- FALSE
                 msg  <- glue("Unable to find function '{mode_check}' check argument '{names(arg)}' with '{mode_check}' as it ")
               }
             },
             "length"  = {
               rslt <- length(val) == as.integer(check)
               msg  <- glue("Argument '{names(arg)}' must be of length {check} rather than {length(val)}.")
             },
             "no_na"   = {
               rslt <- !any(is.na(val))
               msg  <- glue("Argument '{names(arg)}' includes NA values.")
             },
             "n>"      = {
               rslt <- length(val) > as.integer(check)
               msg  <- glue("Argument '{names(arg)}' must be of length greater than {check} rather than {length(val)}.")
             },
             "n<"      = {
               rslt <- length(val) < as.integer(check)
               msg  <- glue("Argument '{names(arg)}' must be of length lesser than {check} rather than {length(val)}.")
             },
             "n>="     = {
               rslt <- length(val) >= as.integer(check)
               msg  <- glue("Argument '{names(arg)}' must be of length greater than or equal to {check} rather than {length(val)}.")
             },
             "n<="     = {
               rslt <- length(val) <= as.integer(check)
               msg  <- glue("Argument '{names(arg)}' must be of length lesser than or equal to {check} rather than {length(val)}.")
             },
             ">"       = {
               if (length(check) == 1) {
                 rslt <- all(val > check)
                 msg  <- glue("Argument '{names(arg)}' must be greater than {check}.")
               } else {
                 rslt <- FALSE
                 msg  <- glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
               }
             },
             "<"       = {
               if (length(check) == 1) {
                 rslt <- all(val < check)
                 msg  <- glue("Argument '{names(arg)}' must be lesser than {check}.")
               } else {
                 rslt <- FALSE
                 msg  <- glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
               }
             },
             ">="       = {
               if (length(check) == 1) {
                 rslt <- all(val >= check)
                 msg  <- glue("Argument '{names(arg)}' must be greater than or equal to {check}.")
               } else {
                 rslt <- FALSE
                 msg  <- glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
               }
             },
             "<="       = {
               if (length(check) == 1) {
                 rslt <- all(val <= check)
                 msg  <- glue("Argument '{names(arg)}' must be lesser than or equal to {check}.")
               } else {
                 rslt <- FALSE
                 msg  <- glue("Expected an exclusive upper bound, but {length(check)} value(s) provided: {check}.")
               }
             },
             "between" = {
               if (length(check) == 2) {
                 rslt <- all(val >= check[1] && val <= check[2])
                 if (is.numeric(val)) {
                   msg  <- glue("Argument '{names(arg)}' must be between {check[1]} and {check[2]} but the range was {paste0(range(val), collapse = ' - ')}.")
                 } else {
                   msg  <- glue("Argument '{names(arg)}' must be a numeric or date vector between {check[1]} and {check[2]} but was provided as {class(arg)}.")
                 }
               } else {
                 if (length(check) < 2) {
                   len_check <- "less than"
                 } else {
                   len_check <- "more than"
                 }
                 rslt <- FALSE
                 msg  <- glue("Expected an inclusive bounding range, but {len_check} 2 values were provided: {check}.")
               }
             },
             "choices" = {
               rslt <- all(val %in% check)
               msg  <- glue("Argument '{names(arg)}' was not present in c({paste0(check, collapse = ', ')}).")
             },
             "call"    = {
               rslt <- tryCatch(do.call(check, list(val)),
                                error   = function(e) e,
                                warning = function(w) w)
               val  <- paste0('"', val, '"', collapse = ", ")
               if ("simpleError" %in% class(rslt)) {
                 msg  <- glue("Error evaluating 'do.call({check}, list({val}))': \"{rslt$message}\"")
                 rslt <- FALSE
               } else if ("simpleWarning" %in% class(rslt)) {
                 msg  <- glue("Warning on 'do.call({check}, list({val}))': \"{rslt$message}\"")
                 rslt <- FALSE
               } else {
                 rslt <- TRUE
               }
             },
             {
               rslt <- FALSE
               msg  <- glue("Could not match condition type '{type}' for argument '{names(arg)}'. Ensure the check is one of: {supported}")
             }
      )
      out$results[[i]][j]   <- rslt
      if (!rslt) {
        out$valid           <- FALSE
        n_msg <- length(out$messages) + 1
        out$messages[n_msg] <- msg
        if (logger) log_warn(msg)
      }
    }
  }
  if (logger) {
    if (out$valid) {
      log_trace('Arguments verified for {from_fn}.')
    } else {
      error_appendix <- ifelse(log_threshold() < 300, " See return for details.", "")
      log_error('Arguments could not be verified{from_fn}.{error_appendix}')
    }
  }
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
          dbDisconnect(this_obj)
          rm(list = env, pos = ".GlobalEnv")
        }
      }
    }
  }
  if (reconnect) {
    # Resolve database file location
    if (logger) log_trace('Finding database file "{db}"')
    where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
    if (length(where) == 0) {
      stop(sprintf("Unable to locate '%s'.", db))
    }
    require(drv_pack, character.only = TRUE)
    con <<- try(dbConnect(do.call(drv, list()), db))
    if (class(con) == "try-error") {
      stop(sprintf("Unable to connect to '%s'.", where))
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

#' Grammatically collapse a list of values
#'
#' Given a vector of arbitrary length that coerces properly to a human-readable
#' character string, return it formatted as one of: "one", "one and two", or
#' "one, two, ..., and three" using `glue::glue`
#'
#' @param namelist vector of values to format
#' 
#' @return CHR vector of length one
#' @export
#' 
#' @examples
#' format_list_of_names("test")
#' format_list_of_names(c("apples", "bananas"))
#' format_list_of_names(c(1:3))
#' format_list_of_names(seq.Date(Sys.Date(), Sys.Date() + 3, by = 1))
format_list_of_names <- function(namelist) {
  if ( length(namelist) == 1 )
    return(glue::glue("{paste0(namelist, collapse = '')}"))
  res <- glue::glue(
    "{paste0(namelist[1:length(namelist) - 1], collapse = ', ')}\\
     {ifelse(length(namelist) > 2, ',', '')} \\
     {ifelse(length(namelist) > 1, paste('and ', namelist[length(namelist)], sep = ''), '')}")
  return(res)
}
