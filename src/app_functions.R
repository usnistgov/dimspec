#' R session information for support needs
#'
#' Several items of interest for this particular project including:
#' - DB_DATE, DB_VERSION, BUILD_FILE, LAST_DB_SCHEMA, LAST_MODIFIED, DEPENDS_ON,
#'   and EXCLUSIONS as defined in the project's ../config/env_R.R file.
#'
#' @param app_info BOOL scalar on whether to return this application's properties
#'
#' @return LIST of values
#' @export
#'
#' @example support_info()
support_info <- function(app_info = TRUE) {
  log_fn("start")
  arg_check <- verify_args(
    args       = as.list(environment()),
    conditions = list(list(c("mode", "logical"),
                           c("length", 1))),
    from_fn    = "support_info")
  if (!arg_check$valid) {
    stop(cat(paste0(arg_check$messages, collapse = "\n")))
  }
  if (!exists("DB_VERSION")) source(list.files(pattern = "env.R", recursive = TRUE))
  if (exists("log_it")) log_it("debug", "Gathering system information...")
  sys_info <- list(
    system          = sessionInfo(),
    project_dir     = getwd(),
    files_available = sort(list.files(full.names = TRUE, recursive  = TRUE)),
    packs_installed = sort(installed.packages()[, 3]),
    packs_active    = sort((.packages()))
  )
  if (app_info) {
    if (exists("log_it")) log_it("debug", "Gathering application information...")
    global <- as.list(.GlobalEnv)
    app <- list(
      DB_NAME         = if (exists("DB_NAME")) DB_NAME else "Not set",
      DB_DATE         = if (exists("DB_DATE")) DB_DATE else "Not set",
      DB_VERSION      = if (exists("DB_VERSION")) DB_VERSION else "Not set",
      DB_SETTINGS     = if (any(grepl("DB_", names(global)))) global[grepl("DB_", names(global))] else "Not set",
      BUILD_FILE      = if (exists("DB_BUILD_FILE")) {
        c(file = DB_BUILD_FILE, exists = file.exists(DB_BUILD_FILE))
      } else {
        "Not set"
      },
      BUILD_FILE_FULL = if (exists("DB_BUILD_FULL")) {
        c(file = DB_BUILD_FULL, exists = file.exists(DB_BUILD_FULL))
      } else {
        "Not set"
      },
      POPULATED_WITH  = if (exists("DB_DATA")) {
        c(file = DB_DATA, exists = file.exists(DB_DATA))
      } else {
        "Not set"
      },
      LAST_DB_SCHEMA  = if (exists("LAST_DB_SCHEMA")) LAST_DB_SCHEMA else "Not set",
      LAST_MODIFIED   = if (exists("LAST_MODIFIED")) LAST_MODIFIED else "Not set",
      DEPENDS_ON      = if (exists("DEPENDS_ON")) DEPENDS_ON else "Not set",
      EXCLUSIONS      = if (exists("EXCLUSIONS")) EXCLUSIONS else "Not set",
      EXPLICIT_PATHS  = if (exists("EXPLICIT_PATHS")) EXPLICIT_PATHS else "Not set",
      RUNNING         = c(
        "API" = if (exists("USE_API")) USE_API else "Not set",
        "RDK" = if (exists("USE_RDKIT")) USE_RDKIT else "Not set",
        "SHINY" = if (exists("USE_SHINY")) USE_SHINY else "Not set",
        "DB" = if (exists("INIT_CONNECT")) INIT_CONNECT else "Not set"
      ),
      LOGGER          = c(global[grepl("^LOG[G_]", names(global))],
                          FILES_EXIST <- lapply(
                            global[grepl("LOG_FILE", names(global))],
                            file.exists) %>%
                            setNames(sprintf("%s_EXISTS", grep("LOG_FILE", names(global), value = TRUE)))
      ),
      CLI_AVAILABLE   = lapply(global[grepl("CLI", names(global))],
                               Sys.which),
      PID             = ifelse(exists("con"), tbl(con, "config") %>% pull(code), NA)
    )
    out <- c(app, sys_info)
  } else {
    out <- sys_info
  }
  log_fn("end")
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
  log_fn("start")
  funcs        <- unlist(lapply(paste0("package:", (.packages())), ls))
  mode_types   <- grep(paste0("^", prefix, "[\\._]"), funcs, value = TRUE)
  mode_types   <- mode_types[-grep("<-", mode_types)]
  mode_types   <- sort(mode_types)
  log_fn("end")
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
#'   logger is enabled and ignored if not; by default it will pull the calling
#'   function's name from the call stack, but can be overwritten by a manual
#'   entry here for better tracing. (default NULL)
#' @param silent LGL scalar of whether to silence warnings for individual
#'   failiures, leaving them only as part of the output. (default: FALSE)
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
verify_args <- function(args, conditions, from_fn = NULL, silent = FALSE) {
  log_fn("start")
  if (exists("VERIFY_ARGUMENTS")) {
    if (!VERIFY_ARGUMENTS) {
      if (exists("log_it")) log_it("warn", "Argument verification is currently off.")
      return(list(valid = TRUE))
    }
  }
  if (length(args) != length(conditions)) {
    if (exists("log_it")) log_it("error", sprintf('Length of "args" [%s] must match the length of "conditions" [%s]',
                                                  length(args),
                                                  length(conditions)))
  }
  if (is.environment(args)) {
    args <- as.list(args)
  }
  if (is.list(args)) {
    n_arg_names <- length(names(args))
    n_con_names <- length(names(conditions))
    if (n_arg_names == 0) {
      if (n_con_names == 0) {
        if (exists("log_it")) log_it("trace", "Arguments and conditions are unnamed. Order will be inferred by index.")
      } else {
        if (exists("log_it")) log_it("trace", "Arguments provided are unnamed. Order will be inferred from the order of conditions.")
        names(args) <- names(conditions)
      }
    } else if (n_con_names == 0) {
      if (exists("log_it")) log_it("trace", "Conditions provided are unnamed. Order will be inferred from the order of arguments.")
      names(conditions) <- names(args)
    } else if (!n_arg_names == n_con_names) {
      stop("Length of named arguments did not match the length of named conditions.")
    } else {
      args <- args[names(conditions)]
    }
  } else if (!any(is.character(args), !is.list(args))) {
    stop("Parameter 'args' must be either an environment or a list of values.")
  }
  if (rlang::is_null(from_fn)) from_fn <- deparse(sys.call(-1)[[1]])
  if (exists("log_it")) log_it("trace", glue('Verifying arguments for "{from_fn}".'))
  if (length(args) != length(conditions)) stop('Each item in "args" needs at least one matching condition.')
  check_types  <- c("class", "mode", "length", "no_na", "n>", "n<", "n>=", "n<=", ">", "<", ">=", "<=", "between", "choices", "FUN", "not_empty", "file_exists")
  supported    <- paste0("'", check_types, "'", collapse = ", ")
  mode_types   <- mode_checks()
  out          <- list(
    valid    = TRUE,
    args     = args,
    checked  = conditions,
    results  = vector("list", length = length(args)),
    messages = vector("character", length = 0L)
  )
  names(out$args)    <- names(conditions)
  names(out$checked) <- names(conditions)
  names(out$results) <- names(conditions)
  for (i in 1:length(args)) {
    arg   <- args[i]
    if (is.null(names(arg))) {
      names(arg) <- glue("argument_{i}")
    }
    needs <- conditions[[i]]
    if (exists("log_it")) log_it("trace", glue('Verify provided value of "{paste0(args[i], collapse = \'", "\')}"'))
    out$results[[i]] <- vector("logical", length = length(needs))
    for(j in 1:length(needs)) {
      rslt  <- TRUE
      msg   <- character(0)
      x     <- needs[[j]]
      val   <- arg[[1]]
      type  <- unlist(x[1])
      check <- unlist(x[-1])
      if (missing(val)) {
        rslt <- FALSE
        msg  <- glue('Parameter "{names(arg)}" is required.')
      } else {
        switch(type,
               "class"   = {
                 rslt <- all(check %in% class(val))
                 class_real <- class(val)
                 class_need <- check[!check %in% class_real]
                 class_real <- glue('{ifelse(length(class_real) == 1, "", "es")} {format_list_of_names(class_real, add_quotes = TRUE)}')
                 class_need <- glue('{ifelse(length(class_need) == 1, "", "es")} {format_list_of_names(class_need, add_quotes = TRUE)}')
                 msg  <- glue('Parameter "{names(arg)}" of class{class_real} was missing required class{class_need}.')
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
                 msg  <- glue('Argument "{names(arg)}" was not present in choice list c("{paste0(check, collapse = \'", "\')}").')
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
               "not_empty" = {
                 msg  <- glue("Object contained nothing but NAs, NULLs, or empty character strings.")
                 rslt <- !has_missing_elements(val)
               },
               "file_exists" = {
                 msg  <- glue("Could not locate file '{val}'.")
                 rslt <- file.exists(val)
               },
               {
                 rslt <- FALSE
                 msg  <- glue("Could not match condition type '{type}' for argument '{names(arg)}'. Ensure the check is one of: {supported}")
               }
        )
      }
      log_msg <- glue('  Check type is "{type}" against: "{format_list_of_names(check)}"')
      out$results[[i]][j]   <- rslt
      if (!rslt) {
        out$valid           <- FALSE
        n_msg <- length(out$messages) + 1
        out$messages[n_msg] <- msg
        log_it("trace", glue('{log_msg} - FAIL'))
        if (!silent) log_it("error", glue("    {msg}"))
      } else {
        if (exists("log_it")) log_it("trace", glue('{log_msg} - PASS'))
      }
    }
  }
  if (out$valid) {
    if (exists("log_it")) log_it("trace", glue("Arguments verified for '{from_fn}'"))
  } else {
    if (exists("log_it")) log_it("error", glue("Arguments could not be verified",
                                               ifelse(from_fn == "NULL", ".", " for '{from_fn}'."),
                                               " See return for details."))
  }
  log_fn("end")
  return(out)
}

#' Grammatically collapse a list of values
#'
#' Given a vector of arbitrary length that coerces properly to a human-readable
#' character string, return it formatted as one of: "one", "one and two", or
#' "one, two, ..., and three" using `glue::glue`. This is functionally the same
#' as a static version of [glue::glue_collapse] with parameters sep = ", ",
#' width = Inf, and last = ", and ".
#'
#' @param namelist vector of values to format
#' @param add_quotes LGL scalar of whether to enclose individual values in
#'   quotation marks
#'
#' @return CHR vector of length one
#' @export
#'
#' @examples
#' format_list_of_names("test")
#' format_list_of_names(c("apples", "bananas"))
#' format_list_of_names(c(1:3))
#' format_list_of_names(seq.Date(Sys.Date(), Sys.Date() + 3, by = 1))
format_list_of_names <- function(namelist, add_quotes = FALSE) {
  log_fn("start")
  require(glue)
  if (length(namelist) == 1) {
    res <- glue::glue("{paste0(namelist, collapse = '')}")
  } else {
    res <- glue::glue(
      "{paste0(namelist[1:length(namelist) - 1], collapse = ', ')}\\
     {ifelse(length(namelist) > 2, ',', '')} \\
     {ifelse(length(namelist) > 1, paste('and ', namelist[length(namelist)], sep = ''), '')}")
  }
  if (add_quotes) {
    res <- str_c('"',
                 res %>%
                   str_replace_all(', ', '", "') %>%
                   str_replace_all(' and ', '" and "') %>%
                   str_replace_all(' "and ', ' and "'),
                 '"')
  }
  log_fn("end")
  return(res)
}

#' Conveniently log a message to the console
#'
#' Use this to log messages of arbitrary level and message. It works best with
#' [logger] but will also print directly to the console to support setups where
#' package [logger] may not be available or custom log levels are desired.
#'
#' When using [logger], create settings for each namespace in file
#' `config/env_logger.R` as a list (see examples there) and make sure it is
#' sourced. If using with [logger] and "file" or "both" is selected for the
#' namespace `LOGGING[[x]]$TO` parameter in `env_logger.R` logs will be written
#' to disk at the file defined in `LOGGING[[x]]$file` as well as the console.
#'
#' @param log_level CHR scalar of the level at which to log a given statement.
#'   If using the [logger] package, must match one of [logger:::log_levels]
#' @param msg CHR scalar of the message to accompany the log.
#'
#' @return Adds to the logger file (if enabled) and/or prints to the console if
#'   enabled. See
#' @export
#'
#' @examples
#' log_it("test", "a test message")
#' test_log <- function() {
#'   log_it("success", "a success message")
#'   log_it("warn", "a warning message")
#' }
#' test_log()
#' # Try it with and without logger loaded.
log_it <- function(log_level, msg = NULL, ns = NULL, reload_logger_settings = FALSE, logger_settings = file.path("config", "env_logger.R"), add_unknown_ns = FALSE, clone_settings_from = NULL) {
  if (is.null(msg) || is.na(msg)) msg <- "[no message provided]"
  if (!exists("LOGGING_ON")) {
    if (reload_logger_settings) {
      if (file.exists(logger_settings)) {
        source(logger_settings)
      } else {
        stop(sprintf('Logging settings not configured. File "%s" not available.', logger_settings))
      }
    }
    LOGGING_ON <- TRUE
  }
  call_func <- sys.call(-1)[[1]]
  if (sys.nframe() > 1) {
    from_log_it <- call_func == "log_it"
  } else {
    from_log_it <- TRUE
    call_func <- rlang::sym("log_it")
  }
  do_log <- TRUE
  if (LOGGING_ON) {
    if (is.na(ns) || is.null(ns)) {
      ns <- NA_character_
    } else {
      if (!exists("LOGGING")) {
        log_it("warn", sprintf('Logging is not set up for namespace "%s".', ns))
        if (add_unknown_ns) {
          log_it("warn", "Setting up a default namespace for interactive logging only.")
          assign(x = "LOGGING", value = setNames(list(list(log = TRUE, ns = ns, threshold = "trace")), toupper(ns)), envir = .GlobalEnv)
        } else {
          log_it("info", sprintf('Call again with "add_unknown_ns = TRUE" to establish the "%s" namespace.', ns))
        }
      } else {
        logging_set <- toupper(ns) %in% names(LOGGING)
        if (logging_set) {
          do_log <- LOGGING[[toupper(ns)]]$log
        } else {
          log_it("warn", sprintf('Logging namespace "%s" is not set up.', ns))
          clone_exists <- !is.null(clone_settings_from) && toupper(clone_settings_from) %in% names(LOGGING)
          if (clone_exists) {
            i <- grep(toupper(clone_settings_from), names(LOGGING))
          } else {
            clone_settings_from <- names(LOGGING)[1]
            i <- 1
          }
          settings_from <- LOGGING[[i]]$ns
          if (add_unknown_ns) {
            if (clone_exists) {
              log_it("info", sprintf('Copying settings for "%s" from LOGGING[["%s"]]. Access settings at LOGGING[["%s"]]', ns, toupper(settings_from), toupper(ns)))
            } else {
              log_it("warn", sprintf('Clone namespace "%s" is not set up. Settings for "%s" will be used instead.', tolower(clone_settings_from), settings_from))
            }
            LOGGING[[toupper(ns)]] <<- LOGGING[[i]]
            LOGGING[[toupper(ns)]]$ns <<- ns
            if (exists("update_logger_settings")) update_logger_settings()
          } else {
            log_it("info", sprintf('Call again with "add_unknown_ns = TRUE" to establish the "%s" namespace with the same settings as "%s".', ns, settings_from))
            log_it("info", sprintf('Logs for "%s" will only be available in the console.', ns))
          }
          do_log <- TRUE
        }
      }
    }
    if (!exists("do_log")) browser()
    if (do_log) {
      log_func  <- sprintf("log_%s", tolower(log_level))
      log_level <- toupper(log_level)
      n_call    <- ifelse(sys.nframe() > 1, -1, 1)
      if (exists(log_func)) {
        log_level(level    = log_level,
                  namespace = ns,
                  .topcall = sys.call(n_call),
                  msg)
      } else {
        # # See below comment about package "cli"...if that reoute is desired, it is necessary to uncomment this block.
        #
        # log_level <- switch(log_level,
        #                     "WARN" = "WARNING",
        #                     "ERROR" = "DANGER",
        #                     log_level)
        msg <- sprintf("[%s] <%s> %s in fn %s(): %s\n",
                       format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                       ifelse(is.na(ns), "global", ns),
                       log_level,
                       deparse(sys.call(n_call)[[1]]),
                       msg)
        # # Possible integration of package "cli" for this, but it's getting very close to recreating some aspects of "logger", so just use logger.
        # 
        # use_func <- sprintf("cli_alert_%s", tolower(log_level))
        # if (!exists(use_func)) {
        #   use_func <- "cli_alert" 
        # }
        # if (exists(use_func)) {
        #   do.call(use_func, args = list(msg))
        # } else {
        cat(msg)
        # }
      }
    } else {
      if (!LOGGING[[toupper(ns)]]$log) {
        msg <- sprintf('Logging is currently turned off for namespace "%s". Set LOGGING$%s$log to TRUE to begin logging.\n',
                       ns,
                       toupper(ns))
        log_it("warn", msg)
      }
    }
  } else {
    if (!LOGGING_ON && (!exists("LOG_IS_OFF") || !LOG_IS_OFF)) {
      assign(x = "LOG_IS_OFF", value = TRUE, envir = .GlobalEnv)
      msg <- 'Set LOGGING_ON to TRUE to activate logging functionality. This message will only appear once.\n'
      cat(msg)
    }
  }
}

#' Simple acronym generator
#'
#' At times it is useful for display purposes to generate acronyms for longer
#' bits of text. This naively generates those by extracting the first letter as
#' upper case from each word in `text` elements.
#'
#' @param text CHR vector of the text to acronym-ize
#'
#' @return CHR vector of length equal to that of `text` with the acronym
#' @export
#'
#' @examples
#' make_acronym("test me")
#' make_acronym(paste("department of ", c("commerce", "energy", "defense")))
make_acronym <- function(text) {
  log_fn("start")
  acronym <- text %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_to_title() %>%
    str_extract_all("[A-Z]") %>%
    lapply(function(x) paste0(x, collapse = "")) %>%
    unlist()
  log_fn("end")
  return(acronym)
}

#' Simple check for if an object is empty
#'
#' Checks for empty vectors, a blank character string, NULL, and NA values. If
#' fed a list object, returns TRUE if any element is is the "empty" set. For
#' data.frames checks that nrow is not 0. [rlang:::is_empty] only checks for
#' length 0.
#'
#' @note Reminder that vectors created with NULL values will be automatically
#'   reduced by R.
#'
#' @param x Object to be checked
#'
#' @return LGL scalar of whether `x` is empty
#' @export
#'
#' @examples
#' has_missing_elements("a")
#' # FALSE
#' has_missing_elements(c(NULL, 1:5))
#' # FALSE
#' has_missing_elements(list(NULL, 1:5))
#' # TRUE
#' has_missing_elements(data.frame(a = character(0)))
#' # TRUE
has_missing_elements <- function(x) {
  log_fn("start")
  if (is.function(x)) {
    out <- FALSE
    log_it("trace", sprintf("%s is a function, not a variable.", deparse(substitute(x))))
  } else if (is.data.frame(x)) {
    log_it("trace", sprintf("%s is a data frame; result checks whether it has 0 rows.", deparse(substitute(x))))
    out <- nrow(x) == 0
  } else if (is.list(x)) {
    log_it("trace", sprintf("%s is a list; result checks whether any element of that list is empty.", deparse(substitute(x))))
    out <- any(
      unlist(
        lapply(x,
               function(y) {
                 any(
                   y == "",
                   is.null(y),
                   is.na(y),
                   length(y) == 0
                 )
               })
      )
    )
  } else {
    out <- any(
      x == "",
      is.null(x),
      is.na(x),
      length(x) == 0
    )
  }
  log_fn("end")
  return(out)
}

#' Simple logging convenience
#'
#' Conveniently add a log message at the trace level. Typically this would be
#' called twice bookending the body of a function along the lines of "Start
#' fn()" and "End fn()" when calling a function. This can help provided
#' traceability to deeply nested function calls within a log.
#'
#' @param status CHR scalar to prefix the log message; will be coerced to
#'   sentence case. Typically "start" or "end" but anything is accepted (default
#'   "start").
#' @param log_ns CHR scalar of the logger namespace to use (default NA_character_)
#' @param level CHR scalar of the logging level to be passed to [log_it]
#'   (default "trace")
#'
#' @return None, hands logging messages to [log_it]
#' @export
#'
#' @examples
#' fn <- function() {log_fn("start"); 1+1; log_fn("end")}
#' fn()
log_fn <- function(status = "start", log_ns = NA_character_, level = "trace") {
  require(stringr)
  msg <- sprintf("%s %s()",
                 str_to_sentence(status),
                 as.character(sys.call(-1)[[1]])
  )
  log_it(level, msg, log_ns)
  if (!is.na(log_ns)) {
    log_it(level, msg, NA_character_)
  }
}

#' Flush a directory with archive
#'
#' Clear a directory and archive those files if desired in any directory
#' matching any pattern.
#'
#' @param archive LGL scalar on whether to archive current logs
#' @param directory CHR scalar path to the directory to flush
#'
#' @return
#' @export
#'
#' @examples
flush_dir <- function(directory, pattern, archive = FALSE) {
  logger <- exists("log_it")
  if (logger) log_fn("start")
  if (dir.exists(directory)) {
    files <- list.files(directory, full.names = TRUE)
    files <- grep(pattern = pattern, x = files, value = TRUE)
    if (archive) {
      archive_dir <- file.path(directory, "archive", format(Sys.time(), "%Y%m%d%H%M"))
      if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
      res <- lapply(files, 
                    function(x) {
                      file.rename(
                        from = x,
                        to = gsub(directory, archive_dir, x)
                      )
                    })
    } else {
      res <- lapply(files, file.remove)
    }
    success <- all(unlist(res))
    if (logger) {
      if (success) {
        log_it("success",
               sprintf("Files in '%s' were %s.",
                       directory,
                       ifelse(archive, sprintf("archived to '%s'", archive_dir), "flushed")
               )
        )
      } else {
        log_it("error",
               sprintf("Could not %s all files in '%s'.",
                       ifelse(archive, "archive", "remove"),
                       directory)
        )
      }
    }
  } else {
    if (logger) {
      log_it("warn", sprintf("The specified directory at '%s' was not found.", directory))
    }
  }
  if (logger) log_fn("end")
}

#' Rectify NULL values provided to functions
#'
#' To support redirection of sensible parameter reads from an environment,
#' either Global or System, functions in this package may include NULL as their
#' default value. This returns values in precedence of `parameter`,
#' `env_parameter` and `default`.
#'
#' @note `log_ns` is only applicable if logging is set up in this project (see
#'   project settings in env_glob.txt, env_R.R, and env_logger.R for details).
#'
#' @note Both [base::.GlobalEnv] and [base::Sys.getenv] are checked, and can be
#'   provided as a character scalar or as an object reference
#'
#' @param parameter the object being evaluated
#' @param env_parameter the name or object of a value to use from the
#'   environment if `parameter` is NULL
#' @param default the fallback value to use if `parameter` is NULL and
#'   `env_parameter` does not exist
#' @param log_ns the namespace to use with [log_it] if available
#'
#' @return
#' @export
#'
#' @examples
rectify_null_from_env <- function(parameter, env_parameter, default, log_ns = NA_character_) {
  logger <- exists("log_it") && LOGGING_ON
  if (logger) log_fn("start", log_ns)
  if (!is.null(parameter)) {
    par_name <- deparse(substitute(parameter))
    par_ref  <- ""
    suffix   <- "as provided"
    out <- parameter
  } else {
    env_par_name <- deparse(substitute(env_parameter))
    par_name <- env_par_name
    suffix   <- ""
    if (exists(env_par_name)) {
      par_ref  <- " environment"
      out      <- env_parameter
    } else {
      if (!Sys.getenv(env_par_name) == "") {
        par_ref <- " environment"
        out <- Sys.getenv(env_par_name)
      } else {
        par_ref <- " default"
        out <- default
        suffix <- sprintf(" as no environment parameter named '%s' is set", par_name)
      }
    }
  }
  par_value_str <- ifelse(length(out) > 1,
                          paste0('c("', paste0(out, collapse = '", "'), '")'),
                          paste0('"', out, '"'))
  if (par_ref == "") {
    feedback <- sprintf("%s ", par_value_str)
  } else if (par_ref == " default") {
    feedback <- sprintf("%s", par_value_str)
  } else {
    feedback <- sprintf("'%s = %s'", par_name, par_value_str)
  }
  if (logger) log_it("info", sprintf("Returning%s parameter %s%s.", par_ref, feedback, suffix), log_ns)
  if (logger) log_fn("end", log_ns)
  return(out)
}
