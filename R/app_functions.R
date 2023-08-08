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
#' @usage support_info()
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
    packs_installed = sort(installed.packages()[, 3]) %>% as.list(),
    packs_active    = sort((.packages()))
  )
  if (app_info) {
    if (exists("log_it")) log_it("debug", "Gathering application information...")
    global <- as.list(.GlobalEnv)
    app <- list(
      DB_SETTINGS     = if (any(grepl("DB_", names(global)))) global[grep("DB_", sort(names(global)), value = TRUE)] else "Not set",
      BUILD_FILE      = if (exists("DB_BUILD_FILE")) {
        fpath <- list.files(path = here::here(),
                            pattern = DB_BUILD_FILE,
                            full.names = TRUE,
                            recursive = TRUE)
        list(file = DB_BUILD_FILE,
             exists = !length(fpath) == 0,
             location = fpath)
      } else {
        "Not set"
      },
      BUILD_FILE_FULL = if (exists("DB_BUILD_FULL")) {
        fpath <- list.files(path = here::here(),
                            pattern = DB_BUILD_FULL,
                            full.names = TRUE,
                            recursive = TRUE)
        list(file = DB_BUILD_FULL,
             exists = !length(fpath) == 0,
             location = fpath)
      } else {
        "Not set"
      },
      POPULATED_WITH  = if (exists("DB_DATA")) {
        fpath <- list.files(path = here::here(),
                            pattern = DB_DATA,
                            full.names = TRUE,
                            recursive = TRUE)
        list(file = DB_DATA,
             exists = !length(fpath) == 0,
             location = fpath)
      } else {
        "Not set"
      },
      LAST_MODIFIED   = if (exists("LAST_MODIFIED")) LAST_MODIFIED else "Not set",
      DEPENDS_ON      = if (exists("DEPENDS_ON")) DEPENDS_ON else "Not set",
      EXCLUSIONS      = if (exists("EXCLUSIONS")) EXCLUSIONS else "Not set",
      EXPLICIT_PATHS  = if (exists("EXPLICIT_PATHS")) EXPLICIT_PATHS else "Not set",
      START_UP        = {
        get_settings <- c("USE_API", "USE_RDKIT", "USE_SHINY", "INIT_CONNECT",
                          "INFORMATICS", "MINIMIZE", "VERIFY_ARGUMENTS",
                          "LOGGING_ON")
        setNames(
          lapply(get_settings,
               function(x) {
                 if (exists(x)) {
                   eval(rlang::sym(x))
                 } else {
                   "Not set"
                 }
               }
          ),
          get_settings
        )},
      PLUMBER         = {
        get_settings <- c("USE_API", "PLUMBER_FILE", "PLUMBER_HOST",
                          "PLUMBER_PORT", "PLUMBER_URL")
        setNames(
          lapply(get_settings,
                 function(x) {
                   if (exists(x)) {
                     tmp <- eval(rlang::sym(x))
                     if (x == "PLUMBER_FILE") {
                       fpath <- list.files(path = here::here(),
                                           pattern = basename(tmp),
                                           full.names = TRUE,
                                           recursive = TRUE)
                       list(file = tmp,
                            exists = !length(fpath) == 0,
                            location = fpath)
                     } else {
                       tmp
                     }
                   } else {
                     "Not set"
                   }
                 }
          ),
          get_settings
        )},
      LOGGER_SETTINGS = c(global[grepl("^LOG[G_]", names(global))],
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
  out$packs_installed
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
#' was in part inspired by the excellent [testthat](https://testthat.r-lib.org/)
#' package and shares concepts with the [Checkmate](https://mllg.github.io/checkmate/)
#' package. However, this function performs many of the common checks without
#' additional package dependencies, and can be inserted into other functions 
#' for a project easily with:
#' \preformatted{  arg_check <- verify_args(args = as.list(environment()),
#'   conditions = list(param1 = c("mode", "logical"), param2 = c("length", 1))}
#' and check the return with
#' \preformatted{  if (!arg_check$valid) cat(paste0(arg_check$messages, "\n"))}
#' where argument \code{conditions} describes the tests. This comes at the price
#' of readability as the list items in \code{conditions} do not have to be
#' named, but can be to improve clarity. See more details below for argument
#' \code{conditions} to view which expectations are currently supported.
#' As this is a nested list condition check, it can also originate from any
#' source coercible to a list (e.g. JSON, XML, etc.) and this feature, along
#' with the return of human-meaningful evaluation strings, is particularly
#' useful for development of shiny applications. Values from other sources MUST
#' be coercible to a full list (e.g. if being parsed from JSON, use
#' \code{jsonlite::fromJSON(simplifyMatrix = FALSE)})
#'
#' @note If logger is enabled, also provides some additional meaningful feedback.
#'
#' @note At least one condition check is required for every element passed to \code{args}.
#'
#' @param args LIST of named arguments and their values, typically passed
#'   directly from a function definition in the form \code{args = list(foo =
#'   1:2, bar = c("a", "b", "c"))} or directly by passing \code{environment()}
#'
#' @param conditions Nested LIST of conditions and values to check, with one
#'   list item for each element in \code{args}. \itemize{\item{The first
#'   element of each list should be a character scalar in the supported list.} 
#'   \item{The second element of each list should be the check values themselves
#'   and may be of any type.}} Multiple expectation conditions can be set for
#'   each element of \code{args} in the form \itemize{\item{\code{conditions =
#'   list(foo = list(c("mode", "numeric"), c("length", 2)), bar = list(c("mode",
#'   "character"), c("n<", 5)))}}} Currently supported expectations are:
#'   \describe{\itemize{\item{\code{class}: {checks strict class expectation by direct
#'   comparison with \code{class} to support object classes not supported with
#'   the \code{is.x} or \code{is_x} family of functions; much stricter than a
#'   "mode" check in that the requested check must be present in the return from
#'   a call to \code{class} e.g. "list" will fail if a "data.frame" object is
#'   passed}} \item{\code{mode}: {checks class expectation by applying the
#'   \code{is.X} or the \code{is_X} family of functions either directly or
#'   flexibly depending on the value provided to \code{conditions} (e.g.
#'   \code{c("mode", "character")} and \code{c("mode", "is.character")} and \code{c("mode",
#'   "is_character")} all work equally well) and will default to the version you
#'   provide explicitly (e.g. if you wish to prioritize "is_character" over
#'   "is.character" simply provide "is_character" as the condition. Only those
#'   modes able to be checked by this family of functions are supported. Run
#'   function \code{mode_checks()} for a complete sorted list for your current
#'   configuration.}} \item{\code{length}: {length of values matches a pre-determined
#'   exact length, typically a single value expectation (e.g. \code{c("length",#'
#'   1)})}} \item{\code{no_na}: {no \code{NA} values are present}} \item{\code{n>}: {length of
#'   values is greater than a given value - "n<" length of values is lesser than
#'   a given value}} \item{\code{n>=}: {length of values is greater than or equal to
#'   a given value}} \item{\code{n<=}: {length of values is lesser than or equal to a
#'   given value}} \item{\code{>}: {numeric or date value is greater than a given
#'   value}} \item{\code{<}: {numeric or date value is greater than a given value}}
#'   \item{\code{>=}: {numeric or date value is greater than or equal to a given
#'   value}} \item{\code{<=}: {numeric or date value is lesser than or equal to a
#'   given value}} \item{\code{between}: {numeric or date values are bound within an
#'   INCLUSIVE range (e.g. \code{c("range", 1:5)})}} \item{\code{choices}: {provided values
#'   are part of a selected list of expectations (e.g. \code{c("choices",
#'   list(letters[1:3]))})}} \item{\code{FUN}: {apply a function to the value and
#'   check that the result is valid or that the function can be executed without
#'   error; this evaluates the check condition using [tryCatch()] via
#'   [do.call()] and so can also accept a full named list of arg values. This
#'   is a strict check in the sense that a warning will also result in a failed
#'   result, passing the warning (or error if the function fails) message back
#'   to the user, but does not halt checks}}}}
#' @param from_fn CHR scalar of the function from which this is called, used if
#'   logger is enabled and ignored if not; by default it will pull the calling
#'   function's name from the call stack, but can be overwritten by a manual
#'   entry here for better tracing. (default \code{NULL})
#' @param silent LGL scalar of whether to silence warnings for individual
#'   failiures, leaving them only as part of the output. (default: \code{FALSE})
#'
#' @return LIST of the resulting values and checks, primarily useful for its
#'   \code{$valid} (\code{TRUE} if all checks pass or \code{FALSE} if any fail)
#'   and \code{$message} values.
#' @export
#'
#' @usage
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
#'                                          c("choices", list("a", "b"))))
#' )
#' 
verify_args <- function(args, conditions, from_fn = NULL, silent = FALSE) {
  log_fn("start")
  if (exists("VERIFY_ARGUMENTS")) {
    if (!VERIFY_ARGUMENTS) {
      if (exists("reported_verification_warning")) {
        reported_verification_warning <<- TRUE
      } else {
        reported_verification_warning <<- FALSE
      }
      if (exists("log_it") && !reported_verification_warning) log_it("warn", "Argument verification is currently off. This warning will only appear once.")
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
  if (!is.null(from_fn) && exists("log_it")) log_it("trace", glue::glue('Verifying arguments for "{from_fn}".'))
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
      names(arg) <- glue::glue("argument_{i}")
    }
    needs <- conditions[[i]]
    if (exists("log_it")) log_it("trace", glue::glue('Verify provided value of "{paste0(args[i], collapse = \'", "\')}"'))
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
        msg  <- glue::glue('Parameter "{names(arg)}" is required.')
      } else {
        switch(type,
               "class"   = {
                 rslt <- all(check %in% class(val))
                 class_real <- class(val)
                 class_need <- check[!check %in% class_real]
                 class_real <- glue::glue('{ifelse(length(class_real) == 1, "", "es")} {format_list_of_names(class_real, add_quotes = TRUE)}')
                 class_need <- glue::glue('{ifelse(length(class_need) == 1, "", "es")} {format_list_of_names(class_need, add_quotes = TRUE)}')
                 msg  <- glue::glue('Parameter "{names(arg)}" of class{class_real} was missing required class{class_need}.')
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
                   msg  <- glue::glue("Argument '{names(arg)}' (checked with '{mode_check}') must be of class '{check}' rather than '{class(val)}'.")
                 } else {
                   rslt <- FALSE
                   msg  <- glue::glue("Unable to find function '{mode_check}' check argument '{names(arg)}' with '{mode_check}' as it ")
                 }
               },
               "length"  = {
                 rslt <- length(val) == as.integer(check)
                 msg  <- glue::glue("Argument '{names(arg)}' must be of length {check} rather than {length(val)}.")
               },
               "no_na"   = {
                 rslt <- !any(is.na(val))
                 msg  <- glue::glue("Argument '{names(arg)}' includes NA values.")
               },
               "n>"      = {
                 rslt <- length(val) > as.integer(check)
                 msg  <- glue::glue("Argument '{names(arg)}' must be of length greater than {check} rather than {length(val)}.")
               },
               "n<"      = {
                 rslt <- length(val) < as.integer(check)
                 msg  <- glue::glue("Argument '{names(arg)}' must be of length lesser than {check} rather than {length(val)}.")
               },
               "n>="     = {
                 rslt <- length(val) >= as.integer(check)
                 msg  <- glue::glue("Argument '{names(arg)}' must be of length greater than or equal to {check} rather than {length(val)}.")
               },
               "n<="     = {
                 rslt <- length(val) <= as.integer(check)
                 msg  <- glue::glue("Argument '{names(arg)}' must be of length lesser than or equal to {check} rather than {length(val)}.")
               },
               ">"       = {
                 if (length(check) == 1) {
                   rslt <- all(val > check)
                   msg  <- glue::glue("Argument '{names(arg)}' must be greater than {check}.")
                 } else {
                   rslt <- FALSE
                   msg  <- glue::glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
                 }
               },
               "<"       = {
                 if (length(check) == 1) {
                   rslt <- all(val < check)
                   msg  <- glue::glue("Argument '{names(arg)}' must be lesser than {check}.")
                 } else {
                   rslt <- FALSE
                   msg  <- glue::glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
                 }
               },
               ">="       = {
                 if (length(check) == 1) {
                   rslt <- all(val >= check)
                   msg  <- glue::glue("Argument '{names(arg)}' must be greater than or equal to {check}.")
                 } else {
                   rslt <- FALSE
                   msg  <- glue::glue("Expected an exclusive upper bound, but {length(check)} value(s) were provided: {check}.")
                 }
               },
               "<="       = {
                 if (length(check) == 1) {
                   rslt <- all(val <= check)
                   msg  <- glue::glue("Argument '{names(arg)}' must be lesser than or equal to {check}.")
                 } else {
                   rslt <- FALSE
                   msg  <- glue::glue("Expected an exclusive upper bound, but {length(check)} value(s) provided: {check}.")
                 }
               },
               "between" = {
                 if (length(check) == 2) {
                   rslt <- all(val >= check[1] && val <= check[2])
                   if (is.numeric(val)) {
                     msg  <- glue::glue("Argument '{names(arg)}' must be between {check[1]} and {check[2]} but the range was {paste0(range(val), collapse = ' - ')}.")
                   } else {
                     msg  <- glue::glue("Argument '{names(arg)}' must be a numeric or date vector between {check[1]} and {check[2]} but was provided as {class(arg)}.")
                   }
                 } else {
                   if (length(check) < 2) {
                     len_check <- "less than"
                   } else {
                     len_check <- "more than"
                   }
                   rslt <- FALSE
                   msg  <- glue::glue("Expected an inclusive bounding range, but {len_check} 2 values were provided: {check}.")
                 }
               },
               "choices" = {
                 rslt <- all(val %in% check)
                 msg  <- glue::glue('Argument "{names(arg)}" was not present in choice list c("{paste0(check, collapse = \'", "\')}").')
               },
               "call"    = {
                 rslt <- tryCatch(do.call(check, list(val)),
                                  error   = function(e) e,
                                  warning = function(w) w)
                 val  <- paste0('"', val, '"', collapse = ", ")
                 if ("simpleError" %in% class(rslt)) {
                   msg  <- glue::glue("Error evaluating 'do.call({check}, list({val}))': \"{rslt$message}\"")
                   rslt <- FALSE
                 } else if ("simpleWarning" %in% class(rslt)) {
                   msg  <- glue::glue("Warning on 'do.call({check}, list({val}))': \"{rslt$message}\"")
                   rslt <- FALSE
                 } else {
                   rslt <- TRUE
                 }
               },
               "not_empty" = {
                 msg  <- glue::glue("Object contained nothing but NAs, NULLs, or empty character strings.")
                 rslt <- !has_missing_elements(val)
               },
               "file_exists" = {
                 msg  <- glue::glue("Could not locate file '{val}'.")
                 rslt <- file.exists(val)
               },
               {
                 rslt <- FALSE
                 msg  <- glue::glue("Could not match condition type '{type}' for argument '{names(arg)}'. Ensure the check is one of: {supported}")
               }
        )
      }
      log_msg <- glue::glue('  Check type is "{type}" against: "{format_list_of_names(check)}"')
      out$results[[i]][j]   <- rslt
      if (!rslt) {
        out$valid           <- FALSE
        n_msg               <- length(out$messages) + 1
        out$messages[n_msg] <- msg
        log_it("trace", glue::glue('{log_msg} - FAIL'))
        if (!silent) log_it("error", glue::glue("    {msg}"))
      } else {
        if (exists("log_it")) log_it("trace", glue::glue('{log_msg} - PASS'))
      }
    }
  }
  if (out$valid) {
    if (!is.null(from_fn) && exists("log_it")) log_it("trace", glue::glue("Arguments verified for '{from_fn}'"))
  } else {
    if (exists("log_it")) log_it("error", glue::glue("Arguments could not be verified",
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
#' "one, two, ..., and three" using \code{glue::glue}. This is functionally the same
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
  if (length(namelist) == 0) {
    return("")
  } else if (length(namelist) == 1) {
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
#' \code{config/env_logger.R} as a list (see examples there) and make sure it is
#' sourced. If using with [logger] and "file" or "both" is selected for the
#' namespace \code{LOGGING[[log_ns]]$to} parameter in \code{env_logger.R} logs will be
#' written to disk at the file defined in \code{LOGGING[[log_ns]]$file} as well as
#' the console.
#'
#' @param log_level CHR scalar of the level at which to log a given statement.
#'   If using the [logger] package, must match one of [logger:::log_levels]
#' @param msg CHR scalar of the message to accompany the log.
#' @param log_ns CHR scalar of the logging namespace to use during execution
#'   (default: NULL prints to the global logging namespace)
#' @param reset_logger_settings LGL scalar indicating whether or not to refresh
#'   the logger settings using the file identified in \code{logger_settings}
#'   (default: FALSE)
#' @param reload_all LGL scalar indicating whether to, during
#'   \code{reset_logger_settings}, to reload the R environment configuration file
#' @param logger_settings CHR file path to the file containing logger settings
#'   (default: file.path("config", "env_logger.R"))
#' @param add_unknown_ns LGL scalar indicating whether or not to add a new
#'   namespace if \code{log_ns} is not defined in \code{logger_settings} (default: FALSE)
#' @param clone_settings_from CHR scalar indicating
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
log_it <- function(log_level,
                   msg = NULL,
                   log_ns = NULL,
                   reset_logger_settings = FALSE,
                   reload_all = FALSE,
                   logger_settings = file.path("config", "env_logger.R"),
                   add_unknown_ns = FALSE,
                   clone_settings_from = NULL) {
  stopifnot(
    length(msg) < 2,
    is.logical(reset_logger_settings) && length(reset_logger_settings) == 1,
    !reset_logger_settings || (reset_logger_settings && length(logger_settings) == 1 && file.exists(logger_settings)),
    is.null(clone_settings_from) || (is.character(clone_settings_from) && length(clone_settings_from) == 1 && exists("LOGGING")),
    is.logical(add_unknown_ns) && length(add_unknown_ns) == 1
  )
  if (has_missing_elements(msg, logging = FALSE)) msg <- "[no message provided]"
  if (!exists("LOGGING_ON")) {
    if (reset_logger_settings) {
      if (file.exists(logger_settings)) {
        reset_logger_settings(reload = reload_all)
      } else {
        stop(sprintf('Logging settings not configured. File "%s" not available.', logger_settings))
      }
    }
    LOGGING_ON <- TRUE
  }
  call_i <- -1
  call_ii <- 1
  call_func <- sys.call(call_i)[[call_ii]]
  uninformative_calls <- c("do.call", "FUN", "apply$", "%>%") %>%
    paste0(collapse = "|")
  while(is.function(call_func) || grepl(uninformative_calls, deparse(substitute(call_func)))) {
    call_i <- call_i - 1
    call_func <- sys.call(call_i)[[call_ii]]
  }
  if (sys.nframe() > 1) {
    from_log_it <- call_func == "log_it"
  } else {
    from_log_it <- TRUE
    call_func <- rlang::sym("log_it")
  }
  do_log <- TRUE
  if (LOGGING_ON) {
    if (is.na(log_ns) || is.null(log_ns)) {
      log_ns <- NA_character_
    } else {
      if (!exists("LOGGING")) {
        log_it("warn", sprintf('Logging is not set up for namespace "%s".', log_ns))
        if (add_unknown_ns) {
          log_it("warn", "Setting up a default namespace for interactive logging only.")
          assign(x = "LOGGING", value = setNames(list(list(log = TRUE, log_ns = log_ns, threshold = "trace")), toupper(log_ns)), envir = .GlobalEnv)
        } else {
          log_it("info", sprintf('Call log_it() again with "add_unknown_ns = TRUE" to establish the "%s" namespace.', log_ns))
        }
      } else {
        logging_set <- toupper(log_ns) %in% names(LOGGING)
        if (logging_set) {
          do_log <- LOGGING[[toupper(log_ns)]]$log
        } else {
          log_it("warn", sprintf('Logging namespace "%s" is not set up.', log_ns))
          clone_exists <- !is.null(clone_settings_from) && toupper(clone_settings_from) %in% names(LOGGING)
          if (clone_exists) {
            i <- grep(toupper(clone_settings_from), names(LOGGING))
          } else {
            i <- 1
          }
          requested_clone <- clone_settings_from
          clone_settings_from <- names(LOGGING)[i]
          settings_from <- LOGGING[[i]]$ns
          if (add_unknown_ns) {
            log_it("info", sprintf('Setting up namespace "%s"', log_ns))
            if (clone_exists) {
              log_it("info", sprintf('Copying settings for "%s" from "%s". Access settings at LOGGING$%s', log_ns, settings_from, toupper(log_ns)))
            } else {
              msg <- ifelse(
                is.null(requested_clone),
                sprintf('No clone namespace provided. Settings for "%s" will be used as a default.', settings_from),
                sprintf('Requestedlone namespace "%s" is not set up. Settings for "%s" will be used instead.', requested_clone, settings_from)
              )
              log_it("warn", msg)
            }
            LOGGING[[toupper(log_ns)]] <- LOGGING[[i]]
            LOGGING[[toupper(log_ns)]]$ns <- tolower(log_ns)
            LOGGING[[toupper(log_ns)]]$file <- file.path(
              dirname(LOGGING[[toupper(log_ns)]]$file),
              sprintf("log_%s.txt", tolower(log_ns))
            )
            assign("LOGGING", LOGGING, envir = .GlobalEnv)
            update_logger_settings(log_all_warnings = FALSE, log_all_errors = FALSE)
            log_it(
              "warn",
              sprintf(
                'Logger settings updated for this session, but will reset to those in "%s" if settings are refreshed (e.g. by calling "update_logger_settings()" or by calls to "log_it(..., reset_logger_settings = TRUE)"',
                logger_settings),
              log_ns)
          } else {
            log_it("info", sprintf('Call again with "add_unknown_ns = TRUE" to establish the "%s" namespace with the same settings as "%s".', log_ns, settings_from))
          }
          do_log <- TRUE
        }
      }
    }
    if (do_log) {
      log_func  <- sprintf("log_%s", tolower(log_level))
      log_level <- toupper(log_level)
      n_call    <- ifelse(sys.nframe() > 1, call_i, 1)
      if (exists(log_func)) {
        log_level(level    = log_level,
                  namespace = log_ns,
                  .topcall = sys.call(n_call),
                  msg)
      } else {
        # # See below comment about package "cli"...if that route is desired, it is necessary to uncomment this block.
        #
        # log_level <- switch(log_level,
        #                     "WARN" = "WARNING",
        #                     "ERROR" = "DANGER",
        #                     log_level)
        msg <- sprintf("[%s] <%s> %s in fn %s(): %s\n",
                       format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                       ifelse(is.na(log_ns), "global", log_ns),
                       log_level,
                       deparse(call_func),
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
      if (!LOGGING[[toupper(log_ns)]]$log) {
        msg <- sprintf('Logging is currently turned off for namespace "%s". Set LOGGING$%s$log to TRUE to begin logging.\n',
                       log_ns,
                       toupper(log_ns))
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
#' upper case from each word in \code{text} elements.
#'
#' @param text CHR vector of the text to acronym-ize
#'
#' @return CHR vector of length equal to that of \code{text} with the acronym
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
#' @param logging LGL scalar of whether or not to make log messages (default: TRUE)
#'
#' @return LGL scalar of whether \code{x} is empty
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
has_missing_elements <- function(x, logging = TRUE) {
  if (logging) log_fn("start")
  if (is.function(x)) {
    out <- FALSE
    if (logging) log_it("trace", sprintf("%s is a function, not a variable.", deparse(substitute(x))))
  } else if (is.data.frame(x)) {
    if (logging) log_it("trace", sprintf("%s is a data frame; result checks whether it has 0 rows.", deparse(substitute(x))))
    out <- nrow(x) == 0
  } else if (is.list(x)) {
    if (logging) log_it("trace", sprintf("%s is a list; result checks whether any element of that list is empty.", deparse(substitute(x))))
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
  if (logging) log_fn("end")
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
#' @usage
#' fn <- function() {log_fn("start"); 1+1; log_fn("end")}
#' fn()
log_fn <- function(status = "start", log_ns = NA_character_, level = "trace") {
  i <- -1
  while (is.function(sys.call(i)[[1]])) i <- i - 1
  if (abs(i) > length(sys.calls())) stop("Cannot locate a named function call in the call stack.")
  this_call <- as.character(sys.call(i)[[1]])
  if (this_call == "do.call") this_call <- as.character(sys.call(i)[[2]])
  require(stringr)
  msg <- sprintf("%s %s()",
                 str_to_sentence(status),
                 this_call
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
#' @return None, executes directory actions
#' @export
#'
#' @usage flush_dir("logs", ".txt")
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
#' default value. This returns values in precedence of \code{parameter},
#' \code{env_parameter} and \code{default}.
#'
#' @note \code{log_ns} is only applicable if logging is set up in this project (see
#'   project settings in env_glob.txt, env_R.R, and env_logger.R for details).
#'
#' @note Both [base::.GlobalEnv] and [base::Sys.getenv] are checked, and can be
#'   provided as a character scalar or as an object reference
#'
#' @param parameter the object being evaluated
#' @param env_parameter the name or object of a value to use from the
#'   environment if \code{parameter} is NULL
#' @param default the fallback value to use if \code{parameter} is NULL and
#'   \code{env_parameter} does not exist
#' @param log_ns the namespace to use with [log_it] if available
#'
#' @return The requested value, either as-is, rectified from the environment, or 
#'   the default
#' @export
#'
#' @usage rectify_null_from_env(test, test, "test")
rectify_null_from_env <- function(parameter = NULL, env_parameter, default, log_ns = NA_character_) {
  logger <- exists("LOGGING_ON") && LOGGING_ON && exists("log_it")
  if (logger) log_fn("start", log_ns)
  par_name <- deparse(substitute(parameter))
  orig_length <- nchar(par_name)
  par_name <- gsub('\\"', '', par_name)
  new_length <- nchar(par_name)
  param_chr <-  orig_length != new_length
  par_ref  <- ""
  suffix <- ""
  if (par_name %in% c("", "NULL", "NA")) {
    out <- NULL
  } else {
    if (exists(par_name)) {
      if (param_chr) {
        out <- eval(rlang::sym(par_name))
      } else {
        out <- parameter
      }
      par_ref  <- " environment"
    } else {
      out <- try(parameter, silent = TRUE)
      if (inherits(out, "try-error") || is.null(parameter)) {
        out <- NULL
        suffix <- sprintf(" as '%s' is either not set or NULL", par_name)
      } else {
        if (length(parameter) == 1) {
          if (par_name == parameter) {
            par_ref <- ""
          } else {
            par_ref <- " calling"
          }
          suffix <- " as provided"
          out <- parameter
        } else {
          suffix <- " as provided"
          out <- parameter
        }
      }
    }
  }
  if (is.null(out)) {
    env_par_name <- deparse(substitute(env_parameter))
    par_name <- env_par_name
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
    feedback <- sprintf("%s", par_value_str)
  } else if (par_ref == " default") {
    feedback <- sprintf("%s", par_value_str)
  } else {
    feedback <- sprintf("'%s = %s'", par_name, par_value_str)
  }
  if (logger) log_it("trace", sprintf("Returning%s parameter %s%s.", par_ref, feedback, suffix), log_ns)
  if (logger) log_fn("end", log_ns)
  return(out)
}

#' Update logger settings
#'
#' This is a simple action wrapper to update any settings that may have been
#' changed with regard to logger. If, for instance, something is not logging the
#' way you expect it to, change the relevant setting and then run
#' \code{update_logger_settings()} to reflect the current environment.
#'
#' @param reload LGL scalar indicating (if TRUE) whether or not to refresh from
#'   \code{env_R.R} or (if FALSE) to use the current environment settings (e.g. for
#'   testing purposes) (default: FALSE)
#'
#' @return None
#' @export
#'
#' @usage reset_logger_settings() 
reset_logger_settings <- function(reload = FALSE) {
  if (reload) source(file.path("config", "env_R.R"))
  if (exists("update_logger_settings")) {
    update_logger_settings()
  } else {
    fpath <- file.path("config", "env_logger.R")
    if (file.exists(fpath)) {
      source(fpath)
    } else {
      stop(sprintf("Could not find file '%s'", fpath))
    }
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
#' @return None, removes files from a directory
#' @export
#'
#' @usage flush_dir(directory = "logs")
flush_dir <- function(archive = FALSE, directory, pattern) {
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
               sprintf("Files in '%s' were successfully %s",
                       directory,
                       ifelse(archive, sprintf("archived to '%s'", archive_dir), "removed")
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

#' Sanity check for environment object names
#'
#' Provides a sanity check on whether or not a name reference exists and return
#' its name if so. If not, return the default name defined from \code{default_name}.
#' This largely is used to prevent naming conflicts as part of managing the
#' plumber service but can be used for any item in the current namespace.
#'
#' @param obj R object or CHR scalar in question to be resolved in the namespace
#' @param default_name CHR scalar name to use for \code{obj} if it does not exist
#'   (default: NULL).
#'
#' @return CHR scalar of the resolved object name
#'
#' @export
#' @usage
#'   if (exists("log_it")) {
#'     obj_name_check("test", "test")
#'     test <- letters
#'     obj_name_check(test)
#'   }
#'
obj_name_check <- function(obj, default_name = NULL) {
  require(stringr)
  if (exists("log_it")) log_it("debug", "Run obj_name_check().", "api")
  if (is.null(obj) || is.na(obj)) {
    if (is.null(default_name) || is.na(default_name)) {
      return(NULL)
    } else {
      obj_name <- default_name
    }
  } else {
    obj_name <- stringr::str_remove_all(deparse(substitute(obj)), '"')
  }
  if (exists(obj_name)) {
    # Placeholder to do maybe do something like preservation/backup of current
    # object or suggest a new name
    
    # pr <- eval(rlang::sym(obj_name))
  } else {
    if (exists("log_it")) {
      log_it("warn", glue::glue('No object named "{obj_name}" exists. Defaulting to "{default_name}".'))
    }
    if (is.null(default_name)) {
      if (exists("log_it")) log_it("warn", "No default name provided. Name given back as-is.")
    } else {
      obj_name <- default_name
    }
  }
  if (exists("log_it")) log_it("debug", "Exiting obj_name_check().")
  return(obj_name)
}

#' Start the RDKit integration
#'
#' If the session was started without RDKit integration, e.g. INFORMATICS or
#' USE_RDKIT were FALSE in [config/env_R.R], start up RDKit in this session.
#'
#' @note RDKit and rcdk are incompatible. If the session was started with
#'   INFORMATICS = TRUE and USE_RDKIT = FALSE, ChemmineR was likely loaded. If
#'   this is the case, the session will need to be restarted due to java
#'   conflicts between the two.
#'
#' @param src_dir CHR scalar file path to settings and functions enabling rdkit
#'   (default: here::here("inst", "rdkit"))
#' @param log_ns CHR scalar name of the logging namespace to use for this
#'   function (default: "rdkit")
#'
#' @return LGL scalar indicating whether starting RDKit integration was
#'   successful
#' @export
#'
start_rdkit <- function(src_dir = here::here("inst", "rdkit"), log_ns = "rdkit") {
  # TODO for publication, src_dir should direct to grep(file.path("[package_name]", "inst", "rdkit"), list.dirs(c(.libPaths(), here::here()), full.names = TRUE), value = TRUE)
  if (any(c("BiocManager", "rcdk", "ChemmineR") %in% loadedNamespaces())) {
    stop("RDKit cannot be loaded if Bioconductor is already running.")
  }
  adding <- character(0)
  if (!exists("INFORMATICS") || !INFORMATICS) {
    adding <- c("INFORMATICS", "USE_RDKIT")
    assign("INFORMATICS", TRUE, envir = .GlobalEnv)
    assign("USE_RDKIT", TRUE, envir = .GlobalEnv)
  } else if (INFORMATICS && !USE_RDKIT) {
    adding <- "USE_RDKIT"
    assign("USE_RDKIT", TRUE, envir = .GlobalEnv)
  }
  if (length(adding) > 0) {
    log_it("trace", glue::glue("Setting {format_list_of_names(adding)} to TRUE."), log_ns)
  }
  sapply(list.files(src_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
         source,
         keep.source = FALSE)
  rdkit_active(make_if_not = TRUE)
}

#' Start the plumber interface from a clean environment
#'
#' This convenience function launches the plumber instance if it was not set to
#' launch during the session setup. It is a thin wrapper with a more intuitive
#' name than [api_reload] and the default background setting turned off to test
#' the server in the current session.
#'
#' @note This function is intended to pull from the environment variables
#'   identifying the plumber file, host, and port.
#'
#' @param plumber_file CHR scalar name of the plumber definition file, which
#'   should be in \code{src_dir} (default: NULL)
#' @param plumber_host CHR scalar of the host server address (default: NULL)
#' @param plumber_port INT scalar of the listening port on the host server
#'   (default: NULL)
#' @param background LGL scalar of whether to launch the API in a background
#'   process (default: FALSE)
#' @param src_dir  CHR scalar file path to settings and functions enabling the
#'   plumber API (default: here::here("inst", "plumber"))
#' @param log_ns CHR scalar name of the logging namespace to use for this
#'   function (default: "api")
#'
#' @return None, launches the plumber instance
#' @export
#'
#' @usage start_api()
start_api <- function(plumber_file = NULL, plumber_host = NULL, plumber_port = NULL, background = FALSE, src_dir = here::here("inst", "plumber"), log_ns = "api") {
  # TODO for publication, src_dir should direct to grep(file.path("[package_name]", "inst", "plumber"), list.dirs(c(.libPaths(), here::here()), full.names = TRUE), value = TRUE)
  if (!exists("api_reload")) {
    source(file.path(src_dir, "api_control.R"))
    reminder <- TRUE
  } else {
    reminder <- FALSE
  }
  plumber_file <- rectify_null_from_env(plumber_file, PLUMBER_FILE, file.path(src_dir, "plumber.R"))
  plumber_host <- rectify_null_from_env(plumber_host, PLUMBER_HOST, getOption("plumber.host", "127.0.0.1"))
  plumber_port <- rectify_null_from_env(plumber_port, PLUMBER_PORT, getOption("plumber.port", 8080))
  running_on   <- api_reload(plumber_file = plumber_file, on_host = plumber_host, on_port = plumber_port, background = background)
  if (reminder) {
    if (!exists("LOGGING_ON") || !LOGGING_ON) message("Logging will be turned on in the background instance.")
    message("Remember to kill the plumber instance (e.g. plumber_service$kill() or api_stop()) when you are finished with it.")
  }
  return(running_on)
}

#' {WIP} Launch a shiny application
#'
#' Call this function to launch an app either directly or in a background
#' process. The name must be present in the app directory or as a named
#' element of \code{SHINY_APPS} in the current environment.
#'
#' @note Background launching of shiny apps is not yet supported.
#'
#' @param app_name CHR scalar name of the shiny app to run, this should be the
#'   name of a directory containing a shiny app that is located within the
#'   directory defined by \code{app_dir} or the name of an app as defined in your
#'   environment SHINY_APPS variable
#' @param app_dir file path to a directory containing shiny apps (default:
#'   here::here("inst", "apps"))
#' @param background LGL scalar of whether to launch the application in a
#'   background process (default: FALSE)
#' @param ... Other named parameters to be passed to [shiny::runApp]
#'
#' @return None, launches a browser with the requested shiny application
#' @export
#'
#' @usage start_app("table_explorer")
start_app <- function(app_name, app_dir = here::here("inst", "apps"), background = FALSE, ...) {
  # TODO make these launchable in the background to keep the session free
  # TODO for publication, src_dir should direct to grep(file.path("[package_name]", "inst", "plumber"), list.dirs(c(.libPaths(), here::here()), full.names = TRUE), value = TRUE)
  if (exists("SHINY_APPS")) {
    app_dir <- SHINY_APPS[[app_name]]
  } else {
    app_dir <- file.path(app_dir, app_name)
  }
  kwargs <- c(
    list(...),
    appDir = app_dir
  )
  if (!is.null(kwargs)) {
    kwargs <- kwargs[which(names(kwargs) %in% names(formals(shiny::runApp)))]
  }
  if (!dir.exists(app_dir)) stop(sprintf("Could not locate an app named %s at %s", app_name, app_dir))
  app_name <- sprintf("app_%s_service", basename(app_dir))
  if (background) {
    message("Background launching of shiny apps is not yet supported.")
    background = FALSE
  }
  if (background) {
    # assign(
    #   x = app_name,
    #   value = callr::r_bg(
    #     args = kwargs,
    #     func = {
    #       if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
    #       do.call(what = shiny::runApp, args = kwargs)
    #     }
    #   ),
    #   envir = .GlobalEnv
    # )
    # message(sprintf("Don't forget to close the application process (e.g. %s$kill()) when you finish with it.", app_name))
  } else {
    do.call(what = shiny::runApp, args = kwargs)
  }
}

#' Append additional named elements to a list
#'
#' This does nothing more than [base::append] ellipsis arguments to be added
#' directly to the end of an existing list object. This primarily supports
#' additional property assignment during the import process for future
#' development and refinement. Call this as part of any function with additional
#' arguments. This may result in failures or ignoring unrecognized named
#' parameters. If no additional arguments are passed \code{obj} is returned as
#' provided.
#'
#' @note If duplicate names exists in \code{obj} and those provided as ellipsis
#'   arguments, those provided as part of the ellipsis will replace those in
#'   \code{obj}.
#'
#' @param obj LIST of any length to be appended to
#' @param ... Additional arguments passed to/from the ellipsis parameter of
#'   calling functions. If named, names are preserved.
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return LIST object of length equal to \code{obj} plus additional named arguments
#' @export
#'
#' @examples
#' tack_on(list(a = 1:3), b = letters, c = rnorm(10))
#' tack_on(list(a = 1:3))
tack_on <- function(obj, ..., log_ns = "db") {
  logging <- exists("LOGGING_ON") && LOGGING_ON && exists("log_it")
  addl_args <- list(...)
  if (any(names(addl_args) %in% names(obj))) {
    applies_to <- names(addl_args)[names(addl_args) %in% names(obj)]
    for(i in applies_to) {
      obj[[i]] <- NULL
    }
  }
  if (logging) log_it("info", glue::glue("Tacking on {length(addl_args)} additional item{ifelse(length(addl_args) > 1, 's', '')} ({format_list_of_names(names(addl_args), add_quotes = TRUE)})."), log_ns)
  out <- append(obj, addl_args)
  return(out)
}

#' Resolve components from a list or named vector
#'
#' Call this to pull a component named \code{obj_component} from a list or named
#' vector provided as \code{obj} and optionally use [tack_on] to append to it. This
#' is intended to ease the process of pulling specific components from a list
#' for further treatment in the import process by isolating that component.
#'
#' This is similar in scope to [purrr::pluck] in many regards, but always
#' returns items with names, and will search an entire list structure, including
#' data frames, to return all values associated with that name in individual
#' elements.
#' 
#' @note This is a recursive function.
#'
#' @note If ellipsis arguments are provided, they will be appended to each
#'   identified component via [tack_on]. Use with caution, but this can be
#'   useful for appending common data to an entire list (e.g. a datetime stamp
#'   for logging processing time or a processor name, human or software).
#'
#' @inheritParams tack_on
#'
#' @param obj LIST or NAMED vector in which to find \code{obj_component}
#' @param obj_component CHR vector of named elements to find in \code{obj}
#' @param silence LGL scalar indicating whether to silence recursive messages,
#'   which may be the same for each element of \code{obj} (default: TRUE)
#' @param log_ns CHR scalar of the logging namespace to use (default: "db")
#'
#' @return LIST object containing the elements of \code{obj}
#' @export
#' 
#' @examples
#' get_component(list(a = letters, b = 1:10), "a")
#' get_component(list(ex = list(a = letters, b = 1:10), ex2 = list(c = 1:5, a = LETTERS)), "a")
#' get_component(list(a = letters, b = 1:10), "a", c = 1:5)
#' 
get_component <- function(obj, obj_component, silence = TRUE, log_ns = "global", ...) {
  stopifnot(is.character(obj_component), length(obj_component) > 0, is.character(log_ns), length(log_ns) == 1)
  logging <- exists("LOGGING_ON") && LOGGING_ON && exists("log_it")
  names_present <- obj_component %in% names(obj)
  if (any(names_present)) {
    if (!all(names_present)) {
      msg <- glue::glue("Requested component{ifelse(sum(!names_present) > 1, 's', '')} {format_list_of_names(obj_component[!names_present], add_quotes = TRUE)} {ifelse(sum(!names_present) > 1, 'were', 'was')} missing.")
      if (logging) {
        log_it("warn", msg, log_ns)
      } else {
        warning(msg)
      }
      return(NULL)
    }
    out <- obj[obj_component[names_present]]
  } else if (is.list(obj)) {
    out <- lapply(obj,
                  function(x) {
                    tmp <- get_component(
                      obj = x,
                      obj_component = obj_component,
                      silence = silence
                    )
                  }) %>%
      purrr::keep(~ length(.x) > 0)
  } else {
    if (logging && !silence) log_it("warn", glue::glue('"No components named {gsub(" and ", " or ", format_list_of_names(obj_component, add_quotes = TRUE))}" found in the namespace of this object..'), log_ns)
    return(NULL)
  }
  kwargs <- list(...)
  if (length(kwargs) > 0) {
    out <- tack_on(obj = out, ... = ...)
  }
  return(out)
}

#' Get function documentation for this project
#'
#' This function is analogous to "?", "??", and "help". For now, this effort is
#' distributed as a project instead of a package. This imposes certain
#' limitations, particularly regarding function documentation. Use this function
#' to see the documentation for functions in this project just as you would any
#' installed package. The other limitation is that these help files will not
#' populate directly as a pop up when using RStudio tab completion.
#' 
#' @note This function will be deprecated if the project is moved to a package.
#'
#' @param fn_name Object or CHR string name of a function in this project.
#'
#' @return None, opens help file.
#' @export
#'
#' @examples
#' fn_help(fn_help)
fn_help <- function(fn_name) {
  fn_name <- gsub('"', '', deparse(substitute(fn_name)))
  stopifnot(length(fn_name) == 1)
  using_rstudio <- try(rstudioapi::isAvailable())
  if (inherits(using_rstudio, "try-error")) {
    using_rstudio <- FALSE
  }
  rd_dir <- here::here("man")
  html_dir <- here::here(rd_dir, "html")
  fn_file <- sprintf("%s.Rd", fn_name)
  fn_file_rendered <- here::here(html_dir, gsub(".Rd", ".html", fn_file))
  fn_file <- here::here(rd_dir, fn_file)
  if (!file.exists(fn_file)) {
    stop(sprintf("No help file exists for %s.", fn_name))
  }
  if (using_rstudio) {
    rstudioapi::previewRd(fn_file)
  } else {
    if (!dir.exists(html_dir)) dir.create(html_dir)
    if (!file.exists(fn_file_rendered)) {
      tools::Rd2HTML(Rd = fn_file, out = fn_file_rendered)
    }
    utils::browseURL(fn_file_rendered)
  }
}

#' Rebuild the help files as HTML with an index
#' 
#' @param rebuild_book LGL scalar of whether or not to rebuild an associated bookdown document
#' @param book Path to folder containing the bookdown document to rebuild
#'
#' @return URL to the requested book
#' 
rebuild_help_htmls <- function(rebuild_book = TRUE, book = "dimspec_user_guide") {
  if (rebuild_book) {
    if (!"bookdown" %in% installed.packages()) warning("The bookdown package is required to build the user guide.")
    rebuild_book <- FALSE
    stopifnot(dir.exists(here::here(book)))
  }
  help_files <- list.files(here::here("man"), pattern = ".Rd$")
  html_dir <- here::here("man", "html")
  if (!dir.exists(html_dir)) dir.create(html_dir)
  index <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><html xmlns="http://www.w3.org/1999/xhtml"><head><title>DIMSpec Help Index</title><meta http-equiv="Content-Type" content="text/html; charset=utf-8"></meta><link rel="stylesheet" type="text/css" href="R-help.css"></head><body><table width="100%" summary="Help Index for DIMSpec Project"><tr><td><h2>DIMSpec Help Index</h2></td><td style="text-align: right;">R Documentation</td></tr></table><table id="function_list">'
  for (fname in help_files) {
    in_file <- here::here("man", fname)
    out_file <- gsub(".Rd", ".html", here::here(html_dir, fname))
    print(sprintf("(%d of %d) Knitting %s to %s", which(help_files == fname), length(help_files), basename(in_file), basename(out_file)))
    tools::Rd2HTML(in_file, out_file, stylesheet = "R-help.css")
    contents <- readLines(in_file)
    title <- grep("^\\\\title", contents, value = TRUE)
    title <- gsub("\\\\|title|\\{|\\}", "", title)
    index <- paste0(
      index,
      sprintf(
        '<tr><td><a id="%s" class="fn_link" href="%s" target="_blank">%s</a></td><td>%s</td></tr>',
        sprintf("fn_%s", tools::file_path_sans_ext(basename(out_file))),
        basename(out_file),
        tools::file_path_sans_ext(basename(out_file)),
        title
      )
    )
  }
  invisible(file.copy(here::here("man", "R-help.css"), here::here("man", "html", "R-help.css")))
  index <- paste0(index, "</table></body></html>")
  readr::write_file(index, here::here(html_dir, "_index.html"))
  if (rebuild_book) {
    book_url <- bookdown::render_book("dimspec_user_guide")
    invisible(file.copy(here::here("man", "R-help.css"), here::here("dimspec_user_guide", "man", "html", "R-help.css")))
    return(book_url)
  } else {
    return("HTML help files rebuilt.")
  }
}

#' View an index of help documentation in your browser
#'
#' @return None
#' @export
fn_guide <- function() {
  index_file <- grep("index", list.files(here::here("man", "html"), full.names = TRUE), value = TRUE)
  if (index_file == "") stop("Could not locate the index file. Do you need to use rebuild_help_htmls()?")
  using_rstudio <- try(rstudioapi::isAvailable())
  if (inherits(using_rstudio, "try-error")) {
    using_rstudio <- FALSE
  }
  if (using_rstudio) {
    rstudioapi::viewer(index_file)
  } else {
    utils::browseURL(index_file)
  }
}

#' Launch the User Guide for DIMSpec
#'
#' Use this function to launch the bookdown version of the User Guide for the
#' NIST Database Infrastructure for Mass Spectrometry (DIMSpec) Toolkit
#'
#' @note This works ONLY when DIMSpec is used as a project with the defined
#'   directory structure
#'
#' @param view_on_github LGL scalar of whether to use the hosted version of the
#'   User Guide on GitHub (default: TRUE is recommended) which will always
#'   display the most up to date version
#' @param path CHR scalar representing a valid file path to the local user guide
#' @param url_gh CHR scalar pointing to the web resource, in this case the URL
#'   to the User Guide hosted on GitHub pages
#'   
#' @usage 
#' user_guide()
#'
#' @return None, opens a browser to the index page of the User Guide
#' @export
#' 
user_guide <- function(view_online = TRUE, path = here::here("docs", "index.html"), url_gh = "https://pages.nist.gov/dimspec/docs/index.html") {
  stopifnot(
    is.logical(view_online), length(view_online) == 1,
    is.character(path), length(path) == 1,
    is.character(url_gh), length(url_gh) == 1
  )
  if (view_online) {
    if (httr::http_error(url_gh)) {
      warning("The User Guide is not available at this time. Using a local copy. Search will be disabled.")
    } else {
      path = url_gh
    }
  } else {
    stopifnot(file.exists(path))
    error("Could not find the DIMSpec User Guide. The default path is 'docs/index.html'; please verify the file used for argument `path` exists.")
  }
  browseURL(url = path)
}
