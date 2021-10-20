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
  require(glue)
  if (length(args) != length(conditions)) {
    log_it("error", sprintf('Length of "args" [%s] must match the length of "conditions" [%s]',
                            length(args),
                            length(conditions)))
  }
  names(args)  <- names(conditions)
  if (is_null(from_fn)) from_fn <- deparse(sys.call(-1)[[1]])
  log_it("trace", glue('Verifying arguments for "{from_fn}".'))
  if (length(args) != length(conditions)) stop('Each item in "args" needs at least one matching condition.')
  check_types  <- c("class", "mode", "length", "no_na", "n>", "n<", "n>=", "n<=", ">", "<", ">=", "<=", "between", "choices", "FUN")
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
        log_it("warn", msg)
      }
    }
  }
  if (out$valid) {
    log_it("trace", sprintf("Arguments verified for '%s'", from_fn))
  } else {
    log_it("error", sprintf("Arguments could not be verified for '%s'. See return for details.", from_fn))
  }
  return(out)
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
  require(glue)
  if (length(namelist) == 1)
    return(glue::glue("{paste0(namelist, collapse = '')}"))
  res <- glue::glue(
    "{paste0(namelist[1:length(namelist) - 1], collapse = ', ')}\\
     {ifelse(length(namelist) > 2, ',', '')} \\
     {ifelse(length(namelist) > 1, paste('and ', namelist[length(namelist)], sep = ''), '')}")
  return(res)
}

#' Conveniently log a message to the console
#'
#' Use this to log messages of various level in the console for situations where
#' package [logger] may not be available.
#'
#' @param log_level CHR scalar of the level at which to log a given statement.
#'   If using the [logger] package, must match one of [logger:::log_levels]
#' @param msg CHR scalar of the message to accompany the log.
#'
#' @return Adds to the logger log (if enabled) and prints to the console in all
#'   cases
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
log_it <- function(log_level, msg) {
  log_func  <- sprintf("log_%s", tolower(log_level))
  n_call    <- sys.nframe() * -1 + 1
  if (exists(log_func)) {
    log_level(level    = toupper(log_level),
              .topcall = sys.call(n_call),
              msg)
  } else {
    msg <- sprintf("%s [%s] in %s(): %s",
                   toupper(log_level),
                   format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                   deparse(sys.call(n_call)[[1]]),
                   msg)
    cat(msg)
  }
}