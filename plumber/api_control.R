#' Start the plumber API
#'
#' This is a thin wrapper to [plumber::pr_run] pointing to a project's plumber
#' file by default at "plumber/plumber.R" with some error trapping. It also
#' provides some infrastructure support for set ups with a "plumber/env_plumb.R"
#' file containing environment variables that will be used as defaults
#' supporting NULL assignments.
#'
#' @param on_host CHR scalar of the host IP address
#' @param on_port CHR or INT scalar of the host port to use
#' @param plumber_file CHR scalar of the path to a plumber API to launch
#'   (default: `file.path("plumber", "plumber.R")`)
#'
#' @return None, launches the plumber API server
#' @export
api_start <- function(on_host = NULL, on_port = NULL, plumber_file = file.path("plumber", "plumber.R")) {
  if (is.null(on_host)) {
    if (exists("PLUMBER_HOST")) {
      on_host <- PLUMBER_HOST
    } else {
      if (exists("log_it")) {
        log_it("error",
               'Provide a host IP address for "on_host" or set variable "PLUMBER_HOST" in the "plumber/env_plumb.R" file.',
               ns = "api")
        return(invisible(NULL))
      }
    }
  }
  if (is.null(on_port)) {
    if (exists("PLUMBER_PORT")) {
      on_port <- PLUMBER_PORT
    } else {
      if (exists("log_it")) {
        log_it("error",
               'Provide a port number for "on_port" or set variable "PLUMBER_PORT" in the "plumber/env_plumb.R" file.',
               ns = "api")
        return(invisible(NULL))
      }
    }
  }
  if (!is.numeric(on_port)) on_port <- as.numeric(on_port)
  
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        on_host      = list(c("mode", "character"), c("length", 1)),
        on_port      = list(c("mode", "numeric"), c("length", 1), "no_na"),
        plumber_file = list(c("mode", "character"), "file_exists", c("length", 1))
      ),
      from_fn = "api_start"
    )
    stopifnot(arg_check$valid)
  } else {
    stopifnot(is.character(on_host))
    stopifnot(length(on_host) == 1)
    stopifnot(is.character(on_port))
    stopifnot(length(on_port) == 1)
    stopifnot(is.character(plumber_file))
    stopifnot(length(plumber_file) == 1)
    stopifnot(file.exists(plumber_file))
  }
  
  plumber::pr_run(
    pr = plumber::pr(plumber_file),
    host = on_host,
    port = on_port
  )
}

#' Open Swagger API documentation
#'
#' This will launch the Swagger UI in a browser tab. The URL suffix "__docs__"
#' will be automatically added if not part of the host URL accepted as `url`.
#'
#' @param url CHR URL/URI of the plumber documentation host (default:
#'   environment variable "plumber_url")
#'
#' @return None, opens a browser to the requested URL
#' @export
api_open_doc <- function(url = plumber_url) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        url      = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "api_open_doc"
    )
    stopifnot(arg_check$valid)
  } else {
    stopifnot(is.character(plumber_url))
    stopifnot(length(plumber_url) == 1)
  }
  
  docs <- "__docs__"
  if (!stringi::stri_detect(str = url, regex = docs)) {
    url <- sprintf("%s/%s/", url, docs)
  }
  utils::browseURL(url)
}

#' Stop the plumber API
#'
#' @note This will also kill and restart the connection object if `flush` is
#'   TRUE to release connections with certain configurations such as SQLite in
#'   write ahead log mode.
#'
#' @param pr Rterm process object of class "r_process", "process", and "R6"
#'   created from [plumber::pr_run]
#' @param flush LGL scalar of whether to disconnect and reconnect to a database
#'   connection named as `db_conn` (default: TRUE)
#' @param db_conn CHR scalar of the connection object name (default: "con")
#' @param remove_service_obj LGL scalar of whether to remove the reference to
#'   `pr` from the current global environment (default: TRUE)
#'
#' @return None, stops the plumber server
#' @export
api_stop <- function(pr = plumber_service, flush = TRUE, db_conn = "con", remove_service_obj = TRUE) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        pr                 = list(c("class", "r_process", "process", "R6")),
        flush              = list(c("mode", "logical"), c("length", 1)),
        db_conn            = list(c("mode", "character"), "file_exists", c("length", 1)),
        remove_service_obj = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn = "api_stop"
    )
    stopifnot(arg_check$valid)
  } else {
    stopifnot(all(c("r_process", "process", "R6") %in% class(pr)))
    stopifnot(is.character(db_conn))
    stopifnot(length(db_conn) == 1)
    stopifnot(is.logical(flush))
    stopifnot(length(flush) == 1)
    stopifnot(is.logical(remove_service_obj))
    stopifnot(length(remove_service_obj) == 1)
  }
  
  pr$kill()
  if (remove_service_obj) {
    rm(list = deparse(substitute(pr)), envir = .GlobalEnv)
  }
  if (!exists(db_conn)) flush <- FALSE
  if (flush) {
    if (active_connection(eval(sym(db_conn))))
    manage_connection(conn_name = db_conn, reconnect = F)
    manage_connection(conn_name = db_conn)
  }
}

#' Reloads the plumber API
#'
#' @param pr Rterm process object of class "r_process", "process", and "R6"
#'   created from [plumber::pr_run] if NULL it will be assumed that no plumber
#'   instance is currently running
#' @param background LGL scalar of whether to load the plumber server as a
#'   background service (default: TRUE); set to FALSE for testing
#'
#' @return
#' @export
#'
#' @examples
api_reload <- function(pr = NULL, background = TRUE, on_host = NULL, on_port = NULL) {
  if (is.null(on_host)) on_host <- PLUMBER_HOST
  if (is.null(on_port)) on_port <- PLUMBER_PORT
  if (!is.numeric(on_port)) on_port <- as.numeric(on_port)
  # Argument validation relies on verify_args
  check_args <- as.list(environment())
  check_args <- check_args[!unlist(lapply(check_args, is.null))]
  check_conds <- list(
    if (is.null(pr)) {
      NULL
    } else {
      pr       = list(c("class", "r_process", "process", "R6"))
    },
    background = list(c("mode", "logical"), c("length", 1)),
    on_host    = list(c("mode", "character"), c("length", 1)),
    on_port    = list(c("mode", "numeric"), "no_na", c("length", 1))
  )
  check_conds <- check_conds[!unlist(lapply(check_conds, is.null))]
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = check_args,
      conditions = check_conds,
      from_fn = "api_stop"
    )
    stopifnot(arg_check$valid)
  } else {
    stopifnot(is.character(on_host))
    stopifnot(length(on_host) == 1)
    stopifnot(is.character(on_port))
    stopifnot(length(on_port) == 1)
    stopifnot(is.character(plumber_file))
    stopifnot(length(plumber_file) == 1)
    stopifnot(file.exists(plumber_file))
  }
  if (!is.null(pr) && pr$is_alive()) api_stop(pr)
  pr_name <- obj_name_check(pr, "plumber_service")
  if (background) {
    assign(
      x = pr_name,
      value = callr::r_bg(
        function() {
          source(file.path("plumber", "env_plumb.R"))
          api_start(
            on_host = on_host,
            on_port = on_port
          )
        }
      ),
      envir = .GlobalEnv
    )
  } else {
    api_start(
      on_host = on_host,
      on_port = on_port
    )
  }
  this_pr <- eval(sym(pr_name))
  if (this_pr$is_alive()) {
    plumber_url <- sprintf("http://%s:%s", on_host, on_port)
    log_it("info",
           sprintf(
             "\nRunning plumber API at %s",
             plumber_url
             )
           )
    log_it("info",
           sprintf(
             "\nView docs at %s/__docs__/ or by calling `api_open_doc(plumber_url)`",
             plumber_url
             )
           )
  } else {
    log_it("error",
           sprintf(
             'Unknown error restarting the plumber API. Inspect "%s" for details.',
             pr_name
           )
    )
  }
}

#' Sanity check for plumber service name
#'
#' Provides a sanity check on whether or not a name reference exists and return
#' its name if so. If not, return the default name defined from `default_name`.
#' This largely is used to prevent naming conflicts as part of managing the
#' plumber service but can be used for any item in the current namespace.
#'
#' @param obj R object or CHR scalar in question to be resolved in the namespace
#' @param default_name CHR scalar name to use for `obj` if it does not exist
#'   (default: NULL).
#'
#' @return CHR scalar of the resolved name
#'
#' @export
#' @example
#' \dontrun {\preformatted {
#'   if (exists("log_it")) {
#'     obj_name_check("test", "test")
#'     test <- letters
#'     obj_name_check(test)
#'   }
#' }}
obj_name_check <- function(obj, default_name = NULL) {
  if (is.null(obj) || is.na(obj)) {
    if (is.null(default_name) || is.na(default_name)) {
      return(NULL)
    } else {
      obj_name <- default_name
    }
  } else {
    obj_name <- deparse(substitute(obj)) %>%
      str_remove_all('"')
  }
  if (exists(obj_name)) {
    # Placeholder to do maybe do something like preservation/backup of current
    # object or suggest a new name
    
    # pr <- eval(sym(obj_name))
  } else {
    log_it("warn", glue('No object named "{obj_name}" exists. Defaulting to "{default_name}".'))
    if (is.null(default_name)) {
      log_it("warn", "No default name provided. Name given back as-is.")
    } else {
      obj_name <- default_name
    }
  }
  return(obj_name)
}
