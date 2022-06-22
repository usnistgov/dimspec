#' Start the plumber API
#'
#' This is a wrapper to [plumber::pr_run] pointing to a project's opinionated
#' plumber settings with some error trapping. The host, port, and plumber file
#' are set in the "config/env_R.R" location as PLUMBER_HOST, PLUMBER_PORT, and
#' PLUMBER_FILE respectively.
#'
#' @note If either of `on_host` or `on_port` are NULL they will default first to
#'   any existing environment values of PLUMBER_HOST and PLUMBER_PORT, then to
#'   getOption("plumber.host", "127.0.0.1") and getOption("plumber.port", 8080)
#'
#' @note This will fail if the requested port is in use.
#'
#' @param plumber_file CHR scalar of the path to a plumber API to launch
#'   (default: NULL)
#' @param on_host CHR scalar of the host IP address (default: NULL)
#' @param on_port CHR or INT scalar of the host port to use (default: NULL)
#'
#' @return LGL scalar with success status
#' @export
api_start <- function(plumber_file = NULL,
                      on_host = NULL,
                      on_port = NULL) {
  if (exists("log_it")) log_fn("start")
  if (is.null(plumber_file)) {
    if (exists("PLUMBER_FILE")) {
      plumber_file <- PLUMBER_FILE
    } else {
      msg <- 'Provide a file path to a plumber file for "plumber_file" or set variable "PLUMBER_FILE" in the "config/env_R.R" file.'
      if (exists("log_it")) {
        log_it("error", msg, "api")
      }
      stop(msg)
    }
  }
  if (is.null(on_host)) {
    if (exists("PLUMBER_HOST")) {
      on_host <- PLUMBER_HOST
    } else {
      if (exists("log_it")) {
        log_it("error",
               'Provide a host IP address for "on_host" or set variable "PLUMBER_HOST" in the "config/env_R.R" file.',
               "api")
        on_host <- getOption("plumber.host", "127.0.0.1")
      }
    }
  }
  if (is.null(on_port)) {
    if (exists("PLUMBER_PORT")) {
      on_port <- PLUMBER_PORT
    } else {
      if (exists("log_it")) {
        log_it("error",
               'Provide a port number for "on_port" or set variable "PLUMBER_PORT" in the "config/env_R.R" file.',
               "api")
        on_port <- getOption("plumber.port", 8080)
      }
    }
  }
  if (!is.numeric(on_port)) on_port <- as.numeric(on_port)
  
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(plumber_file, on_host, on_port),
      conditions = list(
        plumber_file = list(c("mode", "character"), "file_exists", c("length", 1)),
        on_host      = list(c("mode", "character"), c("length", 1)),
        on_port      = list(c("mode", "numeric"), c("length", 1), "no_na")
      )
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
  if (exists("log_it")) log_it("trace", "Request received to start plumber API.", "api")
  attempt <- try(
    plumber::pr_run(
      pr = plumber::pr(plumber_file) %>%
        pr_hook("exit", function() {
          if (exists("log_it")) log_it("info", glue::glue("Plumber API shut down at {Sys.time()}."), "api")
        }),
      host = on_host,
      port = on_port
    )
  )
  url <- sprintf("%s:%s", on_host, on_port)
  success <- inherits(attempt, "try-error") && attempt$is_alive() &&
    api_endpoint("active")
  if (success) {
    if (exists("log_it")) {
      log_it("success", glue::glue("Plumber API started at {Sys.time()} from api_start() on {url}."), "api")
    }
  } else {
    if (exists("log_it")) {
      log_it("error", glue::glue("Could not start plumber API using api_start() on {url}."), "api")
    }
  }
  if (exists("log_it")) log_fn("end")
  return(success)
}

#' Open Swagger API documentation
#'
#' This will launch the Swagger UI in a browser tab. The URL suffix "__docs__"
#' will be automatically added if not part of the host URL accepted as `url`.
#'
#' @param url CHR URL/URI of the plumber documentation host (default:
#'   environment variable "PLUMBER_URL")
#'
#' @return None, opens a browser to the requested URL
#' @export
api_open_doc <- function(url = PLUMBER_URL) {
  if (exists("log_it")) log_fn("start")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        url      = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  } else {
    stopifnot(is.character(url))
    stopifnot(length(url) == 1)
  }
  docs <- "__docs__"
  if (!stringi::stri_detect(str = url, regex = docs)) {
    url <- sprintf("%s/%s/", url, docs)
  }
  if (exists("log_it")) log_it("debug", glue::glue("API documentation accessed at {Sys.time()} on {url}."), "api")
  utils::browseURL(url)
  if (exists("log_it")) log_fn("end")
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
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace", "Run api_stop().", "api")
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        pr                 = list(c("class", "r_process", "process", "R6")),
        flush              = list(c("mode", "logical"), c("length", 1)),
        db_conn            = list(c("mode", "character"), c("length", 1)),
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
  if (exists("log_it")) log_it("debug", "Killing plumber service.", "api")
  pr$kill()
  if (remove_service_obj) {
    rm(list = deparse(substitute(pr)), envir = .GlobalEnv)
  }
  if (!exists(db_conn)) flush <- FALSE
  if (flush) {
    if (exists("log_it")) log_it("debug", "Flushing database connections and reconnecting.", "api")
    if (active_connection(eval(sym(db_conn))))
      manage_connection(conn_name = db_conn, reconnect = F)
    manage_connection(conn_name = db_conn)
  }
  if (exists("log_it")) log_fn("end")
}

#' Reloads the plumber API
#'
#' @param pr Rterm process object of class "r_process", "process", and "R6"
#'   created from [plumber::pr_run] if NULL it will be assumed that no plumber
#'   instance is currently running
#' @param background LGL scalar of whether to load the plumber server as a
#'   background service (default: TRUE); set to FALSE for testing
#' @param on_host CHR scalar of the URL directing to the plumber service;
#'   default (NULL) obtains its value from that set as PLUMBER_HOST in config/env_R.R
#' @param on_port CHR scalar of the remote port for the plumber service;
#'   default (NULL) obtains its value from that set as PLUMBER_PORT in config/env_R.R
#' @param log_ns CHR scalar namespace to use for logging (default: "api")
#'
#' @return None, launches the plumber API service on your local machine
#' @export
#'
api_reload <- function(pr = NULL, background = TRUE, on_host = NULL, on_port = NULL, log_ns = "api") {
  if (exists("log_it")) {
    log_fn("start")
    log_it("debug", "Run api_reload().", log_ns)
  }
  if (is.null(on_host)) on_host <- PLUMBER_HOST
  if (is.null(on_port)) on_port <- PLUMBER_PORT
  if (!exists("PLUMBER_URL")) PLUMBER_URL <- sprintf("%s:%s", on_host, on_port)
  if (!is.numeric(on_port)) on_port <- as.numeric(on_port)
  if (is.character(pr)) {
    pr_name <- pr
    pr <- NULL
  }
  # Argument validation relies on verify_args
  check_args <- list(pr, background, on_host, on_port)
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
  url <- sprintf("http://%s:%s", on_host, on_port)
  if (exists("log_it")) {
    if (!url == PLUMBER_URL) {
      log_it("warn",
             sprintf("Plumber was launched on a url (%s) other than that defined in the environment (%s).",
                     url, PLUMBER_URL),
             log_ns
      )
    }
    log_it("debug", "Calling api_start() from api_reload()", log_ns)
  }
  if (background) {
    assign(
      x = pr_name,
      value = callr::r_bg(
        function() {
          source(file.path("plumber", "env_plumb.R"))
          api_start(
            on_host = PLUMBER_HOST,
            on_port = PLUMBER_PORT,
            plumber_file = PLUMBER_FILE
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
  if (exists("log_it")) log_it("trace", "Evaluating service...", log_ns)
  if (this_pr$is_alive()) {
    if (exists("log_it")) {
      log_it("info",
             sprintf(
               "\nRunning plumber API at %s",
               url
             ), log_ns
      )
      log_it("info",
             sprintf(
               "\nView docs at %s/__docs__/ or by calling `api_open_doc(PLUMBER_URL)`",
               url
             ), log_ns
      )
    }
  } else {
    if (exists("log_it")) {
      log_it("error",
             sprintf(
               'Unknown error restarting the plumber API. Inspect "%s" for details.',
               pr_name
             ), log_ns
      )
    }
  }
  if (exists("log_it")) {
    log_fn("end")
    log_it("debug", "Exiting api_reload().", log_ns)
  }
}


#' Build an API endpoint programmatically
#'
#' This is a convenience function intended to support plumber endpoints. It only
#' assists in the construction (and execution if `execute` == TRUE) of
#' endpoints. Endpoints must still be understood. Validity checking, execution,
#' and opening in a web browser are supported. Invalid endpoints will not be
#' executed or opened for viewing.
#'
#' @note Special support is provided for the way in which the NIST Public Data
#'   Repository treats fragments
#'
#' @param server_addr CHR scalar uniforme resource locator (URL) address of an
#'   API server (e.g. "https://myapi.com:8080") (defaults to the current
#'   environment variable "PLUMBER_URL")
#' @param ... Additional named parameters added to the endpoint, most typically
#'   the query portion. If only one is provided, it can remain unnamed and a
#'   query is assumed. If more than one is provided, all must be named. Named
#'   elements must be components of the return from [httr::parse_url] (see
#'   https://tools.ietf.org/html/rfc3986) for details of the parsing algorithm;
#'   unrecognized elements will be ignored.
#' @param check_valid LGL scalar on whether or not to first check that an
#'   endpoint returns a valid status code (200-299) (default: TRUE).
#' @param execute LGL scalar of whether or not to execute the constructed
#'   endpoint and return the result; will be defaulted to FALSE if `check_valid`
#'   == TRUE and the endpoint returns anything other than a valid status code.
#'   (default: TRUE)
#' @param open_in_browser LGL scalar of whether or not to open the resulting
#'   endpoint in the system's default browser; will be defaulted to FALSE if
#'   `check_valid` == TRUE and the endpoint returns anything other than a valid
#'   status code. (default: FALSE)
#'
#' @return CHR scalar of the constructed endpoint, with messages regarding
#'   status checks, return from the endpoint (typically JSON) if valid and
#'   `execute` == TRUE, or NONE if `open_in_browser` == TRUE
#' @export
#'
#' @examples
#' api_endpoint("https://www.google.com/search", list(q = "something"), open_in_browser = TRUE)
#' api_endpoint("https://www.google.com/search", query = list(q = "NIST Public Data Repository"))
api_endpoint <- function(server_addr     = PLUMBER_URL,
                         ...,
                         check_valid     = TRUE,
                         execute         = TRUE,
                         open_in_browser = FALSE,
                         return_format   = c("vector", "data.frame", "list")) {
  require(httr)
  url <- parse_url(server_addr)
  kwargs <- list(...)
  query <- list()
  if (length(kwargs) == 1 && is.null(names(kwargs))) {
    message("Single arguments provided to the ellipsis will be assumed to be query text.")
    query <- kwargs[[1]]
  } else {
    if (!is.null(names(kwargs))) {
      if (any(names(kwargs) == "")) warning("All arguments should be named if any are.")
      for (kwarg in names(kwargs)[!names(kwargs) == ""]) {
        if (kwarg %in% names(url)) {
          url[[kwarg]] <- kwargs[[kwarg]]
        } else {
          query <- append(query, setNames(kwargs[[kwarg]], kwarg))
        }
      }
    } else {
      warning("If more than one ellipsis argument is provided they must be named.")
    }
  }
  url$query <- append(url$query, query)
  if (url$hostname == "data.nist.gov" && !is.null(url$fragment)) {
    url$path <- paste0(url$path, "#", url$fragment, collapse = "/")
    url$fragment <- NULL
  }
  url <- build_url(url)
  if (check_valid) {
    res <- httr::GET(url)
    if (dplyr::between(res$status_code, 200, 299)) {
      message(sprintf("Endpoint %s is valid.", url))
    } else {
      warning(httr::http_status(res)$message)
      execute <- FALSE
      open_in_browser <- FALSE
    }
  }
  if (execute || open_in_browser) {
    if (open_in_browser) {
      utils::browseURL(url)
    } else {
      return_format <- match.arg(return_format)
      out <- httr::content(res)
      out <- switch(return_format,
                    "vector" = unlist(out),
                    "data.frame" = out %>% bind_rows(),
                    "list" = out)
      return(out)
    }
  } else {
    return(url)
  }
}