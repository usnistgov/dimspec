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
      msg <- 'Provide a file path to a plumber file for "plumber_file" or set variable "PLUMBER_FILE" (if set in the "config/env_R.R" file it will be maintained in the future).'
      if (exists("log_it")) {
        log_it("error", msg, "api")
      }
      stop(msg)
    }
  }
  on_host <- rectify_null_from_env(on_host, PLUMBER_HOST, getOption("plumber.host", "127.0.0.1"))
  on_port <- rectify_null_from_env(on_port, PLUMBER_PORT, getOption("plumber.port", 8080))
  if (!is.integer(on_port)) on_port <- as.integer(on_port)
  
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
      pr = plumber::pr(plumber_file),
      host = on_host,
      port = on_port
    )
  )
  if (on_host == "0.0.0.0" && !API_LOCALHOST && API_HOST == "") on_host <- Sys.info()[["nodename"]]
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
#' @note This function assumes the object referenced by name `pr` exists in the
#'   global environment, and `remove_service_object` will only remove it from
#'   .GlobalEnv.
#'
#' @param pr CHR scalar name of the plumber service object, typically only
#'   created as a background observer from [callr::r_bg] as a result of calling
#'   [api_reload] (default: NULL gets the environment setting for
#'   PLUMBER_OBJ_NAME)
#' @param flush LGL scalar of whether to disconnect and reconnect to a database
#'   connection named as `db_conn` (default: TRUE)
#' @param db_conn CHR scalar of the connection object name (default: "con")
#' @param remove_service_obj LGL scalar of whether to remove the reference to
#'   `pr` from the current global environment (default: TRUE)
#'
#' @return None, stops the plumber server
#' @export
api_stop <- function(pr = NULL, flush = TRUE, db_conn = "con", remove_service_obj = TRUE) {
  if (exists("log_it")) {
    log_fn("start")
    log_it("trace", "Run api_stop().", "api")
  }
  if (is.null(pr)) pr <- rectify_null_from_env(pr, PLUMBER_OBJ_NAME, "plumber_service", "api")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        pr                 = list(c("mode", "character"), c("length", 1)),
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
  if (!exists(pr)) {
    log_it("info", glue::glue("No object named {pr} exists to stop. Is the name correct?"), "api")
    return(invisible(NULL))
  }
  if (exists("log_it")) log_it("debug", "Killing plumber service.", "api")
  if (eval(rlang::sym(pr))$is_alive()) eval(rlang::sym(pr))$kill()
  if (!exists(db_conn)) flush <- FALSE
  if (flush) {
    if (exists("log_it")) log_it("debug", "Flushing database connections and reconnecting.", "api")
    if (active_connection(eval(rlang::sym(db_conn))))
      manage_connection(conn_name = db_conn, reconnect = F)
    manage_connection(conn_name = db_conn)
  }
  if (remove_service_obj) {
    rm(list = pr, envir = .GlobalEnv)
  }
  if (exists("log_it")) log_fn("end")
}

#' Reloads the plumber API
#'
#' Depending on system architecture, the plumber service may take some time to
#' spin up and spin down. If `background` is TRUE, this may mean the calling R
#' thread runs ahead of the background process resulting in unexpected behavior
#' (e.g. newly defined endpoints not being available), effectively binding it to
#' the prior iteration. If the API does not appear to be reloading properly, it
#' may be necessary to manually kill the process controlling it through your OS
#' and to call this function again.
#'
#' @inheritParams api_start
#' @inheritParams api_stop
#'
#' @param background LGL scalar of whether to load the plumber server as a
#'   background service (default: TRUE); set to FALSE for testing
#' @param log_ns CHR scalar namespace to use for logging (default: "api")
#'
#' @return Launches the plumber API service on your local machine and returns
#'   the URL on which it can be accessed as a CHR scalar
#' @export
#' 
api_reload <- function(pr = NULL,
                       background = TRUE,
                       plumber_file = NULL,
                       on_host = NULL,
                       on_port = NULL,
                       log_ns = "api") {
  if (exists("log_it")) {
    log_fn("start")
    log_it("debug", "Run api_reload().", log_ns)
  }
  pr      <- rectify_null_from_env(pr, PLUMBER_OBJ_NAME, "plumber_service")
  pr_name <- pr
  on_host <- rectify_null_from_env(on_host, PLUMBER_HOST, getOption("plumber.host", "127.0.0.1"))
  api_host <- on_host
  on_port <- rectify_null_from_env(on_port, PLUMBER_PORT, getOption("plumber.port", 8080))
  plumber_file <- rectify_null_from_env(plumber_file, PLUMBER_FILE, here::here("inst", "plumber", "plumber.R"))
  api_localhost <- rectify_null_from_env(NULL, API_LOCALHOST, TRUE)
  if (!api_localhost) {
    if (on_host == "" | on_host == "0.0.0.0" | is.null(on_host)) {
      on_host <- "0.0.0.0"
      api_host <- Sys.info()[["nodename"]]
    }
    if (!on_host == "0.0.0.0" && !on_host == Sys.info()[["nodename"]]) {
      message("The provided host name for the API server does not match this machine.")
    }
  }
  if (!is.integer(on_port)) on_port <- as.integer(on_port)
  service_exists <- suppressWarnings(exists(pr))
  if (!service_exists) {
    pr <- NULL
  }
  # Argument validation relies on verify_args
  check_args <- list(pr, background, on_host, on_port, plumber_file)
  check_args <- check_args[!unlist(lapply(check_args, is.null))]
  check_conds <- list(
    pr         = if (is.null(pr)) NULL else list(c("mode", "character"), c("length", 1)),
    background = list(c("mode", "logical"), c("length", 1)),
    on_host    = list(c("mode", "character"), c("length", 1)),
    on_port    = list(c("mode", "numeric"), "no_na", c("length", 1)),
    plumber_file = list(c("mode", "character"), c("length", 1))
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
  }
  stopifnot(file.exists(plumber_file))
  if (!is.null(pr) && eval(rlang::sym(pr))$is_alive()) {
    api_stop(pr = pr)
  }
  url <- sprintf("http://%s:%s", on_host, on_port)
  if (exists("log_it")) {
    if (exists("PLUMBER_URL") && !url == PLUMBER_URL) {
      log_it("warn",
             sprintf("Plumber was launched on a url (%s) other than that defined in the environment (%s).",
                     url, PLUMBER_URL),
             log_ns
      )
    }
    log_it("debug", "Calling api_start() from api_reload()", log_ns)
  }
  url <- sprintf("http://%s:%s", api_host, on_port)
  if (background) {
    assign(
      x = pr_name,
      value = callr::r_bg(
        args = list(
          on_host = on_host,
          on_port = on_port,
          plumber_file = plumber_file
        ),
        func = function(on_host, on_port, plumber_file) {
          if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
          source(here::here("inst", "plumber", "env_plumb.R"))
          api_start(
            on_host = on_host,
            on_port = on_port,
            plumber_file = plumber_file
          )
        }
      ),
      envir = .GlobalEnv
    )
    if (exists("log_it")) {
      sapply(c("api", "global"),
             function(x) {
               log_it("info",
                      glue::glue('Plumber service is spinning up in a background process. Control has been returned to your terminal. Check the status with {pr_name}$is_alive() or call api_endpoint("_ping").'),
                      x)
             })
    }
  } else {
    api_start(
      on_host = on_host,
      on_port = on_port,
      plumber_file = plumber_file
    )
  }
  if (exists("log_it")) log_it("trace", "Evaluating service...", log_ns)
  if (exists(pr_name) && eval(rlang::sym(pr_name))$is_alive()) {
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
  return(invisible(url))
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
#'   Repository treats URL fragments
#' @note This only support [httr::GET] requests.
#'
#' @param path CHR scalar of the endpoint path.
#' @param ... Additional named parameters added to the endpoint, most typically
#'   the query portion. If only one is provided, it can remain unnamed and a
#'   query is assumed. If more than one is provided, all must be named. Named
#'   elements must be components of the return from [httr::parse_url] (see
#'   https://tools.ietf.org/html/rfc3986) for details of the parsing algorithm;
#'   unrecognized elements will be ignored.
#' @param server_addr CHR scalar uniform resource locator (URL) address of an
#'   API server (e.g. "https://myapi.com:8080") (defaults to the current
#'   environment variable "PLUMBER_URL")
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
#' @param max_pings INT scalar maximum number of pings to try before timeout if
#'   using endpoint "_ping"; this is only used for endpoint "_ping" (default:
#'   20)
#' @param return_type CHR scalar on which return type to use, which must be one
#'   of "text", "raw", or "parsed" which will be used to read the content of the
#'   response item (default: "text")
#' @param return_format CHR scalar on which form to return data, which must be
#'   one of "vector", "data.frame", or "list" (default: "vector" to support
#'   primarily single value responses)
#'
#' @return CHR scalar of the constructed endpoint, with messages regarding
#'   status checks, return from the endpoint (typically JSON) if valid and
#'   `execute` == TRUE, or NONE if `open_in_browser` == TRUE
#' @export
#'
#' @examples
#' api_endpoint("https://www.google.com/search", list(q = "something"), open_in_browser = TRUE)
#' api_endpoint("https://www.google.com/search", query = list(q = "NIST Public Data Repository"), open_in_browser = TRUE)
api_endpoint <- function(path,
                         ...,
                         server_addr     = PLUMBER_URL,
                         check_valid     = TRUE,
                         execute         = TRUE,
                         open_in_browser = FALSE,
                         raw_result      = FALSE,
                         max_pings       = 20L,
                         return_type     = c("text", "raw", "parsed"),
                         return_format   = c("vector", "data.frame", "list")) {
  require(httr)
  stopifnot(is.integer(as.integer(max_pings)), length(max_pings) == 1)
  if (str_detect(path, "https|www\\.")) {
    server_addr <- dirname(path)
    path <- basename(path)
  }
  url <- parse_url(server_addr)
  url$path <- path
  kwargs <- list(...)
  query <- list()
  if ("match_criteria" %in% names(kwargs)) {
    if (!is.character(kwargs$match_criteria)) {
      kwargs$match_criteria <- deparse1(kwargs$match_criteria)
    }
  }
  if (length(kwargs) == 1 && is.null(names(kwargs))) {
    message("Single arguments provided to the ellipsis will be assumed to be query text.")
    query <- kwargs[[1]]
  } else if (!length(kwargs) == 0) {
    if (!is.null(names(kwargs))) {
      if (any(names(kwargs) == "")) warning("All arguments should be named if any are.")
      for (kwarg in names(kwargs)[!names(kwargs) == ""]) {
        if (kwarg %in% names(url)) {
          url[[kwarg]] <- kwargs[[kwarg]]
        } else {
          query_i <- kwargs[kwarg]
          for (i in 1:length(query_i)) {
            if (length(query_i[[i]]) > 1) {
              query_i[[i]] <- paste0(kwarg, "=", deparse(substitute(query_i)))
            }
          }
          query <- append(query, query_i)
        }
      }
    } else {
      warning("If more than one ellipsis argument is provided they must be named.")
    }
  }
  if (length(query) > 0) url$query <- append(url$query, query)
  if (url$hostname == "data.nist.gov" && !is.null(url$fragment)) {
    url$path <- paste0(url$path, "#", url$fragment, collapse = "/")
    url$fragment <- NULL
  }
  url <- build_url(url = url)
  if (path == "_ping") {
    pinging <- TRUE
    ping_i <- 1
    ping_limit <- max_pings
    while(pinging) {
      if (ping_i > ping_limit) {
        pinging <- FALSE
        stop("API timeout. Check the `$is_alive()` property of the plumber service or try `api_open_doc` to force a check. You may need to reload with `api_reload()` if the service isn't running.\n")
      } else {
        message(glue::glue("Ping {ping_i} of {ping_limit}...\n"))
        res <- try(httr::GET(url = url))
        if (!inherits(res, "try-error")) {
          message("API is listening.\n")
          pinging <- FALSE
        } else {
          message("API server may still be spinning up...\n")
          Sys.sleep(2)
          ping_i<- ping_i + 1
        }
      }
    }
  }
  res <- httr::GET(url = url)
  if (check_valid) {
    if (res$status_code >= 200 && res$status_code <= 299) {
      message(sprintf("Endpoint %s is valid.", url))
    } else {
      warning(httr::http_status(res)$message)
      execute <- FALSE
      open_in_browser <- FALSE
    }
  }
  if (raw_result) {
    return(res)
  }
  if (execute || open_in_browser) {
    if (open_in_browser) {
      utils::browseURL(url = url)
    } else {
      return_format <- match.arg(return_format)
      return_type   <- match.arg(return_type)
      out <- httr::content(x = res, as = return_type)
      if (return_type == "text") {
        out <- jsonlite::fromJSON(out)
      }
      out <- switch(return_format,
                    "vector" = unlist(out),
                    "data.frame" = bind_rows(out),
                    "list" = out)
      return(out)
    }
  } else {
    return(url)
  }
}
