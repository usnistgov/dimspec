#' Start the plumber API
#'
#' @param on_host CHR scalar of the host IP address
#' @param on_port CHR or INT scalar of the host port to use
#'
#' @return None, launches the plumber API server
#' @export
api_start <- function(on_host, on_port) {
  plumber::pr_run(
    pr = plumber::pr(file.path("plumber", "plumber.R")),
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
#' @param pr Rterm process object of class "r_process", "process", and "R6" created from [plumber::pr_run]
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
  pr_name <- obj_name_check(pr)
  if (all(c("r_process", "process", "R6") %in% class(pr))) {
    if (pr$is_alive()) api_stop(pr)
  }
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
             'Unknown error restarting the plumber API. Inspect the object named "%s" for details.',
             pr_name
           )
    )
  }
}

#' Sanity check for plumber service name
#'
#' Provides a sanity check on whether or not a name reference exists and return
#' its name if so. If not, return the default name of defined from
#' `default_name`. This largely is used to prevent naming conflicts as part of
#' managing the plumber service (thus the default name) but can be used for any
#' item in the current namespace.
#'
#' @param obj R object or CHR scalar in question to be resolved in the namespace
#' @param default_name CHR scalar name to use for `obj` if it does not exist.
#'
#' @return CHR scalar of the resolved name
obj_name_check <- function(obj, default_name = "plumber_service") {
  if (is.character(obj)) {
    pr_name <- obj
  } else {
    pr_name <- deparse(substitute(obj))
  }
  if (exists(pr_name)) {
    pr <- eval(sym(pr_name))
  } else {
    pr_name <- default_name
  }
  return(pr_name)
}
