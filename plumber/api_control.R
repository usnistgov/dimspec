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
#'
#' @return None, stops the plumber server
#' @export
api_stop <- function(pr = plumber_status, flush = TRUE, db_conn = "con") {
  pr$kill()
  if (flush) {
    if (valid_db_conn(eval(sym(db_conn))))
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
api_reload <- function(pr = plumber_status, background = TRUE) {
  api_stop(pr)
  if (background) {
    plumber_status <<- callr::r_bg(
      function() {
        source(file.path("plumber", "env_plumb.R"))
        api_start(
          on_host = PLUMBER_HOST,
          on_port = PLUMBER_PORT
        )
      }
    )
  } else {
    api_start(
      on_host = PLUMBER_HOST,
      on_port = PLUMBER_PORT
    )
  }
  if (plumber_status$is_alive()) {
    cat("Reloaded")
  } else {
    cat("Unknown error")
  }
}
