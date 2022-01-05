api_start <- function(on_host, on_port) {
  plumber::pr_run(
    pr = plumber::pr(file.path("plumber", "plumber.R")),
    host = on_host,
    port = on_port
  )
}

api_open_doc <- function(url = plumber_url) {
  docs <- "__docs__"
  if (!stringi::stri_detect(str = url, regex = docs)) {
    url <- sprintf("%s/%s/", url, docs)
  }
  utils::browseURL(url)
}

api_stop <- function(pr = plumber_status, db_conn = con) {
  pr$kill()
  manage_connection(reconnect = F)
  manage_connection()
}

api_reload <- function(background = FALSE) {
  api_stop()
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
