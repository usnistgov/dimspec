api_start <- function(on_host, on_port) {
  pr_run(
    pr = pr("plumber/plumber.R"),
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

api_stop <- function(pr = plumber_status) {
  if (exists("con")) {
    manage_connection(reconnect = F)
  }
  pr$kill()
}
