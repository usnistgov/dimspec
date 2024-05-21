library(jsonlite)

parse_methodjson <- function(jsonfile) {
  json <- readLines(jsonfile)
  json <- stringr::str_replace_all(
    json,
    '"fragment_radical": ,',
    '"fragment_radical": "unknown",'
  )
  fromJSON(json)
}