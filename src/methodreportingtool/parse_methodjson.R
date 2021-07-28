library(jsonlite)

parse_methodjson <- function(jsonfile) {
  json <- readLines(jsonfile)
  fromJSON(json)
}