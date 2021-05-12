require(xlsx)

open_suspectlist <- function(file = file.choose()) {
  if (length(grep(".xlsx$", file)) > 0) {dat <- read.xlsx(file, sheetIndex = 1)}
  if (length(grep(".csv$", file)) > 0) {dat <- read.csv(file, row.names = NULL)}
  dat
}