# require(xlsx)

xlsx_to_list <- function(xlsx_file) {
  wb <- loadWorkbook(xlsx_file)
  sheets <- getSheets(wb)
  o <- list()
  for (i in 1:length(sheets)) {
    o[[names(sheets)[i]]] = read.xlsx(xlsx_file, sheetIndex = i)
  }
  o
}