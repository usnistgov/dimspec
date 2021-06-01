require(xlsx)

list_to_xlsx <- function(inputlist, file = "output.xlsx", rowNames = FALSE) {
  wb <- createWorkbook(type = "xlsx")
  for (i in 1:length(inputlist)) {
    newsheet <- createSheet(wb, sheetName = names(inputlist[i]))
    addDataFrame(inputlist[[i]], newsheet, row.names = rowNames)
  }
  saveWorkbook(wb, file)
}