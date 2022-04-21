addmetadata <- function(peaklist_data) {
  metadata <- unique(c(peaklist_data$PEAKLIST$MS_METHOD, peaklist_data$PEAKLIST$LC_METHOD, peaklist_data$PEAKLIST$QC_METHOD))
  for (i in metadata) {
    if (!is.na(i) | i != "") {
      #insert method parsers here
      if (length(grep(".txt", i)) == 1) {
        #plain text reader
        peaklist_data[[i]] <- readLines(paste("input/", i, sep = ""))
      }
    }
  }
  peaklist_data
}