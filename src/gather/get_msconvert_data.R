get_msconvert_data <- function(mzml) {
  output <- list()
  if ("dataProcessingList" %in% names(mzml$mzML)) {
    output <- lapply(which(names(mzml$mzML$dataProcessingList$dataProcessing) == "processingMethod"), function(y) {
      x <- mzml$mzML$dataProcessingList$dataProcessing[[y]]
      n <- NULL
      u <- NULL
      sw <- NULL
      if ("cvParam" %in% names(x)) {n <- x$cvParam["name"]}
      if ("userParam" %in% names(x)) {
        nam <- NULL
        v <- NULL
        if ("name" %in% names(x$userParam)) {nam <- x$userParam["name"]}
        if ("value" %in% names(x$userParam)) {v <- x$userParam["value"]}
          u <- paste(nam, v, collapse = " ")
      }
      if ("softwareRef" %in% names(x$.attrs)) {
        sw <- x$.attrs["softwareRef"]
      }
      trimws(gsub("  ", " ", paste(n,u,sw, collapse = " ")))
      })
  }
  output
}