table_msdata <- function(mzml, scans) {
  if (length(scans) == 0) return(NULL)
  do.call(rbind, lapply(scans, function(x) data.frame(scan = x, scantime = gettime(mzml, x), msdata = zipms(extract.ms(mzml, x))  )))
}