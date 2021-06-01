table_msdata <- function(mzml, scans) {
  do.call(rbind, lapply(scans, function(x) data.frame(scan = x, scantime = gettime(mzml, x), msdata = zipms(extract.ms(mzml, x))  )))
}