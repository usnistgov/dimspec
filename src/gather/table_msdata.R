table_msdata <- function(mzml, scans, mz = NA, zoom = NA, masserror = NA, minerror = NA) {
  if (length(scans) == 0) return(NULL)
  out <- NULL
  if (is.na(zoom[1])) {
    out <- do.call(rbind, lapply(scans, 
                          function(x) {ms <- extract.ms(mzml, x);
                                        data.frame(scan = x, 
                                                 scantime = gettime(mzml, x),
                                                 baseion = ms[which.max(ms[,2]),1],
                                                 baseint = ms[which.max(ms[,2]),2],
                                                 masses = paste(ms[,1], collapse = " "),
                                                 intensities = paste(ms[,2], collapse = " "))}))
  }
  if (!is.na(zoom[1]) & !is.na(mz) & !is.na(masserror) & !is.na(minerror)) {
    if (length(zoom) == 2) {
      out <- do.call(rbind, lapply(scans, 
                            function(x) {ms <- extract.ms(mzml, x);
                            ms <- ms[which(ms[,1] >= mz - zoom[1] & ms[,1] <= mz + zoom[2]),];
                            if (length(ms) == 0) {return(NULL)};
                            data.frame(scan = x, 
                                       scantime = gettime(mzml, x),
                                       baseion = repl_nan(mean(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),1], na.rm = TRUE), repl = 0),
                                       baseint = sum(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),2], na.rm = TRUE),
                                       masses = paste(ms[,1], collapse = " "),
                                       intensities = paste(ms[,2], collapse = " "))}))
    }
  }
  out
}

repl_nan <- function(x, repl ="NULL") {
  if (is.nan(x)) {return(repl)}
  if (!is.nan(x)) {return(x)}
}
  