peak_gather <- function(methodxml, mzml) {
  out <- list()
  annotated_cmpds <- sapply(methodxml$annotation, function(x) x$name)
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  charges <- sapply(scans, getcharge, mzml = mzml)
  if (methodxml$method$massspectrometry$polarity == "positive") {charge = 1}
  if (methodxml$method$massspectrometry$polarity == "negative") {charge = -1}
  scans <- scans[which(charges == charge)]
  times <- sapply(scans, gettime, mzml = mzml)
  mslevels <- sapply(scans, getmslevel, mzml = mzml)
  precursors <- sapply(scans, getprecursor, mzml = mzml)
  for (i in 1:length(methodxml$peaks)) {
    out[[i]] <- list()
    out[[i]]$method <- methodxml$method
    out[[i]]$peak <- data.frame(methodxml$peaks[[i]])
    ind <- which(annotated_cmpds == methodxml$peaks[[i]]$name)
    if (length(ind) > 0) {
      out[[i]]$annotation <- do.call(rbind, lapply(which(names(methodxml$annotation[[i]]) == "fragment"), function(x) data.frame(methodxml$annotation[[i]][[x]])))
    }
    all_scans <- which(times >= as.numeric(methodxml$peaks[[i]]$peak_starttime) & times <= as.numeric(methodxml$peaks[[i]]$peak_endtime))
    ms1scans <- all_scans[which(mslevels[all_scans] == 1)]
    if (out[[i]]$method$massspectrometry$ms2exp == "DIA") {
      ms2scans <- all_scans[which(mslevels[all_scans] == 2)]
    }
    if (out[[i]]$method$massspectrometry$ms2exp == "DDA") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - as.numeric(out[[i]]$method$massspectrometry$isowidth) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + as.numeric(out[[i]]$method$massspectrometry$isowidth))]
    }
    if (out[[i]]$method$massspectrometry$ms2exp == "SWATH") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - (as.numeric(out[[i]]$method$massspectrometry$isowidth)/2) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + (as.numeric(out[[i]]$method$massspectrometry$isowidth)/2))]
    }
    out[[i]]$ms1data <- table_msdata(mzml, ms1scans)
    out[[i]]$ms2data <- table_msdata(mzml, ms2scans)
  }
  out
}