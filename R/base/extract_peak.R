extract_peak <- function(mzml, mz, starttime, endtime, ms2exp = "TopN", isowidth = 0, polarity = "ALL") {
  #Extracts peak scans but maintains mzML structure
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  charges <- sapply(scans, getcharge, mzml = mzml)
  if (polarity == "positive") {
    scans <- scans[which(charges == 1)]
  }
  if (polarity == "negative") {
    scans <- scans[which(charges == -1)]
  }
  times <- sapply(scans, gettime, mzml = mzml)
  mslevels <- sapply(scans, getmslevel, mzml = mzml)
  startscan <- which.min(abs(times - starttime))
  endscan <- which.min(abs(times - endtime))
  subscans <- startscan:endscan
  if (ms2exp == "TopN" | ms2exp == "SWATH") {
    if (ms2exp == "SWATH") {isowidth = ceiling(isowidth/2)}
    mslevels <- mslevels[subscans]
    precursors <- sapply(subscans, getprecursor, mzml = mzml)
    ms1scans <- subscans[which(mslevels == 1)]
    ms2scans <- subscans[which((precursors - isowidth) <= mz & (precursors + isowidth) >= mz)]
    peakscans <- sort(c(ms1scans, ms2scans))
  }
  if (ms2exp == "AIF") {
    peakscans <- subscans
  }
 newmzml <- mzml
 newmzml$mzML$run$spectrumList <- mzml$mzML$run$spectrumList[peakscans]
 newmzml
}