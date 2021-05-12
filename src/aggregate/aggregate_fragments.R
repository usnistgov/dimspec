fragments <- read.csv('src/aggregate/fragment_table.csv', row.names = NULL)

aggregate_fragments <- function(peaklist, fragments, masserror, minerror, correl = NULL, ph = NULL, freq = NULL) {
  #for each compound in the peak list, the MS2 data is extract and m/z values that overlap with the fragment table are added as annotated fragments. The results are saved as a single Excel file
  mzmls <- unique(peaklist$FILENAME)
  rawfiles <- list.files("input")
  wb <- list()
  wb[["PEAKLIST"]] <- peaklist
  for (i in 1:length(mzmls)) {
    if (mzmls[i] %in% rawfiles) {
      mzml <- mzMLtoR(paste("input/", mzmls[i], sep = ""))
      subpeaklist <- peaklist[which(peaklist$FILENAME == mzmls[i]),]
      for (j in 1:nrow(subpeaklist)) {
        peak <- extract_peak(mzml, subpeaklist$M.Z[j], subpeaklist$PEAK_STARTTIME[j], subpeaklist$PEAK_ENDTIME[j], subpeaklist$MS2_EXP[j], subpeaklist$Q1_ISOLATION[j], subpeaklist$POLARITY[j])
        scans <- which(names(peak$mzML$run$spectrumList) == "spectrum")
        mslevels <- sapply(scans, getmslevel, mzml = peak)
        if (length(which(mslevels == 2)) > 0) {
          peaktable <- ms2_peaktable(peak, mass = subpeaklist$M.Z[j], masserror, minerror)
          cms2 <- get_cms2(peaktable, correl, ph, freq, normfn = "sum", cormethod = "pearson")
          ftable <- get_fragments(cms2, fragments, polarity = "negative")
          if (length(ftable) > 0) {
            wb[[paste("ID", subpeaklist$ID[j], sep = "")]] <- ftable
          }
        }
      }
    }
  }
  wb
}