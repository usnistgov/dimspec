list_extract_w_metadata <- function(peaklist_data, output_dir = "extract") {
  peaklist <- peaklist_data$PEAKLIST
  rawfiles <- paste(getwd(), "/input/", unique(peaklist$FILENAME), sep = "")
  msconvert <- readLines('config/msconvert_location.txt', warn = FALSE)
  config <- paste(getwd(),"/config/",  "msconvert_config.txt", sep = "")
  outdir <- getwd()
  cols <- colnames(peaklist)
  add_cols <- cols[which(!cols %in% c("ID", "FILENAME", "NAME", "M.Z", "RT", "PEAK_STARTTIME", "PEAK_ENDTIME", "MS2_EXP", "Q1_ISOLATION", "POLARITY", "MS_METHOD", "LC_METHOD", "QC_METHOD"))]
  for (j in 1:length(rawfiles)) {
    mzmlfile = rawfiles[j]
    mzml <- mzMLtoR(mzmlfile)
    subpeaklist <- peaklist[which(peaklist$FILENAME == basename(rawfiles[j])),]
    for (i in 1:nrow(subpeaklist)) {
      peak <- extract_peak(mzml, mz = as.numeric(subpeaklist$M.Z[i]), starttime = as.numeric(subpeaklist$PEAK_STARTTIME[i]), endtime = as.numeric(subpeaklist$PEAK_ENDTIME[i]), ms2exp = subpeaklist$MS2_EXP[i], isowidth = as.numeric(subpeaklist$Q1_ISOLATION[i]), polarity = subpeaklist$POLARITY[i])
      #get metadata
      if (!is.na(subpeaklist$MS_METHOD[i])) {if (subpeaklist$MS_METHOD[i] %in% names(peaklist_data)) {peak$msmethod <- peaklist_data[[which(names(peaklist_data) == subpeaklist$MS_METHOD[i])]]}}
      if (!is.na(subpeaklist$LC_METHOD[i])) {if (subpeaklist$LC_METHOD[i] %in% names(peaklist_data)) {peak$lcmethod <- peaklist_data[[which(names(peaklist_data) == subpeaklist$LC_METHOD[i])]]}}
      if (!is.na(subpeaklist$QC_METHOD[i])) {if (subpeaklist$QC_METHOD[i] %in% names(peaklist_data)) {peak$qcmethod <- peaklist_data[[which(names(peaklist_data) == subpeaklist$QC_METHOD[i])]]}}
      if (paste("ID", subpeaklist$ID[i], sep = "") %in% names(peaklist_data)) {
        peak$ms2_annotation <- peaklist_data[[which(names(peaklist_data) == paste("ID", subpeaklist$ID[i], sep = ""))]]
      }
      peak$peakinfo <- subpeaklist[i,]
      if (length(add_cols) > 0) {
        for (k in 1:length(add_cols)) {
          peak[[add_cols[k]]] <- subpeaklist[i,which(colnames(subpeaklist) == add_cols[k])]
        }
      }
      #check for MS2 Data and save as an RDS object, could be stored in environment.
      scans <- which(names(peak$mzML$run$spectrumList) == "spectrum")
      mslevels <- sapply(scans, getmslevel, mzml = peak)
      if (length(which(mslevels == 2)) > 0) {
        saveRDS(peak, paste("output/", output_dir, "/", gsub("\\.[[:alpha:]]*", "", basename(rawfiles[j])), "_", subpeaklist$DB_IDENTIFIER[i], ".RDS", sep = ""))
        print(paste("Saved file at: output/extract/", gsub("\\.[[:alpha:]]*", "", basename(rawfiles[j])), "_", subpeaklist$DB_IDENTIFIER[i], ".RDS", sep = ""))
      }
      if (length(which(mslevels == 2)) == 0) {
        saveRDS(peak, paste("output/", output_dir, "/", gsub("\\.[[:alpha:]]*", "", basename(rawfiles[j])), "_", subpeaklist$DB_IDENTIFIER[i], "_NoMS2.RDS", sep = ""))
        print(paste("Saved file at: output/extract/", gsub("\\.[[:alpha:]]*", "", basename(rawfiles[j])), "_", subpeaklist$DB_IDENTIFIER[i], ".RDS", sep = ""))
      }
    }
  }
}