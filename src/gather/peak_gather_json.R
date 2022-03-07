source('src/gather/table_msdata.R')

peak_gather_json <- function(methodjson, mzml, compoundtable, zoom = c(1,5), minerror = 0.002) {
  #gathers metadata from methodjson and extracts the MS1 and MS2 data from the mzml
  #crosslists the compound ID's and extract compound data for QC checking
  out <- list()
  annotated_cmpds <- sapply(methodjson$annotation, function(x) x$name)
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  charges <- sapply(scans, getcharge, mzml = mzml)
  if (methodjson$massspectrometry$polarity == "positive") {charge = 1}
  if (methodjson$massspectrometry$polarity == "negative") {charge = -1}
  scans <- scans[which(charges == charge)]
  times <- sapply(scans, gettime, mzml = mzml)
  mslevels <- sapply(scans, getmslevel, mzml = mzml)
  precursors <- sapply(scans, getprecursor, mzml = mzml)
  for (i in 1:length(methodjson$peaks)) {
    out[[i]] <- list()
    out[[i]]$msconvertsettings <- get_msconvert_data(mzml)
    out[[i]]$sample <- methodjson$sample
    out[[i]]$sample$starttime <- mzml$mzML$run$.attrs["startTimeStamp"]
    out[[i]]$chromatography <- methodjson$chromatography
    out[[i]]$massspectrometry <- methodjson$massspectrometry
    out[[i]]$qcmethod <- methodjson$qcmethod
    out[[i]]$peak <- data.frame(methodjson$peaks[[i]])
    out[[i]]$compounddata <- compoundtable[which(as.integer(compoundtable$ID) == as.integer(out[[i]]$peak$identifier)),]
    ind <- which(annotated_cmpds == methodjson$peaks[[i]]$name)
    if (length(ind) > 0) {
      out[[i]]$annotation <- do.call(rbind, lapply(which(names(methodjson$annotation[[ind]]) == "fragment"), function(x) data.frame(methodjson$annotation[[ind]][[x]])))
    }
    all_scans <- which(times >= as.numeric(methodjson$peaks[[i]]$peak_starttime) & times <= as.numeric(methodjson$peaks[[i]]$peak_endtime))
    ms1scans <- all_scans[which(mslevels[all_scans] == 1)]
    if (out[[i]]$massspectrometry$ms2exp == "data-independent acquisition (DIA/AIF)") {
      ms2scans <- all_scans[which(mslevels[all_scans] == 2)]
    }
    if (out[[i]]$massspectrometry$ms2exp == "data-dependent acquisition (DDA/TopN)") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - as.numeric(out[[i]]$massspectrometry$isowidth) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + as.numeric(out[[i]]$massspectrometry$isowidth))]
    }
    if (out[[i]]$massspectrometry$ms2exp == "SWATH") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - (as.numeric(out[[i]]$massspectrometry$isowidth)/2) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + (as.numeric(out[[i]]$massspectrometry$isowidth)/2))]
    }
    ms1data <- table_msdata(mzml, ms1scans, mz = as.numeric(out[[i]]$peak$mz), zoom = zoom, masserror = as.numeric(out[[i]]$massspectrometry$msaccuracy), minerror = minerror)
    ms2data <- table_msdata(mzml, ms2scans)
    ms1data <- cbind(mslevel = rep(1, nrow(ms1data)), ms1data)
    ms2data <- cbind(mslevel = rep(2, nrow(ms2data)), ms2data)
    msdata <- rbind(ms1data, ms2data)
    msdata <- msdata[order(msdata$scantime),]
    out[[i]]$msdata <- do.call(rbind, lapply(1:nrow(msdata), function(x) data.frame(scantime = msdata$scantime[x], ms_n = msdata$mslevel[x], baseion = msdata$baseion[x], base_int = msdata$base_int[x], measured_mz = msdata$masses[x], measured_intensity = msdata$intensities[x])))
  }
  out
}