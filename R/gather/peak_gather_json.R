#' Extract peak data and metadata
#' 
#' gathers metadata from methodjson and extracts the MS1 and MS2 data from the mzml
#' 
#' @param methodjson list of JSON generated from `parse_method_json` function
#' @param mzml list of msdata from `mzMLtoR` function
#' @param compoundtable data.frame containing compound identities [should be extractable from SQL later]
#' @param zoom numeric vector specifying the range around the precursor ion to include, from m/z - zoom[1] to m/z + zoom[2]
#' @param minerror numeric the minimum error (in Da) of the instrument
#'
#' @return list of peak objects
#' @export
#'
peak_gather_json <- function(methodjson, mzml, compoundtable, zoom = c(1,5), minerror = 0.002) {
  
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
    out[[i]]$compounddata <- compoundtable[which(as.integer(compoundtable$id) == as.integer(out[[i]]$peak$identifier)),]
    ind <- which(annotated_cmpds == methodjson$peaks[[i]]$name)
    if (length(ind) > 0) {
      out[[i]]$annotation <- do.call(rbind, lapply(which(names(methodjson$annotation[[ind]]) == "fragment"), function(x) data.frame(methodjson$annotation[[ind]][[x]])))
      #reorder fragments to formal order
      out[[i]]$annotation$fragment_formula <- sapply(out[[i]]$annotation$fragment_formula, formulalize)
    }
    all_scans <- which(times >= as.numeric(methodjson$peaks[[i]]$peak_starttime) & times <= as.numeric(methodjson$peaks[[i]]$peak_endtime))
    ms1scans <- all_scans[which(mslevels[all_scans] == 1)]
    if (out[[i]]$massspectrometry$ms2exp == "data-independent acquisition (DIA/AIF)" | out[[i]]$massspectrometry$ms2exp == "DIA") {
      ms2scans <- all_scans[which(mslevels[all_scans] == 2)]
    }
    if (out[[i]]$massspectrometry$ms2exp == "data-dependent acquisition (DDA/TopN)" | out[[i]]$massspectrometry$ms2exp == "DDA") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - as.numeric(out[[i]]$massspectrometry$isowidth) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + as.numeric(out[[i]]$massspectrometry$isowidth))]
    }
    if (out[[i]]$massspectrometry$ms2exp == "SWATH") {
      ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(out[[i]]$peak$mz) - (as.numeric(out[[i]]$massspectrometry$isowidth)/2) & precursors[all_scans] <= as.numeric(out[[i]]$peak$mz) + (as.numeric(out[[i]]$massspectrometry$isowidth)/2))]
    }
    if (length(ms2scans) == 0) {stop("There are no MS2 scans that fit within the defined search window.")}
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

#' Extract msconvert metadata
#' 
#' Extracts relevant Proteowizard MSConvert metadata from mzml file.
#' Used for `peak_gather_json` function
#'
#' @param mzml list of msdata from `mzMLtoR` function
#'
#' @return list of msconvert parameters
#' @export
#'
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

#' Tabulate MS Data
#' 
#' Pulls specified MS Data from mzML and converts it into table format for further processing
#' Internal function for `peak_gather_json` function
#'
#' @param mzml list of msdata from `mzMLtoR` function
#' @param scans integer vector containing scan numbers to extract MS data
#' @param mz numeric targeted m/z 
#' @param zoom numeric vector specifying the range around m/z, from m/z - zoom[1] to m/z + zoom[2]
#' @param masserror numeric relative mass error (in ppm) of the instrument
#' @param minerror numeric minimum mass error (in Da) of the instrument
#'
#' @return data.frame containing MS data
#' @export
#'
table_msdata <- function(mzml, scans, mz = NA, zoom = NA, masserror = NA, minerror = NA) {
  if (length(scans) == 0) return(NULL)
  out <- NULL
  if (is.na(zoom[1])) {
    out <- do.call(rbind, lapply(scans, 
                                 function(x) {ms <- extract.ms(mzml, x);
                                 data.frame(scan = x, 
                                            scantime = gettime(mzml, x),
                                            baseion = ms[which.max(ms[,2]),1],
                                            base_int = ms[which.max(ms[,2]),2],
                                            masses = paste(ms[,1], collapse = " "),
                                            intensities = paste(ms[,2], collapse = " "),
                                            ms_n = getmslevel(mzml, x))}))
  }
  if (!is.na(zoom[1]) & !is.na(mz) & !is.na(masserror) & !is.na(minerror)) {
    if (length(zoom) == 2) {
      out <- do.call(rbind, lapply(scans, 
                                   function(x) {ms <- extract.ms(mzml, x);
                                   ms <- ms[which(ms[,1] >= mz - zoom[1] & ms[,1] <= mz + zoom[2]),];
                                   if (length(ms) == 0) {return(NULL)};
                                   data.frame(scan = x, 
                                              scantime = gettime(mzml, x),
                                              baseion = mean(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),1], na.rm = TRUE),
                                              base_int = sum(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),2], na.rm = TRUE),
                                              masses = paste(ms[,1], collapse = " "),
                                              intensities = paste(ms[,2], collapse = " "),
                                              ms_n = getmslevel(mzml, x))}))
    }
  }
  out
}

#' Replace NaN
#' 
#' Replace all NaN values with a specified value
#'
#' @param x vector of values
#' @param repl value to replace NaN contained in `x`
#'
#' @return vector with all NaN replaced with `repl`
#' @export
#'
repl_nan <- function(x, repl = NULL) {
  if (is.nan(x)) {return(repl)}
  if (!is.nan(x)) {return(x)}
}