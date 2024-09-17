#' Unzip binary data into vector
#'
#' @param x String of binary data to convert
#' @param type type of compression (see `base::memDecompress`). Default is `gzip`
#'
#' @return vector containing data from converted binary data
#' @export
#'
unzip <- function(x, type = "gzip") {
  require(base64enc)
  if (is.null(x)) {return(0)}
  x <- base64decode(x)
  if (type != "none") {x <- memDecompress(x, type = type)}
  readBin(x, what = "double", n = length(x)%/%8, size = 8)
}




#' Opens file of type mzML into R environment
#'
#' @param mzmlfile the file path of the mzML file which the data are to be read from.
#' @param lockmass NUM scalar m/z value of the lockmass to remove (Waters instruments only) (default: NULL)
#' @param lockmasswidth NUM scalar instrumental uncertainty associated with `lockmass` (default: NULL)
#' @param correct logical if the subsequent spectra should be corrected for the lockmass (Waters instruments only)
#' @param approach character string defining the type of lockmass removal filter to use, default is `hybrid`
#'
#' @return list containing mzML data with unzipped masses and intensity information
#' @export
#'
mzMLtoR <- function(mzmlfile = file.choose(), lockmass = NULL, lockmasswidth = NULL, correct = FALSE, approach = "hybrid") {
  require(XML)
  require(base64enc)
  mzml <- xmlToList(mzmlfile)
  # Waters lockmass logic
  if (!is.null(lockmass) & "MassLynx" %in% unlist(mzml$mzML$softwareList)) {
    if (!is.double(lockmass)) stop("bad lockmass for Waters data")
    if (!is.double(lockmass)) stop("bad lockmass width for Waters data")
    if (!is.logical(correct)) stop("bad correction TRUE/FALSE for Waters data")
    mzml <- lockmass_remove(mzml, lockmass, lockmasswidth, correct, approach = approach)
  }
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  compression <- "none"
  if ("zlib compression" %in% unlist(mzml$mzML$run$spectrumList[[1]]$binaryDataArrayList[["binaryDataArray"]])) {compression = "gzip"}
  output <- lapply(scans, function(x) {ind <- which(names(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList) == "binaryDataArray"); if ("m/z array" %in% unlist(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])) {ind <- rev(ind)}; list(masses = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[1]]], intensities = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])})
  output <- lapply(output, function(x) list(masses = unzip(x$masses$binary, type = compression), intensities = unzip(x$intensities$binary, type = compression)))
  for (x in 1:length(scans)) {
    mzml$mzML$run$spectrumList[[scans[x]]]$masses <- output[[x]]$masses
    mzml$mzML$run$spectrumList[[scans[x]]]$intensities <- output[[x]]$intensities
  }
  mzml
}

#' Remove lockmass scan from mzml object
#' 
#' For Waters instruments only, identifies the scans that are due to a lock mass scan
#' and removes them for easier processing.
#'
#' @param mzml mzML object generated from mzMLtoR() function
#' @param lockmass m/z value of the lockmass to remove
#' @param lockmasswidth m/z value for the half-window of the lockmass scan
#' @param correct logical if the subsequent spectra should be corrected
#'
#' @return A copy of the object provided to `mzml` with the lock mass removed.
#' @export
#'

lockmass_remove <- function(mzml, lockmass = NULL, lockmasswidth = NULL, correct = FALSE, approach = "baseion") {
  lockmasserror <- NA
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  baseions <- sapply(scans, getbaseion, mzml = mzml)
  scanfn <- sapply(scans, getwatersfunction, mzml = mzml)
  mslevels <- sapply(scans, getmslevel, mzml = mzml)
  if (approach == "baseion") {
    # this approach uses the mzML base ion parameter and pulls out scans whose base ion
    # is within the lockmass error. Seems to break with Waters data.
    lockmass_scans <- which(baseions >= (lockmass - lockmasswidth) & baseions <= (lockmass + lockmasswidth))
    lockmasserror <- 1E6*(mean(baseions[lockmass_scans]) - lockmass)/lockmass
  }
  if (approach == "empbaseion") {
    # this approach looks at the empirical base ion, not the mzML parameter
    ms1scans <- scans[which(mslevels == 1)]
    compression <- "none"
    if ("zlib compression" %in% unlist(mzml$mzML$run$spectrumList[[1]]$binaryDataArrayList[["binaryDataArray"]])) {compression = "gzip"}
    output <- lapply(ms1scans, function(x) {ind <- which(names(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList) == "binaryDataArray"); if ("m/z array" %in% unlist(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])) {ind <- rev(ind)}; list(masses = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[1]]], intensities = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])})
    output <- lapply(output, function(x) data.frame(masses = unzip(x$masses$binary, type = compression), intensities = unzip(x$intensities$binary, type = compression)))
    baseions <- sapply(1:length(output), function(x) output[[x]]$masses[which.max(output[[x]]$intensities)])
    lockmass_scans <- ms1scans[which(baseions >= (lockmass - lockmasswidth) & baseions <= (lockmass + lockmasswidth))]
    lockmasserror <- 1E6*(mean(baseions[lockmass_scans]) - lockmass)/lockmass
  }
  if (approach == "ms1freq") {
    # this approach determines the two MS1 scan functions and selects the less frequent scanfn, 
    # assuming the lockmass is a less frequent scan than the survey MS1
    ms1_scanfn <- scanfn[which(mslevels == 1)]
    if (length(unique(ms1_scanfn)) != 2) {stop("there are not only two MS1 scans, this lockmass filter will not work.")}
    lockmass_scanfn <- unique(ms1_scanfn)[which.min(tabulate(match(ms1_scanfn, unique(ms1_scanfn))))]
    lockmass_scans <- scans[which(scanfn == lockmass_scanfn)]
  }
  if (approach == "hybrid") {
    # hybrid approach that takes the empirical base ion and the frequency approach for the scan functions and 
    # selects the most common scan function related to a base ion of the lockmass.
    
    ms1scans <- scans[which(mslevels == 1)]
    ms1_scanfn <- scanfn[which(mslevels == 1)]
    if (length(unique(ms1_scanfn)) != 2) {stop("there are not only two MS1 scans, this lockmass filter will not work.")}
    compression <- "none"
    if ("zlib compression" %in% unlist(mzml$mzML$run$spectrumList[[1]]$binaryDataArrayList[["binaryDataArray"]])) {compression = "gzip"}
    output <- lapply(ms1scans, function(x) {ind <- which(names(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList) == "binaryDataArray"); if ("m/z array" %in% unlist(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])) {ind <- rev(ind)}; list(masses = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[1]]], intensities = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])})
    output <- lapply(output, function(x) data.frame(masses = unzip(x$masses$binary, type = compression), intensities = unzip(x$intensities$binary, type = compression)))
    baseions <- sapply(1:length(output), function(x) output[[x]]$masses[which.max(output[[x]]$intensities)])
    lockmass_scanfn <- ms1_scanfn[which(baseions >= (lockmass - lockmasswidth) & baseions <= (lockmass + lockmasswidth))]
    lockmass_scanfn_val <- unique(lockmass_scanfn)[which.max(tabulate(match(lockmass_scanfn, unique(lockmass_scanfn))))]
    
    lockmass_scans <- scans[which(scanfn == lockmass_scanfn_val)]
    lockmasserror <- 1E6*(mean(baseions[lockmass_scans]) - lockmass)/lockmass
    
  }
  if (correct) {
    # placeholder for correction function
  }
  if (length(lockmass_scans) > 0) {
    mzml$mzML$run$spectrumList <- mzml$mzML$run$spectrumList[-lockmass_scans]
  }
  mzml$lockmass <- list(lockmass = lockmass, lockmasswidth = lockmasswidth, correct = correct, lockmasserror = lockmasserror)
  mzml
}

getwatersfunction <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan) == "cvParam"), function(x) {if(!"preset scan configuration" %in% mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]][which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) == "value")]})))
}

getionint <- function(mzml, i, minmass, maxmass) {
  sum(mzml$mzML$run$spectrumList[[i]]$intensities[which(mzml$mzML$run$spectrumList[[i]]$masses >= minmass & mzml$mzML$run$spectrumList[[i]]$masses <= maxmass)])
}

getTIC <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {if(!"total ion current" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]][[x]][which(names(mzml$mzML$run$spectrumList[[i]][[x]]) == "value")]})))
}

getBIC <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {if(!"base peak intensity" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]][[x]][which(names(mzml$mzML$run$spectrumList[[i]][[x]]) == "value")]})))
}

getbaseion <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {if(!"base peak m/z" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]][[x]][which(names(mzml$mzML$run$spectrumList[[i]][[x]]) == "value")]})))
}

#' Get time of a ms scan within mzML object
#'
#' @param mzml list mzML object generated from `mzMLtoR` function
#' @param i integer scan number
#'
#' @return numeric of the scan time
#' @export
#'
gettime <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan) == "cvParam"), function(x) {if(!"scan start time" %in% mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]][which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) == "value")]})))
}

#' Get MS Level of a ms scan within mzML object
#'
#' @param mzml list mzML object generated from `mzMLtoR` function
#' @param i integer scan number
#'
#' @return integer representing the MS Level (1, 2, ... n)
#' @export
#'
getmslevel <- function(mzml, i) {
  as.integer(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {if(!"ms level" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]][[x]][which(names(mzml$mzML$run$spectrumList[[i]][[x]]) == "value")]})))
}

#' Get precursor ion of a ms scan within mzML object
#'
#' @param mzml list mzML object generated from `mzMLtoR` function
#' @param i integer scan number
#'
#' @return numeric designating the precursor ion (or middle of the scan range for SWATCH or DIA), returns NULL if no precursor was selected
#' @export
#'
getprecursor <- function(mzml, i) {
  if (!"precursorList" %in% names(mzml$mzML$run$spectrumList[[i]])) {return(0)}
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon) == "cvParam"), function(x) {if(!"selected ion m/z" %in% mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]][which(names(mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]]) == "value")]})))
}

#' Get polarity of a ms scan within mzML object
#'
#' @param mzml list mzML object generated from `mzMLtoR` function
#' @param i integer scan number
#'
#' @return integer representing scan polarity (either 1 (positive) or -1 (negative))
#' @export
#'
getcharge <- function(mzml, i) {
  do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {o <- NULL; if("positive scan" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {o <- 1}; if("negative scan" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {o <- -1}; o}))
}

TIC <- function(mzml, MS1 = TRUE, charge = "ALL") {
  # creates a total ion chromatogram object with atomic properties of $intensity and $time (used for plot.chrom)
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  if (MS1 == TRUE & charge == "ALL") {
    precursors <- sapply(scans, getprecursor, mzml = mzml)
    scans <- scans[which(precursors == 0)]
  }
  if (MS1 == TRUE & charge != "ALL") {
    precursors <- sapply(scans, getprecursor, mzml = mzml)
    charges <- sapply(scans, getcharge, mzml = mzml)
    scans <- scans[which(precursors == 0 & charges == charge)]
  }
  if (MS1 == FALSE & charge != "ALL") {
    charges <- sapply(scans, getcharge, mzml = mzml)
    scans <- scans[which(charges == charge)]
  }
  intensities <- sapply(scans, getTIC, mzml = mzml)
  times <- sapply(scans, gettime, mzml = mzml)
  results <- c()
  results$intensity <- intensities
  results$time <- times
  results
}

BIC <- function(mzml, MS1 = TRUE, charge = "ALL") {
  # creates a base ion pair chromatogram object with atomic properties of $intensity and $time (used for plot.chrom)
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  if (MS1 == TRUE & charge == "ALL") {
    precursors <- sapply(scans, getprecursor, mzml = mzml)
    scans <- scans[which(precursors == 0)]
  }
  if (MS1 == TRUE & charge != "ALL") {
    precursors <- sapply(scans, getprecursor, mzml = mzml)
    charges <- sapply(scans, getcharge, mzml = mzml)
    scans <- scans[which(precursors == 0 & charges == charge)]
  }
  if (MS1 == FALSE & charge != "ALL") {
    charges <- sapply(scans, getcharge, mzml = mzml)
    scans <- scans[which(charges == charge)]
  }
  intensities <- sapply(scans, getBIC, mzml = mzml)
  times <- sapply(scans, gettime, mzml = mzml)
  results <- c()
  results$intensity <- intensities
  results$time <- times
  results
}

EIC <- function(mzml, mass, error, minerror = 0.001, MS1 = TRUE, charge = "ALL") {
  # creates a extracted ion chromatogram object (with error in ppm) with atomic properties of $intensity and $time (used for plot.chrom)
  mass.diff <- max(error*mass/(10^6), minerror)
  minmass <- mass - mass.diff
  maxmass <- mass + mass.diff
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  precursors <- sapply(scans, getprecursor, mzml = mzml)
  charges <- sapply(scans, getcharge, mzml = mzml)
  if (MS1 == TRUE & charge == "ALL") {scans <- scans[which(precursors == 0)]}
  if (MS1 == TRUE & charge != "ALL") {scans <- scans[which(precursors == 0 & charges == charge)]}
  if (MS1 == FALSE & charge != "ALL") {scans <- scans[which(charges == charge)]}
  intensities <- sapply(scans, getionint, mzml = mzml, minmass = minmass, maxmass = maxmass)
  times <- sapply(scans, gettime, mzml = mzml)
  results <- c()
  results$intensity <- intensities
  results$time <- times
  results
}

plot.chrom <- function(chrom, timereduce = 1) {
  # plots chromatogram objects, if no $intensity and $time property, will just plot simple vector, timereduce is the denominator for the time values
  timetest <- NULL
  try(timetest <- length(chrom$time), silent = TRUE)
  if (!is.null(timetest)) {
    plot(x = chrom$time/timereduce, y = chrom$intensity, type = "l", xlab = "Time", ylab = "Intensity")
  }
  else {
    plot(chrom, type = "l", xlab = "Scans", ylab = "Intensity")
  }
}

peak.desc <- function(x, peak, width = 1, slope = 0.00044) {
  # Internal function used to determine the peak width in a single dimension
  rx <- peak
  lx <- peak
  rightwidth <- peak
  leftwidth <- peak
  rightslope = -slope - 1
  leftslope = -slope - 1
  while (rightslope <= -slope & rx <= length(x)) {
    rightslope <- slopeest(x, rx:min(c(rx + width, length(x))))
    if (is.na(rightslope)) {rightslope = -slope}
    rightwidth <- rx
    rx <- rx + 1
  }
  while (leftslope <= -slope & lx > 0) {
    leftslope <- slopeest(x, lx:max(c(rx-width, 1)))
    if (is.na(leftslope)) {leftslope = -slope}
    leftwidth <- lx
    lx <- lx - 1
  }
  c(leftwidth, rightwidth)
}

slopeest <- function(x, points) {
  xs <- 1:length(points)
  ys <- x[points]
  sum((xs-mean(xs))*(ys-mean(ys)))/sum((xs-mean(xs))^2)
}

peak.width <- function(chrom, peak, width = 100, slope = 0.0001, level = 0.1) {
  # internal function to determine the peak width at a specific level (i.e., level = 0.1 means peak width at 10% height)
  widths.init <- peak.desc(chrom, peak, width = width, slope = slope)
  leftmin <- (chrom[peak] - chrom[widths.init[1]])*level
  rightmin <- (chrom[peak] - chrom[widths.init[1]])*level
  leftwidth <- which(chrom[widths.init[1]:peak] > leftmin)
  leftwidth <- leftwidth[1] + widths.init[1] - 1
  rightwidth <- which(chrom[widths.init[2]:peak] > leftmin)
  rightwidth <- widths.init[2] - rightwidth[1] + 1
  widths <- c(leftwidth, rightwidth)
  widths
}

extract.ms <- function(mzml, scans, mz.round = 4) {
  # creates mass spectrum object for list of scan numbers (not times)
  full.ms <- NULL
  for (j in scans) {
    x.ms <- cbind(mzml$mzML$run$spectrumList[[j]]$masses, mzml$mzML$run$spectrumList[[j]]$intensities)
    if (length(x.ms) > 0) {
      x.ms <- matrix(x.ms, ncol = 2)
      if (x.ms[1,1] != 0 & x.ms[1,2] != 0) {
        x.ms[,1] <- round(x.ms[,1], digits = mz.round)
        x.ms <- cbind(x.ms[,1], ave(x.ms[,2], x.ms[,1], FUN = sum))
        x.ms <- x.ms[!duplicated(x.ms),]
        x.ms <- matrix(x.ms, ncol = 2)
        if (j == scans[1]) {
          full.ms <- x.ms
        }
        else {
          int.ms <- merge(full.ms, x.ms, by = 1, all = TRUE)
          full.ms <- cbind(int.ms[,1], rowSums(int.ms[,2:3], na.rm = TRUE))
        }
      }
    }
  }
  if (x.ms[1,1] == 0 & x.ms[1,2] == 0) {return(data.frame(mz = 0, int = 0))}
  full.ms[,1] <- round(full.ms[,1], digits = mz.round)
  full.ms <- cbind(full.ms[,1], ave(full.ms[,2], full.ms[,1], FUN = sum))
  full.ms <- matrix(full.ms[!duplicated(full.ms),], ncol = 2)
  full.ms <- data.frame(mz = full.ms[,1], int = full.ms[,2])
  full.ms
}

write.spectrum <- function(ms, file, precursor = "", name = "R-exported Spectrum") {
  # saves mass spectrum as a NIST MS Search-readable file with precursor ion designated
  
  peaks <- nrow(ms)
  names <- paste("Name: ", name) 
  precursor <- paste("PrecursorMZ: ", precursor)
  peaks <- paste("Num Peaks: ", peaks)
  header <- rbind(names, precursor, peaks)
  write.table(header, file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
  if (!is.na(ms[1])) {
    write.table(ms, file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  if (is.na(ms[1])) {
    write.table(cbind(0,0), file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  write("\n\n", file, append = TRUE)
}

getprocessingmethods <- function(mzml) {
  scans <- which(names(mzml$mzML$dataProcessingList$dataProcessing) == "processingMethod")
  if (length(scans) == 0) {return(NULL)}
  do.call(rbind, lapply(scans, function(x) {
    if ("cvParam" %in% names(mzml$mzML$dataProcessingList$dataProcessing[[x]])) {
      output <- c(mzml$mzML$dataProcessingList$dataProcessing[[x]]$cvParam["name"], mzml$mzML$dataProcessingList$dataProcessing[[x]]$cvParam["value"], mzml$mzML$dataProcessingList$dataProcessing[[x]]$.attrs["softwareRef"])
    };
    if ("userParam" %in% names(mzml$mzML$dataProcessingList$dataProcessing[[x]])) {
      output <- c(mzml$mzML$dataProcessingList$dataProcessing[[x]]$userParam["name"], mzml$mzML$dataProcessingList$dataProcessing[[x]]$userParam["value"], mzml$mzML$dataProcessingList$dataProcessing[[x]]$.attrs["softwareRef"])
    };
    output
  }))
  
}

zipms <- function(ms, zip = "gzip") {
  ms <- c(t(ms))
  ms <- writeBin(ms, raw(8), endian = "little")
  if (zip != "none") {
    ms <- memCompress(ms, type = zip)
  }
  base64encode(ms)
}

# Added to explicitly extract collision energies from a given mzML given a scan index "i"
getce <- function(mzml, i) {
  tmp <- mzml$mzML$run$spectrumlist[[i]]
  if (!"precursorList" %in% names(tmp)) {return(NA)}
  if (!"activation" %in% names(tmp$precursorList$precursor)) {return(NA)}
  tmp <- tmp$precursorList$precursor
  as.numeric(do.call(c, lapply(which(names(tmp$activation) == "cvParam"), function(x) {if(!"collision energy" %in% tmp$activation[[x]]) {return(NA)}; tmp$activation[[x]][which(names(activation[[x]]) == "value")]})))
}

# Get ALL cvParam values from an mzML file
# By default, this does chuck out some trivialities that we're already extracting elsewhere...change `exclude` to include those if you want, but this is not tested.
# Only values in the `spectrumList` are extracted.
get_cvParams <- function(mzml, exclude = c("binaryDataArrayList", "masses", "intensities")) {
  if ("mzML" %in% names(mzml)) {
    tmp <- mzml$mzML$run$spectrumList 
  } else {
    tmp <- mzml
  }
  spc_idx <- which(names(tmp) == "spectrum")
  if (length(exclude) > 0) {
    flat_tmp <- lapply(tmp[spc_idx], \(x) x[-which(names(x) %in% exclude)])
  }
  for (i in 1:length(flat_tmp)) {
    while(any(sapply(flat_tmp[[i]], is.list))) {
      flat_tmp[[i]] <- purrr::list_flatten(flat_tmp[[i]])
    }
  }
  flat_tmp <- flat_tmp |>
    lapply(\(x) {
      x <- bind_rows(x)
      if (!"spectrumRef" %in% names(x)) x$spectrumRef = NA_character_
      x |>
        fill(id, .direction = "downup") |>
        fill(spectrumRef, .direction = "downup") |>
        mutate(
          scanIndex = as.integer(stringr::str_extract(stringr::str_extract(id, "scan=[0-9]+"), "[0-9]+")),
          scanIndexParent = as.integer(stringr::str_extract(stringr::str_extract(spectrumRef, "scan=[0-9]+"), "[0-9]+")),
          .before = everything()
        ) |>
        select(-(count:dataProcessingRef)) |>
        filter(!is.na(cvRef))
    }
    )
  tmp <- flat_tmp |>
    bind_rows() |>
    select(any_of(c("scanIndex", "scanIndexParent", "name", "value", "unitName"))) |>
    pivot_wider(id_cols = c(scanIndex, scanIndexParent), names_from = name, values_from = value)
  names(tmp) <- names(tmp) |>
    stringr::str_replace_all("[ \\-]", "_") |>
    stringr::str_remove_all("/")
  tmp <- tmp |>
    mutate(
      across(everything(), ~ifelse(.x == "", NA_character_, .x)),
      across(matches("ms_level|charge_state"), as.integer),
      across(matches("mz$|intensity$|current$|energy$|offset$|time$|limit$|power$"), as.double)
    ) |>
    relocate(collision_energy, .after = scanIndexParent)
  return(tmp)
}
