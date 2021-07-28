require(XML)
require(base64enc)

unzip <- function(x, type = "gzip") {
  #converts string via base encoding, compression, and then binary conversion
  if (is.null(x)) {return(0)}
  x <- base64decode(x)
  if (type != "none") {x <- memDecompress(x, type = type)}
  readBin(x, what = "double", n = length(x)%/%8, size = 8)
}

mzMLtoR <- function(mzmlfile = file.choose()) {
  #maintains XML structure but adds decoded/unzipped $masses and $intensity information to R Object
  mzml <- xmlToList(mzmlfile)
  scans <- which(names(mzml$mzML$run$spectrumList) == "spectrum")
  compression <- "none"
  if ("zlib compression" %in% unlist(mzml$mzML$run$spectrumList[[1]]$binaryDataArrayList[["binaryDataArray"]])) {compression = "gzip"}
  output <- lapply(scans, function(x) {ind <- which(names(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList) == "binaryDataArray"); if ("m/z array" %in% unlist(mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])) {ind <- rev(ind)}; list(masses = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[1]]], intensities = mzml$mzML$run$spectrumList[[x]]$binaryDataArrayList[[ind[2]]])})
  output <- lapply(output, function(x) list(masses = unzip(x$masses$binary), intensities = unzip(x$intensities$binary)))
  for (x in 1:length(scans)) {
    mzml$mzML$run$spectrumList[[scans[x]]]$masses <- output[[x]]$masses
    mzml$mzML$run$spectrumList[[scans[x]]]$intensities <- output[[x]]$intensities
  }
  mzml
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

gettime <- function(mzml, i) {
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan) == "cvParam"), function(x) {if(!"scan start time" %in% mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]][which(names(mzml$mzML$run$spectrumList[[i]]$scanList$scan[[x]]) == "value")]})))
}

getmslevel <- function(mzml, i) {
  as.integer(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]) == "cvParam"), function(x) {if(!"ms level" %in% mzml$mzML$run$spectrumList[[i]][[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]][[x]][which(names(mzml$mzML$run$spectrumList[[i]][[x]]) == "value")]})))
}

getprecursor <- function(mzml, i) {
  if (!"precursorList" %in% names(mzml$mzML$run$spectrumList[[i]])) {return(0)}
  as.numeric(do.call(c, lapply(which(names(mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon) == "cvParam"), function(x) {if(!"selected ion m/z" %in% mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]]) {return(NULL)}; mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]][which(names(mzml$mzML$run$spectrumList[[i]]$precursorList$precursor$selectedIonList$selectedIon[[x]]) == "value")]})))
}

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

plot_ms <- function(ms, xlim = NULL, ylim = NULL, main = "Mass Spectrum", color = "black", size = 1, removal = 0) {
  ms <- ms[which(ms[,3] >= removal),]
  int <- ms[,3]
  intu <- ms[,4]
  mz <- ms[,1]
  mzu <- ms[,2]
  ggplot(data.frame(mz = mz, int = int)) + geom_linerange(aes(x = mz, ymin = 0, ymax = int), color = color, size = size) + geom_pointrange(aes(x = mz, ymin = 0, ymax = int, y= int), shape = 20, size = 0.5) + geom_errorbar(aes(x = mz, ymin = int - intu, ymax = int + intu, width = 0.01), color = "red", na.rm = TRUE, linetype = 5) + geom_errorbarh(aes(y = int, xmin = mz - mzu, xmax = mz + mzu, height = 0.01), color = "red", na.rm = TRUE, linetype = 5) + ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)+ theme_bw() 
}

extract.ms <- function(mzml, scans, mz.round = 4) {
  # creates mass spectrum object for list of scan numbers (not times)
  full.ms <- NULL
  for (j in scans) {
    x.ms <- cbind(mzml$mzML$run$spectrumList[[j]]$masses, mzml$mzML$run$spectrumList[[j]]$intensities)
    x.ms[,1] <- round(x.ms[,1], digits = mz.round)
    x.ms <- cbind(x.ms[,1], ave(x.ms[,2], x.ms[,1], FUN = sum))
    x.ms <- x.ms[!duplicated(x.ms),]
    x.ms
    if (j == scans[1]) {
      full.ms <- x.ms
    }
    else {
      int.ms <- merge(full.ms, x.ms, by = 1, all = TRUE)
      full.ms <- cbind(int.ms[,1], rowSums(int.ms[,2:3], na.rm = TRUE))
    }
  }
  full.ms[,1] <- round(full.ms[,1], digits = mz.round)
  full.ms <- cbind(full.ms[,1], ave(full.ms[,2], full.ms[,1], FUN = sum))
  full.ms <- full.ms[!duplicated(full.ms),]
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