ms2_peaktable <- function(peak, mass, masserror = 5, minerror = 0.002, int0 = NA) {
  #creates MS2 data tables from a peak object including the mass and intensity (separate tables) into a peak table object
  scans <- which(names(peak$mzML$run$spectrumList) == "spectrum")
  mslevels <- sapply(scans, getmslevel, mzml = peak)
  peak.EIC <- EIC(peak, mass, masserror, minerror, MS1 = TRUE)
  ms1scans <- scans[which(mslevels == 1)]
  ms2scans <- scans[which(mslevels == 2)]
  ms1scans_peak <- ms2scans - 1
  ms1scans_ind <- which(ms1scans %in% ms1scans_peak)
  peakindex <- which(ms2scans == ms1scans_peak[which.max(peak.EIC$intensity[ms1scans_ind])]+1)
  mslist <- lapply(ms2scans, function(x) cbind(peak$mzML$run$spectrumList[[x]]$masses, peak$mzML$run$spectrumList[[x]]$intensities))
  merged <- mergems(mslist, peakindex, masserror = masserror, minerror = minerror)
  peaktable_int <- as.data.frame(matrix(NA, nrow = length(merged), ncol = length(ms2scans)))
  peaktable_mass <- as.data.frame(matrix(int0, nrow = length(merged), ncol = length(ms2scans)))
  for (i in 1:length(merged)) {
    peaktable_int[i,merged[[i]]$scans] <- merged[[i]]$ints
    peaktable_mass[i,merged[[i]]$scans] <- merged[[i]]$masses
  }
  list(peaktable_int = peaktable_int, peaktable_mass = peaktable_mass, EIC = peak.EIC, ms1scans = ms1scans, ms2scans = ms2scans)
}

ms1_peaktable <- function(peak, mass, leftwindow = 1, rightwindow = 5, masserror = 5, minerror = 0.002) {
  scans <- which(names(peak$mzML$run$spectrumList) == "spectrum")
  mslevels <- sapply(scans, getmslevel, mzml = peak)
  ms1scans <- scans[which(mslevels == 1)]
  peak.EIC <- EIC(peak, mass, masserror, minerror, MS1 = TRUE)
  peakindex <- which.max(peak.EIC$intensity)
  minmass <- mass - leftwindow
  maxmass <- mass + rightwindow
  mslist <- lapply(ms1scans, function(x) cbind(peak$mzML$run$spectrumList[[x]]$masses[which(peak$mzML$run$spectrumList[[x]]$masses >= minmass & peak$mzML$run$spectrumList[[x]]$masses <= maxmass)], peak$mzML$run$spectrumList[[x]]$intensities[which(peak$mzML$run$spectrumList[[x]]$masses >= minmass & peak$mzML$run$spectrumList[[x]]$masses <= maxmass)]))
  merged <- mergems(mslist, peakindex, masserror = masserror, minerror = minerror)
  peaktable_int <- matrix(0, nrow = length(merged), ncol = length(ms1scans))
  colnames(peaktable_int) <- NA
  peaktable_mass <- matrix(NA, nrow = length(merged), ncol = length(ms1scans))
  for (i in 1:length(merged)) {
    peaktable_int[i,merged[[i]]$scans] <- merged[[i]]$ints
    peaktable_mass[i,merged[[i]]$scans] <- merged[[i]]$masses
  }
  if (peakindex != 1) {
    peaktable_int <- cbind(peaktable_int[,2:(peakindex)], peaktable_int[,1], peaktable_int[,(peakindex+1):ncol(peaktable_int)])
    peaktable_mass <- cbind(peaktable_mass[,2:(peakindex)], peaktable_mass[,1], peaktable_mass[,(peakindex+1):ncol(peaktable_mass)])
  }
  list(peaktable_int = peaktable_int, peaktable_mass = peaktable_mass, EIC = peak.EIC, ms1scans = ms1scans)
}

getEIC <- function(peak, mass, masserror = 5, minerror = 0.002, mslevel = "MS1") {
  #creates extracted ion chromatogram for a specific mass using the peak object
  mass.diff <- max(masserror*mass/(10^6), minerror)
  minmass <- mass - mass.diff
  maxmass <- mass + mass.diff
  time <- sapply(peak, function(x) x$time)
  int <- sapply(peak, function(x) sum(x$intensities[which(x$masses >= minmass & x$masses <= maxmass)]))
  if (mslevel == "MS1") {
    mslevels <- sapply(peak, function(x) x$mslevel)
    int <- int[which(mslevels == 1)]
    time <- time[which(mslevels == 1)]
  }
  if (mslevel == "MS2") {
    mslevels <- sapply(peak, function(x) x$mslevel)
    int <- int[which(mslevels == 2)]
    time <- time[which(mslevels == 2)]
  }
  list(time = time, intensity = int)
}

mergems <- function(mslist, peakindex, masserror = 5, minerror = 0.001) {
  ms <- mslist[[peakindex]]
  if (nrow(ms) == 1) {
    list(masses = ms[1,1], ints = ms[1,2], scans = peakindex)
  }
  mergedms <- lapply(1:nrow(ms), function(x) list(masses = ms[x,1], ints = ms[x,2], scans = peakindex))
  if (length(mslist) == 1) {
    return(mergedms)
  }
  ind <- seq_len(length(mslist))
  inds <- order(abs(ind - peakindex))
  inds <- inds[-1]
  for (i in inds) {
    newms <- lapply(mergedms, function(x) mergemass(x$masses, x$ints, x$scans, mslist[[i]], ind = i, masserror = masserror, minerror = minerror))
    leftin <- do.call(c, lapply(mergedms, function(x) getmergedind(x$masses, mslist[[i]], masserror = masserror, minerror = minerror)))
    leftout <- setdiff(1:nrow(mslist[[i]]), leftin)
    extrams <- lapply(leftout, function(x) list(masses = mslist[[i]][x,1], ints = mslist[[i]][x,2], scans = i))
    mergedms <- append(newms, extrams)
  }
  mergedms
}

mergemass <- function(masses, ints, scans, addms, ind, masserror, minerror) {
  minmass <- min(min(masses) - min(masses)*masserror/(10^6),min(masses) - minerror)
  maxmass <- max(max(masses) + max(masses)*masserror/(10^6),max(masses) + minerror)
  inds <- which(addms[,1] >= minmass & addms[,1] <= maxmass)
  if (length(inds) > 0) {
    masses <- c(masses, mean(addms[inds,1], na.rm = TRUE))
    ints <- c(ints, sum(addms[inds,2], na.rm = TRUE))
    scans <- c(scans, ind)
  }
  output <- list(masses = masses, ints = ints, scans = scans)
  output
}

getmergedind <- function(masses, addms, masserror, minerror) {
  minmass <- min(min(masses) - min(masses)*masserror/(10^6),min(masses) - minerror)
  maxmass <- max(max(masses) + max(masses)*masserror/(10^6),max(masses) + minerror)
  inds <- which(addms[,1] >= minmass & addms[,1] <= maxmass)
  inds
}


get_cms2 <- function(peaktable, correl = NULL, ph = NULL, freq = NULL, normfn = "sum", cormethod = "pearson") {
  ph = ph/100
  eic <- peaktable$EIC$intensity
  minph <- (ph * (max(eic) - min(eic)))+min(eic)
  eic <- peaktable$EIC$intensity[which(peaktable$ms1scans %in% (peaktable$ms2scans - 1))]
  if (ncol(peaktable$peaktable_mass) > 1) {
  if (!is.null(correl)) {
    cors <- apply(peaktable$peaktable_int, 1, function(y) cor(eic, y, use = "complete.obs", method = cormethod))
    ind <- which(cors >= correl)
    peaktable$peaktable_int <- peaktable$peaktable_int[ind,]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[ind,]
  }
  if (!is.null(ph)) {
    if (length(ph) != 0) {
    scans <- peaktable$ms1scans[which(eic >= minph)]
    scans <- which(peaktable$ms2scans >= min(scans) & peaktable$ms2scans <= max(scans))
    peaktable$peaktable_int <- peaktable$peaktable_int[,scans]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[,scans]
    }
  }
  if (!is.null(freq)) {
    tot <- ncol(peaktable$peaktable_mass)
    freq <- tot*freq/100
    ns <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
    ind <- which(ns >= freq)
    peaktable$peaktable_int <- peaktable$peaktable_int[ind,]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[ind,]
  }
  peaktable$peaktable_int <- do.call(cbind, lapply(1:ncol(peaktable$peaktable_int), function(i) peaktable$peaktable_int[,i]/get(normfn)(peaktable$peaktable_int[,i], na.rm = TRUE)))
  mz <- apply(peaktable$peaktable_mass, 1, mean, na.rm = TRUE)
  mz.u <- apply(peaktable$peaktable_mass, 1, sd, na.rm = TRUE)
  int <- apply(peaktable$peaktable_int, 1, mean, na.rm = TRUE)
  int.u <- apply(peaktable$peaktable_int, 1, sd, na.rm = TRUE)
  n <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
  }
  if (ncol(peaktable$peaktable_mass) <= 1) {
    mz <- unlist(c(peaktable$peaktable_mass))
    mz.u <- rep(NA, length(peaktable$peaktable_mass))
    int <- unlist(c(peaktable$peaktable_int/get(normfn)(peaktable$peaktable_int)))
    int.u <- rep(NA, length(peaktable$peaktable_int))
    n <- rep(1, length(peaktable$peaktable_int))
  }
  x <- which(!is.nan(mz))
  data.frame(mz = mz[x], mz.u = mz.u[x], int = int[x], int.u = int.u[x], n = n[x])
}

get_cms1 <- function(peaktable, correl = NULL, ph = NULL, freq = NULL, normfn = "sum", cormethod = "pearson") {
  ph = ph/100
  eic <- peaktable$EIC$intensity
  minph <- (ph * (max(eic) - min(eic)))+min(eic)
  if (!is.null(correl)) {
    cors <- apply(peaktable$peaktable_int, 1, function(y) cor(eic, y, use = "complete.obs", method = cormethod))
    ind <- which(cors >= correl)
    peaktable$peaktable_int <- peaktable$peaktable_int[ind,]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[ind,]
  }
  if (!is.null(ph)) {
    if (length(ph) != 0) {
    scans <- which(eic >= minph)
    peaktable$peaktable_int <- peaktable$peaktable_int[,scans]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[,scans]
    }
  }
  if (!is.null(freq)) {
    tot <- ncol(peaktable$peaktable_mass)
    freq <- tot*freq/100
    ns <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
    ind <- which(ns >= freq)
    peaktable$peaktable_int <- peaktable$peaktable_int[ind,]
    peaktable$peaktable_mass <- peaktable$peaktable_mass[ind,]
  }
  peaktable$peaktable_int <- do.call(cbind, lapply(1:ncol(peaktable$peaktable_int), function(i) peaktable$peaktable_int[,i]/get(normfn)(peaktable$peaktable_int[,i], na.rm = TRUE)))
  mz <- apply(peaktable$peaktable_mass, 1, mean, na.rm = TRUE)
  mz.u <- apply(peaktable$peaktable_mass, 1, sd, na.rm = TRUE)
  int <- apply(peaktable$peaktable_int, 1, mean, na.rm = TRUE)
  int.u <- apply(peaktable$peaktable_int, 1, sd, na.rm = TRUE)
  n <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
  x <- which(!is.nan(mz))
  data.frame(mz = mz[x], mz.u = mz.u[x], int = int[x], int.u = int.u[x], n = n[x])
}