#' Spectral Uncertainty Functions ----------------------------------------------------------

#' Create peak list from SQL ms_data table
#'
#' The function extracts the relevant information and sorts it into nested lists for
#' use in the uncertainty functions
#'
#' @param ms_data extraction of the ms_data from the SQL table for a specified peak
#'
#' @return nested list of all data
#' @export
#'
create_peak_list <- function(ms_data) {
  lapply(1:nrow(ms_data), function(x) list(
    masses =  as.numeric(unlist(strsplit(ms_data$measured_mz[x]," "))), 
    intensities = as.numeric(unlist(strsplit(ms_data$measured_intensity[x], " "))),
    totalion = sum(as.numeric(unlist(strsplit(ms_data$measured_intensity[x], " ")))),
    baseion = ms_data$baseion[x],
    base_int = ms_data$base_int[x],
    time = ms_data$scantime[x], 
    mslevel = ms_data$ms_n[x]))
}


#' Create peak table for MS2 data
#'
#' Takes a nested peak list and creates a peak table for easier determination of 
#' uncertainty of the measurement for MS2 data.
#'
#' @param peaklist result of the `create_peak_list` function
#' @param mass the exact mass of the compound of interest
#' @param masserror the mass accuracy (in ppm) of the instrument data
#' @param minerror the minimum mass error (in Da) of the instrument data
#' @param int0 the default setting for intensity values for missing m/z values
#' 
#' @return nested list of dataframes containing all MS2 data for the peak
#' @export
#'
create_peak_table_ms2 <- function(peak, mass, masserror = 5, minerror = 0.002, int0 = NA) {
  #creates MS2 data tables from a peak object including the mass and intensity (separate tables) into a peak table object
  scans <- 1:length(peak)
  mslevels <- sapply(scans, function(x) peak[[x]]$mslevel)
  peak.EIC <- getEIC(peak, mass, masserror, minerror, mslevel = "MS1")
  ms1scans <- scans[which(mslevels == 1)]
  ms2scans <- scans[which(mslevels == 2)]
  ms1scans_peak <- ms2scans - 1
  ms1scans_ind <- which(ms1scans %in% ms1scans_peak)
  peakindex <- which(ms2scans == ms1scans_peak[which.max(peak.EIC$intensity[ms1scans_ind])]+1)
  mslist <- lapply(ms2scans, function(x) cbind(peak[[x]]$masses, peak[[x]]$intensities))
  merged <- mergems(mslist, peakindex, masserror = masserror, minerror = minerror)
  peaktable_int <- matrix(NA, nrow = length(merged), ncol = length(ms2scans))
  peaktable_mass <- matrix(int0, nrow = length(merged), ncol = length(ms2scans))
  for (i in 1:length(merged)) {
    peaktable_int[i,merged[[i]]$scans] <- merged[[i]]$ints
    peaktable_mass[i,merged[[i]]$scans] <- merged[[i]]$masses
  }
  out <- list(peaktable_int = peaktable_int, peaktable_mass = peaktable_mass, EIC = peak.EIC, ms1scans = ms1scans, ms2scans = ms2scans)
  attr(out, "mslevel") <- 2
  out
}

#' Create peak table for MS1 data
#'
#' Takes a nested peak list and creates a peak table for easier determination of 
#' uncertainty of the measurement for MS1 data.
#'
#' @param peaklist result of the `create_peak_list` function
#' @param mass the exact mass of the compound of interest
#' @param masserror the mass accuracy (in ppm) of the instrument data
#' @param minerror the minimum mass error (in Da) of the instrument data
#' @param int0 the default setting for intensity values for missing m/z values
#' 
#' @return nested list of dataframes containing all MS2 data for the peak
#' @export
#'
create_peak_table_ms1 <- function(peak, mass, masserror = 5, minerror = 0.002, int0 = NA) {
  #creates MS1 data tables from a peak object including the mass and intensity (separate tables) into a peak table object
  scans <- 1:length(peak)
  mslevels <- sapply(scans, function(x) peak[[x]]$mslevel)
  peak.EIC <- getEIC(peak, mass, masserror, minerror, mslevel = "MS1")
  ms1scans <- scans[which(mslevels == 1)]
  ms2scans <- scans[which(mslevels == 2)]
  peakindex <- which.max(peak.EIC$intensity)
  mslist <- lapply(ms1scans, function(x) cbind(peak[[x]]$masses, peak[[x]]$intensities))
  merged <- mergems(mslist, peakindex, masserror = masserror, minerror = minerror)
  peaktable_int <- matrix(NA, nrow = length(merged), ncol = length(ms1scans))
  peaktable_mass <- matrix(int0, nrow = length(merged), ncol = length(ms1scans))
  for (i in 1:length(merged)) {
    peaktable_int[i,merged[[i]]$scans] <- merged[[i]]$ints
    peaktable_mass[i,merged[[i]]$scans] <- merged[[i]]$masses
  }
  out <- list(peaktable_int = peaktable_int, peaktable_mass = peaktable_mass, EIC = peak.EIC, ms1scans = ms1scans, ms2scans = ms2scans)
  attr(out, "mslevel") <- 1
  out
}

# TODO documentation
getEIC <- function(peak, mass, masserror = 5, minerror = 0.002, mslevel = "MS1") {
  #creates extracted ion chromatogram for a specific mass using the peak list
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

# TODO documentation
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
    if (nrow(mslist[[i]]) > 0) {
    newms <- lapply(mergedms, function(x) mergemass(x$masses, x$ints, x$scans, mslist[[i]], ind = i, masserror = masserror, minerror = minerror))
    leftin <- do.call(c, lapply(mergedms, function(x) getmergedind(x$masses, mslist[[i]], masserror = masserror, minerror = minerror)))
    leftout <- setdiff(1:nrow(mslist[[i]]), leftin)
    if (length(leftout) > 0) {
      extrams <- lapply(leftout, function(x) list(masses = mslist[[i]][x,1], ints = mslist[[i]][x,2], scans = i))
      mergedms <- append(newms, extrams)
    }
    }
  }
  mergedms
}

# TODO documentation
mergemass <- function(masses, ints, scans, addms, ind, masserror, minerror) {
  minmass <- min(min(masses) - min(masses)*masserror/(10^6),min(masses) - minerror)
  maxmass <- max(max(masses) + max(masses)*masserror/(10^6),max(masses) + minerror)
  inds <- which(addms[,1] >= minmass & addms[,1] <= maxmass)
  masses <- c(masses, addms[inds,1])
  ints <- c(ints, addms[inds,2])
  scans <- c(scans, rep(ind, length(inds)))
  output <- list(masses = masses, ints = ints, scans = scans)
  output
}

# TODO documentation
getmergedind <- function(masses, addms, masserror, minerror) {
  minmass <- min(min(masses) - min(masses)*masserror/(10^6),min(masses) - minerror)
  maxmass <- max(max(masses) + max(masses)*masserror/(10^6),max(masses) + minerror)
  inds <- which(addms[,1] >= minmass & addms[,1] <= maxmass)
  inds
}

#' Generate consensus mass spectrum
#'
#' The function calculates the uncertainty mass spectrum for a single peak table based
#' on specific settings described in https://doi.org/10.1021/jasms.0c00423
#'
#' @param peaktable result of the `create_peak_table_ms1` or  `create_peak_table_ms1` function
#' @param correl Minimum correlation coefficient between the target ions and the base ion intensity of the targeted m/z to be included in the mass spectrum
#' @param ph Minimum chromatographic peak height from which to extract MS2 data for the mass spectrum
#' @param freq minimum observational frequency of the target ions to be included in the mass spectrum
#' @param normfn the normalization function typically "mean" or "sum" for normalizing the intensity values
#' @param cormethod the correlation method used for calculating the correlation, see `cor` function for methods
#'
#' @return nested list of dataframes containing all MS1 and MS2 data for the peak
#' @export
#'
get_ums <- function(peaktable, correl = NULL, ph = NULL, freq = NULL, normfn = "sum", cormethod = "pearson") {
  
  if (attr(peaktable, "mslevel") == 2) {eic_cor <- eic <- peaktable$EIC$intensity[sapply(peaktable$ms2scans, function(x) which.min(abs(x - peaktable$ms1scans)))]}
  if (attr(peaktable, "mslevel") == 1) {eic_cor <- eic <- peaktable$EIC$intensity}
  if (!is.null(correl)) {
    if (!is.na(correl)) {
    suppressWarnings(cors <- apply(peaktable$peaktable_int, 1, function(y) cor(eic, y, use = "complete.obs", method = cormethod)))
    ind <- which(cors >= correl)
    peaktable$peaktable_int <- data.frame(peaktable$peaktable_int[ind,], fix.empty.names = FALSE)
    peaktable$peaktable_mass <- data.frame(peaktable$peaktable_mass[ind,], fix.empty.names = FALSE)
    }
  }
  if (!is.null(ph)) {
    if (!is.na(ph)) {
    ph = ph/100
    minph <- (ph * (max(eic) - min(eic)))+min(eic)
    scans <- which(eic >= minph)
    peaktable$peaktable_int <- data.frame(peaktable$peaktable_int[,scans], fix.empty.names = FALSE)
    peaktable$peaktable_mass <- data.frame(peaktable$peaktable_mass[,scans], fix.empty.names = FALSE)
    }
  }
  if (!is.null(freq)) {
    if (!is.na(freq)) {
    tot <- ncol(peaktable$peaktable_mass)
    freq <- tot*freq/100
    ns <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
    ind <- which(ns >= freq)
    peaktable$peaktable_int <- data.frame(peaktable$peaktable_int[ind,], fix.empty.names = FALSE)
    peaktable$peaktable_mass <- data.frame(peaktable$peaktable_mass[ind,], fix.empty.names = FALSE)
    }
  }
  peaktable$peaktable_int <- do.call(cbind, lapply(1:ncol(peaktable$peaktable_int), function(i) peaktable$peaktable_int[,i]/get(normfn)(peaktable$peaktable_int[,i], na.rm = TRUE)))
  mz <- apply(peaktable$peaktable_mass, 1, mean, na.rm = TRUE)
  mz.u <- apply(peaktable$peaktable_mass, 1, sd, na.rm = TRUE)
  int <- apply(peaktable$peaktable_int, 1, mean, na.rm = TRUE)
  int.u <- apply(peaktable$peaktable_int, 1, sd, na.rm = TRUE)
  n <- apply(peaktable$peaktable_mass, 1, function(x) length(which(!is.na(x))))
  x <- which(!is.nan(mz) & !is.nan(int))
  out <- data.frame(mz = mz[x], mz.u = mz.u[x], int = int[x], int.u = int.u[x], n = n[x])
  
  attr(out, "numscans") <- ncol(peaktable$peaktable_mass)

  out
}

#' Generate consensus mass spectrum
#'
#' Extract relevant information from a mass spectrum and plot it as an uncertainty mass spectrum.
#'
#' @param peaklist result of the `create_peak_list` function
#'
#' @return ggplot object
#' @export
#'
plot_ms <- function(ms, xlim = NULL, ylim = NULL, main = "Mass Spectrum", color = "black", size = 1, removal = 0) {
  require(ggplot2)
  ms <- ms[which(ms$int >= removal),]
  int <- ms$int
  intu <- ms$int.u
  mz <- ms$mz
  mzu <- ms$mz.u
  if (packageVersion("ggplot2") >= '3.4.0') {
    ggplot(data.frame(mz = mz, int = int)) + geom_linerange(aes(x = mz, ymin = 0, ymax = int), color = color, linewidth = size) + geom_pointrange(aes(x = mz, ymin = 0, ymax = int, y= int), shape = 20, size = 0.5) + geom_errorbar(aes(x = mz, ymin = int - intu, ymax = int + intu, width = 0.01), color = "red", na.rm = TRUE, linetype = 5) + geom_errorbarh(aes(y = int, xmin = mz - mzu, xmax = mz + mzu, height = 0.01), color = "red", na.rm = TRUE, linetype = 5) + ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)+ theme_bw() 
  } else {
    ggplot(data.frame(mz = mz, int = int)) + geom_linerange(aes(x = mz, ymin = 0, ymax = int), color = color, size = size) + geom_pointrange(aes(x = mz, ymin = 0, ymax = int, y= int), shape = 20, size = 0.5) + geom_errorbar(aes(x = mz, ymin = int - intu, ymax = int + intu, width = 0.01), color = "red", na.rm = TRUE, linetype = 5) + geom_errorbarh(aes(y = int, xmin = mz - mzu, xmax = mz + mzu, height = 0.01), color = "red", na.rm = TRUE, linetype = 5) + ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)+ theme_bw() 
  }
}