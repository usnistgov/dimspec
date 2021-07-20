peaktable <- function(msdata, masserror = 5, minerror = 0.002, int0 = NA) {
  mslist <- lapply(msdata, function(x) data.frame(matrix(unzip(x), ncol = 2, byrow = TRUE)))
  peakindex <- which.max(sapply(mslist, function(x) sum(x[,2])))
  merged <- mergems(mslist, peakindex, masserror = masserror, minerror = minerror)
  peaktable_int <- as.data.frame(matrix(NA, nrow = length(merged), ncol = length(mslist)))
  peaktable_mass <- as.data.frame(matrix(int0, nrow = length(merged), ncol = length(mslist)))
  for (i in 1:length(merged)) {
    peaktable_int[i,merged[[i]]$scans] <- merged[[i]]$ints
    peaktable_mass[i,merged[[i]]$scans] <- merged[[i]]$masses
  }
  list(mass = peaktable_mass, int = peaktable_int)
}

mergems <- function(mslist, peakindex, masserror = 5, minerror = 0.001) {
  ms <- mslist[[peakindex]]
  if (nrow(ms) == 1) {
    mergedms <- list(masses = ms[1,1], ints = ms[1,2], scans = peakindex)
  }
  if (nrow(ms) > 0) {
    mergedms <- lapply(1:nrow(ms), function(x) list(masses = ms[x,1], ints = ms[x,2], scans = peakindex))
  }
  if (length(mslist) == 1) {
    return(mergedms)
  }
  ind <- seq_len(length(mslist))
  inds <- order(abs(ind - peakindex))
  inds <- inds[-1]
  for (i in inds) {
    if (length(mslist[[i]] > 0)) {
      newms <- lapply(mergedms, function(x) mergemass(x$masses, x$ints, x$scans, mslist[[i]], ind = i, masserror = masserror, minerror = minerror))
      leftin <- do.call(c, lapply(mergedms, function(x) getmergedind(x$masses, mslist[[i]], masserror = masserror, minerror = minerror)))
      leftout <- setdiff(1:nrow(mslist[[i]]), leftin)
      extrams <- lapply(leftout, function(x) list(masses = mslist[[i]][x,1], ints = mslist[[i]][x,2], scans = i))
      mergedms <- append(newms, extrams)
    }
  }
  mergedms
}

mergemass <- function(masses, ints, scans, addms, ind, masserror, minerror) {
  minmass <- min(min(masses, na.rm = TRUE) - min(masses, na.rm = TRUE)*masserror/(10^6),min(masses, na.rm = TRUE) - minerror)
  maxmass <- max(max(masses, na.rm = TRUE) + max(masses, na.rm = TRUE)*masserror/(10^6),max(masses, na.rm = TRUE) + minerror)
  inds <- which(addms[,1] >= minmass & addms[,1] <= maxmass)
  if (length(inds) > 0) {
    masses <- c(masses, mean(addms[inds,1], na.rm = TRUE)) #this is updated to 7/8/21
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