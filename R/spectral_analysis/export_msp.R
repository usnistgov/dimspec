# Spectral Export Functions ----------------------------------------------------------

#' Export to MSP
#'
#' The function exports an uncertainty mass spectrum into a NIST MS Search .msp file
#'
#' @param ms uncertainty mass spectrum from `get_ums` function
#' @param file export .msp file to save the msp files
#' @param precursor If available, the numeric precursor m/z for the designated mass spectrum
#' @param name Text name to assign to the mass spectrum (not used in spectral searching)
#' @param append boolean (TRUE/FALSE) to append to .msp file (TRUE) or overwrite (FALSE)
#'
#' @export
#'

export_msp <- function(ms, file, precursor = "", name = "Exported Mass Spectrum", append = FALSE) {
  # saves mass spectrum as a NIST MS Search-readable file with precursor ion designated
  peaks <- nrow(ms)
  names <- paste("Name: ", name) 
  precursor <- paste("PrecursorMZ: ", precursor)
  peaks <- paste("Num Peaks: ", peaks)
  header <- rbind(names, precursor, peaks)
  write.table(header, file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
  if (!is.na(ms[1])) {
    write.table(ms[,c("mz","int")], file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  if (is.na(ms[1])) {
    write.table(cbind(0,0), file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  write("\n\n", file, append = append)
}