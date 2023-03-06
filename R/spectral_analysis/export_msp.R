# Spectral Export Functions ----------------------------------------------------------

#' Export to MSP
#'
#' The function exports an uncertainty mass spectrum into a NIST MS Search .msp file
#'
#' @param ms uncertainty mass spectrum from `get_ums` function
#' @param file export .msp file to save the msp files
#' @param precursor If available, the numeric precursor m/z for the designated mass spectrum
#' @param name Text name to assign to the mass spectrum (not used in spectral searching)
#' @param headerdata character string containing named values for additional data to put in the header
#' @param append boolean (TRUE/FALSE) to append to .msp file (TRUE) or overwrite (FALSE)
#'
#' @export
#'

export_msp <- function(ms, file, precursor = "", name = "Exported Mass Spectrum", headerdata = c(), append = FALSE) {
  # saves mass spectrum as a NIST MS Search-readable file with precursor ion designated
  peaks <- nrow(ms)
  names <- paste("Name: ", name) 
  precursor <- paste("PrecursorMZ: ", precursor)
  peaks <- paste("Num Peaks: ", peaks)
  headadd <- c()
  if (length(headerdata) > 0) {
    if (is.null(names(headerdata))) {
      names(headerdata) <- paste0("value", 1:length(headerdata))
    }
    if ("" %in% names(headerdata)) {
      names(headerdata)[which(names(headerdata) == "")] <- paste0("value", which(names(headerdata) == ""))
    }
    for (i in 1:length(headerdata)) {
      headadd <- rbind(headadd, paste0(names(headerdata)[i], ": ", headerdata[i]))
    }
  }
  header <- rbind(names, precursor, headadd, peaks)
  write.table(header, file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
  if (!is.na(ms[1,1])) { #this had to be adjusted for dbGetQuery's output structure, may break something
    write.table(ms[,c("mz","int")], file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  if (is.na(ms[1,1])) {
    write.table(cbind(0,0), file, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE)
  }
  write("\n\n", file, append = append)
}


#' Export SQL Database to a MSP NIST MS Format
#'
#' @param con SQLite database connection 
#' @param optimized_params Boolean TRUE indicates that the optimized parameters for uncertainty mass spectra will be used.
#' @param outputfile Text string file name and/or location to save MSP file format
#' @param cormethod Text string type of correlation function to use (DEFAULT = 'pearson')
#' @param normfn Text string type of normalization function to use (DEFAULT = 'sum')
#'
#' @return None, saves a *.msp file to the local file system.
#' @export
#'

sql_to_msp <- function(con, optimized_params = TRUE, outputfile = paste0("DimSpecExport", Sys.Date(), ".msp"), cormethod = "pearson", normfn = "sum") {
  msdata <- get_msdata(con)
  peak_ids <- unique(msdata$peak_id)
  if (length(peak_ids) == 0) {stop("there is no MS data in the designated database")}
  mslist <- lapply(peak_ids, function(x) create_peak_list(msdata[which(msdata$peak_id == x),]))
  errorinfo <- get_errorinfo(con, peak_ids)
  opt_params <- get_opt_params(con, peak_ids)
  compound_info <- get_compound_info(con, peak_ids)
  opt_params[which(opt_params == -1, arr.ind = TRUE)] <- NA
  if (optimized_params == FALSE) {
    opt_params$correl <- opt_params$ph <- opt_params$freq <- opt_params$n <- rep(NA, nrow(opt_params))
  }
  ptms2 <- lapply(1:length(mslist),  function(x) create_peak_table_ms2(mslist[[x]], mass = as.numeric(errorinfo$precursor_mz[x]), masserror = opt_params$masserror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)], minerror = opt_params$minerror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)]))
  l.ums2 <- lapply(1:length(ptms2), function(x) {
    get_ums(
      peaktable = ptms2[[x]],
      correl = opt_params$correl[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)],
      ph = opt_params$ph[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)],
      freq = opt_params$freq[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)],
      normfn = normfn,
      cormethod = cormethod
    )
  })
  for (i in 1:length(peak_ids)) {
    headerdata <- c(NISTID = unique(compound_info$compound_id[which(compound_info$peakid == peak_ids[i])]),
                    INCHI = unique(compound_info$inchi[which(compound_info$peakid == peak_ids[i])][1])
    )
    export_msp(ms = l.ums2[[i]],
              file = outputfile,
              precursor = get_peak_precursor(con, peakid = peak_ids[i]),
              name = compound_info$name[which(compound_info$peakid == peak_ids[i])],
              headerdata = headerdata,
              append = TRUE)
  }
  print(paste("Exported", length(peak_ids), "mass spectra from the database."))
}


get_compound_info <- function(con, peakid) {
  peakid <- unique(peakid) # remove duplicates, or else the last command fails
  cinfo <- DBI::dbGetQuery(conn = con, 
                  paste0(
                    "SELECT view_compounds.id AS compound_id, view_compounds.name AS name FROM view_compounds
                    LEFT JOIN compound_fragments ON view_compounds.id = compound_fragments.compound_id
                    LEFT JOIN peaks ON compound_fragments.peak_id = peaks.id
                    WHERE peaks.id IN (",
                    paste(peakid, collapse = ","),
                    ")
                    GROUP By peaks.id"
                  ))
  compoundid <- cinfo$compound_id
  structures <- do.call(rbind, lapply(compoundid, function(x) DBI::dbGetQuery(conn = con,
                                                               paste0("SELECT compound_id, alias AS INCHI FROM compound_aliases
                                    WHERE alias_type = 4
                                    AND compound_id IN (",
                                                                      paste(x, collapse = ","),
                                                                      ")"))))
  cbind(peakid, cinfo, inchi = structures$INCHI)
}