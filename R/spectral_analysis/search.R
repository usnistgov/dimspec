
#' Create data.frame containing parameters for extraction and searching
#'
#' Use this to create an intermediate data frame object used as part of the search routine.
#'
#' @param filename CHR scalar path to the mzml file
#' @param precursormz NUM scalar for the mass-to-charge ratio to examine
#' @param rt NUM scalar for the retention time centroid to examine
#' @param rt_start NUM scalar for the retention time start point of the feature
#' @param rt_end NUM scalar for the retention time end point of the feature
#' @param masserror NUM scalar of the instrument mass error value in parts per million
#' @param minerror NUM scalar of the minimum mass error value to use in absolute terms
#' @param ms2exp NUM scalar type of the fragmentation experiment (e.g. MS1 or MS2)
#' @param isowidth NUM scalar mass isolation width to use
#'
#' @return data.frame object collating provided values
#' @export
#'
create_search_df <- function(filename, precursormz, rt, rt_start, rt_end, masserror, minerror, ms2exp, isowidth) {
  data.frame(filename = filename,
             precursormz = precursormz,
             rt = rt,
             rt_start = rt_start,
             rt_end = rt_end,
             masserror = masserror,
             minerror = minerror,
             ms2exp = ms2exp,
             isowidth = isowidth)
}

#' Brings raw data file into environment
#'
#' If filename is not extension .mzML, then converts the raw file
#'
#' @param search_df data.frame output of [create_search_df] or file name of a raw file to be converted
#' @param CONVERT LGL scalar of whether or not to convert the search_df filename (default FALSE)
#' @param CHECKCONVERT LGL scalar of whether or not to verify the conversion format (default TRUE)
#'
#' @return LIST value of the trimmed mzML file matching search criteria
#'
getmzML <- function(search_df, CONVERT = FALSE, CHECKCONVERT = TRUE, is_waters = FALSE, lockmass = NULL, lockmasswidth = NULL, correct = FALSE) {
  ext <-  gsub(pattern = "[[:print:]]*\\.(.*)$", replacement = "\\1", basename(search_df$filename))
  mzmlfile = search_df$filename
  if (ext != "mzML") {
    if (CONVERT == TRUE) {
      outdir = getwd()
      mzmlfile <- mzMLconvert(search_df$filename, msconvert = NULL, config = NULL, outdir = outdir)
      while (!file.exists(paste(outdir, "/convert_done.txt", sep = ""))) {
        Sys.sleep(1)
      }
    }
    if (CONVERT == FALSE) {
      stop("The raw file is not an mzML file, please use Proteowizard MSConvert to convert the file to mzML. \n See Documentation for more information.")
    }
  }
  if (is_waters) {
    outmzml <- try(mzMLtoR(mzmlfile = mzmlfile, lockmass = lockmass, lockmasswidth = lockmasswidth, correct = correct))
  } else {
    outmzml <- try(mzMLtoR(mzmlfile), silent = TRUE)
  }
  if (class(outmzml) == "try-error") {stop("The raw file has not be properly converted to an mzML file, please use Proteowizard MSConvert to convert the file to mzML. \n See Documentation for more information.")}
  if (CHECKCONVERT == TRUE) {
    check <- check_mzML_convert(outmzml)
    if (FALSE %in% check$result) {
      stop(paste("mzML File was converted incorrectly \n", paste(check$msg[which(check$result == FALSE)], collapse = "\n")))
    }
  }
  outmzml$search_df <- search_df
  outmzml
}

#' Check mzML file for specific MSConvert parameters
#'
#' @param mzml list of msdata from `mzMLtoR` function
#'
#' @return data.frame object of conversion veracity checks
#' @export
check_mzML_convert <- function(mzml) {
  msconvertdata <- do.call(c, get_msconvert_data(mzml))
  result <- c(TRUE, TRUE, TRUE)
  msg <- c("","","")
  if (length(grep("Conversion to mzML pwiz", msconvertdata)) == 0) {
    result[1] <- FALSE
    msg[1] <- "The file must be converted to mzML using Proteowizard MSConver.t"
  }
  if (length(grep("peak picking [[:print:]]* pwiz", msconvertdata)) == 0) {
    result[2] <- FALSE
    msg[2] <- "You must include a peak picking algorithm when converting the mzML file."
  }
  if (length(grep("absolute intensity greater than [[:digit:]]* pwiz", msconvertdata)) == 0) {
    result[3] <- FALSE
    msg[3] <- "You should use the threshold function to remove low level signals to reduce the file size."
  }
  data.frame(result = result, msg = msg)
}

#' Generate msdata object from input peak data
#'
#' @param searchmzml mzml with searching dataframe from `getmzML` function
#' @param zoom vector length of 2 containing +/- the area around the MS1 precursor ion to collect data.
#'
#' @return LIST object of data.frames include MS1 and MS2 analytical data, and the search parameters used to generate them
#' @export
get_search_object <- function(searchmzml, zoom = c(1,4)) {
  scans <- which(names(searchmzml$mzML$run$spectrumList) == "spectrum")
  times <- sapply(scans, gettime, mzml=searchmzml)
  mslevels <- sapply(scans, getmslevel, mzml=searchmzml)
  precursors <- sapply(scans, getprecursor, mzml=searchmzml)
  
  all_scans <- scans[which(times >= searchmzml$search_df$rt_start & times <= searchmzml$search_df$rt_end)]
  ms1scans <- all_scans[which(mslevels[all_scans] == 1)]
  if (searchmzml$search_df$ms2exp == "data-independent acquisition (DIA/AIF)" | searchmzml$search_df$ms2exp == "DIA") {
    ms2scans <- all_scans[which(mslevels[all_scans] == 2)]
  }
  if (searchmzml$search_df$ms2exp == "data-dependent acquisition (DDA/TopN)" | searchmzml$search_df$ms2exp == "DDA" | searchmzml$search_df$ms2exp == "DDA (data-dependent acquisition)") {
    ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(searchmzml$search_df$precursormz) - as.numeric(searchmzml$search_df$isowidth) & precursors[all_scans] <= as.numeric(searchmzml$search_df$precursormz) + as.numeric(searchmzml$search_df$isowidth))]
  }
  if (searchmzml$search_df$ms2exp == "SWATH") {
    ms2scans <- all_scans[which(precursors[all_scans] >= as.numeric(searchmzml$search_df$precursormz) - (as.numeric(searchmzml$search_df$isowidth)/2) & precursors[all_scans] <= as.numeric(searchmzml$search_df$precursormz) + (as.numeric(searchmzml$search_df$isowidth)/2))]
  }
  peak_scans <- sort(c(ms1scans, ms2scans))
  mz <- searchmzml$search_df$precursormz
  masserror <- searchmzml$search_df$masserror
  minerror <- searchmzml$search_df$minerror
  msdata <- do.call(rbind, lapply(peak_scans, function(x) {
    ms <- extract.ms(searchmzml, x)
    if (getmslevel(searchmzml, x) == 1 & !is.na(zoom[1])) {
      ms <- ms[which(ms[,1] >= mz - zoom[1] & ms[,1] <= mz + zoom[2]),]
      
    }
    if (length(ms) == 0) {return(NULL)}
    data.frame(scan = x, 
               scantime = gettime(searchmzml, x),
               baseion = mean(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),1], na.rm = TRUE),
               base_int = sum(ms[which(ms[,1] >= mz - max(mz*masserror*1E-6,minerror) & ms[,1] <= mz + max(mz*masserror*1E-6,minerror)),2], na.rm = TRUE),
               masses = paste(ms[,1], collapse = " "),
               intensities = paste(ms[,2], collapse = " "),
               ms_n = getmslevel(searchmzml, x))
  }))
  pl <- lapply(1:nrow(msdata), function(x) list(
    masses =  as.numeric(unlist(strsplit(msdata$masses[x]," "))), 
    intensities = as.numeric(unlist(strsplit(msdata$intensities[x], " "))),
    totalion = sum(as.numeric(unlist(strsplit(msdata$intensities[x], " ")))),
    baseion = msdata$baseion[x],
    base_int = msdata$base_int[x],
    time = msdata$scantime[x], 
    mslevel = msdata$ms_n[x]))
  pt_ms1 <- create_peak_table_ms1(pl, mass = searchmzml$search_df$precursormz, masserror = searchmzml$search_df$masserror, minerror = searchmzml$search_df$minerror)
  pt_ms2 <- create_peak_table_ms2(pl, mass = searchmzml$search_df$precursormz, masserror = searchmzml$search_df$masserror, minerror = searchmzml$search_df$minerror)
  list(pt_ms1 = pt_ms1, pt_ms2 = pt_ms2, search_df = searchmzml$search_df)
}


#' Generate uncertainty mass spectrum for MS1 and MS2 data
#'
#' @param searchobj list object generated from `get_search-object`
#' @param correl correlation limit for ions to MS1
#' @param ph peak height to select scans for generating mass spectrum
#' @param freq observational frequency minimum for ions to use for generating mass spectrum
#' @param normfn normalization function, options are "sum" or "mean"
#' @param cormethod correlation function, default is "pearson"
#'
#' @return list object containing the ms1 uncertainty mass spectrum `ums1`, ms2 uncertainty mass spectrum `ums2` and respective uncertainty mass spectrum parameters `ms1params` and `ms2params`
#' @export
create_search_ms <- function(searchobj, correl = NULL, ph = NULL, freq = NULL, normfn = "sum", cormethod = "pearson") {
  ms1 <- NULL
  ms2 <- NULL
  ms1params <- list(correl = correl, ph = ph, freq = freq, normfn = normfn, cormethod = cormethod)
  ms2params <- list(correl = correl, ph = ph, freq = freq, normfn = normfn, cormethod = cormethod)
  ms1 <- try(get_ums(searchobj$pt_ms1, correl, ph, freq, normfn, cormethod), silent = TRUE)
  ms2 <- try(get_ums(searchobj$pt_ms2, correl, ph, freq, normfn, cormethod), silent = TRUE)
  #failsafe, for now to perform ums
  if (is.null(ms1) | class(ms1) == "try-error" | nrow(ms1) == 0) {
    ms1params <- optimal_ums(searchobj$pt_ms1, max_correl = correl, max_ph = ph, max_freq = freq, cormethod = cormethod)
    ms1 <- get_ums(searchobj$pt_ms1, as.numeric(ms1params["correl"]), as.numeric(ms1params["ph"]), as.numeric(ms1params["freq"]), normfn, cormethod)
  }
  if (is.null(ms2) | class(ms2) == "try-error" | nrow(ms2) == 0) {
    ms2params <- optimal_ums(searchobj$pt_ms2, max_correl = correl, max_ph = ph, max_freq = freq, cormethod = cormethod)
    ms2 <- get_ums(searchobj$pt_ms2, as.numeric(ms2params["correl"]), as.numeric(ms2params["ph"]), as.numeric(ms2params["freq"]), normfn, cormethod)
  }
  list(ums1 = ms1, ms1params = ms1params, ums2 = ms2, ms2params = ms2params, search_df = searchobj$search_df)
}

#' Get all mass spectral data with a specific precursor ion
#'
#' @param con SQLite database connection
#' @param precursorion numeric precursor ion m/z value
#' @param masserror numeric relative mass error (ppm)
#' @param minerror numeric minimum mass error (Da)
#'
#' @return data.frame of mass spectral data
#' @export
get_msdata_precursors <- function(con, precursorion, masserror, minerror) {
  stopifnot(
    DBI::dbIsValid(con),
    length(precursorion) > 0,
    all(precursorion == as.numeric(precursorion)),
    length(masserror) == 1,
    masserror == as.numeric(masserror),
    length(minerror) == 1,
    minerror == as.numeric(minerror)
  )
  precursorion <- as.numeric(precursorion)
  masserror <- as.numeric(masserror)
  minerror <- as.numeric(minerror)
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT * FROM ms_data
      LEFT JOIN peaks ON ms_data.peak_id = peaks.id
      WHERE peaks.precursor_mz BETWEEN ", 
      precursorion - max(precursorion*masserror*10^-6,minerror),
      " AND ",
      precursorion + max(precursorion*masserror*10^-6,minerror)
    )
  )
}

#' Get all mass spectral data for a specific compound
#'
#' @param con SQLite database connection
#' @param compoundid integer compound ID value
#'
#' @return data.frame of mass spectral data
#' @export
#'
#' @usage
#' get_msdata_compound(con, 15)
get_msdata_compound <- function(con, compoundid) {
  stopifnot(DBI::dbIsValid(con), all(compoundid == as.integer(as.numeric(compoundid))))
  compoundid <- as.integer(as.numeric(compoundid))
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT * FROM ms_data 
      LEFT JOIN peaks ON ms_data.peak_id = peaks.id 
      LEFT JOIN compound_fragments ON peaks.id = compound_fragments.peak_id 
      WHERE compound_fragments.compound_id = ", compoundid
    )
  )
}

#' Get all mass spectral data for a specific peak id
#'
#' @param con  SQLite database connection
#' @param peakid integer vector of peak ids
#'
#' @return data.frame of mass spectral data
#' @export
#'
#' @usage
#' get_msdata_peakid(con, 15)
get_msdata_peakid <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT * FROM ms_data
      WHERE ms_data.peak_id IN (", paste(peakid, collapse = ","), ")"
    )
  )
}

#' Get all annotated fragments have matching masses
#'
#' @param con SQLite database connection
#' @param fragmentions numeric vector containing m/z values for fragments to search
#' @param masserror numeric relative mass error (ppm)
#' @param minerror numeric minimum mass error (Da) 
#'
#' @return data.frame of mass spectral data
#' @export
get_annotated_fragments <- function(con, fragmentions, masserror, minerror) {
  stopifnot(
    DBI::dbIsValid(con),
    length(fragmentions) > 0,
    all(fragmentions == as.numeric(fragmentions)),
    length(masserror) == 1,
    masserror == as.numeric(masserror),
    length(minerror) == 1,
    minerror == as.numeric(minerror)
  )
  fragmentions <- as.numeric(fragmentions)
  masserror <- as.numeric(masserror)
  minerror <- as.numeric(minerror)
  do.call(rbind, lapply(fragmentions, function(ion) 
    DBI::dbGetQuery(
      conn = con,
      paste0(
        "SELECT * FROM norm_fragments
      WHERE norm_fragments.fixedmass BETWEEN ", 
        ion - max(ion*masserror*10^-6,minerror),
        " AND ",
        ion + max(ion*masserror*10^-6,minerror)
      )
    )
  ))
}

#' Get all fragments associated with compounds
#'
#' @param con SQLite database connection
#' @param fragmentions numeric vector containing m/z values for fragments to search
#' @param masserror numeric relative mass error (ppm)
#' @param minerror numeric minimum mass error (Da)  
#'
#' @return data.frame object describing known fragments in the database with known compound and peak references attached
#' @export
get_compound_fragments <- function(con, fragmentions, masserror, minerror) {
  stopifnot(
    DBI::dbIsValid(con),
    length(fragmentions) > 0,
    all(fragmentions == as.numeric(fragmentions)),
    length(masserror) == 1,
    masserror == as.numeric(masserror),
    length(minerror) == 1,
    minerror == as.numeric(minerror)
  )
  fragmentions <- as.numeric(fragmentions)
  masserror <- as.numeric(masserror)
  minerror <- as.numeric(minerror)
  do.call(rbind, lapply(fragmentions, function(ion) 
    DBI::dbGetQuery(
      conn = con,
      statement = DBI::sqlInterpolate(
        conn = con,
        sql = "
          select nf.id as norm_fragment_id, nf.fixedmass, nf.netcharge, nf.formula, nf.radical, nf.smiles, cf.peak_id, cf.compound_id, cf.annotated_fragment_id
          from norm_fragments nf
          left join annotated_fragments af on nf.id = af.fragment_id
          left join compound_fragments cf ON af.id = cf.annotated_fragment_id
          where nf.fixedmass between ?mz_lo and ?mz_hi
        ",
        mz_lo = ion - max(ion*masserror*10^-6,minerror),
        mz_hi = ion + max(ion*masserror*10^-6,minerror)
      )
    )
  ))
}

#' Get all mass spectral data within the database
#'
#' @param con SQLite database connection 
#'
#' @return data.frame of mass spectral data
#' @export
#'
get_msdata <- function(con) {
  stopifnot(DBI::dbIsValid(con))
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT * FROM ms_data"
    )
  )
}

#' Get precursor ion m/z for a specific peak
#'
#' @param con  SQLite database connection
#' @param peakid integer primary key for peaks table
#'
#' @return numeric value of precursor ion m/z value
#' @export
get_peak_precursor <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  stopifnot(all(peakid %in% DBI::dbGetQuery(con, "select id from peaks")[[1]]))
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT precursor_mz FROM peaks 
      WHERE peaks.id =", peakid
    )
  )
}



#' Get annotated fragments for a specific peak
#'
#' @param con  SQLite database connection
#' @param peakid integer vector of primary keys for peaks table
#'
#' @return data.frame of annotated fragments
#' @export
get_peak_fragments <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  stopifnot(all(peakid %in% DBI::dbGetQuery(con, "select id from peaks")[[1]]))
  DBI::dbGetQuery(
    conn = con,
    paste0(
      "SELECT peak_id, fixedmass, formula, radical, smiles, GROUP_CONCAT(citation) AS citations FROM compound_fragments 
      LEFT JOIN annotated_fragments ON compound_fragments.annotated_fragment_id = annotated_fragments.id
      LEFT JOIN norm_fragments ON annotated_fragments.fragment_id = norm_fragments.id 
      LEFT JOIN fragment_sources ON annotated_fragments.id = fragment_sources.annotated_fragments_id
      WHERE compound_fragments.peak_id IN (",
      paste(peakid, collapse = ","),
      ")
      GROUP BY peak_id, norm_fragments.id"
    )
  )
}

#' Determine number of matching fragments between unknown mass spectrum and specific peaks
#'
#' @param con SQLite database connection 
#' @param ums uncertainty mass spectrum of unknown compound
#' @param peakid integer vector of primary keys for peaks table
#' @param masserror numeric relative mass error (ppm)
#' @param minerror numeric minimum mass error (Da) 
#'
#' @return table of fragments and TRUE/FALSE for if the fragment is within the unknown mass spectrum
#' @export
check_fragments <- function(con, ums, peakid, masserror = 5, minerror = 0.001) {
  stopifnot(
    DBI::dbIsValid(con),
    length(peakid) > 0,
    all(peakid == as.integer(as.numeric(peakid))),
    length(masserror) == 1,
    masserror == as.numeric(masserror),
    length(minerror) == 1,
    minerror == as.numeric(minerror)
  )
  peakid <- as.integer(as.numeric(peakid))
  masserror <- as.numeric(masserror)
  minerror <- as.numeric(minerror)
  peak_fragments <- get_peak_fragments(con, peakid)
  results <- rep(FALSE, nrow(peak_fragments))
  for (i in 1:nrow(peak_fragments)) {
    if (!is.na(peak_fragments$fixedmass[i] | peak_fragments$fixedmass[i] != "NA")) {
      matched_ind <-  which(ums$mz >= peak_fragments$fixedmass[i] - max(peak_fragments$fixedmass[i]*masserror*1E-6,minerror) & ums$mz <= peak_fragments$fixedmass[i] + max(peak_fragments$fixedmass[i]*masserror*1E-6,minerror))
      results[i] <- length(matched_ind) > 0
    }
  }
  data.frame(peak_fragments, results)
}

#' Summarize results of check_fragments function
#'
#' @param fragments_checked output of `check_fragments` function
#'
#' @return table summary of check_fragments function
#' @export
summarize_check_fragments <- function(fragments_checked) {
  summarized <- data.frame(total_ann_fragments = rep(0, length(unique(fragments_checked$peak_id))),
                           total_ann_structures = rep(0, length(unique(fragments_checked$peak_id))),
                           total_ann_citations = rep(0, length(unique(fragments_checked$peak_id))))
  for (i in 1:length(unique(fragments_checked$peak_id))) {
    peak = unique(fragments_checked$peak_id)[i]
    peak_sum <- fragments_checked[which(fragments_checked$peak_id == peak),]
    summarized[i,] <- c(total_ann_fragments = length(which(peak_sum$results == TRUE)),
                        total_ann_structures = length(which(peak_sum$results == TRUE & !is.na(peak_sum$smiles))),
                        total_ann_citations = length(which(peak_sum$results == TRUE & !is.na(peak_sum$citations) & peak_sum$citations != "USER")))
  }
  summarized
}

#' Get sample class information for specific peaks
#'
#' @param con SQLite database connection 
#' @param peakid integer vector of primary keys for peaks table
#'
#' @return data.frame object of sample classes associated with a given peak
#' @export
get_sample_class <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  DBI::dbGetQuery(conn = con, 
                  paste0(
                    "SELECT norm_sample_classes.name AS sample_class FROM samples
                    LEFT JOIN norm_sample_classes ON samples.sample_class_id = norm_sample_classes.id
                    LEFT JOIN peaks ON samples.id = peaks.sample_id
                    WHERE peaks.id IN (",
                    paste(peakid, collapse = ","),
                    ")"
                  ))
}

#' Get compound ID and name for specific peaks
#'
#' @param con SQLite database connection 
#' @param peakid integer vector of primary keys for peaks table
#'
#' @return table of compound IDs and names
#' @export
get_compoundid <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  DBI::dbGetQuery(conn = con, 
                  paste0(
                    "SELECT view_compounds.id AS compound_id, view_compounds.name AS name FROM view_compounds
                    LEFT JOIN compound_fragments ON view_compounds.id = compound_fragments.compound_id
                    LEFT JOIN peaks ON compound_fragments.peak_id = peaks.id
                    WHERE peaks.id IN (",
                    paste(peakid, collapse = ","),
                    ")
                    GROUP By peaks.id"
                  ))
}

get_errorinfo <- function(con, peakid) {
  stopifnot(DBI::dbIsValid(con), all(peakid == as.integer(as.numeric(peakid))))
  peakid <- as.integer(as.numeric(peakid))
  DBI::dbGetQuery(conn = con,
                  paste0(
                    "SELECT * FROM view_masserror
                    WHERE peak_id IN (",
                    paste(peakid, collapse = ","),
                    ")"
                  ))
}

#' Search the database for all compounds with matching precursor ion m/z values
#'
#' @inheritParams get_ums
#'
#' @param con SQLite database connection
#' @param searchms object generated from `create_search_ms` function
#' @param optimized_params LGL scalar indicating whether or not to use the
#'   optimal search parameters stored in the database table `opt_ums_params`
#'
#' @return table of match statistics for the compound of interest
#' @export
search_precursor <- function(con, searchms, normfn = "sum", cormethod = "pearson", optimized_params = TRUE) {
  msdata <- get_msdata_precursors(con, searchms$search_df$precursormz, searchms$search_df$masserror, searchms$search_df$minerror)
  peak_ids <- unique(msdata$peak_id)
  if (length(peak_ids) == 0) {
    return(list(result = data.frame(ms1match.scores = NULL, ms2match.scores = NULL, peak_sum = NULL, sample_classes = NULL, peak_ids = NULL, compounds = NULL),
                ums2_compare = list(),
                ums1_compare = list()))
  }
  mslist <- lapply(peak_ids, function(x) create_peak_list(msdata[which(msdata$peak_id == x),]))
  errorinfo <- get_errorinfo(con, peak_ids)
  opt_params <- get_opt_params(con, peak_ids)
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
  ptms1 <- lapply(1:length(mslist),  function(x) create_peak_table_ms1(mslist[[x]], mass = as.numeric(errorinfo$precursor_mz[x]), masserror = opt_params$masserror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)], minerror = opt_params$minerror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)]))
  l.ums1 <- lapply(1:length(ptms1), function(x) {
    get_ums(
      peaktable = ptms1[[x]],
      correl = opt_params$correl[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      ph = opt_params$ph[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      freq = opt_params$freq[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      normfn = normfn,
      cormethod = cormethod
    )
  })
  #match_compare algorithm
  ms2match.scores <- do.call(rbind, lapply(l.ums2, compare_ms, ms1 = searchms$ums2))
  names(ms2match.scores) <- paste0("ms2_", names(ms2match.scores))
  
  ms1match.scores <- do.call(rbind, lapply(l.ums1, compare_ms, ms1 = searchms$ums1))
  names(ms1match.scores) <- paste0("ms1_", names(ms1match.scores))
  
  #get possible fragments
  peak_fragments <- get_peak_fragments(con, peak_ids)
  fragments_checked <- check_fragments(con, searchms$ums2, peak_ids, masserror = searchms$search_df$masserror, minerror = searchms$search_df$minerror)
  peak_sum <- summarize_check_fragments(fragments_checked)
  
  #get sample class info
  sample_classes <- get_sample_class(con, peak_ids)
  
  #get compound identities
  compounds <- get_compoundid(con, peak_ids)
  
  #scoring report
  result <- data.frame(ms1match.scores, ms2match.scores, peak_sum, sample_classes, peak_ids, compounds)
  reorder <- order(rowSums(result[,1:4]), decreasing = TRUE)
  list(result = result[reorder,],
       ums2_compare = l.ums2[reorder],
       ums1_compare = l.ums1[reorder])
}

#' Search all mass spectra within database against unknown mass spectrum
#' 
#' @inheritParams get_ums
#'
#' @param con SQLite database connection 
#' @param searchms object generated from `create_search_ms` function 
#' @param optimized_params LGL scalar indicating whether or not to use the
#'   optimal search parameters stored in the database table `opt_ums_params`
#'
#' @return LIST of search results
#' @export
search_all <- function(con, searchms, normfn = "sum", cormethod = "pearson", optimized_params = TRUE) {
  msdata <- get_msdata(con)
  peak_ids <- unique(msdata$peak_id)
  if (length(peak_ids) == 0) {
    return(list(result = data.frame(ms1match.scores = NULL, ms2match.scores = NULL, peak_sum = NULL, sample_classes = NULL, peak_ids = NULL, compounds = NULL),
                ums2_compare = list(),
                ums1_compare = list()))
  }
  mslist <- lapply(peak_ids, function(x) create_peak_list(msdata[which(msdata$peak_id == x),]))
  errorinfo <- get_errorinfo(con, peak_ids)
  opt_params <- get_opt_params(con, peak_ids)
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
  ptms1 <- lapply(1:length(mslist),  function(x) create_peak_table_ms1(mslist[[x]], mass = as.numeric(errorinfo$precursor_mz[x]), masserror = opt_params$masserror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)], minerror = opt_params$minerror[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 2)]))
  l.ums1 <- lapply(1:length(ptms1), function(x) {
    get_ums(
      peaktable = ptms1[[x]],
      correl = opt_params$correl[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      ph = opt_params$ph[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      freq = opt_params$freq[which(opt_params$peak_id == peak_ids[x] & opt_params$mslevel == 1)],
      normfn = normfn,
      cormethod = cormethod
    )
  })
  
  #match_compare algorithm
  ms2match.scores <- do.call(rbind, lapply(l.ums2, compare_ms, ms1 = searchms$ums2))
  names(ms2match.scores) <- paste0("ms2_", names(ms2match.scores))
  
  ms1match.scores <- do.call(rbind, lapply(l.ums1, compare_ms, ms1 = searchms$ums1))
  names(ms1match.scores) <- paste0("ms1_", names(ms1match.scores))
  
  #get possible fragments
  peak_fragments <- get_peak_fragments(con, peak_ids)
  fragments_checked <- check_fragments(con, searchms$ums2, peak_ids, masserror = searchms$search_df$masserror, minerror = searchms$search_df$minerror)
  peak_sum <- summarize_check_fragments(fragments_checked)
  
  #get sample class info
  sample_classes <- get_sample_class(con, peak_ids)
  
  #get compound identities
  compounds <- get_compoundid(con, peak_ids)
  
  #scoring report, still doesn't work due to db issues 06152022 BJP
  result <- data.frame(ms1match.scores, ms2match.scores, peak_sum, sample_classes, peak_ids, compounds, row.names = NULL)
  reorder <- order(rowSums(result[,1:4]), na.rm = TRUE, decreasing = TRUE)
  list(result = result[reorder,],
       ums2_compare = l.ums2[reorder],
       ums1_compare = l.ums1[reorder])
}
