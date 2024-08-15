

#' Quality Control Check of Import Data
#' 
#' Performs the quality control check on the imported data from the peak gather function.
#'
#' @param gather_peak peak object generated from `peak_gather_json` function
#' @param exactmasses exactmasses list
#' @param ms1range 2-component vector containing stating the range to evaluate the isotopic pattern of the precursor ion, from mass - ms1range[1] to mass + ms1range[2]
#' @param ms1isomatchlimit the reverse dot product minimum score for the isotopic pattern match
#' @param minerror the minimum mass error (in Da) allowable for the instrument
#' @param max_correl [TODO PLACEHOLDER]
#' @param correl_bin [TODO PLACEHOLDER]
#' @param max_ph [TODO PLACEHOLDER]
#' @param ph_bin [TODO PLACEHOLDER]
#' @param max_freq [TODO PLACEHOLDER]
#' @param freq_bin [TODO PLACEHOLDER]
#' @param min_n_peaks [TODO PLACEHOLDER]
#' @param cormethod [TODO PLACEHOLDER]
#'
#' @return nested list of quality control check results
#' @export
#'
gather_qc <- function(gather_peak, exactmasses, exactmasschart, ms1range = c(0.5, 3), ms1isomatchlimit = 0.5, minerror = 0.002, max_correl = 0.8, correl_bin = 0.1, max_ph = 10, ph_bin = 1, max_freq = 10, freq_bin = 1, min_n_peaks = 3, cormethod = "pearson") {
  require(stringr)
  #performs quality check on the submitted data and adds 'check' list item
  check <- list()
  #check for minimum error setting within gather_peak
  if (!is.null(gather_peak$massspectrometry$msminerror)) {
    minerror <- as.numeric(gather_peak$massspectrometry$msminerror)
  }
  
  #check 1: is the compound identified accurate to the measured m/z
  true_compound <- gather_peak$compounddata
  true_compound_form <- true_compound$formula
  parentadduct <- gather_peak$peak$ionstate
  charge <- if (str_ends(parentadduct, "\\+")) {
    "positive"
  } else if (str_ends(parentadduct, "\\-")) {
    "negative"
  } else {
    "neutral"
  }
  adduct <- str_extract(parentadduct, "[\\-\\+][A-Za-z0-9]+")
  true_compound_ion_form <- adduct_formula(true_compound_form, adduct)
  true_compound_mz <- calculate.monoisotope(true_compound_form, exactmasses, adduct = adduct)
  measurederror <- (1E6)*(as.numeric(gather_peak$peak$mz) - true_compound_mz)/true_compound_mz
  result <- abs(measurederror) <= as.numeric(gather_peak$massspectrometry$msaccuracy)
  check[[length(check)+1]] <- data.frame(parameter = "measurederror", reportedmz = as.numeric(gather_peak$peak$mz), compoundmz = true_compound_mz, value = measurederror, limit = as.numeric(gather_peak$massspectrometry$msaccuracy), result = result)
  
  #check 2: calculate the match score of the MS1 isotopic pattern
  peaklist <- create_peak_list(gather_peak$msdata)
  ms1empirical <- create_peak_table_ms1(peaklist, mass = as.numeric(gather_peak$peak$mz), masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror, int0 = NA)
  ms1empirical <- data.frame(mz = rowMeans(ms1empirical$peaktable_mass, na.rm = TRUE), int = rowMeans(ms1empirical$peaktable_int, na.rm = TRUE))
  #comment: must adjust max range to fit within m/z range, ms1range - 1 will fit all ions
  ms1match <- check_isotopedist(ms1empirical, true_compound_ion_form, exactmasschart, error = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror, max.dist = ms1range[2]-1, min.int = 0.001, charge = charge, m = 1, n = 0.5)
  matchscore <- as.numeric(ms1match[2])
  result <- matchscore >= ms1isomatchlimit
  check[[length(check)+1]] <- data.frame(parameter = "ms1_isotopepattern", value = matchscore, limit = ms1isomatchlimit, result = result)
  
  #check 3: is the provided precursor ion m/z in the MS1 spectrum?
  ind <- which(ms1empirical$mz >= as.numeric(gather_peak$peak$mz) - max(as.numeric(gather_peak$peak$mz)*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, minerror) & ms1empirical$mz <= gather_peak$peak$mz + max(as.numeric(gather_peak$peak$mz)*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, minerror))
  value <- length(ind) > 0 
  if (value == FALSE) {
    result <- FALSE
    measuredmz <- NA
  }
  if (value == TRUE) {
    result <- TRUE
    measuredmz = ms1empirical$mz[ind]
  }
  check[[length(check)+1]] <- data.frame(parameter = "ms1precursor_detected", reportedmz = as.numeric(gather_peak$peak$mz), measuredmz = measuredmz, msaccuracy = as.numeric(gather_peak$massspectrometry$msaccuracy), value = value, result = result)
  
  #check 4: do the annotated fragments appear in the MS2 spectra (average)
  if (is.null(gather_peak$annotation) | length(gather_peak$annotation) == 0) {check[[length(check) + 1]] <- data.frame(parameter = "annfragments_detected", value = NA, result = NA)}
  if (!is.null(gather_peak$annotation) & length(gather_peak$annotation) > 0) {
    ms2empirical <- create_peak_table_ms2(peaklist,mass = as.numeric(gather_peak$peak$mz), masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror, int0 = NA)
    ms2empirical <- data.frame(mz = rowMeans(ms2empirical$peaktable_mass, na.rm = TRUE), int = rowMeans(ms2empirical$peaktable_int, na.rm = TRUE))
    matched_ind <- sapply(gather_peak$annotation$fragment_mz, function(x) which(ms2empirical$mz >= x - max(x*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6,minerror) & ms2empirical$mz <= x + max(x*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6,minerror))[1]) #sometimes two match.
    value <- sapply(matched_ind, function(x) length(x) > 0)
    result <- TRUE
    if (FALSE %in% value) {result <- FALSE}
    check[[length(check)+1]] <- data.frame(parameter = "annfragments_detected", reportedmz = as.numeric(gather_peak$annotation$fragment_mz), measuredmz = ms2empirical$mz[matched_ind], msaccuracy = as.numeric(gather_peak$massspectrometry$msaccuracy), value = value, result = result)
  }
  
  #check 5: calculate mass accuracy of the annotated fragments
  if (is.null(gather_peak$annotation) | length(gather_peak$annotation) == 0) {check[[length(check) + 1]] <- data.frame(parameter = "annfragments_accuracy", value = NA, result = NA)}
  if (!is.null(gather_peak$annotation) & length(gather_peak$annotation) > 0) {
    mz <- gather_peak$annotation$fragment_mz
    if (gather_peak$massspectrometry$polarity == "negative") {charge <- "-"}
    if (gather_peak$massspectrometry$polarity == "positive") {charge <- "+"}
    adducts <- paste(charge, sapply(1:nrow(gather_peak$annotation), function(x) {
      if (gather_peak$annotation$fragment_radical[x] == TRUE) {return("radical")}
      if (gather_peak$annotation$fragment_radical[x] == FALSE) {return("")}
      if (gather_peak$annotation$fragment_radical[x] %in% c("", "unknown")) {return("")}
    }), sep = "")
    calculated_mz <- sapply(1:nrow(gather_peak$annotation), function(x) calculate.monoisotope(gather_peak$annotation$fragment_formula[x], exactmasses, adducts[x]))
    massdiff <- sapply(1:length(mz), function(x) mz[x] - calculated_mz[x])
    value <- sapply(1:length(mz), function(x) ((mz[x] - calculated_mz[x])/mz[x])*1E6)
    result <- sapply(1:length(mz), function(x) abs(value[x]) <= as.numeric(gather_peak$massspectrometry$msaccuracy) | abs(massdiff[x]) <= minerror)
    check[[length(check)+1]] <- data.frame(parameter = "annfragments_accuracy", measuredmz = mz, calculatedmz = calculated_mz, msaccuracy = as.numeric(gather_peak$massspectrometry$msaccuracy), minmzerror = minerror, mzdiff = massdiff, error = value, result = result)
  }
  
  #check 6: are the fragments subsets of the parent structure
  if (is.null(gather_peak$annotation) | length(gather_peak$annotation) == 0) {check[[length(check) + 1]] <- data.frame(parameter = "annfragments_subset", value = NA, result = NA)}
  if (!is.null(gather_peak$annotation) & length(gather_peak$annotation) > 0) {
    fragment_form <- gather_peak$annotation$fragment_formula
    result <- sapply(fragment_form, function(x) is_elemental_subset(x, gather_peak$compounddata$formula))
    check[[length(check)+1]] <- data.frame(parameter = "annfragments_subset", reportedformula = fragment_form, parentformula = gather_peak$compounddata$formula, result = result, row.names = c())
  }
  #check 7: do the structures give the same formula as the proposed formula?
  if (is.null(gather_peak$annotation) | length(gather_peak$annotation) == 0) {check[[length(check) + 1]] <- data.frame(parameter = "annfragments_elementalmatch", value = NA, result = NA)}
  if (!is.null(gather_peak$annotation) & length(gather_peak$annotation) > 0) {
    fragment_form <- gather_peak$annotation$fragment_formula
    fragment_smiles <- gather_peak$annotation$fragment_SMILES
    value <- c()
    result <- c()
    for (i in 1:length(fragment_smiles)) {
      if (is.null(fragment_smiles[i]) | fragment_smiles[i] == "") {
        value <- c(value, NA)
        result <- c(result, NA)
      }
      if (!is.null(fragment_smiles[i]) & fragment_smiles[i] != "") {
        smiles_formula <- smilestoformula(fragment_smiles[i])$FORMULA
        value <- c(value, smiles_formula)
        result <- c(result, is_elemental_match(smiles_formula, fragment_form[i]))
      }
    }
    result <- TRUE
    if (FALSE %in% value) {result <- FALSE}
    check[[length(check)+1]] <- data.frame(parameter = "annfragments_elementalmatch", reportedformula = fragment_form, reported_smiles = fragment_smiles, calculatedformula = value, result = result)
  }
  
  #added 06142022, get optimal ums settings
  ms1empirical <- create_peak_table_ms1(peaklist, mass = as.numeric(gather_peak$peak$mz), masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror, int0 = NA)
  opt_ums1_params <- optimal_ums(ms1empirical, max_correl = max_correl, correl_bin = correl_bin, max_ph = max_ph, ph_bin = ph_bin, max_freq = max_freq, freq_bin = freq_bin, min_n_peaks = min_n_peaks, cormethod = cormethod)
  opt_ums1_params <- c(opt_ums1_params, masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror)
  ms2empirical <- create_peak_table_ms2(peaklist,mass = as.numeric(gather_peak$peak$mz), masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror, int0 = NA)
  opt_ums2_params <- optimal_ums(ms2empirical, max_correl = max_correl, correl_bin = correl_bin, max_ph = max_ph, ph_bin = ph_bin, max_freq = max_freq, freq_bin = freq_bin, min_n_peaks = min_n_peaks, cormethod = cormethod)
  opt_ums2_params <- c(opt_ums2_params, masserror = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = minerror)
  opt_ums_params <- data.frame(parameter = "optimized_ums_parameters", mslevel = c(1,2), rbind(opt_ums1_params, opt_ums2_params))
  
  #return results
  
  list(check = check, opt_ums_params = opt_ums_params)
}
