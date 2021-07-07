source('src/qualitycontrol/elementalcomposition_calc.R')
source('src/base/peaktable2.R')
source('src/qualitycontrol/check_isotopedist.R')
source('src/qualitycontrol/smilestoformula.R')
source('src/misc/dotproduct_ms.R')

exactmasschart <- readRDS('src/qualitycontrol/exactmasschart.RDS')

gather_qc <- function(gather_peak, exactmasses, ms1range = c(0.5, 3), ms1isomatchlimit = 0.2) {
  #performs quality check on the submitted data and adds 'check' list item
  check <- list()
  #check 1: is the compound identified accurate to the measured m/z
  true_compound <- gather_peak$compounddata
  true_compound_form <- true_compound$FORMULA
  adduct <- gather_peak$peak$ionstate
  if (adduct == "[M+H]+") {adduct = "+H"; charge = "positive"}
  if (adduct == "[M-H]-") {adduct = "-H"; charge = "negative"}
  true_compound_ion_form <- adduct_formula(true_compound_form, adduct)
  true_compound_mz <- calculate.monoisotope(extract.elements(true_compound_form), exactmasses, adduct = adduct)
  measurederror <- (1E6)*(as.numeric(gather_peak$peak$mz) - true_compound_mz)/true_compound_mz
  result <- abs(measurederror) <= as.numeric(gather_peak$massspectrometry$msaccuracy)
  check<- data.frame(parameter = "measurederror", value = measurederror, result = result)
  
  #check 2: calculate the match score of the MS1 isotopic pattern
  ms1list <- lapply(gather_peak$msdata, function(x) {if (x$msn == 1) {matrix(unzip(x$msdata), ncol = 2, byrow = TRUE)}})
  ms1list <- lapply(ms1list, function(x) x[which(x[,1] >= as.numeric(gather_peak$peak$mz) - ms1range[1] & x[,1] <= as.numeric(gather_peak$peak$mz) + ms1range[2]),])
  ms1list <- ms1list[-which(sapply(lapply(ms1list, nrow), is.null))]
  ms1list <- lapply(ms1list, zipms)
  ms1empirical <- peaktable(ms1list, masserror = as.numeric(gather_peak$massspectrometry$msaccuracy))
  ms1empirical <- data.frame(mz = rowMeans(ms1empirical$mass, na.rm = TRUE), int = rowMeans(ms1empirical$int, na.rm = TRUE))
  ms1match <- check_isotopedist(ms1empirical, true_compound_ion_form, exactmasschart, error = as.numeric(gather_peak$massspectrometry$msaccuracy), minerror = 0.002, max.dist = ms1range[2], min.int = 0.001, charge = charge, m = 1, n = 0.5)
  matchscore <- as.numeric(ms1match[2])
  result <- matchscore >= ms1isomatchlimit
  check <- rbind(check, c(parameter = "ms1_isotopepattern", value = matchscore, result = result))
  
  #check 3: is the provided precursor ion m/z in the MS1 spectrum?
  value <- length(which(ms1empirical$mz >= as.numeric(gather_peak$peak$mz) - max(as.numeric(gather_peak$peak$mz)*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, 0.002) & ms1empirical$mz <= gather_peak$peak$mz + max(as.numeric(gather_peak$peak$mz)*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, 0.002))) > 0 
  result <- TRUE
  if (value == FALSE) {return <- FALSE}
  check <- rbind(check, c(parameter = "ms1precursor_detected", value = value, result = result))
  
  #check 4: do the annotated fragments appear in the MS2 spectra (average)
  if (is.null(gather_peak$annotation)) {check <- rbind(check, c(parameter = "annfragments_detected", value = NA, result = NA))}
  if (!is.null(gather_peak$annotation)) {
    ms2list <- lapply(gather_peak$msdata, function(x) {if (x$msn == 2) {x$msdata}})
    ms2list <- ms2list[-which(sapply(ms2list, function(x) length(nchar(x)) == 0))]
    ms2empirical <- peaktable(ms2list, masserror = as.numeric(gather_peak$massspectrometry$msaccuracy))
    ms2empirical <- data.frame(mz = rowMeans(ms2empirical$mass, na.rm = TRUE), int = rowMeans(ms2empirical$int, na.rm = TRUE))
    value <- sapply(gather_peak$annotation$fragment_mz, function(x) length(which(ms2empirical$mz >= x - max(x*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6,0.002) & ms2empirical$mz <= x + max(x*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6,0.002))) > 0)
    result <- TRUE
    if (FALSE %in% value) {result <- FALSE}
    value <- paste(value, collapse = " ")
    check <- rbind(check, c(parameter = "annfragments_detected", value = value, result = result))
  }
  
  #check 5: calculate mass accuracy of the annotated fragments
  if (is.null(gather_peak$annotation)) {check <- rbind(check, c(parameter = "annfragments_accuracy", value = NA, result = NA))}
  if (!is.null(gather_peak$annotation)) {
    mz <- gather_peak$annotation$fragment_mz
    if (gather_peak$massspectrometry$polarity == "negative") {charge <- "-"}
    if (gather_peak$massspectrometry$polarity == "positive") {charge <- "+"}
    adducts <- paste(charge, sapply(1:nrow(gather_peak$annotation), function(x) {
      if (gather_peak$annotation$fragment_radical[x] == TRUE) {return("radical")}
      if (gather_peak$annotation$fragment_radical[x] == FALSE) {return("")}
    }), sep = "")
    calculated_mz <- sapply(1:nrow(gather_peak$annotation), function(x) calculate.monoisotope(extract.elements(gather_peak$annotation$fragment_formula[x]), exactmasses, adducts[x]))
    value <- sapply(1:length(mz), function(x) {mz[x] >= calculated_mz[x] - max(calculated_mz[x]*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, 0.002) & mz[x] <= calculated_mz[x] + max(calculated_mz[x]*as.numeric(gather_peak$massspectrometry$msaccuracy)*1E-6, 0.002)})
    result <- TRUE
    if (FALSE %in% value) {result <- FALSE}
    value <- paste(value, collapse = " ")
    check <- rbind(check, c(parameter = "annfragments_accuracy", value = value, result = result))
  }
  
  #check 6: are the fragments subsets of the parent structure
  fragment_form <- gather_peak$annotation$fragment_formula
  value <- sapply(fragment_form, function(x) is_elemental_subset(x, gather_peak$compounddata$FORMULA))
  result <- TRUE
  if (FALSE %in% value) {result <- FALSE}
  value <- paste(value, collapse = " ")
  check <- rbind(check, c(parameter = "annfragments_subset", value = value, result = result))
  
  #check 7: do the structures give the same formula as the proposed formula?
  fragment_form <- gather_peak$annotation$fragment_formula
  fragment_smiles <- gather_peak$annotation$fragment_SMILES
  value <- c()
  for (i in 1:length(fragment_smiles)) {
    if (is.null(fragment_smiles[i])) {value <- c(value, NA)}
    if (!is.null(fragment_smiles[i])) {
      smiles_formula <- smilestoformula(fragment_smiles[i])
      value <- c(value, is_elemental_match(fragment_smiles[i], fragment_form[i]))
    }
  }
  result <- TRUE
  if (FALSE %in% value) {result <- FALSE}
  value <- paste(value, collapse = " ")
  check <- rbind(check, c(parameter = "annfragments_elementalmatch", value = value, result = result))
  
  #return results
  check
}