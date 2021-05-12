get_fragments <- function(cms, fragments, polarity = "positive", masserror = 5, minerror = 0.002) {
  if (polarity == "negative") {charge = "N"}
  if (polarity == "positive") {charge = "P"}
  match_fragments <- do.call(c, lapply(1:nrow(cms), function(i) which(fragments$FRAGMENT_EXACTMASS >= cms$mz[i] - max(minerror, cms$mz[i]*masserror*1E-6) & fragments$FRAGMENT_EXACTMASS <= cms$mz[i] + max(minerror, cms$mz[i]*masserror*1E-6) & fragments$FRAGMENT_CHARGE == charge)))
  data.frame(M.Z = fragments$FRAGMENT_EXACTMASS[match_fragments], FORMULA = fragments$FRAGMENT_FORMULA[match_fragments], RADICAL = gsub("N", "FALSE", gsub("Y", "TRUE", fragments$FRAGMENT_RADICAL[match_fragments])), SMILES = fragments$SMILES[match_fragments], SOURCE = fragments$EMPIRICAL_CITATION[match_fragments])
}