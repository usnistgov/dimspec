get_COMPOUNDID <- function(value, header = "INCHIKEY", compounds) {
  if (!header %in% colnames(compounds)) {return(NA)}
  ind <- which(compounds[,header] == value)
  if (length(ind) == 0) {return(NA)}
  CID <- compounds[ind,"COMPOUND_ID"]
  CID
} 