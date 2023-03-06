#' Convert SMILES string to Formula and other information
#'
#' The function converts SMILES strings into a data frame containing the molecular
#' formula (FORMULA), fixed mass of the formula (FIXED MASS), and the net charge (NETCHARGE).
#'
#' @param SMILES vector of SMILES strings
#'
#' @return data frame
#' @export
#'
#' @examples
#' smilestoformula(c("CCCC", "C(F)(F)F"))
#' 
#' smilestoformula("CCCC")

smilestoformula <- function(SMILES) {
#if rdkit has not been installed, must use package rcdk
  if (ifelse(exists("INFORMATICS"), INFORMATICS, TRUE)) {
    if (ifelse(exists("USE_RDKIT"), !USE_RDKIT, TRUE)) {
      molecule <- rcdk::parse.smiles(SMILES)[[1]]
      FIXEDMASS <- rcdk::get.exact.mass(molecule)
      NETCHARGE <- rcdk::get.total.charge(molecule)
      FORMULA <- rcdk::get.mol2formula(molecule, NETCHARGE)@string
      FORMULA <- gsub("-", "", gsub("\\+", "", gsub("\\[", "", gsub("\\]", "", FORMULA))))
    }
  }
  #If rdkit has been installed and "rdk" object exists
    if (ifelse(exists("USE_RDKIT"), USE_RDKIT, TRUE)) {
      if (exists("rdk")) {
        m <- lapply(SMILES, rdk$Chem$MolFromSmiles)
        FIXEDMASS <- sapply(m, rdk$Chem$Descriptors$ExactMolWt)
        NETCHARGE <- sapply(m, rdk$Chem$GetFormalCharge)
        FORMULA <- sapply(m, rdk$Chem$rdMolDescriptors$CalcMolFormula)
        FORMULA <- gsub("-", "", gsub("\\+", "", gsub("\\[", "", gsub("\\]", "", FORMULA))))
      }
  }
  data.frame(SMILES, FORMULA, FIXEDMASS, NETCHARGE)
}
