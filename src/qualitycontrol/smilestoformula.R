library("ChemmineR")
library("rcdk")

smilestoformula <- function(SMILES) {
  molecule <- parse.smiles(SMILES)[[1]]
  FIXEDMASS <- get.exact.mass(molecule)
  NETCHARGE <- get.total.charge(molecule)
  FORMULA <- get.mol2formula(molecule, NETCHARGE)@string
  FORMULA <- gsub("-", "", gsub("\\+", "", gsub("\\[", "", gsub("\\]", "", FORMULA))))
  data.frame(FORMULA, FIXEDMASS, NETCHARGE)
}
