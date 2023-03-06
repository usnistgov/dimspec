#' Generate standard chemical formula notation
#'
#' @param formula CHR string of an elemental formula
#'
#' @return string with a standard ordered formula
#'
#' @examples
#' 
#' formula <- "C10H15S1O3"
#' formulalize(formula)

formulalize <- function(formula) {
  extract_formula <- extract.elements(formula) #from qualitycontrol functions
  if ("C" %in% extract_formula$elements) {
    c_wo_c <- extract_formula$counts[-which(extract_formula$elements == "C")]
    e_wo_c <- extract_formula$elements[-which(extract_formula$elements == "C")]
    elements <- c("C", e_wo_c[order(e_wo_c)])
    counts <- c(extract_formula$counts[which(extract_formula$elements == "C")], c_wo_c[order(e_wo_c)])
  }
  if (!"C" %in% extract_formula$elements) {
    elements <- extract_formula$elements[order(extract_formula$elements)]
    counts <- extract_formula$counts[order(extract_formula$elements)]
  }
  paste(elements, counts, sep = "", collapse = "")
}

