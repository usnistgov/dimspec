is_elemental_subset <- function(fragmentformula, parentformula) {
  frag_list <- extract.elements(fragmentformula, remove.elements = c())
  par_list <- extract.elements(parentformula, remove.elements = c())
  if (length(which(frag_list$elements %in% par_list$elements)) < length(frag_list$elements)) {return(FALSE)}
  if (length(which(frag_list$elements %in% par_list$elements)) == length(frag_list$elements)) {
    count_check <- sapply(frag_list$elements, function(x) frag_list$counts[which(frag_list$elements == x)] <= par_list$counts[which(par_list$elements == x)])
    if (FALSE %in% count_check) {return(FALSE)}
    if (!FALSE %in% count_check) {return(TRUE)}
  }
}