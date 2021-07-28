is_elemental_match <- function(testformula, trueformula) {
  test_list <- extract.elements(testformula, remove.elements = c())
  true_list <- extract.elements(trueformula, remove.elements = c())
  if (length(which(test_list$elements %in% true_list$elements)) < length(test_list$elements)) {return(FALSE)}
  if (length(which(test_list$elements %in% true_list$elements)) == length(test_list$elements)) {
    count_check <- sapply(test_list$elements, function(x) test_list$counts[which(test_list$elements == x)] == true_list$counts[which(true_list$elements == x)])
    if (FALSE %in% count_check) {return(FALSE)}
    if (!FALSE %in% count_check) {return(TRUE)}
  }
}