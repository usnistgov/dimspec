is_valid_samplejson <- function(samplejson, dbconn = con) {
  #json structure check
  open_json <- try(parse_methodjson(samplejson))
  if (class(open_json) == "try-error") {
    error("The file does not have a valid JSON structure.")
    return(FALSE)
  }
  
  #adduct check
  
  adducts <- sapply(open_json$peaks, function(x) x$ionstate)
  if (exists(as.character(substitute(dbconn))) && DBI::dbIsValid(dbconn)) {
    known_adducts <- DBI::dbGetQuery(dbconn, "SELECT name FROM norm_ion_states")
  } else if (USE_API) {
    known_adducts <- api_endpoint(
      path = "table_search",
      table_name = "norm_ion_states",
      column_names = "name",
      return_format = "data.frame",
      single_column_as_vector = FALSE
    )
  } else {
    error("No database connection available.")
    return(FALSE)
  }
  if (FALSE %in% (adducts %in% known_adducts$name)) {
    error("There is at least one ion state that is not in the current database.")
    return(FALSE)
  }
  
  #smiles check
  if (length(open_json$annotation) > 0) {
    all_SMILES <- do.call(c, lapply(open_json$annotation, function(x) sapply(which(names(x) == "fragment"), function(y) x[[y]]$fragment_SMILES)))
    for (i in 1:length(all_SMILES)) {
      smile <- try(smilestoformula(all_SMILES[i]))
      if (class(smile) == "try-error") {
        error(paste("There is at least one invalid SMILES (", all_SMILES[i], ")"))
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}