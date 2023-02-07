is_valid_samplejson <- function(samplejson, dbconn = con) {
  #json structure check
  open_json <- try(parse_methodjson(samplejson))
  if (class(open_json) == "try-error") stop("The file does not have a valid JSON structure.")
  
  #adduct check
  
  adducts <- sapply(open_json$peaks, function(x) x$ionstate)
  known_adducts <- DBI::dbGetQuery(dbconn, "SELECT name FROM norm_ion_states")
  if (FALSE %in% (adducts %in% known_adducts$name)) stop("There is at least one ion state that is not in the current database.")
  
  #smiles check
  if (length(open_json$annotation) > 0) {
    all_SMILES <- do.call(c, lapply(open_json$annotation, function(x) sapply(which(names(x) == "fragment"), function(y) x[[y]]$fragment_SMILES)))
    for (i in 1:length(all_SMILES)) {
      smile <- try(smilestoformula(all_SMILES[i]))
      if (class(smile) == "try-error") stop(paste("There is at least one invalid SMILES (", all_SMILES[i], ")"))
    }
    
  }
  return(TRUE)
}