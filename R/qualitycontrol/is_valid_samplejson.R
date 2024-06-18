is_valid_samplejson <- function(samplejson, dbconn = con) {
  errors <- character(0L)
  msg <- character(0L)
  
  #json structure check
  open_json <- try(parse_methodjson(samplejson))
  if (class(open_json) == "try-error") {
    msg <- "The file does not have a valid JSON structure."
    errors <- c(errors, msg)
    warning(msg)
  }
  
  #adduct check
  
  adducts <- sapply(open_json$peaks, function(x) x$ionstate)
  if (length(adducts) > 0) {
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
      msg <- "No database connection available."
      errors <- c(errors, msg)
      warning(msg)
    }
    if (FALSE %in% (adducts %in% known_adducts$name)) {
      msg <- "There is at least one ion state that is not in the current database."
      errors <- c(errors, "There is at least one ion state that is not in the current database.")
      warning(msg)
    }
  }
  
  # smiles check validity and match against fragment_formula
  if ("annotation" %in% names(open_json) && "fragment_SMILES" %in% names(open_json$annotation)) {
    smiles <- open_json$annotation$fragment_SMILES
    checks <- lapply(smiles, \(x) try(smilestoformula(x)))
    failures <- which(sapply(checks, inherits, what = "try-error"))
    smiles_failures <- character(0L)
    if (length(failures) > 0) {
      smiles_failures <- smiles[failures]
      msg <- sprintf("There were %d annotated SMILES that could not be verified: %s", length(failures), str_flatten_comma(smiles_failures, last = ", and "))
      errors <- c(errors, msg)
      warning(msg)
      checks <- checks[-failures]
    }
    checks <- bind_rows(checks)
    formulas <- open_json$annotation$fragment_formula
    formula_mismatches <- !formulas == checks$FORMULA
    if (any(formula_mismatches)) {
      formula_mismatches <- formulas[formula_mismatches]
      n_mismatches <- sum(formula_mismatches)
      msg <- sprintf("There were %d annotated formula mismatch%s among valid SMILES.", n_mismatches, ifelse(n_mismatches > 1, "es", ""))
      errors <- c(errors, msg)
      warning(msg)
    }
  }
  out <- length(errors) == 0
  if (!out) attr(out, "errors") <- errors
  return(out)
}