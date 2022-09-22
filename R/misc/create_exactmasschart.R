create_exactmasschart <- function(exactmasstable = NULL, con = NULL) {
  if (!is.null(con)) {
    exactmasstable <- DBI::dbGetQuery(con, "SELECT * FROM view_element_isotopes")
  }
  exactmasstable %>% group_by(symbol) %>% summarize(count = n()) -> ecounts
  maxcol <- max(ecounts$count)
  elements <- unique(exactmasstable$symbol)
  masses <- matrix(NA, nrow = length(elements), ncol = maxcol)
  abundances <- matrix(NA, nrow = length(elements), ncol = maxcol)
  for (i in 1:length(elements)) {
    add_ab <- exactmasstable$abundance[which(exactmasstable$symbol == elements[i])]
    add_mass <- exactmasstable$exact_mass[which(exactmasstable$symbol == elements[i])]
    abundances[i, 1:length(add_ab)] <- add_ab
    masses[i, 1:length(add_mass)] <- add_mass
  }
  list(elements = elements, masses = masses, abundances = abundances)
}