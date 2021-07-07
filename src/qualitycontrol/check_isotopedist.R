check_isotopedist <- function(ms, elementalformula, exactmasschart, error, minerror = 0.002, remove.elements = c(), max.dist = 3, min.int = 0.001, charge = "neutral", m = 1, n = 0.5) {
  #calculates the isotopic distribution of the stated elemental formula and compares against the empirical ms
  ms_sim <- isotopic_distribution(elementalformula, exactmasschart, remove.elements, max.dist, min.int, charge)
  compare_ms(ms, ms_sim, error, minerror, m, n)
}