#' Isotopic distribution functions

#' Generate isotopic distribution mass spectrum of elemental formula
#'
#' @param elementalformula character string of elemental formula to simulate isotopic pattern
#' @param exactmasschart exact mass chart generated from function create_exactmasschart
#' @param remove.elements character vector of elements to remove from elemental formula
#' @param max.dist numeric maximum mass distance (in Da) from exact mass to include in simulated isotopic pattern
#' @param min.int numeric minimum relative intensity (maximum = 1, minimum = 0) to include in simulated isotopic pattern
#' @param charge character string for the charge state of the simulated isotopic pattern, options are `neutral`, `positive`, and `negative`
#'
#' @return data frame containing mz and int values of mass spectrum
#' @export
#'
isotopic_distribution <- function(elementalformula, exactmasschart, remove.elements = c(), max.dist = 3, min.int = 0.001, charge = "neutral") {
  elementlist <- extract.elements(elementalformula, remove.elements)
  abund <- lapply(elementlist$elements, function(x) {
    ab <- exactmasschart$abundances[which(exactmasschart$elements == x),];
    mass <- exactmasschart$masses[which(exactmasschart$elements == x),];
    ab <- ab[which(!is.na(ab))];
    mass <- mass[which(!is.na(mass))];
    data.frame(mass = mass[order(mass)],ab = ab[order(mass)])
    })
  abund <- lapply(1:length(abund), function(x) {
    n <- sapply(abund[[x]]$mass, function(y) min(floor(max.dist / (y - abund[[x]]$mass[1])), elementlist$counts[x]));
    data.frame(abund[[x]], n = n)
  })
  iso_elements <- lapply(1:length(abund), function(x) sapply(1:nrow(abund[[x]]), function(y) elementlist$elements[x]))
  iso_masses <- lapply(1:length(abund), function(x) sapply(1:nrow(abund[[x]]), function(y) abund[[x]]$mass[y]))
  iso_abund <- lapply(1:length(abund), function(x) sapply(1:nrow(abund[[x]]), function(y) abund[[x]]$ab[y]))
  iso_max <- lapply(1:length(abund), function(x) sapply(1:nrow(abund[[x]]), function(y) abund[[x]]$n[y]))
  combolist <- lapply(1:length(abund), function(x) {
    if (length(abund[[x]]$n) == 1) {o <- data.frame(abund[[x]]$n[1]); colnames(o) <- paste(elementlist$elements[x], round(abund[[x]]$mass, 0), sep = "")}
    if (length(abund[[x]]$n) > 1) {o <- expand.grid(lapply(abund[[x]]$n[2:length(abund[[x]]$n)], function(y) {if (y == 0) {return(0)}; 0:abund[[x]]$n[y]}));
                                    o <- cbind(abund[[x]]$n[1] - rowSums(o), o)
                                    o <- o[which(o[,1] >= 0),]
                                    colnames(o) <- paste(elementlist$elements[x], round(abund[[x]]$mass, 0), sep = "")
                                    }
    o
  })
  masslist <- lapply(1:length(combolist), function(x) sapply(1:nrow(combolist[[x]]), function(y) sum(combolist[[x]][y,]*iso_masses[[x]])))
  abundlist <- lapply(1:length(combolist), function(x) sapply(1:nrow(combolist[[x]]), function(y) (factorial(sum(combolist[[x]][y,]))/prod(sapply(combolist[[x]][y,], factorial)))*prod(iso_abund[[x]]^combolist[[x]][y,])))
  mz <- rowSums(expand.grid(masslist))
  int <- apply(expand.grid(abundlist), 1, prod)
  int <- int/max(int)
  mz <- mz[which(int >= min.int)]
  int <- int[which(int >= min.int)]
  mass.adj <- 0
  if (charge == "positive") {mass.adj = -0.0005484}
  if (charge == "negative") {mass.adj = 0.0005484}
  mz <- mz + mass.adj
  data.frame(mz, int)
}

#' Compare Isotopic Pattern to simulated pattern
#' 
#' calculates the isotopic distribution of the stated elemental formula and compares against the empirical ms
#'
#' @param ms data.frame mass spectrum containing pair-wise m/z and intensity values of empirical isotopic pattern
#' @param elementalformula character string of elemental formula to simulate isotopic pattern
#' @param exactmasschart exact mass chart
#' @param error numeric relative mass error (in ppm) of mass spectrometer
#' @param minerror numeric minimum mass error (in Da) of mass spectrometer
#' @param remove.elements character vector of elements to remove from elemental formula
#' @param max.dist numeric maximum mass distance (in Da) from exact mass to include in simulated isotopic pattern
#' @param min.int numeric minimum relative intensity (maximum = 1, minimum = 0) to include in simulated isotopic pattern
#' @param charge character string for the charge state of the simulated isotopic pattern, options are `neutral`, `positive`, and `negative`
#' @param m numeric dot product mass weighting
#' @param n numeric dot product intensity weighting
#'
#' @return numeric vector of match scores between the empirical and calculated isotopic distribution.
#' @export
#'
check_isotopedist <- function(ms, elementalformula, exactmasschart, error, minerror = 0.002, remove.elements = c(), max.dist = 3, min.int = 0.001, charge = "neutral", m = 1, n = 0.5) {
  ms_sim <- isotopic_distribution(elementalformula, exactmasschart, remove.elements, max.dist, min.int, charge)
  compare_ms(ms, ms_sim, error = rep(error,2), minerror = rep(minerror, 2), m, n)
}