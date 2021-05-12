exactmasschart <- readRDS('src/qualitycontrol/exactmasschart.RDS')

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
    if (length(abund[[x]]$n) > 1) {o <- expand.grid(lapply(abund[[x]]$n[2:length(abund[[x]]$n)], function(y) 0:abund[[x]]$n[y]));
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