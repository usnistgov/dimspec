extract.elements <- function(composition.str, remove.elements = c()) {
  single.elem <- gregexpr("[A-Z]", composition.str)[[1]]
  double.elem <- gregexpr("[A-Z][a-z]", composition.str)[[1]]
  single.elem <- single.elem[! single.elem %in% double.elem]
  if (length(single.elem) > 0) {
    elements <- substring(composition.str, single.elem, single.elem)
  }
  if (double.elem[1] != -1) {
    elements <- c(elements, substring(composition.str, double.elem, double.elem + 1))
  }
  ecounts <- rep(1, length(elements))
  nums <- gregexpr("[0-9]+", composition.str)[[1]]
  nums.count <- attr(nums, "match.length")
  for (i in 1:length(nums)) {
    if (substr(composition.str, nums[i] - 1, nums[i] - 1) %in% elements) {
      ecounts[which(elements == substr(composition.str, nums[i] - 1, nums[i] - 1))] <- substr(composition.str, nums[i], nums[i] + nums.count[i] - 1)
    }
    if (substr(composition.str, nums[i] - 2, nums[i] - 1) %in% elements) {
      ecounts[which(elements == substr(composition.str, nums[i] - 2, nums[i] - 1))] <- substr(composition.str, nums[i], nums[i] + nums.count[i] - 1)
    }
  }
  results <- c()
  results$elements <- elements
  results$counts <- as.integer(ecounts)
  results$counts <- results$counts[which(!results$elements %in% remove.elements)]
  results$elements <- results$elements[which(!results$elements %in% remove.elements)]
  results
}

calculate.monoisotope <- function(elementlist, exactmasses, adduct = "neutral") {
  mass <- 0
  for (i in 1:length(elementlist$elements)) {
    mass <- mass + as.numeric(exactmasses[which(exactmasses[,1] == elementlist$elements[i]),2])*elementlist$counts[i]
  }
  if (adduct != "neutral") {
    if (adduct == "+H") {mass.adj = -1.0072767}
    if (adduct == "-H") {mass.adj = 1.0072766}
    if (adduct == "+Na") {mass.adj = -22.9892213}
    if (adduct == "+K") {mass.adj = -38.9631585}
    if (adduct == "+") {mass.adj = 0.0005484}
    if (adduct == "-") {mass.adj = -0.0005484}
    if (adduct == "-radical") {mass.adj = 2*-0.0005484}
    if (adduct == "+radical") {mass.adj = 0}
    mass <- mass - mass.adj
  }
  mass
}

monoisotope.list <- function(list, column, exactmasses, remove.elements = c("Na", "K", "Ca"), adduct = "neutral") {
  formulas <- list[,column]
  masslist <- c()
  for (i in 1:length(formulas)) {
    elements <- extract.elements(composition.str = formulas[i], remove.elements = remove.elements)
    addmass <- calculate.monoisotope(elements, exactmasses, adduct)
    masslist <- c(masslist, addmass)
  }
  cbind(list, masslist)
}

adduct_formula <- function(elementalformula, adduct = "+H") {
  elist <- extract.elements(elementalformula, remove.elements = c())
  element <- gsub("\\+", "", gsub("-", "", adduct))
  change <- unlist(strsplit(adduct, split = ""))[1]
  if (element %in% elist$elements) {
    if (change  == "+") {
      elist$counts[which(elist$elements == element)] <- elist$counts[which(elist$elements == element)] + 1
    }
    if (change  == "-") {
      elist$counts[which(elist$elements == element)] <- elist$counts[which(elist$elements == element)] - 1
      if (elist$counts[which(elist$elements == element)] == 0) {
        elist$counts <- elist$counts[-which(elist$elements == element)]
        elist$elements <- elist$elements[-which(elist$elements == element)]
      }
    }
  }
  if (!element %in% elist$elements) {
    if (change == "+") {
      elist$elements <- c(elist$elements, element)
      elist$counts <- c(elist$counts, 1)
    }
  }
  paste(sapply(1:length(elist$elements), function(x) paste(elist$elements[x], elist$counts[x], sep = "")), collapse = "")
}
