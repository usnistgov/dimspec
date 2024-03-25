#' Elemental Formula Functions

#' Extract elements from formula
#' 
#' Converts elemental formula into list of `elements` and `counts` corresponding to the composition
#'
#' @param composition.str  character string elemental formula
#' @param remove.elements character vector containing elements to remove from 
#'
#' @return list with `elements` and `counts`
#' @export
#'
#' @examples
#' extract.elements("C2H5O")
#' 
#' extract.elements("C2H5ONa", remove.elements = c("Na", "Cl"))

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

#' Calculate the monoisotopic mass of an elemental formula list
#'
#' @param elementlist list of elemental formula from `extract.elements` function
#' @param exactmasses list of exact masses of elements
#' @param adduct character string adduct/charge state to add to the elemental formula, options are `neutral`, `+H`, `-H`, `+Na`, `+K`, `+`, `-`, `-radical`, `+radical`
#' @param db_conn database connection object, either a CHR scalar name (default: "con") or the connection object itself (preferred)
#'
#' @return numeric monoisotopic exact mass
#' @export
#'
#' @examples
#' elementlist <- extract.elements("C2H5O")
#' calculate.monoisotope(elementalist, adduct = "neutral")
#'

calculate.monoisotope <- function(elementlist, exactmasses = NULL, adduct = "neutral", db_conn = "con") {
  if (is.character(elementlist)) {
    elementlist <- lapply(elementlist, extract.elements)
  }
  use_db <- FALSE
  use_api <- FALSE
  if (is.null(exactmasses)) {
    if (is.character(db_conn)) {
      if (exists(db_conn)) {
        db_conn <- eval(rlang::sym(db_conn))
      }
    }
    use_db <- try(DBI::dbIsValid(db_conn))
    if (inherits(use_db, "try-error")) {
      use_db <- FALSE
      if (exists("api_endpoint")) {
        use_api <- api_endpoint("_ping")$status == "OK"
      }
    }
    if (use_db) {
        exactmasses <- tbl(db_conn, "view_exact_masses") %>%
          select(symbol, exact_mass) %>%
          collect()
    } else if (use_api) {
      exactmasses <- api_endpoint(
        path = "table_search",
        table_name = "view_exact_masses",
        return_format = "data.frame"
      )
    } else {
      exactmasses <- setNames(
        readRDS(here::here("R", "misc", "exactmasses.RDS")),
        c("symbol", "exact_mass")
      )
    }
  }
  mass <- lapply(elementlist,
                 function(x) {
                   tmp <- sum(x$counts * exactmasses$exact_mass[match(x$elements, exactmasses$symbol)])
                   if (adduct != "neutral") {
                     mass.adj <- get_massadj(adduct, exactmasses)
                     tmp <- tmp + mass.adj
                   }
                   return(tmp)
                 })
  # if (use_db) {
  #   mass <- lapply(elementlist,
  #                  function(x) {
  #                    tmp <- sum(x$counts * exactmasses$exact_mass[match(x$elements, exactmasses$symbol)])
  #                    if (adduct != "neutral") {
  #                      mass.adj <- get_massadj(adduct, exactmasses = exactmasses, db_conn = db_conn)
  #                      tmp <- tmp + mass.adj
  #                    }
  #                    return(tmp)
  #                  })
  # } else if (use_api) {
    # mass <- lapply(elementlist,
    #                function(x) {
    #                  tmp <- sum(x$counts * exactmasses$exact_mass[match(x$elements, exactmasses$symbol)])
    #                  if (adduct != "neutral") {
    #                    mass.adj <- get_massadj(adduct, exactmasses = exactmasses)
    #                    tmp <- tmp + mass.adj
    #                  }
    #                  return(tmp)
    #                })
  # } else {
  #   mass <- lapply(elementlist,
  #                  function(x) {
  #                    tmp <- sum(x$counts * exactmasses$exact_mass[match(x$elements, exactmasses$symbol)])
  #                    if (adduct != "neutral") {
  #                      if (adduct == "+H") {mass.adj = -1.0072767}
  #                      if (adduct == "-H") {mass.adj = 1.0072766}
  #                      if (adduct == "+Na") {mass.adj = -22.9892213}
  #                      if (adduct == "+K") {mass.adj = -38.9631585}
  #                      if (adduct == "+") {mass.adj = 0.0005484}
  #                      if (adduct == "-") {mass.adj = -0.0005484}
  #                      if (adduct == "-radical") {mass.adj = 2*-0.0005484}
  #                      if (adduct == "+radical") {mass.adj = 0}
  #                      tmp <- tmp - mass.adj
  #                    }
  #                    return(tmp)
  #                  })
  # }
  unlist(mass)
}

#' Calculate the monoisotopic mass of a elemental formulas in 
#'
#' @param df data.frame with at least one column with elemental formulas
#' @param column integer or CHR scalar indicating the column containing the elemental formulas, if CHR then regex match is used
#' @param exactmasses list of exact masses
#' @param remove.elements elements to remove from the elemental formulas
#' @param adduct character string adduct/charge state to add to the elemental formula, options are `neutral`, `+H`, `-H`, `+Na`, `+K`, `+`, `-`, `-radical`, `+radical`
#'
#' @return data.frame with column of exact masses appended to it
#' @export
#'

monoisotope.list <- function(df, column, exactmasses, remove.elements = c(), adduct = "neutral") {
  if (is.character(column)) {
    column <- grep(column, names(df), value = TRUE)
    stopifnot(length(column) == 1)
  }
  formulas <- df[,column]
  masslist <- c()
  elements <- lapply(formulas, extract.elements, remove.elements = remove.elements)
  addmass <- calculate.monoisotope(elements, exactmasses, adduct)
  masslist <- c(masslist, addmass)
  cbind(df, masslist)
}

#' Add Adduct to Formula
#'
#' @param elementalformula character string elemental formula
#' @param adduct character string adduct state to add to the elemental formula, must contain an element, options are `+H`, `-H`, `+Na`, `+K`
#'
#' @return character string containing elemental formula with adduct
#' @export
#'
#' @examples
#' adduct_formula("C2H5O", adduct = "+H")

adduct_formula <- function(elementalformula, adduct = "+H") {
  elist <- extract.elements(elementalformula, remove.elements = c())
  addformula <- gsub("\\+", "", gsub("-", "", adduct))
  if (addformula != "") {
  elements <- extract.elements(addformula)
  change <- unlist(strsplit(adduct, split = ""))[1]
  for (i in 1:length(elements$elements)) {
    element <- elements$elements[i]
    addcount <- elements$counts[i]
  if (element %in% elist$elements) {
    if (change  == "+") {
      elist$counts[which(elist$elements == element)] <- elist$counts[which(elist$elements == element)] + addcount
    }
    if (change  == "-") {
      elist$counts[which(elist$elements == element)] <- elist$counts[which(elist$elements == element)] - addcount
      if (elist$counts[which(elist$elements == element)] <= 0) {
        if (elist$counts[which(elist$elements == element)] < 0) {stop("The adduct formula cannot be applied to the formula.")}
        elist$counts <- elist$counts[-which(elist$elements == element)]
        elist$elements <- elist$elements[-which(elist$elements == element)]
      }
    }
  }
  if (!element %in% elist$elements) {
    if (change == "+") {
      elist$elements <- c(elist$elements, element)
      elist$counts <- c(elist$counts, addcount)
    }
  }
  }
  }
  paste(sapply(1:length(elist$elements), function(x) paste(elist$elements[x], elist$counts[x], sep = "")), collapse = "")
}

#' Checks if two elemental formulas match
#'
#' @param testformula character string of elemental formula to test
#' @param trueformula character string of elemental formula to check against (truth)
#'
#' @return logical
#' @export 
#'
is_elemental_match <- function(testformula, trueformula) {
  test_list <- extract.elements(testformula, remove.elements = c())
  true_list <- extract.elements(trueformula, remove.elements = c())
  if (length(which(test_list$elements %in% true_list$elements)) < length(test_list$elements)) {return(FALSE)}
  if (length(which(test_list$elements %in% true_list$elements)) > length(test_list$elements)) {return(FALSE)}
  if (length(which(test_list$elements %in% true_list$elements)) == length(test_list$elements)) {
    count_check <- sapply(test_list$elements, function(x) test_list$counts[which(test_list$elements == x)] == true_list$counts[which(true_list$elements == x)])
    if (FALSE %in% count_check) {return(FALSE)}
    if (!FALSE %in% count_check) {return(TRUE)}
  }
}

#' Check if elemental formula is a subset of another formula
#'
#' @param fragmentformula character string of elemental formula subset to test
#' @param parentformula character string of elemental formula to check for subset
#'
#' @return logical
#' @export
#'
#' @examples
#' is_elemental_subset("C2H2", "C2H5O")
#' 
#' is_elemental_subset("C2H2", "C2H1O")

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
