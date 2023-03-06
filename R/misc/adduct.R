#' Calculate the mass adjustment for a specific adduct
#'
#' @param adduct character string containing the + or - and the elemental formula of the adduct, note "2H" should be represented as "H2"
#' @param exactmasses list of exact masses of elements, NULL pulls from the database
#' @param db_conn database connection object, either a CHR scalar name (default: "con") or the connection object itself (preferred)
#'
#' @return NUM scalar of the mass adjustment value
#' @export
#' 
get_massadj <- function(adduct = "+H", exactmasses = NULL, db_conn = "con") {
  #normally exact masses would be called from a previous function, but just in case.
  # This should really get pulled into its own function and used both here and in calculate.monoisotope but the flexibility might be nice.
  if (is.null(exactmasses)) {
    if (is.character(db_conn)) {
      if (exists(db_conn)) {
        db_conn <- eval(rlang::sym(db_conn))
      }
    }
    use_db <- try(DBI::dbIsValid(db_conn))
    if (inherits(use_db, "try-error")) {
      use_db <- FALSE
    }
    if (use_db) {
      exactmasses <- tbl(db_conn, "view_exact_masses") %>%
        select(symbol, exact_mass) %>%
        collect()
    } else {
      exactmasses <- setNames(
        readRDS(here::here("R", "misc", "exactmasses.RDS")),
        c("symbol", "exact_mass")
      )
    }
  }
  
  change <- unlist(strsplit(adduct, split = ""))[1]
  if (change != "+" & change != "-") {stop("The adduct must have a change sign (+/-)")}
  formula <- gsub("\\+", "", gsub("-", "", adduct))
  mass = 0
  if (formula != "" & formula != "radical") {
  elementlist <- extract.elements(formula)
  if (formula != "radical") {
    mass <- sum(elementlist$counts * exactmasses$exact_mass[match(elementlist$elements, exactmasses$symbol)])
  }
  if (formula == "radical") {mass <- 0.0005486} #the table lost this again.
  }
  if (change == "+") {mass <- as.double(paste0(change, mass)) - 0.0005486} #might need to adjust this
  if (change == "-") {mass <- as.double(paste0(change, mass)) + 0.0005486} #might need to adjust this
  mass.adj <- mass
  mass.adj
}