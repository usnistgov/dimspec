extract_collision_energies <- function(obj) {
  stopifnot(is.list(obj), "massspectrometry" %in% names(obj), "ce_value" %in% names(obj$massspectrometry))
  ce <- obj$massspectrometry$ce_value
  if (ce %in% c("null", "0", "", "NULL") || is.na(ce)) return(NA)
  if (is.na(suppressWarnings(as.numeric(ce)))) {
    ce <- ce |>
      str_remove_all("[a-zA-Z]") |>
      str_split("[^0-9\\.]") |>
      unlist() |>
      as.integer()
  }
  return(ce)
}

split_by_collision_energy <- function(obj) {
  stopifnot(is.list(obj))
  ce <- extract_collision_energies(obj)
  if (length(ce) == 1) return(list(obj))
  n_ce <- length(ce)
  # Ensure all MS2 scans are evenly divisible
  n_ms2_scans <- obj$msdata |>
    dplyr::filter(ms_n == 2) |>
    nrow()
  if (!n_ms2_scans %% n_ce == 0) warning(sprintf("Cannot evenly split %d MS2 scans by the %d reported collision energy values.", n_ms2_scans, n_ce))
  ms2_scans <- obj$msdata |>
    arrange(scantime) |>
    mutate(index = cumsum(ifelse(ms_n == 1, 1, 0))) |>
    filter(ms_n == 2) |>
    group_by(index) |>
    mutate(ce = ce[1:n()]) |>
    group_by(ce) |>
    group_split()
  out <- replicate(n_ce, obj, simplify = FALSE)
  for (i in 1:n_ce) {
    out[[i]]$msdata <- bind_rows(
      out[[i]]$msdata |>
        filter(ms_n == 1),
      ms2_scans[[i]] |>
        select(-index, -ce)
    ) |>
      arrange(scantime)
    out[[i]]$massspectrometry$ce_value <- ce[i]
  }
  return(out)
}
