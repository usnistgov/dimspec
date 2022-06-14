#' Get the optimal uncertainty mass spectrum parameters for data
#'
#' @param peaktable list generated from `create_peak_table_ms1` or `create_peak_table_ms2`
#' @param max_correl numeric maximum acceptable correlation
#' @param correl_bin numeric sequence bin width from max_correl..0
#' @param max_ph numeric maximum acceptable peak height (%)
#' @param ph_bin numeric sequence bin width from max_ph..0
#' @param max_freq numeric maximum acceptable observational frequency (%)
#' @param freq_bin numeric sequence bin width from max_freq..0
#' @param min_n_peaks integer ideal minimum number of scans for mass spectrum
#' @param cormethod string indicating correlation function to use (see `cor` for description)
#'
#' @return
#' @export
#'
#' @examples
optimal_ums <- function(peaktable, max_correl = 0.75, correl_bin = 0.05, max_ph = 10, ph_bin = 1, max_freq = 10, freq_bin = 1, min_n_peaks = 3, cormethod = "pearson") {
  correl_range <- c(seq(from = max_correl, to = 0, by = -correl_bin), NA)
  ph_range <- c(seq(from = max_ph, to = 0, by = -ph_bin), NA)
  freq_range <- c(seq(from = max_freq, to = 0, by = -freq_bin), NA)
  combos <- expand.grid(correl = correl_range, ph = ph_range, freq = freq_range)
  results <- apply(combos, 1, function(x) try(get_ums(peaktable, correl = x["correl"], ph = x["ph"], freq = x["freq"]), silent = TRUE))
  n_peaks <- sapply(results, function(x) {x <- attr(x, "numscans"); if (is.null(x)) {x <- 0}; x})
  errors <- sapply(results, function(x) attr(x, "class"))
  combos <- cbind(combos, n = n_peaks)
  combos <- combos[which(n_peaks >= min_n_peaks & errors != "try-error"),]
  norm_combos <- t(t(combos) / c(max_correl, max_ph, max_freq,1))
  combos <- combos[which.max(rowSums(norm_combos[,1:3])),]
  combos <- combos[order(combos$n, decreasing = TRUE),]
  combos[1,]
}