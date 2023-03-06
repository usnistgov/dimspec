#' Get the optimal uncertainty mass spectrum parameters for data
#'
#' @param peaktable list generated from `create_peak_table_ms1` or `create_peak_table_ms2`
#' @param max_correl numeric maximum acceptable correlation
#' @param correl_bin numeric sequence bin width from max_correl..0
#' @param max_ph numeric maximum acceptable peak height (\%)
#' @param ph_bin numeric sequence bin width from max_ph..0
#' @param max_freq numeric maximum acceptable observational frequency (\%)
#' @param freq_bin numeric sequence bin width from max_freq..0
#' @param min_n_peaks integer ideal minimum number of scans for mass spectrum
#' @param cormethod string indicating correlation function to use (see [cor()] for description)
#'
#' @return data.frame object containing optimized search parameters
#' 
#' @export
#'
optimal_ums <- function(peaktable, max_correl = 0.75, correl_bin = 0.05, max_ph = 10, ph_bin = 1, max_freq = 10, freq_bin = 1, min_n_peaks = 3, cormethod = "pearson") {
  if (ncol(peaktable$peaktable_mass) < min_n_peaks) {min_n_peaks <- 1}
  opt_ums <- try(get_ums(peaktable, correl = max_correl, ph = max_ph, freq = max_freq, cormethod = cormethod), silent = TRUE)
  if (!inherits(opt_ums, "try-error")) {
    if (nrow(opt_ums) > 0) {
    if (attr(opt_ums, "numscans") >= min_n_peaks) {
      #if the optimal settings are good enough for the requirements, do not do the recursive route
    
      return(data.frame(correl = max_correl, ph = max_ph, freq = max_freq, n = attr(opt_ums, "numscans")))
    }
    }
  }
  correl_range <- c(seq(from = max_correl, to = 0, by = -correl_bin), NA)
  ph_range <- c(seq(from = max_ph, to = 0, by = -ph_bin), NA)
  freq_range <- c(seq(from = max_freq, to = 0, by = -freq_bin), NA)
  combos <- expand.grid(correl = correl_range, ph = ph_range, freq = freq_range)
  results <- apply(combos, 1, function(x) try(get_ums(peaktable, correl = x["correl"], ph = x["ph"], freq = x["freq"], cormethod = cormethod), silent = TRUE))
  n_peaks <- sapply(results, function(x) {y <- attr(x, "numscans"); if (is.null(y)) {y <- 0}; y})
  errors <- sapply(results, function(x) attr(x, "class"))
  nrows <- sapply(results, function(x) {y <- nrow(x); if (is.null(y)) {y <- 0}; y})
  combos <- cbind(combos, n = n_peaks)
  combos <- combos[which(n_peaks >= min_n_peaks & errors != "try-error" & nrows > 0),]
  norm_combos <- t(t(combos) / c(max_correl, max_ph, max_freq,max(n_peaks, na.rm = TRUE)))
  norm_combos[which(is.na(norm_combos), arr.ind = TRUE)] <- 0
  combos <- combos[which.max(rowSums(norm_combos)),]
  combos <- combos[order(combos$n, decreasing = TRUE),]
  combos[1,]
}

#' Get optimized uncertainty mass spectra parameters for a peak
#'
#' @param con SQLite database connection
#' @param peak_ids integer vector of primary keys for peaks table
#'
#' @return data.frame object of available optimized search parameters
#' @export
#'
get_opt_params <- function(con, peak_ids) {
  output <- DBI::dbGetQuery(conn = con, 
                  paste0(
                    "SELECT * FROM opt_ums_params
                    WHERE peak_id IN (",
                    paste(peak_ids, collapse = ","),
                    ")"
                  ))
  data.frame(peak_id = output$peak_id, 
             mslevel = output$mslevel,
             correl = suppressWarnings(as.numeric(output$correl)),
             ph = suppressWarnings(as.numeric(output$ph)),
             freq = suppressWarnings(as.numeric(output$freq)),
             n = suppressWarnings(as.numeric(output$n)),
             masserror = suppressWarnings(as.numeric(output$masserror)),
             minerror = suppressWarnings(as.numeric(output$minerror)))
}