# This file contains functions that will unpack spectral data and make them tidy
# for further manipulation.

# Format Parsers ----------------------------------------------------------

#' Parse "Separated" MS Spectra
#'
#' The "separated" format includes spectra packed into two separate columns, one
#' for mass and another for intensity. All values for a given scan time are
#' packed into these columns, separated by space, with an unlimited number of
#' discrete values, and must be a 1:1 ratio of values between the two columns.
#'
#' @param df data.frame object containing spectra compressed in the "separated"
#'   format
#' @param ms_cols CHR vector of length 2 identifying the column names to use for
#'   mass and intensity in the source data; must be of length 2, with the first
#'   value identifying the mass column and the second identifying the intensity
#'   column
#'
#' @return data.frame object of the unpacked spectra as a list column
#' @export
#'
#' @examples
#' ### JSON Example
#' tmp <- jsonify::as.json('{
#'  "masses": "712.9501 713.1851", 
#'  "intensities": "15094.41015625 34809.9765625"
#' }')
#' ms_spectra_separated(as.data.frame(jsonify::from_json(tmp)))
#' 
#' ### Example data.frame
#' tmp <- data.frame(
#'   measured_mz = "712.9501 713.1851",
#'   measured_intensity = "15094.41015625 34809.9765625"
#' )
#' ms_spectra_separated(tmp)
ms_spectra_separated <- function(df, ms_cols = c("measured_mz", "measured_intensity")) {
  # Argument validation ----
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        df      = list(c("mode", "data.frame")),
        ms_cols = list(c("mode", "character"), c("length", 2))
      ),
      from_fn    = "ms_spectra_separated")
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Function body ----
  if (!all(ms_cols %in% names(df))) {
    stop(sprintf("Could not find both '%s' in names of the object provided to argument 'df'.",
                 format_list_of_names(ms_cols))
    )
  }
  out <- df %>%
    mutate(masses = str_split(df[[ms_cols[1]]], " "),
           intensities = str_split(df[[ms_cols[2]]], " ")) %>%
    rowwise() %>%
    mutate(spectra = list(
      tibble(
        mz        = as.numeric(masses),
        intensity = as.numeric(intensities)
      )
    )) %>%
    select(-masses, -intensities, -any_of(ms_cols))
  return(out)
}


#' Parse "Zipped" MS Spectra
#'
#' The "zipped" format includes spectra packed into one column containing
#' alternating mass and intensity values for all observations. All values are
#' packed into these columns for a given scan time, separated by spaces, with an
#' unlimited number of discrete values, and must be in an alternating 1:1
#' pattern of values of the form "mass intensity mass intensity".
#'
#' @param df data.frame object containing spectra compressed in the "zipped"
#'   format
#' @param spectra_col CHR vector of length 2 identifying the column names to use
#'   for mass and intensity in the source data; must be of length 2, with the
#'   first value identifying the mass column and the second identifying the
#'   intensity column
#'
#' @return data.frame object containing unpacked spectra as a list column
#' @export
#'
#' @examples
#' ### JSON Example
#' tmp <- jsonify::as.json{
#'  "msdata": "712.9501 15094.41015625 713.1851 34809.9765625"
#' }
#' ms_spectra_separated(as.data.frame(jsonify::from_json(tmp)))
#' 
#' ### Example data.frame
#' tmp <- data.frame(
#'   msdata = "712.9501 15094.41015625 713.1851 34809.9765625"
#' )
#' ms_spectra_zipped(tmp)
ms_spectra_zipped <- function(df, spectra_col = "msdata") {
  # Argument validation ----
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        df          = list(c("mode", "data.frame")),
        spectra_col = list(c("mode", "character"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Function body ----
  out <- df[[spectra_col]] %>%
    str_split(" ") %>%
    lapply(as.numeric)
  out <- lapply(out, function(x) {
    tmp <- matrix(x, ncol = 2, byrow = TRUE)
    tmp <- tibble(mass      = tmp[, 1],
                  intensity = tmp[, 2])
    return(tmp)
  })
  out <- df %>% 
    select(-!!spectra_col) %>%
    mutate(spectra = out)
  return(out)
}

#' Tidy Spectra
#'
#' An abstraction function to take outputs from `ms_spectra_separated` and
#' `ms_spectra_zipped` and return them as a tidy expression by unpacking the
#' list column "spectra".
#'
#' @param df data.frame object containing nested spectra in a column
#'
#' @return data.frame object containing tidy spectra
#' @export
#'
#' @examples
tidy_ms_spectra <- function(df) {
  # Argument validation ----
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        df = list(c("mode", "data.frame"))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  # Function body ----
  df %>%
    drop_na() %>%
    unnest_longer(spectra) %>%
    mutate(mz = unname(spectra[[1]]),
           intensity = unname(spectra[[2]])) %>%
    select(-spectra)
}


# Wrapper -----------------------------------------------------------------

#' Decompress Spectra
#'
#' This convenience wrapper will automatically decompress ms spectra in the
#' "separate" and "zipped" formats and return them as tidy data frames suitable
#' for further manipulation or visualization.
#'
#' @param target CHR scalar file path to use OR an R object containing
#'   compressed spectral data in the "separate" or "zipped" format
#' @param is_file BOOL scalar of whether or not `target` is a file. Set to FALSE
#'   to use an existing R object, which should contain an object with a named
#'   element matching parameter `spectra_set` (default TRUE)
#' @param is_format CHR scalar of the compression format, which must be one of
#'   the supported compression forms ("separated" or "zipped"); ignored if the
#'   compression format can be inferred from the text in `target` (default
#'   "separate")
#' @param spectra_set CHR scalar of the object name holding a spectra data frame
#'   to decompress (default "msdata")
#' @param ms_col_sep CHR vector of the column names holding spectral masses and
#'   intensities in the "separate" format (default c("masses", "intensities"))
#' @param ms_col_unzip CHR scalar of the name of the column holding spectral
#'   masses and intensities in the "unzip" format (default "msdata")
#' @param from_JSON BOOL scalar of whether or not `target` is a JSON expression
#'   needing conversion (default TRUE)
#'
#' @return data.frame object containing unpacked spectra
#' @export
#'
#' @examples
tidy_spectra <- function(target,
                         is_file      = TRUE,
                         is_format    = "separated",
                         spectra_set  = "msdata",
                         ms_col_sep   = c("measured_mz", "measured_intensity"),
                         ms_col_unzip = "msdata",
                         from_JSON    = TRUE) {
  # Argument validation ----
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(is_file, is_format, spectra_set, ms_col_sep, ms_col_unzip, from_JSON),
      conditions = list(
        is_file = list(c("mode", "logical"), c("length", 1)),
        is_format = list(c("mode", "character"), c("length", 1)),
        spectra_set = list(c("mode", "character"), c("length", 1)),
        ms_col_sep = list(c("mode", "character"), c("length", 2)),
        ms_col_unzip = list(c("mode", "character"), c("length", 1)),
        from_JSON = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "tidy_spectra")
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  is_format <- match.arg(is_format, c("separated", "zipped"))
  # Function body ----
  # Check if is file or local object
  if (is_file) {
    out <- read_file(target)
    if (grepl("separate", target)) {
      is_format <- "separated"
    } else if (grepl("unzip", target)) {
      is_format <- "zipped"
    }
  } else {
    out <- target
  }
  if (is_file && from_JSON) {
    out <- fromJSON(out)
  }
  if (spectra_set %in% names(out)) {
    out <- out[[spectra_set]]
  } else {
    warning(sprintf("Could not identify '%s' in object names. Using directly as-is", spectra_set))
  }
  if (!is.data.frame(out)) {
    out <- as.data.frame(out)
  }
  out <- switch(is_format,
                "separated" = ms_spectra_separated(out, ms_cols = ms_col_sep),
                "zipped"    = ms_spectra_zipped(out, spectra_col = ms_col_unzip))
  out <- tidy_ms_spectra(out)
  return(out)
}

#' Plot a peak from database mass spectral data
#'
#' @param data data.frame of spectral data in the form of the `ms_data` table
#' @param mz_tolerance INT scalar mass to charge ratio tolerance to group peaks,
#'   with at minimum columns for intensity (as base_int), ion m/z value (as
#'   base_ion), and scan time (as scantime) - (default: 0 goes to unit
#'   resolution)
#' @param drop_ratio NUM scalar threshold of the maximum intensity below which
#'   traces will be dropped (default: 1e-2 means any trace with a maximum
#'   intensity less than 1% of the maximum intensity in the plot will be
#'   dropped)
#' @param text_offset NUM scalar y-axis offset as a fraction of the maximum
#'   intensity for trace annotation (default: 0.02 offsets labels in the
#'   positive direction by 2% of the maximum intensity)
#'
#' @return
#' @export
#' 
plot_peak <- function(data, mz_tolerance = 0, drop_ratio = 1e-2, text_offset = 0.02) {
  cutoff <- max(data$base_int) * drop_ratio 
  plot_data <- data %>%
    mutate(ion_group = round(base_ion, mz_tolerance)) %>%
    group_by(ion_group) %>%
    filter(max(base_int) > cutoff)
  annotation_data <- plot_data %>%
    filter(base_int == max(base_int))
  out <- plot_data %>%
    ggplot(
      aes(x = scantime,
          y = base_int,
          group = ion_group)
    ) +
    geom_line() +
    geom_text(data = annotation_data,
              aes(label = ion_group,
                  x = scantime,
                  y = base_int),
              nudge_y = text_offset * max(data$base_int))
  out +
    theme_bw()
}
