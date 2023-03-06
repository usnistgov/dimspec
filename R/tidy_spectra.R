# This file contains functions that will unpack spectral data and make them tidy
# for further manipulation.

# Format Parsers ----------------------------------------------------------

#' Parse "Separated" MS Data
#'
#' The "separated" format includes spectra packed into two separate columns, one
#' for mass and another for intensity. All values for a given scan time are
#' packed into these columns, separated by space, with an unlimited number of
#' discrete values, and must be a 1:1 ratio of values between the two columns.
#'
#' @note ms_cols is treated as regex expressions, but it is safest to provide
#'   matching column names
#'
#' @param df data.frame or json object containing spectra compressed in the
#'   "separated" format
#' @param ms_cols CHR vector of length 2 identifying the column names to use for
#'   mass and intensity in the source data; must be of length 2, with the first
#'   value identifying the mass-to-charge ratio column and the second
#'   identifying the intensity column
#'
#' @return data.frame object of the unpacked spectra as a list column
#' @export
#'
#' @examples
#' ### JSON Example
#' tmp <- jsonify::as.json('{
#'  "measured_mz": "712.9501 713.1851",
#'  "measured_intensity": "15094.41015625 34809.9765625"
#' }')
#' ms_spectra_separated(tmp)
#'
#' ### Example data.frame
#' tmp <- data.frame(
#'   measured_mz = "712.9501 713.1851",
#'   measured_intensity = "15094.41015625 34809.9765625"
#' )
#' ms_spectra_separated(tmp)
ms_spectra_separated <- function(df, ms_cols = c("mz", "intensity")) {
  mz_col <- grep(ms_cols[1], names(df), value = TRUE)[1]
  int_col <- grep(ms_cols[2], names(df), value = TRUE)[1]
  ms_cols <- c(mz_col, int_col)
  out <- df %>%
    mutate(masses = str_split(df[[mz_col]], " "),
           intensities = str_split(df[[int_col]], " ")) %>%
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


#' Parse "Zipped" MS Data
#'
#' The "zipped" format includes spectra packed into one column containing
#' alternating mass and intensity values for all observations. All values are
#' packed into these columns for a given scan time, separated by spaces, with an
#' unlimited number of discrete values, and must be in an alternating 1:1
#' pattern of values of the form "mass intensity mass intensity".
#'
#' @note spectra-col is treated as a regex expression, but it is safest to
#'   provide a matching column name
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
#' tmp <- jsonlite::as.json('{"msdata": "712.9501 15094.41015625 713.1851 34809.9765625"}')
#' ms_spectra_separated(tmp)
#'
#' ### Example data.frame
#' tmp <- data.frame(
#'   msdata = "712.9501 15094.41015625 713.1851 34809.9765625"
#' )
#' ms_spectra_zipped(tmp)
ms_spectra_zipped <- function(df, spectra_col = "data") {
  if (inherits(df, "json")) {
    df <- jsonlite::fromJSON(df) %>%
      as_tibble()
  }
  if (!inherits(df, "data.frame")) {
    stop("Argument df should be either a data.frame object or JSON coercible to one.")
  }
  if (!all(grepl(paste0(spectra_col, collapse = "|"), names(df)))) {
    stop(sprintf("Could not find both '%s' in names of the object provided to argument 'df'.",
                 format_list_of_names(ms_cols))
    )
  }
  spectra_col <- grep(spectra_col, names(df), value = TRUE)
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
#' A convenience function to take outputs from [ms_spectra_separated] and
#' [ms_spectra_zipped] and return them as a tidy data frame by unpacking the
#' list column "spectra".
#'
#' @param df data.frame object containing nested spectra in a column
#'
#' @return data.frame object containing tidy spectra
#' @export
#'
#' @usage tidy_ms_spectra(df = packed_data)
tidy_ms_spectra <- function(df) {
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
#' @param ms_col_zip CHR scalar of the name of the column holding spectral
#'   masses and intensities in the "unzip" format (default "msdata")
#' @param is_JSON BOOL scalar of whether or not `target` is a JSON expression
#'   needing conversion (default TRUE)
#'
#' @return data.frame object containing unpacked spectra
#' @export
#'
#' @examples
#' tidy_spectra('{"msdata": "712.9501 15094.41015625 713.1851 34809.9765625"}', is_format = "zipped")
#' tidy_spectra('{"measured_mz":"712.9501 713.1851","measured_intensity":"15094.41015625 34809.9765625"}')
tidy_spectra <- function(target,
                         is_file     = FALSE,
                         is_format   = c("separated", "zipped"),
                         spectra_set = "msdata",
                         ms_col_sep  = c("measured_mz", "measured_intensity"),
                         ms_col_zip  = "data",
                         is_JSON     = FALSE) {
  is_format <- match.arg(is_format)
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(is_file, is_format, spectra_set, ms_col_sep, ms_col_zip, is_JSON),
      conditions = list(
        is_file = list(c("mode", "logical"), c("length", 1)),
        is_format = list(c("mode", "character"), c("length", 1)),
        spectra_set = list(c("mode", "character"), c("length", 1)),
        ms_col_sep = list(c("mode", "character"), c("length", 2)),
        ms_col_zip = list(c("mode", "character"), c("length", 1)),
        is_JSON = list(c("mode", "logical"), c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  if (is_file) {
    if (file_exists(target)) {
      if (grepl("separate", target)) {
        is_format <- "separated"
      } else if (grepl("zip", target)) {
        is_format <- "zipped"
      }
      ext <- tools::file_ext(target)
      if (ext == "csv") {
        out <- readr::read_csv(target)
      } else if (ext == "json") {
        out <- readr::read_file(target)
        is_JSON <- TRUE
      }
    } else {
      stop("Called with `is_file` == TRUE but could not find the referenced file.")
    }
  } else {
    out <- target
  }
  if (is_JSON || inherits(out, "json") || all(inherits(out, "character"), length(out) == 1)) {
    out <- out %>%
      jsonlite::fromJSON() %>%
      as_tibble()
  }
  if (!inherits(out, "data.frame")) {
    if (all(inherits(x = out, what = c("tbl_sql", "tbl_lazy"), which = TRUE) > 0)) {
      out <- collect(out)
    } else {
      stop("Argument `target` should be either a data.frame object or JSON coercible to one.")
    }
  }
  if (is_format == "separated") {
    check_cols <- ms_col_sep
  } else if (is_format == "zipped") {
    check_cols <- ms_col_zip
  }
  req_col_count <- stringr::str_detect(names(out), paste0(check_cols, collapse = "|")) %>% sum()
  if (any(is_format == "separated" && !req_col_count == 2,
          is_format == "zipped" && !req_col_count == 1)) {
    stop(glue::glue("Could not find required names ({format_list_of_names(check_cols, add_quote = TRUE)}) in the object provided to argument `target`."))
  }
  out <- switch(is_format,
                "separated" = ms_spectra_separated(df = out, ms_cols = ms_col_sep),
                "zipped"    = ms_spectra_zipped(df = out, spectra_col = ms_col_zip))
  out <- tidy_ms_spectra(out)
  return(out)
}

#' Plot a peak from database mass spectral data
#'
#' Plots the intensity of ion traces over the scan period and annotates them
#' with the mass to charge value. Several flexible plotting aspects are provided
#' as data may become complicated.
#'
#' The basic default plot will group all mass-to-charge ratio values by unit
#' resolution (increase resolution with `peak_mz_resolution`) and plot them as
#' an area trace over the scanning period. Traces are annotated with the
#' grouping value. Values of `peak_mz_resolution` greater than available data
#' (e.g. 10 when data resolution is to the 5th decimal point) will default to
#' maximum resolution.
#'
#' Traces are filtered out completely if their maximum intensity is below the
#' ratio set by `peak_drop_ratio`; only complete traces are filtered out this
#' way, not individual data points within a retained trace. Set this as the
#' fraction of the base peak (the peak of maximum intensity) to use to filter
#' out low-intensity traces. The calculated intensity threshold will be printed
#' to the caption.
#'
#' @note Increasing `peak_mz_resolution` will likely result in multiple separate
#'   traces.
#' @note Implicitly missing values are not interpolated, but lines are drawn
#'   through to the next point.
#' @note `peak_type` can will accept abbreviations of its accepted values (e.g.
#'   "l" for "line")
#'
#' @param data data.frame of spectral data in the form of the `ms_data` table
#' @param peak_type CHR scalar of the plot type to draw, must be one of "line",
#'   "segment", or "area" (default: "line")
#' @param peak_facet_by CHR scalar name of a column by which to facet the
#'   resulting plot (default: "ms_n")
#' @param peak_line_color CHR scalar name of the color to use for the "color"
#'   aesthetic (only a single color is supported; default: "black")
#' @param peak_fill_color CHR scalar name of the color to use for the "fill"
#'   aesthetic (only a single color is supported; default: "grey70")
#' @param peak_repel_labels LGL scalar on whether to use the [ggrepel] package
#'   to space out m/z labels in the plot (default: TRUE). If [ggrepel] is not
#'   installed, it will default to FALSE rather than requiring an installation
#' @param peak_mz_resolution INT scalar mass to charge ratio tolerance to group
#'   peaks, with at minimum columns for intensity (as "base_int"), ion m/z value
#'   (as "base_ion"), and scan time (as "scantime") - (default: 0 goes to unit
#'   resolution)
#' @param peak_drop_ratio NUM scalar threshold of the maximum intensity below
#'   which traces will be dropped (default: 1e-2 means any trace with a maximum
#'   intensity less than 1\% of the maximum intensity in the plot will be
#'   dropped); if > 1 the inversion will be used (1e5 -> 1e-5)
#' @param peak_text_offset NUM scalar y-axis offset as a fraction of the maximum
#'   intensity for trace annotation (default: 0.02 offsets labels in the
#'   positive direction by 2\% of the maximum intensity)
#' @param db_conn database connection (default: con) which must be live to pull
#'   sample and compound identification information
#'
#' @return ggplot object
#' @export
#' 
ms_plot_peak <- function(data,
                         peak_type = c("area", "line", "segment"),
                         peak_facet_by = "ms_n",
                         peak_mz_resolution = 0,
                         peak_drop_ratio = 1e-2,
                         peak_repel_labels = TRUE,
                         peak_line_color = "black",
                         peak_fill_color = "grey50",
                         peak_fill_alpha = 0.2,
                         peak_text_size = 3,
                         peak_text_offset = 0.02,
                         include_method = TRUE,
                         db_conn = con) {
  if (inherits(data, "tbl_sql")) {
    data <- collect(data)
  }
  if (peak_repel_labels) {
    if(!"ggrepel" %in% installed.packages()) {
      peak_repel_labels <- FALSE
    } else {
      require(ggrepel)
    }
  }
  if (inherits(invisible(try(class(db_conn))), "try-error")) {
    db_conn <- NULL
  }
  peak_type <- match.arg(peak_type)
  if (peak_drop_ratio > 1) peak_drop_ratio <- 1 / peak_drop_ratio
  cutoff <- max(data$base_int) * peak_drop_ratio
  plot_data <- data %>%
    mutate(ion_group = round(base_ion, peak_mz_resolution),
           ms_n = paste0("MS", ms_n)) %>%
    group_by(ion_group) %>%
    filter(max(base_int) > cutoff)
  annotation_data <- plot_data %>%
    group_by(.data[[peak_facet_by]]) %>%
    filter(base_int == max(base_int)) %>%
    mutate(base_int = base_int * (1 + peak_text_offset))
  this_aes <- switch(
    peak_type,
    "line" = aes(x = scantime,
                 y = base_int,
                 group = ion_group),
    "segment" = aes(x = scantime,
                    xend = scantime,
                    y = base_int,
                    yend = 0,
                    group = ion_group),
    "area" = aes(x = scantime,
                 y = base_int,
                 group = ion_group)
  )
  this_geom <- switch(
    peak_type,
    "line" = geom_line,
    "segment" = geom_segment,
    "area" = geom_area
  )
  if (peak_repel_labels) {
    text_geom <- geom_label_repel
  } else {
    text_geom <- geom_label
  }
  
  if (!is.null(db_conn)) {
    plot_titles <- ms_plot_titles(plot_data = plot_data,
                                  mz_resolution = peak_mz_resolution,
                                  drop_ratio = peak_drop_ratio,
                                  include_method = include_method,
                                  db_conn = db_conn)
  } else {
    plot_titles <- list(title = NULL, subtitle = NULL, caption = NULL)
  }
  
  out <- plot_data %>%
    ggplot(
      mapping = this_aes
    ) +
    suppressWarnings(this_geom(color = peak_line_color,
                               fill = peak_fill_color,
                               alpha = ifelse(peak_type == "area", peak_fill_alpha, 1))) +
    suppressWarnings(text_geom(data = annotation_data,
                               aes(label = sprintf("%.*f", peak_mz_resolution, ion_group),
                                   x = scantime,
                                   y = base_int,
                                   vjust = 0,
                                   hjust = 0.5),
                               size = peak_text_size,
                               check_overlap = FALSE,
                               max.overlaps = 20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       labels = scales::label_scientific(digits = 2)) +
    theme_bw() +
    labs(x = "Scan Time",
         y = "Signal Intensity",
         title = plot_titles$title,
         subtitle = plot_titles$subtitle,
         caption = plot_titles$caption
    ) +
    facet_wrap(as.formula(paste("~", peak_facet_by)),
               nrow = n_distinct(plot_data[[peak_facet_by]]),
               scales = "free_y")
    # facet_wrap(as.formula(paste("~", peak_facet_by)),
    #            nrow = 2)
  return(out)
}

#' Consistent title for ms_plot_x functions
#'
#' This helper function creates consistently formatted plot label elements in an
#' opinionated manner. This is unlikely to be useful outside the direct context
#' of [ms_plot_peak], [ms_plot_spectra], and [ms_plot_spectral_intensity].
#'
#' @param plot_data data.frame object passed from the plotting function
#' @param mz_resolution NUM scalar passed from the plotting function
#' @param drop_ratio NUM scalar passed from the plotting function
#' @param include_method LGL scalar indicating whether or not to get the method
#'   narrative from the database
#' @param db_conn database connection (default: con) which must be live to pull
#'   sample and compound identification information
#'
#' @return LIST of strings named for ggplot title elements "title", "subtitle",
#'   and "caption"
#' @export
#' 
ms_plot_titles <- function(plot_data, mz_resolution, drop_ratio, include_method, db_conn = con) {
  out <- list(
    plot_title = NULL,
    plot_subtitle = NULL,
    plot_caption = NULL
  )
  if ("peak_id" %in% names(plot_data)) {
    this_peak <- unique(plot_data$peak_id)
  } else {
    this_peak <- NA
  }
  if (length(this_peak) > 1 | !active_connection(db_conn)) {
    return(out)
  }
  if (!any(is.na(this_peak)) && !is.null(db_conn) && active_connection(db_conn)) {
    compound <- tbl(db_conn, "compound_fragments") %>%
      filter(peak_id %in% this_peak) %>%
      distinct(compound_id) %>%
      left_join(
        tbl(con, "compounds"),
        by = c("compound_id" = "id")
      ) %>%
      select(name) %>%
      collect()
    print_peak <- format_list_of_names(this_peak) %>%
      str_trunc(30)
    peak_id <- sprintf("peak #%s", print_peak)
    if (nrow(compound) == 0) {
      compound_name <- NULL
    } else {
      compound_name <- format_list_of_names(compound[[grep("name", names(compound), value = TRUE)]]) %>%
        str_trunc(60)
    }
    sample <- tbl(db_conn, "peaks") %>%
      filter(id %in% this_peak) %>%
      distinct(sample_id) %>%
      left_join(
        tbl(con, "samples"),
        by = c("sample_id" = "id")
      ) %>%
      distinct(sample_id, mzml_name) %>%
      collect()
    sample_id <- NULL
    sample_name <- NULL
    method_info <- NULL
    if (nrow(sample) == 1) {
      if (include_method) {
        method_info <- tbl(db_conn, "samples") %>%
          filter(id == local(sample$sample_id)) %>%
          left_join(tbl(db_conn, "view_method_narrative"), by = c("ms_methods_id" = "Method ID")) %>%
          pull(Narrative)
      }
      sample_id <- sprintf("as measured in sample #%s", sample$sample_id)
      sample_name <- sample[[grep("name", names(sample), value = TRUE)]]
    } else if (nrow(sample) > 1) {
      sample_id <- sprintf("present in %s samples", nrow(sample))
    }
  } else {
    peak_id <- NULL
    sample_id <- NULL
    compound_name <- NULL
    sample_name <- NULL
    method_info <- NULL
  }
  drop_ratio_text <- paste0("ratio > ",
                            round(drop_ratio * 100, 2),
                            "% max")
  measure <- grep("base_int|intensity", names(plot_data), value = TRUE)
  if (length(measure) > 0) {
    measure <- measure[1]
    if (is.numeric(plot_data[[measure]])) {
      drop_ratio_text <- drop_ratio * max(plot_data[[measure]])
      if (drop_ratio_text < 1) {
        drop_ratio_text <- "0"
      } else {
        drop_ratio_text <- format(drop_ratio_text,
                                  scientific = TRUE,
                                  digits = 3)
      }
      drop_ratio_text <- sprintf("> %s", drop_ratio_text)
    }
  }
  plot_caption <- sprintf("m/z tolerance: %s  |  intensity %s",
                          mz_resolution,
                          drop_ratio_text)
  if (is.null(sample_id)) sample_id <- ""
  out <- list(
    title = sprintf("%s %s",
                    str_to_sentence(peak_id),
                    sample_id
    ),
    subtitle = sprintf("%s in %s%s",
              compound_name,
              sample_name,
              ifelse(is.null(method_info),
                     '',
                     sprintf("\n%s",
                             stringr::str_wrap(string = method_info, width = 180)
                     )
              )
    ),
    caption = plot_caption
  )
  return(out)
}

#' Plot a fragment map from database mass spectral data
#'
#' Especially for non-targeted analysis workflows, it is often necessary to
#' examine annotated fragment data for spectra across a given peak of interest.
#' Annotated fragments lend increasing confidence in the identification of the
#' compound giving rise to a mass spectral peak. If a fragment has been
#' annotated, that identification is displayed along with the mass to charge
#' value in blue. Annotations of the mass to charge ratio for unannotated
#' fragments are displayed in red.
#'
#' @note If `spectra_animate` is set to true, it requires the [gganimate]
#'   package to be installed (and may also require the [gifski] package) and
#'   WILL take a large amount of time to complete, but results in an animation
#'   that will iterate through the scan period and display mass spectral data as
#'   they appear across the peak. Your mileage likely will vary.
#'
#' @param data data.frame of spectral data in the form of the `ms_data` table
#' @param spectra_mz_resolution INT scalar mass to charge ratio tolerance to
#'   group peaks, with at minimum columns for intensity (as base_int), ion m/z
#'   value (as base_ion), and scan time (as scantime) - (default: 3)
#' @param spectra_drop_ratio NUM scalar threshold of the maximum intensity below
#'   which traces will be dropped (default: 1e-2 means any trace with a maximum
#'   intensity less than 1\% of the maximum intensity in the plot will be
#'   dropped); if > 1 the inversion will be used (1e5 -> 1e-5)
#' @param spectra_repel_labels LGL scalar on whether to use the [ggrepel]
#'   package to space out m/z labels in the plot (default: TRUE). If [ggrepel]
#'   is not installed, it will default to FALSE rather than requiring an
#'   installation
#' @param spectra_repel_line_color CHR scalar name of the color to use for the
#'   "color" aesthetic of the lines connecting repelled labels to their data
#'   points; passed to [ggrepel::geom_text_repel] as segment.color (only a
#'   single color is supported; default: "grey50")
#' @param spectra_nudge_y_factor NUM scalar y-axis offset as a fraction of the
#'   maximum intensity for trace annotation (default: 0.03 offsets labels in the
#'   positive direction by 3\% of the maximum intensity)
#' @param spectra_log_y LGL scalar of whether or not to apply a log10 scaling
#'   factor to the y-axis (default: FALSE)
#' @param spectra_is_file LGL scalar of whether data are coming from a file
#'   (default: FALSE)
#' @param spectra_from_JSON LGL scalar of whether data are in JSON format; other
#'   formats are not supported when `spectra_is_file = TRUE` (default: FALSE)
#' @param spectra_animate LGL scalar of whether to produce an animation across
#'   the scantime for these data (default: FALSE)
#' @param spectra_text_size NUM scalar of the text size to use for annotation
#'   labels (default: 3)
#' @param spectra_max_overlaps INT scalar of the maximum number of text overlaps
#'   to allow (default: 50)
#' @param db_conn database connection (default: con) which must be live to pull
#'   sample and compound identification information
#'
#' @return ggplot object
#' @export
#' 
ms_plot_spectra <- function(data,
                            spectra_type = c("separated", "zipped"),
                            spectra_mz_resolution = 3,
                            spectra_drop_ratio = 1e-2,
                            spectra_repel_labels = TRUE,
                            spectra_repel_line_color = "grey50",
                            spectra_nudge_y_factor = 0.03,
                            spectra_log_y = FALSE,
                            spectra_is_file = FALSE,
                            spectra_from_JSON = FALSE,
                            spectra_animate = FALSE,
                            spectra_text_size = 3,
                            spectra_max_overlaps = 50,
                            include_method = TRUE,
                            db_conn = con) {
  if (spectra_animate && !"gganimate" %in% installed.packages()) animate = FALSE
  if (inherits(data, "tbl_sql")) {
    data <- collect(data)
  }
  if (!is.data.frame(data) && is.list(data.frame)) {
    data <- bind_rows(data)
  }
  if (!"mz" %in% names(data)) {
    data <- data %>%
      tidy_spectra(is_format = spectra_type,
                   is_file = spectra_is_file,
                   is_JSON = spectra_from_JSON) %>%
      suppressWarnings()
  }
  if (!"scantime" %in% names(data)) animate = FALSE
  if (spectra_repel_labels) {
    require(ggrepel)
    text_geom <- geom_text_repel
  } else {
    text_geom <- geom_text
  }
  if (spectra_drop_ratio > 1) spectra_drop_ratio <- 1 / spectra_drop_ratio
  cutoff <- max(data$intensity) * spectra_drop_ratio
  if (spectra_log_y) {
    nudge_y_by <- spectra_nudge_y_factor
  } else {
    nudge_y_by <- max(data$intensity) * spectra_nudge_y_factor
  }
  plot_data <- data %>%
    mutate(ion_group = round(mz, spectra_mz_resolution)) %>%
    group_by(ion_group) %>%
    filter(max(intensity) > cutoff)
  if (!"formula" %in% names(plot_data)) {
    plot_data <- plot_data %>%
      left_join(
        tbl(db_conn, "view_annotated_fragments") %>%
          select(mz, formula) %>%
          collect(),
        by = c("mz" = "mz")
      )
  }
  if (!spectra_animate) {
    plot_data <- plot_data %>%
      group_by(ion_group) %>%
      filter(intensity == max(intensity))
  }
  annotation_data <- plot_data %>%
    mutate(label = ifelse(is.na(formula),
                          sprintf("%.*f", spectra_mz_resolution, mz),
                          sprintf("%s (%s)", formula, sprintf("%.*f", spectra_mz_resolution, mz))),
           lab_color = ifelse(is.na(formula), "", "annotated")
           ) %>%
    group_by(ion_group)
  
  plot_titles <- ms_plot_titles(plot_data = plot_data,
                                mz_resolution = spectra_mz_resolution,
                                drop_ratio = spectra_drop_ratio,
                                include_method = include_method,
                                db_conn = db_conn)
  
  out <- plot_data %>%
    ggplot(aes(x = mz,
               y = intensity,
               xend = mz,
               yend = 0)) +
    geom_segment() +
    suppressWarnings(text_geom(
      data = annotation_data,
      aes(x = mz,
          y = intensity,
          label = label,
          color = lab_color),
      max.overlaps = spectra_max_overlaps,
      segment.color = spectra_repel_line_color,
      nudge_y = nudge_y_by,
      size = spectra_text_size,
      show.legend = FALSE
    )) +
    theme_bw() +
    labs(x = "Mass to charge ratio (m/z)",
         y = "Signal Intensity",
         title = plot_titles$title,
         subtitle = plot_titles$subtitle,
         caption = plot_titles$caption
    ) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(c(0, 0.1)),
                       labels = scales::label_scientific(digits = 2)) +
    facet_wrap(~ ms_n, scales = "free", ncol = 1)
  if (spectra_log_y) {
    out <- out +
      scale_y_log10()
  }
  if (spectra_animate) {
    require(gganimate)
    out <- out +
      transition_time(scantime) +
      shadow_mark(
        past = TRUE,
        future = TRUE,
        color = "grey",
        alpha = 0.25
      ) +
      ease_aes() +
      labs(caption = "Scan: {sprintf('%.4f, frame_time)}\n{sprintf('m/z tolerance: %s  |  intensity ratio: %s',
                           spectra_mz_resolution,
                           spectra_drop_ratio)}")
  }
  return(out)
}

#' Create a spectral intensity plot
#'
#' Often it is useful to get an overview of mass-to-charge intensity across the
#' scanning time of a peak. Typically this is done with individual traces in the
#' peak fashion, but large peaks can often mask smaller ones, or wash out lower
#' intensity signals. Use this to plot m/z as dependent upon scan time with
#' intensity shown by color and size. It is intended as a complement to
#' [ms_plot_peak] and may be called at the same levels of granularity, generally
#' greater so than [ms_plot_peak] which is more of an overview.
#'
#' @param data tibble or pointer with data to plot, either at the peak level, in
#'   which case "base_ion" must be present, or at the spectral level, in which
#'   case "intensity" must be present
#' @param intensity_mz_resolution INT scalar mass to charge ratio tolerance to
#'   group peaks, with at minimum columns for intensity (as "base_int" or
#'   "intensity"), ion m/z value (as "base_ion" or "mz"), and scan time (as
#'   "scantime") - (default: 5)
#' @param intensity_drop_ratio NUM scalar threshold of the maximum intensity
#'   below which traces will be dropped (default: 0 returns all); if > 1 the
#'   inversion will be used (1e5 -> 1e-5)
#' @param intensity_facet_by CHR scalar of a column name in `data` by which to
#'   facet the resulting plot (default: NULL)
#' @param db_conn database connection (default: con) which must be live to pull
#'   sample and compound identification information
#'
#' @return object of classes `gg` and `ggplot`
#' @export
#' 
ms_plot_spectral_intensity <- function(data,
                                       intensity_mz_resolution = 5,
                                       intensity_drop_ratio = 0,
                                       intensity_facet_by = NULL,
                                       intensity_plot_resolution = c("spectra", "peak"),
                                       include_method = TRUE,
                                       db_conn = con) {
  if (inherits(data, "tbl_sql")) {
    data <- collect(data)
  }
  if (!"scantime" %in% names(data)) {
    stop("Spectral intensity plot requires a scantime column.")
  }
  intensity_plot_resolution <- match.arg(intensity_plot_resolution)
  plot_data <- data
  stopifnot(any(all(c("base_ion", "base_int") %in% names(data)),
                all(c("mz", "intensity") %in% names(data)),
                "scantime" %in% names(data)))
  if (!"mz" %in% names(data)) {
    if (intensity_plot_resolution == "spectra") {
    plot_data <- plot_data %>%
      tidy_spectra(is_file = FALSE,
                   is_JSON = FALSE) %>%
      suppressWarnings()
    } else {
      if ("base_ion" %in% names(data)) {
        plot_data <- plot_data %>%
          rename("mz" = "base_ion",
                 "intensity" = "base_int")
      } else {
        log_it("error", "Could not resolve mass spectral data.", "db")
        return(NULL)
      }
    }
  }
  if (intensity_drop_ratio > 1) intensity_drop_ratio <- 1 / intensity_drop_ratio
  cutoff <- max(plot_data$intensity) * intensity_drop_ratio
  if (cutoff < 1) cutoff <- 0
  plot_data <- plot_data %>%
    mutate(ion_group = round(mz, intensity_mz_resolution)) %>%
    group_by(ion_group) %>%
    filter(max(intensity) > cutoff) %>%
    arrange(intensity)
  plot_labs <- ms_plot_titles(plot_data = plot_data,
                              mz_resolution = intensity_mz_resolution,
                              drop_ratio = intensity_drop_ratio,
                              include_method = include_method,
                              db_conn = db_conn)
  out <- plot_data %>%
    ggplot(aes(x = scantime,
               y = ion_group,
               color = intensity,
               size = intensity)) +
    geom_point(show.legend = FALSE) +
    labs(x = "Scan Time",
         y = "Mass to charge ratio (m/z)",
         title = plot_labs$title,
         subtitle = plot_labs$subtitle,
         caption = plot_labs$caption) +
    theme_bw()
  if (!is.null(intensity_facet_by) && intensity_facet_by %in% names(plot_data)) {
    out <- out +
      facet_grid(cols = vars(.data[[intensity_facet_by]]))
  }
  return(out)
}

#' Create a patchwork plot of peak spectral properties
#'
#' Call this function to generate a combined plot from [ms_plot_peak],
#' [ms_plot_spectra], and [ms_plot_spectral_intensity] using the [patchwork]
#' package, which must be installed. All arguments will be passed directly to
#' the underlying functions to provide flexibility in the final display. The
#' default settings match those of the called plotting functions, and the output
#' can be further manipulated with the patchwork package.
#'
#' @note Requires a live connection to the database to pull all plots for a
#'   given peak_id.
#' @note Defaults are as for called functions
#'
#' @inheritParams ms_plot_peak
#' @inheritParams ms_plot_spectra
#' @inheritParams ms_plot_spectral_intensity
#'
#' @param patchwork_design the layout of the final plot see [patchwork::design]
#' @param as_individual_plots LGL scalar of whether to return the plots
#'   individually in a list (set TRUE) or as a patchwork plot (default: FALSE)
#'
#' @return object of classes `gg` and `ggplot`, as a patchwork unless
#'   `as_individual_plots` is TRUE
#' @export
#' 
ms_plot_peak_overview <- function(plot_peak_id,
                                  peak_type = c("area", "line", "segment"),
                                  peak_facet_by = "ms_n",
                                  peak_mz_resolution = 0,
                                  peak_drop_ratio = 1e-2,
                                  peak_repel_labels = TRUE,
                                  peak_line_color = "black",
                                  peak_fill_color = "grey50",
                                  peak_fill_alpha = 0.2,
                                  peak_text_size = 3,
                                  peak_text_offset = 0.02,
                                  spectra_mz_resolution = 3,
                                  spectra_drop_ratio = 1e-2,
                                  spectra_repel_labels = TRUE,
                                  spectra_repel_line_color = "grey50",
                                  spectra_nudge_y_factor = 0.03,
                                  spectra_log_y = FALSE,
                                  spectra_text_size = 3,
                                  spectra_max_overlaps = 50,
                                  intensity_plot_resolution = c("spectra", "peak"),
                                  intensity_mz_resolution = 3,
                                  intensity_drop_ratio = 0,
                                  patchwork_design = c(
                                      area(1, 4, 7, 7),
                                      area(1, 1, 4, 2),
                                      area(6, 1, 7, 2)
                                    ),
                                  as_individual_plots = FALSE,
                                  include_method = TRUE,
                                  db_conn = con,
                                  log_ns = "global"
) {
  stopifnot(require(patchwork),
            active_connection(db_conn),
            length(plot_peak_id) == 1)
  intensity_plot_resolution <- match.arg(intensity_plot_resolution)
  peak_type <- match.arg(peak_type)
  these_args <- as.list(environment())
  peak_data <- tbl(db_conn, "ms_data") %>%
    filter(peak_id == plot_peak_id) %>%
    collect()
  if (nrow(peak_data) == 0) {
    log_it("error", glue::glue("No data exist for peak {plot_peak_id}."), log_ns)
    return(NULL)
  }
  if (intensity_plot_resolution == "spectra") {
    spectral_data <- tbl(db_conn, "peak_spectra") %>%
      filter(peak_id == plot_peak_id) %>%
      collect()
    if (nrow(spectral_data) == 0) {
      log_it("warn", glue::glue("Spectra are not unpacked for peak {plot_peak_id}. Using the peak data instead."), log_ns)
      spectral_data <- peak_data
    }
  } else {
    spectral_data <- peak_data
  }
  
  plot_titles <- ms_plot_titles(plot_data = peak_data,
                                mz_resolution = peak_mz_resolution,
                                drop_ratio = peak_drop_ratio,
                                include_method = include_method,
                                db_conn = db_conn)
  
  plot_peak <- do.call(
    ms_plot_peak,
    append(
      list(data = peak_data),
      these_args[names(these_args) %in% names(formals(ms_plot_peak))]
    )
  )
  
  plot_spectra <- do.call(
    ms_plot_spectra,
    append(
      list(data = spectral_data),
      these_args[names(these_args) %in% names(formals(ms_plot_spectra))]
    )
  )
  
  plot_intensity <- do.call(
    ms_plot_spectral_intensity,
    append(
      list(data = switch(intensity_plot_resolution,
                  "peak" = peak_data,
                  "spectra" = spectral_data)),
      these_args[names(these_args) %in% names(formals(ms_plot_spectral_intensity))]
    )
  )
  
  if (as_individual_plots) {
    return(
      list(
        peak_overview = plot_peak,
        spectral_intensity = plot_intensity,
        annotations = plot_spectra
      )
    )
  } else {
    plot_spectra +
      labs(title = NULL,
           subtitle = "Fragment Annotations") +
      plot_peak +
      labs(title = NULL,
           y = "Intensity",
           subtitle = "Peak Overview") +
      plot_intensity +
      labs(title = NULL,
           y = "m/z",
           subtitle = sprintf("Intensity (%s)",
                              ifelse(intensity_plot_resolution == "peak",
                                     "Peak",
                                     "Spectra"))) +
      plot_layout(design = patchwork_design) +
      plot_annotation(
        title = plot_titles$title,
        subtitle = plot_titles$subtitle
      )
  }
}
