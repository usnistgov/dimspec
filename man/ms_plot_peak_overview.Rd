% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_spectra.R
\name{ms_plot_peak_overview}
\alias{ms_plot_peak_overview}
\title{Create a patchwork plot of peak spectral properties}
\usage{
ms_plot_peak_overview(
  plot_peak_id,
  peak_type = c("area", "line", "segment"),
  peak_facet_by = "ms_n",
  peak_mz_resolution = 0,
  peak_drop_ratio = 0.01,
  peak_repel_labels = TRUE,
  peak_line_color = "black",
  peak_fill_color = "grey50",
  peak_fill_alpha = 0.2,
  peak_text_size = 3,
  peak_text_offset = 0.02,
  spectra_mz_resolution = 3,
  spectra_drop_ratio = 0.01,
  spectra_repel_labels = TRUE,
  spectra_repel_line_color = "grey50",
  spectra_nudge_y_factor = 0.03,
  spectra_log_y = FALSE,
  spectra_text_size = 3,
  spectra_max_overlaps = 50,
  intensity_plot_resolution = c("spectra", "peak"),
  intensity_mz_resolution = 3,
  intensity_drop_ratio = 0,
  patchwork_design = c(area(1, 4, 7, 7), area(1, 1, 4, 2), area(6, 1, 7, 2)),
  as_individual_plots = FALSE,
  include_method = TRUE,
  db_conn = con,
  log_ns = "global"
)
}
\arguments{
\item{peak_type}{CHR scalar of the plot type to draw, must be one of "line",
"segment", or "area" (default: "line")}

\item{peak_facet_by}{CHR scalar name of a column by which to facet the
resulting plot (default: "ms_n")}

\item{peak_mz_resolution}{INT scalar mass to charge ratio tolerance to group
peaks, with at minimum columns for intensity (as "base_int"), ion m/z value
(as "base_ion"), and scan time (as "scantime") - (default: 0 goes to unit
resolution)}

\item{peak_drop_ratio}{NUM scalar threshold of the maximum intensity below
which traces will be dropped (default: 1e-2 means any trace with a maximum
intensity less than 1\% of the maximum intensity in the plot will be
dropped); if > 1 the inversion will be used (1e5 -> 1e-5)}

\item{peak_repel_labels}{LGL scalar on whether to use the [ggrepel] package
to space out m/z labels in the plot (default: TRUE). If [ggrepel] is not
installed, it will default to FALSE rather than requiring an installation}

\item{peak_line_color}{CHR scalar name of the color to use for the "color"
aesthetic (only a single color is supported; default: "black")}

\item{peak_fill_color}{CHR scalar name of the color to use for the "fill"
aesthetic (only a single color is supported; default: "grey70")}

\item{peak_text_offset}{NUM scalar y-axis offset as a fraction of the maximum
intensity for trace annotation (default: 0.02 offsets labels in the
positive direction by 2\% of the maximum intensity)}

\item{spectra_mz_resolution}{INT scalar mass to charge ratio tolerance to
group peaks, with at minimum columns for intensity (as base_int), ion m/z
value (as base_ion), and scan time (as scantime) - (default: 3)}

\item{spectra_drop_ratio}{NUM scalar threshold of the maximum intensity below
which traces will be dropped (default: 1e-2 means any trace with a maximum
intensity less than 1\% of the maximum intensity in the plot will be
dropped); if > 1 the inversion will be used (1e5 -> 1e-5)}

\item{spectra_repel_labels}{LGL scalar on whether to use the [ggrepel]
package to space out m/z labels in the plot (default: TRUE). If [ggrepel]
is not installed, it will default to FALSE rather than requiring an
installation}

\item{spectra_repel_line_color}{CHR scalar name of the color to use for the
"color" aesthetic of the lines connecting repelled labels to their data
points; passed to [ggrepel::geom_text_repel] as segment.color (only a
single color is supported; default: "grey50")}

\item{spectra_nudge_y_factor}{NUM scalar y-axis offset as a fraction of the
maximum intensity for trace annotation (default: 0.03 offsets labels in the
positive direction by 3\% of the maximum intensity)}

\item{spectra_log_y}{LGL scalar of whether or not to apply a log10 scaling
factor to the y-axis (default: FALSE)}

\item{spectra_text_size}{NUM scalar of the text size to use for annotation
labels (default: 3)}

\item{spectra_max_overlaps}{INT scalar of the maximum number of text overlaps
to allow (default: 50)}

\item{intensity_mz_resolution}{INT scalar mass to charge ratio tolerance to
group peaks, with at minimum columns for intensity (as "base_int" or
"intensity"), ion m/z value (as "base_ion" or "mz"), and scan time (as
"scantime") - (default: 5)}

\item{intensity_drop_ratio}{NUM scalar threshold of the maximum intensity
below which traces will be dropped (default: 0 returns all); if > 1 the
inversion will be used (1e5 -> 1e-5)}

\item{patchwork_design}{the layout of the final plot see [patchwork::design]}

\item{as_individual_plots}{LGL scalar of whether to return the plots
individually in a list (set TRUE) or as a patchwork plot (default: FALSE)}

\item{db_conn}{database connection (default: con) which must be live to pull
sample and compound identification information}
}
\value{
object of classes `gg` and `ggplot`, as a patchwork unless
  `as_individual_plots` is TRUE
}
\description{
Call this function to generate a combined plot from [ms_plot_peak],
[ms_plot_spectra], and [ms_plot_spectral_intensity] using the [patchwork]
package, which must be installed. All arguments will be passed directly to
the underlying functions to provide flexibility in the final display. The
default settings match those of the called plotting functions, and the output
can be further manipulated with the patchwork package.
}
\note{
Requires a live connection to the database to pull all plots for a
  given peak_id.

Defaults are as for called functions
}
