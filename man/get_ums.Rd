% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uncertainty.R
\name{get_ums}
\alias{get_ums}
\title{Generate consensus mass spectrum}
\usage{
get_ums(
  peaktable,
  correl = NULL,
  ph = NULL,
  freq = NULL,
  normfn = "sum",
  cormethod = "pearson"
)
}
\arguments{
\item{peaktable}{result of the `create_peak_table_ms1` or  `create_peak_table_ms1` function}

\item{correl}{Minimum correlation coefficient between the target ions and the base ion intensity of the targeted m/z to be included in the mass spectrum}

\item{ph}{Minimum chromatographic peak height from which to extract MS2 data for the mass spectrum}

\item{freq}{minimum observational frequency of the target ions to be included in the mass spectrum}

\item{normfn}{the normalization function typically "mean" or "sum" for normalizing the intensity values}

\item{cormethod}{the correlation method used for calculating the correlation, see `cor` function for methods}
}
\value{
nested list of dataframes containing all MS1 and MS2 data for the peak
}
\description{
The function calculates the uncertainty mass spectrum for a single peak table based
on specific settings described in https://doi.org/10.1021/jasms.0c00423
}
