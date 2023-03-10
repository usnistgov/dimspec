% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_gather_json.R
\name{table_msdata}
\alias{table_msdata}
\title{Tabulate MS Data}
\usage{
table_msdata(mzml, scans, mz = NA, zoom = NA, masserror = NA, minerror = NA)
}
\arguments{
\item{mzml}{list of msdata from `mzMLtoR` function}

\item{scans}{integer vector containing scan numbers to extract MS data}

\item{mz}{numeric targeted m/z}

\item{zoom}{numeric vector specifying the range around m/z, from m/z - zoom[1] to m/z + zoom[2]}

\item{masserror}{numeric relative mass error (in ppm) of the instrument}

\item{minerror}{numeric minimum mass error (in Da) of the instrument}
}
\value{
data.frame containing MS data
}
\description{
Pulls specified MS Data from mzML and converts it into table format for further processing
Internal function for `peak_gather_json` function
}
