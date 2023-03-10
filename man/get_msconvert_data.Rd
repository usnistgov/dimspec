% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_gather_json.R
\name{get_msconvert_data}
\alias{get_msconvert_data}
\title{Extract msconvert metadata}
\usage{
get_msconvert_data(mzml)
}
\arguments{
\item{mzml}{list of msdata from `mzMLtoR` function}
}
\value{
list of msconvert parameters
}
\description{
Extracts relevant Proteowizard MSConvert metadata from mzml file.
Used for `peak_gather_json` function
}
