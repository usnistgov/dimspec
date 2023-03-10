% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral_comparison.R
\name{pool.ums}
\alias{pool.ums}
\title{Pool uncertainty mass spectra}
\usage{
pool.ums(umslist, error = 5, minerror = 0.002)
}
\arguments{
\item{umslist}{A list where each item is a uncertainty mass spectrum from function `get_ums`}

\item{minerror}{the minimum mass error (in Da) of the instrument data}

\item{masserror}{the mass accuracy (in ppm) of the instrument data}
}
\description{
Calculates a pooled uncertainty mass spectrum that is a result of data from multiple 
uncertainty mass spectra.
}
