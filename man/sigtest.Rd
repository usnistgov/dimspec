% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral_comparison.R
\name{sigtest}
\alias{sigtest}
\title{Significance testing function}
\usage{
sigtest(x1, x2, s1, s2, n1, n2, sig = 0.95)
}
\arguments{
\item{x1, x2}{mean values to be compared}

\item{s1, s2}{standard deviation of their respective values}

\item{n1, n2}{number of observations of the respective values}

\item{sig}{significance level to test (0.95 = 95\%)}
}
\description{
Internal function: enables significance testing between two values
}
