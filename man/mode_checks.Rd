% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_functions.R
\name{mode_checks}
\alias{mode_checks}
\title{Get list of available functions}
\usage{
mode_checks(prefix = "is", use_deprecated = FALSE)
}
\arguments{
\item{prefix}{CHR scalar for the function prefix to search (default "is")}

\item{use_deprecated}{BOOL scalar indicating whether or not to include
functions marked as deprecated (PLACEHOLDER default FALSE)}
}
\value{
CHR vector of functions matching \code{prefix}
}
\description{
Helper function for \code{\link{verify_args()}} that returns all the
currently available functions matching a given prefix. This searches the
entire library associated with the current R install.
}
\details{
Note: argument \code{use_deprecated} is not currently used but serves as a
placeholder for future development to avoid or include deprecated functions
}
\examples{
mode_checks()
}
