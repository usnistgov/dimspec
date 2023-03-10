% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_suspectlist.R
\name{suspectlist_at_NIST}
\alias{suspectlist_at_NIST}
\title{Open the NIST PDR entry for the current NIST PFAS suspect list}
\usage{
suspectlist_at_NIST(url_file = file.path("config", "suspectlist_url.txt"))
}
\value{
none
}
\description{
This simply points your browser to the NIST public data repository for the
current NIST suspect list, where you can find additional information. Click
the download button in the left column of any file to download it.
s
Requires the file "suspectlist_url.txt" to be present in the `config`
subdirectory of the current working directory.
}
\examples{
suspectlist_at_NIST()
}
