% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_qc.R
\name{report_qc}
\alias{report_qc}
\title{Export QC result JSONfile into PDF}
\usage{
report_qc(
  jsonfile = file.choose(),
  outputfile = gsub(".json", ".pdf", jsonfile, ignore.case = TRUE)
)
}
\arguments{
\item{jsonfile}{jsonfile file path}

\item{outputfile}{output pdf file path}
}
\value{
generates reporting PDF
}
\description{
Export QC result JSONfile into PDF
}
