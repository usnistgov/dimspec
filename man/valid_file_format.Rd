% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_functions_msqc.R,
%   R/app_functions_spectral_match.R
\name{valid_file_format}
\alias{valid_file_format}
\title{Ensure files uploaded to a shiny app are of the required file type}
\usage{
req(valid_file_format(input$file_upload, c(".csv", ".xls")))

req(valid_file_format(input$file_upload, c(".csv", ".xls")))
}
\arguments{
\item{filename}{CHR scalar name of the file uploaded to the shiny server}

\item{accepts}{CHR vector of acceptable file formats}

\item{show_alert}{LGL scalar indicating whether or not to show an alert, set
FALSE to return the status of the check}
}
\value{
Whether or not all required values are present.

Whether or not all required values are present.
}
\description{
This input validation check uses [tools::file_ext] to ensure that files
uploaded to [shiny::fileInput] are among the acceptable file formats. Users
may sometimes wish to load a file outside the "accepts" format list by
manually changing it during the upload process. If they are not, a
[nist_shinyalert] modal is displayed prompting the user to upload a file in
one of the requested formats.

This input validation check uses [tools::file_ext] to ensure that files
uploaded to [shiny::fileInput] are among the acceptable file formats. Users
may sometimes wish to load a file outside the "accepts" format list by
manually changing it during the upload process. If they are not, a
[nist_shinyalert] modal is displayed prompting the user to upload a file in
one of the requested formats.
}
