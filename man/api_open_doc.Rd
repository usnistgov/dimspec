% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_control.R
\name{api_open_doc}
\alias{api_open_doc}
\title{Open Swagger API documentation}
\usage{
api_open_doc(url = PLUMBER_URL)
}
\arguments{
\item{url}{CHR URL/URI of the plumber documentation host (default:
environment variable "PLUMBER_URL")}
}
\value{
None, opens a browser to the requested URL
}
\description{
This will launch the Swagger UI in a browser tab. The URL suffix "__docs__"
will be automatically added if not part of the host URL accepted as `url`.
}
