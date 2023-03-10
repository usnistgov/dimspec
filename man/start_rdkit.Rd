% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_functions.R
\name{start_rdkit}
\alias{start_rdkit}
\title{Start the RDKit integration}
\usage{
start_rdkit(src_dir = here::here("inst", "rdkit"), log_ns = "rdkit")
}
\arguments{
\item{src_dir}{CHR scalar file path to settings and functions enabling rdkit
(default: here::here("inst", "rdkit"))}

\item{log_ns}{CHR scalar name of the logging namespace to use for this
function (default: "rdkit")}
}
\value{
LGL scalar indicating whether starting RDKit integration was
  successful
}
\description{
If the session was started without RDKit integration, e.g. INFORMATICS or
USE_RDKIT were FALSE in [config/env_R.R], start up RDKit in this session.
}
\note{
RDKit and rcdk are incompatible. If the session was started with
  INFORMATICS = TRUE and USE_RDKIT = FALSE, ChemmineR was likely loaded. If
  this is the case, the session will need to be restarted due to java
  conflicts between the two.
}
