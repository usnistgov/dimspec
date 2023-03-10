% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_comm.R
\name{save_data_dictionary}
\alias{save_data_dictionary}
\title{Save the current data dictionary to disk}
\usage{
save_data_dictionary(db_conn = con)
}
\arguments{
\item{db_conn}{connection object (default: con)}

\item{output_format}{CHR scalar, one of (capitalization insensitive) "json",
"csv", "data.frame", or "list" (default "json")}

\item{output_file}{CHR scalar indicating where to save the resulting file; an
appropriate file name will be constructed if left NULL (default: NULL)}

\item{overwrite_existing}{LGL scalar indicating whether to overwrite an
existing file whose name matches that determined from `output_file`
(default: TRUE); file names will be appended with "(x)" sequentially if
this is FALSE and a file with matching name exists.}
}
\value{
None, saves a file to the current working directory
}
\description{
Executes [data_dictionary()] and saves the output to a local file. If \code{output_format}
is one of "data.frame" or "list", the resulting file will be saved as an RDS.
Parameter \code{output_file} will be used during the save process; relative paths
will be identified by the current working directory.
}
