% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_comm.R
\name{resolve_multiple_values}
\alias{resolve_multiple_values}
\title{Utility function to resolve multiple choices interactively}
\usage{
resolve_multiple_values(values, search_value, as_regex = FALSE, db_table = "")
}
\arguments{
\item{values}{CHR vector of possible values}

\item{search_value}{CHR scalar of the value to search}

\item{as_regex}{LGL scalar of whether to treat `search_value` as a regular
expression string (TRUE) or to use it directly (FALSE, default)}

\item{db_table}{CHR scalar name of the database table to search, used for
printing log messages only (default: "")}
}
\value{
CHR scalar result of the user's choice
}
\description{
This function is generally not called directly, but rather as a workflow
component from within [resolve_normalization_value] during interactive
sessions to get feedback from users during the normalization value resolution
process.
}
