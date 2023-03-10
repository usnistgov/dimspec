% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIST_import_routines.R
\name{add_or_get_id}
\alias{add_or_get_id}
\title{Utility function to add a record}
\usage{
add_or_get_id(
  db_table,
  values,
  db_conn = con,
  ensure_unique = TRUE,
  require_all = TRUE,
  ignore = FALSE,
  log_ns = "db"
)
}
\arguments{
\item{db_table}{CHR scalar name of the database table being modified}

\item{values}{named vector of the values being added, passed to
[build_db_action]}

\item{db_conn}{connection object (default: con)}

\item{ensure_unique}{LGL scalar of whether or not to first check that the
values provided form a new unique record (default: TRUE)}

\item{require_all}{LGL scalar of whether to require all columns (except the
assumed primary key column of "id") or only those defined as "NOT NULL"
(default: TRUE requires the presence of all columns in the table)}

\item{ignore}{LGL scalar on whether to treat the insert try as an "INSERT OR
IGNORE" SQL statement (default: FALSE)}

\item{log_ns}{CHR scalar of the logging namespace to use (default: "db")}
}
\value{
INT scalar of the record identifier
}
\description{
Checks a table in the attached SQL connection for a primary key ID matching
the provided `values` and returns the ID. If none exists, adds a record and
returns the resulting ID if successful. Values should be provided as a named
vector of the values to add. No data coercion is performed, relying almost
entirely on the database schema or preprocessing to ensure data integrity.
}
\details{
Provided values are checked agaisnt required columns in the table using
[verify_import_columns].

Operations to add the record and get the resulting ID are both performed with
[build_db_action] and are performed virtually back to back with the
latest-added ID being given preference in cases where added values may match
multiple extant records.
}
\note{
If this is used in high volume/traffic applications, ID conflicts may
  occur if the timing is such that another record containing identical values
  is added before the call getting the ID completes.
}
