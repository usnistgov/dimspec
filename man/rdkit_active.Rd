% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/py_setup.R
\name{rdkit_active}
\alias{rdkit_active}
\title{Sanity check on RDKit binding}
\usage{
rdkit_active(
  rdkit_ref = NULL,
  rdkit_name = NULL,
  log_ns = NULL,
  make_if_not = FALSE
)
}
\arguments{
\item{rdkit_ref}{CHR scalar OR R object of an RDKit binding (default NULL
goes to "rdk" for convenience with other pipelines in this project)}

\item{rdkit_name}{CHR scalar the name of a python environment able to run
rdkit (default NULL goes to "rdkit" for convenience with other pipelines in
this project)}

\item{log_ns}{}

\item{make_if_not}{LGL scalar of whether or not to create a new python
environment using [activate_py_env] if the binding is not active}
}
\value{
LGL scalar of whether or not the test of RDKit was successful
}
\description{
Given a name of an R object, performs a simple check on RDKit availability on
that object, creating it if it does not exist. A basic structure conversion
check is tried and a TRUE/FALSE result returned. Leave all arguments as their
defaults of NULL to ensure they will honor the settings in `rdkit/env_py.R`.
}
