# Environment variables specific to R.

# Set to search for files in any directory. Setting to TRUE will increase
# fidelity but also load times.

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = sprintf("%s$", DB_NAME), recursive = !EXPLICIT_PATHS))$ctime
DB_BUILT       <- length(DB_DATE) > 0
# Releases will be coded by:
#   - First position: major version (e.g. schema changes)
#   - Second position: data changes (e.g. new compounds)
#   - Third position: application tooling changes (e.g. new tools)
#   - Fourth position: creation date
DB_RELEASE     <- "0.0.0"
DB_VERSION     <- sprintf("%s.%s",
                          DB_RELEASE,
                          ifelse(DB_BUILT,
                                 format(DB_DATE, "%Y%m%d"),
                                 format(Sys.Date(), "%Y%m%d"))
)
INIT_CONNECT   <- TRUE

# Supplemental information about working with this database
DB_PACKAGE     <- "RSQLite"
DB_DRIVER      <- "SQLite"
DB_CLASS       <- "SQLite"
DICT_FILE_NAME <- "data_dictionary"

# The last time the main database schema defined in BUILD_FILE was updated
LAST_DB_SCHEMA <- file.info(list.files(pattern = DB_BUILD_FILE,recursive = !EXPLICIT_PATHS))$mtime

# The last time any file in this project was modified
LAST_MODIFIED  <- max(file.info(list.files(recursive = !EXPLICIT_PATHS))$mtime)

# Set logger threshold.
LOG_THRESHOLD  <- "INFO"
LOGGING_ON     <- TRUE

# These are the packages on which the project depends and must be loaded.
DEPENDS_ON     <- c("base64enc",
                    "logger",
                    "DBI",
                    "RSQLite",
                    "lubridate",
                    "glue",
                    "stringi",
                    "tidyverse",
                    "dbplyr",
                    "jsonlite",
                    "tools",
                    "xlsx",
                    "XML")

# Also needs ChemmineR, which is only available through Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
  BiocManager::install("ChemmineR")
}

# Files matching these patterns will be excluded from sourcing.
EXCLUSIONS     <- c(".RDS",
                    "compliance",
                    "create_method_list",
                    "generate_db",
                    "metadata_xml")

# Use function argument verification. Set to FALSE to serve as a global cut off
# and increase execution speed.
VERIFY_ARGUMENTS <- TRUE

# Import namespace checks
IMPORT_HEADERS <- list(
  software = "msconvertsettings",
  samples  = "sample",
  method   = c("chromatography", "massspectrometry", "qcmethod"),
  data     = c("peak", "compounddata", "annotation", "msdata", "qc")
)

# Run the plumber API?
# Set to true to run plumber
ACTIVATE_API   <- TRUE
PLUMBER_HOST   <- "127.0.0.1"
# PLUMBER_PORT   <- getOption("plumber.port", NULL)
PLUMBER_PORT   <- 8080

# Convenience collection of the items above is available from `support_info()`
# once `source("compliance.R")` is complete.
