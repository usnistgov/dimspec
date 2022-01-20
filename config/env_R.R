# Environment variables specific to R.

# Several settings reference project variables defined in env_glob.txt

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = sprintf("%s$", DB_NAME), recursive = !EXPLICIT_PATHS))$ctime
DB_BUILT       <- length(DB_DATE) > 0
# Releases will be coded by:
#   - First position: major version (e.g. schema changes)
#   - Second position: application tooling changes (e.g. new tools)
#   - Third position: data changes (e.g. new compounds)
#   - Fourth position: creation date
DB_RELEASE     <- "0.0.0"
DB_VERSION     <- sprintf("%s.%s",
                          DB_RELEASE,
                          ifelse(DB_BUILT,
                                 format(DB_DATE, "%Y%m%d"),
                                 format(Sys.Date(), "%Y%m%d"))
)

# Supplemental information about working with this database
DB_PACKAGE     <- "RSQLite"
DB_DRIVER      <- "SQLite"
DB_CLASS       <- "SQLite"
DICT_FILE_NAME <- "data_dictionary"

# The last time the main database schema defined in BUILD_FILE was updated.
LAST_DB_SCHEMA <- file.info(list.files(pattern = DB_BUILD_FILE, recursive = !EXPLICIT_PATHS))$mtime

# The last time any file in this project was modified.
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
                    # "xlsx",
                    "XML")

# Decide whether to use ChemmineR, which is only available through Bioconductor,
# or RDKit through reticulate (preferred). This is controlled through USE_RDKIT
# in env_glob.txt by default. Reticulate and the "rcdk" package often cause
# conflicts due to the "rJava" package, making "rpytools" unavailable to
# reticulate.
if (ifelse(exists("USE_RDKIT"), !USE_RDKIT, TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  if (!requireNamespace("ChemmineR", quiety = TRUE)) {
    BiocManager::install("ChemmineR")
  }
  library("ChemmineR")
  if (!"rcdk" %in% installed.packages()) install.packages("rcdk")
  library("rcdk")
}

# Files matching these patterns will be excluded from sourcing at startup.
EXCLUSIONS     <- c(".RDS",
                    "compliance",
                    "create_method_list",
                    "generate_db",
                    "metadata_xml")

# Whether to use function argument verification. Set to FALSE to serve as a
# global cut off and increase execution speed.
VERIFY_ARGUMENTS <- TRUE

# WIP: Import namespace checks
IMPORT_HEADERS <- list(
  software = "msconvertsettings",
  samples  = "sample",
  method   = c("chromatography", "massspectrometry", "qcmethod"),
  data     = c("peak", "compounddata", "annotation", "msdata", "qc")
)

# Plumber host options. [ADVANCED USE ONLY]
if (ACTIVATE_API) {
  PLUMBER_HOST   <- "127.0.0.1"
  # PLUMBER_PORT   <- getOption("plumber.port", NULL)
  PLUMBER_PORT   <- 8080
}

# A collection of the items above is available from `support_info()` as a
# troubleshooting convenience once `source("compliance.R")` is complete.
