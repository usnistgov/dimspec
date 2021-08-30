# Environment variables specific to R.

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = DB_NAME, recursive = TRUE))$ctime
DB_VERSION     <- "0.0.1"
DB_VERSION     <- ifelse(length(DB_DATE) > 0, DB_VERSION, NA)
DB_BUILT       <- !is.na(DB_VERSION)

# Supplemental information about working with this database
DB_PACKAGE     <- "RSQLite"
DB_DRIVER      <- "SQLite"
DB_CLASS       <- "SQLite"

# The last time the main database schema defined in BUILD_FILE was updated
LAST_DB_SCHEMA <- file.info(list.files(pattern = DB_BUILD_FILE, recursive = TRUE))$mtime

# The last time any file in this project was modified
LAST_MODIFIED  <- max(file.info(list.files(recursive = TRUE))$mtime)

# Set logger threshold.
LOG_THRESHOLD  <- "INFO"

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

# Convenience collection of the items above is available from `support_info()`
# once `source("compliance.R")` is complete.
