# Environment variables specific to R.

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = sprintf("%s$", DB_NAME),
                                       recursive = TRUE))$ctime
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
                                 NA)
)
INIT_CONNECT   <- TRUE

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
