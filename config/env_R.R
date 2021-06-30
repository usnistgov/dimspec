# Environment variables specific to R.

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = DB_NAME, recursive = TRUE))$ctime
DB_VERSION     <- ifelse(length(DB_DATE) > 0, "0.0.1", NA)
DB_BUILT       <- !is.na(DB_VERSION)

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
                    "stringi",
                    "tidyverse",
                    "dbplyr",
                    "tools",
                    "xlsx",
                    "XML")

# Files matching these patterns will be excluded from sourcing.
EXCLUSIONS     <- c(".RDS",
                    "compliance",
                    "create_method_list",
                    "generate_db",
                    "metadata_xml")

# Convenience collection of the items above is available from `support_info()`
# once `source("compliance.R")` is complete.
