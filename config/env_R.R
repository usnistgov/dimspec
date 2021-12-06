# Environment variables specific to R.

# Current version of the database and the date it was created (if present)
DB_DATE        <- file.info(list.files(pattern = sprintf("%s$", DB_NAME),
                                       recursive = FALSE))$ctime
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
INCLUDE_DICT   <- TRUE
INCLUDE_MAP    <- TRUE

# Set this to TRUE to isolate environment to the renv.lock file
USE_RENV_LOCK  <- TRUE

# The last time the main database schema defined in BUILD_FILE was updated
LAST_DB_SCHEMA <- file.info(
  list.files(path = "config",
             pattern = DB_BUILD_FILE,
             recursive = FALSE)
)$mtime

# The last time any file in this project's config directory was modified
search_paths <- list.dirs(recursive = FALSE)
path_exclude <- "^\\./\\.[[:alnum:]]*|renv|data/backups"
search_paths <- search_paths[-grep(path_exclude, search_paths)]
LAST_MODIFIED  <- max(
  file.info(
    list.files(path = search_paths,
               recursive = TRUE,
               full.names = TRUE
    )
  )$mtime,
  na.rm = TRUE)
rm(search_paths, path_exclude)

# Set logger threshold.
LOG_THRESHOLD  <- "INFO"
LOGGING_ON     <- TRUE

# These are the packages on which the project depends and must be loaded.
DEPENDS_ON     <- c("base64enc",
                    "logger",
                    "DBI",
                    "RSQLite",
                    "glue",
                    "stringi",
                    "tidyverse",
                    "jsonlite",
                    "rlang",
                    "tools",
                    "xlsx",
                    "XML")

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

# Convenience collection of the items above is available from `support_info()`
# once `source("compliance.R")` is complete.
