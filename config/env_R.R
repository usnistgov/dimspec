# Environment variables specific to R.

# A collection of these items is available using `support_info()` as a
# troubleshooting convenience once `source("compliance.R")` is complete.

# Several settings reference project variables defined in env_glob.txt
if (!exists("DB_NAME")) source(file.path("config", "env_glob.txt"))

# Database ---------------------------------------------------------------------
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

# Dependencies -----------------------------------------------------------------
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

# Runtime quality assurance ----------------------------------------------------
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

# Plumber API ------------------------------------------------------------------
# Plumber host options. [ADVANCED USE ONLY]
if (USE_API) {
  PLUMBER_HOST <- getOption("plumber.host", "127.0.0.1")
  PLUMBER_PORT <- getOption("plumber.port", 8080)
  PLUMBER_URL  <- sprintf("http://%s:%s", PLUMBER_HOST, PLUMBER_PORT)
  PLUMBER_FILE <- file.path("plumber", "plumber.R")
}

# Shiny options ----------------------------------------------------------------
# Placeholder for shiny app options in future development
if (USE_SHINY) {
  USE_SHINY    <- ifelse(exists("USE_SHINY"), USE_SHINY, FALSE)
  SHINY_APPS   <- list.dirs("apps", full.names = TRUE)
  SHINY_HOST   <- getOption("shiny.host", "127.0.0.1")
}

# Logging options --------------------------------------------------------------
# Whether or not to log actions throughout this project. LOGGING_ON == FALSE
# will override individual settings and prevent logging. Set LOGGING_ON in
# env_glob.txt to control broadly, or override that setting here as needed.
LOGGING_ON     <- ifelse(exists("LOGGING_ON"), LOGGING_ON, TRUE)
LOGGING_GLOBAL <- TRUE
LOGGING_DB     <- TRUE
LOGGING_API    <- TRUE
LOGGING_RDK    <- TRUE
LOGGING_SHINY  <- FALSE
LOGGING_WARNS  <- TRUE
LOGGING_ERRORS <- TRUE
# Destination of logging messages. Set "console" for console only, "file" for
# file only which will write to "logs/logger.txt", or "both" to do both. If it
# is not an interactive session, these will default to the file option.
LOG_GLOBAL_TO  <- "both"
LOG_API_TO     <- "both"
LOG_DB_TO      <- "both"
LOG_RDK_TO     <- "both"
LOG_SHINY_TO   <- "file"
LOG_DIRECTORY  <- file.path("logs")
LOG_FILE_GLOBAL<- file.path(LOG_DIRECTORY, "log.txt")
LOG_FILE_DB    <- file.path(LOG_DIRECTORY, "log_db.txt")
LOG_FILE_API   <- file.path(LOG_DIRECTORY, "log_api.txt")
LOG_FILE_RDK   <- file.path(LOG_DIRECTORY, "log_rdk.txt")
LOG_FILE_SHINY <- file.path(LOG_DIRECTORY, "log_shiny.txt")
# Set the logging threshold, which if using the logger package, should be a
# valid logging level (e.g. TRACE, DEBUG, INFO, SUCCESS, WARN, ERROR, or FATAL).
# If not using the logger package, or if you issue a custom logging level to
# `log_it` it will still include in the log.
LOG_THR_GLOBAL <- "INFO"
LOG_THR_DB     <- "INFO"
LOG_THR_API    <- "INFO"
LOG_THR_RDK    <- "INFO"
LOG_THR_SHINY  <- "INFO"
