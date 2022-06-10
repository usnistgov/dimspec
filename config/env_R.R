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
DB_CONN_NAME   <- "con"
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
                    "usethis",
                    "XML")

# Decide whether to use ChemmineR, which is only available through Bioconductor,
# or RDKit through reticulate (preferred). This is controlled through USE_RDKIT
# in env_glob.txt by default. Reticulate and the "rcdk" package often cause
# conflicts due to the "rJava" package, making "rpytools" unavailable to
# reticulate.
if (ifelse(exists("INFORMATICS"), INFORMATICS, FALSE)) {
  if (ifelse(exists("USE_RDKIT"), !USE_RDKIT, FALSE)) {
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
}

# Files matching these patterns will be excluded from sourcing at startup.
EXCLUSIONS       <- c(".RDS",
                      paste0("suspectlist", .Platform$file.sep),
                      "compliance",
                      "create_method_list",
                      "generate_db",
                      "metadata_xml")

# Import map to use ------------------------------------------------------------
IMPORT_MAP       <- file.path("config", "map_NTA_MRT.csv")
if (!file.exists(IMPORT_MAP)) IMPORT_MAP <- file.path("..", "..", IMPORT_MAP)
if (file.exists(IMPORT_MAP)) {
  IMPORT_MAP <- readr::read_csv(IMPORT_MAP)
} else {
  IMPORT_MAP <- NULL
}

# Runtime quality assurance/control --------------------------------------------
# Whether to use application logging to print or record log messages during use.
# Settings are in the config/env_logger.R file.
# Whether to activate logging for this session. [SET IN env_glob.txt, override here if necessary.]
LOGGING_ON       <- ifelse(exists("LOGGING_ON"), LOGGING_ON, TRUE)

# Whether to use function argument verification. Set to FALSE to serve as a
# global cut off and increase execution speed.
VERIFY_ARGUMENTS <- TRUE

# Set speed and performance boost. Setting this to true will turn off function
# argument verification, logging,
MINIMIZE         <- FALSE
if (MINIMIZE) {
  VERIFY_ARGUMENTS <- FALSE
  LOGGING_ON       <- FALSE
}

# Plumber API ------------------------------------------------------------------
# Plumber host options. [ADVANCED USE ONLY]
# Whether to activate plumber integration. [SET IN env_glob.txt]
USE_API <- ifelse(exists("USE_API"), USE_API, TRUE)
if (USE_API) {
  PLUMBER_HOST <- getOption("plumber.host", "127.0.0.1")
  PLUMBER_PORT <- getOption("plumber.port", 8080)
  PLUMBER_URL  <- sprintf("http://%s:%s", PLUMBER_HOST, PLUMBER_PORT)
  PLUMBER_FILE <- file.path("plumber", "plumber.R")
}

# Shiny options ----------------------------------------------------------------
# Placeholder for shiny app options in future development
# Whether to spin up Shiny apps. [SET IN env_glob.txt]
USE_SHINY <- ifelse(exists("USE_SHINY"), USE_SHINY, TRUE)
if (USE_SHINY) {
  USE_SHINY    <- ifelse(exists("USE_SHINY"), USE_SHINY, FALSE)
  SHINY_APPS   <- list.dirs("apps", full.names = TRUE)
  SHINY_HOST   <- getOption("shiny.host", "127.0.0.1")
}