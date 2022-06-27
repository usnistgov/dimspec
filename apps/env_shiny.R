# Set up environment -----------------------------------------------------------
if (!exists("DB_TITLE")) source(here::here("config", "env_glob.txt"))
USE_API      <- TRUE
USE_SHINY    <- TRUE
INFORMATICS  <- TRUE
USE_RDKIT    <- TRUE
LOGGING_ON   <- TRUE
DB_CONN_NAME <- "con"
if (!exists("RENV_ESTABLISHED") || !RENV_ESTABLISHED) source(here::here("config", "env_R.R"))
if (!exists("RENV_ESTABLISHED_COMPLIANCE") || !RENV_ESTABLISHED_COMPLIANCE) source(here::here("R", "compliance.R"))
packs <- c("shiny",
           "shinyalert",
           "shinydashboard",
           "shinycssloaders",
           "shinyjs",
           "shinydisconnect",
           "shinythemes",
           "DT",
           "plotly",
           "httr",
           "readr",
           "readxl")
packs_TRUE  <- which(packs %in% installed.packages())
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   quiet        = TRUE,
                   dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)
rm(packs)
# 
# # Set up logger ----------------------------------------------------------------
# # Set this as the name of the "LOGGING" element referring to the API settings
# # from env_logger.R e.g. "SHINY" to refer to LOGGING$SHINY (the default)
log_ns <- "SHINY"
LOG_DIRECTORY <- here::here(ifelse(exists("LOG_DIRECTORY"), LOG_DIRECTORY, "logs"))
logger_settings <- list(
  list(
    log = TRUE,
    ns = tolower(log_ns),
    to = "file",
    file = file.path(LOG_DIRECTORY, "log_shiny.txt"),
    threshold = "info")
) %>%
  setNames(log_ns)
if (!exists("LOGGING")) {
  LOGGING <- logger_settings
} else if (!log_ns %in% names(LOGGING)) {
  LOGGING <- append(
    LOGGING,
    logger_settings
  )
} else if (!LOGGING[[log_ns]]$log) {
  LOGGING[[log_ns]]$log <- TRUE
}
require(logger)
update_logger_settings()

RENV_ESTABLISHED_SHINY <- TRUE
log_it("info", "Shiny environment established.", "shiny")
