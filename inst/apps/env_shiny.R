# Set up environment -----------------------------------------------------------
if (!exists("start_api")) {
  source(here::here("R", "app_functions.R"))
  start_api()
}

# Check API
if (exists("PLUMBER_URL")) {
  if (!dplyr::between(api_endpoint(path = "_ping", raw_result = TRUE)$status, 200, 299)) {
    stop("API service does not appear to be available (PLUMBER_URL does not exist). Please run `api_reload()` and try again.")
  }
} else {
  stop("This app requires an active API connection.")
}

# Package requirements
packs <- c("dplyr",
           "tidyr",
           "jsonlite",
           "ggrepel",
           "shiny",
           "shinyBS",
           "shinyalert",
           "shinydashboard",
           "shinycssloaders",
           "shinyjs",
           "shinydisconnect",
           "shinythemes",
           "shinyWidgets",
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
# Set up logger ----------------------------------------------------------------
# Set this as the name of the "LOGGING" element referring to the API settings
# from env_logger.R e.g. "SHINY" to refer to LOGGING$SHINY (the default)
LOGGING_ON <- ifelse(exists("LOGGING_ON"), LOGGING_ON, TRUE)
if (LOGGING_ON) {
  if (!exists("RENV_ESTABLISHED_LOGGER") || !RENV_ESTABLISHED_LOGGER) source(here::here("config", "env_logger.R"))
  log_ns <- "SHINY"
  logger_settings <- list(
    list(
      log = TRUE,
      ns = tolower(log_ns),
      to = "file",
      file = file.path(LOG_DIRECTORY, sprintf("log_%s.txt", tolower(log_ns))),
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
  }
  LOGGING <<- LOGGING
  require(logger)
  update_logger_settings(log_all_warnings = FALSE, log_all_errors = FALSE)
  rm(logger_settings)
}

RENV_ESTABLISHED_SHINY <- TRUE
log_it("info", "Shiny environment established.", "shiny")
