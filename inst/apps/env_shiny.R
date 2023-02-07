# Set up environment -----------------------------------------------------------
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
           "shinyvalidate",
           "DT",
           "plotly",
           "httr",
           "readr",
           "readxl",
		   "stringr")
packs_TRUE  <- which(packs %in% installed.packages())
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   quiet        = FALSE,
                   dependencies = TRUE)
}
lapply(packs, require, character.only = TRUE, quietly = TRUE)
rm(packs)
# Minimum app requirements
api_override <- ifelse(exists("USE_API"), USE_API, NA)
rdk_override <- ifelse(exists("APP_RDKIT"), APP_RDKIT, FALSE)
source(here::here("config", "env_glob.txt"))
if (!is.na(api_override)) {
  USE_API <- api_override
}
if (!API_LOCALHOST) {
  if (API_HOST == "") {
    API_HOST <- Sys.info()[["nodename"]]
  } else {
    if (!API_HOST == Sys.info()[["nodename"]]) {
      message("The provided host name for the API server does not match this machine.")
    }
  }
}
LOGGING_ON   <- TRUE
source(here::here("R", "app_functions.R"))
# Check API
if (USE_API) {
  options(
    plumber.host = ifelse(API_LOCALHOST, "127.0.0.1", "0.0.0.0"),
    plumber.port = API_PORT
  )
  source(here::here("inst", "plumber", "api_control.R"))
  if (!exists("PLUMBER_URL")) PLUMBER_URL <- sprintf("http://%s:%s", getOption("plumber.host"), getOption("plumber.port"))
  if (!API_LOCALHOST) PLUMBER_URL <- gsub("0.0.0.0", API_HOST, PLUMBER_URL)
  plumber_available <- suppressMessages(try(dplyr::between(api_endpoint(path = "_ping", max_pings = 1L, raw_result = TRUE)$status, 200, 299)))
  if (inherits(plumber_available, "try-error")) {
    PLUMBER_URL <- start_api(background = TRUE)
    if (!API_LOCALHOST) PLUMBER_URL <- gsub("0.0.0.0", API_HOST, PLUMBER_URL)
  }
  if (!dplyr::between(api_endpoint(path = "_ping", raw_result = TRUE)$status, 200, 299)) {
    stop("This app requires an active API connection.")
  }
}

# Set up rdkit if needed -------------------------------------------------------
if (!is.na(rdk_override)) {
  INFORMATICS <- rdk_override
  USE_RDKIT   <- rdk_override
}
if (USE_RDKIT) {
  if (!exists("RENV_ESTABLISHED_RDKIT") || !RENV_ESTABLISHED_RDKIT) source(here::here("inst", "rdkit", "env_py.R"))
  rdkit_active(make_if_not = TRUE)
}

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
