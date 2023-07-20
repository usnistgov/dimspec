# Set up environment -----------------------------------------------------------
PLUMBER_OBJ_NAME <- "plumber_service"
rdkit_override <- TRUE
if (!exists("DB_TITLE")) source(here::here("config", "env_glob.txt"))
if (!exists("RENV_ESTABLISHED") || !RENV_ESTABLISHED) {
  source(here::here("config", "env_R.R"))
  r_scripts <- c(
    here::here("R", "app_functions.R"),
    here::here("R", "db_comm.R"),
    here::here("R", "api_generator.R"),
    here::here("R", "tidy_spectra.R"),
    unname(
      unlist(
        sapply(c("base", "misc", "qualitycontrol", "spectral_analysis"),
           function(x) {
             list.files(path = here::here("R", x), pattern = "\\.R$", full.names = TRUE)
           })
      )
    )
  )
  invisible(lapply(r_scripts, source))
}
if (!"plumber" %in% installed.packages()) install.packages("plumber")
if (!exists("RENV_ESTABLISHED_API") || !RENV_ESTABLISHED_API) {
  source(here::here("inst", "plumber", "api_control.R"))
}

lapply(c(DEPENDS_ON, "plumber"), library, character.only = TRUE, quietly = TRUE)

# Set up logger ----------------------------------------------------------------
# Set this as the name of the "LOGGING" element referring to the API settings
# from env_logger.R e.g. "API" to refer to LOGGING$API (the default)
LOGGING_ON <- TRUE
if (!exists("RENV_ESTABLISHED_LOGGER") || !RENV_ESTABLISHED_LOGGER) source(here::here("config", "env_logger.R"))
log_ns <- "API"
logger_settings <- setNames(
  list(
    list(
      log = TRUE,
      ns = tolower(log_ns),
      to = "file",
      file = file.path(LOG_DIRECTORY, "log_{tolower(log_ns)}.txt"),
      threshold = "info"
    )
  ),
  log_ns
)
if (!exists("LOGGING")) {
  LOGGING <- logger_settings
} else if (!log_ns %in% names(LOGGING)) {
  LOGGING <- append(
    LOGGING,
    logger_setings
  )
}
rm(logger_settings)
update_logger_settings(log_all_warnings = FALSE, log_all_errors = FALSE)

# Enable RDKit -----------------------------------------------------------------
# RDKIT is required for certain endpoints.
if (rdkit_override || INFORMATICS && USE_RDKIT) {
  if (!exists("RENV_ESTABLISHED_RDKIT") || !RENV_ESTABLISHED_RDKIT) source(here::here("inst", "rdkit", "env_py.R"))
  rdkit_active(make_if_not = TRUE, log_ns = "api")
}

# Initialize connection --------------------------------------------------------
manage_connection(db = DB_NAME, conn_name = "con", log_ns = "api")
RENV_ESTABLISHED_API <- TRUE
