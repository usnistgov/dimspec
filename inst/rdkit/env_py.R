# [REQUIRED]--------------------------------------------------------------------
# The name of the python environment to use. If this environment exists on the
# current system, it will be used, otherwise one will be created with this name.
PYENV_NAME <- "nist_dimspec"

# The name of the R object to use for importing the rdkit module. Every effort
# has been made to make this flexible, but for best use this should be "rdk" and
# only be changed for interactive use.
PYENV_REF <- "rdk"

# Set the python version from which to build the environment.
USE_PY_VER <- 3.9

# Set the option to install the python environment. Must be one of "conda", 
# "git", or "local", with "local" being recommended.
#  ** GIT IS NOT SUPPORTED IN THIS VERSION FOR AUTOMATIC CONFIGURATION **
INSTALL_FROM <- "local"

# [REQUIRED IF INSTALL_FROM == "local"] Set the file from which the python
# environment will be built.
INSTALL_FROM_FILE <- here::here("inst", "rdkit", "environment.yml")

# [OPTIONAL] -------------------------------------------------------------------
# [ADVANCED] Set required conda libraries to install, depending on it to define
# its dependencies. This will be used if INSTALL_FROM = "conda". Note that the
# r-reticulate python package will automatically be added to this list during
# installation.
PYENV_LIBRARIES <- c("rdkit=2021.09.4", "r-reticulate=1.24")
PYENV_MODULES <- "rdkit"
# Channel from which to install packages.
PYENV_CHANNELS <- "conda-forge"

# [ADVANCED] -------------------------------------------------------------------
# Set the path for advanced conda setups.
CONDA_PATH <- "auto"
# CONDA_PATH <- "~/miniforge3/bin/conda" # Example

# _[ENSURE R] ------------------------------------------------------------------
if (!exists("RENV_ESTABLISHED") || !RENV_ESTABLISHED) {
  source(here::here("config", "env_R.R"))
}

# _[LOGGER] --------------------------------------------------------------------
# Set this as the name of the "LOGGING" element referring to the API settings
# from env_logger.R e.g. "API" to refer to LOGGING$API (the default)
if (exists("LOGGING_ON") && LOGGING_ON) {
  if (!exists("RENV_ESTABLISHED_LOGGER") || !RENV_ESTABLISHED_LOGGER) source(here::here("config", "env_logger.R"))
  log_ns <- "RDK"
  LOG_DIRECTORY <- ifelse(exists("LOG_DIRECTORY"), LOG_DIRECTORY, here::here("logs"))
  logger_settings <- setNames(
    list(
      list(
        log = TRUE,
        ns = tolower(log_ns),
        to = "file",
        file = file.path(LOG_DIRECTORY, glue::glue("log_{tolower(log_ns)}.txt")),
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
}
# _[FINALIZE] ------------------------------------------------------------------
if (!"reticulate" %in% installed.packages()) install.packages("reticulate")
require(reticulate)
source(here::here("inst", "rdkit", "py_setup.R"))
if (!exists("rectify_null_from_env")) {
  if (!"magrittr" %in% installed.packages()) install.packages("magrittr")
  require(magrittr)
  source(here::here("R", "app_functions.R"))
}
rdkit_active(make_if_not = TRUE, log_ns = ifelse(exists("log_ns"), tolower(log_ns), NA_character_))
RENV_ESTABLISHED_RDKIT <- TRUE
