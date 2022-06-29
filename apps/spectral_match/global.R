dev <- TRUE
advanced_use <- FALSE
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
APP_TITLE <- "NIST PFAS Database Spectra Match"
app_dir <- "spectral_match"
if (!exists("RENV_ESTABLISHED_SHINY") || !RENV_ESTABLISHED_SHINY) source(here::here("apps", "env_shiny.R"))
source("app_functions.R")
if (exists("LOGGING") && LOGGING_ON) {
  log_ns <- "APP_SPECTRAL_MATCH"
  if (!log_ns %in% names(LOGGING)) {
    LOG_DIRECTORY <- here::here(ifelse(exists("LOG_DIRECTORY"), LOG_DIRECTORY, "logs"))
    logger_settings <- list(
      list(
        log = TRUE,
        ns = tolower(log_ns),
        to = "file",
        file = file.path(LOG_DIRECTORY, sprintf("log_%s.txt", tolower(log_ns))),
        threshold = "info")
    ) %>%
      setNames(log_ns)
    LOGGING <- append(LOGGING, logger_settings)
    update_logger_settings()
  }
  log_it("info", "Starting app: app_spectral_match", "shiny")
  log_it("info", "Starting app", tolower(log_ns))
}

# Session variables
if (exists("PLUMBER_URL")) {
  if (!dplyr::between(api_endpoint(path = "_ping", raw_result = TRUE)$status, 200, 299)) {
    stop("API service does not appear to be available. Please run `api_reload()` and try again.")
  }
} else {
  stop("This app requires an active API connection.")
}

app_settings <- list(
  experiment_types = api_endpoint(path = "table_search",
                                  query = list(table_name = "norm_ms_n_types"),
                                  return_format = "list") %>%
    lapply(function(x) {
      if (x$name == "none") {
        NULL 
      } else {
        setNames(x$acronym, HTML(glue::glue("{x$acronym} ({x$name})")))
      }
    }) %>%
    purrr::flatten(),
  data_input_import_file_types = c(".mzML"),
  data_input_import_search_settings_types = c(".csv", ".xls", ".xlsx"),
  data_input_relative_error = list(value = 5, min = 0.1, max = 50, step = 0.1),
  data_input_minimum_error = list(value = 0.002, min = 0.0001, max = 0.5, step = 0.0001),
  data_input_isolation_width = list(value = 0.7, min = 0.1, max = 100, step = 0.1),
  data_input_isolation_width_warn_threshold = 4,
  data_input_search_zoom = list(value = c(1, 4), min = 0, max = 10, step = 0.1, ticks = FALSE),
  data_input_correlation = list(value = 0.5, min = 0, max = 1, step = 0.1, ticks = FALSE),
  data_input_ph = list(value = 10, min = 0, max = 100, step = 1, ticks = FALSE),
  data_input_freq = list(value = 10, min = 3, max = 15, step = 1, ticks = FALSE),
  data_input_normfn = c("sum", "mean"),
  data_input_cormethod = c("pearson"),
  # data_input_max_correl = list(value = 0.5, min = 0, max = 1, step = 0.1, ticks = FALSE),
  # data_input_correl_bin = list(value = 0.1, min = 0, max = 1, step = 0.1, ticks = FALSE),
  # data_input_max_ph = list(value = 10, min = 0, max = 100, step = 1, ticks = FALSE),
  # data_input_ph_bin = list(value = 1, min = 0, max = 100, step = 1, ticks = FALSE),
  # data_input_max_freq = list(value = 10, min = 3, max = 15, step = 1, ticks = FALSE),
  # data_input_freq_bin = list(value = 1, min = 1, max = 10, step = 1, ticks = FALSE),
  # data_input_min_n_peaks = list(value = 4, min = 3, max = 15, step = 1, ticks = FALSE),
  search_compounds_bootstrap_iterations = list(value = 1e4, min = 1e2, max = 1e5, step = 1e2)
)

rdkit_available <- api_endpoint(path = "rdkit_active")

lapply(list.files(here::here("apps", app_dir, "modals"), pattern = ".R", full.names = TRUE),
       source)

jscode <- HTML("
$('body').on('shown.bs.modal', (x) =>
  $(x.target).find('input[type=\"number\"]:first').focus())
               ")

# Increase the file upload size to 20 MB
file_mbs <- 250
options(shiny.maxRequestSize = file_mbs*1024^2)