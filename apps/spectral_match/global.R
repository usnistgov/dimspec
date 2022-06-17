dev <- TRUE
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
APP_TITLE <- "NIST PFAS Database Spectra Match"
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
if (exists("PLUMBER_URL") && api_endpoint(PLUMBER_URL, path = "active")) {
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
        setNames(x$id, HTML(glue::glue("{x$acronym} ({x$name})")))
      }
    }) %>%
    purrr::flatten()
)


lapply(list.files("modals", pattern = ".R", full.names = TRUE),
       source)

jscode <- HTML("
$('body').on('shown.bs.modal', (x) =>
  $(x.target).find('input[type=\"number\"]:first').focus())
               ")