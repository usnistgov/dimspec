dev <- TRUE
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
  try <- 1
  alive <- try(api_endpoint(PLUMBER_URL, path = "active"))
  if (inherits(alive, "try-error")) {
    # This may execute faster than the plumber service is able to spin up, so pause for a bit and try it again.
    # TODO The /correct/ way to treat this would be to poll the service object (no reference here) and wait until it reports that it is alive.
    stop("API service does not appear to be available. Please run `api_reload()` and try again.")
    # Sys.sleep(1)
    # try <- try + 1
    # alive <- try(api_endpoint(PLUMBER_URL, path = "active"))
    # if (try == 5) {
    #   warning("API endpoint was not active after 5 tries. Restarting API service.")
    #   api_reload()
    # }
    # if (try == 10) {
    #   stop("API endpoint not active after 10 tries. Aborting app spin up.")
    # }
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
        setNames(x$id, HTML(glue::glue("{x$acronym} ({x$name})")))
      }
    }) %>%
    purrr::flatten()
)

lapply(list.files(here::here("apps", app_dir, "modals"), pattern = ".R", full.names = TRUE),
       source)

jscode <- HTML("
$('body').on('shown.bs.modal', (x) =>
  $(x.target).find('input[type=\"number\"]:first').focus())
               ")
