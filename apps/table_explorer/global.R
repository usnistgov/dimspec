if (exists("LOGGING") && LOGGING_ON) {
  log_ns <- "APP_TABLE_VIEWER"
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
  log_it("info", "Starting app: table_viewer", "shiny")
  log_it("info", "Starting app", tolower(log_ns))
}

# Session variables
if (exists("PLUMBER_URL") && api_endpoint(PLUMBER_URL, path = "active")) {
  table_list <- api_endpoint(PLUMBER_URL, path = "list_tables")
} else {
  stop("This app requires an active API connection.")
}
