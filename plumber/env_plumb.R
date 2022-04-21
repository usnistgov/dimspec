# Set up environment -----------------------------------------------------------
source(file.path("config", "env_glob.txt"))
source(file.path("config", "env_R.R"))
source(file.path("R", "app_functions.R"))
source(file.path("R", "db_comm.R"))
source(file.path("R", "api_generator.R"))
source(file.path("plumber", "api_control.R"))
packs <- DEPENDS_ON
lapply(packs, library, character.only = TRUE, quietly = TRUE)
require(logger)

# Set up logger ----------------------------------------------------------------
# Set this as the name of the "LOGGING" element referring to the API settings
# from env_logger.R e.g. "API" to refer to LOGGING$API (the default)
log_ns <- "API"
LOG_DIRECTORY <- ifelse(exists("LOG_DIRECTORY"), LOG_DIRECTORY, "logs")
if (!exists("LOGGING")) {
  LOGGING <- list(
    list(
      log = TRUE,
      ns = tolower(log_ns),
      to = "file",
      file = file.path(LOG_DIRECTORY, "log_api.txt"),
      threshold = "info"),
    API_DB = list(log = TRUE,
                  ns = "api_db",
                  to = "file",
                  file = file.path(LOG_DIRECTORY, "log_api_db_call.txt"),
                  threshold = "info")
  )
  names(LOGGING)[1] <- log_ns
}
for (ns in names(LOGGING)) {
  if (ns %in% names(LOGGING) && "log" %in% names(LOGGING[[ns]]) && LOGGING[[ns]]$log) {
  if (!exists(sprintf("log_%s", LOGGING[[ns]]$threshold))) {
    stop(glue::glue('Value of logging threshold must be one of the logger levels ["trace", "debug", "info", "success", "warn", "error", or "fatal"] rather than "{LOGGING[[ns]]$threshold}".'))
  }
  LOGGING[[ns]]$threshold  <- toupper(LOGGING[[ns]]$threshold)
  if (!LOGGING[[ns]]$to %in% c("both", "console", "file")) {
    stop(glue::glue('Value of logging "to" must be one of "both", "console", or "file" rather than {LOGGING[[ns]]$to}.'))
  }
  if (LOGGING[[ns]]$to %in% c("both", "console")) {
    if (!interactive()) LOGGING[[ns]]$to <- "file"
  }
  if (LOGGING[[ns]]$to %in% c("file", "both")) {
    if (!dir.exists(LOG_DIRECTORY)) dir.create(LOG_DIRECTORY)
    if (!file.exists(LOGGING[[ns]]$file)) file.create(LOGGING[[ns]]$file)
  }
  log_appender(
    switch(LOGGING[[ns]]$to,
           "console" = appender_console,
           "file"    = appender_file(LOGGING[[ns]]$file, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOGGING[[ns]]$file, max_lines = 10000, max_files = 10L)
    ),
    namespace = LOGGING[[ns]]$ns
  )
  layout <- layout_glue_generator(
    format = paste('[{format(time, "%Y-%m-%d %H:%M:%OS3")}] <{ns}> {level}',
                   "in fn {fn}(): {msg}")
  )
  log_threshold(level = toupper(LOGGING[[ns]]$threshold), namespace = LOGGING[[ns]]$ns)
  log_layout(layout = layout, namespace = LOGGING[[ns]]$ns)
}
  
  # TODO automatic logging of warnings and errors does not appear to be
  # supported in a background process called from callr::r_bg. This throws an
  # error in the background process that is effectively inaccessible: "ERROR
  # [2022-01-26 08:52:48] should not be called with handlers on the stack"
  # Logging should be handled discretely to write to the error log.
  #
  # log_warnings()
  #
  # log_errors()
}

# Initialize connection --------------------------------------------------------
library(plumber)
manage_connection(log_ns = "API_DB")
