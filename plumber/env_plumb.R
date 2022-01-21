# Set up logger ----------------------------------------------------------------
LOGGING_ON    <- TRUE
LOG_THRESHOLD <- "trace"
LOGGING_NS    <- "api"
LOGGING_TO    <- "file"
LOGGING_FILE  <- file.path("logs", "log_plumber.R")

if (LOGGING_ON) {
  log_threshold(LOG_THRESHOLD, namespace = LOGGING_NS)
  log_layout(layout_simple, namespace = LOGGING_NS)
  log_formatter(formatter_glue, namespace = LOGGING_NS)
  log_warnings()
  log_errors()
  if (!LOGGING_TO %in% c("both", "console", "file")) {
    stop('Value of LOGGING_TO in env_plumb.R must be one of "both", "console", or "file".')
  } else if (LOGGING_TO %in% c("both", "console")) {
    if (!interactive()) LOGGING_TO <- "file"
  }
  if (LOGGING_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOGGING_FILE)) file.create(LOGGING_FILE)
  }
  switch(LOGGING_TO,
         "console" = log_appender(appender_console, namespace = LOGGING_NS),
         "file"    = log_appender(appender_file(LOGGING_FILE, max_lines = 10000, max_files = 10L), namespace = LOGGING_NS),
         "both"    = log_appender(appender_tee(LOGGING_FILE, max_lines = 10000, max_files = 10L), namespace = LOGGING_NS)
  )
}

# Set up environment -----------------------------------------------------------
source(file.path("config", "env_glob.txt"))
source(file.path("config", "env_R.R"))
source(file.path("src", "app_functions.R"))
source(file.path("src", "db_comm.R"))
source(file.path("src", "api_generator.R"))
source(file.path("plumber", "api_control.R"))
packs <- DEPENDS_ON
lapply(packs, library, character.only = TRUE, quietly = TRUE)
library(plumber)
manage_connection()
