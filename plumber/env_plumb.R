# Set up environment -----------------------------------------------------------
source(file.path("config", "env_glob.txt"))
source(file.path("config", "env_R.R"))
source(file.path("src", "app_functions.R"))
source(file.path("src", "db_comm.R"))
source(file.path("src", "api_generator.R"))
source(file.path("plumber", "api_control.R"))
packs <- DEPENDS_ON
lapply(packs, library, character.only = TRUE, quietly = TRUE)

# Set up logger ----------------------------------------------------------------
if (LOGGING_API) {
  # Set to true to override settings in env_logger.R or set as FALSE to accept.
  OVERRIDE_ENV  <- TRUE
  if (!OVERRIDE_ENV) {
    source(file.path("config", "env_logger.R"))
  } else {
    LOGGING_API  <- TRUE
    LOG_THR_API  <- "trace"
    LOGGING_NS   <- "api"
    LOG_API_TO   <- ifelse(interactive(), "both", "file")
    LOG_FILE_API <- file.path("logs", "log_api.txt")

    if (!exists(sprintf("log_%s", LOG_THR_API))) {
      stop('Value of LOG_THR_API must be one of the logger levels ["trace", "debug", "info", "success", "warn", "error", or "fatal"]')
    }
    LOG_THR_API  <- toupper(LOG_THR_API)

    if (!LOG_API_TO %in% c("both", "console", "file")) {
      stop('Value of LOG_API_TO in env_R.R must be one of "both", "console", or "file".')
    }
    if (LOG_API_TO %in% c("both", "console")) {
      if (!interactive()) LOG_API_TO <- "file"
    }
    if (LOG_API_TO %in% c("file", "both")) {
      if (!dir.exists(LOG_DIRECTORY)) dir.create(LOG_DIRECTORY)
      if (!file.exists(LOG_FILE_API)) file.create(LOG_FILE_API)
    }
    log_appender(
      switch(LOG_API_TO,
             "console" = appender_console,
             "file"    = appender_file(LOG_FILE_API, max_lines = 10000, max_files = 10L),
             "both"    = appender_tee(LOG_FILE_API, max_lines = 10000, max_files = 10L)
      ),
      namespace = LOGGING_NS
    )
    layout <- layout_glue_generator(
      format = paste('[{format(time, "%Y-%m-%d %H:%M:%OS3")}] <{ns}> {level}',
                     "in {fn}(): {msg}")
    )
    log_threshold(level = LOG_THR_API, namespace = LOGGING_NS)
    log_layout(layout = layout, namespace = LOGGING_NS)
    
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
}

# Initialize connection --------------------------------------------------------
library(plumber)
manage_connection()
