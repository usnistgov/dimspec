# Universal settings
layout_console <- layout_glue_generator(
  format = paste('[{crayon::italic(format(time, "%Y-%m-%d %H:%M:%OS3"))}]', 
                 "<{crayon::bold(ns)}>",
                 "{crayon::bold(colorize_by_log_level(level, levelr))}",
                 "in {fn}(): {grayscale_by_log_level(msg, levelr)}")
)
layout_file <- layout_glue_generator(
  format = paste('[{format(time, "%Y-%m-%d %H:%M:%OS3")}] <{ns}> {level}',
                 "in {fn}(): {msg}")
)
log_formatter(formatter_glue)
if (LOGGING_WARNS) log_warnings()
if (LOGGING_ERRORS) log_errors()

# Namespace settings
# _Global ----------------------------------------------------------------------
if (LOGGING_GLOBAL) {
  if (!LOG_GLOBAL_TO %in% c("both", "console", "file")) {
    stop('Value of LOG_GLOBAL_TO in env_R.R must be one of "both", "console", or "file".')
  }
  if (LOG_GLOBAL_TO %in% c("both", "console")) {
    if (!interactive()) LOG_GLOBAL_TO <- "file"
  }
  if (LOG_GLOBAL_TO %in% c("both", "console")) {
    layout <- layout_console
  } else {
    layout <- layout_file
  }
  if (LOG_GLOBAL_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOG_FILE_GLOBAL)) file.create(LOG_FILE_GLOBAL)
  }
  log_appender(
    switch(LOG_GLOBAL_TO,
           "console" = appender_console,
           "file"    = appender_file(LOG_FILE_GLOBAL, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOG_FILE_GLOBAL, max_lines = 10000, max_files = 10L)
    ),
    namespace = "global"
  )
  log_threshold(level = LOG_THR_GLOBAL, namespace = "global")
  log_layout(layout = layout, namespace = "global")
}

# _Database operations ---------------------------------------------------------
if (LOGGING_DB) {
  if (!LOG_DB_TO %in% c("both", "console", "file")) {
    stop('Value of LOG_DB_TO in env_R.R must be one of "both", "console", or "file".')
  }
  if (LOG_DB_TO %in% c("both", "console")) {
    if (!interactive()) LOG_DB_TO <- "file"
  }
  if (LOG_DB_TO %in% c("both", "console")) {
    layout <- layout_console
  } else {
    layout <- layout_file
  }
  if (LOG_DB_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOG_FILE_DB)) file.create(LOG_FILE_DB)
  }
  log_appender(
    switch(LOG_DB_TO,
           "console" = appender_console,
           "file"    = appender_file(LOG_FILE_DB, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOG_FILE_DB, max_lines = 10000, max_files = 10L)
    ),
    namespace = "db"
  )
  log_threshold(level = LOG_THR_DB, namespace = "db")
  log_layout(layout = layout, namespace = "db")
}

# _Shiny -----------------------------------------------------------------------
if (LOGGING_SHINY) {
  if (!LOG_SHINY_TO %in% c("both", "console", "file")) {
    stop('Value of LOG_SHINY_TO in env_R.R must be one of "both", "console", or "file".')
  }
  if (LOG_SHINY_TO %in% c("both", "console")) {
    if (!interactive()) LOG_SHINY_TO <- "file"
  }
  if (LOG_SHINY_TO %in% c("both", "console")) {
    layout <- layout_console
  } else {
    layout <- layout_file
  }
  if (LOG_SHINY_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOG_FILE_SHINY)) file.create(LOG_FILE_SHINY)
  }
  log_appender(
    switch(LOG_SHINY_TO,
           "console" = appender_console,
           "file"    = appender_file(LOG_FILE_SHINY, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOG_FILE_SHINY, max_lines = 10000, max_files = 10L)
    ),
    namespace = "shiny"
  )
  log_threshold(level = LOG_THR_SHINY, namespace = "shiny")
  log_layout(layout = layout, namespace = "shiny")
}

# _Plumber API -----------------------------------------------------------------
if (LOGGING_API) {
  if (!LOG_API_TO %in% c("both", "console", "file")) {
    stop('Value of LOG_API_TO in env_R.R must be one of "both", "console", or "file".')
  }
  if (LOG_API_TO %in% c("both", "console")) {
    if (!interactive()) LOG_API_TO <- "file"
  }
  if (LOG_API_TO %in% c("both", "console")) {
    layout <- layout_console
  } else {
    layout <- layout_file
  }
  if (LOG_API_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOG_FILE_API)) file.create(LOG_FILE_API)
  }
  log_appender(
    switch(LOG_API_TO,
           "console" = appender_console,
           "file"    = appender_file(LOG_FILE_API, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOG_FILE_API, max_lines = 10000, max_files = 10L)
    ),
    namespace = "api"
  )
  log_threshold(level = LOG_THR_API, namespace = "api")
  log_layout(layout = layout, namespace = "api")
}

# _RDKit integration -----------------------------------------------------------
if (LOGGING_RDK) {
  if (!LOG_RDK_TO %in% c("both", "console", "file")) {
    stop('Value of LOG_RDK_TO in env_R.R must be one of "both", "console", or "file".')
  }
  if (LOG_RDK_TO %in% c("both", "console")) {
    if (!interactive()) LOG_RDK_TO <- "file"
  }
  if (LOG_RDK_TO %in% c("both", "console")) {
    layout <- layout_console
  } else {
    layout <- layout_file
  }
  if (LOG_RDK_TO %in% c("file", "both")) {
    if (!dir.exists("logs")) dir.create("logs")
    if (!file.exists(LOG_FILE_RDK)) file.create(LOG_FILE_RDK)
  }
  log_appender(
    switch(LOG_RDK_TO,
           "console" = appender_console,
           "file"    = appender_file(LOG_FILE_RDK, max_lines = 10000, max_files = 10L),
           "both"    = appender_tee(LOG_FILE_RDK, max_lines = 10000, max_files = 10L)
    ),
    namespace = "rdk"
  )
  log_threshold(level = LOG_THR_RDK, namespace = "rdk")
  log_layout(layout = layout, namespace = "rdk")
}
