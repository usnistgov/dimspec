# Set up environment -----------------------------------------------------------
if (!exists("DB_TITLE")) source(here::here("config", "env_glob.txt"))
USE_API      <- TRUE
USE_SHINY    <- TRUE
INFORMATICS  <- TRUE
USE_RDKIT    <- TRUE
DB_CONN_NAME <- "con"
if (!exists("RENV_ESTABLISHED") || !RENV_ESTABLISHED) source(here::here("config", "env_R.R"))
if (!exists("RENV_ESTABLISHED_COMPLIANCE") || !RENV_ESTABLISHED_COMPLIANCE) source(here::here("R", "compliance.R"))
packs <- c("shiny",
           "shinyalert",
           "shinydashboard",
           "shinyjs",
           "shinydisconnect",
           "shinythemes",
           "DT",
           "httr",
           "readr")
lapply(packs, library, character.only = TRUE, quietly = TRUE)
rm(packs)
RENV_ESTABLISHED_SHINY <- TRUE
# 
# # Set up logger ----------------------------------------------------------------
# # Set this as the name of the "LOGGING" element referring to the API settings
# # from env_logger.R e.g. "SHINY" to refer to LOGGING$SHINY (the default)
log_ns <- "SHINY"
LOG_DIRECTORY <- here::here(ifelse(exists("LOG_DIRECTORY"), LOG_DIRECTORY, "logs"))
logger_settings <- list(
  list(
    log = TRUE,
    ns = tolower(log_ns),
    to = "file",
    file = file.path(LOG_DIRECTORY, "log_shiny.txt"),
    threshold = "info")
) %>%
setNames(log_ns)
if (!exists("LOGGING")) {
  LOGGING <- logger_settings
} else if (!log_ns %in% names(LOGGING)) {
  LOGGING <- append(
    LOGGING,
    logger_settings
  )
}
require(logger)
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
}
log_it("info", "Shiny environment established.", "shiny")
