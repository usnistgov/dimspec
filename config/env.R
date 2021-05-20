DB_NAME        <- "nist_pfas_nta_dev"
VERSION        <- "0.0.1"
LAST_MODIFIED  <- max(file.info(list.files(recursive = TRUE))$mtime)
LAST_DB_SCHEMA <- file.info(file.path("config", "build.sql"))$mtime
LOG_THRESHOLD  <- "TRACE"

source(list.files(pattern = "compliance.R", full.names = TRUE, recursive = TRUE))

layout <- layout_glue_generator(format = paste("{crayon::bold(colorize_by_log_level(level, levelr))}", 
                                               "[{crayon::italic(format(time, \"%Y-%m-%d %H:%M:%OS3\"))}]", 
                                               "{grayscale_by_log_level(msg, levelr)}",
                                               "- {fn}")) 
log_layout(layout)
log_formatter(formatter_glue)
log_warnings()
log_ns = list(db = "db_transaction")
