# Start with clean environment -------------------------------------------------
# _Remove environment variables ------------------------------------------------
rm(list = ls())
# _Unload non-core packages ----------------------------------------------------
unload_packs <- c(
  names(sessionInfo()$loadedOnly),
  names(sessionInfo()$otherPkgs)
)
invisible(
  lapply(unload_packs, function(pkgs)
         function(pkgs) {
    detach(
      paste0('package:', pkgs),
      character.only = T,
      unload = T,
      force = T
    )
  })
)

# Ensure compliant environment -------------------------------------------------
# _Verify required directory presence ------------------------------------------
if (!dir.exists("input")) {dir.create("input")}
if (!dir.exists("output")) {dir.create("output")}
if (!dir.exists("output/gather")) {dir.create("output/gather")}
if (!dir.exists("output/aggregate")) {dir.create("output/aggregate")}
if (!dir.exists("output/extract")) {dir.create("output/extract")}
if (!dir.exists("output/example")) {dir.create("output/example")}

# _Set operational env variables -----------------------------------------------
source(file.path("config", "env_glob.txt"))
source(file.path("config", "env_R.R"))

# _Load required packages ------------------------------------------------------
# - here all are from CRAN 
packs       <- DEPENDS_ON
packs_TRUE  <- which(packs %in% installed.packages())
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   quiet        = TRUE,
                   dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)
rm(packs, unload_packs, packs_TRUE, packs_FALSE)

# _Source required files -------------------------------------------------------
# - If this changes to a formal package we'll want to redefine these
exclusions <- paste0(EXCLUSIONS, collapse = "|")
sources <- list.files('src', pattern = ".R", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source))

# _Set up logger ---------------------------------------------------------------
layout <- layout_glue_generator(format = paste("{crayon::bold(colorize_by_log_level(level, levelr))}", 
                                               "[{crayon::italic(format(time, \"%Y-%m-%d %H:%M:%OS3\"))}]", 
                                               "- {fn} {grayscale_by_log_level(msg, levelr)}")) 
log_threshold(LOG_THRESHOLD)
log_layout(layout)
log_formatter(formatter_glue)
log_ns <- list(db       = "db_transaction",
               build    = "db_build",
               internal = "internal_function",
               app      = "app_function")

# _Clean up --------------------------------------------------------------------
rm(sources, exclusions, fragments, exactmasschart)
