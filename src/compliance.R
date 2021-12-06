# Start with clean environment -------------------------------------------------
# _Remove environment variables ------------------------------------------------
rm(list = ls())
# _Unload non-core packages ----------------------------------------------------
#     Note this does not reliably remove namespaces.
unload_packs <- unique(c(
  if (exists("DEPENDS_ON")) DEPENDS_ON else NULL,
  names(sessionInfo()$loadedOnly),
  names(sessionInfo()$otherPkgs)
))
invisible(
  lapply(unload_packs,
         function(x) {
           this_pack <- paste('package', x, sep = ":")
           if (this_pack %in% search()) {
             detach(
               name = this_pack,
               character.only = TRUE,
               unload = TRUE,
               force = TRUE
             )
           }
         })
)

# Ensure compliant environment -------------------------------------------------
# _Verify required directory presence ------------------------------------------
if (!dir.exists("input")) {dir.create("input")}
if (!dir.exists("output")) {dir.create("output")}
if (!dir.exists(file.path("output", "gather"))) {dir.create(file.path("output", "gather"))}
if (!dir.exists(file.path("output", "aggregate"))) {dir.create(file.path("output", "aggregate"))}
if (!dir.exists(file.path("output", "extract"))) {dir.create(file.path("output", "extract"))}
if (!dir.exists(file.path("output", "example"))) {dir.create(file.path("output", "example"))}

# _Set operational env variables -----------------------------------------------
source(file.path("config", "env_glob.txt"))
source(file.path("config", "env_R.R"))

# _Load required packages ------------------------------------------------------
installed_packages <- installed.packages()
if (!"renv" %in% installed_packages) {
  if (!"remotes" %in% installed_packages) {
    install.packages("remotes")
  }
  remotes::install_github("rstudio/renv")
}
renv::activate()
renv::restore()

packs       <- DEPENDS_ON
packs_TRUE  <- which(packs %in% installed_packages)
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  renv::install(packages = packs_FALSE)
}
suppressPackageStartupMessages(
  lapply(packs, library, character.only = TRUE, quietly = TRUE)
)
rm(packs, unload_packs, packs_TRUE, packs_FALSE)

# _Source required files -------------------------------------------------------
# - If this changes to a formal package we'll want to redefine these
exclusions <- paste0(EXCLUSIONS, collapse = "|")
sources <- list.files(path = 'src', pattern = ".R$", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source))

# _Set up logger ---------------------------------------------------------------
layout <- logger::layout_glue_generator(
  format = paste("{crayon::bold(colorize_by_log_level(level, levelr))}",
                 "[{crayon::italic(format(time, \"%Y-%m-%d %H:%M:%OS3\"))}]",
                 "in {fn}(): {grayscale_by_log_level(msg, levelr)}")
)
log_threshold(LOG_THRESHOLD)
log_layout(layout)
log_formatter(formatter_glue)

# _Build database if it doesn't exist ------------------------------------------
if (!DB_BUILT) build_db()
if (INIT_CONNECT) {
  manage_connection()
  if (INCLUDE_MAP) db_map <- er_map()
  if (INCLUDE_DICT) {
    db_dict <- list.files(pattern = "dictionary", full.names = TRUE)
    if (length(db_dict) == 1) {
      db_dict <- read_json(db_dict) %>%
        lapply(bind_rows)
    } else {
      db_dict <- data_dictionary()
    }
  }
}

# _Clean up --------------------------------------------------------------------
rm(sources, exclusions, fragments, exactmasschart)
