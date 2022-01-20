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
# - here all are from CRAN, ChemmineR and rcdk are set in env_R depending on the
# set value of USE_RDKIT
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
sources <- list.files('src', pattern = ".R$", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source))

# _Set up logger ---------------------------------------------------------------
layout <- layout_glue_generator(format = paste("{crayon::bold(colorize_by_log_level(level, levelr))}", 
                                               "[{crayon::italic(format(time, \"%Y-%m-%d %H:%M:%OS3\"))}]", 
                                               "in {fn}(): {grayscale_by_log_level(msg, levelr)}")) 
log_threshold(LOG_THRESHOLD)
log_layout(layout)
log_formatter(formatter_glue)

# _Build database if it doesn't exist ------------------------------------------
if (!DB_BUILT) build_db()
if (INIT_CONNECT) {
  manage_connection()
  db_map <- er_map()
  db_dict <- list.files(pattern = "dictionary", full.names = TRUE)
  if (length(db_dict) == 1) {
    db_dict <- read_json(db_dict) %>%
      lapply(bind_rows)
  } else {
    db_dict <- data_dictionary()
  }
}

# _Plumber set up --------------------------------------------------------------
if (ACTIVATE_API) {
  log_it("trace", "Activating plumber API...")
  if (!"plumber" %in% installed.packages()) install.packages("plumber")
  source(file.path("plumber", "api_control.R"))
  plumber_service <- callr::r_bg(
    function() {
      source(file.path("plumber", "env_plumb.R"))
      api_start(
        on_host = PLUMBER_HOST,
        on_port = PLUMBER_PORT
      )
    }
  )
  plumber_url <- sprintf("http://%s:%s", PLUMBER_HOST, PLUMBER_PORT)
  if (plumber_service$is_alive()) {
    log_it("info", glue::glue("Running plumber API at {plumber_url}"))
    log_it("info", glue::glue("View plumber docs at {plumber_url}/__docs__/ or by calling `api_open_doc(plumber_url)`"))
  } else {
    log_it("warn", "There was a problem launching the plumber API.")
  }
} else {
  log_it("trace", "Plumber API not requested according to ACTIVATE_API setting in env_R.R settings.")
}

# _RDKit set up ----------------------------------------------------------------
if (USE_RDKIT) {
  log_it("info", "Using RDKit for this session. Setting up...")
  if (!"reticulate" %in% installed.packages()) install.packages("reticulate")
  source(file.path("rdkit", "env_py.R"))
  source(file.path("rdkit", "py_setup.R"))
  if (!exists("PYENV_NAME")) PYENV_NAME <- "nist_hrms_db"
  if (!exists("PYENV_REF")) PYENV_REF <- "rdk"
  setup_rdkit(PYENV_NAME, PYENV_REF)
} else {
  log_it("info", "Using ChemmineR for this session.")
}

# _Clean up --------------------------------------------------------------------
rm(sources, exclusions, fragments, exactmasschart)

