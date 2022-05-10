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
sources <- list.files("R", pattern = ".R$", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source, keep.source = FALSE))

# _Set up logger ---------------------------------------------------------------
if (LOGGING_ON) {
  source(file.path("config", "env_logger.R"))
  update_logger_settings()
  log_it("info", "Setting up logger for use with this session...")
} else {
  cat('\nLogging not requested according to LOGGING_ON in "env_glob.txt" settings.\n')
}

# _Build database if it doesn't exist ------------------------------------------
if (!DB_BUILT) build_db()
if (INIT_CONNECT) {
  status <- try(manage_connection())
  if ("try-error" %in% class(status)) {
    if (LOGGING_ON) log_it("error", "Could not establish a connection to the database.")
  } else {
    if (LOGGING_ON) log_it("success", sprintf('Interactive database connection is live as object "%s".', DB_CONN_NAME))
  }
  db_map <- try(er_map())
  if ("try-error" %in% class(db_map)) {
    if (LOGGING_ON) log_it("error", 'There was an error loading the database map using "er_map()".')
  } else {
    if (LOGGING_ON) log_it("success", 'A database map is available as object "db_map".')
  }
  db_dict <- list.files(pattern = "dictionary", full.names = TRUE)
  if (length(db_dict) == 1) {
    if (LOGGING_ON) log_it("info", sprintf('Dictionary file located at %s', db_dict))
    db_dict <- try(lapply(read_json(db_dict), bind_rows))
    if ("try-error" %in% class(db_dict)) {
      if (LOGGING_ON) log_it("error", 'There was an error reading in the data dictionary.')
    } else {
      if (LOGGING_ON) log_it("success", 'A data dictionary is available as object "db_dict".')
    }
  } else {
    if (length(db_dict) == 0) {
      if (LOGGING_ON) log_it("info", "No database dictionary file located. Building...")
    } else {
      if (LOGGING_ON) log_it("info", "Multiple dictionary files located.")
      if (interactive()) {
        selected_dictionary <- select.list(
          choices = c(db_dict, "Build New")
        )
        if (selected_dictionary == "Build New") selected_dictionary <- NULL
      } else {
        selected_dictionary <- NULL
      }
    }
    if (is.null(selected_dictionary)) {
      db_dict <- try(data_dictionary())
    } else {
      db_dict <- try(lapply(read_json(db_dict), bind_rows))
    }
    if (inherits(db_dict, "try-error")) {
      if (LOGGING_ON) log_it("error", 'There was an error building the data dictionary with "data_dictionary()".')
    } else {
      if (LOGGING_ON) log_it("success", 'A data dictionary is available as object "db_dict".')
    }
  }
}

# _Plumber set up --------------------------------------------------------------
if (USE_API) {
  log_it("info", "Activating plumber API...", "api")
  if (!"plumber" %in% installed.packages()) install.packages("plumber")
  source(file.path("plumber", "api_control.R"))
  api_reload(
    pr = "plumber_service",
    background = TRUE,
    on_host = PLUMBER_HOST,
    on_port = PLUMBER_PORT
  )
  if (plumber_service$is_alive()) {
    log_it("success", glue::glue("Running plumber API at {PLUMBER_URL}"))
    log_it("info", glue::glue("View plumber docs at {PLUMBER_URL}/__docs__/ or by calling `api_open_doc(PLUMBER_URL)`"))
  } else {
    log_it("warn", "There was a problem launching the plumber API.", "api")
  }
} else {
  log_it("info", "Plumber API not requested according to USE_API setting in env_R.R settings.", "api")
}

# _RDKit set up ----------------------------------------------------------------
if (INFORMATICS) {
  if (USE_RDKIT) {
    log_it("info", "Using RDKit for this session. Setting up...", "rdk")
    if (!"reticulate" %in% installed.packages()) install.packages("reticulate")
    require(reticulate)
    source(file.path("rdkit", "env_py.R"))
    source(file.path("rdkit", "py_setup.R"))
    if (!exists("PYENV_NAME")) PYENV_NAME <- "nist_hrms_db"
    if (!exists("PYENV_LIBRARIES")) PYENV_LIBRARIES <- c("rdkit=2021.09.4", "r-reticulate=1.24")
    if (!exists("PYENV_REF")) PYENV_REF <- "rdk"
    if (!exists("CONDA_PATH")) CONDA_PATH <- "rdk"
    setup_rdkit(
      env_name           = PYENV_NAME,
      required_libraries = PYENV_LIBRARIES,
      env_ref            = PYENV_REF,
      log_ns             = "rdk",
      conda_path         = CONDA_PATH
    )
  } else {
    log_it("info", "Using ChemmineR for this session.", "rdk")
  }
}

# _Clean up --------------------------------------------------------------------
rm(sources, exclusions)

