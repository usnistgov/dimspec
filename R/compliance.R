# # Start with clean environment -------------------------------------------------
# # _Remove environment variables ------------------------------------------------
# rm(list = ls())
# # _Unload non-core packages ----------------------------------------------------
# #     Note this does not reliably remove namespaces.
# unload_packs <- unique(c(
#   if (exists("DEPENDS_ON")) DEPENDS_ON else NULL,
#   names(sessionInfo()$loadedOnly),
#   names(sessionInfo()$otherPkgs)
# ))
# invisible(
#   lapply(unload_packs,
#          function(x) {
#            this_pack <- paste('package', x, sep = ":")
#            if (this_pack %in% search()) {
#              detach(
#                name = this_pack,
#                character.only = TRUE,
#                unload = TRUE,
#                force = TRUE
#              )
#            }
#          })
# )
# rm(unload_packs)

# _Set operational env variables -----------------------------------------------
installed_packages <- installed.packages()
if (!"here" %in% installed_packages) install.packages("here")
if (!exists("RENV_ESTABLISHED") || !RENV_ESTABLISHED) source(here::here("config", "env_R.R"))

# Ensure compliant environment -------------------------------------------------
# _Verify required directory presence ------------------------------------------
if (!dir.exists(here::here("input"))) {dir.create(here::here("input"))}
if (!dir.exists(here::here("output"))) {dir.create(here::here("output"))}
if (!dir.exists(here::here("images"))) {dir.create(here::here("images"))}
if (!dir.exists(here::here("output", "gather"))) {dir.create(here::here("output", "gather"))}
if (!dir.exists(here::here("output", "aggregate"))) {dir.create(here::here("output", "aggregate"))}
if (!dir.exists(here::here("output", "extract"))) {dir.create(here::here("output", "extract"))}
if (!dir.exists(here::here("output", "example"))) {dir.create(here::here("output", "example"))}

# _Load required packages ------------------------------------------------------
# - here all are from CRAN, ChemmineR and rcdk are set in env_R depending on the
# set value of USE_RDKIT
packs       <- DEPENDS_ON
packs_TRUE  <- which(packs %in% installed_packages)
if (length(packs_TRUE) == 0) {
  packs_FALSE <- packs
} else {
  packs_FALSE <- packs[-packs_TRUE]
}
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   quiet        = FALSE,
                   dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)
rm(packs, packs_TRUE, packs_FALSE)

# _Source required files -------------------------------------------------------
# - If this changes to a formal package we'll want to redefine these
exclusions <- paste0(EXCLUSIONS, collapse = "|")
sources <- list.files(path = here::here("R"), pattern = ".R$", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source, keep.source = FALSE))

# _Set up logger ---------------------------------------------------------------
if (LOGGING_ON) {
  source(here::here("config", "env_logger.R"))
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
  db_dict <- list.files(path = here::here(), pattern = "dictionary", full.names = TRUE)
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
      db_dict <- try(lapply(read_json(selected_dictionary), bind_rows))
    }
    if (inherits(db_dict, "try-error")) {
      if (LOGGING_ON) log_it("error", 'There was an error building the data dictionary. With the database connected, try again with "data_dictionary()".')
    } else {
      if (LOGGING_ON) log_it("success", 'A data dictionary is available as object "db_dict".')
    }
  }
}

# _RDKit set up ----------------------------------------------------------------
if (INFORMATICS) {
  if (USE_RDKIT) {
    log_it("info", "Using RDKit for this session. Setting up...", "rdk")
    if (!"reticulate" %in% installed.packages()) install.packages("reticulate")
    require(reticulate)
    source(here::here("inst", "rdkit", "env_py.R"))
    source(here::here("inst", "rdkit", "py_setup.R"))
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


# _Plumber set up --------------------------------------------------------------
if (USE_API) {
  if (!exists("RENV_ESTABLISHED_API") || !RENV_ESTABLISHED_API) {
    log_it("info", "Resolving plumber API environment...", "api")
    source(here::here("inst", "plumber", "env_plumb.R"))
  }
  PLUMBER_URL <- api_reload(
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
# _Clean up --------------------------------------------------------------------
rm(sources, exclusions, installed_packages)
RENV_ESTABLISHED_COMPLIANCE <- TRUE

cat("\n-------------------------------------\n")
cat("\nWelcome to the Database Infrastructure for Mass Spectrometry (DIMSpec) Project.\n")
cat("\n  ___ ___ __  __ ___              \n |   \\_ _|  \\/  / __|_ __  ___ __ \n | |) | || |\\/| \\__ \\ '_ \\/ -_) _|\n |___/___|_|  |_|___/ .__/\\___\\__|\n                    |_|           \n")
message("\nThis product of the NIST Material Measurement Laboratory's Chemical Sciences Division is provided by NIST as a public service.\n\nYou may use, copy, and distribute copies of the software in any medium, provided that you keep intact this entire notice. You may improve, modify, and create derivative works of the software or any portion of the software, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the software and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the software.\n\nNIST-developed software is expressly provided 'AS IS'. NIST MAKES NO WARRANTY OF ANY KIND, EXPRESS, IMPLIED, IN FACT, OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF, INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR USEFULNESS OF THE SOFTWARE.\n\nYou are solely responsible for determining the appropriateness of using and distributing the software and you assume all risks associated with its use, including but not limited to the risks and costs of program errors, compliance with applicable laws, damage to or loss of data, programs or equipment, and the unavailability or interruption of operation. This software is not intended to be used in any situation where a failure could cause risk of injury or damage to property. The software developed by NIST employees is not subject to copyright protection within the United States.")
cat("\n-------------------------------------\n")

cat("\nTips for this session:\n")
if (exists("fn_guide")) {
  message('- Help documentation for functions is available from the console using `fn_help(X)` where X is the name (quoted or unquoted) of a function in this project, or open the indexed HTML function documentation using `fn_guide()`.')
}
if (INIT_CONNECT && dbIsValid(eval(sym(DB_CONN_NAME)))) {
  message(sprintf('- You are connected to "%s" using object "%s".', DB_NAME, DB_CONN_NAME))
  if (!"try-error" %in% class(db_map)) {
    message('- A database map is available as object "db_map".')
  }
  if (!"try-error" %in% class(db_dict)) {
    message('- A data dictionary is available as object "db_dict".')
  }
}
if (exists("user_guide")) {
  message('- View the full DIMSpec User Guide with `user_guide()`.')
}
if (INFORMATICS && USE_RDKIT) {
  message(sprintf('- Chemometrics were requested through RDKit using the "%s" environment as session object "%s". You may verify that it is running using `rdkit_active()`.', PYENV_NAME, PYENV_REF))
}
if (USE_API) {
  message('- The API service was requested and may still be launching in a background process. Check it using `api_endpoint("_ping")` or view the interactive documentation with `api_open_doc()`.')
}
if (USE_SHINY && exists("SHINY_APPS") && length(SHINY_APPS) > 0) {
  message(sprintf(
    '- Launch any of %s available shiny applications using "start_app(X)" where X is the name of an app. Currently available options include %s. Use files in the "app_template" directory to begin building new apps with a similar style.',
    length(SHINY_APPS),
    format_list_of_names(names(SHINY_APPS))
  ))
}
cat("\n-------------------------------------\n")
