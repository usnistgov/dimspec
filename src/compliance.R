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
  
  # ---- try some stuff from icpmsflow
  # source(file.path("rdkit", "py_setup_icpmsflow.R"))
  # python_enabled <- py_setup()
  # if (!python_enabled) {
  #   stop("Unable to establish python environment.")
  # }
  # rdk <- import("rdkit")
  
  # --- try simplistic - this works
  # library(reticulate)
  # use_condaenv(PYENV_NAME)
  # rdk <- import("rdkit")
  # rdkit_active <- !"try-error" %in% class(
  #   invisible(
  #     try(
  #       rdk$Chem$inchi$MolToInchi(
  #         rdk$Chem$MolFromSmiles('C1CC1[C@H](F)C1CCC1')
  #       )
  #     )
  #   )
  # )
  # if (rdkit_active) {
  #   log_it("success", "RDKit initialized.")
  # } else {
  #   log_it("warn", "Something went wrong initializing RDKit.")
  # }
  
  # if (!exists("empty_variable")) source(file.path("src", "app_functions.R"))
  # env_name <- ifelse(empty_variable(PYENV_NAME), "rdkit", PYENV_NAME)
  # env_mods <- if (empty_variable(PYENV_MODULE)) "rdkit" else PYENV_MODULE
  # env_ref <- ifelse(empty_variable(PYENV_REF), "rdk", PYENV_REF)
  # require(reticulate)
  # log_it("trace", "Discovering python installations...")
  # py_settings <- py_discover_config(required_module = env_mods[1])
  # if (!length(py_settings$pythonhome) == 1) {
  #   log_it("warn", "Python was not discoverable on this system. Installing miniconda...")
  #   install_miniconda(force = FALSE)
  #   py_settings <- py_discover_config(required_module = env_mods[1])
  # }
  # log_it("trace", "Checking for environment name match...")
  # py_envs <- py_settings$python_versions %>%
  #   stringr::str_remove_all("python.exe") %>%
  #   stringr::str_sub(1, nchar(.) - 1) %>%
  #   basename()
  # if (env_name %in% py_envs) {
  #   log_it("trace", glue::glue('Environment "{env_name}" located.'))
  # } else {
  #   log_it("info", glue::glue('Environment "{env_name}" could not be found; creating...'))
  #   create_rdkit_conda_env(env_name)
  # }
  # use_condaenv(condaenv = env_name)
  # log_it("trace", "Activating environment...")
  # py_config()
  # if (!py_available()) {
  #   log_it("error", "Unknown error. RDKit will not be available.")
  # } else {
  #   log_it("trace", "Checking module compliance...")
  #   env_mods <- c(env_mods, "rpytools")
  #   module_available <- env_mods %>%
  #     lapply(py_module_available) %>%
  #     unlist()
  #   if (!all(module_available)) {
  #     missing_modules <- env_mods[!module_available]
  #     log_it("error",
  #            sprintf("Modules %s %s not available.",
  #                    format_list_of_names(missing_modules),
  #                    ifelse(length(missing_modules) > 1, "were", "was")
  #            )
  #     )
  #     rm(missing_modules)
  #   } else {
  #     log_it("trace", glue::glue("Assigning RDKit to {env_ref}..."))
  #     assign(
  #       x = env_ref,
  #       value = import("rdkit"),
  #       envir = .GlobalEnv
  #     )
  #   }
  #   rm(module_available)
  # }
  # rm(env_name, env_mods, env_ref, py_settings, py_envs)
  
  # --- Ideally, just do this simply here by abstraction ---
  if (!exists(PYENV_NAME)) {
    if (exists("PYENV_NAME")) {
      if (!exists("PYENV_REF")) {
        PYENV_REF <- PYENV_NAME
      }
      setup_rdkit(PYENV_NAME, PYENV_REF)
    }
  } else {
    log_it("trace", "RDKit not requested according to USE_RDKIT setting in env_glob.txt settings.")
  }
  # ---
} else {
  log_it("info", "Using ChemmineR for this session.")
}

# _Clean up --------------------------------------------------------------------
rm(sources, exclusions, fragments, exactmasschart)

