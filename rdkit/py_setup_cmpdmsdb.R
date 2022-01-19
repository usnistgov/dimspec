#' Activate a python environment
#'
#' Programmatically setting up python bindings is a bit more convoluted than in
#' a standard script.
#'
#' @param env_name CHR scalar of the python environment name to bind. The
#'   default, NULL, will look for a global environment variable named
#'   "PYENV_NAME"
#'
#' @return
#' @export
#'
#' @examples
activate_py_env <- function(env_name = NULL) {
  stopifnot(require(reticulate))
  if (!is.null(env_name)) {
    # Argument validation relies on verify_args
    if (exists("verify_args")) {
      arg_check <- verify_args(
        args       = as.list(environment()),
        conditions = list(
          env_name = list(c("mode", "character"), c("length", 1))
        ),
        from_fn = "activate_py_env"
      )
      stopifnot(arg_check$valid)
    }
    log_it("info", glue('Attemping to bind to python environment "{env_name}".'))
  } else {
    if (exists("PYENV_NAME")) {
      log_it("info", glue('Attemping to bind to python environment "{PYENV_NAME}".'))
      env_name <- PYENV_NAME
    } else {
      log_it("warn", "No environment name available. Searching for installed environments...")
    }
    py_envs <- c(
      conda_list()$name,
      virtualenv_list()
    )
    if (length(py_envs) == 0) {
      log_it("error", "No python environments were located on this system.")
      stop()
    } else if (length(py_envs) == 1) {
      log_it("info", glue::glue('Only one python environment named "{py_envs}" was located.'))
      if (interactive()) {
        do_bind <- select.list(
          title = "Is this the correct environment?",
          choices = c("Yes", "No", "Abort")
        )
        if (!do_bind == "Yes") {
          log_it("Python binding aborted.")
          return(FALSE)
        }
      } else {
        log_it("warn", "Automatic binding may cause failures if the python environment is not set up correctly.")
      }
      env_name <- py_envs
    } else {
      if (!exists("PYENV_NAME")) {
        if (interactive()) {
          env_name <- select.list(
            title = "Select an existing python environment to activate.",
            choices = py_envs
          )
        } else {
          log_it("error", 'Global variable "PYENV_NAME" was not defined. Please provide a python environment name to use.')
          return(FALSE)
        }
      }
    }
  }
  
  virt_env  <- virtualenv_exists(env_name)
  conda_env <- env_name %in% conda_list()$name
  if (virt_env) {
    use_virtualenv(virtualenv = env_name, required = TRUE)
  } else if (conda_env) {
    use_condaenv(condaenv = env_name, required = TRUE)
  }
  if (any(virt_env, conda_env)) {
    log_it("success", glue::glue('Python environment "{env_name}" activated.'))
  } else {
    log_it("warn", glue::glue('Cannot identify a virtual or conda environment named "{env_name}".'))
    if (interactive()) {
      log_it("info", "Building environment...")
      create_rdkit_conda_env(env_name)
      activate_py_env(env_name)
    } else {
      log_it("error", "Non-interactive session, terminating python binding. RDKit will not be available.")
      return(FALSE)
    }
  }
  # Force binding here
  py_config()
  # Ensure availability
  if (!py_available()) {
    lot_it("error", "There was a problem binding to python.")
    return(FALSE)
  }
  # Trap rpytools issue
  rpytools_available <- py_module_available("rpytools")
  if (!rpytools_available) {
    log_it("warn", glue::glue('Module rpytools not available the "{env_name}" environment. Attempting to add...'))
    add_rpytools()
  }
  return(any(virt_env, conda_env, rpytools_available))
}

create_rdkit_conda_env <- function(env_name = NULL) {
  if (is.null(env_name)) {
    env_name <- ifelse(
      exists(PYENV_NAME),
      PYENV_NAME,
      "rdkit"
    )
  }
  env_mods <- if (exists("PYENV_MODULE")) PYENV_MODULE else "rdkit"
  if (!"r-reticulate" %in% env_mods) env_modes <- c("r-reticulate", env_mods)
  if (!env_name %in% conda_list()$name) {
    conda_create(envname = env_name, packages = env_mods)
  }
}

molecule_picture <- function(mol, mol_type = "smiles", file_name = NULL, file_format = "png", rdkit_name = "rdk", show = FALSE) {
  if (!is.character(rdkit_name)) rdkit_name <- deparse(substitute(rdkit_name))
  stopifnot(exists(rdkit_name))
  rdk <- .GlobalEnv[[rdkit_name]]
  from_func <- sprintf("MolFrom%s", stringr::str_to_sentence(mol_type))
  if (!from_func %in% names(rdk$Chem)) {
    stop("Did not recognize '", from_func, "' as a valid RDkit.Chem module.")
  } else {
    molecule <- rdk$Chem[[from_func]](mol)
    filepath <- sprintf(
      "%s.%s",
      ifelse(
        is.null(file_name),
        paste0(sample(c(letters, 0:9), 10, replace = TRUE), collapse = ""),
        tools::file_path_sans_ext(file_name)
      ),
      file_format
    )
    rdk$Chem$Draw$MolToFile(molecule, filepath)
    if (show) file.show(filepath)
  }
}

rdkit_active <- function(rdkit_ref = NULL) {
  if (!exists("PYENV_REF")) PYENV_REF <- "rdk"
  if (is.null(rdkit_ref)) rdkit_ref <- PYENV_REF
  if (!exists("PYENV_MODULE")) PYENV_MODULE <- "rdkit"
  if (!py_available()) {
    stop("Python is not available.")
  }
  if (!exists(eval(rdkit_ref))) {
    assign(
      PYENV_REF,
      import(PYENV_MODULE),
      envir = .GlobalEnv
    )
    cat('RDKit assigned to .GlobalEnv as "', rdkit_ref, '".\n', sep = "")
  }
  rdk <- .GlobalEnv[[rdkit_ref]]
  if ("Chem" %in% names(rdk)) {
    from_smiles <- rdk$Chem$MolFromSmiles
  } else if ("MolFromSmiles" %in% names(rdk)) {
    from_smiles <- rdk$MolFromSmiles
  }
  active <- try(from_smiles("CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
  success <- !"try-error" %in% class(active)
  return(success)
}

setup_rdkit <- function(env_name, env_ref) {
  can_activate <- activate_py_env(env_name)
  success <- rdkit_active(env_ref)
  if (!success) stop("Unable to set up RDKit.")
}

add_rpytools <- function() {
  src_path <- file.path(
    grep("reticulate",
         list.files(.libPaths(), full.names = TRUE),
         value = TRUE),
    "python",
    "rpytools"
  )
  py_dest_path <- py_config()$numpy$path
  py_dest_path <- stringr::str_replace(py_dest_path, "numpy", "rpytools")
  clean_up <- FALSE
  if (!length(src_path) == 1) {
    dest_dir <- file.path("rdkit", "reticulate")
    dest_file <- file.path(dest_dir, "main.zip")
    if (!dir.exists(dest_dir)) dir.create(dest_dir)
    if (!file.exists(dest_file)) {
      log_it("info", "Downloading the latest version from github...")
      download.file(
        url = "https://github.com/rstudio/reticulate/archive/refs/heads/main.zip",
        destfile = dest_file
      )
    }
    clean_up <- TRUE
    log_it("info", "Unzipping...")
    relevant_files <- utils::unzip(zipfile = dest_file, list = TRUE)
    relevant_files <- relevant_files[grep("rpytools", relevant_files$Name), 1]
    utils::unzip(
      zipfile = dest_file,
      files = relevant_files,
      exdir = file.path(dest_dir, "unzipped")
    )
    src_path <- list.dirs(dest_dir)
    src_path <- grep("rpytools", src_path, value = TRUE)
  }
  log_it("info", "Copying files...")
  if (!dir.exists(py_dest_path)) {
    dir.create(py_dest_path)
  }
  for (d_name in list.dirs(src_path, full.names = TRUE, recursive = TRUE)) {
    dest_d_name <- str_remove(d_name, src_path)
    dest_d_name <- str_remove(dest_d_name, .Platform$file.sep)
    dest_d_name <- file.path(py_dest_path, dest_d_name)
    if (!dir.exists(dest_d_name)) dir.create(dest_d_name)
  }
  for (f_name in list.files(src_path, full.names = TRUE, recursive = TRUE)) {
    dest_f_name <- str_remove(f_name, src_path)
    dest_f_name <- str_remove(dest_f_name, .Platform$file.sep)
    file.copy(
      from = f_name,
      to = file.path(py_dest_path, dest_f_name)
    )
  }
  copy_complete <- all(basename(list.files(src_path)) %in% basename(list.files(py_dest_path)))
  if (copy_complete) {
    log_it("success", "Module transferred.")
  } else {
    log_it("error", "There was an error transferring this module.")
  }
  log_it("info", "Cleaning up...")
  if (clean_up) unlink(file.path(dest_dir, "unzipped"), recursive = TRUE)
}
