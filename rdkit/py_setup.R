#' Activate a python environment
#'
#' Programmatically setting up python bindings is a bit more convoluted than in
#' a standard script. Given the name of a Python environment, it either (1)
#' checks the provided `env_name` against currently installed environments and
#' binds the current session to it if found OR (2) installs a new environment
#' with [create_rdkit_conda_env] and activates it by calling itself.
#'
#' It is recommended that project variables in `env_py.R` and `env_glob.txt` be
#' used to control most of the behavior of this function. This works with both
#' virtual and conda environments, though creation of new environments is done
#' in conda.
#'
#' @param env_name CHR scalar of a python environment name to bind. The
#'   default, NULL, will look for a global environment variable named
#'   `PYENV_NAME`
#'
#' @return LGL scalar of whether or not activate was successful
#' @export
#' 
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
      log_it("warn", "No environment name available at PYENV_NAME. Searching for installed environments...")
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
    log_it("error", "There was a problem binding to python.")
    return(FALSE)
  }
  return(any(virt_env, conda_env))
}

#' Update a conda environment from a requirements file
#'
#' The `requirements_file` can be any formatted file that contains a definition
#' for python libraries to add to an environment (e.g. requirements.txt,
#' environment.yml, etc) that is understood by conda. Relative file paths are
#' fine, but the file will not be discovered (e.g. by `list.files`) so
#' specificity is always better.
#'
#' @note This requires conda CLI tools to be installed.
#' @note A default installation alias of "conda" is assumed.
#' @note Set global variable `CONDA_CLI` to your conda alias for better support.
#'
#' @param env_name CHR scalar of a python environment
#' @param requirements_file CHR scalar file path to a suitable requirements.txt
#'   or environment.yml file
#' @param conda_alias CHR scalar of the command line interface alias for your
#'   conda tools (default: NULL is translated first to the environment variable
#'   CONDA_CLI and then to "conda")
#'
#' @return
#' @export
#'
#' @examples
update_conda_env <- function(env_name, requirements_file, conda_alias = NULL) {
  # Argument validation relies on verify_args
  if (is.null(conda_alias)) {
    conda_alias <- ifelse(exists("CONDA_CLI"), CONDA_CLI, "conda")
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        env_name          = list(c("mode", "character"), c("length", 1)),
        requirements_file = list(c("mode", "character"), c("length", 1)),
        conda_alias       = list(c("mode", "character"), "not_empty")
      ),
      from_fn = "update_conda_env"
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(file.exists(requirements_file))
  if (Sys.which(conda_alias) == "") {
    stop("CLI alias '", conda_alias, "' not recognized.")
  }
  sys_cmd <- sprintf(
    "conda env update --name %s --file %s",
    env_name,
    requirements_file
  )
  log_it("trace", sprintf("Issuing system command '%s'", sys_cmd))
  system(sys_cmd)
}

#' Create a python environment for RDKit
#'
#' This project offers a full integration of RDKit via [reticulate]. This
#' function does the heavy lifting for setting up that environment, either from
#' an environment specifications file or from the conda forge channel.
#'
#' Preferred set up is to set variables in the `env_py.R` file, which will be
#' used over the internal defaults chosen here. The exception is if
#' `INSTALL_FROM == "local"` and no value is provided for `INSTALL_FROM_FILE`
#' which has no internal default.
#'
#' Germane variables are `PYENV_NAME` (default "reticulated_rdkit"),
#' `CONDA_PATH` (default "auto"), `CONDA_MODULES` (default "rdkit",
#' "r-reticulate" will be added), `INSTALL_FROM` (default "conda"),
#' `INSTALL_FROM_FILE` (default "rdkit/environment.yml"), `MIN_PY_VER` (default
#' 3.9).
#'
#' @param env_name CHR scalar of a python environment
#'
#' @return None
#' @export
#'
#' @examples
#' create_rdkit_conda_env()
create_rdkit_conda_env <- function(env_name = NULL) {
  require(reticulate)
  if (is.null(env_name)) {
    env_name <- ifelse(
      exists(PYENV_NAME),
      PYENV_NAME,
      "reticulated_rdkit"
    )
  }
  env_mods <- if (exists("CONDA_MODULES")) CONDA_MODULES else "rdkit"
  if (!"r-reticulate" %in% env_mods) env_mods <- c("r-reticulate", env_mods)
  if (!env_name %in% conda_list()$name) {
    install_from <- ifelse(
      exists("INSTALL_FROM"),
      INSTALL_FROM,
      "conda"
    )
    if (install_from == "local") {
      if (exists("INSTALL_FROM_FILE")) {
        if (file.exists(INSTALL_FROM_FILE)) {
          if (!length(INSTALL_FROM_FILE) == 1) {
            install_from <- "conda"
            log_it("warn", "More than one environment file found; defaulting to conda build.")
          } else {
            log_it("info",
                   sprintf('Building from %s...',
                           basename(INSTALL_FROM_FILE))
            )
            if (!exists("MIN_PY_VER")) MIN_PY_VER <- 3.9
            conda_create(env_name, python_version = MIN_PY_VER)
            update_conda_env(env_name, INSTALL_FROM_FILE)
          }
        } else {
          log_it("warn",
                 sprintf('Environment file "%s" does not exist; defaulting to conda build.',
                         INSTALL_FROM_FILE)
          )
          install_from == "conda"
        }
      } else {
        log_it("warn", "No environment file defined (INSTALL_FROM_FILE); defaulting to conda build.")
        install_from == "conda"
      }
    }
    if (install_from == "conda") {
      if (!exists("CONDA_PATH")) CONDA_PATH <- "auto"
      log_it("info",
             sprintf("Building using `conda_create(env_name = %s, forge = TRUE, conda = %s, packages = %s)`.",
                     paste0('"', env_name, '"'),
                     paste0('"', CONDA_PATH, '"'),
                     sprintf('c("%s")',
                             paste0(env_mods, collapse = '", "')
                             )
                     )
      )
      conda_create(
        envname = env_name,
        forge = TRUE,
        conda = CONDA_PATH,
        packages = env_mods
      )
    }
  }
}

#' Picture a molecule from structural notation
#'
#' This is a thin wrapper to RDkit.Chem.MolFromX methods to generate molecular
#' models from common structure notation such as InChI or SMILES. All picture
#' files produced will be in portable network graphics (.png) format.
#'
#' @note Supported `mol` expressions include FASTA, HELM, Inchi, Mol2Block,
#'   Mol2File, MolBlock, MolFile, PDBBlock, PDBFile, PNGFile, PNGString,
#'   RDKitSVG, Sequence, Smarts, Smiles, TPLBlock, and TPLFile
#' @note
#'
#' @param mol CHR scalar expression of molecular structure
#' @param mol_type CHR scalar indicating the expression type of `mol` (default:
#'   "smiles")
#' @param file_name CHR scalar of an intended file destination (default: NULL
#'   will produce a random 10 character file name). Note that any file
#'   extensions provided here will be ignored.
#' @param rdkit_name CHR scalar indication the name of the R object bound to
#'   RDkit OR the name of the R object directly (i.e. without quotes)
#' @param show LGL scalar of whether to open the file after creation (default:
#'   FALSE)
#'
#' @return None, or displays the resulting picture if `show == TRUE`
#' @export
#'
#' @examples
#' caffeine <- "C[n]1cnc2N(C)C(=O)N(C)C(=O)c12"
#' molecule_picture(caffeine, show = TRUE)
molecule_picture <- function(mol, mol_type = "smiles", file_name = NULL, rdkit_name = "rdk", show = FALSE) {
  if (!is.character(rdkit_name)) rdkit_name <- deparse(substitute(rdkit_name))
  stopifnot(exists(rdkit_name))
  rdk <- .GlobalEnv[[rdkit_name]]
  from_func <- sprintf("MolFrom%s", stringr::str_to_sentence(mol_type))
  if (!from_func %in% names(rdk$Chem)) {
    stop("Did not recognize '", from_func, "' as a valid RDkit.Chem module.")
  } else {
    molecule <- rdk$Chem[[from_func]](mol)
    if (!dir.exists("images")) dir.create("images")
    if (!dir.exists(file.path("images", "molecules"))) dir.create(file.path("images", "molecules"))
    filepath <- file.path(
      "images",
      "molecules",
      sprintf(
        "%s.png",
        ifelse(
          is.null(file_name),
          paste0(sample(c(letters, 0:9), 10, replace = TRUE), collapse = ""),
          tools::file_path_sans_ext(file_name)
        )
      )
    )
    picture <- try(rdk$Chem$Draw$MolToFile(molecule, filepath))
    successful <- !"try-error" %in% class(picture)
    if (successful) {
      log_it("success", sprintf('File created at "%s"', filepath))
      if (show) file.show(filepath)
    } else {
      log_it("error", "There was a problem drawing this molecule.")
      filepath <- NA
    }
    out <- list(structure = mol,
                notation = mol_type,
                valid = successful,
                file = filepath)
    return(out)
  }
}

#' Sanity check on RDKit binding
#'
#' Given a name of an R object, performs a simple check on RDKit availability on
#' that object, creating it if it does not exist. A basic structure conversion
#' check is tried and a TRUE/FALSE result returned.
#'
#' @param rdkit_ref CHR scalar OR R object of an RDKit binding (default NULL
#'   goes to "rdk" for convenience with other pipelines in this project)
#'
#' @return LGL scalar of whether or not the test of RDKit was successful
#' @export
#' 
rdkit_active <- function(rdkit_ref = NULL) {
  if (!exists("PYENV_REF")) PYENV_REF <- "rdk"
  if (is.null(rdkit_ref)) {
    rdkit_ref <- PYENV_REF
  } else {
    if (!is.character(rdkit_ref)) {
      rdkit_ref <- deparse(substitute(rdkit_ref))
    }
  }
  if (!exists("PYENV_MODULE")) PYENV_MODULE <- "rdkit"
  if (!py_available()) {
    stop("Python is not available.")
  }
  for (mod in PYENV_MODULE) {
    if (!py_module_available(mod)) {
      stop(mod, " is not available.")
    }
  }
  if (!exists(eval(rdkit_ref))) {
    assign(
      PYENV_REF,
      import(PYENV_MODULE),
      envir = .GlobalEnv
    )
  }
  rdk <- .GlobalEnv[[rdkit_ref]]
  caffeine <- "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
  active <- try(rdk$Chem$MolFromSmiles(caffeine))
  if ("try-error" %in% class(active)) {
    active <- try(rdk$MolFromSmiles(caffeine))
  }
  success <- !"try-error" %in% class(active)
  if (success) {
    log_it("success", sprintf('RDKit assigned to .GlobalEnv as "%s".', rdkit_ref))
  } else {
    log_it("error", "An unknown error occurred. See log for details.")
  }
  return(success)
}

#' Conveniently set up an RDKit python environment for use with R
#'
#' @param env_name CHR scalar of the name of a python environment
#' @param env_ref CHR scalar of the name of an R expression bound to a python
#'   library OR an R object reference by name to an existing object that should be
#'   bound to RDKit (e.g. from [reticulate::import])
#'
#' @return None, though calls to utility functions will give their own returns
#' @export
#'
setup_rdkit <- function(env_name, env_ref) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        env_name = list(c("mode", "character"), c("length", 1)),
        env_ref  = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "setup_rdkit"
    )
    stopifnot(arg_check$valid)
  }
  can_activate <- activate_py_env(env_name)
  success <- rdkit_active(env_ref)
  if (!success) stop("Unable to set up RDKit.")
}
