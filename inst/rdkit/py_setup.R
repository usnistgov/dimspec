#' Activate a python environment
#'
#' Programmatically setting up python bindings is a bit more convoluted than in
#' a standard script. Given the name of a Python environment, it either (1)
#' checks the provided `env_name` against currently installed environments and
#' binds the current session to it if found OR (2) installs a new environment
#' with [create_py_env] and activates it by calling itself.
#'
#' It is recommended that project variables in `../config/env_py.R` and
#' `../config/env_glob.txt` be used to control most of the behavior of this
#' function. This works with both virtual and conda environments, though
#' creation of new environments is done in conda.
#'
#' @note Where parameters are NULL, [rectify_null_from_env] will be used to get
#'   a value associated with it if they exist.
#'
#' @param env_name CHR scalar of a python environment name to bind. The default,
#'   NULL, will look for an environment variable named `PYENV_NAME`
#' @param required_libraries CHR vector of python libraries to include in the
#'   environment, if building a new environment. Ignored if `env_name` is an
#'   existing environment. The default, NULL, will look for an environment
#'   variable named `PYENV_LIBRARIES`.
#' @param required_modules CHR vector of modules to be checked for availability
#'   once the environment is activated. The default, NULL, will look for an
#'   environment variable named `PYENV_MODULES`.
#' @param log_ns CHR scalar of the logging namespace to use, if any.
#'
#' @return LGL scalar of whether or not activate was successful
#' @export
#' 
activate_py_env <- function(env_name = NULL, required_libraries = NULL, required_modules = NULL, log_ns = NULL, conda_path = NULL) {
  stopifnot(require(reticulate))
  logger <- exists("log_it")
  if (logger) log_fn("start")
  log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
  env_name <- rectify_null_from_env(env_name, PYENV_NAME, NULL, log_ns)
  required_libraries <- rectify_null_from_env(required_libraries, PYENV_LIBRARIES, NULL, log_ns)
  required_modules <- rectify_null_from_env(required_modules, PYENV_MODULES, NULL, log_ns)
  conda_path <- rectify_null_from_env(conda_path, CONDA_PATH, NULL, log_ns)
  # if (!is.null(env_name)) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(env_name, log_ns),
      conditions = list(
        env_name = list(c("mode", "character"), c("length", 1)),
        log_ns   = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "activate_py_env"
    )
    stopifnot(arg_check$valid)
  }
  if (logger) {
    log_it("info",
           glue::glue('Attemping to bind to python environment "{env_name}".'),
           log_ns)
  }
  
  if (logger) log_it("info", "Checking for python installations...this may take a moment...", log_ns)
  py_discover_config(required_module = required_modules, use_environment = env_name)
  
  virt_env  <- env_name %in% virtualenv_list()
  conda_env <- env_name %in% conda_list(conda = conda_path)$name
  if (virt_env) {
    use_virtualenv(virtualenv = env_name, required = TRUE)
  } else if (conda_env) {
    use_condaenv(condaenv = env_name, conda = conda_path, required = TRUE)
  }
  if (any(virt_env, conda_env)) {
    if (logger) log_it("success", glue::glue('Python environment "{env_name}" activated.'), log_ns)
  } else {
    if (logger) log_it("warn", glue::glue('Cannot identify a virtual or conda environment named "{env_name}".'), log_ns)
    if (interactive()) {
      create <- select.list(
        title = sprintf('Create a new python environment named "%s"?', env_name),
        choices = c("Yes", "No"),
        preselect = "Yes"
      )
      if (create == "Yes") { 
        if (logger) {
          if (is.null(required_libraries)) {
            log_it("warn", "No required libraries were defined. You will need to manually add libraries.")
          }
          log_it("info", "Building environment...", log_ns)
        }
        create_py_env(env_name, required_libraries, log_ns, conda_path, activate = TRUE)
      } else {
        if (logger) log_it("info", "Environment creation aborted.", log_ns)
        return(invisible(FALSE))
      }
    } else {
      if (logger) log_it("error", "Non-interactive session, terminating python binding.", log_ns)
      return(invisible(FALSE))
    }
  }
  # Force binding here
  py_config()
  # Ensure availability
  if (!py_available()) {
    if (logger) {
      log_it("error", "There was a problem binding to python.", log_ns)
      if (exists("create") && create == "Yes") {
        log_it("info", sprintf('A new environment named "%s" was created but "%s" is currently active.', env_name, py_config()$name))
      }
    }
    return(invisible(FALSE))
  }
  if (!py_modules_available(required_modules)) {
    return(invisible(FALSE))
  }
  if (logger) log_fn("end", log_ns)
  return(invisible(TRUE))
}

#' Update a conda environment from a requirements file
#'
#' The `requirements_file` can be any formatted file that contains a definition
#' for python libraries to add to an environment (e.g. requirements.txt,
#' environment.yml, etc) that is understood by conda. Relative file paths are
#' fine, but the file will not be discovered (e.g. by `list.files`) so
#' specificity is always better.
#'
#' This is a helper function, largely to support versions of reticulate prior to
#' the introduction of the environment argument in version 1.24+.
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
#' @return None, directly updates the referenced python environment
#' @export
#'
#' @usage update_env_from_file("nist_hrms_db")
update_env_from_file <- function(env_name, requirements_file, conda_alias = NULL, log_ns = NULL) {
  logger <- exists("log_it")
  if (logger) {
    log_ns <- rectify_null_from_env(log_ns, PYENV_REF, "rdk")
    log_fn("start", log_ns)
  }
  # Argument validation relies on verify_args
  conda_alias <- rectify_null_from_env(conda_alias, CONDA_CLI, "conda")
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(env_name, requirements_file, conda_alias),
      conditions = list(
        env_name          = list(c("mode", "character"), c("length", 1)),
        requirements_file = list(c("mode", "character"), c("length", 1)),
        conda_alias       = list(c("mode", "character"), "not_empty")
      ),
      from_fn = "update_env_from_file"
    )
    stopifnot(arg_check$valid)
  }
  stopifnot(file.exists(requirements_file))
  if (Sys.which(conda_alias) == "") {
    if (logger) log_it("warn", "CLI alias '", conda_alias, "' not recognized. There may be a PATH issue. Attempting fallback update...")
    # TODO fall back if conda alias is not available...not sure what a good option is here other than activating and installing dependencies manually...
  } else {
    sys_cmd <- sprintf(
      '%s env update --name %s --file "%s" --prune',
      conda_alias,
      env_name,
      requirements_file
    )
    if (logger) log_it("trace", sprintf("Issuing shell command '%s'", sys_cmd), log_ns)
    if (.Platform$OS.type == "windows") {
      shell(sys_cmd)
    } else {
      system(sys_cmd)
    }
  }
  if (logger) log_it("end", log_ns)
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
#' @usage create_py_env("nist_hrms_db", c("reticulate", "rdkit"))
create_py_env <- function(env_name = NULL, required_libraries = NULL, log_ns = NULL, conda_path = NULL, activate = TRUE) {
  stopifnot(require(reticulate))
  if (is.null(required_libraries)) {
    if (exists("PYENV_LIBRARIES")) {
      required_libraries <- PYENV_LIBRARIES
    }
  }
  env_name <- rectify_null_from_env(env_name, PYENV_NAME, NULL)
  required_libraries <- unique(
    c(
      rectify_null_from_env(required_libraries, PYENV_LIBRARIES, "r-reticulate"),
      "r-reticulate"
    )
  )
  conda_path <- rectify_null_from_env(conda_path, CONDA_PATH, "auto")
  log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
  if (is.null(env_name)) {
    if (length(required_libraries) == 1 && required_libraries == "r-reticulate") {
      env_name <- "r-reticulate"
    } else {
      env_name <- sprintf("r-%s", required_libraries[1])
    }
  }
  
  logger <- exists("log_it")
  if (logger) log_fn("start", log_ns)
  env_names <- c(
    conda_list()$name,
    virtualenv_list()
  )
  if (env_name %in% env_names) {
    stop("A python environment named", env_name, "already exists on this system.")
  } else {
    use_py_ver <- rectify_null_from_env(NULL, USE_PY_VER, "3.9")
    if (logger) {
      log_it("info",
             sprintf("Creating through reticulate using python %s and conda path %s",
                     use_py_ver, conda_path),
             log_ns)
    }
    install_from <- rectify_null_from_env(NULL, INSTALL_FROM, "conda")
    if (install_from == "local") {
      install_from_file <- rectify_null_from_env(NULL, INSTALL_FROM_FILE, NULL)
      if (!is.null(install_from_file)) {
        if (file.exists(install_from_file)) {
          if (!length(install_from_file) == 1) {
            install_from <- "conda"
            if (logger) {
              log_it("warn",
                     "More than one requirements file found; defaulting to conda build.",
                     log_ns)
            }
          } else {
            if (packageVersion("reticulate") >= "1.23") {
              if (logger) {
                cmd <- sprintf('conda_create(envname = "%s", environment = "%s")',
                               env_name,
                               install_from_file)
                log_it("info",
                       sprintf('Building using `%s`. This may take a moment...', cmd),
                       log_ns)
              }
              conda_create(envname = env_name, environment = install_from_file, conda = conda_path)
            } else {
              conda_cli <- rectify_null_from_env(NULL, CONDA_CLI, "conda")
              if (logger) {
                cmd <- sprintf('conda_create(envname = "%s", python_version = %s, conda = "%s")',
                               env_name,
                               use_py_ver,
                               conda_path)
                cmd2 <- sprintf('update_env_from_file(env_name = "%s", requirements_file = "%s", conda_alias = "%s", log_ns = "%s")',
                                env_name,
                                install_from_file,
                                conda_cli,
                                log_ns)
                log_it("info",
                       sprintf('Reticulate version %s was detected (1.23+ is recommended). Building with `%s` and then updating with `%s`. This may take a moment...',
                               packageVersion("reticulate"),
                               cmd,
                               cmd2),
                       log_ns)
              }
              conda_create(envname = env_name, python_version = use_py_ver, conda = conda_path)
              update_env_from_file(env_name = env_name, requirements_file = install_from_file, conda_alias = conda_cli, log_ns = log_ns)
            }
          }
        } else {
          if (logger) {
            log_it("warn",
                   sprintf('Environment file "%s" does not exist; defaulting to conda build.',
                           install_from_file),
                   log_ns)
          }
          install_from <- "conda"
        }
      } else {
        if (logger) {
          log_it("warn",
                 "No environment file defined as INSTALL_FROM_FILE in env_py.R; defaulting to conda build.",
                 log_ns)
        }
        install_from <- "conda"
      }
    }
    if (install_from == "conda") {
      if (logger) {
        cmd <- sprintf('conda_create(env_name = "%s", forge = TRUE, conda = "%s", packages = %s)',
                       env_name,
                       conda_path,
                       sprintf('c("%s")',
                               paste0(required_libraries, collapse = '", "')))
        log_it("info",
               sprintf("Building using `%s`. This may take a moment.", cmd),
               log_ns)
      }
      conda_create(
        envname = env_name,
        forge = TRUE,
        conda = conda_path,
        python_version = use_py_ver,
        packages = required_libraries
      )
    }
  }
  if (activate) {
    use_condaenv(condaenv = env_name, conda = conda_path, required = TRUE)
  }
  if (logger) log_fn("end", log_ns)
}

#' Picture a molecule from structural notation
#'
#' This is a thin wrapper to rdkit.Chem.MolFromX methods to generate molecular
#' models from common structure notation such as InChI or SMILES. All picture
#' files produced will be in portable network graphics (.png) format.
#'
#' @note Supported `mol` expressions include FASTA, HELM, Inchi, Mol2Block,
#'   Mol2File, MolBlock, MolFile, PDBBlock, PDBFile, PNGFile, PNGString,
#'   RDKitSVG, Sequence, Smarts, Smiles, TPLBlock, and TPLFile
#'
#' @param mol CHR scalar expression of molecular structure
#' @param mol_type CHR scalar indicating the expression type of `mol` (default:
#'   "smiles")
#' @param file_name CHR scalar of an intended file destination (default: NULL
#'   will produce a random 10 character file name). Note that any file
#'   extensions provided here will be ignored.
#' @param rdkit_name CHR scalar indication the name of the R object bound to
#'   RDkit OR the name of the R object directly (i.e. without quotes)
#' @param open_file LGL scalar of whether to open the file after creation
#'   (default: FALSE)
#' @param show LGL scalar of whether to return the image itself as an object
#'   (default: FALSE)
#'
#' @return None, or displays the resulting picture if `show == TRUE`
#' @export
#'
#' @usage
#' caffeine <- "C[n]1cnc2N(C)C(=O)N(C)C(=O)c12"
#' molecule_picture(caffeine, show = TRUE)
molecule_picture <- function(mol,
                             mol_type = c("smiles", "inchi", "smarts", "sequence"),
                             file_name = NULL,
                             rdkit_name = "rdk",
                             open_file = FALSE,
                             show = FALSE,
                             log_ns = NULL) {
  if (exists("log_it")) {
    logging <- TRUE
    log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
  } else {
    logging <- FALSE
  }
  if (logging) log_fn("start", log_ns)
  if (!is.character(rdkit_name)) rdkit_name <- deparse(substitute(rdkit_name))
  stopifnot(exists(rdkit_name))
  rdk <- .GlobalEnv[[rdkit_name]]
  mol_type <- tolower(mol_type)
  mol_type <- match.arg(mol_type)
  if (stringr::str_detect(mol, "InChI=") && !mol_type == "inchi") mol_type <- "inchi"
  from_func <- sprintf("MolFrom%s", stringr::str_to_sentence(mol_type))
  if (!from_func %in% names(rdk$Chem)) {
    stop("Did not recognize '", from_func, "' as a valid RDkit.Chem module.")
  } else {
    molecule <- try(rdk$Chem[[from_func]](mol))
    if (!dir.exists(here::here("images"))) dir.create(here::here("images"))
    if (!dir.exists(here::here("images", "molecules"))) dir.create(here::here("images", "molecules"))
    filepath <- here::here(
      "images",
      "molecules",
      sprintf(
        "%s.png",
        ifelse(
          is.null(file_name),
          mol %>%
			# Bad end
			str_replace_all("\\.+$| $", "") %>%
		    # SMILES specific
		    str_replace_all("\\*", "arom") %>%
		    str_replace_all("\\.", "dscn") %>%
			# OS invalid
		    str_replace_all("/", "frsl") %>%
		    str_replace_all("<", "lt") %>%
		    str_replace_all(">", "gt") %>%
		    str_replace_all(":", "colon") %>%
		    str_replace_all("\"", "dbquo") %>%
		    str_replace_all("\\\\", "bksl") %>%
		    str_replace_all("\\|", "vbar") %>%
		    str_replace_all("\\?", "unkn") %>%
			# Windows reserved
			str_replace_all("(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])", "cntrl\\1") %>%
			str_replace_all("\\\\([0-9]+)", "ascii\\1"),
          tools::file_path_sans_ext(file_name)
        )
      )
    )
    file_exists <- file.exists(filepath)
    if (file_exists) {
      picture <- imager::load.image(filepath)
    } else {
      picture <- try(rdk$Chem$Draw$MolToFile(molecule, filepath))
    }
    successful <- !inherits(picture, "try-error")
    if (successful) {
      if (logging) log_it("success", sprintf('File %s at "%s"', ifelse(file_exists, "located", "created"), filepath), log_ns)
      if (open_file) {
        mol_image <- load.image(filepath)
		return(mol_image)
      } else if (show) {
        browseURL(filepath)
      }
    } else {
      if (logging) log_it("error", "There was a problem drawing this molecule.", log_ns)
      filepath <- NA
    }
    out <- list(structure = mol,
                notation = mol_type,
                valid = successful,
                file = filepath)
    if (logging) log_fn("end", log_ns)
    return(out)
  }
}

#' Are all conda modules available in the active environment
#'
#' Checks that all defined modules are available in the currently active python
#' binding. Supports error logging
#'
#' @param required_modules CHR vector of required modules
#'
#' @return LGL scalar of whether or not all modules are available. Check console
#'   for further details.
#' @export
#'
#' @usage py_modules_available("rdkit")
py_modules_available <- function(required_modules, log_ns = NULL) {
  log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
  logging <- exists("log_it")
  if (logging) log_fn("start", log_ns)
  if (!py_available()) {
    msg <- "Python is not available. Bind a python environment first, perhaps with reticulate::py_config()."
    if (logging) {
      log_it("error", msg, log_ns)
    } else {
      cat("\n", paste0(msg, collapse = "\n"))
    }
    return(FALSE)
  }
  required_modules <- unique(c("rpytools", required_modules))
  modules_available <- sapply(required_modules, py_module_available)
  if (any(modules_available)) {
    msg <- sprintf("Module '%s' is available.", required_modules[modules_available])
    if (logging) {
      sapply(msg, function(x) log_it("trace", x, log_ns))
    } else {
      cat("\n", paste0(msg, collapse = "\n"))
    }
  }
  if (!all(modules_available)) {
    msg <- sprintf("Module '%s' is not available.", required_modules[!modules_available])
    if (logging) {
      sapply(msg, function(x) log_it("error", x, log_ns))
    } else {
      cat("\n", paste0(msg, collapse = "\n"))
    }
  }
  if (logging) log_fn("end", log_ns)
  return(all(modules_available))
}

#' Sanity check on RDKit binding
#'
#' Given a name of an R object, performs a simple check on RDKit availability on
#' that object, creating it if it does not exist. A basic structure conversion
#' check is tried and a TRUE/FALSE result returned. Leave all arguments as their
#' defaults of NULL to ensure they will honor the settings in `rdkit/env_py.R`.
#'
#' @param rdkit_ref CHR scalar OR R object of an RDKit binding (default NULL
#'   goes to "rdk" for convenience with other pipelines in this project)
#' @param rdkit_name CHR scalar the name of a python environment able to run
#'   rdkit (default NULL goes to "rdkit" for convenience with other pipelines in
#'   this project)
#' @param make_if_not LGL scalar of whether or not to create a new python
#'   environment using [activate_py_env] if the binding is not active
#' @param log_ns
#'
#' @return LGL scalar of whether or not the test of RDKit was successful
#' @export
#' 
rdkit_active <- function(rdkit_ref = NULL, rdkit_name = NULL, log_ns = NULL, make_if_not = FALSE) {
  if (!is.character(rdkit_ref)) rdkit_ref <- deparse(substitute(rdkit_ref))
  if (rdkit_ref == "NULL") rdkit_ref <- NULL
  rdkit_name <- rectify_null_from_env(rdkit_name, PYENV_NAME, "rdkit")
  rdkit_ref <- rectify_null_from_env(rdkit_ref, PYENV_REF, "rdk")
  log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
  logging <- exists("log_it") && exists("LOGGING_ON") && LOGGING_ON
  if (logging) log_fn("start", log_ns)
  if (!rdkit_ref %in% names(.GlobalEnv)) {
    msg <- sprintf('Object "%s" not found.', rdkit_ref)
    if (logging) {
      log_it("warn", msg, log_ns)
    } else {
      cat(msg)
    }
    if (make_if_not) {
      if (logging) {
        log_it("info", sprintf('Tying object "%s" to rdkit in this environment.', rdkit_ref), log_ns)
      }
      activate_py_env(env_name = rdkit_name)
      assign(rdkit_ref, import("rdkit"), envir = .GlobalEnv)
      return(rdkit_active(rdkit_ref = rdkit_ref, log_ns = log_ns))
    } else {
      if (logging) {
        log_it("error", sprintf('"%s" was not added to the environment (rdkit_active was called with "make_if_not" = FALSE)', rdkit_ref), log_ns)
      }
      return(FALSE)
    }
  }
  rdk <- .GlobalEnv[[rdkit_ref]]
  if (!py_modules_available("rdkit")) return(FALSE)
  caffeine <- "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
  active <- try(rdk$Chem$MolFromSmiles(caffeine))
  if ("try-error" %in% class(active)) {
    active <- try(rdk$MolFromSmiles(caffeine))
  }
  success <- !"try-error" %in% class(active)
  if (logging) {
    if (success) {
      log_it("success", sprintf('RDKit is assigned to the environment as "%s".', rdkit_ref), log_ns)
    } else {
      log_it("error", "An unknown error occurred. See log for details.", log_ns)
    }
    log_fn("end", log_ns)
  }
  return(success)
}

#' Conveniently set up an RDKit python environment for use with R
#'
#' @param env_name CHR scalar of the name of a python environment
#' @param env_ref CHR scalar of the name of an R expression bound to a python
#'   library OR an R object reference by name to an existing object that should be
#'   bound to RDKit (e.g. from [reticulate::import])
#' @param ns CHR scalar
#'
#' @return None, though calls to utility functions will give their own returns
#' 
#' @usage setup_rdkit(env_name = "nist_hrms_db", required_libraries = c("reticulate", "rdkit"), env_ref = "rdk")
setup_rdkit <- function(env_name = NULL, required_libraries = NULL, env_ref = NULL, log_ns = NULL, conda_path = NULL) {
  logger <- exists("log_it")
  if (logger) {
    log_ns <- rectify_null_from_env(log_ns, PYENV_REF, NA_character_)
    log_fn("start", log_ns)
  }
  env_name <- rectify_null_from_env(env_name, PYENV_NAME, NULL)
  required_libraries <- rectify_null_from_env(required_libraries, PYENV_LIBRARIES, "rdkit")
  env_ref <- rectify_null_from_env(env_ref, PYENV_REF, "rdk")
  conda_path <- rectify_null_from_env(conda_path, CONDA_PATH, "auto")
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(env_name, required_libraries, env_ref, log_ns, conda_path),
      conditions = list(
        env_name           = list(c("mode", "character"), c("length", 1)),
        required_libraries = list(c("mode", "character"), c("n>=", 1)),
        env_ref            = list(c("mode", "character"), c("length", 1)),
        log_ns             = list(c("mode", "character"), c("length", 1)),
        conda_path         = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "setup_rdkit"
    )
    stopifnot(arg_check$valid)
  }
  can_activate <- activate_py_env(env_name = env_name, required_libraries = required_libraries, log_ns = log_ns, conda_path = conda_path)
  if (!can_activate && logger) log_it("warn", sprintf('There was a problem activating environment "%s". Searching for suitable environments...', env_name), log_ns)
  success <- rdkit_active(rdkit_ref = env_ref, log_ns = log_ns, make_if_not = TRUE)
  if (!success) stop("Unable to set up RDKit.")
  if (exists("log_it")) log_fn("end", log_ns)
}

#' Create aliases for a molecule from RDKit
#'
#' Call this function to generate any number of machine-readable aliases from an
#' identifier set. Given the `identifiers` and their `type`, RDKit will be
#' polled for conversion functions to create a mol object. That mol object is
#' then used to create machine-readable aliases in any number of supported
#' formats. See the [RDKit Documentation](http://rdkit.org/docs/index.html) for
#' options. The `type` argument is used to match against a "MolFromX" funtion,
#' while the `aliases` argument is used to match against a "MolToX" function.
#'
#' At the time of authorship, RDK v2021.09.4 was in use, which contained the
#' following options findable by this function: CMLBlock, CXSmarts, CXSmiles,
#' FASTA, HELM, Inchi, InchiAndAuxInfo, InchiKey, JSON, MolBlock, PDBBlock,
#' RandomSmilesVect, Sequence, Smarts, Smiles, TPLBlock, V3KMolBlock, XYZBlock.
#'
#' @note Both `type` and `aliases` are case insensitive.
#' @note If `aliases` is set to NULL, all possible expressions (excluding those
#'   with "File" in the name) are returned from RDKit, which will likely produce
#'   NULL values and module ArgumentErrors.
#'
#' @inheritParams rdkit_active
#'
#' @param identifiers CHR vector of machine-readable molecule identifiers in a
#'   format matching `type`
#' @param type CHR scalar of the type of encoding to use for `identifiers`
#'   (default: smiles)
#' @param mol_from_prefix CHR scalar of the prefix to identify an RDKit function
#'   to create a mol object from`identifiers` (default: "MolFrom")
#' @param get_aliases CHR vector of aliases to produce (default: c("inchi",
#'   "inchikey"))
#' @param mol_to_prefix CHR scalar of the prefix to identify an RDKit function
#'   to create an alias from a mol object (default: "MolTo")
#'
#' @return data.frame object containing the aliases and the original identifiers
#' @export
#' 
rdkit_mol_aliases <- function(identifiers, type = "smiles", mol_from_prefix = "MolFrom", get_aliases = c("inchi", "inchikey"), mol_to_prefix = "MolTo", rdkit_ref = "rdk", log_ns = "rdk", make_if_not = TRUE) {
  logging <- exists("LOGGING_ON") && LOGGING_ON && exists("log_it")
  stopifnot(
    all(unlist(lapply(c(type, mol_from_prefix, mol_to_prefix, rdkit_ref, log_ns), is.character))),
    all(unlist(lapply(c(type, rdkit_ref, log_ns, make_if_not), function(x) length(x) == 1))),
    length(identifiers) > 0,
    is.logical(make_if_not)
  )
  can_calc <- rdkit_active(
    rdkit_ref = rdkit_ref,
    log_ns = log_ns,
    make_if_not = make_if_not
  )
  if (!can_calc) return(NULL)
  if (tolower(type) %in% tolower(names(identifiers))) {
    link_i <- which(tolower(names(identifiers)) == tolower(type))
    original_name <- names(identifiers)[link_i]
    identifiers <- identifiers[[link_i]]
  } else {
    original_name <- type
  }
  if (can_calc) {
    if (logging) log_it("info", "Verifying existence of identifiers.", log_ns)
    to_remove <- which(identifiers == "" | is.na(identifiers))
    if (length(to_remove) > 0) {
      if (logging) log_it("warn", glue::glue("{length(to_remove)} identifiers were blank or NA."), log_ns)
      identifiers <- identifiers[-to_remove]
    }
    aliases <- tolower(get_aliases)
    # Check for doubled names
    if (any(aliases %in% tolower(names(identifiers)))) {
      aliases <- aliases[!aliases %in% tolower(names(identifiers))]
    }
    if (any(aliases %in% tolower(type))) {
      aliases <- aliases[!aliases %in% tolower(type)]
    }
    rdk <- eval(rlang::sym(rdkit_ref))
    to_mol <- grep(paste0("^", mol_from_prefix, type, "$"),
                   names(rdk$Chem),
                   ignore.case = TRUE,
                   value = TRUE)
    if (length(to_mol) == 0) {
      msg <- sprintf("Could not identify a function to create a mol structure from type = '%s'.", type)
      if (logging) {
        log_it("warn", msg, log_ns)
      } else {
        warning(msg)
      }
      return(NULL)
    }
    mols <- lapply(identifiers,
                   function(x) {
                     rdk$Chem[[to_mol]](x)
                   })
    if (is.null(get_aliases)) {
      aliases <- grep(paste0("^", mol_to_prefix), names(rdk$Chem), value = TRUE)
      alias_funcs <- aliases
    } else {
      alias_funcs <- paste0("^", mol_to_prefix, aliases, "$")
      aliases <- grep(paste0(alias_funcs, collapse = "|"),
                      names(rdk$Chem),
                      ignore.case = TRUE,
                      value = TRUE)
    }
    alias_funcs <- gsub("\\^|\\$", "", alias_funcs)
    file_refs <- grep("file", aliases, ignore.case = TRUE)
    if (length(file_refs) > 0) {
      aliases <- aliases[-file_refs]
    }
    if (length(aliases) < length(get_aliases)) {
      unfound <- get_aliases[!tolower(alias_funcs) %in% tolower(aliases)]
      msg <- sprintf('Could not identify %s function%s to create %salias%s using %s.',
                     ifelse(length(unfound) > 1, "any", "a"),
                     ifelse(length(unfound) > 1, "s", ""),
                     ifelse(length(unfound) > 1, "", "an "),
                     ifelse(length(unfound) > 1, "es", ""),
                     format_list_of_names(unfound, add_quotes = TRUE)
      )
      if (logging) {
        log_it("warn", msg, log_ns)
      } else {
        warning(msg)
      }
      if (length(aliases) == 0) return(NULL)
    }
    if (logging) log_it("info", "Generating aliases.", log_ns)
    out <- lapply(mols,
                  function(x) {
                    lapply(aliases,
                           function(func) {
                             if (logging) log_it("trace", glue::glue("Generating {gsub(mol_to_prefix, '', tolower(func))} for {type} = {x}."), log_ns)
                             res <- try(rdk$Chem[[func]](x))
                             if (inherits(res, "try-error")) {
                               return(NULL)
                             } else {
                               return(paste0(res, collapse = "; "))
                             }
                           }) %>%
                      setNames(aliases)
                  })
    out <- out %>%
      bind_rows() %>%
      select(-which(tolower(names(.)) == tolower(type))) %>%
      mutate(original = identifiers) %>%
      rename("{original_name}" := "original") %>%
      select(any_of(c(original_name, aliases)))
    names(out) <- gsub(mol_to_prefix, "", names(out))
    return(out)
  } else {
    msg <- sprintf("RDKit is not available at '%s'.", rdkit_ref)
    if (logging) {
      log_it("warn", msg, log_ns)
    } else {
      warning(msg)
    }
    return(NULL)
  }
}
