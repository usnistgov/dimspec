#' Make sure a file actually exists
#'
#' Get the fully qualified path to a given file. If the file cannot be
#' identified in the current directory (and direct subdirectories) the user will
#' be prompted to choose a file. It must be a direct match to a file name.
#'
#' @param filename CHR scalar of the expected file base name (default: NULL)
#'
#' @return CHR scalar of the fully qualified file path
#' @export
#'
#' @examples
assure_file_path <- function(filename = NULL) {
  all_files    <- list.files(recursive = TRUE)
  if (is.null(filename)) {
    filename <- file.choose()
  } else {
    direct_match <- all_files[which(basename(all_files) == filename)]
    if (length(direct_match) == 0) {
      filename <- file.choose()
    } else {
      filename <- direct_match
    }
  }
  filepath <- normalizePath(filename)
  return(filepath)
}

# Attempt at abstraction to install in a variety of ways...
# conda_reconcile <- function(py_req,
#                             use_env,
#                             install_from,
#                             method            = "auto",
#                             conda             = "auto",
#                             use_forge         = FALSE,
#                             force_pip         = FALSE) {
#   require(reticulate)
#   require(glue)
#   require(stringr)
#   cat(
#     sprintf("No python environment named '%s' was found.\n\nPlease select an existing environment to continue or choose option 1 to create a new one.",
#             use_env)
#   )
#   if (py_req == "") py_req <- NULL
#   py_envs     <- conda_list(conda = conda)$name
#   txt_new_env <- sprintf('Create new as: "%s"', use_env)
#   chosen_env  <- select.list(c(txt_new_env, py_envs))
#   min_packs   <- Sys.getenv("MIN_PACKAGES")
#   if (min_packs == "") {
#     min_packs <- character()
#   } else {
#     min_packs <- str_split(min_packs, " ")[[1]]
#   }
#   if (chosen_env == txt_new_env) {
#     if (install_from == "local") {
#       install_from_file <- assure_file_path(Sys.getenv("INSTALL_FROM_FILE"))
#       if (!any(c("txt", "yaml", "yml") %in% tools::file_ext(install_from_file))) {
#         stop("Only *.txt, *.yaml, and *.yml files are supported for installation from files at this time.")
#       }
#     } else if (install_from == "git") {
#       force_pip <- TRUE
#     }
#     channels         <- Sys.getenv("CONDA_CHANNELS")
#     if (channels == "") {
#       channels <- character()
#     } else{
#       channels <- str_split(channels, " ")[[1]]
#     }
#     install_packages <- switch(install_from,
#                        "conda" = Sys.getenv("CONDA_PACKAGE"),
#                        "git"   = c("git", glue('git+https://github.com/{Sys.getenv("GIT_SOURCE")}.git{Sys.getenv("GIT_BRANCH")}')),
#                        "local" = glue('--file {Sys.getenv("INSTALL_FROM_FILE")}')
#     )
#     conda_create(envname        = use_env,
#                  packages       = min_packs,
#                  channel        = channels,
#                  conda          = conda,
#                  python_version = py_req)
#     conda_install(envname              = use_env,
#                   packages             = install_packages,
#                   channel              = channels,
#                   conda                = conda,
#                   pip                  = force_pip,
#                   pip_ignore_installed = TRUE,
#                   python_version       = py_req)
#     has_env <- use_env %in% conda_list(conda = conda)$name
#   } else {
#     use_env <- chosen_env
#   }
#   use_condaenv(use_env, conda = conda, required = TRUE)
#   return(has_env)
# }

#' Install necessary python packages
#'
#' @param py_req CHR | NUM scalar of the python version to use. Set this to NULL
#'   to use the latest available from your channels. Default: system variable
#'   "MIN_PY_VER"
#' @param use_env
#' @param packages
#' @param channels
#' @param method
#' @param conda
#'
#' @return
#' @export
#'
#' @examples
install_from_conda <- function(py_req   = Sys.getenv("MIN_PY_VER"),
                               use_env  = Sys.getenv("USE_PY_ENV"),
                               packages = Sys.getenv("CONDA_PACKAGE"),
                               channels = Sys.getenv("CONDA_CHANNELS"),
                               method   = "auto",
                               conda    = "auto") {
  if (py_req == "") py_req <- NULL
  if (channels == "") {
    channels <- character()
  } else{
    channels <- str_split(channels, " ")[[1]]
  }
  if (packages == "") {
    packages <- character()
  } else{
    packages <- str_split(packages, " ")[[1]]
  }
  conda_create(envname        = use_env,
               packages       = packages,
               channel        = channels,
               conda          = conda,
               python_version = py_req)
  has_env <- use_env %in% conda_list(conda = conda)$name
  use_condaenv(use_env, conda = conda, required = TRUE)
  return(has_env)
}


#' Is python available?
#' 
#' Convenience check for whether python is available from the system path.
#'
#' @param py_req NUM or CHR scalar of the version of python to check
#' @return LGL scalar indicating availability.
#' @export
#'
#' @examples
#' python_available(3.8)
python_available <- function(py_req = NULL) {
  require(reticulate)
  if (py_req != "") {
    py_req = 0
  }
  py_ver <- max(as.numeric(py_discover_config()$version))
  has_python <- as.numeric(py_req) <= py_ver
  if (!has_python) {
    cat(sprintf("Python %s is required but the maximum available version is %s\n", py_req, py_ver))
  }
  return(has_python)
}

python_available_and_stop <- function(py_req = NULL) {
  if (!python_available(py_req = py_req)) {
    stop("Please address Python deficiencies and try again.")
  }
}


#' Is conda available?
#' 
#' Convenience check for whether conda is available from the system path.
#'
#' @return LGL scalar indicating availability
#' @export
#'
#' @examples
#' conda_available()
conda_available <- function(conda = "auto") {
  require(reticulate)
  # NOTE: this is not perfect.  If you pass a bad `conda`, an error will be thrown
  conda_ver <- try(reticulate::conda_version(conda = conda))
  if (class(conda_ver) == "try-error") {
    has_conda <- FALSE
  } else {
    has_conda <- conda_ver != ""
  }
  return(has_conda)
}

#' Is git available?
#' 
#' Convenience check for whether git is available. If a conda environment is requested (i.e. not ""), it will be checked via reticulate to make sure that the currently active environment includes the git module. If no conda environment has ye
#'
#' @param conda_env CHR scalar of the requested python environment
#'
#' @return LGL scalar indicating availability
#' @export
#'
#' @examples
#' git_available()
git_available <- function(conda_env = Sys.getenv("USE_PY_ENV")) {
  if (all(conda_env != "", !is.null(conda_env), !is.na(conda_env))) {
    active_conda_env <- basename(py_discover_config()$pythonhome)
    if (conda_env != active_conda_env) {
      # This will only work if reticulate has not yet been initialized.
      res <- use_condaenv(condaenv = conda_env, required = TRUE)
      has_git <- class(res) != "try-error"
      if (!has_git) {
        cat(geterrmessage(), "\nReticulate was initialized with another version of conda.")
      }
    }
    if (!has_git) {
      install_to_conda_env <- askYesNo('Module "git" is not available in this environment. Add it?')
      if (install_to_conda_env) {
        conda_install(envname = conda_env, packages = "git")
      }
    }
  } else {
    has_git <- Sys.which("git")[[1]] != ""
    if (!has_git) {
      cat("Git is required to install from GitHub or GitLab.\nIf you have one of them installed, they may not be available in your PATH.\n")
    }
  }
  return(has_git)
}

py_setup <- function(py_req       = NULL,
                     use_env      = NULL,
                     install_from = NULL,
                     min_packages = NULL,
                     conda = "auto",
                     ...) {
  # On my system, this always fails.  Need to activate conda first...
  ## if (!python_available(py_req))       stop("Please address Python deficiencies and try again.")
  if (!conda_available(conda = conda)) {
    install_mini <- askYesNo("A conda installation was not found on this system. Would you like to install miniconda?\n")
    if (install_mini) install_miniconda()
    # stop(sprintf("%s requires a conda installation and will halt.", Sys.getenv("PROJECT_NAME")))
  }
  if (is.null(use_env))      use_env      <- Sys.getenv("USE_PY_ENV")
  if (is.null(install_from)) install_from <- Sys.getenv("INSTALL_FROM")
  if (is.null(py_req))       py_req       <- Sys.getenv("MIN_PY_VER")
  if (is.null(min_packages)) min_packages <- Sys.getenv("MIN_PACKAGES")
  
  py_envs <- conda_list(conda = conda)$name
  has_env <- use_env %in% py_envs
  if (has_env) {
    use_condaenv(use_env, conda = conda, required = TRUE)
  } else {
    if (py_req == "") warning("No version of python specified. The conda default version of python will be used.")
    if (any(use_env == "", install_from == "")) stop("System variables do not completely define the desired conda environment. Please provide explicit values for all parameters.")
    cat(
      sprintf("No python environment named '%s' was found.\n\nPlease select an existing environment to continue or choose option 1 to create a new one.",
              use_env)
    )
    # repeat of above?
    py_envs     <- conda_list(conda = conda)$name
    txt_new_env <- sprintf('Create new as: "%s"', use_env)
    if (interactive()) {
      chosen_env  <- select.list(c(txt_new_env, py_envs))
      if (chosen_env == txt_new_env) {
        if (install_from == "conda") {
          has_env <- install_from_conda(use_env = use_env, py_req = py_req, conda = conda, ...)
        } else {
          stop("Creation of environments is only available from conda at this time.")
        }
      }  else {
        use_condaenv(chosen_env, conda = conda, required = TRUE)
        has_env <- TRUE
      }
    } else {
      has_env <- install_from_conda(use_env = use_env, py_req = py_req, conda = conda, ...)
    }
  }
  python_available_and_stop(py_req)
  return(has_env)
}
