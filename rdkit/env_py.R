# [REQUIRED]--------------------------------------------------------------------
# The name of the python environment to use. If this environment exists on the
# current system, it will be used, otherwise one will be created with this name.
PYENV_NAME <- "nist_hrms_db"

# The name of the R object to use for importing the rdkit module. Every effort
# has been made to make this flexible, but for best use this should be "rdk" and
# only be changed for interactive use.
PYENV_REF <- "rdk"

# Set the python version from which to build the environment.
USE_PY_VER <- 3.9

# Set the option to install the python environment. Must be one of "conda", 
# "git", or "local", with "local" being recommended.
#  ** GIT IS NOT SUPPORTED IN THIS VERSION FOR AUTOMATIC CONFIGURATION **
INSTALL_FROM <- "local"

# [REQUIRED IF INSTALL_FROM == "local"] Set the file from which the python
# environment will be built.
# INSTALL_FROM_FILE <- file.path(
#   "rdkit",
#   switch(.Platform$OS.type,
#          "windows" = "environment_windows.yml",
#          "unix"    = "environment_ubuntu.yml")
# )
INSTALL_FROM_FILE <- file.path("rdkit", "environment.yml")

# [OPTIONAL] -------------------------------------------------------------------
# [ADVANCED] Set required conda libraries to install, depending on it to define
# its dependencies. This will be used if INSTALL_FROM = "conda". Note that the
# r-reticulate python package will automatically be added to this list during
# installation.
PYENV_LIBRARIES <- c("rdkit=2021.09.4", "r-reticulate=1.24")
PYENV_MODULES <- "rdkit"
# Channel from which to install packages.
PYENV_CHANNELS <- "conda-forge"

# [ADVANCED] -------------------------------------------------------------------
# Set the path for advanced conda setups.
CONDA_PATH <- "auto"
# CONDA_PATH <- "~/miniforge3/bin/conda" # Example

RENV_ESTABLISHED_RDKIT <- TRUE
