# [REQUIRED]--------------------------------------------------------------------
# The name of the python environment to use. If this environment exists on the
# current system, it will be used, otherwise one will be created with this name.
PYENV_NAME <- "nist_hrms_db"

# The name of the R object to use for importing the rdkit module. Every effort
# has been made to make this flexible, but for best use this should be "rdk" and
# only be changed for interactive use.
PYENV_REF <- "rdk"

# The name of the modules for installation from conda. This should always include
# "rdkit"; if additional modules or libraries are needed, list them here as a
# character vector.PYENV_MODULE <- "rdkit"

# Set the minimum python version from which to build the environment.
MIN_PY_VER <- 3.9

# Set the option to install the python environment.
# Must be one of "conda", "git", or "local" 
#  ** GIT IS NOT SUPPORTED IN THIS VERSION FOR AUTOMATIC CONFIGURATION **
INSTALL_FROM <- "conda"

# [REQUIRED IF INSTALL_FROM == "local"] Set the file from which the python
# environment will be built.
# ------------------------------------------------------------------------------
INSTALL_FROM_FILE <- file.path("rdkit", "environment.yml")
# ------------------------------------------------------------------------------

# [OPTIONAL, ADVANCED, COMMAND LINE BUILD] Set the main conda packages
# to install, depending on it to define its dependencies. This will be used if
# INSTALL_FROM = "conda"
# ------------------------------------------------------------------------------
CONDA_MODULES <- c("r-reticulate", "rdkit")
# Channel from which to install packages
CONDA_CHANNELS <- "conda-forge"
# ------------------------------------------------------------------------------

# [OPTIONAL, ADVANCED] set the local path for advanced conda setups
# ------------------------------------------------------------------------------
CONDA_PATH <- "auto"
# CONDA_PATH = "~/miniforge3/bin/conda'
# The command line interface alias for your installation of conda.
CONDA_CLI <- "conda"
# ------------------------------------------------------------------------------
