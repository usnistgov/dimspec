# # The name of the python environment to use.
# # PYENV_NAME     = "test"
# 
# # Name of the RDKit module for installation from conda. This should always
# # include "rdkit"; if additional modules or libraries are needed, list them here
# # as a character vector.
# # PYENV_MODULE   = "rdkit"
# 
# # The name of the R object to use for importing the rdkit module.
# # PYENV_REF      = "rdk"

# [REQUIRED] Set to determine whether autoconfiguration should be attempted.
# If you already have a python environment established, set this to FALSE.
# If FALSE, the value of USE_PY_ENV will be used as your reticulate environment.
# TRY_AUTOCONFIG    = TRUE

# [REQUIRED FOR AUTOCONFIG] This is the desired python environment name. 
#   Default: "icpmsflow"
# ------------------------------------------------------------------------------
USE_PY_ENV        = "rdkit"
# ------------------------------------------------------------------------------

# [REQUIRED FOR AUTOCONFIG] Set the minimum python version from which to build 
#   the environment.
# ------------------------------------------------------------------------------
MIN_PY_VER        = 3.9
# ------------------------------------------------------------------------------

# [REQUIRED FOR AUTOCONFIG] Set the option to install the python environment. 
#   Must be one of "conda", "git", or "local"
# ** ONLY CONDA IS SUPPORTED IN THIS VERSION **
# ------------------------------------------------------------------------------
INSTALL_FROM      = "conda"
# ------------------------------------------------------------------------------

# [OPTIONAL] Set the main conda packages to install, depending on it to define 
#   its dependencies. This will be used if INSTALL_FROM = "conda"
# ------------------------------------------------------------------------------
CONDA_PACKAGE     = "rdkit"
CONDA_CHANNELS    = "conda-forge"
# ------------------------------------------------------------------------------

# [OPTIONAL, ADVANCED] set the local path for advanced conda setups
# ------------------------------------------------------------------------------
CONDA_PATH        = "auto"
# CONDA_PATH = "~/miniforge3/bin/conda'
# ------------------------------------------------------------------------------

# These will be used if INSTALL_FROM = "git"
# [OPTIONAL] Set the source git repository for this project. If using a fork, 
#   change to the correct repo from which to build.
# ------------------------------------------------------------------------------
# GIT_SOURCE        = "wpk-nist-gov/icpmsflow"
# ------------------------------------------------------------------------------
# [OPTIONAL] Set the branch from which to build the project. Ensure the @ 
#   prefix is present.
# ------------------------------------------------------------------------------
# GIT_BRANCH        = "@develop"
# ------------------------------------------------------------------------------
# [OPTIONAL] Set additional packages to install to support a git branch. These 
#   will be installed via conda.
# ------------------------------------------------------------------------------
# [DEFAULT VALUE - comment to customize]
MIN_PACKAGES      = "numpy pandas rdkit"
# ------------------------------------------------------------------------------

# [OPTIONAL] Set the file from which the python environment will be built.
#   This will be used if INSTALL_FROM = "local"
# ------------------------------------------------------------------------------
# INSTALL_FROM_FILE = "environment.yaml"
# ------------------------------------------------------------------------------
