# Set the basics for display and identification for this application here, such
# as the page name, logging namespace, and database title.
APP_TITLE          <- "[APP_TITLE]"
app_name           <- basename(getwd())
app_dir            <- file.path(app_name)
app_ns             <- paste0("app_", app_name)
default_title      <- "[DIMSpec App Template]"
# Set to true to enable development mode, which includes a link to the
# underlying API documentation and a live inspection button to see the app's
# current state in the console.
dev <- TRUE
# Set to true in order to base this app on the API defined in the environment files.
USE_API            <- TRUE
# Set to whether or not this needs an rdkit integration
APP_RDKIT          <- FALSE
# The following settings are necessary for the application. Only change these if
# it is required (e.g. to include other source files that you want to use).
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
if (!exists("RENV_ESTABLISHED_SHINY") || !RENV_ESTABLISHED_SHINY) source(here::here("inst", "apps", "env_shiny.R"))
DB_TITLE <- rectify_null_from_env("DB_TITLE", DB_TITLE, "[DATABASE TITLE HERE]")

# Set need_files to those necessary for your app to function
need_files <- c(
  "app_functions.R",
  here::here("inst", "apps", "shiny_helpers.R")
)
sapply(need_files, source, keep.source = FALSE)

# To activate logging, uncomment and set these as necessary
# log_ns <- "APP_[NAME]"
# log_it("info", sprintf("Starting app: app_%s", app_name), "shiny")
# log_it("info", "Starting app", tolower(log_ns), add_unknown_ns = TRUE, clone_settings_from = "SHINY")

# Add any app wide default settings to reference when populating input values
app_settings <- list()


lapply(list.files("modals", pattern = ".R", full.names = TRUE), source)

# Add any customized javascript coding,
# e.g. to activate the first number type input when a modal is shown use
# jscode <- HTML("
# $('body').on('shown.bs.modal', (x) =>
#   $(x.target).find('input[type=\"number\"]:first').focus())
#                ")
jscode <- HTML("")

# Set file upload size (here it is set to 25 MB)
file_MB_limit <- 25
options(shiny.maxRequestSize = file_MB_limit*1024^2)