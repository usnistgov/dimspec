dev <- TRUE
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
APP_TITLE <- "[APP_TITLE]"
# app_dir <- "app_dir"
if (!exists("RENV_ESTABLISHED_SHINY") || !RENV_ESTABLISHED_SHINY) source(here::here("inst", "apps", "env_shiny.R"))
DB_TITLE <- rectify_null_from_env("DB_TITLE", DB_TITLE, "[DISPLAY TITLE HERE]")

# Set need_files to those necessary for your app to function
need_files <- c(
  "app_functions.R"
)
sapply(need_files, source, keep.source = FALSE)

# To activate logging, uncomment and set these as necessary
# log_ns <- "APP_[NAME]"
# log_it("info", "Starting app: app_[name]", "shiny")
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