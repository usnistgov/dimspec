# Set the basics for display and identification for this application here, such
# as the page name, logging namespace, and database title.
APP_TITLE          <- "NIST MS Data Quality Checker"
app_name           <- basename(getwd())
app_dir            <- file.path(app_name)
app_ns             <- paste0("app_", app_name)
default_title      <- "NIST MS Data Quality Checker for PFAS"

# Set to true to enable development mode, which includes a link to the
# underlying API documentation and a live inspection button to see the app's
# current state in the console.
dev                <- TRUE

# Set the start options to use advanced settings and tooltips by default. These
# can be changed while using the app at any time if the "enable" options are set
# to TRUE, otherwise they will honor the individual settings.
#enable_adv_use     <- TRUE
#advanced_use       <- FALSE
#enable_more_help   <- TRUE
#provide_more_help  <- FALSE
#tooltip_text       <- readr::read_csv(file.path("www", "tooltip_text.csv"))[ ,c("element_id", "tooltip_text")]
#tooltip_text       <- setNames(object = tooltip_text$tooltip_text, nm = tooltip_text$element_id)

# If using dev mode, automatically fill with example data from local RDS files.
toy_data           <- FALSE
src_toy_data       <- "toy_data.RDS"
src_toy_parameters <- "toy_parameters.RDS"

# The following settings are necessary for the application. Only change these if
# it is required (e.g. to include other source files that you want to use).
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
if (!exists("RENV_ESTABLISHED_SHINY") || !RENV_ESTABLISHED_SHINY) source(here::here("inst", "apps", "env_shiny.R"))
need_files <- c(
  here::here("R", "tidy_spectra.R"),
  list.files(path = here::here("R", c("spectral_analysis", "base", "gather", "misc")),
             pattern = "\\.R$",
             full.names = TRUE)
)
sapply(need_files, source, keep.source = FALSE)

# Load all modals in this directory
lapply(list.files("modals", pattern = ".R", full.names = TRUE),
       source,
       keep.source = FALSE)

# Set up logging options for this application. These again should only be
# changed to meet the needs of the application.
if (LOGGING_ON) {
  shiny_ns <- "shiny"
  if (!shiny_ns %in% names(LOGGING)) {
    log_it("info", sprintf("Creating logging namespace for '%s'", shiny_ns), shiny_ns, add_unknown_ns = TRUE)
  }
  log_ns <- toupper(app_ns)
  if (!log_ns %in% names(LOGGING)) {
    log_it("info", sprintf("Creating logging namespace for '%s'", app_ns), shiny_ns, add_unknown_ns = TRUE, clone_settings_from = toupper(shiny_ns))
  }
  log_it("info", sprintf("Starting app: %s", app_name), shiny_ns)
  log_it("info", sprintf("Starting app: %s", app_name), app_ns, add_unknown_ns = TRUE, clone_settings_from = toupper(shiny_ns))
}

# Override the database title from set up files once environment is established.
DB_TITLE           <- rectify_null_from_env("DB_TITLE", DB_TITLE, default_title)

# Add any settings for controls within the application. These will be used to
# populate controls in the UI at run time.
app_settings <- list(
  experiment_types = list(
    choices = api_endpoint(path = "table_search",
                           query = list(table_name = "norm_ms_n_types"),
                           return_format = "data.frame") %>%
      filter(!name == "none", !is.na(name), !name == "") %>%
      arrange(acronym) %>%
      mutate(display = glue::glue("{acronym} ({name})")) %>%
      with(.,
           setNames(acronym, display))
  ),
  rawdata_import_file_types = c(".mzML"),
  methodjson_import_file_types = c(".JSON")
)

# Check that rdkit is available in the API so the application can decide whether
# to display certain elements that require rdkit, or to hide them.
rdkit_available <- api_endpoint(path = "rdkit_active")

# Add any javascript queries that must run on the page itself.
jscode <- HTML("
$('body').on('shown.bs.modal', (x) =>
  $(x.target).find('input[type=\"number\"]:first').focus());
               ")

# Set the file upload size t(e.g. to 250 MB)
file_MB_limit <- 250
options(shiny.maxRequestSize = file_MB_limit*1024^2)