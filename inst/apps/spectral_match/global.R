dev <- TRUE
advanced_use <- FALSE
toy_data <- TRUE
vowels <- c("a", "e", "i", "o", "u")
vowels <- c(vowels, toupper(vowels))
APP_TITLE <- "NIST PFAS Database Spectra Match"
app_dir <- "spectral_match"
if (!exists("RENV_ESTABLISHED_SHINY") || !RENV_ESTABLISHED_SHINY) source(here::here("inst", "apps", "env_shiny.R"))
DB_TITLE <- rectify_null_from_env("DB_TITLE", DB_TITLE, "NIST HRAMS Database for PFAS")
need_files <- c(
  "app_functions.R",
  list.files(path = here::here("R", c("spectral_analysis")),
             pattern = "\\.R$",
             full.names = TRUE)
)
sapply(need_files, source)
log_ns <- "APP_SPECTRAL_MATCH"
log_it("info", "Starting app: app_spectral_match", "shiny")
log_it("info", "Starting app", tolower(log_ns), add_unknown_ns = TRUE, clone_settings_from = "SHINY")

app_settings <- list(
  experiment_types = api_endpoint(path = "table_search",
                                  query = list(table_name = "norm_ms_n_types"),
                                  return_format = "data.frame") %>%
    filter(!name == "none", !is.na(name), !name == "") %>%
    mutate(display = glue::glue("{acronym} ({name})")) %>%
    with(.,
         setNames(acronym, display)),
  data_input_import_file_types = c(".mzML"),
  data_input_import_search_settings_types = c(".csv", ".xls", ".xlsx"),
  data_input_relative_error = list(value = 5, min = 0.1, max = 50, step = 0.1),
  data_input_minimum_error = list(value = 0.002, min = 0.0001, max = 0.5, step = 0.0001),
  data_input_isolation_width = list(value = 0.7, min = 0.1, max = 100, step = 0.1),
  data_input_isolation_width_warn_threshold = 4,
  data_input_search_zoom = list(value = c(1, 4), min = 0, max = 10, step = 0.1, ticks = FALSE),
  data_input_correlation = list(value = 0.5, min = 0, max = 1, step = 0.1, ticks = FALSE),
  data_input_ph = list(value = 10, min = 0, max = 100, step = 1, ticks = FALSE),
  data_input_freq = list(value = 10, min = 3, max = 15, step = 1, ticks = FALSE),
  data_input_normfn = c("sum", "mean"),
  data_input_cormethod = c("pearson"),
  # data_input_max_correl = list(value = 0.5, min = 0, max = 1, step = 0.1, ticks = FALSE),
  # data_input_correl_bin = list(value = 0.1, min = 0, max = 1, step = 0.1, ticks = FALSE),
  # data_input_max_ph = list(value = 10, min = 0, max = 100, step = 1, ticks = FALSE),
  # data_input_ph_bin = list(value = 1, min = 0, max = 100, step = 1, ticks = FALSE),
  # data_input_max_freq = list(value = 10, min = 3, max = 15, step = 1, ticks = FALSE),
  # data_input_freq_bin = list(value = 1, min = 1, max = 10, step = 1, ticks = FALSE),
  # data_input_min_n_peaks = list(value = 4, min = 3, max = 15, step = 1, ticks = FALSE),
  uncertainty_mass_error_compare_actual = list(value = 5, min = 0.1, max = 50, step = 0.1),
  uncertainty_mass_error_compare_with = list(value = 5, min = 0.1, max = 50, step = 0.1),
  uncertainty_min_error_compare_actual = list(value = 0.002, min = 0.0001, max = 0.5, step = 0.0001),
  uncertainty_min_error_compare_with = list(value = 0.002, min = 0.0001, max = 0.5, step = 0.0001),
  uncertainty_weighting_mass = list(value = 1, min = 0.1, max = 1, step = 0.1, ticks = FALSE),
  uncertainty_weighting_intensity = list(value = 0.5, min = 0.1, max = 1, step = 0.1, ticks = FALSE),
  uncertainty_bootstrap_iterations = list(choices = c(10, 50, 100, 500, 1000, 5000, 10000), selected = 100)
)

rdkit_available <- api_endpoint(path = "rdkit_active")

lapply(list.files("modals", pattern = ".R", full.names = TRUE),
       source)

jscode <- HTML("
$('body').on('shown.bs.modal', (x) =>
  $(x.target).find('input[type=\"number\"]:first').focus())
               ")

# Increase the file upload size to 250 MB
file_MB_limit <- 250
options(shiny.maxRequestSize = file_MB_limit*1024^2)