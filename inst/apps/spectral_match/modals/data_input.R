mod_data_input_parameters_manual <- function() {
  modalDialog(
    title = "Peak search parameters",
    size = "s",
    easyClose = FALSE,
    numericInput(inputId = "mod_search_parameter_precursor",
                 label = "Precursor m/z",
                 value = NULL,
                 min = 0,
                 max = 1e5,
                 width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_search_parameter_precursor"]]),
    numericInput(inputId = "mod_search_parameter_rt",
                 label = "Retention Time (Centroid)",
                 value = NULL,
                 min = 0,
                 max = 1e5,
                 width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_search_parameter_rt"]]),
    numericInput(inputId = "mod_search_parameter_rt_start",
                 label = "Retention Time (Start)",
                 value = NULL,
                 min = 0,
                 max = 1e5,
                 width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_search_parameter_rt_start"]]),
    numericInput(inputId = "mod_search_parameter_rt_end",
                 label = "Retention Time (End)",
                 value = NULL,
                 min = 0,
                 max = 1e5,
                 width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_search_parameter_rt_end"]]),
    div(class = "flex-container",
        actionButton(inputId = "mod_data_input_search_parameter_save",
                     label = "Save Parameters",
                     icon = icon("save")) %>%
          with_help(tooltip = tooltip_text[["mod_data_input_search_parameter_save"]]),
        actionButton(inputId = "mod_search_parameter_cancel",
                     label = "Cancel",
                     icon = icon("close", verify_fa = FALSE)) %>%
          with_help(tooltip = tooltip_text[["mod_search_parameter_cancel"]])
    ),
    footer = NULL
  )
}

mod_data_input_parameters_upload <- function(data) {
  modalDialog(
    title = "Import peak search parameters",
    size = "s",
    easyClose = FALSE,
    tags$caption("Select one column from your file corresponding to each of the following parameters."),
    hr(),
    selectizeInput(inputId = "mod_upload_parameter_precursor",
                   label = "Precursor m/z",
                   choices = names(data),
                   selected = names(data)[1],
                   multiple = FALSE,
                   width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_upload_parameter_precursor"]]),
    selectizeInput(inputId = "mod_upload_parameter_rt",
                   label = "Retention Time (Centroid)",
                   choices = names(data),
                   selected = names(data)[2],
                   multiple = FALSE,
                   width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_upload_parameter_rt"]]),
    selectizeInput(inputId = "mod_upload_parameter_rt_start",
                   label = "Retention Time (Start)",
                   choices = names(data),
                   selected = names(data)[3],
                   multiple = FALSE,
                   width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_upload_parameter_rt_start"]]),
    selectizeInput(inputId = "mod_upload_parameter_rt_end",
                   label = "Retention Time (End)",
                   choices = names(data),
                   selected = names(data)[4],
                   multiple = FALSE,
                   width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_upload_parameter_rt_end"]]),
    checkboxInput(inputId = "mod_upload_parameter_append",
                  label = "Append to the current parameter list",
                  value = TRUE) %>%
      with_help(tooltip = tooltip_text[["mod_upload_parameter_append"]]),
    div(class = "flex-container",
        actionButton(inputId = "mod_data_input_upload_parameter_save",
                     label = "Load Parameters",
                     icon = icon("save")) %>%
          with_help(tooltip = tooltip_text[["mod_data_input_upload_parameter_save"]]),
        actionButton(inputId = "mod_upload_parameter_cancel",
                     label = "Cancel",
                     icon = icon("close", verify_fa = FALSE)) %>%
          with_help(tooltip = tooltip_text[["mod_upload_parameter_cancel"]])
    ),
    footer = NULL
  )
}
