mod_uncertainty_evaluation <- function(msn, advanced_use) {
  modalDialog(
    title = "Uncertainty Evaluation",
    size = "m",
    easyClose = TRUE, 
    DTOutput(outputId = "mod_uncertainty_match_dt",
             width = "100%"),
    if (advanced_use) {
      span(id = "mod_uncertainty_additional",
           box(title = tagList(icon("screwdriver-wrench", verify_fa = FALSE),
                               "Advanced parameters"),
               width = 12,
               solidHeader = FALSE,
               status = "primary",
               class = "left",
               collapsible = TRUE,
               collapsed = TRUE,
               with(app_settings,
                    tagList(
                      h4(style = "color: #3571a5;", "Fine tuning of search parameters should only be done under expert advice."),
                      div(class = "form-grouping",
                          fluidRow(
                            column(6,
                                   numericInput(inputId = "mod_uncertainty_mass_error_compare_actual",
                                                label = "Tolerable ppm error (this spectrum)",
                                                width = "100%",
                                                # ticks = uncertainty_mass_error_compare_actual$ticks,
                                                value = uncertainty_mass_error_compare_actual$this_value,
                                                min = uncertainty_mass_error_compare_actual$this_min,
                                                max = uncertainty_mass_error_compare_actual$this_max,
                                                step = uncertainty_mass_error_compare_actual$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_mass_error_compare_actual"]]),
                                   numericInput(inputId = "mod_uncertainty_min_error_compare_actual",
                                                label = "Minimum mass error (this spectrum)",
                                                width = "100%",
                                                # ticks = uncertainty_min_error_compare_actual$ticks,
                                                value = uncertainty_min_error_compare_actual$this_value,
                                                min = uncertainty_min_error_compare_actual$this_min,
                                                max = uncertainty_min_error_compare_actual$this_max,
                                                step = uncertainty_min_error_compare_actual$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_min_error_compare_actual"]]),
                                   sliderInput(inputId = "mod_uncertainty_weighting_mass",
                                               label = "Uncertainty weighting mass",
                                               width = "100%",
                                               ticks = uncertainty_weighting_mass$ticks,
                                               value = uncertainty_weighting_mass$this_value,
                                               min = uncertainty_weighting_mass$this_min,
                                               max = uncertainty_weighting_mass$this_max,
                                               step = uncertainty_weighting_mass$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_weighting_mass"]])
                            ),
                            column(6,
                                   numericInput(inputId = "mod_uncertainty_mass_error_compare_with",
                                                label = "Tolerable mass error for comparison against reference spectra (ppm)",
                                                width = "100%",
                                                # ticks = uncertainty_mass_error_compare_with$ticks,
                                                value = uncertainty_mass_error_compare_with$this_value,
                                                min = uncertainty_mass_error_compare_with$this_min,
                                                max = uncertainty_mass_error_compare_with$this_max,
                                                step = uncertainty_mass_error_compare_with$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_mass_error_compare_with"]]),
                                   numericInput(inputId = "mod_uncertainty_min_error_compare_with",
                                                label = "Minimum mass error for comparison against reference spectra (Da)",
                                                width = "100%",
                                                # ticks = uncertainty_min_error_compare_with$ticks,
                                                value = uncertainty_min_error_compare_with$this_value,
                                                min = uncertainty_min_error_compare_with$this_min,
                                                max = uncertainty_min_error_compare_with$this_max,
                                                step = uncertainty_min_error_compare_with$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_min_error_compare_with"]]),
                                   sliderInput(inputId = "mod_uncertainty_weighting_intensity",
                                               label = "Uncertainty Weighting Intensity",
                                               width = "100%",
                                               ticks = uncertainty_weighting_intensity$ticks,
                                               value = uncertainty_weighting_intensity$this_value,
                                               min = uncertainty_weighting_intensity$this_min,
                                               max = uncertainty_weighting_intensity$this_max,
                                               step = uncertainty_weighting_intensity$this_step
                                   ) %>%
                                     with_help(tooltip = tooltip_text[["mod_uncertainty_weighting_intensity"]])
                            )
                          )
                      )
                    )
               )
           )
      )
    },
    hr(),
    shinyWidgets::radioGroupButtons(
      inputId = "mod_uncertainty_msn",
      width = "100%",
      label = "Spectral level",
      size = "xs",
      choices = c("MS1", "MS2"),
      selected = msn,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    ) %>%
      with_help(tooltip = tooltip_text[["mod_uncertainty_msn"]]),
    sliderTextInput(inputId = "mod_uncertainty_iterations",
                    label = "Bootstrap Iterations",
                    selected = as.character(app_settings$uncertainty_bootstrap_iterations$selected),
                    width = "100%",
                    force_edges = TRUE,
                    grid = FALSE,
                    choices = prettyNum(app_settings$uncertainty_bootstrap_iterations$choices, big.mark = ",")) %>%
      with_help(tooltip = tooltip_text[["mod_uncertainty_iterations"]]),
    actionButton(inputId = "mod_uncertainty_calculate",
                 label = "Calculate Uncertainty",
                 icon = icon("arrows-left-right-to-line", verify_fa = FALSE),
                 width = "100%") %>%
      with_help(tooltip = tooltip_text[["mod_uncertainty_calculate"]]),
    div(id = "mod_uncertainty_results",
        hr(),
        checkboxInput(inputId = "mod_uncertainty_include_nan",
                      label = "Include out of bounds estimates as 0.",
                      value = FALSE),
        htmlOutput(outputId = "mod_uncertainty_narrative"),
        hr(),
        plotOutput(outputId = "mod_uncertainty_boxplot",
                   width = "100%") %>%
          withSpinner()
    ),
    div(class = "flex-container",
        actionButton(inputId = "mod_uncertainty_close",
                     label = "Close",
                     width = "100%",
                     icon = icon("close", verify_fa = FALSE)) %>%
          with_help(tooltip = tooltip_text[["mod_uncertainty_close"]],
                    placement = "top")
    ),
    footer = NULL
  )
}
