mod_uncertainty_evaluation <- function(msn, advanced_use) {
  modalDialog(
    title = "Uncertainty Evaluation",
    size = "m",
    easyClose = TRUE, 
    DTOutput(outputId = "mod_uncertainty_match_dt",
             width = "100%"),
    p("Set the number of bootstrap iterations to run and click the button below to estimate the confidence in your match score."),
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
                                                value = uncertainty_mass_error_compare_actual$value,
                                                min = uncertainty_mass_error_compare_actual$min,
                                                max = uncertainty_mass_error_compare_actual$max,
                                                step = uncertainty_mass_error_compare_actual$step
                                   ),
                                   numericInput(inputId = "mod_uncertainty_min_error_compare_actual",
                                                label = "Minimum mass error (this spectrum)",
                                                width = "100%",
                                                # ticks = uncertainty_min_error_compare_actual$ticks,
                                                value = uncertainty_min_error_compare_actual$value,
                                                min = uncertainty_min_error_compare_actual$min,
                                                max = uncertainty_min_error_compare_actual$max,
                                                step = uncertainty_min_error_compare_actual$step
                                   ),
                                   sliderInput(inputId = "mod_uncertainty_weighting_mass",
                                               label = "Minimum mass error (this spectrum)",
                                               width = "100%",
                                               ticks = uncertainty_weighting_mass$ticks,
                                               value = uncertainty_weighting_mass$value,
                                               min = uncertainty_weighting_mass$min,
                                               max = uncertainty_weighting_mass$max,
                                               step = uncertainty_weighting_mass$step
                                   )
                            ),
                            column(6,
                                   numericInput(inputId = "mod_uncertainty_mass_error_compare_with",
                                                label = "Tolerable ppm error (reference spectra)",
                                                width = "100%",
                                                # ticks = uncertainty_mass_error_compare_with$ticks,
                                                value = uncertainty_mass_error_compare_with$value,
                                                min = uncertainty_mass_error_compare_with$min,
                                                max = uncertainty_mass_error_compare_with$max,
                                                step = uncertainty_mass_error_compare_with$step
                                   ),
                                   numericInput(inputId = "mod_uncertainty_min_error_compare_with",
                                                label = "Minimum mass error (reference spectra)",
                                                width = "100%",
                                                # ticks = uncertainty_min_error_compare_with$ticks,
                                                value = uncertainty_min_error_compare_with$value,
                                                min = uncertainty_min_error_compare_with$min,
                                                max = uncertainty_min_error_compare_with$max,
                                                step = uncertainty_min_error_compare_with$step
                                   ),
                                   sliderInput(inputId = "mod_uncertainty_weighting_intensity",
                                               label = "Minimum mass error (reference spectra)",
                                               width = "100%",
                                               ticks = uncertainty_weighting_intensity$ticks,
                                               value = uncertainty_weighting_intensity$value,
                                               min = uncertainty_weighting_intensity$min,
                                               max = uncertainty_weighting_intensity$max,
                                               step = uncertainty_weighting_intensity$step
                                   )
                            )
                          )
                      )
                    )
               )
           )
      )
    },
    sliderTextInput(inputId = "mod_uncertainty_iterations",
                    label = "Bootstrap Iterations",
                    selected = as.character(app_settings$uncertainty_bootstrap_iterations$selected),
                    width = "100%",
                    force_edges = TRUE,
                    grid = FALSE,
                    choices = prettyNum(app_settings$uncertainty_bootstrap_iterations$choices, big.mark = ",")),
    actionButton(inputId = "mod_uncertainty_calculate",
                 label = "Calculate Uncertainty",
                 icon = icon("arrows-left-right-to-line", verify_fa = FALSE),
                 width = "100%"),
    hr(),
    shinyWidgets::radioGroupButtons(
      inputId = "mod_uncertainty_msn",
      width = "100%",
      label = NULL,
      choices = c("MS1", "MS2"),
      size = "xs",
      selected = msn,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    ),
    div(id = "mod_uncertainty_results",
        htmlOutput(outputId = "mod_uncertainty_narrative"),
        plotOutput(outputId = "mod_uncertainty_boxplot",
                   width = "100%") %>%
          withSpinner()
    ),
    modalButton("Close",
                icon = icon("close", verify_fa = FALSE)),
    footer = NULL
  )
}