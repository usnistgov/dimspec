dashboardPage(
  skin = "blue",
  title = paste0(DB_TITLE, " - ", APP_TITLE),
  header = dashboardHeader(
    title = a(
      img(src = "NIST-Logo-Brand-White.svg"),
      href = "https://www.nist.gov/programs-projects/measurement-science-and-polyfluoroalkyl-substances-pfas#:~:text=Overview%20of%20the%20NIST%20program%20on%20per-%20and,for%20a%20variety%20of%20commercial%20and%20industrial%20applications.",
      target = "_blank"
    )
  ),
  sidebar = dashboardSidebar(
    h4(style = "padding-left: 15px;", APP_TITLE),
    if (dev) {
      div(style = "padding-left: 15px;",
          h4(id = "dev_mode", style = "padding-right: 15px; color: red; text-align: center;", "DEVELOPMENT MODE"),
          p("Plumber instance is live at ", PLUMBER_URL, "; view the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), target = "_blank", "here")),
          actionButton("browser", "Live Inspect", icon = icon("user-ninja"))
      )
    } else {
      NULL
    },
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(span(id = "nav_index", "Home"),
               tabName = "index",
               icon = icon("house", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = tooltip_text[["nav_index"]],
                  placement = "right"),
      menuItem(span(id = "nav_data_input", "Data Input"),
               tabName = "data_input",
               icon = icon("file")
      ) %>%
        with_help(tooltip = tooltip_text[["nav_data_input"]],
                  placement = "right"),
      menuItem(span(id = "nav_search_compounds", "Compound Match"),
               tabName = "search_compounds",
               icon = icon("magnifying-glass", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = tooltip_text[["nav_search_compounds"]],
                  placement = "right"),
      menuItem(span(id = "nav_fragments", "Fragment Match"),
               tabName = "search_fragments",
               icon = icon("puzzle-piece", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = tooltip_text[["nav_fragments"]],
                  placement = "right"),
      menuItem(span(id = "nav_about", "About"),
               tabName = "about",
               icon = icon("circle-info", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = tooltip_text[["nav_about"]],
                  placement = "right"),
      span(id = "additional_options",
           if (support_excel_downloads) {
             actionButton(inputId = "nav_download_all",
                          label = "Download All Results",
                          icon = icon("download")
             ) %>%
               with_help(tooltip = tooltip_text[["nav_download_all"]],
                         placement = "top")
           },
           div(id = "nav_show_help_div",
               class = "nav-checkbox-right",
               icon("question", verify_fa = FALSE),
               tags$label(id = "nav_show_help_label", "Show Tooltips") %>%
                 with_help(tooltip = tooltip_text[["nav_show_help_label"]],
                           placement = "top"),
               prettySwitch(inputId = "nav_show_help",
                            label = NULL,
                            value = provide_more_help,
                            inline = TRUE,
                            status = "success")
           ),
           div(id = "nav_show_advanced_settings_div",
               class = "nav-checkbox-right",
               icon("gear", verify_fa = FALSE),
               tags$label(id = "nav_show_advanced_settings_label", "Advanced Settings") %>%
                 with_help(tooltip = tooltip_text[["nav_show_advanced_settings_label"]],
                           placement = "top"),
               prettySwitch(inputId = "nav_show_advanced_settings",
                            label = NULL,
                            value = advanced_use,
                            inline = TRUE,
                            status = "warning")
           )
      )
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
    tags$script(type = "text/javascript", jscode),
    tabItems(
      # Home Page ----
      tabItem("index",
              actionButton(inputId = "index_go_data_input",
                           label = "Click Here to Get Started",
                           width = "100%",
                           icon = icon("circle-play", verify_fa = FALSE)
              ) %>%
                with_help(tooltip = tooltip_text[["index_go_data_input"]]),
              includeHTML("index.html"),
              hr()
      ),
      # Data Input ----
      tabItem("data_input",
              div(class = "overlay",
                  id = "data_input_overlay",
                  img(src = "processing.gif"),
                  h3(id = "data_input_overlay_text")
              ),
              fluidRow(
                column(4,
                       style = "padding: 0; margin: 0",
                       box(title = tagList(icon("file-contract", verify_fa = FALSE),
                                           "Load Data"),
                           width = 12,
                           solidHeader = FALSE,
                           collapsible = FALSE,
                           collapsed = FALSE,
                           status = "primary",
                           class = "left",
                           with(app_settings,
                                tagList(
                                  tags$label(id = "data_input_filename_label",
                                             glue::glue("Choose a data file ({format_list_of_names(data_input_import_file_types)})")) %>%
                                    with_help(tooltip = tooltip_text[["data_input_filename_label"]]),
                                  div(class = "form-grouping",
                                      fileInput(inputId = "data_input_filename",
                                                label = NULL,
                                                width = "100%",
                                                multiple = FALSE,
                                                accept = data_input_import_file_types,
                                                buttonLabel = span(id = "data_input_filename_load_btn", "Load"),
                                                placeholder = "Select a file to begin"
                                      )
                                  ),
                                  tags$label("Set Instrument Parameters"),
                                  div(class = "form-grouping",
                                      selectizeInput(inputId = "data_input_experiment_type",
                                                     label = "MS Experiment Type",
                                                     choices = experiment_types$choices,
                                                     width = "100%"
                                      ) %>%
                                        with_help(tooltip = tooltip_text[["data_input_experiment_type"]]),
                                      with(data_input_relative_error,
                                           numericInput(inputId = "data_input_relative_error",
                                                        label = "Relative Error (ppm)",
                                                        width = "100%",
                                                        value = this_value,
                                                        min = this_min,
                                                        max = this_max,
                                                        step = this_step
                                           ) %>%
                                             with_help(tooltip = glue::glue(tooltip_text[["data_input_relative_error"]]))
                                      ),
                                      with(data_input_minimum_error,
                                           numericInput(inputId = "data_input_minimum_error",
                                                        label = "Minimum Error (Da)",
                                                        width = "100%",
                                                        value = this_value,
                                                        min = this_min,
                                                        max = this_max,
                                                        step = this_step
                                           ) %>%
                                             with_help(tooltip = glue::glue(tooltip_text[["data_input_minimum_error"]]))
                                      ),
                                      with(data_input_isolation_width,
                                           numericInput(inputId = "data_input_isolation_width",
                                                        label = "Isolation Width (Da)",
                                                        width = "100%",
                                                        value = this_value,
                                                        min = this_min,
                                                        max = this_max,
                                                        step = this_step
                                           ) %>%
                                             with_help(tooltip = glue::glue(tooltip_text[["data_input_isolation_width"]]))
                                      )
                                  ),
                                  div(class = "form-grouping",
                                      checkboxInput(inputId = "data_input_is_waters",
                                                    label = "Data were collected with an instrumental lock mass",
                                                    value = FALSE,
                                                    width = "100%"
                                      ) %>%
                                        with_help(tooltip = glue::glue(tooltip_text[["data_input_is_waters"]])),
                                      div(id = "data_input_waters_settings",
                                          numericInput(inputId = "data_input_waters_lockmass",
                                                       label = "Lock mass (Da)",
                                                       value = data_input_waters_lockmass,
                                                       width = "100%"
                                          ) %>%
                                            with_help(tooltip = glue::glue(tooltip_text[["data_input_waters_lockmass"]])),
                                          numericInput(inputId = "data_input_waters_lockmass_width",
                                                       label = "Lock mass width (Da)",
                                                       value = data_input_waters_lockmass_width,
                                                       width = "100%"
                                          ) %>%
                                            with_help(tooltip = glue::glue(tooltip_text[["data_input_waters_lockmass_width"]])),
                                          checkboxInput(inputId = "data_input_waters_lockmass_correct",
                                                        label = "Measurements are lock mass corrected",
                                                        value = data_input_waters_lockmass_correct
                                          ) %>%
                                            with_help(tooltip = glue::glue(tooltip_text[["data_input_waters_lockmass_correct"]]))
                                      )
                                  )
                                )
                           )
                       )
                ),
                box(title = tagList(icon("magnifying-glass-location", verify_fa = FALSE),
                                    "Feature Identification"),
                    width = 8,
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    status = "primary",
                    class = "right",
                    tags$label(id = "data_input_parameters",
                               "Select parameters identifying peaks to examine.") %>%
                      with_help(tooltip = tooltip_text[["data_input_parameters"]]),
                    div(class = "flex-container",
                        actionButton(inputId = "data_input_dt_peak_list_add_row",
                                     label = "Add",
                                     width = "100%",
                                     icon = icon("plus")
                        ) %>%
                          with_help(tooltip = tooltip_text[["data_input_dt_peak_list_add_row"]]),
                        actionButton(inputId = "data_input_dt_peak_list_edit_row",
                                     label = "Edit",
                                     width = "100%",
                                     icon = icon("pencil-alt", verify_fa = FALSE)
                        ) %>%
                          with_help(tooltip = tooltip_text[["data_input_dt_peak_list_edit_row"]]),
                        actionButton(inputId = "data_input_dt_peak_list_remove_row",
                                     label = "Remove",
                                     width = "100%",
                                     icon = icon("times", verify_fa = FALSE)
                        ) %>%
                          with_help(tooltip = tooltip_text[["data_input_dt_peak_list_remove_row"]])
                    ),
                    DTOutput(outputId = "data_input_dt_peak_list"),
                    fileInput(inputId = "data_input_import_search",
                              label = NULL,
                              buttonLabel = "Import",
                              multiple = FALSE,
                              accept = app_settings$data_input_import_search_settings_types,
                              width = "100%",
                              placeholder = "Select a file to import parameters"
                    )
                )
              ),
              fluidRow(
                column(12,
                       actionButton(inputId = "data_input_process_btn",
                                    label = "Process Data",
                                    icon = icon("circle-play",
                                                verify_fa = FALSE),
                                    width = "100%"
                       ) %>%
                         with_help(tooltip = tooltip_text[["data_input_process_btn"]])
                ),
                column(12,
                       id = "data_input_next_actions",
                       h4("Data processed. Choose next action from here or from the navigation menu on the left."),
                       div(class = "flex-container",
                           actionButton(inputId = "data_input_go_compound",
                                        label = "Compound Match",
                                        width = "100%",
                                        icon = icon("magnifying-glass",
                                                    verify_fa = FALSE)
                           ) %>%
                             with_help(tooltip = tooltip_text[["data_input_go_compound"]]),
                           actionButton(inputId = "data_input_go_fragment",
                                        label = "Fragment Match",
                                        width = "100%",
                                        icon = icon("puzzle-piece",
                                                    verify_fa = FALSE)
                           ) %>%
                             with_help(tooltip = tooltip_text[["data_input_go_fragment"]])
                       )
                )
              )
      ),
      # Search Compounds ----
      tabItem("search_compounds",
              div(class = "overlay",
                  id = "search_compounds_overlay",
                  img(src = "processing.gif"),
                  h3(id = "search_compounds_overlay_text")
              ),
              fluidRow(
                box(title = "Compound Matching",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    tags$label(
                      "Select a search type and feature of interest to get started. Features of interest are determined by the list in the",
                      actionLink(inputId = "search_compounds_go_data_input", "data input"),
                      "page."
                    ),
                    fluidRow(
                      column(12,
                             fluidRow(
                               column(3,
                                      selectizeInput(inputId = "search_compounds_search_type",
                                                     label = "Search Type",
                                                     choices = c("Precursor Search" = "precursor", "All" = "all"),
                                                     selected = 1,
                                                     multiple = FALSE,
                                                     width = "100%") %>%
                                        with_help(tooltip = tooltip_text[["search_compounds_search_type"]])
                               ),
                               column(9,
                                      selectizeInput(inputId = "search_compounds_mzrt",
                                                     label = "Feature of Interest",
                                                     choices = NULL,
                                                     selected = NULL,
                                                     multiple = FALSE,
                                                     width = "100%",
                                                     options = list(placeholder = "Please add search parameters on the Data Input page.")) %>%
                                        with_help(tooltip = tooltip_text[["search_compounds_mzrt"]])
                               )
                             ),
                             span(id = "search_compounds_additional",
                                  box(title = tagList(icon("screwdriver-wrench", verify_fa = FALSE),
                                                      span(id = "search_compounds_additional_title", "Advanced search parameters") %>%
                                                        with_help(tooltip = tooltip_text[["search_compounds_additional_title"]])),
                                      width = 12,
                                      solidHeader = FALSE,
                                      status = "primary",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      h4(style = "color: #3571a5;", "Fine tuning of search parameters should only be done under expert advice."),
                                      column(6,
                                             with(app_settings,
                                                  tagList(
                                                    tags$label("Search object settings"),
                                                    div(class = "form-grouping",
                                                        with(search_compounds_search_zoom,
                                                             sliderInput(inputId = "search_compounds_search_zoom",
                                                                         label = "Search zoom window (Da)",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_search_zoom"]]))
                                                        )
                                                    ),
                                                    tags$label("Search mass spectra settings"),
                                                    div(class = "form-grouping",
                                                        with(search_compounds_correlation,
                                                             sliderInput(inputId = "search_compounds_correlation",
                                                                         label = HTML("Search correlation (&rho;)"),
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_correlation"]]))
                                                        ),
                                                        with(search_compounds_ph,
                                                             sliderInput(inputId = "search_compounds_ph",
                                                                         label = "Search peak height threshold (%)",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_ph"]]))
                                                        ),
                                                        with(search_compounds_freq,
                                                             sliderInput(inputId = "search_compounds_freq",
                                                                         label = "Search frequency",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_freq"]]))
                                                        ),
                                                        selectizeInput(inputId = "search_compounds_norm_function",
                                                                       label = "Search normalization function",
                                                                       choices = search_compounds_norm_function$choices,
                                                                       width = "100%"
                                                        ) %>%
                                                          with_help(tooltip = tooltip_text[["search_compounds_norm_function"]]),
                                                        selectizeInput(inputId = "search_compounds_correlation_method",
                                                                       label = "Search correlation method",
                                                                       choices = search_compounds_correlation_method$choices,
                                                                       width = "100%"
                                                        ) %>%
                                                          with_help(tooltip = tooltip_text[["search_compounds_correlation_method"]])
                                                    )
                                                  )
                                             )
                                      ),
                                      column(6,
                                             with(app_settings,
                                                  tagList(
                                                    tags$label("Search match refinement settings"),
                                                    div(class = "form-grouping",
                                                        with(search_compounds_max_correl,
                                                             sliderInput(inputId = "search_compounds_max_correl",
                                                                         label = HTML("Maximum correlation (&rho;)"),
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_max_correl"]]))
                                                        ),
                                                        with(search_compounds_correl_bin,
                                                             sliderInput(inputId = "search_compounds_correl_bin",
                                                                         label = "Correlation bin size",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_correl_bin"]]))
                                                        ),
                                                        with(search_compounds_max_ph,
                                                             sliderInput(inputId = "search_compounds_max_ph",
                                                                         label = "Maximum peak height (%)",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_max_ph"]]))
                                                        ),
                                                        with(search_compounds_ph_bin,
                                                             sliderInput(inputId = "search_compounds_ph_bin",
                                                                         label = "Peak height bin size",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_ph_bin"]]))
                                                        ),
                                                        with(search_compounds_max_freq,
                                                             sliderInput(inputId = "search_compounds_max_freq",
                                                                         label = "Maximum frequency",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_max_freq"]]))
                                                        ),
                                                        with(search_compounds_freq_bin,
                                                             sliderInput(inputId = "search_compounds_freq_bin",
                                                                         label = "Frequency bin size",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_freq_bin"]]))
                                                        ),
                                                        with(search_compounds_min_n_peaks,
                                                             sliderInput(inputId = "search_compounds_min_n_peaks",
                                                                         label = "Minimum number of spectra",
                                                                         width = "100%",
                                                                         ticks = ticks,
                                                                         value = this_value,
                                                                         min = this_min,
                                                                         max = this_max,
                                                                         step = this_step
                                                             ) %>%
                                                               with_help(tooltip = glue::glue(tooltip_text[["search_compounds_min_n_peaks"]]))
                                                        ),
                                                    )
                                                  )
                                             )
                                      )
                                  )
                             ),
                             div(class = "flex-container",
                                 actionButton(inputId = "search_compounds_search_btn",
                                              label = "Search",
                                              width = "100%",
                                              icon = icon("magnifying-glass", verify_fa = FALSE)
                                 ) %>%
                                   with_help(tooltip = tooltip_text[["search_compounds_search_btn"]]),
                                 div(id = "search_compounds_use_optimized_parameters_div",
                                     checkboxInput(inputId = "search_compounds_use_optimized_parameters",
                                                   label = "Use Optimized Search Parameters",
                                                   value = TRUE
                                     ) %>%
                                       with_help(tooltip = tooltip_text[["search_compounds_use_optimized_parameters"]],
                                                 placement = "left")
                                 )
                             )
                      )
                    ),
                    fluidRow(
                      column(12,
                             id = "search_compounds_results_span",
                             fluidRow(
                               column(4,
                                      div(id = "search_compounds_status_div",
                                          htmlOutput(outputId = "search_compounds_match_top",
                                                     width = "100%"),
                                          htmlOutput(outputId = "search_compounds_match_selected",
                                                     width = "100%")
                                      ),
                                      h3(id = "search_compounds_butterfly_plot_title",
                                         "Comparison Mass Spectrum"
                                      ),
                                      p(id = "search_compounds_butterfly_plot_label",
                                        HTML(
                                          paste0(
                                            "Your measurement is in&nbsp;",
                                            strong(style = "color: black", "black"),
                                            ". Comparison spectrum is in&nbsp;",
                                            strong(style = "color: red", "red"),
                                            "."
                                          )
                                        )
                                      ) %>%
                                        with_help(tooltip = tooltip_text[["search_compounds_butterfly_plot_label"]]),
                                      shinyWidgets::radioGroupButtons(
                                        inputId = "search_compounds_msn",
                                        label = NULL,
                                        choices = c("MS1", "MS2"),
                                        size = "xs",
                                        selected = "MS2",
                                        justified = TRUE,
                                        checkIcon = list(
                                          yes = icon("ok", 
                                                     lib = "glyphicon"))
                                      ),
                                      plotlyOutput(outputId = "search_compounds_butterfly_plot",
                                                   width = "100%") %>%
                                        withSpinner(),
                                      htmlOutput(outputId = "search_compounds_method_narrative",
                                                 width = "100%"),
                                      actionButton(inputId = "search_compounds_uncertainty_btn",
                                                   label = "Estimate Match Score Uncertainty",
                                                   width = "100%",
                                                   icon = icon("arrows-left-right-to-line", verify_fa = FALSE)) %>%
                                        with_help(tooltip = tooltip_text[["search_compounds_uncertainty_btn"]],
                                                  placement = "top")
                               ),
                               column(8,
                                      DTOutput(outputId = "search_compounds_dt",
                                               width = "100%") %>%
                                        withSpinner()
                               )
                             )
                      )
                    )
                )
              )
      ),
      # Search Fragments ----
      tabItem("search_fragments",
              div(class = "overlay",
                  id = "search_fragments_overlay",
                  img(src = "processing.gif"),
                  h3(id = "search_fragments_overlay_text")
              ),
              fluidRow(
                box(title = "Fragment Analysis",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    tags$label(
                      "Select a feature of interest to get started. Features of interest are determined by the list in the",
                      actionLink(inputId = "search_fragments_go_data_input", "data input"),
                      "page."
                    ),
                    fluidRow(
                      column(12,
                             selectizeInput(inputId = "search_fragments_mzrt",
                                            label = "Feature of Interest",
                                            choices = NULL,
                                            selected = NULL,
                                            multiple = FALSE,
                                            width = "100%",
                                            options = list(placeholder = "Please add search parameters on the Data Input page.")) %>%
                               with_help(tooltip = tooltip_text[["search_fragments_mzrt"]]),
                             htmlOutput(outputId = "search_fragments_has_mzrt_has_ions",
                                        width = "100%"),
                             actionButton(inputId = "search_fragments_search_btn",
                                          label = "Search",
                                          icon = icon("magnifying-glass", verify_fa = FALSE),
                                          width = "100%") %>%
                               with_help(tooltip = tooltip_text[["search_fragments_search_btn"]]),
                             textOutput(outputId = "search_fragments_status")
                      )
                    ),
                    span(id = "search_fragments_results_span",
                         hr(),
                         column(12,
                                fluidRow(
                                  column(8,
                                         plotOutput(outputId = "search_fragments_spectral_plot",
                                                    width = "100%") %>%
                                           withSpinner()
                                  ),
                                  column(4,
                                         h4("Your spectral data included"),
                                         DTOutput(outputId = "search_fragments_spectral_data",
                                                  width = "100%") %>%
                                           withSpinner()
                                  )
                                ),
                                hr()
                         ),
                         span(id = "search_fragments_no_results",
                              h3(width = "100%",
                                 style = "text-align: center; color: grey",
                                 "No Matching Fragments Identified")
                         ),
                         span(id = "search_fragments_has_results",
                              column(12,
                                     h4("Select a row in the left-hand table to view additional information for that annotated fragment."),
                                     fluidRow(
                                       column(width = 4,
                                              DTOutput(outputId = "search_fragments_dt",
                                                       width = "100%") %>%
                                                withSpinner()
                                       ),
                                       column(width = 4,
                                              htmlOutput(outputId = "search_fragments_ballstick_caption"),
                                              span(class = "centered-image",
                                                   imageOutput(outputId = "search_fragments_ballstick",
                                                               height = "200px"),
                                                   if (rdkit_available) {
                                                     actionLink(inputId = "search_fragments_fragment_info",
                                                                label = "More Fragment Information",
                                                                icon = icon("search", verify_fa = FALSE)
                                                     ) %>%
                                                       with_help(tooltip = tooltip_text[["search_fragments_fragment_info"]],
                                                                 placement = "top")
                                                   } else {
                                                     NULL
                                                   }
                                              )
                                       ),
                                       column(width = 4,
                                              h4("This fragment has been annotated in the following"),
                                              tabsetPanel(
                                                type = "tabs",
                                                id = "search_fragments_compounds_peaks_tabset",
                                                tabPanel("Compounds",
                                                         DTOutput(outputId = "search_fragments_compound_list",
                                                                  width = "100%") %>%
                                                           withSpinner(),
                                                         actionLink(inputId = "search_fragments_compound_info",
                                                                    label = "More Compound Information",
                                                                    icon = icon("search", verify_fa = FALSE)) %>%
                                                           with_help(tooltip = tooltip_text[["search_fragments_compound_info"]],
                                                                     placement = "top")
                                                ),
                                                tabPanel("Peaks",
                                                         DTOutput(outputId = "search_fragments_peak_list",
                                                                  width = "100%") %>%
                                                           withSpinner(),
                                                         actionLink(inputId = "search_fragments_peak_info",
                                                                    label = "More Peak Information",
                                                                    icon = icon("search", verify_fa = FALSE)) %>%
                                                           with_help(tooltip = tooltip_text[["search_fragments_peak_info"]],
                                                                     placement = "top")
                                                )
                                              )
                                              
                                       )
                                     )
                              )
                         )
                    )
                )
              )
      ),
      # About ----
      tabItem("about",
              fluidRow(
                box(title = NULL,
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    height = "calc(100vh - 100px)",
                    includeHTML("about.html")
                )
              )
      )
    )
  )
)
