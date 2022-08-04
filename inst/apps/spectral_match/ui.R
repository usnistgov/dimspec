dashboardPage(
  skin = "blue",
  title = DB_TITLE,
  header = dashboardHeader(
    title = a(
      img(src = "NIST-Logo-Brand-White.svg"),
      href = "https://www.nist.gov/programs-projects/measurement-science-and-polyfluoroalkyl-substances-pfas#:~:text=Overview%20of%20the%20NIST%20program%20on%20per-%20and,for%20a%20variety%20of%20commercial%20and%20industrial%20applications."
    )
  ),
  sidebar = dashboardSidebar(
    h4(style = "padding-left: 15px;", "HRAMS Database for PFAS"),
    if (dev) {
      div(style = "padding-left: 15px;",
          if (dev) {
            h4(id = "dev_mode", style = "padding-right: 15px; color: red; text-align: center;", "DEVELOPMENT MODE")
          } else {
            NULL
          },
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
               badgeLabel = "WIP",
               badgeColor = "orange",
               icon = icon("house", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = "Contextural material for the application you are currently using, along with some information about the project itself.",
                  placement = "right"),
      menuItem(span(id = "nav_data_input", "Data Input"),
               tabName = "data_input",
               icon = icon("file")
      ) %>%
        with_help(tooltip = "Start here with data entry where you will upload a data file and select features of interest by mass-to-charge ratio (m/z) and retention time.",
                  placement = "right"),
      menuItem(span(id = "nav_search_compounds", "Compound Match"),
               tabName = "search_compounds",
               icon = icon("magnifying-glass", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = "Search the library for compounds matching the mass spectrometric fingerprints for features of interest.",
                  placement = "right"),
      menuItem(span(id = "nav_fragments", "Fragment Match"),
               tabName = "search_fragments",
               icon = icon("puzzle-piece", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = "Match fragments from your mass spectrometry experiment against those held in the library, including linked metrics for which compounds and peaks those fragments have been annotated within.",
                  placement = "right"),
      menuItem(span(id = "nav_about", "About"),
               tabName = "about",
               badgeLabel = "WIP",
               badgeColor = "orange",
               icon = icon("circle-info", verify_fa = FALSE)
      ) %>%
        with_help(tooltip = "Your reference guide to this application, including user guides and other information.",
                  placement = "right"),
      span(id = "additional_options",
           div(id = "nav_show_help_div",
               class = "nav-checkbox-right",
               icon("question", verify_fa = FALSE),
               tags$label("Show Tooltips"),
               prettySwitch(inputId = "nav_show_help",
                            label = NULL,
                            value = provide_more_help,
                            inline = TRUE,
                            status = "success")
           ),
           div(id = "nav_show_advanced_settings_div",
               class = "nav-checkbox-right",
               icon("gear", verify_fa = FALSE),
               tags$label("Advanced Settings"),
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
    # if (dev) div(class = "title-banner", "DEVELOPMENT VERSION") else NULL,
    tabItems(
      # Home Page ----
      tabItem("index",
              h2(APP_TITLE, style = "margin: 0px;"),
              hr(),
              actionButton(inputId = "index_go_data_input",
                           label = "Click Here to Get Started",
                           width = "100%",
                           icon = icon("circle-play", verify_fa = FALSE)
              ) %>%
                with_help("Click here to go to the data input page and get started."),
              hr(),
              includeHTML("index.html")
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
                                    with_help('Start by dragging a data file here, or click "Load" to select one from your computer'),
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
                                      with(data_input_relative_error,
                                           numericInput(inputId = "data_input_relative_error",
                                                        label = "Relative Error (ppm)",
                                                        width = "100%",
                                                        value = this_value,
                                                        min = this_min,
                                                        max = this_max,
                                                        step = this_step
                                           ) %>%
                                             with_help(glue::glue("Set the instrument mass-to-charge relative error in parts-per-million (ppm) for processing, ranging from {this_min} ppm to {this_max} ppm. The default of {this_value} ppm is typical for most applications."))
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
                                             with_help(glue::glue("Set the instrument minimum mass-to-charge absolute error in Daltons (Da) for processing, ranging from {this_min} Da to {this_max} Da. The default of {this_value} Da is typical for most applications."))
                                      ),
                                      selectizeInput(inputId = "data_input_experiment_type",
                                                     label = "MS Experiment Type",
                                                     choices = experiment_types$choices,
                                                     width = "100%"
                                      ) %>%
                                        with_help('Set the experiment type for your data file; choices are defined in the "norm_ms_n_types" database table.'),
                                      with(data_input_isolation_width,
                                           numericInput(inputId = "data_input_isolation_width",
                                                        label = "Isolation Width (Da)",
                                                        width = "100%",
                                                        value = this_value,
                                                        min = this_min,
                                                        max = this_max,
                                                        step = this_step
                                           ) %>%
                                             with_help(glue::glue("Set the instrument data isolation width in Daltons (Da) for processing, ranging from {this_min} Da to {this_max} Da. The default of {this_value} Da is typical for data-dependent acquisition, but may be different for other acquisition types (e.g. for SWATH or HRM it may be much larger)."))
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
                      with_help('Start by clicking "Add" to manually add a feature of interest, or by dragging a data file to the widget below or clicking "Import" to select one from your computer'),
                    div(class = "flex-container",
                        actionButton(inputId = "data_input_dt_peak_list_add_row",
                                     label = "Add",
                                     width = "100%",
                                     icon = icon("plus")
                        ) %>%
                          with_help("Click to manually add a feature of interest by its mass-to-charge ratio value and retention time properties."),
                        actionButton(inputId = "data_input_dt_peak_list_edit_row",
                                     label = "Edit",
                                     width = "100%",
                                     icon = icon("pencil-alt")
                        ) %>%
                          with_help("Click to edit a feature of interest selected in the table below."),
                        actionButton(inputId = "data_input_dt_peak_list_remove_row",
                                     label = "Remove",
                                     width = "100%",
                                     icon = icon("times")
                        ) %>%
                          with_help("Click to remove a feature of interest selected in the table below.")
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
                         with_help("Click here to begin processing the data file provided and extract data matching the features of interest defined in the list above. More options will be available once data have been processed.")
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
                             with_help("Data have been processed. Click here to go to the compound match screen and search for compounds matching your defined features of interest. This is most useful for a broad search or if you suspect an identity already."),
                           actionButton(inputId = "data_input_go_fragment",
                                        label = "Fragment Match",
                                        width = "100%",
                                        icon = icon("puzzle-piece",
                                                    verify_fa = FALSE)
                           ) %>%
                             with_help("Data have been processed. Click here to go to the fragment match screen and search for fragments matching your defined features of interest. This is most useful for examining which fragments within your feature of interest are shared by other compounds.")
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
                                        with_help("Select the type of search to perform. Precursor search limits itself to matching precursor mass-to-charge ratios between the error limits defined earlier. Search all attempts to find every possible match within the database and will take considerably longer.")
                               ),
                               column(9,
                                      selectizeInput(inputId = "search_compounds_mzrt",
                                                     label = "Feature of Interest",
                                                     choices = NULL,
                                                     selected = NULL,
                                                     multiple = FALSE,
                                                     width = "100%",
                                                     options = list(placeholder = "Please add search parameters on the Data Input page.")) %>%
                                        with_help("Select the feature of interest to use in the search. These were provided on the data input page. If you need to change them, go back to the data input page, alter the feature list, and process your data again.")
                               )
                             ),
                             span(id = "search_compounds_additional",
                                  box(title = tagList(icon("screwdriver-wrench", verify_fa = FALSE),
                                                      span(id = "search_compounds_additional_title", "Advanced search parameters") %>%
                                                        with_help("Alter advanced parameters for compound matching here. Click the plus icon to the right to expand this box.")),
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
                                                               with_help(glue::glue("Set the search window which will determine how data are grouped, ranging from {this_min} to {this_max} Daltons. The default of {paste0(this_value, collapse = ' to ')} Da is typical for most applications."))
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
                                                               with_help(glue::glue("Set the correlation rho threshold for peak deconvolution, ranging from {this_min} (uncorrelated) to {this_max} (fully correlated). The default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the percentage peak height threshold for processing, ranging from {this_min}% to {this_max}%. The default of {this_value}% is typical for most applications. Peaks with lesser relative heights will be ignored."))
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
                                                               with_help(glue::glue("Set the minimum frequency for the number of times a mass must appear in scans across a peak, ranging from {this_min} to {this_max}. The default of {this_value} is typical for most applications, but this depends in part upon scan rate."))
                                                        ),
                                                        selectizeInput(inputId = "search_compounds_norm_function",
                                                                       label = "Search normalization function",
                                                                       choices = search_compounds_norm_function$choices,
                                                                       width = "100%"
                                                        ) %>%
                                                          with_help('Choose a normalization function to use when matching compounds, either "sum" (typical default) for base peak normalization or "mean" for relative intensity normalization.'),
                                                        selectizeInput(inputId = "search_compounds_correlation_method",
                                                                       label = "Search correlation method",
                                                                       choices = search_compounds_correlation_method$choices,
                                                                       width = "100%"
                                                        ) %>%
                                                          with_help("Choose a correlation method to use when matching compounds; pearson is the only method currently supported.")
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
                                                               with_help(glue::glue("Set the maximum correlation to be used during the search refinement stage of the search function, ranging from {this_min} (uncorrelated) to {this_max} (completely correlated); the default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the correlation bin size which will be used to group correlation results during the refinement stage of the search function, ranging from {this_min} to {this_max}; the default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the percentage of base peak height threshold to be used during the refinement stage of the search function, ranging from {this_min}% to {this_max}%; the default of {this_value}% is typical for most applications. Peaks with lesser relative heights will be ignored."))
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
                                                               with_help(glue::glue("Set the peak height bin size which will be used to group potential results during the refinement stage of the search function, ranging from {this_min} to {this_max}; the default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the maximum observation frequency will be used to group potential results during the refinement stage of the search function, ranging from {this_min} to {this_max}; the default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the frequency bin size which will be used to during the refinement stage of the search function, ranging from {this_min} to {this_max}; the default of {this_value} is typical for most applications."))
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
                                                               with_help(glue::glue("Set the minimum number of spectra which must be present to be included in a match, ranging from {this_min} to {this_max}; the default of {this_value} is typical for most applications."))
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
                                   with_help("Click here to execute a search for compounds matching this feature."),
                                 div(id = "search_compounds_use_optimized_parameters_div",
                                     checkboxInput(inputId = "search_compounds_use_optimized_parameters",
                                                   label = "Use Optimized Search Parameters",
                                                   value = TRUE
                                     ) %>%
                                       with_help("Ensure this is checked to use optimized search parameters held within the database and speed up your search. If it is left unchecked, new search parameters will be generated, which may take a considerable amount of time.",
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
                                      p(
                                        HTML(
                                          paste0(
                                            "Your measurement is in&nbsp;",
                                            strong(style = "color: black", "black"),
                                            ". Comparison spectrum is in&nbsp;",
                                            strong(style = "color: red", "red"),
                                            "."
                                          )
                                        )
                                      ),
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
                                                   label = "Estimate Spectral Uncertainty",
                                                   width = "100%",
                                                   icon = icon("arrows-left-right-to-line", verify_fa = FALSE))    
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
                    h4("[USE INSTRUCTIONS HERE]."),
                    fluidRow(
                      column(12,
                             selectizeInput(inputId = "search_fragments_mzrt",
                                            label = "Feature of Interest",
                                            choices = NULL,
                                            selected = NULL,
                                            multiple = FALSE,
                                            width = "100%",
                                            options = list(placeholder = "Please add search parameters on the Data Input page.")),
                             htmlOutput(outputId = "search_fragments_has_mzrt_has_ions",
                                        width = "100%"),
                             actionButton(inputId = "search_fragments_search_btn",
                                          label = "Search",
                                          icon = icon("magnifying-glass", verify_fa = FALSE),
                                          width = "100%"),
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
                                           # plotlyOutput(outputId = "search_fragments_spectral_plot",
                                           #              width = "100%") %>%
                                           withSpinner()
                                  ),
                                  column(4,
                                         h4("Your spectral data included"),
                                         DTOutput(outputId = "search_fragments_spectral_data",
                                                  width = "100%") %>%
                                           withSpinner()
                                  )
                                )
                         ),
                         hr(),
                         column(12,
                                h4("Select a row in the table to view additional information for that annotated fragment."),
                                fluidRow(
                                  column(width = ifelse(rdkit_available, 4, 6),
                                         DTOutput(outputId = "search_fragments_dt",
                                                  width = "100%") %>%
                                           withSpinner()
                                  ),
                                  column(width = ifelse(rdkit_available, 4, 0),
                                         if (rdkit_available) {
                                           tagList(
                                             htmlOutput(outputId = "search_fragments_ballstick_caption"),
                                             span(class = "centered-image",
                                                  imageOutput(outputId = "search_fragments_ballstick",
                                                              height = "200px"),
                                                  actionLink(inputId = "search_fragments_fragment_info",
                                                             label = "More Fragment Information",
                                                             icon = icon("search", verify_fa = FALSE)
                                                  )
                                             )
                                           )
                                         } else {
                                           NULL
                                         }
                                  ),
                                  column(width = ifelse(rdkit_available, 4, 6),
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
                                                               icon = icon("search", verify_fa = FALSE))
                                           ),
                                           tabPanel("Peaks",
                                                    DTOutput(outputId = "search_fragments_peak_list",
                                                             width = "100%") %>%
                                                      withSpinner(),
                                                    actionLink(inputId = "search_fragments_peak_info",
                                                               label = "More Peak Information",
                                                               icon = icon("search", verify_fa = FALSE))
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
                box(title = "About this tool",
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
