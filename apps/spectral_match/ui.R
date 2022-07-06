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
        if (dev) {
            div(
                h4("Plumber instance is live at ", PLUMBER_URL),
                p("View the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), target = "_blank", "here")),
                actionButton("browser", "Live Inspect", icon = icon("user-ninja"))
            )
        } else {
            NULL
        },
        h4(style = "padding-left: 15px;", "HRAMS Database for PFAS"),
        sidebarMenu(
            id = "sidebar_menu",
            menuItem("Home",
                     tabName = "index",
                     icon = icon("house", verify_fa = FALSE)
            ),
            menuItem("Data Input",
                     tabName = "data_input",
                     icon = icon("file")
            ),
            menuItem("Compound Match",
                     tabName = "search_compounds",
                     icon = icon("magnifying-glass", verify_fa = FALSE)
            ),
            menuItem("Uncertainty",
                     tabName = "uncertainty",
                     icon = icon("arrows-left-right-to-line", verify_fa = FALSE)
            ),
            menuItem("Fragment Match",
                     tabName = "search_fragments",
                     icon = icon("puzzle-piece", verify_fa = FALSE)
            ),
            menuItem("About",
                     tabName = "about",
                     icon = icon("circle-info", verify_fa = FALSE)
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
                    h2(APP_TITLE, style = "margin: 0px;"),
                    hr(),
                    actionButton(inputId = "index_go_data_input",
                                 label = "Click Here to Get Started",
                                 width = "100%",
                                 icon = icon("circle-play", verify_fa = FALSE)
                    ),
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
                                   status = "primary",
                                   class = "left",
                                   with(app_settings,
                                        tagList(
                                            tags$label(glue::glue("Choose a data file ({format_list_of_names(data_input_import_file_types)})")),
                                            div(class = "form-grouping",
                                                fileInput(inputId = "data_input_filename",
                                                          label = NULL,
                                                          width = "100%",
                                                          multiple = FALSE,
                                                          accept = data_input_import_file_types,
                                                          buttonLabel = "Load",
                                                          placeholder = "Select a file to begin"
                                                )
                                            ),
                                            tags$label("Set Instrument Parameters"),
                                            div(class = "form-grouping",
                                                numericInput(inputId = "data_input_relative_error",
                                                             label = "Relative Error (ppm)",
                                                             width = "100%",
                                                             value = data_input_relative_error$value,
                                                             min = data_input_relative_error$min,
                                                             max = data_input_relative_error$max,
                                                             step = data_input_relative_error$step
                                                ),
                                                numericInput(inputId = "data_input_minimum_error",
                                                             label = "Minimum Error (Da)",
                                                             width = "100%",
                                                             value = data_input_minimum_error$value,
                                                             min = data_input_minimum_error$min,
                                                             max = data_input_minimum_error$max,
                                                             step = data_input_minimum_error$step
                                                ),
                                                selectizeInput(inputId = "data_input_experiment_type",
                                                               label = "MS Experiment Type",
                                                               choices = experiment_types,
                                                               width = "100%",
                                                ),
                                                numericInput(inputId = "data_input_isolation_width",
                                                             label = "Isolation Width (Da)",
                                                             width = "100%",
                                                             value = data_input_isolation_width$value,
                                                             min = data_input_isolation_width$min,
                                                             max = data_input_isolation_width$max,
                                                             step = data_input_isolation_width$step
                                                )
                                            )
                                        )
                                   )
                               ),
                               span(id = "data_input_additional",
                                    box(title = tagList(icon("screwdriver-wrench", verify_fa = FALSE),
                                                        "Advanced search parameters"),
                                        width = 12,
                                        solidHeader = FALSE,
                                        status = "primary",
                                        class = "left",
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        with(app_settings,
                                             tagList(
                                                 h4(style = "color: #3571a5;", "Fine tuning of search parameters should only be done under expert advice."),
                                                 tags$label("Search object settings"),
                                                 div(class = "form-grouping",
                                                     sliderInput(inputId = "data_input_search_zoom",
                                                                 label = "Search zoom window",
                                                                 width = "100%",
                                                                 ticks = data_input_search_zoom$ticks,
                                                                 value = data_input_search_zoom$value,
                                                                 min = data_input_search_zoom$min,
                                                                 max = data_input_search_zoom$max,
                                                                 step = data_input_search_zoom$step
                                                     )
                                                 ),
                                                 tags$label("Search mass spectra settings"),
                                                 div(class = "form-grouping",
                                                     sliderInput(inputId = "data_input_correlation",
                                                                 label = "Search correlation",
                                                                 width = "100%",
                                                                 ticks = data_input_correlation$ticks,
                                                                 value = data_input_correlation$value,
                                                                 min = data_input_correlation$min,
                                                                 max = data_input_correlation$max,
                                                                 step = data_input_correlation$step
                                                     ),
                                                     sliderInput(inputId = "data_input_ph",
                                                                 label = "Search peak height",
                                                                 width = "100%",
                                                                 ticks = data_input_ph$ticks,
                                                                 value = data_input_ph$value,
                                                                 min = data_input_ph$min,
                                                                 max = data_input_ph$max,
                                                                 step = data_input_ph$step
                                                     ),
                                                     sliderInput(inputId = "data_input_freq",
                                                                 label = "Search frequency",
                                                                 width = "100%",
                                                                 ticks = data_input_freq$ticks,
                                                                 value = data_input_freq$value,
                                                                 min = data_input_freq$min,
                                                                 max = data_input_freq$max,
                                                                 step = data_input_freq$step
                                                     ),
                                                     selectizeInput(inputId = "data_input_norm_function",
                                                                    label = "Search normalization function",
                                                                    choices = data_input_normfn,
                                                                    width = "100%",
                                                     ),
                                                     selectizeInput(inputId = "data_input_correlation_method",
                                                                    label = "Search correlation method",
                                                                    choices = data_input_cormethod,
                                                                    width = "100%",
                                                     )
                                                 # ),
                                                 # tags$label("Search match refinement settings"),
                                                 # div(class = "form-grouping",
                                                 #     sliderInput(inputId = "data_input_max_correl",
                                                 #                 label = "Maximum correlation",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_max_correl$ticks,
                                                 #                 value = data_input_max_correl$value,
                                                 #                 min = data_input_max_correl$min,
                                                 #                 max = data_input_max_correl$max,
                                                 #                 step = data_input_max_correl$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_correl_bin",
                                                 #                 label = "Correlation bin size",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_correl_bin$ticks,
                                                 #                 value = data_input_correl_bin$value,
                                                 #                 min = data_input_correl_bin$min,
                                                 #                 max = data_input_correl_bin$max,
                                                 #                 step = data_input_correl_bin$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_max_ph",
                                                 #                 label = "Maximum peak height",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_max_ph$ticks,
                                                 #                 value = data_input_max_ph$value,
                                                 #                 min = data_input_max_ph$min,
                                                 #                 max = data_input_max_ph$max,
                                                 #                 step = data_input_max_ph$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_ph_bin",
                                                 #                 label = "Peak height bin size",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_ph_bin$ticks,
                                                 #                 value = data_input_ph_bin$value,
                                                 #                 min = data_input_ph_bin$min,
                                                 #                 max = data_input_ph_bin$max,
                                                 #                 step = data_input_ph_bin$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_max_freq",
                                                 #                 label = "Maximum frequency",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_max_freq$ticks,
                                                 #                 value = data_input_max_freq$value,
                                                 #                 min = data_input_max_freq$min,
                                                 #                 max = data_input_max_freq$max,
                                                 #                 step = data_input_max_freq$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_freq_bin",
                                                 #                 label = "Frequency bin size",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_freq_bin$ticks,
                                                 #                 value = data_input_freq_bin$value,
                                                 #                 min = data_input_freq_bin$min,
                                                 #                 max = data_input_freq_bin$max,
                                                 #                 step = data_input_freq_bin$step
                                                 #     ),
                                                 #     sliderInput(inputId = "data_input_min_n_peaks",
                                                 #                 label = "Minimum number of spectra",
                                                 #                 width = "100%",
                                                 #                 ticks = data_input_min_n_peaks$ticks,
                                                 #                 value = data_input_min_n_peaks$value,
                                                 #                 min = data_input_min_n_peaks$min,
                                                 #                 max = data_input_min_n_peaks$max,
                                                 #                 step = data_input_min_n_peaks$step
                                                 #     )
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
                            status = "primary",
                            class = "right",
                            tags$label("Select parameters identifying peaks to examine."),
                            div(class = "flex-container",
                                actionButton(inputId = "data_input_dt_peak_list_add_row",
                                             label = "Add",
                                             icon = icon("plus")
                                ),
                                actionButton(inputId = "data_input_dt_peak_list_edit_row",
                                             label = "Edit",
                                             icon = icon("pencil-alt")
                                ),
                                actionButton(inputId = "data_input_dt_peak_list_remove_row",
                                             label = "Remove",
                                             icon = icon("times")
                                )
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
                                            icon = icon("circle-play", verify_fa = FALSE),
                                            width = "100%")
                        ),
                        column(12,
                               id = "data_input_next_actions",
                               h4("Data processed. Choose next action from here or from the navigation menu on the left."),
                               div(class = "flex-container",
                                   
                                   actionButton(inputId = "data_input_go_compound",
                                                label = "Match Compounds",
                                                icon = icon("magnifying-glass", verify_fa = FALSE)
                                   ),
                                   actionButton(inputId = "data_input_go_uncertainty",
                                                label = "Evaluate Uncertainty",
                                                icon = icon("arrows-left-right-to-line", verify_fa = FALSE)
                                   ),
                                   actionButton(inputId = "data_input_go_fragment",
                                                label = "Match Fragments",
                                                icon = icon("puzzle-piece", verify_fa = FALSE)
                                   )
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
                                                                 width = "100%")
                                           ),
                                           column(9,
                                                  selectizeInput(inputId = "search_compounds_mzrt",
                                                                 label = "Feature of Interest",
                                                                 choices = NULL,
                                                                 selected = NULL,
                                                                 multiple = FALSE,
                                                                 width = "100%",
                                                                 options = list(placeholder = "Please add search parameters on the Data Input page."))
                                           )
                                       ),
                                       div(class = "flex-container",
                                           actionButton(inputId = "search_compounds_search_btn",
                                                        label = "Search",
                                                        icon = icon("magnifying-glass", verify_fa = FALSE)
                                           ),
                                           div(id = "search_compounds_use_optimized_parameters_div",
                                               checkboxInput(inputId = "search_compounds_use_optimized_parameters",
                                                             label = "Use Optimized Search Parameters",
                                                             value = TRUE
                                               )
                                           )
                                       )
                                )
                            ),
                            fluidRow(
                                column(12,
                                       id = "search_compounds_results_span",
                                       fluidRow(
                                           column(4,
                                                  htmlOutput(outputId = "search_compounds_status",
                                                             width = "100%"),
                                                  htmlOutput(outputId = "search_compounds_status2",
                                                             width = "100%"),
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
                                                               icon = icon("arrows-left-right-to-line", verify_fa = FALSE)),    
                                                  numericInput(inputId = "search_compounds_uncertainty_iterations",
                                                               label = "Bootstrap Iterations",
                                                               value = app_settings$search_compounds_bootstrap_iterations$value,
                                                               min = app_settings$search_compounds_bootstrap_iterations$min,
                                                               max = app_settings$search_compounds_bootstrap_iterations$max,
                                                               step = app_settings$search_compounds_bootstrap_iterations$step)
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
            # Uncertainty ----
            tabItem("uncertainty",
                    div(class = "overlay",
                        id = "uncertainty_overlay",
                        img(src = "processing.gif")
                    ),
                    fluidRow(
                        box(title = "Uncertainty Analysis",
                            width = 12,
                            solidHeader = FALSE,
                            status = "primary",
                            h4("[USE INSTRUCTIONS HERE]."),
                            fluidRow(
                                column(6,
                                       h3("Uncertainty Mass Spectrum"),
                                       htmlOutput(outputId = "uncertainty_status",
                                                  width = "100%"),
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
                                       plotlyOutput(outputId = "uncertainty_butterfly_plot",
                                                    width = "100%") %>%
                                           withSpinner(),
                                       htmlOutput(outputId = "uncertainty_method_narrative"),  
                                       # If ALL match criteria, then provide a DT object here instead
                                       # Download can go off DT object if using DT ot present
                                       verbatimTextOutput(outputId = "uncertainty_summary")
                                ),
                                column(6,
                                       plotlyOutput(outputId = "uncertainty_boxplot",
                                                    width = "100%") %>%
                                           withSpinner()
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
                        h3("Extracting data...")
                    ),
                    fluidRow(
                        box(title = "Fragment Analysis",
                            width = 12,
                            solidHeader = FALSE,
                            status = "primary",
                            h4("[USE INSTRUCTIONS HERE]."),
                            fluidRow(
                                column(12,
                                       fluidRow(
                                           column(3,
                                                  selectizeInput(inputId = "search_fragments_search_type",
                                                                 label = "Search Type",
                                                                 choices = c("Fragment Search" = "fragments", "All" = "all"),
                                                                 selected = 1,
                                                                 multiple = FALSE,
                                                                 width = "100%")
                                           ),
                                           column(9,
                                                  selectizeInput(inputId = "search_fragments_mzrt",
                                                                 label = "Feature of Interest",
                                                                 choices = NULL,
                                                                 selected = NULL,
                                                                 multiple = FALSE,
                                                                 width = "100%",
                                                                 options = list(placeholder = "Please add search parameters on the Data Input page.")),
                                                  htmlOutput(outputId = "search_fragments_has_mzrt_has_ions",
                                                             width = "100%")
                                           )
                                       ),
                                       actionButton(inputId = "search_fragments_search_btn",
                                                    label = "Search",
                                                    icon = icon("magnifying-glass", verify_fa = FALSE),
                                                    width = "100%"),
                                       textOutput(outputId = "search_fragments_status")
                                )
                            ),
                            span(id = "search_fragments_results_span",
                                 fluidRow(
                                     column(4,
                                            plotlyOutput(outputId = "search_fragments_spectral_plot",
                                                         width = "100%") %>%
                                                withSpinner(),
                                            if (rdkit_available) {
                                                tagList(
                                                    textOutput(outputId = "search_fragment_ballstick_caption"),
                                                    span(class = "centered-image",
                                                         imageOutput(outputId = "search_fragment_ballstick",
                                                                     height = "300px") %>%
                                                             withSpinner()
                                                    )
                                                )
                                            }
                                     ),
                                     column(8,
                                            DTOutput(outputId = "search_fragments_dt",
                                                     width = "100%") %>%
                                                withSpinner()
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
