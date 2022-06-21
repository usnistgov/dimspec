dashboardPage(
    skin = "blue",
    title = DB_TITLE,
    header = dashboardHeader(
        title = a(
            img(src = "NIST_logo.png"),
            href = "https://www.nist.gov/programs-projects/measurement-science-and-polyfluoroalkyl-substances-pfas#:~:text=Overview%20of%20the%20NIST%20program%20on%20per-%20and,for%20a%20variety%20of%20commercial%20and%20industrial%20applications."
        )
    ),
    sidebar = dashboardSidebar(
        if (dev) {
            div(
                h4("Plumber instance is live at ", PLUMBER_URL),
                p("View the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), "here")),
                actionButton("browser", "Live Inspect", icon = icon("user-ninja"))
            )
        } else {
            NULL
        },
        h3(APP_TITLE, style = "padding: 5px; margin: 0px;"),
        sidebarMenu(
            id = "sidebar_menu",
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
            # Data Input ----
            tabItem("data_input",
                    fluidRow(
                        box(title = "Load Data",
                            width = 4,
                            solidHeader = TRUE,
                            status = "primary",
                            class = "left",
                            fileInput(inputId = "data_input_filename",
                                      label = "Choose a data file (.mzML or .raw)",
                                      multiple = FALSE,
                                      accept = c(".mzML", ".raw"),
                                      buttonLabel = "Load",
                                      placeholder = "Select a file to begin"
                            ),
                            tags$label("Set instrument parameters"),
                            numericInput(inputId = "data_input_relative_error",
                                         label = "Relative Error (ppm)",
                                         value = 5,
                                         min = 0.1,
                                         max = 50,
                                         step = 0.1
                            ),
                            numericInput(inputId = "data_input_minimum_error",
                                         label = "Minimum Error (Da)",
                                         value = 0.002,
                                         min = 0.0001,
                                         max = 0.5,
                                         step = 0.0001
                            ),
                            selectizeInput(inputId = "data_input_experiment_type",
                                           label = "MS Experiment Type",
                                           choices = app_settings$experiment_types
                            ),
                            numericInput(inputId = "data_input_isolate_width",
                                         label = "Isolation Width (Da)",
                                         value = 0.7,
                                         min = 0.1,
                                         max = 100,
                                         step = 0.1
                            )
                        ),
                        box(title = "Select Data Extraction Parameters",
                            width = 8,
                            solidHeader = TRUE,
                            status = "primary",
                            class = "right",
                            tags$label("Select parameters identifying peakss to examine."),
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
                                      accept = c(".csv", ".xls", ".xlsx"),
                                      width = "100%",
                                      placeholder = "Select a file to import parameters"
                            )
                        )
                    ),
                    fluidRow(
                        column(12,
                               actionButton(inputId = "data_input_import",
                                            label = "Import Data",
                                            width = "100%")
                        )
                    )
            ),
            # Search Compounds ----
            tabItem("search_compounds",
                    # div(id = "search_compounds_overlay",
                    #     class = "overlay",
                    #     h3("Please load data first")),
                    fluidRow(
                        box(title = "Spectral Comparison",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            h4("[USE INSTRUCTIONS HERE]."),
                            fluidRow(
                                column(12,
                                       fluidRow(
                                           column(3,
                                                  selectizeInput(inputId = "search_compounds_search_type",
                                                                 label = "Search Type",
                                                                 choices = c("Precursor Search" = 1, "All" = 2),
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
                                                                 width = "100%")
                                           )
                                       ),
                                       actionButton(inputId = "search_compounds_search_btn",
                                                    label = "Search",
                                                    icon = icon("magnifying-glass", verify_fa = FALSE),
                                                    width = "100%"),
                                       textOutput(outputId = "search_compounds_status")
                                )
                            ),
                            fluidRow(
                                column(12,
                                       id = "search_compounds_results_span",
                                       fluidRow(
                                           column(12,
                                                  DTOutput(outputId = "search_compounds_dt",
                                                           width = "100%")
                                           )
                                       ),
                                       fluidRow(
                                           column(8,
                                                  plotlyOutput(outputId = "search_compounds_butterfly_plot",
                                                               width = "100%"),
                                                  textOutput(outputId = "search_compounds_method_description")       
                                           ),
                                           column(4,
                                                  numericInput(inputId = "search_compounds_uncertainty_iterations",
                                                               label = "Bootstrap Iterations",
                                                               value = 1e4,
                                                               min = 1e2,
                                                               max = 1e5,
                                                               step = 1e2),
                                                  actionButton(inputId = "search_compounds_uncertainty_btn",
                                                               label = "Send to Uncertainty Estimate",
                                                               width = "100%",
                                                               icon = icon("arrows-left-right-to-line", verify_fa = FALSE))
                                           )
                                       )
                                )
                            )
                        )
                    )
            ),
            # Uncertainty ----
            tabItem("uncertainty",
                    # div(id = "uncertainty_overlay",
                    #     class = "overlay",
                    #     h3("Please load data first")),
                    fluidRow(
                        box(title = "Uncertainty Analysis",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            h4("[USE INSTRUCTIONS HERE]."),
                            fluidRow(
                                column(6,
                                       textOutput(outputId = "uncertainty_method_description"),  
                                       plotlyOutput(outputId = "uncertainty_butterfly_plot",
                                                    width = "100%"),
                                       # If ALL match criteria, then provide a DT object here instead
                                       # Download can go off DT object if using DT ot present
                                       verbatimTextOutput(outputId = "uncertainty_summary")
                                ),
                                column(6,
                                       plotlyOutput(outputId = "uncertainty_boxplot",
                                                    width = "100%")
                                )
                            )
                        )
                    )
            ),
            # Search Fragments ----
            tabItem("search_fragments",
                    # div(id = "search_fragments_overlay",
                    #     class = "overlay",
                    #     h3("Please load data first")),
                    fluidRow(
                        box(title = "Fragment Analysis",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            h4("[USE INSTRUCTIONS HERE]."),
                            fluidRow(
                                column(12,
                                       fluidRow(
                                           column(3,
                                                  selectizeInput(inputId = "search_fragments_search_type",
                                                                 label = "Search Type",
                                                                 choices = c("Fragment Search" = 1, "All" = 2),
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
                                     column(12,
                                            DTOutput(outputId = "search_fragments_dt",
                                                     width = "100%")
                                     )
                                 ),
                                 fluidRow(
                                     column(ifelse(rdkit_active(), 6, 12),
                                            plotlyOutput(outputId = "search_fragments_spectral_plot",
                                                         width = "100%")
                                     ),
                                     column(ifelse(rdkit_active(), 6, 0),
                                            imageOutput(outputId = "search_fragment_ballstick")
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
                            solidHeader = TRUE,
                            status = "primary",
                            height = "calc(100vh - 100px)",
                            includeHTML("about.html")
                        )
                    )
            )
        )
    )
)
