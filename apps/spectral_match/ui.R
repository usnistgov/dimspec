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
            tabItem("data_input",
                    fluidRow(
                        box(title = "Load Data",
                            width = 4,
                            fileInput(inputId = "data_input_filename",
                                      label = "Choose a data file (.mzML or .raw)",
                                      multiple = FALSE,
                                      accept = c(".mzML", ".raw"),
                                      buttonLabel = "Choose File",
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
                            tags$label("Extraction parameters determine which peaks are selected for examination."),
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
                                      buttonLabel = "Import Parameters",
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
            tabItem("search_compounds",
                    # h3("Find compounds for a peak"),
                    div(id = "search_compounds_overlay",
                        class = "overlay",
                        width = "100%",
                        height = "100%",
                        h3("Please load data first")),
                    fluidRow(
                        column(4,
                               ),
                        column(8,
                               )
                    )
            ),
            tabItem("uncertainty",
                    # h3("Evaluate Uncertainty"),
                    div(id = "uncertainty_overlay",
                        class = "overlay",
                        width = "100%",
                        height = "100%",
                        h3("Please load data first")),
                    fluidRow(
                        column(4,
                        ),
                        column(8,
                        )
                    )
            ),
            tabItem("search_fragments",
                    # h3("Find matching fragments"),
                    div(id = "search_fragments_overlay",
                        class = "overlay",
                        width = "100%",
                        height = "100%",
                        h3("Please load data first")),
                    fluidRow(
                        column(4,
                        ),
                        column(8,
                        )
                    )
            ),
            tabItem("about",
                    box(title = "About this tool",
                        width = 12,
                        height = "90vh",
                        includeHTML("about.html")
                    )
            )
        )
    )
)
