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
                p("View the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), "here")),
                actionButton("browser", "Live Inspect", icon = icon("user-ninja"))
            )
        } else {
            NULL
        },
        h3(DB_TITLE, style = "padding: 5px; margin: 0px;"),
        sidebarMenu(
            id = "sidebar_menu",
            menuItem("Data Import",
                     tabName = "data_import",
                     icon = icon("file")
            ),
            menuItem("Quality Review",
                     tabName = "qc_review",
                     icon = icon("check-double")
            ),
            menuItem("Export Data",
                     tabName = "export",
                     icon = icon("download")
            )
        )
    ),
    body = dashboardBody(
        tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
        tabItems(
            tabItem("data_import",
                    fluidRow(
                        column(4,
                               fileInput(inputId = "rawdata_filename",
                                         label = NULL,
                                         width = "100%",
                                         multiple = FALSE,
                                         accept = ".mzML",
                                         buttonLabel = span(id = "rawdata_filename_load_btn", "Load"),
                                         placeholder = "Select a raw data file to begin"
                               ),
                               fileInput(inputId = "sampleJSON_filename",
                                         label = NULL,
                                         width = "100%",
                                         multiple = FALSE,
                                         accept = ".JSON",
                                         buttonLabel = span(id = "sampleJSON_filename_load_btn", "Load"),
                                         placeholder = "Select a sample.JSON file to begin"
                               )
                        ),
                        column(2,
                               numericInput(inputId = "ms1zoom_low",
                                            label = "Lower MS1 window value",
                                            value = 0.5,
                                            min = 0,
                                            width = "100%"),
                               numericInput(inputId = "ms1zoom_high",
                                            label = "Upper MS1 window value",
                                            value = 4,
                                            min = 0,
                                            width = "100%"),
                               numericInput(inputId = "ms1matchlimit",
                                            label = "Minimum MS1 isotopic match score",
                                            value = 0.5,
                                            min = 0,
                                            max = 1,
                                            width = "100%"),
                               numericInput(inputId = "minerror",
                                            label = "Minimum m/z error of instrument",
                                            value = 0.01,
                                            min = 0,
                                            max = 1,
                                            width = "100%")
                        )
                    ),
                    fluidRow(
                      actionButton(inputId = "process_data_btn",
                                   label = "Process Data",
                                   width = "50%",
                                   icon = icon("circle-play")
                      )
                    ),
                    fluidRow(
                      textOutput(outputId = "qc_review_status")
                    )
            ),
            tabItem("qc_review",
                    selectizeInput(inputId = "select_peak", label = "Select peak", choices = NULL, width = "25%"),
                    textOutput(outputId = "overall_qc_results"),
                    selectizeInput(inputId = "select_qc_check", label = "Select QC Check", choices = NULL, width = "25%"),
                    DT::dataTableOutput(outputId = "qc_dt", width = "50%")
            ),
            tabItem("export",
                    h5("Export all QC data for sample."),
                    # textOutput(outputId = "output_dir"),
                    # shinyFiles::shinyDirButton(id = "export_dir", label = "Select a folder to export data", title = "Please select a folder"),
                    actionButton(inputId = "export_btn", label = "Export data to folder"),
                    textOutput(outputId = "export_status")
                    )
        )
    )
)
