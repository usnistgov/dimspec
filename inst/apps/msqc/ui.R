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
            menuItem("About",
                     tabName = "about",
                     icon = icon("question")
            ),
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
            ),
            menuItem("QC Settings",
                     tabName = "settings",
                     icon = icon("gears")
            )
        )
    ),
    body = dashboardBody(
        tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
        tabItems(
          tabItem("about",
                  h3("Quality Control Check Tool for Data Import"),
                  hr(),
                  includeHTML('about.html')
                  ),
            tabItem("data_import",
                    h3("Import Raw Data Files and Sample JSON Files"),
                    fluidRow(
                        column(4,
                               fileInput(inputId = "rawdata_filename",
                                         label = NULL,
                                         multiple = TRUE,
                                         width = "100%",
                                         accept = ".mzML",
                                         buttonLabel = span(id = "rawdata_filename_load_btn", "Load"),
                                         placeholder = "Select a raw data file to begin"
                               ),
                               fileInput(inputId = "sampleJSON_filename",
                                         label = NULL,
                                         width = "100%",
                                         multiple = TRUE,
                                         accept = ".JSON",
                                         buttonLabel = span(id = "sampleJSON_filename_load_btn", "Load"),
                                         placeholder = "Select a sample.JSON file to begin"
                               )
                        ),
                        column(4,
                               DT::dataTableOutput(outputId = "file_table",width = "100%")
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
                    h3("Review Quality Control check data."),
                    DT::dataTableOutput(outputId = "sample_qc", width = "50%"),
                    DT::dataTableOutput(outputId = "peak_qc", width = "50%"),
                    textOutput(outputId = "overall_qc_results"),
                    selectizeInput(inputId = "select_qc_check", label = "Select QC Check", choices = NULL, width = "25%"),
                    DT::dataTableOutput(outputId = "peak_data", width = "50%")
            ),
            tabItem("export",
                    h5("Export all QC data for sample."),
                    downloadButton(outputId = "export_btn", label = "Export all data", icon = icon("file-download")),
                    textOutput(outputId = "export_status")
                    ),
            tabItem("settings",
                    h3("Settings for QC analysis"),
                    column(2,
                           h4("Instrument Settings"),
                           numericInput(inputId = "ms1zoom_low",
                                        label = "Lower MS1 window value",
                                        value = 1,
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
                    )
        )
    )
)
