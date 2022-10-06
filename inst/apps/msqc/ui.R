dashboardPage(
  skin = "blue",
  title = paste0(DB_TITLE, " - ", APP_TITLE),
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
    h3(APP_TITLE, style = "padding: 5px; margin: 0px;"),
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
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
    tags$script(type = "text/javascript", jscode),
    tabItems(
      tabItem("about",
              h3("Quality Control Check Tool for Data Import"),
              hr(),
              actionButton(inputId = "go_data_import",
                           label = "Click Here to get Started",
                           width = "100%",
                           icon = icon("circle-play", verify_fa = FALSE)),
              hr(),
              includeHTML('about.html')
      ),
      tabItem("data_import",
              div(class = "overlay",
                  id = "data_import_overlay",
                  img(src = "processing.gif"),
                  h3(id = "data_import_overlay_text")
              ),
              h3("Import Raw Data Files and Sample JSON Files"),
              fluidRow(
                column(4,
                       fileInput(inputId = "rawdata_filename",
                                 label = "1) Load raw mzML file(s)",
                                 multiple = TRUE,
                                 width = "100%",
                                 accept = ".mzML",
                                 buttonLabel = "Load",
                                 placeholder = "Select a raw data file to begin"
                       ),
                       checkboxInput(inputId = "has_lockmass",
                                     label = "These data were collected with a lock mass",
                                     value = FALSE,
                                     width = "100%"
                       ),
                       div(id = "lockmass_settings",
                           checkboxInput(inputId = "lockmass_correct",
                                         label = "Measurements are lock mass corrected",
                                         value = FALSE
                           ),
                           column(6,
                                  numericInput(inputId = "lockmass",
                                               label = "Lock mass (Da)",
                                               value = NA,
                                               width = "100%"
                                  )
                           ),
                           column(6,
                                  numericInput(inputId = "lockmass_width",
                                               label = "Lock mass width (Da)",
                                               value = NA,
                                               width = "100%"
                                  )
                           )
                       ),
                       fileInput(inputId = "sampleJSON_filename",
                                 label = "2) Load Sample JSON file(s)",
                                 width = "100%",
                                 multiple = TRUE,
                                 accept = ".JSON",
                                 buttonLabel = "Load",
                                 placeholder = "Select a sample.JSON file to begin"
                       ),
                       actionButton(inputId = "process_data_btn",
                                    label = "Process Data",
                                    width = "100%",
                                    icon = icon("circle-play", verify_fa = FALSE)
                       ),
                       span(id = "qc_results_span",
                            # h5("QC Data Import Status:"),
                            hr(),
                            textOutput(outputId = "qc_review_status"),
                            actionButton(inputId = "go_to_quality_review",
                                         label = "Quality Review",
                                         width = "100%",
                                         icon = icon("circle-play", verify_fa = FALSE))
                       )
                ),
                column(8,
                       DT::dataTableOutput(outputId = "file_table",width = "100%")
                )
              )
      ),
      tabItem("qc_review",
              fluidRow(
                column(4,
                       h3("Quality Control Checks"),
                       DT::dataTableOutput(outputId = "sample_qc", width = "100%"),
                       hr(),
                       span(id = "peak_qc_selector",
                            DT::dataTableOutput(outputId = "peak_qc", width = "100%"),
                            hr(),
                            textOutput(outputId = "overall_qc_results")
                       )
                ),
                column(8,
                       checkboxInput(inputId = "results_rendered", label = "New Results Rendered (hidden input)", value = FALSE),
                       uiOutput(outputId = "quality_data")
                )
              )
      ),
      tabItem("export",
              div(class = "overlay",
                  id = "export_overlay",
                  img(src = "processing.gif"),
                  h3(id = "export_overlay_text")
              ),
              h5("Export all QC data for sample."),
              downloadButton(outputId = "export_btn", label = "Export all data", icon = icon("file-download", verify_fa = FALSE)),
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
              ),
              column(2),
              column(4,
                     h4("Initial Uncertainty Mass Spectrum Settings"),
                     sliderInput(inputId = "max_correl",
                                 label = "Maximum correlation limit",
                                 min = 0,
                                 max = 1,
                                 value = 0.8,
                                 step = 0.1),
                     sliderInput(inputId = "correl_bin",
                                 label = "Correlation limit bin size",
                                 min = 0,
                                 max = 0.5,
                                 value = 0.1,
                                 step = 0.05),
                     sliderInput(inputId = "max_ph",
                                 label = "Maxmimum peak height limit",
                                 min = 0,
                                 max = 90,
                                 value = 10,
                                 step = 5),
                     sliderInput(inputId = "ph_bin",
                                 label = "Peak height limit bin size",
                                 min = 0,
                                 max = 10,
                                 value = 1,
                                 step = 1),
                     sliderInput(inputId = "max_freq",
                                 label = "Maxmimum observational frequency limit",
                                 min = 0,
                                 max = 90,
                                 value = 10,
                                 step = 5),
                     sliderInput(inputId = "freq_bin",
                                 label = "Observational frequency limit bin size",
                                 min = 0,
                                 max = 10,
                                 value = 1,
                                 step = 1),
                     numericInput(inputId = "min_n_peaks",
                                  label = "Minimum number of scans to include in mass spectrum",
                                  value = 3,
                                  min = 1,
                                  max = 10),
                     selectInput(inputId = "cormethod",
                                 label = "Correlation function method",
                                 choices = c("pearson", "kendall", "spearman"),
                                 selected = "pearson"
                     )
              )
      )
    )
  )
)
