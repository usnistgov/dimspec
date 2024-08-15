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
  # Sidebar ----
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
      menuItem("Home",
               tabName = "about",
               icon = icon("house")
      ),
      menuItem("Data Import",
               tabName = "data_import",
               icon = icon("file")
      ),
      menuItem("Quality Review",
               tabName = "qc_review",
               icon = icon("check-double")
      ),
      menuItem("QC Settings",
               tabName = "settings",
               icon = icon("gears")
      )
    )
  ),
  # Body ----
  body = dashboardBody(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
    tags$script(type = "text/javascript", jscode),
    tabItems(
      ## About ----
      tabItem("about",
              h3("Quality Control Check Tool for Data Import"),
              hr(),
              actionButton(inputId = "go_to_data_import",
                           label = "Click Here to get Started",
                           width = "100%",
                           icon = icon("circle-play", verify_fa = FALSE)),
              hr(),
              includeHTML('about.html')
      ),
      ## Data Import ----
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
                                 label = tags$strong("1) Load raw mzML file(s)"),
                                 multiple = TRUE,
                                 width = "100%",
                                 accept = ".mzML",
                                 buttonLabel = "Load",
                                 placeholder = "Select a raw data file to begin"
                       ),
                       uiOutput(outputId = "rawdata_filename_list"),
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
                       hr(style = "border-color: #d2d6de;"),
                       fileInput(inputId = "sampleJSON_filename",
                                 label = tags$strong("2) Load Sample JSON file(s)"),
                                 width = "100%",
                                 multiple = TRUE,
                                 accept = ".JSON",
                                 buttonLabel = "Load",
                                 placeholder = "Select a sample.JSON file to begin"
                       ),
                       uiOutput(outputId = "sampleJSON_filename_list"),
                       uiOutput(outputId = "processing_issues")
                ),
                column(8,
                       DT::dataTableOutput(outputId = "file_table",width = "100%"),
                       hr(),
                       actionButton(inputId = "go_to_settings",
                                    label = "Change QC Settings",
                                    width = "100%",
                                    icon = icon("gears", verify_fa = FALSE)
                       ),
                       actionButton(inputId = "process_data_btn",
                                    label = "Process Data",
                                    width = "100%",
                                    icon = icon("circle-play", verify_fa = FALSE)
                       ),
                       actionButton(inputId = "go_to_quality_review",
                                    label = "Quality Review",
                                    width = "100%",
                                    icon = icon("circle-play", verify_fa = FALSE)
                       )
                )
              )
      ),
      ## Quality Review ----
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
                       ),
                       hr(),
                       downloadButton(outputId = "export_from_review", label = "Export Data for import to DIMSpec", icon = icon("download"))
                ),
                column(8,
                       checkboxInput(inputId = "results_rendered", label = "New Results Rendered (hidden input)", value = FALSE),
                       uiOutput(outputId = "quality_data")
                )
              )
      ),
      ## QC Settings ----
      tabItem("settings",
              h3("Settings for QC analysis"),
              column(3,
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
                                  width = "100%"),
                     actionButton(  
                       inputId = "go_to_data_import2",
                       label = "Go To Data Import",
                       icon = icon("circle-play", verify_fa = FALSE),
                       width = "100%"
                     )
              ),
              column(9,
                     h4("Initial Uncertainty Mass Spectrum Settings"),
                     column(6,
                            sliderInput(inputId = "min_n_peaks",
                                        label = "Minimum number of scans to include in mass spectrum",
                                        value = 3,
                                        min = 1,
                                        step = 1,
                                        max = 10,
                                        width = "100%"),
                            sliderInput(inputId = "max_correl",
                                        label = "Maximum correlation limit",
                                        min = 0,
                                        max = 1,
                                        value = 0.8,
                                        step = 0.1,
                                        width = "100%"),
                            sliderInput(inputId = "correl_bin",
                                        label = "Correlation limit bin size",
                                        min = 0,
                                        max = 0.5,
                                        value = 0.1,
                                        step = 0.05,
                                        width = "100%"),
                            selectInput(inputId = "cormethod",
                                        label = "Correlation function method",
                                        choices = c("pearson", "kendall", "spearman"),
                                        selected = "pearson",
                                        width = "100%"
                            )
                     ),
                     column(6,
                            sliderInput(inputId = "max_freq",
                                        label = "Maxmimum observational frequency limit",
                                        min = 0,
                                        max = 90,
                                        value = 10,
                                        step = 5,
                                        width = "100%"),
                            sliderInput(inputId = "freq_bin",
                                        label = "Observational frequency limit bin size",
                                        min = 0,
                                        max = 10,
                                        value = 1,
                                        step = 1,
                                        width = "100%"),
                            sliderInput(inputId = "max_ph",
                                        label = "Maxmimum peak height limit",
                                        min = 0,
                                        max = 90,
                                        value = 10,
                                        step = 5,
                                        width = "100%"),
                            sliderInput(inputId = "ph_bin",
                                        label = "Peak height limit bin size",
                                        min = 0,
                                        max = 10,
                                        value = 1,
                                        step = 1,
                                        width = "100%")
                     )
              )
      )
    )
  )
)
