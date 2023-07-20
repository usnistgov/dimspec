dashboardPage(
    skin = "blue",
    title = APP_TITLE,
    header = dashboardHeader(
      title = a(
        img(src = "NIST-Logo-Brand-White.svg"),
        href = "https://www.nist.gov/programs-projects/measurement-science-and-polyfluoroalkyl-substances-pfas#:~:text=Overview%20of%20the%20NIST%20program%20on%20per-%20and,for%20a%20variety%20of%20commercial%20and%20industrial%20applications.",
        target = "_blank"
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
        h3("Currently connected to ", DB_TITLE, style = "padding: 5px; margin: 0px;"),
        sidebarMenu(
            id = "sidebar_menu",
            menuItem("Table Viewer",
                     tabName = "table_viewer",
                     icon = icon("eye")
            ),
            menuItem("Entity Relationship Diagram",
                     tabName = "erd",
                     icon = icon("sitemap")
            ),
            menuItem("About",
                     tabName = "about",
                     icon = icon("circle-info")
            )
        )
    ),
    body = dashboardBody(
        tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
        tabItems(
            tabItem("table_viewer",
                    fluidRow(
                        column(4,
                               selectizeInput(inputId = "selected_table",
                                              label = "Select a table to view",
                                              choices = table_list,
                                              multiple = FALSE),
                               htmlOutput(outputId = "dt_table_comments")
                        ),
                        column(8,
                               tags$label("Table data"),
                               DT::dataTableOutput(outputId = "dt_table")
                        )
                    )
            ),
            tabItem("erd",
                    img(src = "ERD.png",
                        width = "100%",
                        height = "100%"),
                    p(style = "font-size: smaller;", "Right click to open in another tab to view in full resolution.")
            ),
            tabItem("about",
                    includeHTML("about.html")
            )
        )
    )
)
