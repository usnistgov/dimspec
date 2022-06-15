# Define UI for application that draws a histogram
dashboardPage(
    skin = "blue",
    title = DB_TITLE,
    header = dashboardHeader(
        title = img(src = "NIST_logo.png",
                    height = "40px",
                    width = "152px")
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
            menuItem("Table Viewer",
                     tabName = "table_viewer",
                     icon = icon("eye")
            ),
            menuItem("Entity Relationship Diagram",
                     tabName = "erd",
                     icon = icon("sitemap")
            )
        )
    ),
    body = dashboardBody(
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
            )
        )
    )
)
