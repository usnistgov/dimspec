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
        selectizeInput(inputId = "selected_table",
                       label = "Select a table to view",
                       choices = table_list,
                       multiple = FALSE)
    ),
    body = dashboardBody(
        h3("Plumber instance is live at ", PLUMBER_URL),
        p("View the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), "here")),
        DT::dataTableOutput(outputId = "dt_table")
    )
)
