dashboardPage(
  skin = "blue",
  title = DB_TITLE,
  header = dashboardHeader(
    title = a(
      img(src = "NIST-Logo-Brand-White.svg"),
      href = "#"
    )
  ),
  sidebar = dashboardSidebar(
    h4(style = "padding-left: 15px;", DB_TITLE),
    if (dev) {
      div(style = "padding-left: 15px;",
          h4(style = "padding-right: 15px; color: red; text-align: center;", "DEVELOPMENT MODE"),
          p("Plumber instance is live at ", PLUMBER_URL, "; view the API guide", a(href = sprintf("%s/__docs__/", PLUMBER_URL), target = "_blank", "here")),
          actionButton("browser", "Live Inspect", icon = icon("user-ninja"))
      )
    } else {
      NULL
    },
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Home",
               tabName = "index",
               icon = icon("house", verify_fa = FALSE)
      )
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "nist_style.css"),
    tags$script(type = "text/javascript", jscode),
    div(class = "title-banner", "DEVELOPMENT VERSION"),
    tabItems(
      # Home Page ----
      tabItem("index",
              h2(APP_TITLE, style = "margin: 0px;"),
              hr(),
              includeHTML("index.html")
      )
    )
  )
)
