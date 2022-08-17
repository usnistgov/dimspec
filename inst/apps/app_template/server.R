shinyServer(function(input, output, session) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Session Data ----
  
  # Style adjustments ----
  
  # Element Display ----
  
  # Navigation ----
  observeEvent(input$sidebar_menu, {
    if (!dev) {
    }
  })
  
  # Dashboard pages ----
})
  