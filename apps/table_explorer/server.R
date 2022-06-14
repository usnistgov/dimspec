shinyServer(function(input, output) {
  output$dt_table <- renderDT({
    DT::datatable(data = api_endpoint(path = "table_search",
                                      query = list(table_name = input$selected_table),
                                      return_format = "data.frame"),
                  rownames = FALSE)
  })
  
})
