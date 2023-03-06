shinyServer(function(input, output) {
  observeEvent(input$browser, browser())
  output$dt_table <- renderDT({
    DT::datatable(data = api_endpoint(path = "table_search",
                                      query = list(table_name = input$selected_table),
                                      return_format = "data.frame"),
                  rownames = FALSE,
                  extensions = "Responsive",
                  filter = "top")
  })
  output$dt_table_comments <- renderUI({
    req(db_dict)
    this_dict <- db_dict[[input$selected_table]]
    this_map <- db_map[[input$selected_table]]
    table_info <- character(0)
    n_refs <- length(this_map$references_tables)
    if (n_refs > 0) {
      table_info <- c(
        table_info,
        glue::glue("references table{ifelse(n_refs > 1, 's', '')} {format_list_of_names(this_map$references_tables)}")
      )
    }
    n_norms <- length(this_map$normalizes_tables)
    if (n_norms > 0) {
      table_info <- c(
        table_info,
        glue::glue("normalizes table{ifelse(n_norms > 1, 's', '')} {format_list_of_names(this_map$normalizes_tables)}")
      )
    }
    n_views <- length(this_map$used_in_view)
    if (n_views > 0) {
      table_info <- c(
        table_info,
        glue::glue("is used in {format_list_of_names(this_map$used_in_view)}")
      )
    }
    field_references <- tibble(
      value = this_map$references
    ) %>%
      separate(value, into = c("name", "f_table", "f_column"), sep = " REFERENCES |\\(") %>%
      mutate(f_column = str_remove(f_column, "\\)"),
             field_links = glue::glue("<br>It references column <p style='color: blue; display: inline;'>{f_column}</p> in table <p style='color: blue; display: inline;'>{f_table}</p>."))
    tagList(
      # h3(input$selected_table),
      tagList(
        h4(unique(this_dict$table_comment)),
        if (length(table_info) > 0) p("This table ", format_list_of_names(table_info), ".") else NULL,
        span(
          this_dict %>%
            left_join(field_references, by = c("name" = "name")) %>%
            mutate(field_comments = str_to_sentence(field_comments),
                   field_links = replace_na(field_links, ""),
                   desc = glue::glue("<p style='display: inline;'><b>{name}</b>{ifelse(pk == 0, '', ' (primary key)')} is {ifelse(substr(type, 1, 1) %in% vowels, 'an', 'a')} <p style='color: blue; display: inline;'>{type}</p> column that <p style='display: inline; color: {ifelse(notnull == 0, 'blue;\\'>is NOT', 'red;\\'>is')} required</p>{ifelse(unique, ' and must be unique', '')}.<br>{field_comments}.{field_links}</p>")
            ) %>%
            pull(desc) %>%
            paste0() %>%
            HTML()
        )
      )
    )
  })
})
