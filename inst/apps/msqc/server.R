shinyServer(function(input, output) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Environment Objects ----
  import_results <- reactiveValues(processed_data = NULL, qc_results = list(), data_select = NULL, qc_check_select = NULL)
  data_react <- reactiveValues(compoundtable = api_endpoint(path = "table_search",
                                                query = list(table_name = "view_compounds"),
                                                return_format = "data.frame"),
                 exactmasses = api_endpoint(path = "table_search",
                                            query = list(table_name = "view_exact_masses"),
                                            return_format = "data.frame"),
                 exactmasschart = create_exactmasschart(api_endpoint(path = "table_search",
                                                                     query = list(table_name = "view_element_isotopes"),
                                                                                  return_format = "data.frame")),
                 export_dir = paste0(getwd(), "/data/")
  )
  
  if (is.null(isolate(import_results$processed_data))) {
    output$qc_review_status <- renderText("No processed data.")
    import_results$qc_results <- list()
    }
  
  # Process data ----
  observeEvent(input$process_data_btn, isolate({
    mzml <- mzMLtoR(input$rawdata_filename$datapath)
    samplejson <- parse_methodjson(input$sampleJSON_filename$datapath)
    
    # Import data quality check
    
    # Process data
    import_results$processed_data <- peak_gather_json(samplejson, mzml, data_react$compoundtable)
    
    for (j in 1:length(import_results$processed_data)) {
      qc <- gather_qc(import_results$processed_data[[j]],
                      exactmasses = data_react$exactmasses,
                      exactmasschart = data_react$exactmasschart,
                      ms1range = c(input$ms1zoom_low, input$ms1zoom_high),
                      ms1isomatchlimit = input$ms1matchlimit,
                      minerror = input$minerror
                      )
        import_results$qc_results[[j]] <- qc
    }
    
    # put out the various updates
    import_results$data_select <- sapply(import_results$processed_data, function(x) paste("m/z ", x$peak$mz[1], " @ ", x$peak$rt[1], " min", sep = ""))
    updateSelectizeInput(inputId = "select_peak", choices = import_results$data_select, selected = 1)
    output$qc_review_status <- renderText("Data processing complete!")
    print("Complete!")
  }))
  
  # QC Review functions ---
  observeEvent(input$select_peak, isolate({
    ind <- which(import_results$data_select == input$select_peak)
    if (length(ind) != 0) {
      dat <- import_results$processed_data[[ind]]
      qc <- import_results$qc_results[[ind]]
      all_results <- do.call(c, lapply(qc, function(x) c(x$result)))
      if (FALSE %in% all_results) {output$overall_qc_results <- renderText(paste0("There are ", length(which(all_results == FALSE)), " failed QC checks for this peak."))}
      if (!FALSE %in% all_results) {output$overall_qc_results <- renderText("There are no failed QC checks for this peak.")}
      import_results$qc_check_select <- unique(do.call(c, lapply(qc, function(x) c(x$parameter))))
      updateSelectizeInput(inputId = "select_qc_check", choices = import_results$qc_check_select, selected = 1)
    }
  }))
  
  observeEvent(input$select_qc_check, isolate({
    ind <- which(import_results$data_select == input$select_peak)
    if (length(ind) != 0) {
      dat <- import_results$processed_data[[ind]]
      qc <- import_results$qc_results[[ind]]
      ind2 <- which(import_results$qc_check_select == input$select_qc_check)
      if (length(ind2) != 0) {
        outtable <- qc[[ind2]]
        outtable <- outtable[,-1]
        output$qc_dt <- DT::renderDataTable(outtable)
      }
    }
  }))
  
  #shinyFiles::shinyDirChoose(input, id = "export_dir", roots = getVolumes())
  
  # observeEvent(input$export_dir, {
  #   if (!is.null(input$export_dir)) {
  #     output$output_dir <- renderText(input$export_dir)
  #   }
  #   if (is.null(input$export_dir)) {
  #     output$output_dir <- renderText("No directory has been selected")
  #   }
  # })
  
  observeEvent(input$export_btn, isolate({
    if (!is.null(data_react$export_dir)) {
      for (i in 1:length(import_results$processed_data)) {
        outdat <- import_results$processed_data[[i]]
        outdat$qc <- import_results$qc_results[[i]]
        write_json(outdat, paste(data_react$export_dir, gsub("\\.", "_", outdat$sample$name),
                                 "_cmpd", outdat$compounddata$id, ".JSON", sep = ""), auto_unbox = TRUE,
                   pretty = TRUE)
      }
      output$export_status <- renderText("All files have been exported.")
    }
  }))
})
