shinyServer(function(input, output) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Environment Objects ----
  import_results <- reactiveValues(processed_data = NULL, 
                                   qc_results = list(), 
                                   data_select = NULL, 
                                   qc_check_select = NULL,
                                   file_dt = data.frame(RawFile = NULL, SampleJSON = NULL, Valid = NULL),
                                   sample_list = NULL,
                                   peak_list = NULL
                                   )
  data_react <- reactiveValues(compoundtable = NULL,
                 exactmasses = NULL,
                 exactmasschart = NULL,
                 export_dir = paste0(getwd(), "/data/")
  )
  
  if (is.null(isolate(import_results$processed_data))) {
    output$qc_review_status <- renderText("No processed data.")
    import_results$qc_results <- list()
  }
  
  # Element Display ----
  hideElement("process_data_btn")
  
  # Check data ----
  observeEvent(input$rawdata_filename, isolate({
    if (!is.null(input$rawdata_filename) & !is.null(input$sampleJSON_filename)) {
      sampleJSONs <- lapply(input$sampleJSON_filename$datapath, parse_methodjson)
      sampleJSON_name <- input$sampleJSON_filename$name
      RawFile <- input$rawdata_filename$name
      sampleJSON_raw <- sapply(sampleJSONs, function(x) x$sample$name)
      SampleJSON <- rep(NA, length(RawFile))
      Valid <- rep(FALSE, length(RawFile))
      for (i in 1:length(RawFile)) {
        ind <- which(sampleJSON_raw == RawFile[i])
        if (length(ind) > 0) {
          SampleJSON[i] <- sampleJSON_name[ind]
          Valid[i] <- TRUE
        }
      }
      output$file_table <- DT::renderDataTable(data.frame(RawFile = RawFile, SampleJSON = SampleJSON, Valid = Valid))
      showElement("process_data_btn")
    }
  }))
  
  observeEvent(input$sampleJSON_filename, isolate({
    if (!is.null(input$rawdata_filename) & !is.null(input$sampleJSON_filename)) {
      sampleJSONs <- lapply(input$sampleJSON_filename$datapath, parse_methodjson)
      sampleJSON_name <- input$sampleJSON_filename$name
      RawFile <- input$rawdata_filename$name
      sampleJSON_raw <- sapply(sampleJSONs, function(x) x$sample$name)
      SampleJSON <- rep(NA, length(RawFile))
      Valid <- rep(FALSE, length(RawFile))
      for (i in 1:length(RawFile)) {
        ind <- which(sampleJSON_raw == RawFile[i])
        if (length(ind) > 0) {
          SampleJSON[i] <- sampleJSON_name[ind]
          Valid[i] <- TRUE
        }
      }
      import_results$file_dt <- data.frame(RawFile = RawFile, SampleJSON = SampleJSON, Valid = Valid)
      output$file_table <- DT::renderDataTable(DT::datatable(import_results$file_dt,
                                                            options = list(paging = FALSE, searching = FALSE, ordering = FALSE)))
      showElement("process_data_btn")
    }
  }))
  
  # Process data ----
  # Process data ----
  observeEvent(input$process_data_btn, isolate({
    #get reference information
    data_react$compoundtable <- api_endpoint(path = "table_search",
                                             query = list(table_name = "view_compounds"),
                                             return_format = "data.frame")
    data_react$exactmasses <-  api_endpoint(path = "table_search",
                                            query = list(table_name = "view_exact_masses"),
                                            return_format = "data.frame")
    data_react$exactmasschart <- create_exactmasschart(api_endpoint(path = "table_search",
                                                                    query = list(table_name = "view_element_isotopes"),
                                                                    return_format = "data.frame"))
    
    
    # Import Data into Environment
    if (!is.null(input$rawdata_filename) & !is.null(input$sampleJSON_filename) & !is.null(import_results$file_dt)) {
      for (i in 1:nrow(import_results$file_dt)) {
        if (import_results$file_dt$Valid[i] == TRUE) {
          mzml <- mzMLtoR(input$rawdata_filename$datapath[which(input$rawdata_filename$name == import_results$file_dt$RawFile[i])])
          samplejson <- parse_methodjson(input$sampleJSON_filename$datapath[which(input$sampleJSON_filename$name == import_results$file_dt$SampleJSON[i])])
          import_results$processed_data[[i]] <- peak_gather_json(samplejson, mzml, data_react$compoundtable, zoom = c(input$ms1zoom_low, input$ms1zoom_high), minerror = input$minerror)
        }
      }
      
      # Process data
      peak_results <- list()
      sample_results <- rep(FALSE, length(import_results$processed_data))
      for (j in 1:length(import_results$processed_data)) {
        import_results$qc_results[[j]] <- list()
        peak_results[[j]] <- rep(FALSE, length(import_results$processed_data[[j]]))
        for (i in 1:length(import_results$processed_data[[j]])) {
          qc <- gather_qc(import_results$processed_data[[j]][[i]],
                          exactmasses = data_react$exactmasses,
                          exactmasschart = data_react$exactmasschart,
                          ms1range = c(input$ms1zoom_low, input$ms1zoom_high),
                          ms1isomatchlimit = input$ms1matchlimit,
                          minerror = input$minerror,
                          max_correl = input$max_correl,
                          correl_bin = input$correl_bin,
                          max_ph = input$max_ph,
                          ph_bin = input$ph_bin,
                          max_freq = input$max_freq,
                          freq_bin = input$freq_bin,
                          min_n_peaks = input$min_n_peaks,
                          cormethod = input$cormethod
          )
          import_results$qc_results[[j]][[i]] <- qc
          
          # populate summary tables
          all_results <- do.call(c, lapply(qc, function(x) c(x$result)))
          if (!FALSE %in% all_results) {peak_results[[j]][i] <- TRUE}
        }
        import_results$peak_list[[j]] <- data.frame(peak = sapply(import_results$processed_data[[j]], function(x) x$peak$name), PassCheck = peak_results[[j]])
        sample_results[j] <- FALSE
        if (!FALSE %in% peak_results[[j]]) {sample_results[j] <- TRUE}
      }
      
      # put out the various updates
      import_results$sample_list <- data.frame(RawFile = import_results$file_dt$RawFile, PassCheck = sample_results)
      output$sample_qc <- DT::renderDataTable(DT::datatable(import_results$sample_list, 
                                                            options = list(
                                                              paging = FALSE,
                                                              filtering = FALSE,
                                                              ordering = FALSE,
                                                              searching = FALSE
                                                            ),
                                                            selection = "single"
      ))
      output$qc_review_status <- renderText("Data processing complete!")
      print("Complete!")
    }
  }))
  
  # QC Review functions ---

  observeEvent(input$sample_qc_rows_selected, isolate({
    if (!is.null(input$sample_qc_rows_selected)) {
      output$peak_qc <- DT::renderDataTable(DT::datatable(import_results$peak_list[[input$sample_qc_rows_selected]],
                                                          options = list(
                                                            paging = FALSE,
                                                            filtering = FALSE,
                                                            ordering = FALSE,
                                                            searching = FALSE
                                                          ),
                                                          selection = "single"
                                                          ))
    }
  }))
  
  observeEvent(input$peak_qc_rows_selected, isolate({
    if (!is.null(input$peak_qc_rows_selected) & !is.null(input$sample_qc_rows_selected)) {
      dat <- import_results$processed_data[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
      qc <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
      all_results <- do.call(c, lapply(qc, function(x) c(x$result)))
      if (FALSE %in% all_results) {output$overall_qc_results <- renderText(paste0("There are ", length(which(all_results == FALSE)), " failed QC checks for this peak."))}
      if (!FALSE %in% all_results) {output$overall_qc_results <- renderText("There are no failed QC checks for this peak.")}
      import_results$qc_check_select <- unique(do.call(c, lapply(qc, function(x) c(x$parameter))))
      updateSelectizeInput(inputId = "select_qc_check", choices = import_results$qc_check_select, selected = 1)
    }
  }))
  
  observeEvent(input$select_qc_check, isolate({
    if (!is.null(input$peak_qc_rows_selected) & !is.null(input$sample_qc_rows_selected)) {
        dat <- import_results$processed_data[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
        qc <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
        ind <- which(import_results$qc_check_select == input$select_qc_check)
        if (length(ind) != 0) {
          outtable <- qc[[ind]]
          outtable <- outtable[,-1]
          output$peak_data <- DT::renderDataTable(DT::datatable(outtable, options = list(filtering = FALSE, ordering = FALSE, paging = FALSE, searching = FALSE)))
        }
      
    }
  }))
  
  
# Export Functions ---- 
  
output$export_btn <- downloadHandler(
  filename = function() {
    paste0("database_import_files", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_directory)
    
    for (j in 1:length(import_results$processed_data)) {
      for (i in 1:length(import_results$processed_data[[j]])) {
        outdat <- import_results$processed_data[[j]][[i]]
        outdat$qc <- import_results$qc_results[[j]][[i]]
        write_json(outdat, paste0(temp_directory, "/", gsub("\\.", "_", outdat$sample$name), "_cmpd", outdat$compounddata$id, ".JSON"),
                   auto_unbox = TRUE,
                   pretty = TRUE)
      }
    }
    
    zip::zip(zipfile = file,
             files = dir(temp_directory),
             root = temp_directory)
    
  },
  contentType = "application/zip"
)
  
})
