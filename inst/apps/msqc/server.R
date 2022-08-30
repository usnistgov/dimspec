shinyServer(function(input, output) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Environment Objects ----
  import_results <- reactiveValues(
    processed_data = NULL, 
    qc_results = list(), 
    data_select = NULL, 
    qc_check_select = NULL,
    file_dt = NULL,
    sample_list = NULL,
    peak_list = NULL
  )
  data_react <- reactiveValues(
    compoundtable = NULL,
    exactmasses = exactmasses,
    exactmasschart = exactmasschart,
    export_dir = paste0(getwd(), "/data/")
  )
  file_rawdata <- reactiveVal(NULL)
  observeEvent(input$rawdata_filename, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (all(valid_file_format(input$rawdata_filename$name, ".mzML"))) {
      out <- input$rawdata_filename
    } else {
      reset("rawdata_filename")
      out <- NULL
    }
    file_rawdata(out)
  })
  file_samplejson <- reactiveVal(NULL)
  observeEvent(input$sampleJSON_filename, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (all(valid_file_format(input$sampleJSON_filename$name, ".json"))) {
      out <- input$sampleJSON_filename
    } else {
      reset("sampleJSON_filename")
      out <- NULL
    }
    file_samplejson(out)
  })
  qc_check_results <- reactive({
    req(import_results$processed_data,
        input$peak_qc_rows_selected,
        input$sample_qc_rows_selected)
    list(
      dat = import_results$processed_data[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]],
      qc = import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
    )
  })
  peak_data <- reactive({
    req(import_results$processed_data,
        import_results$qc_results,
        input$select_qc_check,
        input$sample_qc_rows_selected,
        input$peak_qc_rows_selected)
    dat <- import_results$processed_data[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
    qc <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
    ind <- which(import_results$qc_check_select == input$select_qc_check)
    if (length(ind) != 0) {
      outtable <- qc[[ind]][,-1]
    } else {
      NULL
    }
  })
  observe({
    req(file_rawdata(), file_samplejson())
    sampleJSONs <- lapply(file_samplejson()$datapath, parse_methodjson)
    sampleJSON_name <- file_samplejson()$name
    RawFile <- file_rawdata()$name
    sampleJSON_raw <- sapply(sampleJSONs, function(x) x$sample$name)
    SampleJSON <- rep(NA, length(RawFile))
    Valid <- rep(FALSE, length(RawFile))
    rawfiles <- tibble(
      RawFile = RawFile,
      Valid = RawFile %in% sampleJSON_raw
    )
    file_check <- rawfiles %>%
      left_join(
        tibble(sampleJSON_raw = sampleJSON_raw, SampleJSON = sampleJSON_name),
        by = c("RawFile" = "sampleJSON_raw")
      )
    if (!nrow(file_check) == length(sampleJSON_raw)) {
      no_match <- sampleJSON_name[!sampleJSON_name %in% file_check$SampleJSON]
      nist_shinyalert(
        title = "Inapplicable JSON Files",
        type = "warning",
        text = tagList(
          h3(sprintf("%d files did not contain matching data.", length(no_match))),
          h4("These will be excluded from QC checks."),
          HTML(paste0(no_match, collapse = "\n"))
        )
      )
    }
    import_results$file_dt <- file_check
  })
  
  output$qc_review_status <- renderText({
    shiny::validate(
      need(!is.null(import_results$processed_data), "Could not process these data.")
    )
    "Data processing complete."
  })
  
  # Element Display ----
  observe({
    toggleElement(id = "process_data_btn", condition = !is.null(import_results$file_dt))
    toggleElement(id = "qc_results_span", condition = !is.null(import_results$processed_data))
  })
  hideElement("results_rendered")
  
  observeEvent(input$go_data_import, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "data_import")
  })
  
  # Check data ----
  output$file_table <- renderDT(
    DT::datatable(
      data = req(import_results$file_dt),
      rownames = FALSE,
      options = list(paging = FALSE, searching = FALSE, ordering = FALSE)
    )
  )
  
  # Process data ----
  observeEvent(input$process_data_btn, {
    req(import_results$file_dt)
    #get reference information
    data_react$compoundtable <- api_endpoint(path = "table_search",
                                             query = list(table_name = "view_compounds"),
                                             return_format = "data.frame")
    
    # Import Data into Environment
    
    withProgress(message = "Processing Data", value = 0, {
      check_name <- ""
      n <- nrow(import_results$file_dt)
      for (i in 1:n) {
        if (import_results$file_dt$Valid[i] == TRUE) {
          if (!check_name == import_results$file_dt$RawFile[i]) {
            mzml <- mzMLtoR(input$rawdata_filename$datapath[which(input$rawdata_filename$name == import_results$file_dt$RawFile[i])])
          }
          samplejson <- parse_methodjson(input$sampleJSON_filename$datapath[which(input$sampleJSON_filename$name == import_results$file_dt$SampleJSON[i])])
          import_results$processed_data[[i]] <- peak_gather_json(samplejson, mzml, data_react$compoundtable, zoom = c(input$ms1zoom_low, input$ms1zoom_high), minerror = input$minerror)
          check_name <- import_results$file_dt$RawFile[i]
        }
        incProgress(amount = 1/n, detail = sprintf("Processing entry %d of %d", i, n))
      }
    })
    
    # Process data
    peak_results <- list()
    sample_results <- rep(FALSE, length(import_results$processed_data))
    withProgress(message = "Processing Data", value = 0, {
      nj <- length(import_results$processed_data)
      ntotal <- sum(sapply(import_results$processed_data, length))
      for (j in 1:nj) {
        import_results$qc_results[[j]] <- list()
        peak_results[[j]] <- rep(FALSE, length(import_results$processed_data[[j]]))
        ni <- length(import_results$processed_data[[j]])
        for (i in 1:ni) {
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
          incProgress(amount = 1/ntotal, detail = sprintf("File %d of %d - Gathering quality control data %d of %d.", j, nj, i, ni))
        }
        import_results$peak_list[[j]] <- data.frame(peak = sapply(import_results$processed_data[[j]], function(x) x$peak$name), PassCheck = peak_results[[j]])
        sample_results[j] <- FALSE
        if (!FALSE %in% peak_results[[j]]) {sample_results[j] <- TRUE}
      }
    })
    
    # put out the various updates
    import_results$sample_list <- data.frame(RawFile = import_results$file_dt$RawFile, PassCheck = sample_results)
    print("Complete!")
  })
  
  output$sample_qc <- renderDT(
    DT::datatable(
      data = req(import_results$sample_list), 
      rownames = FALSE,
      options = list(
        paging = FALSE,
        filtering = FALSE,
        ordering = FALSE,
        searching = FALSE
      ),
      selection = "single"
    )
  )
  
  # QC Review functions ---
  output$peak_qc <- renderDT(
    DT::datatable(
      data = req(import_results$peak_list)[[req(input$sample_qc_rows_selected)]],
      rownames = FALSE,
      options = list(
        paging = FALSE,
        filtering = FALSE,
        ordering = FALSE,
        searching = FALSE
      ),
      selection = "single"
    )
  )
  
  output$overall_qc_results <- renderText({
    qc <- req(qc_check_results())
    all_results <- do.call(c, lapply(qc, function(x) c(x$result)))
    ifelse(any(FALSE %in% all_results),
           paste0("There are ", length(which(all_results == FALSE)), " failed QC checks for this peak."),
           "There are no failed QC checks for this peak."
    )
  })
  
  observeEvent(input$go_to_quality_review, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "qc_review")
  })
  
  observeEvent(qc_check_results()$qc, ignoreNULL = TRUE, {
    import_results$qc_check_select <- unique(do.call(c, lapply(qc_check_results()$qc, function(x) c(x$parameter))))
  })
  
  # observeEvent(import_results$qc_check_select, ignoreNULL = TRUE, {
  #   updateSelectizeInput(inputId = "select_qc_check", choices = import_results$qc_check_select, selected = 1)
  # })
  output$quality_data <- renderUI({
    req(import_results)
    shiny::validate(
      need(!is.null(input$sample_qc_rows_selected), message = "Please select a file on the left."),
      need(!is.null(input$peak_qc_rows_selected), message = "Please select a peak on the left.")
    )
    qc_data <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
    tagList(
      h3("Quality Control Metrics"),
      p("Click to expand or collapse details for any given metric."),
      lapply(qc_data,
             function(x) {
               span(
                 id = sprintf("qc_result_box_%s", unique(x$parameter)),
                 box(
                   title = x$parameter %>%
                     unique() %>%
                     str_replace_all("_", " ") %>%
                     str_to_title(),
                   solidHeader = FALSE,
                   height = "auto",
                   status = "primary",
                   width = "100%",
                   collapsible = TRUE,
                   collapsed = FALSE,
                   # Will not render properly while collapsed until a screen resize
                   DT::datatable(
                     data = select(x, -parameter),
                     rownames = FALSE,
                     width = "100%",
                     height = "auto",
                     extensions = "Responsive",
                     options = list(
                       dom = "t"
                     )
                   ),
                 )
               )
             }),
      tags$script('Shiny.setInputValue("results_rendered", "TRUE")'),
      tags$script("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
    )
  })
  
  observeEvent(input$results_rendered, {
    if (input$results_rendered) {
      # Collapse all result boxes but the first
      qc_data <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
      qc_names <- lapply(qc_data, function(x) unique(x$parameter)) %>%
        unlist() %>% unname()
      if (length(qc_names) > 1) {
      lapply(qc_names[-1],
             function(x) {
               runjs(sprintf('$("#qc_result_box_%s > div > div > div.box-header > div > button").click()', x))
             })
      }
      updateCheckboxInput(inputId = "results_rendered", value = FALSE)
    }
  })
  
  output$peak_data <- renderDT(
    DT::datatable(
      data = req(peak_data()),
      options = list(filtering = FALSE, ordering = FALSE, paging = FALSE, searching = FALSE)
    )
  )
  
  
# Export Functions ---- 
  
output$export_btn <- downloadHandler(
  filename = function() {
    paste0("database_import_files", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
    dir.create(temp_directory)
    withProgress(message = "Creating download file.", value = 0, {
      nj <- length(import_results$processed_data)
      ntotal <- sapply(import_results$processed_data, length) + 1
      for (j in 1:nj) {
        ni <- length(import_results$processed_data[[j]])
        for (i in 1:ni) {
          outdat <- import_results$processed_data[[j]][[i]]
          outdat$qc <- import_results$qc_results[[j]][[i]]
          write_json(outdat, paste0(temp_directory, "/", gsub("\\.", "_", outdat$sample$name), "_cmpd", outdat$compounddata$id, ".JSON"),
                     auto_unbox = TRUE,
                     pretty = TRUE)
        }
        incProgress(amount = 1/ntotal, details = paste0("Input file %d of %d, writing peak JSON file %d of %d.", j, nj, i, ni))
      }
      zip::zip(zipfile = file,
               files = dir(temp_directory),
               root = temp_directory)
      incProgress(amount = 1/ntotal, details = "Zipping up results.")
    })
    
  },
  contentType = "application/zip"
)
  
})
