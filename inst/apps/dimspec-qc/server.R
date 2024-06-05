shinyServer(function(input, output) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Environment Objects ----
  import_results <- reactiveValues(
    processed_data = NULL, 
    qc_results = list(), 
    opt_ums_params = list(),
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
    if (all(sapply(input$rawdata_filename$name, valid_file_format, accepts = app_settings$rawdata_import_file_types))) {
      out <- input$rawdata_filename
    } else {
      reset("rawdata_filename")
      out <- NULL
    }
    file_rawdata(out)
  })
  file_samplejson <- reactiveVal(NULL)
  observeEvent(input$sampleJSON_filename, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (all(sapply(input$sampleJSON_filename$name, valid_file_format, accepts = app_settings$methodjson_import_file_types))) {
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
      qc = import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]],
      opt_ums_params = import_results$opt_ums_params[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
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
    opt_ums_params <- import_results$opt_ums_params[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
    ind <- which(import_results$qc_check_select == input$select_qc_check)
    if (length(ind) != 0) {
      outtable <- qc[[ind]][,-1]
    } else {
      NULL
    }
  })
  observe({
    req(file_rawdata(), file_samplejson())
    import_results$processed_data = NULL
    import_results$qc_results = list()
    import_results$opt_ums_params = list()
    import_results$data_select = NULL
    import_results$qc_check_select = NULL
    import_results$file_dt = NULL
    import_results$sample_list = NULL
    import_results$peak_list = NULL
    sampleJSONs <- lapply(file_samplejson()$datapath, parse_methodjson)
    sampleJSON_name <- file_samplejson()$name
    RawFile <- file_rawdata()$name
    sampleJSON_raw <- sapply(sampleJSONs, function(x) x$sample$name)
    # Catch case of unenforced NTA-MRT sample name entry without the file extension.
    # Only replace those where the extension is blank.
    if (any(tools::file_ext(sampleJSON_raw) == "")) {
      for (i in 1:length(sampleJSON_raw)) {
        target_ext <- tools::file_ext(RawFile[i])
        if (tools::file_ext(sampleJSON_raw[i]) == "") {
          sampleJSONs[[i]]$sample$name <- paste0(sampleJSONs[[i]]$sample$name, ".", target_ext)
          sampleJSON_raw[i] <- paste0(sampleJSON_raw[i], ".", target_ext)
        }
      }
    }
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
    if (any(!file_check$Valid)) {
      no_sample_file <- file_check$RawFile[!file_check$Valid]
      nist_shinyalert(
        title = sprintf(
          "Unable to match .%s files to .%s files",
          str_flatten_comma(unique(tools::file_ext(sampleJSON_name))),
          str_flatten_comma(unique(tools::file_ext(no_sample_file)))
        ),
        type = "warning",
        text = tagList(
          h3(sprintf("%d instrument file(s) did not match sample file(s).", length(no_sample_file))),
          p("Typically this is because the name in the sample file(s) does not exactly match that in the instrument file(s). Perhaps you chose the wrong file? These will be excluded from QC checks."),
          br(),
          HTML(paste0(no_sample_file, collapse = "\n"))
        )
      )
    }
    if (any(!sampleJSON_raw %in% file_check$RawFile)) {
      no_instrument_file <- setdiff(sampleJSON_name, file_check$SampleJSON)
      nist_shinyalert(
        title = sprintf(
          "Unable to match .%s files to .%s files",
          str_flatten_comma(unique(tools::file_ext(no_instrument_file))),
          str_flatten_comma(unique(tools::file_ext(sampleJSON_name)))
        ),
        type = "warning",
        text = tagList(
          h3(sprintf("%d sample file(s) did not match instrument file(s).", length(no_instrument_file))),
          p("Typically this is because the name in the instrument file(s) does not exactly match that in the sample file(s). Perhaps you chose the wrong file? These will be excluded from QC checks."),
          br(),
          HTML(paste0(no_instrument_file, collapse = "\n"))
        )
      )
    }
    file_check <- file_check[file_check$Valid, ]
    if (nrow(file_check) > 0) {
      import_results$file_dt <- file_check
    } else {
      nist_shinyalert(
        title = "No Matches",
        type = "error",
        text = "None of the provided sample files could be matched with the provided instrument files."
      )
      import_results$file_dt <- NULL
    }
  })
  
  output$qc_review_status <- renderText({
    shiny::validate(
      need(!is.null(import_results$processed_data), "Could not process these data.")
    )
    "Data processing complete."
  })
  
  # Element Display ----
  observe({
    toggleElement(id = "process_data_btn", condition = !is.null(import_results$file_dt) && is.null(import_results$processed_data))
    toggleElement(id = "peak_qc_selector", condition = !is.null(input$sample_qc_rows_selected))
    toggleElement(id = "qc_results_span", condition = !is.null(import_results$processed_data))
    toggleElement(id = "lockmass_settings", condition = input$has_lockmass)
  })
  hideElement("results_rendered")
  hideElement("data_import_overlay")
  hideElement("export_overlay")
  
  observeEvent(input$go_data_import, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "data_import")
  })
  
  observeEvent(input$sidebar_menu, {
    if (input$sidebar_menu %in% c("qc_review", "export")) {
      if (is.null(import_results$processed_data)) {
        nist_shinyalert(
          title = "No data",
          type = "info",
          text = "Please load and process data before using this page."
        )
        updateTabsetPanel(inputId = "sidebar_menu", selected = "data_import")
      }
    }
  })
  
  # Check data ----
  output$file_table <- renderDT(
    DT::datatable(
      data = req(import_results$file_dt),
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
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
    runjs("$('#data_import_overlay_text').text('Processing Data...');")
    showElement("data_import_overlay")
    withProgress(message = "Processing Data", value = 0, {
      check_name <- ""
      n <- nrow(import_results$file_dt)
      for (i in 1:n) {
        if (import_results$file_dt$Valid[i] == TRUE) {
          if (!check_name == import_results$file_dt$RawFile[i]) {
            runjs("$('#data_import_overlay_text').text('Reading mzML file...');")
            mzml_file <- input$rawdata_filename$datapath[which(input$rawdata_filename$name == import_results$file_dt$RawFile[i])]
            checkjson <- try(is_valid_samplejson(input$sampleJSON_filename$datapath[which(input$sampleJSON_filename$name == import_results$file_dt$SampleJSON[i])]))
            if (inherits(checkjson, "try-error")) nist_shinyalert("Bad JSON file", text = paste("At least one file is not a valid Sample JSON file."))
            if (input$has_lockmass) {
              if (is.na(input$lockmass)) {
                nist_shinyalert("Missing Settings", text = "Please provide a numeric value for the lock mass in Daltons.")
              }
              if (is.na(input$lockmass_width)) {
                nist_shinyalert("Missing Settings", text = "Please provide a numeric value for the lock mass width in Daltons.")
              }
              mzml <- mzMLtoR(
                mzmlfile = mzml_file,
                lockmass = input$lockmass,
                lockmasswidth = input$lockmass_width,
                correct = input$lockmass_correct
              )
            } else {
              mzml <- mzMLtoR(
                mzmlfile = mzml_file
              )
            }
          }
          
          samplejson <- parse_methodjson(input$sampleJSON_filename$datapath[which(input$sampleJSON_filename$name == import_results$file_dt$SampleJSON[i])])
          check_name <- import_results$file_dt$RawFile[i]
          check_name_ext <- tools::file_ext(check_name)
          # Fragility checks for name matching and values (e.g. msaccuracy) entered as text.
          if (!tools::file_ext(samplejson$sample$name) == check_name_ext) {
            samplejson$sample$name <- paste0(tools::file_path_sans_ext(basename(samplejson$sample$name)), ".", check_name_ext)
          }
          samplejson$massspectrometry$msaccuracy <- samplejson$massspectrometry$msaccuracy %>%
            str_remove_all("[A-Za-z]") %>%
            str_trim()
          import_results$processed_data[[i]] <- peak_gather_json(samplejson, mzml, data_react$compoundtable, zoom = c(input$ms1zoom_low, input$ms1zoom_high), minerror = input$minerror)
          
        }
        runjs("$('#data_import_overlay_text').text('Processing mzML data...');")
        incProgress(amount = 1/n, detail = sprintf("Processing entry %d of %d", i, n))
      }
    })
    
    # Process data
    peak_results <- list()
    sample_results <- rep(FALSE, length(import_results$processed_data))
    runjs("$('#data_import_overlay_text').text('Processing Sample JSONs...');")
    withProgress(message = "Processing Data", value = 0, {
      nj <- length(import_results$processed_data)
      ntotal <- sum(sapply(import_results$processed_data, length))
      for (j in 1:nj) {
        import_results$qc_results[[j]] <- list()
        import_results$opt_ums_params[[j]] <- list()
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
          import_results$qc_results[[j]][[i]] <- qc$check
          import_results$opt_ums_params[[j]][[i]] <- qc$opt_ums_params
          
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
    runjs("$('#data_import_overlay_text').text('');")
    hideElement("data_import_overlay")
  })
  
  output$sample_qc <- renderDT(
    DT::datatable(
      data = req(import_results$sample_list), 
      rownames = FALSE,
      caption = "1) Click a row to select an mzML file.",
      options = list(
        dom = "t",
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
      caption = "2) Click a row to see metrics for that peak.",
      options = list(
        dom = "t",
        paging = FALSE,
        filtering = FALSE,
        ordering = FALSE,
        searching = FALSE
      ),
      selection = "single"
    )
  )
  
  output$overall_qc_results <- renderText({
    qc <- req(qc_check_results()$qc)
    all_results <- unlist(lapply(qc, function(x) x$result))
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
    sample_qc_index <- input$sample_qc_rows_selected
    peak_qc_index <- input$peak_qc_rows_selected
    selected_file <- import_results$sample_list$RawFile[sample_qc_index]
    selected_analyte <- import_results$peak_list[[sample_qc_index]]$peak[peak_qc_index]
    qc_data <- import_results$qc_results[[sample_qc_index]][[peak_qc_index]]
    qc_valid <- !sapply(qc_data, \(x) is.na(x$result) || is.nan(x$result) || is.null(x$result))
    if (any(!qc_valid)) {
      qc_invalid <- qc_data[!qc_valid]
      qc_invalid_names <- sapply(qc_invalid, \(x) x$parameter)
      no_annotations <- all(str_detect(qc_invalid_names, "annfragments"))
      nist_shinyalert(
        title = "QC Checks Unavailable",
        type = "warning",
        text = tagList(
          p("The following checks could not be performed and will be excluded from further analysis and from the export."),
          if (no_annotations) p("No fragment annotations were detected."),
          br(),
          lapply(qc_invalid_names |>
                   str_replace_all("_", " ") |>
                   str_to_title(),
                 p
          )
        )
      )
      qc_data <- qc_data[qc_valid]
      import_results$qc_results[[sample_qc_index]][[peak_qc_index]] <- qc_data
    }
    tagList(
      h3(sprintf("Metrics for %s in %s", selected_analyte, selected_file)),
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
                   ) %>%
                     formatStyle(
                       "result",
                       target = 'row',
                       backgroundColor = styleEqual(c(TRUE, FALSE), c("none", "tomato"))
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
      qc_data <- import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
      qc_names <- lapply(qc_data, function(x) unique(x$parameter)) %>%
        unlist() %>% unname()
      qc_failures <- qc_data %>%
        lapply(\(x) !all(x$result)) %>%
        unlist()
      if (all(!qc_failures)) {
        # Collapse all result boxes but the first if no errors
        if (length(qc_names) > 1) {
          lapply(qc_names[-1],
                 function(x) {
                   runjs(sprintf('$("#qc_result_box_%s > div > div > div.box-header > div > button").click()', x))
                 })
        }
      } else {
        # Expand out boxes with failed checks otherwise
        if (length(qc_names) > 1) {
          lapply(qc_names[!qc_failures],
                 function(x) {
                   runjs(sprintf('$("#qc_result_box_%s > div > div > div.box-header > div > button").click()', x))
                 })
        }
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
      paste0("DIMSpec_import_files_", Sys.Date(), ".zip")
    },
    content = function(file) {
      runjs("$('#export_overlay_text').text('Preparing download file...');")
      showElement("export_overlay")
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      nj <- length(import_results$processed_data)
      ntotal <- sapply(import_results$processed_data, length) + 1
      for (j in 1:nj) {
        ni <- length(import_results$processed_data[[j]])
        for (i in 1:ni) {
          outdat <- import_results$processed_data[[j]][[i]]
          outdat$qc <- import_results$qc_results[[j]][[i]]
          outdat$opt_ums_params <- import_results$opt_ums_params[[j]][[i]]
          # Insert USER for missing fragment_citation
          outdat$annotation$fragment_citation[outdat$annotation$fragment_citation == ""] <- "USER"
          write_json(outdat, paste0(temp_directory, "/", gsub("\\.", "_", outdat$sample$name), "_cmpd", outdat$compounddata$id, ".JSON"),
                     auto_unbox = TRUE,
                     pretty = TRUE)
          runjs(sprintf("$('#data_import_overlay_text').text('%s');", sprintf("Input file %d of %d, writing peak JSON file %d of %d.", j, nj, i, ni)))
        }
      }
      runjs("$('#data_import_overlay_text').text('Zipping up results...');")
      hideElement("export_overlay")
      zip::zip(zipfile = file,
               files = dir(temp_directory),
               root = temp_directory)
    },
    contentType = "application/zip"
  )
  
})
