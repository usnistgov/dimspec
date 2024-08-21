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
  
  ## Instrument File(s) ----
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
  output$rawdata_filename_list <- renderUI({
    req(input$rawdata_filename)
    tagList(
      tags$small(
        class = "zero-space underlined",
        sprintf(
          "Loaded instrument file%s",
          ifelse(nrow(input$rawdata_filename) > 1, "s", "")
        )
      ),
      lapply(input$rawdata_filename$name, tags$small, class = "zero-space")
    )
  })
  
  ## Sample file(s) ----
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
  output$sampleJSON_filename_list <- renderUI({
    req(file_samplejson())
    sampleJSONs <- lapply(file_samplejson()$datapath, parse_methodjson)
    sampleJSON_raw <- sapply(sampleJSONs, function(x) x$sample$name)
    expects <- sprintf(
      "%s expects %s",
      file_samplejson()$name,
      sampleJSON_raw
    )
    tagList(
      tags$small(
        class = "zero-space underlined",
        sprintf(
          "Loaded sample file%s",
          ifelse(nrow(file_samplejson()) > 1, "s", "")
        )
      ),
      lapply(expects, tags$small, class = "zero-space"),
      br()
    )
  })
  
  qc_check_results <- reactive({
    req(import_results$processed_data,
        input$sample_qc_rows_selected,
        input$peak_qc_rows_selected,
        input$peak_qc_rows_selected <= length(import_results$qc_results[[input$sample_qc_rows_selected]]))
    if (import_results$sample_list$PassCheck[input$sample_qc_rows_selected]) {
      list(
        dat = import_results$processed_data[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]],
        qc = import_results$qc_results[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]],
        opt_ums_params = import_results$opt_ums_params[[input$sample_qc_rows_selected]][[input$peak_qc_rows_selected]]
      )
    } else {
      NULL
    }
  })
  
  ## File Matching ----
  observe({
    req(file_rawdata(), file_samplejson())
    updateActionButton(inputId = "process_data_btn", label = "Process Data")
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
    # File matching and verification
    rawfiles <- tibble(
      RawFile = RawFile,
      Valid = RawFile %in% sampleJSON_raw
    )
    samplefiles <- tibble(
      samplefile = sampleJSON_name,
      refers_to = sampleJSON_raw,
      Valid = sampleJSON_raw %in% RawFile
    )
    file_check <- rawfiles %>%
      left_join(
        tibble(sampleJSON_raw = sampleJSON_raw, SampleJSON = sampleJSON_name),
        by = c("RawFile" = "sampleJSON_raw")
      )
    # Check for unmatchable files
    missing_sample_files <- samplefiles$samplefile[!samplefiles$Valid]
    missing_instrument_files <- rawfiles$RawFile[!rawfiles$Valid]
    if (length(missing_sample_files) > 0 & length(missing_instrument_files) > 0) {
      alert_size <- "s"
      alert_title <- "Unable to match files"
      alert_h3 <- h3("Mismatches were identified for both sample and instrument files")
      alert_description <- tagList(
        p(
          sprintf(
            "Typically this is because name references in %s and %s do not exactly match. Perhaps you chose the wrong file or renamed %s?",
            ifelse(length(sampleJSON_name) > 1, "sample files", "the sample file"),
            ifelse(length(RawFile) > 1, "instrument files", "the instrument file"),
            ifelse(length(RawFile) > 1, "instrument files", "the instrument file")
          )
        ),
        br(),
        tags$strong(
          sprintf(
            "If you continue, the following %d file%s will be excluded from QC checks. It is recommended that you reload file(s) where all names can be matched.",
            length(c(missing_sample_files, missing_instrument_files)),
            ifelse(length(c(missing_sample_files, missing_instrument_files)) > 1, "s", "")
          )
        )
      )
      alert_details <- tagList(
        hr(),
        p(
          sprintf(
            "%s did not have a matching sample file.",
            ifelse(length(missing_instrument_files) > 1, "The following instrument files", "This instrument file")
          )
        ),
        br(),
        lapply(missing_instrument_files, tags$li),
        hr(),
        p(
          sprintf(
            "%s did not have have a matching instrument file.",
            ifelse(length(missing_sample_files) > 1, "The following sample files", "This sample file")
          )
        ),
        br(),
        tags$ul(
          sprintf(
            "%s expected instrument file %s",
            samplefiles$samplefile[!samplefiles$Valid],
            samplefiles$refers_to[!samplefiles$Valid]
          ) |>
            lapply(tags$li, class = "align-left")
        )
      )
    } else if (length(missing_sample_files) > 0) {
      alert_size <- "s"
      alert_title <- sprintf(
        "Unable to match all %s file%s with %s file%s",
        str_flatten_comma(unique(tools::file_ext(missing_sample_files)), last = ", or "),
        ifelse(length(missing_sample_files) > 1, "s", ""),
        str_flatten_comma(unique(tools::file_ext(RawFile)), last = ", or "),
        ifelse(length(RawFile) > 1, "s", "")
      )
      alert_h3 <- h3(
        sprintf(
          "%d sample file%s could not be matched.",
          length(missing_sample_files),
          ifelse(length(missing_sample_files) > 1, "s", "")
        )
      )
      alert_description <- tagList(
        p(
          sprintf(
            "Typically this is because the name%s referenced in the sample file%s not exactly match that in %s. Perhaps you chose the wrong file or renamed %s?",
            ifelse(length(missing_sample_files) > 1, "s", ""),
            ifelse(length(missing_sample_files) > 1, "s do", " does"),
            ifelse(length(RawFile) > 1, "instrument files", "the instrument file"),
            ifelse(length(RawFile) > 1, "instrument files", "the instrument file")
          )
        ),
        br(),
        tags$strong(
          sprintf(
            "If you continue, the following sample file%s will be excluded from QC checks. It is recommended that you reload instrument file(s) matching the expected names.",
            ifelse(length(missing_sample_files) > 1, "s", "")
          )
        ),
      )
      alert_details <- tagList(
        p(
          sprintf(
            "Unmatched sample file%s:",
            ifelse(length(missing_sample_files) > 1, "s were", " is")
          )
        ),
        br(),
        tags$ul(
          sprintf(
            "%s expected instrument file %s",
            samplefiles$samplefile[!samplefiles$Valid],
            samplefiles$refers_to[!samplefiles$Valid]
          ) |>
            lapply(tags$li, class = "align-left")
        ),
        hr(),
        p(
          sprintf(
            "Currently loaded instrument file%s %s:",
            ifelse(length(RawFile) > 1, "s", ""),
            ifelse(length(RawFile) > 1, "includes", "is")
          )
        ),
        br(),
        tags$ul(
          lapply(RawFile, tags$li, class = "align-left")
        )
      )
    } else if (length(missing_instrument_files) > 0) {
      alert_size <- "s"
      alert_title <- sprintf(
        "Unable to match %s file%s to %s file%s",
        str_flatten_comma(unique(tools::file_ext(missing_instrument_files)), last = ", or "),
        ifelse(length(missing_instrument_files) > 1, "s", ""),
        str_flatten_comma(unique(tools::file_ext(sampleJSON_name)), last = ", or "),
        ifelse(length(sampleJSON_name) > 1, "s", "")
      )
      alert_h3 <- h3(
        sprintf(
          "%d instrument file%s could not be matched.",
          length(missing_instrument_files),
          ifelse(length(missing_instrument_files) > 1, "s", "")
        )
      )
      alert_description <- tagList(
        p(
          sprintf(
            "Typically this is because the name%s of the instrument file%s not exactly match %s in the sample file%s. Perhaps you chose the wrong file or renamed %s?",
            ifelse(length(missing_instrument_files) > 1, "s", ""),
            ifelse(length(missing_instrument_files) > 1, "s do", " does"),
            ifelse(nrow(samplefiles) > 1, "any of those", "the one"),
            ifelse(nrow(samplefiles) > 1, "s", ""),
            ifelse(length(RawFile) > 1, "instrument files", "the instrument file")
          )
        ),
        br(),
        tags$strong(
          sprintf(
            "If you continue, the following instrument file%s will be excluded from QC checks, but you can always load new files:",
            ifelse(length(missing_instrument_files) > 1, "s", "")
          )
        )
      )
      alert_details <- tagList(
        p(
          sprintf(
            "Unmatched instrument file%s:",
            ifelse(length(missing_instrument_files) > 1, "s were", " is")
          )
        ),
        br(),
        tags$ul(
          lapply(missing_instrument_files, tags$li, class = "align-left")
        ),
        hr(),
        p(
          sprintf(
            "Currently loaded sample file%s expected:",
            ifelse(nrow(samplefiles) > 1, "s", "")
          )
        ),
        br(),
        tags$ul(
          lapply(sampleJSON_raw, tags$li, class = "align-left")
        )
      )
    }
    if (any(length(missing_sample_files) > 0, length(missing_instrument_files) > 0)) {
      nist_shinyalert(
        title = alert_title,
        size = alert_size,
        type = "warning",
        text = tagList(
          alert_h3,
          br(),
          alert_description,
          hr(),
          alert_details
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
        text = tagList(
          p(
            sprintf(
              "None of the provided sample file%s could be matched with the provided instrument file%s.",
              ifelse(length(sampleJSON_raw) > 1, "s", ""),
              ifelse(length(RawFile) > 1, "s", "")
            )
          ),
          br(),
          p(
            sprintf(
              "Names of mzML files must match%s:",
              ifelse(length(sampleJSON_raw) > 1, " one of", "")
            )
          ),
          br(),
          tags$ul(
            lapply(sampleJSON_raw, tags$li, class = "align-left")
          )
        )
      )
      import_results$file_dt <- NULL
    }
  })
  
  observeEvent(import_results$processed_data, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if ("Processed" %in% names(import_results$file_dt) && all(import_results$file_dt$Processed)) {
      nist_shinyalert(
        title = NULL,
        type = "success",
        text = h3("Data Processing Complete")
      )
    }
  })
  
  # Element Display ----
  observe({
    toggleElement(id = "process_data_btn", condition = !is.null(import_results$file_dt) && ifelse("Processed" %in% names(import_results$file_dt), !all(import_results$file_dt$Processed), TRUE))
    toggleElement(id = "go_to_settings", condition = ifelse("Processed" %in% names(import_results$file_dt), !all(import_results$file_dt$Processed), FALSE))
    toggleElement(id = "peak_qc_selector", condition = !is.null(input$sample_qc_rows_selected))
    toggleElement(id = "go_to_quality_review", condition = ifelse("Processed" %in% names(import_results$file_dt), any(import_results$file_dt$Processed), FALSE))
    toggleElement(id = "lockmass_settings", condition = input$has_lockmass)
    toggleElement(id = "export_from_review", condition = !is.null(input$peak_qc_rows_selected))
  })
  hideElement("results_rendered")
  hideElement("data_import_overlay")
  hideElement("export_overlay")
  
  # Navigation Buttons ----
  observeEvent(input$go_to_data_import | input$go_to_data_import2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "data_import")
  })
  observeEvent(input$go_to_settings, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "settings")
  })
  observeEvent(input$go_to_settings2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "settings")
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
  
  ## Processing issues ----
  output$processing_issues <- renderUI({
    req("Processed" %in% names(import_results$file_dt) && !all(import_results$file_dt$Processed))
    i <- which(sapply(import_results$processed_data, inherits, what = "try-error"))
    sample_files <- import_results$file_dt$SampleJSON[i]
    issues <- sapply(import_results$processed_data[i], \(x) attr(x, which = "condition")$message) %>%
      str_flatten_comma(last = ", and ")
    issues <- sprintf("[%s]: %s", sample_files, issues)
    tagList(
      hr(),
      tags$strong("Processing issues were identified."),
      tags$ul(
        lapply(issues, tags$li)
      )
    )
  })
  
  ## Checked data table ----
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
    
    ## Import Data ----
    runjs("$('#data_import_overlay_text').text('Processing Data...');")
    showElement("data_import_overlay")
    unable_to_process <- character(0L)
    json_processed <- logical(0L)
    err_messages <- character(0L)
    withProgress(message = "Processing Data", value = 0, {
      check_name <- ""
      n <- nrow(import_results$file_dt)
      ## Check each mzml/json file pair ----
      for (i in 1:n) {
        json_filename <- import_results$file_dt$SampleJSON[i]
        raw_filename <- import_results$file_dt$RawFile[i]
        if (import_results$file_dt$Valid[i] == TRUE) {
          if (!check_name == import_results$file_dt$RawFile[i]) {
            runjs("$('#data_import_overlay_text').text('Reading mzML file...');")
            mzml_file <- input$rawdata_filename$datapath[which(input$rawdata_filename$name == raw_filename)]
            checkjson <- try(is_valid_samplejson(input$sampleJSON_filename$datapath[which(input$sampleJSON_filename$name == json_filename)]))
            if (inherits(checkjson, "try-error")) {
              nist_shinyalert(
                title = "Could not validate JSON file",
                type = "error",
                immediate = TRUE,
                closeOnClickOutside = FALSE,
                text = sprintf("There was an issue validating JSON file %s.", json_filename)
              )
            } else if (!checkjson) {
              nist_shinyalert(
                title = "Issues Detected",
                type = "error",
                immediate = TRUE,
                closeOnClickOutside = FALSE,
                text = tagList(
                  p(
                    sprintf("The following errors were encountered with file %s.", json_filename)
                  ),
                  tags$ul(
                    lapply(attr(checkjson, "errors"), tags$li)
                  )
                )
              )
            }
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
          if (samplejson$massspectrometry$ce_value == "0") {
            samplejson$massspectrometry$ce_value <- "null"
          }
          samplejson$massspectrometry$msaccuracy <- samplejson$massspectrometry$msaccuracy %>%
            str_remove_all("[A-Za-z]") %>%
            str_trim()
          gathered_json <- try(
            peak_gather_json(samplejson, mzml, data_react$compoundtable, zoom = c(input$ms1zoom_low, input$ms1zoom_high), minerror = input$minerror)
          )
          if (inherits(gathered_json, "try-error")) {
            unable_to_process <- c(unable_to_process, import_results$file_dt$SampleJSON[i])
            err_messages <- c(err_messages, sprintf("[peak_gather_json] %s", attr(gathered_json, "condition")[1]$message))
            json_processed[i] <- FALSE
          } else {
            json_processed[i] <- TRUE
          }
          import_results$processed_data[[i]] <- gathered_json
        }
        # runjs("$('#data_import_overlay_text').text('Processing mzML data...');")
        incProgress(amount = 1/n, detail = sprintf("Processing mzML %d of %d", i, n))
      }
    })
    
    # Gather QC ----
    if (any(json_processed)) {
      peak_results <- list()
      sample_results <- rep(FALSE, length(import_results$processed_data))
      runjs("$('#data_import_overlay_text').text('Processing Sample JSONs...');")
      withProgress(message = "Processing Data", value = 0, {
        nj <- which(json_processed)
        ntotal <- sum(sapply(import_results$processed_data, length))
        # for (j in 1:nj) {
        for (j in nj) {
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
          sample_results[j] <- all(peak_results[[j]])
        }
      })
      
      # put out the various updates
      import_results$sample_list <- data.frame(RawFile = import_results$file_dt$RawFile, PassCheck = sample_results)
    }
    runjs("$('#data_import_overlay_text').text('');")
    hideElement("data_import_overlay")
    ## Processing feedback ----
    if (length(unable_to_process) > 0) {
      nist_shinyalert(
        title = sprintf("Processing error%s detected", ifelse(length(unable_to_process) > 1, "s", "")),
        type = "error",
        text = tagList(
          tags$strong(
            sprintf(
              "Could not process%s sample file%s with current settings.",
              ifelse(length(unable_to_process) > 1, sprintf(" %d", length(unable_to_process)), " this"),
              ifelse(length(unable_to_process) > 1, "s", "")
            )
          ),
          br(),
          br(),
          tags$ul(
            lapply(unable_to_process, tags$li, class = "align-left")
          ),
          hr(),
          tags$strong("Error Messages"),
          br(),
          br(),
          lapply(unique(err_messages), p, class = "align-left"),
          hr(),
          tags$strong("Current Settings"),
          br(),
          br(),
          tags$ul(
            tags$li(class = "align-left", sprintf("Zoom: %d - %d", input$ms1zoom_low, input$ms1zoom_high)),
            tags$li(class = "align-left", sprintf("Minimum Error: %g", input$minerror))
          ),
          br(),
          actionLink(inputId = "go_to_settings2", label = "Go To Settings", icon = icon("gears"))
        )
      )
    }
    import_results$file_dt$Processed <- json_processed
    updateActionButton(inputId = "process_data_btn", label = "Reprocess Data")
  })
  
  output$sample_qc <- renderDT({
    shiny::validate(
      need(!is.null(import_results$sample_list), message = "No samples could be processed.")
    )
    DT::datatable(
      data = import_results$sample_list, 
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
  })
  
  # QC Review Outputs ----
  output$peak_qc <- renderDT({
    selected <- req(input$sample_qc_rows_selected)
    shiny::validate(
      need(import_results$sample_list$PassCheck[selected], message = "Select an mzML file above where PassCheck is true. QC checks cannot be performed on the currently selected file.")
    )
    DT::datatable(
      data = import_results$peak_list[[selected]] %>%
        rename("Analyte" = "peak"),
      rownames = FALSE,
      caption = "2) Click a row to see metrics for that analyte",
      options = list(
        dom = "t",
        paging = FALSE,
        filtering = FALSE,
        ordering = FALSE,
        searching = FALSE
      ),
      selection = "single"
    )
  })
  
  output$overall_qc_results <- renderText({
    qc <- req(qc_check_results()$qc)
    all_results <- unlist(lapply(qc, function(x) x$result))
    n_fail <- length(which(all_results == FALSE))
    ifelse(n_fail > 0,
           sprintf("There %s %s failed QC check%s for this peak.", ifelse(n_fail == 1, "is", "are"), n_fail, ifelse(n_fail == 1, "", "s")),
           "There are no failed QC checks for this peak."
    )
  })
  
  observeEvent(input$go_to_quality_review, {
    updateTabsetPanel(inputId = "sidebar_menu", selected = "qc_review")
  })
  
  observeEvent(qc_check_results()$qc, ignoreNULL = TRUE, {
    req(!is.null(qc_check_results()$qc))
    import_results$qc_check_select <- unique(do.call(c, lapply(qc_check_results()$qc, function(x) c(x$parameter))))
  })
  
  output$quality_data <- renderUI({
    sample_qc_index <- isolate(input$sample_qc_rows_selected)
    peak_qc_index <- input$peak_qc_rows_selected
    shiny::validate(need(sample_qc_index, "Please select a RawFile from the list on the left."))
    shiny::validate(need(peak_qc_index, "Please select an analyte from the list on the left."))
    peak_list <- isolate(import_results$peak_list)
    req(
      !is.null(sample_qc_index),
      !is.null(peak_qc_index),
      peak_qc_index <= nrow(peak_list[[sample_qc_index]])
    )
    sample_list <- isolate(import_results$sample_list)
    qc_results <- isolate(import_results$qc_results)
    selected_file <- sample_list$RawFile[sample_qc_index]
    selected_analyte <- peak_list[[sample_qc_index]]$peak[peak_qc_index]
    qc_data <- qc_results[[sample_qc_index]][[peak_qc_index]]
    names(qc_data) <- sapply(qc_data, \(x) x$parameter)
    annotation_index <- which(str_detect(names(qc_data), "annfragments"))
    annotation_names <- names(qc_data)[annotation_index]
    qc_valid <- !sapply(
      qc_data,
      \(x) {
        any(is.na(x$result)) ||
          any(is.nan(x$result)) ||
          any(is.null(x$result))
      })
    if (any(!qc_valid)) {
      qc_invalid <- qc_data[!qc_valid]
      no_annotations <- any(annotation_names %in% names(qc_invalid))
      nist_shinyalert(
        title = "QC Checks Unavailable",
        type = "warning",
        text = tagList(
          p("The following checks could not be performed and will be excluded from export files."),
          if (no_annotations) p(br(), "Fragment annotations were not available."),
          br(),
          lapply(qc_invalid |>
                   names() |>
                   str_replace_all("_", " ") |>
                   str_to_title(),
                 p
          )
        )
      )
      qc_data <- qc_data[qc_valid]
      import_results$qc_results[[sample_qc_index]][[peak_qc_index]] <- qc_data
    }
    updateCheckboxInput(inputId = "results_rendered", value = TRUE)
    tagList(
      h3(sprintf("QC Metrics for %s in %s", selected_analyte, selected_file)),
      tags$script("
        $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box').find('[data-widget=collapse]').click();});
      "),
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
                     )
                 )
               )
             })
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
  
  # Export Functions ---- 
  output$export_from_review <- downloadHandler(
    filename = function() {
      paste0("DIMSpec_import_files_", Sys.Date(), ".zip")
    },
    content = function(file) {
      runjs("$('#export_overlay_text').text('Preparing download file...');")
      showElement("export_overlay")
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      nj <- length(import_results$processed_data)
      for (j in 1:nj) {
        ni <- length(import_results$processed_data[[j]])
        for (i in 1:ni) {
          outdat <- import_results$processed_data[[j]][[i]]
          outdat$qc <- import_results$qc_results[[j]][[i]]
          for (z in length(outdat$qc):1) {
            outdat$qc[[z]] <- filter(outdat$qc[[z]], !is.na(result))
            if (nrow(outdat$qc[[z]]) == 0) {
              outdat$qc[[z]] <- NULL
            }
          }
          outdat$opt_ums_params <- import_results$opt_ums_params[[j]][[i]]
          if ("annotation" %in% names(outdat)){
            if (length(outdat$annotation) == 0) {
              outdat$annotation <- NULL
            } else {
              if (!"fragment_citation" %in% names(outdat$annotation) || any(outdat$annotation$fragment_citation == "")) {
                # Insert USER for missing fragment_citation
                outdat$annotation$fragment_citation[outdat$annotation$fragment_citation == ""] <- "USER"
              }
            }
          }
          outname <- paste0(gsub("\\.", "_", outdat$sample$name), "_cmpd", outdat$compounddata$id, ".JSON")
          outdat <- split_by_collision_energy(outdat)
          n_out <- length(outdat)
          runjs(sprintf("$('#data_import_overlay_text').text('%s');", sprintf("Input file %d of %d, writing peak JSON file %d of %d.", j, nj, i, ni)))
          for (ix in 1:n_out) {
            if (n_out > 1) {
              ce <- outdat[[ix]]$massspectrometry$ce_value
              outname2 <- gsub(".JSON", sprintf("_ce_%d.JSON", ce), outname)
            } else {
              outname2 <- outname
            }
            outname2 <- file.path(temp_directory, outname2)
            write_json(x = outdat[[ix]], path = outname2, auto_unbox = TRUE, pretty = TRUE)
          }
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
