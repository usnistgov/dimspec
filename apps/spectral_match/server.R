shinyServer(function(input, output, session) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  if (!dev) {
    if (!active_connection() && DIRECT_CONNECTION) {
      manage_connection(conn_name = "con")
    }
  }
  
  # Session Data ----
  user_data <- reactiveVal(
    if (!toy_data) {
      NULL
    } else {
      readRDS("toy_data.RDS")
    }
  )
  data_input_search_upload <- reactiveVal(NULL)
  data_input_search_parameters <- reactiveVal(
    if (!toy_data) {
    tibble(
      precursor = numeric(0),
      rt = numeric(0),
      rt_start = numeric(0),
      rt_end = numeric(0)
    )
    } else {
      readRDS("toy_parameters.RDS")
    }
  )
  search_compounds_results <- reactiveVal(NULL)
  search_compounds_results_selected <- reactive({
    req(search_compounds_results()$result,
        input$search_compounds_dt_rows_selected)
    search_compounds_results()$result %>%
      bind_rows() %>%
      slice(input$search_compounds_dt_rows_selected)
  })
  method_narrative <- reactive({
    req(search_compounds_results_selected())
    paste0("This reference spectrum was m",
           api_endpoint(
             path = "method_narrative",
             type = "peak",
             table_pk = search_compounds_results_selected()$peak_ids[1]
           ) %>%
             stringr::str_sub(2)
    )
  })
  search_fragments_results <- reactiveVal(
      tibble(
        fragment_id = integer(0),
        formula = character(0),
        smiles = character(0),
        fragment_mz = numeric(0),
        exactmass = numeric(0),
        mass_error = numeric(0),
        compounds = character(0)
      )
  )
  search_fragments_results_selected <- reactive(
    req(search_fragments_results()) %>%
      slice(req(input$search_fragments_dt_rows_selected))
  )
  mod_search_params <- c(
    "mod_search_parameter_precursor",
    "mod_search_parameter_rt",
    "mod_search_parameter_rt_start",
    "mod_search_parameter_rt_end"
  )
  mod_upload_params <- c(
    "mod_upload_parameter_precursor",
    "mod_upload_parameter_rt",
    "mod_upload_parameter_rt_start",
    "mod_upload_parameter_rt_end"
  )
  
  # Style adjustments ----
  runjs("$('.box-body.left').parent().parent().addClass('box-left');")
  runjs("$('.box-body.right').parent().parent().addClass('box-right');")
  hideElement(selector = "#data_input_overlay")
  hideElement(selector = "#data_input_next_actions")
  hideElement(selector = "#search_compounds_overlay")
  hideElement(selector = "#uncertainty_overlay")
  hideElement(selector = "#search_fragments_overlay")
  
  # Element Display ----
  observe({
    toggleElement("data_input_additional", condition = advanced_use)
    if (!dev) {
      toggleElement("data_input_dt_peak_list_edit_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
      toggleElement("data_input_dt_peak_list_remove_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
      toggleElement("data_input_process_btn", condition = !is.null(input$data_input_filename) && nrow(data_input_search_parameters()) > 0)
      toggleElement("data_input_dt_peak_list", condition = nrow(data_input_search_parameters()) > 0)
      toggleElement("search_compounds_results_span", condition = !is.null(search_compounds_results()) && nrow(search_compounds_results()) > 0)
      toggleElement("search_fragments_results_span", condition = !is.null(search_fragments_results()) && nrow(search_fragments_results()) > 0)
    }
  })
  
  # Navigation ----
  observeEvent(input$sidebar_menu, {
    if (!dev) {
      if (!input$sidebar_menu %in% c("data_input", "index", "about") && any(is.null(user_data()), nrow(data_input_search_parameters()) == 0)) {
        nist_shinyalert(title = "Insufficient data",
                        type = "info",
                        text = "Please load a data file and select search parameters first.")
        updateTabsetPanel(session = session,
                          inputId = "sidebar_menu",
                          selected = "data_input")
      }
      if (input$sidebar_menu == "uncertainty" && is.null(search_compounds_results())) {
        nist_shinyalert(title = "No match selected",
                        type = "info",
                        text = "Please run a compound match search first.")
        updateTabsetPanel(session = session,
                          inputId = "sidebar_menu",
                          selected = "search_compounds")       
      }
    }
  })
  
  # HOME PAGE ----
  observeEvent(input$index_go_data_input, {
    updateTabsetPanel(session = session,
                      inputId = "sidebar_menu",
                      selected = "data_input")
    })

  # DATA INPUT PAGE ----
  # _Reactives ----
  # __Data Table
  output$data_input_dt_peak_list <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data_input_search_parameters() %>%
        select(precursor:rt_end),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      editable = TRUE,
      autoHideNavigation = TRUE,
      colnames = c("Precursor m/z", "RT", "RT Start", "RT End"),
      caption = "Data will be searched for peaks matching these characteristics. Select a row to edit or remove it.",
      options = list(
        pageLength = 15,
        extensions = c("Responsive"),
        columnDefs = list(
          list(className = "dt-center", targets = 0:3)
        )
      )
    )
  )
  # _Observers ----
  # __Search parameters ----
  observeEvent(data_input_search_parameters(), {
    n_orig <- nrow(data_input_search_parameters())
    n_uniq <- nrow(distinct(data_input_search_parameters()))
    if (!n_orig == n_uniq) {
      nist_shinyalert(
        title = "Duplicate values detected",
        type = "info",
        text = glue::glue("Search parameters must represent unique combinations. Duplicated settings (n = {n_orig - n_uniq}) have been removed.")
      )
    }
    data_input_search_parameters() %>%
      distinct() %>%
      arrange(rt) %>%
      data_input_search_parameters()
    if (nrow(data_input_search_parameters()) == 0) {
      updateSelectizeInput(
        session = session,
        inputId = "search_compounds_mzrt",
        choices = character(0),
        options = list(placeholder = "Please add search parameters on the Data Input page.")
      )
      updateSelectizeInput(
        session = session,
        inputId = "search_fragments_mzrt",
        choices = character(0),
        options = list(placeholder = "Please add search parameters on the Data Input page.")
      )
    } else {
      updateSelectizeInput(
        session = session,
        inputId = "search_compounds_mzrt",
        choices = data_input_search_parameters() %>%
          mutate(label = glue::glue("m/z {round(precursor, 4)} @ {round(rt, 2)} ({round(rt_start, 2)} - {round(rt_end, 2)})")) %>%
          pull(label) %>%
          setNames(object = 1:length(.),
                   nm = .)
      )
      updateSelectizeInput(
        session = session,
        inputId = "search_fragments_mzrt",
        choices = data_input_search_parameters() %>%
          mutate(label = glue::glue("m/z {round(precursor, 4)} @ {round(rt, 2)} ({round(rt_start, 2)} - {round(rt_end, 2)})")) %>%
          pull(label) %>%
          setNames(object = 1:length(.),
                   nm = .)
      )
    }
  })
  # __Verify isolation width / experiment type ----
  observeEvent({
    input$data_input_isolation_width
    input$data_input_experiment_type
    }, {
    if (input$data_input_isolation_width > app_settings$data_input_isolation_width_warn_threshold) {
      types <- app_settings$experiment_types
      experiment_type <- names(types[types == input$data_input_experiment_type])
      if (!str_detect(experiment_type, "SWATH")) {
        nist_shinyalert(
          title = "Abnormal value suspected",
          type = "info",
          text = glue::glue("Typically, the value of Isolation Width should be less than 4 Da unless the experiment type is SWATH or HRM.")
        )
      }
    }
  })
  # __Data file upload ----
  observeEvent(input$data_input_filename, {
    req(input$data_input_filename)
    fn <- input$data_input_filename
    valid <- valid_file_format(fn$name, app_settings$data_input_import_file_types)
    if (!valid) {
      reset("data_input_filename")
    }
    hideElement("data_input_next_actions")
  })
  # __Import parameters ----
  observeEvent(input$data_input_import_search, {
    req(input$data_input_import_search)
    fn <- input$data_input_import_search
    if (!valid_file_format(fn$name, app_settings$data_input_import_search_settings_types)) {
      reset("data_input_import_search")
    } else {
      upload <- switch(
        tools::file_ext(fn$name),
        "csv" = read_csv(file = fn$datapath),
        "xls" = read_xls(path = fn$datapath, sheet = 1),
        "xlsx" = read_xlsx(path = fn$datapath, sheet = 1)
      )
      data_input_search_upload(upload)
      showModal(mod_data_input_parameters_upload(data = upload))
    }
  })
  observeEvent(input$mod_data_input_upload_parameter_save, {
    valid <- complete_form_entry(input, mod_upload_params)
    req(valid)
    select_cols <- c(
      input$mod_upload_parameter_precursor,
      input$mod_upload_parameter_rt,
      input$mod_upload_parameter_rt_start,
      input$mod_upload_parameter_rt_end
    )
    if (any(table(select_cols) > 1)) {
      nist_shinyalert(
        title = "Duplicate columns detected",
        type = "error",
        text = "Please select unique columns."
      )
    } else {
      upload_parameters <- data_input_search_upload() %>%
        select(all_of(select_cols)) %>%
        setNames(names(data_input_search_parameters()))
      reasonable_rts <- upload_parameters %>%
        mutate(reasonable_rt = rt >= rt_start && rt <= rt_end,
               reasonable_rtstart = rt_start <= rt_end,
               reasonable = all(reasonable_rt, reasonable_rtstart))
      if (!all(reasonable_rts$reasonable)) {
        bad_entries <- reasonable_rts %>%
          filter(!reasonable,
                 across(everything(), ~ !is.na(.x))) %>%
          mutate(msg_prefix = glue::glue("<div style='text-align: left; padding-top: 2px;' class='one-line'><p>For m/z value <p style='font-weight: bold;'>{precursor}</p>, "),
                 msg_rt = ifelse(reasonable_rt,
                                        "",
                                        glue::glue("RT <p style='font-weight: bold; color: red;'>{rt}</p> does not fall between <p style='font-weight: bold;'>{rt_start}</p> and <p style='font-weight: bold;'>{rt_end}</p>.")),
                 msg_rtstart = ifelse(reasonable_rtstart,
                                        "",
                                        glue::glue("RT Start <p style='font-weight: bold; color: red;>{rt_start}</p> was after RT End <p style='font-weight: bold;'>{rt_end}</p>.")),
                 message = glue::glue("{msg_prefix} {msg_rt} {msg_rtstart}</p></div>")
          )
        nist_shinyalert(
          title = "Unreasonable retention times",
          size = "m",
          type = "error",
          text = glue::glue(
            "<p style='font-weight: bold;'>Please address the following issues in the source file '{input$data_input_import_search$name}' and try again.</p><br>{paste0(bad_entries$message, collapse = '')}"
          )
        )
      } else {
        which_complete <- complete.cases(upload_parameters)
        if (any(!which_complete)) {
          nist_shinyalert(
            title = "Incomplete data",
            type = "warning",
            text = glue::glue("Some rows (n = {sum(!which_complete)}) in the file \"{input$data_input_import_search$name}\" do not contain values for all parameters. Those rows have been removed. Please adjust your file to contain complete values for all features of interest. Once fixed, you may upload this file again and any duplicates will be automatically removed.")
          )
          upload_parameters <- upload_parameters[which_complete, ]
        }
        if (input$mod_upload_parameter_append) {
          upload_parameters <- bind_rows(
            data_input_search_parameters(),
            upload_parameters
          )
        }
        data_input_search_parameters(upload_parameters)
      }
      removeModal()
      reset("data_input_import_search")
    }
  })
  # __Manually adjust parameters ----
  observeEvent(input$data_input_dt_peak_list_add_row, {
    if (all(mod_search_params %in% names(input))) {
      lapply(mod_search_params, function(x) updateNumericInput(inputId = x, value = NULL))
    }
    showModal(mod_data_input_parameters_manual())
  })
  observeEvent(input$data_input_dt_peak_list_edit_row, {
    req(input$data_input_dt_peak_list_rows_selected)
    selected_params <- data_input_search_parameters()[input$data_input_dt_peak_list_rows_selected, ]
    lapply(mod_search_params,
           function(x) {
             val <- selected_params[[str_remove(x, "mod_search_parameter_")]]
             updateNumericInput(inputId = x, value = val)
           }
    )
    showModal(mod_data_input_parameters_manual())
  })
  observeEvent(input$data_input_dt_peak_list_remove_row, {
    req(input$data_input_dt_peak_list_rows_selected)
    data_input_search_parameters()[-input$data_input_dt_peak_list_rows_selected, ] %>%
      data_input_search_parameters()
  })
  observeEvent(input$mod_data_input_search_parameter_save, {
    valid <- complete_form_entry(input, mod_search_params)
    req(valid)
    # req(complete_form_entry(input, mod_search_params))
    tmp <- data_input_search_parameters()
    values <- data.frame(
      input$mod_search_parameter_precursor,
      input$mod_search_parameter_rt,
      input$mod_search_parameter_rt_start,
      input$mod_search_parameter_rt_end
    ) %>%
      setNames(names(data_input_search_parameters()))
    if (!is.null(input$data_input_dt_peak_list_rows_selected)) {
      tmp <- tmp[-input$data_input_dt_peak_list_rows_selected, ]
    }
    valid <- TRUE
    if (!input$mod_search_parameter_rt >= input$mod_search_parameter_rt_start) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Centroid) should be after Retention Time (Start).")
      valid <- FALSE
    }
    if (!input$mod_search_parameter_rt <= input$mod_search_parameter_rt_end) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Centroid) should be before Retention Time (End).")
      valid <- FALSE
    }
    if (!input$mod_search_parameter_rt_start <= input$mod_search_parameter_rt_end) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Start) should be before Retention Time (End).")
      valid <- FALSE
    }
    if (valid) {
      tmp %>%
        bind_rows(values) %>%
        data_input_search_parameters()
      removeModal()
    }
  })
  # __Process Data ----
  observeEvent(input$data_input_process_btn, {
    showElement(selector = "#data_input_overlay")
    user_data(NULL)
    required <- grep("data_input", names(input), value = TRUE)
    required <- required[-grep("dt_peak_list|import_search|filename|_process|^mod_data|_go_", required)]
    req(input$data_input_filename,
        data_input_search_parameters())
    sapply(required,
           function(x) {
             req(input[[x]])
           })
    mzml <- try(
      getmzML(data.frame(filename = input$data_input_filename$datapath))
    )
    if (inherits(mzml, "try-error")) {
      nist_shinyalert(
        title = "Conversion issue",
        type = "error",
        text = "Could not safely convert this file to a readable mzML file."
      )
    } else {
      hideElement("data_input_overlay")
      hideElement("data_input_process_btn")
      showElement("data_input_next_actions")
      user_data(mzml)
    }
  })
  # __Navigation to next ----
  observeEvent(input$data_input_go_compound, {
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "search_compounds"
    )
  })
  observeEvent(input$data_input_go_uncertainty, {
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "uncertainty"
    )
  })
  observeEvent(input$data_input_go_fragment, {
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "search_fragments"
    )
  })
  
  # COMPOUND SEARCH PAGE ----
  # _Reactives ----
  # __Match statuses ----
  output$search_compounds_status <- renderUI({
    req(search_compounds_results())
    matches <- search_compounds_results()$result %>%
      slice(1)
    match_out <- h3("Top match is",
                    p(style = "font-weight: bold;", matches$name),
                    "from",
                    ifelse(substr(matches$sample_class, 1, 1) %in% vowels,
                           "an",
                           "a"),
                    matches$sample_class)
    match_score1 <- p("MS1 Score: ",
                      p(style = "font-weight: bold;", matches$ms1_dp),
                      "( rev score ", matches$ms1_rdp, ")")
    if (!is.na(matches$ms2_dp)) {
      match_score2 <- p("| MS2 Score: ",
                        p(style = "font-weight: bold;;", matches$ms2_dp),
                        "( rev score ", matches$ms2_rdp, ")")
    } else {
      match_score2 <- NULL
    }
    tagList(match_out, match_score1, match_score2)
  })
  output$search_compounds_status2 <- renderUI({
    req(
      search_compounds_results(),
      input$search_compounds_dt_rows_selected
    )
    if (input$search_compounds_dt_rows_selected == 1) {
      NULL
    } else {
      matches <- search_compounds_results()$result %>%
        slice(input$search_compounds_dt_rows_selected)
      match_out <- h4(
          "Selected comparison is is",
          p(style = "font-weight: bold; display: inline;", matches$name),
          "from",
          ifelse(substr(matches$sample_class, 1, 1) %in% vowels,
                 "an",
                 "a"),
          matches$sample_class
        )
      match_score1 <- p(
          "MS1 Score: ",
          p(style = "font-weight: bold;", matches$ms1_dp),
          "(rev score ", matches$ms1_rdp, ")"
        )
      if (!is.na(matches$ms2_dp)) {
        match_score2 <- p(
          " |  MS2 Score: ",
          p(style = "font-weight: bold;;", matches$ms2_dp),
          "( rev score ", matches$ms2_rdp, ")"
        )
      } else {
        match_score2 <- p(style = "color: darkgray;", "| (No MS2 score)")
      }
      tagList(match_out, match_score1, match_score2)
    }
  })
  # __Data Table ----
  output$search_compounds_dt <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data = req(search_compounds_results()$result) %>%
        select(compound_id, name, ms1_dp, ms1_rdp, ms2_dp, ms2_rdp,total_ann_fragments, total_ann_structures, total_ann_citations, sample_class, peak_ids),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      autoHideNavigation = TRUE,
      colnames = c("Compound ID", "Compound ID", "MS1 Score", "MS1 Score (Rev)", "MS2 Score", "MS2 Score (Rev)", "# Annotated Fragments", "# Annotated Structures", "# Annotated Citations", "Sample Class", "Peak ID"),
      caption = "Select a row to view the match or send it to uncertainty estimation.",
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = "Btp",
        pageLength = 10,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 10)),
          list(className = "dt-center", targets = 2:9),
          list(className = "dt-left", targets = 1)
        )
      )
    )
  )
  # __Butterfly plot ----
  output$search_compounds_butterfly_plot <- renderPlotly({
    req(
      search_compounds_results(),
      !all(is.null(input$search_compounds_dt_rows_selected),
           is.null(input$search_compounds_dt_row_last_clicked))
    )
    if (input$search_compounds_msn == "MS2") {
      compare_actual <- search_compounds_results()$search_object$ums2
      compare_with <- search_compounds_results()$ums2_compare
    } else {
      compare_actual <- search_compounds_results()$search_object$ums1
      compare_with <- search_compounds_results()$ums1_compare
    }
    this_row <- ifelse(is.null(input$search_compounds_dt_rows_selected),
                       input$search_compounds_dt_row_last_clicked,
                       input$search_compounds_dt_rows_selected)
    plot_compare_ms(
      ums1 = compare_actual,
      ums2 = compare_with[[this_row]],
      ylim.exp = 0.1,
      main = element_blank()
    ) %>%
      ggplotly()
  })
  # __Method narrative ----
  output$search_compounds_method_narrative <- renderUI(
    tags$caption(
      style = "display: inline;",
      req(method_narrative())
    )
  )
  # _Observers ----
  # __Navigation ----
  observeEvent(input$search_compounds_go_data_input, {
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "data_input"
    )
  })
  # __Update to MS1 if no MS2 ----
  observe({
    if (is.na(search_compounds_results_selected()$ms2_dp) &&
        input$search_compounds_msn == "MS2") {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "search_compounds_msn",
        selected = "MS1"
      )
    }
  })
  # __Execute search ----
  observeEvent(input$search_compounds_search_btn, {
    if (input$search_compounds_mzrt == "") {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please choose a Feature of Interest"
      )
    }
    if (input$search_compounds_search_type == "") {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please choose a Search Type"
      )
    }
    if (input$search_compounds_search_type == "all") {
      nist_shinyalert(
        title = "Long action",
        type = "info",
        text = "This will search your measured spectrum against all records in the database regardless and may take quite a while to execute."
      )
    }
    req(
      user_data(),
      data_input_search_parameters(),
      input$search_compounds_mzrt,
      input$search_compounds_search_type,
      input$data_input_relative_error,
      input$data_input_minimum_error,
      input$data_input_experiment_type,
      input$data_input_isolation_width,
      input$data_input_search_zoom,
      # input$data_input_max_correl,
      # input$data_input_correl_bin,
      input$data_input_ph
      # input$data_input_max_ph,
      # input$data_input_ph_bin,
      # input$data_input_max_freq,
      # input$data_input_freq_bin,
      # input$data_input_min_n_peaks
    )
    runjs("$('#search_compounds_overlay_text').text('Executing compound search...');")
    showElement("search_compounds_overlay")
    runjs("$('#search_compounds_overlay_text').text('Validating inputs...');")
    mzrt <- isolate(data_input_search_parameters()[input$search_compounds_mzrt, ])
    tmp <- list(
      mzML = isolate(user_data()$mzML),
      search_df = tibble(
        filename = user_data()$search_df$filename,
        precursormz = mzrt$precursor,
        masserror = isolate(input$data_input_relative_error),
        minerror = isolate(input$data_input_minimum_error),
        rt = mzrt$rt,
        rt_start = mzrt$rt_start,
        rt_end = mzrt$rt_end,
        ms2exp = isolate(input$data_input_experiment_type),
        isowidth = isolate(input$data_input_isolation_width)
      )
    )
    runjs("$('#search_compounds_overlay_text').text('Creating search object...');")
    search_object <- get_search_object(
      searchmzml = tmp,
      zoom = isolate(input$data_input_search_zoom)
    ) %>%
      create_search_ms(
        searchobj = .,
        correl = isolate(input$data_input_correlation),
        ph = isolate(input$data_input_ph),
        freq = isolate(input$data_input_freq),
        normfn = isolate(input$data_input_norm_function),
        cormethod = isolate(input$data_input_correlation_method)
      )
    runjs("$('#search_compounds_overlay_text').text('Scoring database matches...');")
    search_result <- api_endpoint(
      path               = "search_compound",
      type               = isolate(input$search_compounds_search_type),
      search_ms          = jsonlite::toJSON(search_object),
      norm_function      = isolate(input$data_input_norm_function),
      correlation_method = isolate(input$data_input_correlation_method),
      optimized_params   = isolate(input$search_compounds_use_optimized_parameters),
      return_format      = "list"
    )
    search_compounds_results(
      list(
        search_object = search_object,
        result = bind_rows(search_result$result),
        ums1_compare = lapply(search_result$ums1_compare, bind_rows),
        ums2_compare = lapply(search_result$ums2_compare, bind_rows)
      )
    )
    hideElement("search_compounds_overlay")
  })
  # __Evaluate uncertainty ----
  observeEvent(input$search_compounds_uncertainty_btn, ignoreNULL = TRUE, ignoreInit = TRUE, {
    # search_compounds_results_selected() %>%
    #   bootstrap_compare_ms()
    browser()
    updateTabsetPanel(session = session,
                      inputId = "sidebar_menu",
                      selected = "uncertainty")
  })

  # UNCERTAINTY PAGE ----
  # _Reactives ----
  # __Feedback ----
  # __Method narrative ----
  output$uncertainty_method_narrative <- renderUI({
    req(method_narrative())
    tags$caption(
      style = "display: inline;",
      method_narrative()
    )
  })
  output$uncertainty_status <- renderUI({
    req(
      search_compounds_results(),
      input$search_compounds_dt_rows_selected
    )
      matches <- search_compounds_results()$result %>%
        slice(input$search_compounds_dt_rows_selected)
      match_out <- h4(
        "Uncertainty evaluation for the selected match of",
        p(style = "font-weight: bold; display: inline;", matches$name),
        "from",
        ifelse(substr(matches$sample_class, 1, 1) %in% vowels,
               "an",
               "a"),
        matches$sample_class
      )
      match_score1 <- p(
        "MS1 Score: ",
        p(style = "font-weight: bold;", matches$ms1_dp),
        "(rev score ", matches$ms1_rdp, ")"
      )
      if (!is.na(matches$ms2_dp)) {
        match_score2 <- p(
          " |  MS2 Score: ",
          p(style = "font-weight: bold;;", matches$ms2_dp),
          "( rev score ", matches$ms2_rdp, ")"
        )
      } else {
        match_score2 <- p(style = "color: darkgray;", "| (No MS2 score)")
      }
      tagList(match_out, match_score1, match_score2)
  })
  # __Butterfly plot ----
  output$uncertainty_butterfly_plot <- renderPlotly({
    req(
      search_compounds_results(),
      !all(is.null(input$search_compounds_dt_rows_selected),
           is.null(input$search_compounds_dt_row_last_clicked))
    )
    if (input$search_compounds_msn == "MS2") {
      compare_actual <- search_compounds_results()$search_object$ums2
      compare_with <- search_compounds_results()$ums2_compare
    } else {
      compare_actual <- search_compounds_results()$search_object$ums1
      compare_with <- search_compounds_results()$ums1_compare
    }
    this_row <- ifelse(is.null(input$search_compounds_dt_rows_selected),
                       input$search_compounds_dt_row_last_clicked,
                       input$search_compounds_dt_rows_selected)
    plot_compare_ms(
      ums1 = compare_actual,
      ums2 = compare_with[[this_row]],
      ylim.exp = 0.1,
      main = element_blank()
    ) %>%
      ggplotly()
  })
  # output$uncertainty_boxplot <- renderPlotly(
  #   search_compounds_results_selected() %>%
  #     boxplot_quant() %>%
  #     ggplotly()
  # )
  # output$uncertainty_summary <- renderText(
  #   search_compounds_results_selected() %>%
  #     bootstrap_compare_ms()
  # )

  # FRAGMENT SEARCH PAGE ----
  # _Reactives ----
  output$search_fragments_dt <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      req(search_fragments_results()),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      autoHideNavigation = TRUE,
      colnames = c("Elemental Formula", "Fragment m/z", "SMILES", "Exact Mass", "Mass Error (ppm)", "Compounds"),
      caption = "Select a row to view the matching fragment.",
      options = list(
        dom = "Btp",
        pageLength = 15,
        extensions = c("Responsive", "Buttons"),
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 2)),
          list(className = "dt-center", targets = c(3, 4, 5)),
          list(className = "dt-left", targets = c(1, 6))
        )
      )
    )
  )
  output$search_fragments_spectral_plot <- renderPlotly({
    validate(
      need(nrow(search_fragments_results()) > 0,
           message = "Please select a fragment in the table.")
    )
  }
  # A better plot here might be a linked data plot with the fragment highlighted, and the ability to click back and forth between the two.
    # search_fragments_results_selected() %>%
    #   plot_ms()
  )
  output$search_fragment_ballstick <- renderImage({
    validate(
      need(search_fragments_results_selected(),
           message = "Please select a fragment in the table.")
    )
    fragment_id <- search_fragments_results_selected()
    list(
      src =  api_endpoint(path = "molecular_model/file",
                          type = "fragment",
                          fragment_id = fragment_id$fragment_id),
      alt = glue::glue("Ball and stick model for a fragment with ID {fragment_id$fragment_id} with SMILES notation {fragment_id$smiles}.")
    )
  },
    deleteFile = FALSE
  )
  output$search_fragment_ballstick_caption <- renderText({
    selected_fragment <- req(search_fragments_results_selected())
    glue::glue("The fragment measured at m/z {round(selected_fragment$fragment_mz, 4)} has been previously annotated as fragment ID {selected_fragment$fragment_id} with structure {selected_fragment$smiles} and has been previously associated with {selected_fragment$compounds}. The measurement error compared with the expected exact mass is {round(selected_fragment$mass_error, 3)} ppm.")
  })
  # _Observers ----
  observeEvent(input$fragments_search_btn, {
    type <- req(input$search_fragments_search_type)
    mzrt <- req(input$search_fragments_mzrt)
    search <- switch(
      type,
      "precursor" = search_precursor,
      "all" = search_all
    )
    # out <- search(data)
    # search_compounds_results(out)
  })
})
