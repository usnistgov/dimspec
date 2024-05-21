shinyServer(function(input, output, session) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Validation rules ----
  exclude_auto_validation <- c("search_compounds_search_zoom", "data_input_waters_lockmass", "data_input_waters_lockmass_width")
  validator <- InputValidator$new()
  sapply(names(app_settings),
         function(x) {
           validator$add_rule(x, sv_required())
           if (!x %in% exclude_auto_validation) {
             vals <- app_settings[[x]]
             if ("this_min" %in% names(app_settings[[x]])) {
               validator$add_rule(x, sv_gte(vals$this_min))
             }
             if ("this_max" %in% names(app_settings[[x]])) {
               validator$add_rule(x, sv_lte(vals$this_max))
             }
             if ("choices" %in% names(app_settings[[x]])) {
               validator$add_rule(x, sv_in_set(vals$choices))
             }
           }
         })
  validator$add_rule("data_input_experiment_type", sv_required())
  validator$add_rule("search_compounds_mzrt", sv_required())
  validator$add_rule("search_fragments_mzrt", sv_required())
  validator$enable()
  
  # Session Data ----
  # _General ----
  advanced_use <- reactive(input$nav_show_advanced_settings)
  show_help <- reactive(input$nav_show_help)
  # _User Data ----
  user_data <- reactiveVal(
    if (!toy_data) {
      NULL
    } else {
      readRDS(src_toy_data)
    }
  )
  # _Data Input page ----
  data_file_loaded <- reactiveVal(FALSE)
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
      readRDS(src_toy_parameters)
    }
  )
  data_input_parameter_edit <- reactiveVal(FALSE)
  # _Compound Match page ----
  # execute_compound_search <- reactiveVal(FALSE)
  search_compounds_results <- reactiveVal(NULL)
  search_compounds_mzrt <- reactive(as.integer(input$search_compounds_mzrt))
  search_compounds_mzrt_text <- reactive(
    data_input_search_parameters() %>%
      mutate(label = glue::glue("m/z {round(precursor, 4)} @ {round(rt, 2)} ({round(rt_start, 2)} - {round(rt_end, 2)})")) %>%
      pull(label)
  )
  search_compounds_row_selected <- reactiveVal(NULL)
  observeEvent(input$search_compounds_dt_rows_selected, {
    old_val <- isolate(search_compounds_row_selected())
    if (is.null(input$search_compounds_dt_rows_selected)) {
      if (is.null(input$search_compounds_dt_row_last_clicked)) {
        new_val <- "1"
      } else {
        new_val <- input$search_compounds_dt_row_last_clicked
      }
    } else {
      new_val <- input$search_compounds_dt_rows_selected
    }
    if (is.null(old_val) || new_val != old_val) search_compounds_row_selected(new_val)
  })
  search_compounds_results_selected <- reactive({
    req(search_compounds_results(),
        search_compounds_row_selected())
    search_compounds_results()$result %>%
      slice(search_compounds_row_selected())
  })
  search_compounds_method_narrative <- reactive({
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
  uncertainty_results <- reactiveVal(NULL)
  # _Fragment Match page ----
  search_fragments_results <- reactiveVal(NULL)
  search_fragments_mzrt <- reactive(as.integer(input$search_fragments_mzrt))
  search_fragments_mzrt_text <- reactive(
    data_input_search_parameters() %>%
      mutate(label = glue::glue("m/z {round(precursor, 4)} @ {round(rt, 2)} ({round(rt_start, 2)} - {round(rt_end, 2)})")) %>%
      pull(label)
  )
  search_fragments_row_selected <- reactiveVal(NULL)
  observeEvent(input$search_fragments_dt_rows_selected, {
    old_val <- isolate(search_fragments_row_selected())
    if (is.null(input$search_fragments_dt_rows_selected)) {
      if (is.null(input$search_fragments_dt_row_last_clicked)) {
        new_val <- "1"
      } else {
        new_val <- input$search_fragments_dt_row_last_clicked
      }
    } else {
      new_val <- input$search_fragments_dt_rows_selected
    }
    if (is.null(old_val) || new_val != old_val) search_fragments_row_selected(new_val)
  })
  search_fragments_results_selected <- reactive({
    req(search_fragments_results(),
        search_fragments_row_selected())
    search_fragments_results()$result %>%
      slice(search_fragments_row_selected())
  })
  search_fragments_compounds_data <- reactive({
    frag_id <- req(search_fragments_results_selected()$norm_fragment_id)
    search_fragment_index <- isolate(search_fragments_mzrt())
    search_fragments_results()$linked_data %>%
      filter(norm_fragment_id == frag_id) %>%
      pull(compounds) %>%
      .[[1]] %>%
      pull(compound_id) %>%
      api_endpoint(
        path = "table_search",
        table_name = "view_compounds",
        match_criteria = list(id = .),
        return_format = "data.frame"
      ) %>%
      select(id, name, category, formula, fixedmass, source_type, obtained_from, additional, local_positive, local_negative, netcharge) %>%
      mutate(
        mz_diff_precursor = abs(
          fixedmass - (
            data_input_search_parameters()[search_fragment_index, ]$precursor
          )
        )
      ) %>%
      arrange(mz_diff_precursor) %>%
      select(-mz_diff_precursor)
  })
  search_fragments_peaks_data <- reactive({
    ann_frag_id <- req(search_fragments_results_selected()$norm_fragment_id)
    search_fragment_index <- req(isolate(search_fragments_mzrt()))
    search_fragments_results()$linked_data %>%
      filter(norm_fragment_id == ann_frag_id) %>%
      pull(peak_ids) %>%
      .[[1]] %>%
      api_endpoint(
        path = "table_search",
        table_name = "view_peaks",
        match_criteria = list(id = .),
        return_format = "data.frame"
      ) %>%
      select(confidence, num_points, precursor_mz, ion_state, rt_centroid, rt_start, rt_end, id, sample_id) %>%
      mutate(
        mz_diff_precursor = abs(
          precursor_mz - (
            data_input_search_parameters()[search_fragment_index, ]$precursor
          )
        )
      ) %>%
      arrange(mz_diff_precursor) %>%
      select(-mz_diff_precursor)
  })
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
  
  # Initial logging statements ----
  log_it("info", glue::glue("App launched with tooltips {ifelse(provide_more_help, 'activated', 'deactivated')}."), app_ns)
  log_it("info", glue::glue("User {ifelse(enable_more_help, 'can', 'cannot')} toggle tooltips."), app_ns)
  log_it("info", glue::glue("App launched with advanced settings {ifelse(isolate(advanced_use()), 'activated', 'deactivated')}."), app_ns)
  log_it("info", glue::glue("User {ifelse(enable_adv_use, 'can', 'cannot')} toggle advanced settings."), app_ns)
  
  # Style adjustments ----
  runjs("$('.box-body.left').parent().parent().addClass('box-left');")
  runjs("$('.box-body.right').parent().parent().addClass('box-right');")
  hideElement(selector = "#data_input_overlay")
  hideElement(selector = "#data_input_next_actions")
  hideElement(selector = "#search_compounds_overlay")
  hideElement(selector = "#search_fragments_overlay")
  hideElement(selector = "#search_fragments_no_results")
  hideElement(selector = "#search_fragments_fragment_info")
  if (!isolate(show_help())) runjs('$(".info-tooltip").hide()')
  
  # Element Display ----
  observe({
    toggleElement("nav_show_help_div", condition = enable_more_help)
    toggleElement("nav_show_advanced_settings_div", condition = enable_adv_use)
    toggleElement("search_compounds_additional", condition = advanced_use())
    toggleElement("data_input_dt_peak_list_edit_row_span", condition = nrow(data_input_search_parameters()) > 0 && !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_dt_peak_list_remove_row_span", condition = nrow(data_input_search_parameters()) > 0 && !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_process_btn", condition = is.null(user_data()))
    toggleElement("data_input_next_actions", condition = !is.null(user_data()))
    toggleElement("nav_download_all", condition = !is.null(user_data()))
    toggleElement("data_input_process_btn", condition = !is.null(input$data_input_filename) && nrow(data_input_search_parameters()) > 0 && is.null(user_data()))
    toggleElement("data_input_dt_peak_list", condition = nrow(data_input_search_parameters()) > 0)
    toggleElement("data_input_waters_settings", condition = input$data_input_is_waters)
    toggleElement("search_compounds_results_span", condition = !is.null(search_compounds_results()) && nrow(search_compounds_results()$result) > 0)
    toggleElement("search_compounds_no_results", condition = !is.null(search_compounds_results()) && nrow(search_compounds_results()$result) == 0)
    toggleElement("search_fragments_results_span", condition = !is.null(search_fragments_results()))
    toggleElement("mod_uncertainty_results", condition = !is.null(uncertainty_results()$results))
    toggleElement("search_fragments_ballstick", condition = !is.null(search_fragments_results_selected()))
    toggleElement("search_fragments_plot_div", condition = !is.null(search_fragments_results()))
    toggleElement("search_fragments_no_results", condition = nrow(search_fragments_results()$result) == 0)
    toggleElement("search_fragments_has_results", condition = nrow(search_fragments_results()$result) > 0)
  })
  observeEvent(show_help(), ignoreInit = TRUE, {
    if (input$nav_show_help) {
      nist_shinyalert(
        title = "Tooltips Enabled",
        type = "info",
        text = span(
          "Look for a question mark icon (",
          icon("question"),
          ") next to any control and hover over it to get more information about that control.",
        )
      )
      log_it("info", "Tooltips activated.", app_ns)
      runjs('$(".info-tooltip").show()')
    } else {
      nist_shinyalert(
        title = "Tooltips Disabled",
        type = "info",
        text = 'Click the toggle switch at the bottom left of the screen labeled "Show Tooltips" at any time to re-enable tooltips.'
      )
      log_it("info", "Tooltips deactivated.", app_ns)
      runjs('$(".info-tooltip").hide()')
    }
  })
  observeEvent(advanced_use(), ignoreInit = TRUE, {
    if (advanced_use()) {
      log_it("info", "Advanced settings activated.", app_ns)
    } else {
      log_it("info", "Advanced settings deactivated.", app_ns)
    }
  })
  
  # Navigation ----
  observeEvent(input$sidebar_menu, {
    log_it("info", glue::glue("Nav menu page '{input$sidebar_menu}' requested."), app_ns)
    if (!dev) {
      if (!input$sidebar_menu %in% c("data_input", "index", "about") && is.null(user_data())) {
        has_data_file <- !is.null(input$data_input_filename)
        has_search_parameters <- nrow(data_input_search_parameters()) > 0
        if (has_data_file && !has_search_parameters) {
          msg <- "Please add search parameters first."
        } else if (!has_data_file && has_search_parameters) {
          msg <- "Please load a data file first."
        } else if (has_data_file && has_search_parameters) {
          msg <- 'Please click the "Process Data" button.'
        } else {
          msg <- "Please load a data file and add search parameters first."
        }
        log_it("warn", glue::glue("Insufficient data to activate page '{input$sidebar_menu}': {msg}."), app_ns)
        nist_shinyalert(title = "Insufficient data",
                        type = "info",
                        text = msg)
        updateTabsetPanel(session = session,
                          inputId = "sidebar_menu",
                          selected = "data_input")
      } else {
        log_it("info", glue::glue("Page '{input$sidebar_menu}' activated."), app_ns)
      }
    }
  })
  # Download All Utility (PLANNED FEATURE) ----
  # observeEvent(input$nav_download_all, {
  #   nist_shinyalert(
  #     title = "This action may take a long time.",
  #     type = "warning",
  #     text = tagList(
  #       p("This application is intended for interactive use. Complete downloads are provided as a convenience. Please confirm."),
  #       actionButton(inputId = "nav_download_all_confirm",
  #                    label = "Confirm",
  #                    icon = icon("exclamation"),
  #                    width = "100%"),
  #       actionButton(inputId = "nav_download_all_cancel",
  #                    label = "Cancel",
  #                    icon = icon("times"),
  #                    width = "100%")
  #     ),
  #     showCancelButton = FALSE,
  #     showConfirmButton = FALSE
  #   )
  # })
  # observeEvent(input$nav_download_all_confirm, {
  #   showModal(mod_download_all())
  # })
  # output$mod_download_all_save <- downloadHandler(
  #   filename = function() glue::glue("{default_title} matches from file {input$data_input_filename$name}.xlsx"),
  #   content = function() {
  #     to_export <- list(
  #       app_settings = data.frame(
  #         all app settings here in a 2xN data frame showing the settings under which these matches were produced
  #       ),
  #       loop through features of interest
  #         execute compound matching
  #         execute fragment matching
  #     )
  #     pack_as_excel(to_export) # TODO write this function
  #   }
  # )
  # observeEvent(input$mod_download_all_cancel, removeModal())
  
  # HOME PAGE ----
  observeEvent(input$index_go_data_input, {
    updateTabsetPanel(session = session,
                      inputId = "sidebar_menu",
                      selected = "data_input")
    log_it("trace", "Button 'index_go_data_input' clicked.", app_ns)
  })
  
  # DATA INPUT PAGE ----
  # _Outputs ----
  # __Data Table ----
  output$data_input_dt_peak_list <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data = data_input_search_parameters(),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      editable = FALSE,
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
  observeEvent(data_input_search_parameters(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    log_it("trace", "New search parameters provided.", app_ns)
    n_orig <- nrow(data_input_search_parameters())
    n_uniq <- nrow(distinct(data_input_search_parameters()))
    if (!n_orig == n_uniq) {
      nist_shinyalert(
        title = "Duplicate values detected",
        type = "info",
        text = glue::glue("Search parameters must represent unique combinations. Duplicated settings (n = {n_orig - n_uniq}) have been removed.")
      )
      log_it("warn", glue::glue("Duplicate (n = {n_orig - n_uniq}) search parameters provided and removed."), app_ns)
    }
    data_input_search_parameters() %>%
      distinct() %>%
      arrange(precursor, rt) %>%
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
      log_it("warn", "No search parameters provided.", app_ns)
    } else {
      updateSelectizeInput(
        session = session,
        inputId = "search_compounds_mzrt",
        choices = search_compounds_mzrt_text() %>%
          setNames(object = 1:length(.),
                   nm = .),
        options = list(placeholder = "Please select a Feature of Interest to proceed.")
      )
      updateSelectizeInput(
        session = session,
        inputId = "search_fragments_mzrt",
        choices = search_fragments_mzrt_text() %>%
          setNames(object = 1:length(.),
                   nm = .),
        options = list(placeholder = "Please select a Feature of Interest to proceed.")
      )
      log_it("trace", "Search parameters updated.", app_ns)
    }
    hideElement(id = "data_input_next_actions")
  })
  # __Verify isolation width / experiment type ----
  observeEvent({
    input$data_input_isolation_width
    input$data_input_experiment_type
  }, {
    req(input$data_input_isolation_width, input$data_input_experiment_type)
    if (input$data_input_isolation_width > app_settings$data_input_isolation_width_warn_threshold) {
      types <- app_settings$experiment_types$choices
      experiment_type <- names(types[types == input$data_input_experiment_type])
      if (!str_detect(experiment_type, "SWATH")) {
        nist_shinyalert(
          title = "Abnormal value suspected",
          type = "info",
          text = glue::glue("Typically, the value of Isolation Width should be less than 4 Da unless the experiment type is SWATH or HRM.")
        )
        log_it("warn", glue::glue("Isolation width ({input$data_input_isolation_width}) was not appropriate for the chosen experiment type ('{experiment_type}')."), app_ns)
      }
    }
  })
  # __Data file upload ----
  observeEvent(input$data_input_filename, {
    req(input$data_input_filename)
    fn <- input$data_input_filename
    valid <- valid_file_format(fn$name, app_settings$data_input_import_file_types)
    if (!valid) {
      log_it("warn", glue::glue("Invalid file format ({tools::file_ext(fn$name)}) for 'data_input_filename'."), app_ns)
      reset("data_input_filename")
    } else {
      log_it("success", glue::glue("Data file selected ('{fn$name}') for 'data_input_filename'."), app_ns)
      if (data_file_loaded()) {
        data_input_search_parameters(slice(data_input_search_parameters(), 0))
        search_compounds_results(NULL)
        search_fragments_results(NULL)
      }
    }
    data_file_loaded(TRUE)
    user_data(NULL)
  })
  # __Import parameters ----
  observeEvent(input$data_input_import_search, {
    req(input$data_input_import_search)
    fn <- input$data_input_import_search
    if (!valid_file_format(fn$name, app_settings$data_input_import_search_settings_types)) {
      log_it("warn", glue::glue("Invalid file format ({tools::file_ext(fn$name)}) for 'data_input_import_search'."), app_ns)
      reset("data_input_import_search")
    } else {
      upload <- switch(
        tools::file_ext(fn$name),
        "csv" = read_csv(file = fn$datapath),
        "xls" = read_xls(path = fn$datapath, sheet = 1),
        "xlsx" = read_xlsx(path = fn$datapath, sheet = 1)
      )
      data_input_search_upload(upload)
      log_it("success", glue::glue("Data file selected ('{fn$name}') for 'data_input_import_search'."), app_ns)
      showModal(mod_data_input_parameters_upload(data = upload))
      if (!show_help()) runjs('$(".info-tooltip").hide()')
    }
  })
  observeEvent(input$mod_upload_parameter_cancel, {
    reset("data_input_import_search")
    removeModal()
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
      log_it("warn", "Duplicate columns selected in search parameter upload", app_ns)
    } else {
      upload_parameters <- data_input_search_upload() %>%
        select(all_of(select_cols))
      # Check complete cases
      which_complete <- complete.cases(upload_parameters)
      if (any(!which_complete)) {
        cols_with_na <- upload_parameters %>%
          select(where(~any(is.na(.)))) %>%
          names()
        nist_shinyalert(
          title = "Incomplete data",
          type = "warning",
          text = glue::glue("Some rows (n = {sum(!which_complete)} of {length(which_complete)}) in the file \"{input$data_input_import_search$name}\" do not contain values for {format_list_of_names(cols_with_na)}. Those rows have been removed. Please add any missing parameters manually OR adjust your file to contain complete values for all features of interest. Once fixed, you may upload this file again and any duplicates will be automatically removed.")
        )
        upload_parameters <- upload_parameters[which_complete, ]
        log_it("warn", "Missing values in search parameter save.", app_ns)
      }
      upload_parameters <- upload_parameters %>%
        mutate(across(everything(), as.numeric))
      # Check that all are numeric
      coercion_check <- complete.cases(upload_parameters)
      if (any(!coercion_check)) {
        cols_nonnumeric <- upload_parameters %>%
          select(where(~any(is.na(.)))) %>%
          names()
        nist_shinyalert(
          title = "Non-numeric data",
          type = "warning",
          text = glue::glue("Some rows (<em>n</em> = {sum(!coercion_check)} of {length(coercion_check)}) in the file \"{input$data_input_import_search$name}\" do not contain numeric values for {format_list_of_names(cols_nonnumeric)}. Please select columns containing only numeric data or adjust these in your upload file and try again.")
        )
        log_it("error", "Search parameters were not numeric.", app_ns)
        return(NULL)
      }
      upload_parameters <- upload_parameters %>%
        setNames(names(data_input_search_parameters()))
      # Check for reasonable retention times
      reasonable_rts <- upload_parameters %>%
        mutate(reasonable_rt = (rt >= rt_start & rt <= rt_end | rt >= rt_end & rt <= rt_start),
               reasonable_rtstart = rt_start <= rt_end,
               reasonable = all(reasonable_rt, reasonable_rtstart))
      if (!all(reasonable_rts$reasonable)) {
        bad_entries <- reasonable_rts %>%
          filter(!reasonable,
                 across(everything(), ~ !is.na(.x))) %>%
          mutate(msg_prefix = glue::glue("<div style='text-align: left; padding-top: 2px;' class='one-line'><p>For precursor <em>m/z</em> value <p style='font-weight: bold;'>{precursor}</p>, "),
                 msg_rt = ifelse(reasonable_rt,
                                 "",
                                 glue::glue("RT <p style='font-weight: bold; color: red;'>{rt}</p> does not fall between <p style='font-weight: bold;'>{rt_start}</p> and <p style='font-weight: bold;'>{rt_end}</p>.")),
                 msg_rtstart = ifelse(reasonable_rtstart,
                                      "",
                                      glue::glue("RT Start <p style='font-weight: bold; color: red;'>{rt_start}</p> was after RT End <p style='font-weight: bold; color: red;'>{rt_end}</p>.")),
                 message = glue::glue("{msg_prefix} {msg_rt} {msg_rtstart}</p></div>")
          )
        nist_shinyalert(
          title = "Unreasonable retention times",
          size = "m",
          type = "error",
          text = glue::glue(
            "<p style='font-weight: bold;'>Please select columns carefully or address the following issues in the source file '{input$data_input_import_search$name}' and try again.</p><br><ul><li>{paste0(bad_entries$message, collapse = '</li><li>')}</li></ul>"
          )
        )
        log_it("warn", "Unreasonable retention times identified during search parameter save.", app_ns)
        return(NULL)
      } else {
        if (input$mod_upload_parameter_append) {
          upload_parameters <- data_input_search_parameters() %>%
            bind_rows(upload_parameters)
        }
        data_input_search_parameters(upload_parameters)
        user_data(NULL)
        log_it("success", "Search parameters updated.", app_ns)
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
    log_it("trace", "Request to add a search parameter manually.", app_ns)
    showModal(mod_data_input_parameters_manual())
    if (!show_help()) runjs('$(".info-tooltip").hide()')
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
    log_it("trace", "Request to edit a search parameter manually.", app_ns)
    data_input_parameter_edit(TRUE)
    showModal(mod_data_input_parameters_manual())
    if (!show_help()) runjs('$(".info-tooltip").hide()')
  })
  observeEvent(input$data_input_dt_peak_list_remove_row, {
    log_it("trace", "Request to delete a search parameter manually.", app_ns)
    req(input$data_input_dt_peak_list_rows_selected)
    data_input_search_parameters()[-input$data_input_dt_peak_list_rows_selected, ] %>%
      data_input_search_parameters()
    user_data(NULL)
  })
  observeEvent(input$mod_search_parameter_cancel, {
    log_it("trace", "Search parameter entry modal closed.", app_ns)
    removeModal()
  })
  observeEvent(input$mod_data_input_search_parameter_save, {
    valid <- complete_form_entry(input, mod_search_params)
    if (valid) {
      log_it("warn", "Search parameter save request: modal form was complete.", app_ns)
    } else {
      log_it("trace", "Search parameter save request: modal form was incomplete.", app_ns)
    }
    req(valid)
    tmp <- data_input_search_parameters()
    values <- data.frame(
      input$mod_search_parameter_precursor,
      input$mod_search_parameter_rt,
      input$mod_search_parameter_rt_start,
      input$mod_search_parameter_rt_end
    ) %>%
      setNames(names(data_input_search_parameters()))
    if (!is.null(input$data_input_dt_peak_list_rows_selected) && data_input_parameter_edit()) {
      tmp <- tmp[-input$data_input_dt_peak_list_rows_selected, ]
    }
    valid <- TRUE
    if (!input$mod_search_parameter_rt >= input$mod_search_parameter_rt_start) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Centroid) should be after Retention Time (Start).")
      valid <- FALSE
      log_it("error", "Search parameter save request: retention time centroid was before retention time start.", app_ns)
    }
    if (!input$mod_search_parameter_rt <= input$mod_search_parameter_rt_end) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Centroid) should be before Retention Time (End).")
      valid <- FALSE
      log_it("error", "Search parameter save request: retention time centroid was after retention time end.", app_ns)
    }
    if (!input$mod_search_parameter_rt_start <= input$mod_search_parameter_rt_end) {
      nist_shinyalert(title = "Data Entry Validation",
                      type = "warning",
                      text = "Retention Time (Start) should be before Retention Time (End).")
      valid <- FALSE
      log_it("error", "Search parameter save request: retention time start was after retention time centroid.", app_ns)
    }
    if (valid) {
      tmp %>%
        bind_rows(values) %>%
        data_input_search_parameters()
      log_it("success", "Search parameter save request successful.", app_ns)
      removeModal()
      data_input_parameter_edit(FALSE)
    }
    user_data(NULL)
  })
  # __Process Data ----
  observeEvent(input$data_input_process_btn, {
    log_it("trace", "Button 'data_input_process_btn' clicked.", app_ns)
    req(
      input$data_input_filename,
      nrow(data_input_search_parameters() > 0)
    )
    required <- grep("data_input", names(input), value = TRUE)
    if (!input$data_input_is_waters) {
      required <- required[-grep("waters", required)]
    }
    required <- required[-grep("dt_peak_list|import_search|filename|_process|^mod_data|_go_", required)]
    valid <- complete_form_entry(input, required)
    if (valid) {
      log_it("warn", "Data input process form was complete.", app_ns)
    } else {
      log_it("trace", "Data input process form was incomplete.", app_ns)
    }
    req(valid)
    showElement(selector = "#data_input_overlay")
    log_it("info", sprintf("Processing user data from file %s...", input$data_input_filename$name), app_ns)
    mzml <- try(
      getmzML(
        search_df = data.frame(filename = input$data_input_filename$datapath),
        is_waters = input$data_input_is_waters,
        lockmass = input$data_input_waters_lockmass,
        lockmasswidth = input$data_input_waters_lockmass_width,
        correct = input$data_input_waters_lockmass_correct
      )
    )
    if (inherits(mzml, "try-error")) {
      nist_shinyalert(
        title = "Conversion issue",
        type = "error",
        text = "Data from this file could not be safely extracted."
      )
      log_it("error", "User data could not be safely extracted.", app_ns)
    } else {
      hideElement("data_input_overlay")
      log_it("success", "User data processed.", app_ns)
      user_data(mzml)
    }
  })
  # __Navigation to next ----
  observeEvent(input$data_input_go_compound, {
    log_it("trace", "Button 'data_input_go_compound' clicked.", app_ns)
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "search_compounds"
    )
  })
  observeEvent(input$data_input_go_fragment, {
    log_it("trace", "Button 'data_input_go_fragment' clicked.", app_ns)
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "search_fragments"
    )
  })
  
  # COMPOUND MATCH PAGE ----
  # _Outputs ----
  # __Match statuses ----
  output$search_compounds_match_top <- renderUI({
    req(search_compounds_results())
    matches <- search_compounds_results()$result
    ranks <- table(matches$name)
    top_match <- matches$name[1]
    most_common_match <- ranks[which.max(ranks)]
    n_most_common <- paste0(
      em("n"), " = ", most_common_match, " of ", sum(ranks)
    )
    matches <- slice(matches, 1)
    if (names(most_common_match) == top_match) {
      match_common <- tagList(
        icon("check", class = "success", verify_fa = FALSE),
        HTML(paste0("<p>This is the most common (", n_most_common,
                    ") compound match.</p>",
                    collapse = ""))
      )
    } else {
      match_common <- tagList(
        icon("exclamation", class = "warning", verify_fa = FALSE),
        HTML(paste0("<p>The most common (", n_most_common,
                    ") compound match is ", strong(names(most_common_match)), ".</p>",
                    collapse = ""))
      )
    }
    match_title <- h3("Top match is",
                      strong(matches$name),
                      "from",
                      ifelse(substr(matches$sample_class, 1, 1) %in% vowels,
                             "an",
                             "a"),
                      matches$sample_class)
    if (!is.na(matches$ms1_dp)) {
      match_score1 <- p(
        "MS1 Score: ",
        p(style = "font-weight: bold;", matches$ms1_dp),
        paste0("(rev score ", matches$ms1_rdp, ")", collapse = "")
      )
    } else {
      match_score1 <- p(style = "color: darkgrey", "No MS1 Score")
    }
    if (!is.na(matches$ms2_dp)) {
      match_score2 <- p(
        "| MS2 Score: ",
        p(style = "font-weight: bold;;", matches$ms2_dp),
        paste0("(rev score ", matches$ms2_rdp, ")", collapse = "")
      )
    } else {
      match_score2 <- p(style = "color: darkgrey", "| no MS2 Score")
    }
    tagList(
      div(class = "one-line", match_title),
      div(class = "one-line", match_score1, match_score2),
      div(class = "one-line", match_common)
    )
  })
  output$search_compounds_match_selected <- renderUI({
    req(
      search_compounds_results(),
      search_compounds_results_selected()
    )
    if (search_compounds_row_selected() == 1) {
      NULL
    } else {
      matches <- search_compounds_results_selected()
      match_out <- h4(
        "Selected comparison is",
        p(style = "font-weight: bold; display: inline;", matches$name),
        "from",
        ifelse(substr(matches$sample_class, 1, 1) %in% vowels,
               "an",
               "a"),
        matches$sample_class
      )
      if (!is.na(matches$ms1_dp)) {
        match_score1 <- p(
          "MS1 Score: ",
          p(style = "font-weight: bold;", matches$ms1_dp),
          paste0("(rev score ", matches$ms1_rdp, ")", collapse = "")
        )
      } else {
        match_score1 <- p(style = "color: darkgrey", "No MS1 Score")
      }
      if (!is.na(matches$ms2_dp)) {
        match_score2 <- p(
          "| MS2 Score: ",
          p(style = "font-weight: bold;;", matches$ms2_dp),
          paste0("(rev score ", matches$ms2_rdp, ")", collapse = "")
        )
      } else {
        match_score2 <- p(style = "color: darkgrey", "| no MS2 Score")
      }
      div(class = "one-line", tagList(match_out, match_score1, match_score2))
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
      colnames = c("Compound ID", "Compound", "MS1 Score", "MS1 Score (Rev)", "MS2 Score", "MS2 Score (Rev)", "# Annotated Fragments", "# Annotated Structures", "# Annotated Citations", "Sample Class", "Peak ID"),
      caption = "Select a row to view the match or send it to uncertainty estimation, or click a button at the bottom to save this table.",
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = ifelse(nrow(search_compounds_results()$result) <= 10, "tB", "tBp"),
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
      search_compounds_row_selected()
    )
    plot_compare_ms(
      ums1 = search_compounds_results()$search_object[[sprintf("u%s", tolower(input$search_compounds_msn))]],
      ums2 = search_compounds_results()[[sprintf("u%s_compare", tolower(input$search_compounds_msn))]][[search_compounds_row_selected()]],
      ylim.exp = 0.1,
      main = element_blank()
    ) %>%
      ggplotly()
  })
  # __Method narrative ----
  output$search_compounds_method_narrative <- renderUI(
    tags$caption(
      style = "display: inline;",
      req(search_compounds_method_narrative())
    )
  )
  # _Observers ----
  # __Navigation ----
  observeEvent(input$search_compounds_go_data_input, {
    log_it("trace", "Button 'search_compounds_go_data_input' clicked.", app_ns)
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "data_input"
    )
  })
  # __Update to MS1 if no MS2 ----
  observe({
    req(search_compounds_results())
    if (is.na(search_compounds_results_selected()$ms2_dp) &&
        input$search_compounds_msn == "MS2") {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "search_compounds_msn",
        selected = "MS1"
      )
      nist_shinyalert(
        title = NULL,
        type = "info",
        text = "There are no MS2 data to evaluate."
      )
      log_it("warn", "MS2 data requested but no MS2 data were found.", app_ns)
      if (!is.null(uncertainty_results())) {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "mod_uncertainty_msn",
          selected = "MS1"
        )
      }
    }
  })
  # __Update to MS2 if no MS1 ----
  observe({
    req(search_compounds_results())
    if (is.na(search_compounds_results_selected()$ms1_dp) &&
        input$search_compounds_msn == "MS1") {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "search_compounds_msn",
        selected = "MS2"
      )
      if (!is.null(uncertainty_results())) {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "mod_uncertainty_msn",
          selected = "MS2"
        )
        log_it("warn", "MS1 data requested but no MS1 comparison was possible.", app_ns)
      }
    }
  })
  # __Execute search ----
  observeEvent(input$search_compounds_search_btn, {
    log_it("trace", "Button 'search_compounds_search_btn' clicked.", app_ns)
    if (input$search_compounds_mzrt == "") {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please choose a Feature of Interest"
      )
      log_it("warn", "No feature of interest selected in 'search_compounds_mzrt'.", app_ns)
      return()
    }
    if (input$search_compounds_search_type == "") {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please choose a Search Type"
      )
      log_it("warn", "No search type selected in 'search_compounds_search_type'.", app_ns)
      return()
    }
    if (input$search_compounds_search_type == "all") {
      nist_shinyalert(
        inputId = "execute_compound_search",
        title = "Long action",
        type = "info",
        text = "This will search your measured spectrum against all records in the database and may take quite a while to execute.",
        # closeOnEsc = FALSE,
        # closeOnClickOutside = FALSE,
        # showCancelButton = TRUE,
        # confirmButtonText = "Proceed"
      )
      log_it("info", "User requested long-running search of type 'all'.", app_ns)
    }
    # execute_compound_search(TRUE)
    # })
    # observeEvent(execute_compound_search(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    #   if (!execute_compound_search)
    #   if (input$execute_compound_search) {
    #     log_it("info", "User confirmed long-running search of type 'all'.", app_ns)
    #   } else {
    #     log_it("info", "User cancelled search type 'all'.", app_ns)
    #     return()
    #   }
    req(
      user_data(),
      data_input_search_parameters(),
      input$search_compounds_search_type,
      input$data_input_relative_error,
      input$data_input_minimum_error,
      input$data_input_experiment_type,
      input$data_input_isolation_width,
      input$search_compounds_search_zoom,
      input$search_compounds_max_correl,
      input$search_compounds_correl_bin,
      input$search_compounds_ph,
      input$search_compounds_max_ph,
      input$search_compounds_ph_bin,
      input$search_compounds_max_freq,
      input$search_compounds_freq_bin,
      input$search_compounds_min_n_peaks
    )
    search_compound_index <- req(search_compounds_mzrt())
    runjs("$('#search_compounds_overlay_text').text('Executing compound search...');")
    showElement("search_compounds_overlay")
    runjs("$('#search_compounds_overlay_text').text('Validating inputs...');")
    log_it("trace", "Validating compound search inputs.", app_ns)
    mzrt <- isolate(data_input_search_parameters()[search_compound_index, ])
    log_it("info", glue::glue("Compound search started for {search_compounds_mzrt_text()[search_compound_index]}."), app_ns)
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
    log_it("trace", "Creating search object.", app_ns)
    search_object <- try(
      get_search_object(
        searchmzml = tmp,
        zoom = isolate(input$search_compounds_search_zoom)
      )
    )
    if (inherits(search_object, "try-error")) {
      msg <- sprintf("Could not find a feature in data file %s matching %s",
                     input$data_input_filename$name,
                     search_compounds_mzrt_text()[search_compound_index])
      nist_shinyalert(
        title = NULL,
        type = "warning",
        text = msg
      )
      hideElement("search_compounds_overlay")
      log_it("warn", msg, app_ns)
      return(NULL)
    }
    #needed to make this search_ms object because it needs to check and re-run and should not replace object
    search_ms <- 
      try(create_search_ms(
        searchobj = search_object,
        correl = isolate(input$search_compounds_correlation),
        ph = isolate(input$search_compounds_ph),
        freq = isolate(input$search_compounds_freq),
        normfn = isolate(input$search_compounds_norm_function),
        cormethod = isolate(input$search_compounds_correlation_method)
      ))
    if (inherits(search_ms, "try-error")) {
      msg <- sprintf("Could not generate consensus mass spectra for data file %s and compound %s using optimized parameters due to sparse MS2 data. The application will attempt to calculate non-optimal consensus mass spectra.",
                     input$data_input_filename$name,
                     search_compounds_mzrt_text()[search_compound_index])
      nist_shinyalert(
        title = NULL,
        type = "warning",
        text = msg
      )
      log_it("warn", msg, app_ns)
      search_ms <- 
        try(create_search_ms(
          searchobj = search_object,
          correl = NA,
          ph = NA,
          freq = NA,
          normfn = isolate(input$search_compounds_norm_function),
          cormethod = isolate(input$search_compounds_correlation_method)
        ))
    }
    if (inherits(search_ms, "try-error")) {
      msg <- sprintf("Could not generate consensus mass spectra for data file %s and compound %s due to sparse MS2 data.",
                     input$data_input_filename$name,
                     search_compounds_mzrt_text()[search_compound_index])
      nist_shinyalert(
        title = NULL,
        type = "warning",
        text = msg
      )
      hideElement("search_compounds_overlay")
      log_it("warn", msg, app_ns)
      return(NULL)
    }
    search_object <- search_ms #had to swap these due to language following this point
    runjs("$('#search_compounds_overlay_text').text('Scoring database matches...');")
    log_it("trace", "Scoring database matches.", app_ns)
    search_result <- api_endpoint(
      path               = "search_compound",
      type               = isolate(input$search_compounds_search_type),
      search_ms          = jsonlite::toJSON(search_object),
      norm_function      = isolate(input$search_compounds_norm_function),
      correlation_method = isolate(input$search_compounds_correlation_method),
      optimized_params   = isolate(input$search_compounds_use_optimized_parameters),
      return_format      = "list"
    )
    log_it("trace", "Creating compound search results.", app_ns)
    if (!is.list(search_result) || length(search_result$result) == 0) {
      if (!is.list(search_result)) {
        msg <- glue::glue("There was a problem with the API call when searching for compound matches for the feature of interest at {search_compounds_mzrt_text()[search_compounds_mzrt()]}.")
        msg_title <- "Communication Error"
      } else {
        msg <- glue::glue("No compound matches were found for the feature of interest at {search_compounds_mzrt_text()[search_compounds_mzrt()]}.")
        msg_title <- "No Matches"
      }
      nist_shinyalert(type = "warning", title = msg_title, text = msg)
      log_it("warn", msg, app_ns)
    } else {
      search_result$result <- search_result$result %>%
        mutate(index = 1:n()) %>%
        filter(if_any(starts_with("ms"), ~!is.na(.))) %>%
        mutate(sum_match = rowSums(across(starts_with("ms")), na.rm = TRUE)) %>%
        arrange(desc(sum_match))
      search_result$ums1_compare <- search_result$ums1_compare[search_result$result$index]
      search_result$ums2_compare <- search_result$ums2_compare[search_result$result$index]
      search_result$result <- select(search_result$result, -index)
      search_compounds_results(
        list(
          search_object = search_object,
          result = search_result$result,
          ums1_compare = lapply(search_result$ums1_compare, bind_rows),
          ums2_compare = lapply(search_result$ums2_compare, bind_rows)
        )
      )
      log_it("success", glue::glue("Compound search complete with n = {nrow(search_result$result)} matches found."), app_ns)
    }
    hideElement("search_compounds_overlay")
  })
  # __Evaluate uncertainty (modal) ----
  observeEvent(input$search_compounds_uncertainty_btn, ignoreNULL = TRUE, ignoreInit = TRUE, {
    log_it("trace", "Button 'search_compounds_uncertainty_btn' clicked.", app_ns)
    uncertainty_results(NULL)
    showModal(mod_uncertainty_evaluation(input$search_compounds_msn, advanced_use()))
    if (!show_help()) runjs('$(".info-tooltip").hide()')
    if (!is.null(input$mod_uncertainty_msn) && input$mod_uncertainty_msn == input$search_compounds_msn) click("mod_uncertainty_calculate")
  })
  observeEvent(input$mod_uncertainty_calculate, ignoreNULL = TRUE, ignoreInit = TRUE, {
    log_it("info", "Calculating uncertainty.", app_ns)
    tmp <- req(search_compounds_results())
    runjs(sprintf("$('#search_compounds_overlay_text').text('Running %s bootstrap iterations...');",
                  input$mod_uncertainty_iterations))
    showElement("search_compounds_overlay")
    n_runs <- as.numeric(gsub(",", "", input$mod_uncertainty_iterations))
    compare_actual <- sprintf("u%s", tolower(input$mod_uncertainty_msn))
    compare_with <- sprintf("u%s_compare", tolower(input$mod_uncertainty_msn))
    bootstrap_compare_ms(
      ms1 = tmp$search_object[[compare_actual]],
      ms2 = tmp[[compare_with]][[search_compounds_row_selected()]],
      runs = n_runs
    ) %>%
      uncertainty_results()
    hideElement("search_compounds_overlay")
  })
  observeEvent(input$mod_uncertainty_msn, {
    log_it("trace", "Changing MSn in uncertainty modal.", app_ns)
    if (is.na(search_compounds_results_selected()$ms2_dp) &&
        input$mod_uncertainty_msn == "MS2") {
      nist_shinyalert(
        title = NULL,
        type = "info",
        text = "There are no MS2 match scores to evaluate."
      )
      log_it("warn", "There are no MS2 match scores to evaluate uncertainty.", app_ns)
      updateRadioGroupButtons(
        session = session,
        inputId = "mod_uncertainty_msn",
        selected = input$search_compounds_msn
      )
    } else {
      updateRadioGroupButtons(
        session = session,
        inputId = "search_compounds_msn",
        selected = input$mod_uncertainty_msn
      )
      log_it("trace", "Updating MSn choice on 'search_compounds' page.", app_ns)
      click("mod_uncertainty_calculate")
    }
  })
  output$mod_uncertainty_match_dt <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data = req(search_compounds_results_selected()) %>%
        select(compound_id, name, ms1_dp, ms1_rdp, ms2_dp, ms2_rdp,total_ann_fragments, total_ann_structures, total_ann_citations, sample_class, peak_ids),
      rownames = FALSE,
      selection = list(mode = "none"),
      autoHideNavigation = TRUE,
      colnames = c("Compound ID", "Compound", "MS1", "MS1 (Rev)", "MS2", "MS2 (Rev)", "# Annotated Fragments", "# Annotated Structures", "# Annotated Citations", "Sample Class", "Peak ID"),
      extensions = "Responsive",
      options = list(
        dom = "t",
        pageLength = 10,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 10)),
          list(className = "dt-center", targets = 2:9),
          list(className = "dt-left", targets = 1),
          list(responsivePriority = 1, targets = 1:5),
          list(responsivePriority = 10000, targets = c(0, 6:10)),
          list("min-width" = "200px", targets = 1)
        )
      )
    ))
  output$mod_uncertainty_narrative <- renderUI({
    req(uncertainty_results())
    scores <- uncertainty_results()$result
    include_nan <- input$mod_uncertainty_include_nan
    oob_iter <- scores |>
      filter(is.nan(dp) | is.nan(rdp)) |>
      nrow()
    if (include_nan) {
      scores$dp[which(is.nan(scores$dp))] <- 0
      scores$rdp[which(is.nan(scores$rdp))] <- 0
    } else {
      scores <- scores |>
        filter(!is.nan(dp), !is.nan(rdp))
    }
    iter <- as.numeric(req(isolate(input$mod_uncertainty_iterations)))
    if (nrow(scores) == iter && !include_nan) {
      hide("mod_uncertainty_include_nan")
    } else {
      show("mod_uncertainty_include_nan")
    }
    tagList(
      if (!nrow(scores) == iter | include_nan) {
        p(glue::glue("Attributes were sampled outside error tolerance levels for {oob_iter} iterations; these have been {ifelse(include_nan, 'included as zeros for', 'excluded from')} summary statistics."))
      },
      p(glue::glue("Forward match scores ranged {paste0(round(range(scores$dp), 4), collapse = ' - ')} with an IQR of {round(IQR(scores$dp), 4)} and median of {round(median(scores$dp), 4)}.")),
      p(glue::glue("Reverse match scores ranged {paste0(round(range(scores$rdp), 4), collapse = ' - ')} with an IQR of {round(IQR(scores$rdp), 4)} and median of {round(median(scores$rdp), 4)}.")),
      # hr()
    )
  })
  output$mod_uncertainty_boxplot <- renderPlot({
    res <- req(uncertainty_results()$results)
    # ADDED PLAYING --v
    if (input$mod_uncertainty_include_nan) {
      res$dp[which(is.nan(res$dp))] <- 0
      res$rdp[which(is.nan(res$rdp))] <- 0
    }
    # ADDED PLAYING --^
    iter <- req(isolate(input$mod_uncertainty_iterations))
    msn <- req(isolate(input$mod_uncertainty_msn))
    real_match <- req(isolate(search_compounds_results_selected())) %>%
      select(starts_with(tolower(msn))) %>%
      pivot_longer(everything()) %>%
      mutate(name = case_when(
        grepl("_rdp", name) ~ "Reverse",
        grepl("_dp", name) ~ "Forward")
      )
    out <- tibble(
      name = factor(
        c(
          rep("Forward", nrow(res)),
          rep("Reverse", nrow(res))
        ),
        levels = c("Forward", "Reverse")
      ),
      value = c(res$dp, res$rdp
      )
    ) %>%
      ggplot(
        aes(x = name,
            y = value,
            fill = name)
      ) +
      geom_boxplot(show.legend = FALSE) +
      geom_point(
        data = real_match,
        shape = 23,
        size = 4,
        fill = "green",
        show.legend = FALSE
      ) +
      theme_bw() +
      labs(
        title = glue::glue("Range of Match Scores ({iter} Iterations)"),
        subtitle = glue::glue("{msn} match for {search_compounds_results_selected()$name}"),
        caption = "Original match scores are shown as green diamonds.",
        x = "Direction",
        y = "Score"
      )
    out
  })
  observeEvent(input$mod_uncertainty_close, removeModal())
  
  # FRAGMENT MATCH PAGE ----
  # _Outputs ----
  # __Spectrum plot ----
  # output$search_fragments_spectral_plot <- renderPlotly({
  output$search_fragments_spectral_plot <- renderPlot({
    shiny::validate(
      need(nrow(search_fragments_results()$spectra) > 0,
           message = "No spectra available to plot.")
    )
    dat <- search_fragments_results()$spectra %>%
      arrange(mz, norm_fragment_id) %>%
      select(match, mz, mz.u, int, int.u, formula) %>%
      distinct()
    conf <- dat$match
    cap1 <- if ("unknown" %in% conf) glue::glue('{sum(conf == "unknown")} unknown fragment{ifelse(sum(conf == "unknown") == 1, "", "s")}.') else NULL
    cap2 <- if ("formula" %in% conf) glue::glue('{sum(conf == "formula")} fragment{ifelse(sum(conf == "formula") == 1, "", "s")} with an annotated formula.') else NULL
    cap3 <- if ("structure" %in% conf) glue::glue('{sum(conf == "structure")} fragment{ifelse(sum(conf == "structure") == 1, "", "s")} with an annotated structure.') else NULL
    chosen_mzrt <- isolate(search_fragments_mzrt_text()[as.numeric(search_fragments_mzrt())])
    this_caption <- paste0(c(chosen_mzrt, cap1, cap2, cap3), collapse = "\n")
    p <- dat %>%
      ggplot(aes(x = mz, y = int, ymin = 0, ymax = int, color = match))
    if (packageVersion("ggplot2") >= '3.4.0') {
      p <- p + geom_linerange(linewidth = 1)
    } else {
      p <- p + geom_linerange(size = 1)
    }
    p +
      geom_pointrange(shape = 20, size = 0.5,
                      show.legend = FALSE) +
      geom_errorbar(aes(ymin = int - int.u, ymax = int + int.u),
                    width = 0.01, 
                    color = "red",
                    na.rm = TRUE,
                    linetype = 5) +
      geom_errorbarh(aes(xmin = mz - mz.u, xmax = mz + mz.u),
                     height = 0.01,
                     color = "red", 
                     na.rm = TRUE,
                     linetype = 5) +
      ggrepel::geom_text_repel(aes(label = formula),
                               # color = "black",
                               show.legend = FALSE,
                               hjust = 0,
                               vjust = 0) +
      # geom_text(aes(label = formula),
      #           color = "black",
      #           show.legend = FALSE,
      #           hjust = 0,
      #           vjust = 0) +
      # ggrepel::geom_text_repel(aes(label = round(mz, 4)), color = "black", show.legend = FALSE) +
      # geom_text(aes(label = round(mz, 4)), color = "black", show.legend = FALSE) +
      labs(title = glue::glue("Annotated Fragments"),
           subtitle = this_caption,
           alt = glue::glue("Annotated Fragments for {chosen_mzrt} plotted as an uncertainty mass spectrum with mass-to-charge on the x-axis and intensity on the y-axis. Line colors are black for unknowns, blue for fragments with known formula, and green for fragments with an annotated structure. Red error bars around each measurement point represent the uncertainty in both axes."),
           x = "m/z",
           colour = "Annotation Level",
           y = "Relative Intensity") +
      scale_color_manual(values = c("unknown" = "black",
                                    "formula" = "dodgerblue2",
                                    "structure" = "green4"),
                         aesthetics = c("colour")) +
      scale_y_continuous(expand = c(0, 0.01)) +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.title.position = "plot", 
            title = element_text(size = 15))
  })
  # __Spectral Data ----
  output$search_fragments_spectral_data <- renderDT(
    server = FALSE,
    DT::datatable(
      data = req(search_fragments_results()$spectra) %>%
        arrange(mz, norm_fragment_id) %>%
        relocate(norm_fragment_id, .after = everything()) %>%
        group_by(across(mz:match)) %>%
        summarise(norm_fragment_id = str_c(norm_fragment_id, collapse = ", ")) %>%
        select(match, mz, int, n, formula, mz.u, int.u, norm_fragment_id, has_smiles),
      rownames = FALSE,
      selection = list(mode = "none"),
      autoHideNavigation = TRUE,
      colnames = c("Match Type", "Mean m/z", "Mean Intensity", "n", "Annotated Formula", "Uncertainty (m/z)", "Uncertainty (intensity)", "Annotated Fragment ID", "Has SMILES Notation"),
      caption = NULL,
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = ifelse(nrow(search_fragments_results()$spectra) <= 15, "tB", "tBp"),
        pageLength = 15,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(className = "dt-left", targets = c("match", "formula")),
          list(className = "dt-center", targets = c("mz", "int", "mz.u", "int.u", "n", "norm_fragment_id", "has_smiles"))
        )
      )
    ) %>%
      formatRound(columns = c("mz", "mz.u", "int", "int.u"), digits = 4, interval = 3)
  )
  # __Annotations Table ----
  output$search_fragments_dt <- renderDT(
    server = FALSE,
    DT::datatable(
      data = req(search_fragments_results()$result) %>%
        mutate(
          radical = replace_na(recode(radical, `0` = "No", `1` = "Yes", .default = "Not Recorded"), "Not Recorded"),
          netcharge = replace_na(as.character(netcharge), "Not Recorded")
        ) %>%
        select(norm_fragment_id, formula, measured_mz, fixedmass, mass_error, smiles, radical, netcharge, n_compounds, n_peaks),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      autoHideNavigation = TRUE,
      colnames = c("Annotated Fragment ID", "Fragment", "Measured at m/z", "Exact Mass", "Mass Error (ppm)", "SMILES", "Radical", "Net Charge", "Found in n Compounds", "Found in n Peaks"),
      caption = NULL,
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = ifelse(nrow(search_fragments_results()$result) <= 10, "tB", "tBp"),
        pageLength = 10,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = c("norm_fragment_id")),
          list(className = "dt-center", targets = c("measured_mz", "fixedmass", "mass_error", "radical", "netcharge", "n_compounds", "n_peaks")),
          list(className = "dt-left", targets = c("formula", "smiles"))
        )
      )
    ) %>%
      formatRound(columns = c("measured_mz", "fixedmass", "mass_error"), digits = 4, interval = 3)
  )
  # __Compound list ----
  output$search_fragments_compound_list <- renderDT(
    server = FALSE,
    DT::datatable(
      data = req(search_fragments_compounds_data()) %>%
        select(-additional),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      autoHideNavigation = TRUE,
      # colnames = c("Compound ID", "Name", "Category", "Formula", "Exact Mass", "Source Type", "Obtained From", "Additional", "Local (+)", "Local (-)", "Net Charge"),
      colnames = c("Compound ID", "Name", "Category", "Formula", "Exact Mass", "Source Type", "Obtained From", "Local (+)", "Local (-)", "Net Charge"),
      caption = NULL,
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = ifelse(nrow(search_fragments_compounds_data()) <= 10, "tB", "tBp"),
        pageLength = 10,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = c("id", "category")),
          list(className = "dt-center", targets = c("fixedmass", "source_type", "obtained_from", "local_positive", "local_negative", "netcharge")),
          list(className = "dt-left", targets = c("name", "formula"))
        )
      )
    )
  )
  # __Peak list ----
  output$search_fragments_peak_list <- renderDT(
    server = FALSE,
    DT::datatable(
      data = req(search_fragments_peaks_data()),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      colnames = c("Identification Confidence", "n Points", "Precursor m/z", "Ion State", "RT Centroid", "RT Start", "RT End", "Peak ID", "Sample ID"),
      caption = NULL,
      extensions = c("Responsive", "Buttons"),
      options = list(
        dom = ifelse(nrow(search_fragments_peaks_data()) <= 10, "tB", "tBp"),
        pageLength = 10,
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(className = "dt-center", targets = c("num_points", "precursor_mz", "ion_state", "rt_start", "rt_centroid", "rt_end")),
          list(className = "dt-left", targets = c("confidence"))
        )
      )
    )
  )
  # __Molecular model ----
  # __Molecular model candidate formula
  output$search_fragments_selected_formula <- renderText(
    paste0(tags$label(req(search_fragments_results_selected()$formula)))
  )
  # __Molecular model graphic
  output$search_fragments_ballstick <- renderImage({
    shiny::validate(
      need(rdkit_available,
           message = "[ This version of MSMatch requires RDKit integration to display molecular models, but it is currently unavailable. ]"),
      need(search_fragments_results_selected()$has_smiles,
           message = "This fragment has not had a structure assigned.")
    )
    fragment_id <- search_fragments_results_selected()
    log_it("trace", glue::glue("Finding or rendering molecular model (id = {fragment_id$annotated_fragment_id}) for display."), app_ns)
    alt_text <- glue::glue("Molecular model for fragment {fragment_id$formula} (ID {fragment_id$norm_fragment_id}) with SMILES notation {fragment_id$smiles}, as generated from RDKit.")
    list(
      src =  api_endpoint(path = "molecular_model/file",
                          type = "fragment",
                          fragment_id = fragment_id$norm_fragment_id),
      alt = alt_text
    )
  },
  deleteFile = FALSE
  )
  # __Molecular model caption
  output$search_fragments_ballstick_caption <- renderText({
    shiny::validate(
      need(search_fragments_results_selected(),
           message = "Please select a row from the fragments table.")
    )
    with(req(search_fragments_results_selected()), {
      structure_text <- ifelse(
        has_smiles,
        glue::glue("with structure {smiles}"),
        "but has not had a structure defined"
      )
      p("The fragment measured at m/z ",
        em(sprintf("%.4f", measured_mz)),
        "has been previously annotated as ",
        strong(formula),
        glue::glue("(fragment ID {norm_fragment_id}) {structure_text}."),
        glue::glue("It has been previously associated with {n_compounds} compounds."), 
        "The measurement error compared with the expected exact mass is ",
        em(sprintf("%s%.4f ppm", ifelse(mass_error > 0, "+", ""), mass_error)),
        "."
      ) %>%
        paste0()
    })
  })
  # _Observers ----
  # __Execute search ----
  observeEvent(input$search_fragments_search_btn, {
    log_it("trace", "Button 'search_fragments_search_btn' clicked.", app_ns)
    if (input$search_fragments_mzrt == "") {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please choose a Feature of Interest"
      )
    }
    req(
      search_compounds_mzrt(),
      search_fragments_mzrt(),
      user_data()
    )
    log_it("info", glue::glue("Fragment search started for {search_fragments_mzrt_text()[search_fragments_mzrt()]}."), app_ns)
    if (!is.null(search_compounds_results()) && search_compounds_mzrt() == search_fragments_mzrt()) {
      fragments <- search_compounds_results()$search_object$ums2
    } else {
      req(
        user_data(),
        data_input_search_parameters(),
        input$search_compounds_mzrt,
        input$search_compounds_search_type,
        input$data_input_relative_error,
        input$data_input_minimum_error,
        input$data_input_experiment_type,
        input$data_input_isolation_width,
        input$search_compounds_search_zoom,
        input$search_compounds_max_correl,
        input$search_compounds_correl_bin,
        input$search_compounds_ph,
        input$search_compounds_max_ph,
        input$search_compounds_ph_bin,
        input$search_compounds_max_freq,
        input$search_compounds_freq_bin,
        input$search_compounds_min_n_peaks
      )
      search_fragment_index <- req(search_fragments_mzrt())
      mass_error <- input$data_input_relative_error
      min_error <- input$data_input_minimum_error
      runjs("$('#search_fragments_overlay_text').text('Executing fragment search...');")
      log_it("trace", "Executing fragment search.", app_ns)
      showElement("search_fragments_overlay")
      runjs("$('#search_fragments_overlay_text').text('Validating inputs...');")
      log_it("trace", "Validating fragment search inputs.", app_ns)
      mzrt <- isolate(data_input_search_parameters()[input$search_fragments_mzrt, ])
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
      runjs("$('#search_fragments_overlay_text').text('Identifying fragment ions...');")
      log_it("trace", "Identifying fragment ions.", app_ns)
      search_object <- try(
        get_search_object(
          searchmzml = tmp,
          zoom = isolate(input$search_compounds_search_zoom)
        )
      )
      if (inherits(search_object, "try-error")) {
        msg <- sprintf("Could not find a feature in data file %s matching %s",
                       input$data_input_filename$name,
                       search_fragments_mzrt_text()[search_fragment_index])
        nist_shinyalert(
          title = NULL,
          type = "warning",
          text = msg
        )
        hideElement("search_fragments_overlay")
        log_it("warn", msg, app_ns)
        return(NULL)
      }
      search_object <- search_object %>%
        create_search_ms(
          searchobj = .,
          correl = isolate(input$search_compounds_correlation),
          ph = isolate(input$search_compounds_ph),
          freq = isolate(input$search_compounds_freq),
          normfn = isolate(input$search_compounds_norm_function),
          cormethod = isolate(input$search_compounds_correlation_method)
        )
      fragments <- search_object$ums2
    }
    runjs("$('#search_fragments_overlay_text').text('Matching with annotated fragments...');")
    log_it("trace", "Matching with annotated fragments.", app_ns)
    fragment_matches <- api_endpoint(
      path = "search_fragments",
      fragment_ions = toJSON(fragments$mz),
      mass_error = input$data_input_relative_error,
      min_error = input$data_input_minimum_error,
      return_format = "data.frame"
    )
    if (!"netcharge" %in% names(fragment_matches)) {
      fragment_matches <- fragment_matches %>%
        mutate(netcharge = NA_integer_)
    }
	if (!"radical" %in% names(fragment_matches)) {
      fragment_matches <- fragment_matches %>%
        mutate(radical = NA_integer_)
	}
	if (!"smiles" %in% names(fragment_matches)) {
      fragment_matches <- fragment_matches %>%
        mutate(smiles = NA_character_)
	}
    if (nrow(fragment_matches) == 0) {
      fragment_matches <- tibble(
        norm_fragment_id = integer(),
        compounds = list(),
        peak_ids = list(),
        annotated_fragments = list(),
        formula = character(),
        fixedmass = numeric(),
        radical = integer(),
        has_smiles = logical(),
        n_compounds = integer(),
        n_peaks = integer(),
        n_annotations = integer(),
        netcharge = integer(),
        smiles = character()
      )
    } else {
      fragment_matches <- select(
        fragment_matches,
        norm_fragment_id,
        compounds,
        peak_ids,
        annotated_fragments,
        formula,
        fixedmass,
        radical,
        has_smiles,
        n_compounds,
        n_peaks,
        n_annotations,
        netcharge,
        smiles
      )
    }
    fragment_matches <- fragment_matches %>%
      arrange(desc(has_smiles), fixedmass)
    log_it("trace", "Building fragment match output.", app_ns)
    fragment_links <- lapply(
      fragments$mz,
      function(x) {
        delta <- abs(x - fragment_matches$fixedmass)
        candidates <- fragment_matches %>%
          filter(delta < max(input$data_input_relative_error * 1e-6, input$data_input_minimum_error)) %>%
          mutate(mz = x) %>%
          select(mz, norm_fragment_id)
      }
    ) %>%
      bind_rows() %>%
      distinct()
    list(
      linked_data = fragment_matches %>%
        select(norm_fragment_id, compounds, peak_ids, annotated_fragments),
      result = fragment_matches %>%
        select(-compounds, -peak_ids) %>%
        left_join(fragment_links) %>%
        mutate(mass_error = (mz - fixedmass) / fixedmass * 1e6) %>%
        rename("measured_mz" = "mz") %>%
        arrange(fixedmass),
      spectra = fragments %>%
        left_join(fragment_links) %>%
        left_join(fragment_matches %>%
                    select(norm_fragment_id, formula, has_smiles)
        ) %>%
        left_join(eval(.) %>%
                    group_by(mz) %>%
                    summarise(filter_check = any(has_smiles)),
                  copy = TRUE) %>%
        filter(filter_check == has_smiles | is.na(norm_fragment_id)) %>%
        select(-filter_check) %>%
        mutate(
          match = case_when(
            !is.na(norm_fragment_id) & has_smiles ~ "structure",
            !is.na(norm_fragment_id) & !has_smiles ~ "formula",
            TRUE ~ "unknown"
          ),
          match = factor(match, levels = c("unknown", "formula", "structure"))
        ) %>%
        arrange(mz)
    ) %>%
      search_fragments_results()
    log_it("success", glue::glue("Fragment matching complete with n = {nrow(search_fragments_results()$result)} matches found."), app_ns)
    hideElement("search_fragments_overlay")
  })
  # __More compound information ----
  observeEvent(input$search_fragments_compound_info, {
    if (is.null(input$search_fragments_compound_list_rows_selected)) {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please select a compound from the list."
      )
      log_it("warn", "Compound info requested but no row selected in 'search_fragments_compound_list'.", app_ns)
    } else {
      compound_id <- req(search_fragments_compounds_data()$id[input$search_fragments_compound_list_rows_selected[1]])
      log_it("trace", glue::glue("Compound info requested for id = {compound_id}. Getting aliases."), app_ns)
      aliases <- api_endpoint(
        path = "table_search",
        table_name = "compound_url",
        match_criteria = list(id = compound_id),
        return_format = "data.frame"
      ) %>%
        mutate(
          ref_type = case_when(
            ref_type == "Acronym" ~ "User Supplied Acronym",
            ref_type == "Alias" ~ "Supplied Alias",
            ref_type == "CASRN" ~ "CAS Registry Number",
            ref_type == "DTXCID" ~ "EPA CompTox CID",
            ref_type == "DTXSID" ~ "EPA CompTox SID",
            ref_type == "Fixed InChI" ~ "Fixed InChI Notation",
            ref_type == "InChI" ~ "InChI Notation",
            ref_type == "InChIKey" ~ "InChIKey Notation",
            ref_type == "NIST Suspec List" ~ "NIST Suspect List ID",
            ref_type == "PUBCHEMID" ~ "PubChem CID",
            ref_type == "Preferred Name" ~ "Preferred Name",
            ref_type == "SMILES" ~ "SMILES Notation",
            ref_type == "ADDITIONAL" ~ "Additional Notations",
            TRUE ~ ref_type
          ),
          link = lapply(link,
                        function(x) {
                          src_text <- str_split(string = x, pattern = "http[s]*://|/")
                          paste0(
                            a(href = x,
                              target = "_blank",
                              icon("link"),
                              paste0("See more at ",
                                     src_text[[1]][[2]])
                            )
                          ) %>%
                            str_remove_all("\\\n") %>%
                            str_replace_all("See more at www.google.com", "Search Google for this alias.")
                        }) %>%
            unlist()
        )
      log_it("info", glue::glue("Displaying additional compound information for id = {compound_id}."), app_ns)
      nist_shinyalert(
        title = glue::glue("Additional Information for {unique(aliases$compound)}"),
        type = "info",
        size = "m",
        text = tagList(
          div(id = "additional_information",
              width = "100%",
              height = "auto",
              DT::datatable(
                data = aliases,
                width = "100%",
                selection = "single",
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Compound ID", "Name", "Reference", "Alias", "Link"),
                caption = NULL,
                extensions = c("Responsive"),
                options = list(
                  dom = ifelse(nrow(aliases) <= 10, "t", "tp"),
                  pageLength = 10,
                  buttons = c("copy", "csv", "excel"),
                  columnDefs = list(
                    list('max-width' = '500px', targets = 3),
                    list(visible = FALSE, targets = c("id", "compound")),
                    list(className = "dt-center", targets = c("_all"))
                  )
                )
              )
          )
        )
      )
    }
  })
  # __More fragment information ----
  observeEvent(input$search_fragments_fragment_info, {
    req(search_fragments_row_selected())
    fragment_id <- search_fragments_results_selected()$norm_fragment_id
    if (is.null(fragment_id)) {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please select a fragment from the list."
      )
      log_it("warn", "Fragment info requested but no row selected in 'search_fragments_fragment_list'.", app_ns)
    } else {
      log_it("info", glue::glue("Displaying additional fragment information for id = {compound_id}."), app_ns)
      nist_shinyalert(title = glue::glue("Fragment id = {fragment_id}"),
                      type = "info",
                      text = "Work in progress")
    }
  })
  # __More peak information ----
  observeEvent(input$search_fragments_peak_info, {
    if (is.null(input$search_fragments_peak_list_rows_selected[1])) {
      nist_shinyalert(
        title = "More information needed",
        type = "info",
        text = "Please select a peak from the list."
      )
      log_it("warn", "Peak info requested but no row selected in 'search_fragments_peak_list'.", app_ns)
    } else {
      peak_data <- req(search_fragments_peaks_data()[input$search_fragments_peak_list_rows_selected[1], ])
      method_narrative <- api_endpoint(
        path = "method_narrative",
        type = "peak",
        table_pk = peak_data$id
      )
      sample_narrative <- api_endpoint(path = sprintf("sample_narrative/%d", peak_data$sample_id))
      if (is.na(sample_narrative)) sample_narrative <- "[ Sample narrative unavailable. ]"
      # links <- api_endpoint(
      #   path = "table_search",
      #   table_name = "compound_fragments",
      #   match_criteria = list(peak_id = peak_data$id),
      #   return_format = "data.frame"
      # )
      # peak_ms <- api_endpoint(
      #   path = "table_search",
      #   table_name = "ms_data",
      #   match_criteria = list(peak_id = peak_data$id),
      #   return_format = "data.frame"
      # ) %>%
      #   tidy_spectra(is_file = FALSE)
      nist_shinyalert(
        title = NULL,
        type = "info",
        text = tagList(
          # TODO Some plot here to expose peak data? Endpoint "/plot_peak"?
          h4(sprintf("Narrative for Peak ID %d in Sample %d", peak_data$id, peak_data$sample_id)),
          hr(),
          p(style = "font-size: small;", method_narrative),
          hr(),
          p(style = "font-size: small;", sample_narrative),
          hr()
        )
      )
      log_it("info", glue::glue("Displaying additional peak information for id = {peak_data$id}."), app_ns)
    }
  })
})
