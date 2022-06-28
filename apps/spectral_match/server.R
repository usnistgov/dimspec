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
    if (!dev) {
      NULL
    } else {
      readRDS("toy_data.RDS")
    }
  )
  data_input_search_upload <- reactiveVal(NULL)
  data_input_search_parameters <- reactiveVal(
    if (!dev) {
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
  search_compounds_results <- reactiveVal(
    # if (!dev) {
      tibble(
        compound_id = character(0),
        compound_name = character(0),
        ms1_dot_product = numeric(0),
        ms2_dot_product = numeric(0),
        num_fragments = numeric(0),
        source = character(0),
        confidence = character(0)
      )
    # } else {
    #   tibble(
    #     compound_id = 1:25,
    #     compound_name = sapply(1:25, function(x) paste0(sample(letters, 10), collapse = "")),
    #     ms1_dot_product = rnorm(25),
    #     ms2_dot_product = rnorm(25),
    #     num_fragments = sample(1:10, 25, TRUE),
    #     source = rep(paste0(sample(letters, 10), collapse = ""), 25),
    #     confidence = sample(1:5, 25, TRUE)
    #   )
    # }
  )
  search_compounds_results_selected <- reactive(
    search_compounds_results() %>%
      slice(req(input$search_compounds_dt_rows_selected))
  )
  search_fragments_results <- reactiveVal(
    # if (!dev) {
      tibble(
        fragment_id = integer(0),
        formula = character(0),
        smiles = character(0),
        fragment_mz = numeric(0),
        exactmass = numeric(0),
        mass_error = numeric(0),
        compounds = character(0)
      )
    # } else {
    #   tbl(con, "view_annotated_fragments") %>%
    #     select(id, formula, smiles, mz, fixedmass, ppm_error) %>%
    #     mutate(compounds = "") %>%
    #     collect() %>%
    #     slice(1:25) %>% 
    #     setNames(
    #       c("fragment_id", "formula", "smiles", "fragment_mz", "exactmass", "mass_error", "compounds")
    #     )
    # }
  )
  # selected_fragment <- reactiveVal(1)
  search_fragments_results_selected <- reactive(
    search_fragments_results() %>%
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
  hideElement(selector = "#search_compounds_overlay")
  hideElement(selector = "#uncertainty_overlay")
  hideElement(selector = "#search_fragments_overlay")
  
  # Element Display ----
  observe({
    toggleElement("data_input_additional", condition = advanced_use)
    if (!dev) {
      toggleElement("data_input_dt_peak_list_edit_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
      toggleElement("data_input_dt_peak_list_remove_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
      toggleElement("data_input_process_btn", condition = (!is.null(user_data()) && nrow(data_input_search_parameters()) > 0))
      toggleElement("data_input_dt_peak_list", condition = nrow(data_input_search_parameters()) > 0)
      toggleElement("search_compounds_results_span", condition = nrow(search_compounds_results()) > 0)
      toggleElement("search_fragments_results_span", condition = nrow(search_fragments_results()) > 0)
    }
  })
  
  # Navigation ----
  observeEvent(input$sidebar_menu, {
    if (!dev) {
      if (!input$sidebar_menu %in% c("data_input", "index", "about") && any(is.null(user_data()), nrow(data_input_search_parameters()) == 0)) {
        shinyalert(title = "Insufficient data",
                   type = "info",
                   showElementCancelButton = FALSE,
                   showElementConfirmButton = TRUE,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   immediate = TRUE,
                   text = "Please load a data file and select search parameters first.")
        updateTabsetPanel(session = session,
                          inputId = "sidebar_menu",
                          selected = "data_input")
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
      shinyalert(
        title = "Duplicate values detected",
        type = "info",
        showElementCancelButton = FALSE,
        showElementConfirmButton = TRUE,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        immediate = FALSE,
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
          text = glue::glue("Typically, the value of Isolation Width should be less than 4 Da unless the experiment type is SWATH.")
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
    showElement("data_input_process_btn")
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
  output$search_compounds_dt <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      req(search_compounds_results()),
      rownames = FALSE,
      selection = list(mode = "single",
                       target = "row",
                       selected = 1),
      autoHideNavigation = TRUE,
      colnames = c("Compound Name/ID", "MS1 Score", "MS2 Score", "# Annotated Fragments", "Source", "Source Confidence"),
      caption = "Select a row to view the match or send it to uncertainty estimation.",
      options = list(
        dom = "tp",
        pageLength = 15,
        extensions = c("Responsive", "Buttons"),
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(className = "dt-center", targets = 1:3),
          list(className = "dt-left", targets = c(0, 4, 5))
        )
      )
    )
  )
  # output$search_compounds_butterfly_plot <- renderPlotly(
  #   req(search_compounds_results_selected()) %>%
  #     slice(search_row) %>%
  #     plot_compare_ms() %>%
  #     ggplotly()
  # )
  # _Observers ----
  # __Navigation
  observeEvent(input$search_compounds_go_data_input, {
    updateTabsetPanel(
      session = session,
      inputId = "sidebar_menu",
      selected = "data_input"
    )
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
      input$data_input_correl_bin,
      input$data_input_ph,
      input$data_input_ph_bin,
      input$data_input_max_freq,
      input$data_input_freq_bin,
      input$data_input_min_n_peaks
    )
    type <- isolate(input$search_compounds_search_type)
    mzrt <- isolate(data_input_search_parameters()[input$search_compounds_mzrt, ])
    tmp <- isolate(user_data())
    tmp$search_df <- tibble(
      filename = tmp$search_df$filename,
      precursormz = mzrt$precursor,
      masserror = isolate(input$data_input_relative_error),
      minerror = isolate(input$data_input_minimum_error),
      rt = mzrt$rt,
      rt_start = mzrt$rt_start,
      rt_end = mzrt$rt_end,
      ms2exp = isolate(input$data_input_experiment_type),
      isowidth = isolate(input$data_input_isolation_width)
    )
    browser()
    search_object <- get_search_object(
      searchmzml = tmp,
      zoom = isolate(input$data_input_search_zoom)
    )
    api_endpoint(path            = "search_compounds",
                 type            = type,
                 search_ms       = jsonlite::tojson(user_data()),
                 correlation_max = isolate(input$data_input_max_correl),
                 correlation_bin = isolate(input$data_input_correl_bin),
                 peak_height_max = isolate(input$data_input_ph),
                 peak_height_bin = isolate(input$data_input_ph_bin),
                 frequency_max   = isolate(input$data_input_max_freq),
                 frequency_bin   = isolate(input$data_input_freq_bin),
                 min_n_peaks     = isolate(input$data_input_min_n_peaks),
                 return_format   = "data.frame") %>%
      search_compounds_results()
  })
  observeEvent(input$search_compounds_uncertainty_btn, ignoreNULL = TRUE, ignoreInit = TRUE, {
    # search_compounds_results_selected() %>%
    #   bootstrap_compare_ms()
    updateTabsetPanel(session = session,
                      inputId = "sidebar_menu",
                      selected = "uncertainty")
  })

  # UNCERTAINTY PAGE ----
  # _Reactives ----
  # output$uncertainty_butterfly_plot <- renderPlotly(
  #   search_compounds_results_selected() %>%
  #     plot_compare_ms() %>%
  #     ggplotly()
  # )
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
        dom = "tp",
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
