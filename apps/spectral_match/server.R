shinyServer(function(input, output, session) {
  # Live Inspect ----
  observeEvent(input$browser, browser())
  
  # Session Data ----
  user_data <- reactive(NULL)
  data_input_search_upload <- reactiveVal(NULL)
  data_input_search_parameters <- reactiveVal(
    tibble(
      precursor = numeric(0),
      rt = numeric(0),
      rt_start = numeric(0),
      rt_end = numeric(0)
    )
  )
  search_compounds_results <- reactiveVal(
    tibble(
      compound_id = character(0),
      compound_name = character(0),
      ms1_dot_product = numeric(0),
      ms2_dot_product = numeric(0),
      num_fragments = numeric(0),
      source = character(0),
      confidence = character(0)
    )
  )
  selected_compound <- reactiveVal(1)
  search_compounds_results_selected <- reactive(
    search_compounds_results() %>%
      slice(selected_compound())
  )
  search_fragments_results <- reactiveVal(
    tibble(
      fragment_id = integer(0),
      formula = character(0),
      fragment_mz = numeric(0),
      exactmass = numeric(0),
      mass_error = numeric(0),
      compounds = character(0)
    )
  )
  selected_fragment <- reactiveVal(1)
  search_fragments_results_selected <- reactive(
    search_compounds_results() %>%
      slice(selected_fragment())
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
  
  # Element Display ----
  observe({
    toggleElement("data_input_dt_peak_list_edit_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_dt_peak_list_remove_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_import", condition = !is.null(user_data()) && nrow(data_input_search_parameters()) > 0)
    toggleElement("data_input_dt_peak_list", condition = nrow(data_input_search_parameters()) > 0)
    # toggleElement("search_compounds_results_span", condition = nrow(search_compounds_results()) > 0)
    # toggleElement("search_fragments_results_span", condition = nrow(search_fragments_results()) > 0)
    # toggleElement("search_compounds_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
    # toggleElement("uncertainty_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
    # toggleElement("search_fragments_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
  })
  
  # Navigation ----
  observeEvent(input$sidebar_menu, {
    if (!dev) {
      if (!input$sidebar_menu == "data_input" && any(is.null(user_data()), nrow(data_input_search_parameters()) == 0)) {
        shinyalert(title = "Insufficient data",
                   type = "info",
                   showCancelButton = FALSE,
                   showConfirmButton = TRUE,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   immediate = TRUE,
                   text = "Please load a data file and select search parameters first.")
        updateTabsetPanel(inputId = "sidebar_menu",
                          selected = "data_input")
      }
    }
  })
  
  # DATA INPUT PAGE ----
  # _Reactives ----
  output$data_input_dt_peak_list <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data_input_search_parameters() %>%
        select(precursor:rt_end),
      rownames = FALSE,
      selection = "single",
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
  observeEvent(data_input_search_parameters(), {
    req(nrow(data_input_search_parameters()) > 0)
    n_orig <- nrow(data_input_search_parameters())
    n_uniq <- nrow(distinct(data_input_search_parameters()))
    if (!n_orig == n_uniq) {
      shinyalert(
        title = "Duplicate values detected",
        type = "info",
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
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
    updateSelectizeInput(
      inputId = "search_compounds_mzrt",
      choices = data_input_search_parameters() %>%
        mutate(label = glue::glue("{precursor} m/z @ {rt} ({rt_start} - {rt_end})")) %>%
        pull(label) %>%
        setNames(object = 1:length(.),
                 nm = .)
    )
    updateSelectizeInput(
      inputId = "search_fragments_mzrt",
      choices = data_input_search_parameters() %>%
        mutate(label = glue::glue("{precursor} m/z @ {rt} ({rt_start} - {rt_end})")) %>%
        pull(label) %>%
        setNames(object = 1:length(.),
                 nm = .)
    )
  })
  # __Data file upload ----
  observeEvent(input$data_input_filename, {
    req(input$data_input_filename)
    fn <- input$data_input_filename
    if (!valid_file_format(fn$name, c("raw", "mzML"))) {
      reset("data_input_filename")
    } else {
      # TODO parse uploaded file as either an mzML or a raw file
      # parse_data(fn$datapath) %>%
      #   user_data()
    }
  })
  # __Import parameters ----
  observeEvent(input$data_input_import_search, {
    req(input$data_input_import_search)
    fn <- input$data_input_import_search
    if (!valid_file_format(fn$name, c("csv", "xls", "xlsx"))) {
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
      shinyalert(
        title = "Duplicate columns detected",
        type = "error",
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        immediate = TRUE,
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
        shinyalert(
          title = "Unreasonable retention times",
          size = "m",
          type = "error",
          showCancelButton = FALSE,
          showConfirmButton = TRUE,
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          immediate = TRUE,
          html = TRUE,
          text = glue::glue(
            "<p style='font-weight: bold;'>Please address the following issues in the source file '{input$data_input_import_search$name}' and try again.</p><br>{paste0(bad_entries$message, collapse = '')}"
          )
        )
      } else {
        which_complete <- complete.cases(upload_parameters)
        if (any(!which_complete)) {
          shinyalert(
            title = "Incomplete data",
            type = "warning",
            showCancelButton = FALSE,
            showConfirmButton = TRUE,
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            immediate = TRUE,
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
      shinyalert(title = "Data Entry Validation",
                 size = "s",
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 immediate = TRUE,
                 type = "warning",
                 text = "Retention Time (Centroid) should be after Retention Time (Start).")
      valid <- FALSE
    }
    if (!input$mod_search_parameter_rt <= input$mod_search_parameter_rt_end) {
      shinyalert(title = "Data Entry Validation",
                 size = "s",
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 immediate = TRUE,
                 type = "warning",
                 text = "Retention Time (Centroid) should be before Retention Time (End).")
      valid <- FALSE
    }
    if (!input$mod_search_parameter_rt_start <= input$mod_search_parameter_rt_end) {
      shinyalert(title = "Data Entry Validation",
                 size = "s",
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 immediate = TRUE,
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
  
  # COMPOUND SEARCH PAGE ----
  # _Reactives ----
  output$search_compounds_dt <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      search_compounds_results(),
      rownames = FALSE,
      selection = "single",
      autoHideNavigation = TRUE,
      colnames = c("Compound Name/ID", "MS1 Score", "MS2 Score", "# Annotated Fragments", "Source", "Source Confidence"),
      caption = "Select a row to view the match or send it to uncertainty estimation.",
      options = list(
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
  #   search_compounds_results_selected() %>%
  #     slice(search_row) %>%
  #     plot_compare_ms() %>%
  #     ggplotly()
  # )
  # _Observers ----
  observeEvent(input$compounds_search_btn, {
    type <- req(input$search_compounds_search_type)
    mzrt <- req(input$search_compounds_mzrt)
    search <- switch(
      type,
      "1" = search_precursor,
      "2" = search_all
    )
    # out <- search(data)
    # search_compounds_results(out)
  })
  observeEvent(input$search_compounds_dt_rows_selected, {
    if (!is.null(input$search_compounds_dt_rows_selected)) {
      selected_compound(input$search_compounds_dt_rows_selected)
    }
  })
  observeEvent(input$search_compounds_uncertainty_btn, {
    # search_compounds_results_selected() %>%
    #   bootstrap_compare_ms()
    updateTabsetPanel(inputId = "sidebar_menu",
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
      search_fragments_results(),
      rownames = FALSE,
      selection = "single",
      autoHideNavigation = TRUE,
      colnames = c("Elemental Formula", "Fragment m/z", "Exact Mass", "Mass Error (ppm)", "Compounds"),
      caption = "Select a row to view the matching fragment.",
      options = list(
        pageLength = 15,
        extensions = c("Responsive", "Buttons"),
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(className = "dt-center", targets = c(2, 3, 4)),
          list(className = "dt-left", targets = c(1, 5))
        )
      )
    )
  )
  # output$search_fragments_spectral_plot <- renderPlotly(
  #   search_fragments_results_selected() %>%
  #     plot_ms()
  # )
  # output$search_fragment_ballstick <- renderImage(
  #   search_fragments_results_selected() %>%
  #     select()
  # )
  # _Observers ----
  observeEvent(input$fragments_search_btn, {
    type <- req(input$search_fragments_search_type)
    mzrt <- req(input$search_fragments_mzrt)
    search <- switch(
      type,
      "1" = search_precursor,
      "2" = search_all
    )
    # out <- search(data)
    # search_compounds_results(out)
  })
  observeEvent(input$search_fragments_dt_rows_selected, {
    if (!is.null(input$search_fragments_dt_rows_selected)) {
      selected_fragment(input$search_fragments_dt_rows_selected)
    }
  })
})
