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
  
  # Element Display ----
  observe({
    toggleElement("data_input_dt_peak_list_edit_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_dt_peak_list_remove_row", condition = !is.null(input$data_input_dt_peak_list_rows_selected))
    toggleElement("data_input_import", condition = !is.null(user_data()) && nrow(data_input_search_parameters()) > 0)
    toggleElement("data_input_dt_peak_list", condition = nrow(data_input_search_parameters()) > 0)
    toggleElement("search_compounds_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
    toggleElement("uncertainty_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
    toggleElement("search_fragments_overlay", condition = is.null(user_data()) || nrow(data_input_search_parameters()) > 0)
  })
  
  # Data input reactives ----
  output$data_input_dt_peak_list <- renderDT(
    server = FALSE,
    expr = DT::datatable(
      data_input_search_parameters(),
      rownames = FALSE,
      selection = "single",
      editable = TRUE,
      autoHideNavigation = TRUE,
      colnames = c("Precursor m/z", "RT", "RT Start", "RT End"),
      caption = "Data will be searched for peaks matching these characteristics. Select a row to edit or remove it.",
      options = list(
        pageLength = 15,
        extensions = c("Responsive"),
        columnDefs = list(list(className = "dt-center", targets = 0:3))
      )
    )
  )
  # Data Input observers ----
  observeEvent(data_input_search_parameters(), {
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
  })
  # _Data file upload ----
  observeEvent(input$data_input_filename, {
    req(input$data_input_filename)
    fn <- input$data_input_filename
    if (!valid_file_format(fn$name, c("raw", "mzML"))) {
      reset("data_input_filename")
    } else {
      # TODO parse uploaded file as either an mzML or a raw file
      parse_data(fn$datapath) %>%
        user_data()
    }
  })
  # _Import parameters ----
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
      removeModal()
      reset("data_input_import_search")
    }
  })
  # _Manually adjust parameters ----
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
    tmp %>%
      bind_rows(values) %>%
      data_input_search_parameters()
    removeModal()
  })
})
