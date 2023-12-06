#' Ensure files uploaded to a shiny app are of the required file type
#'
#' This input validation check uses [tools::file_ext] to ensure that files
#' uploaded to [shiny::fileInput] are among the acceptable file formats. Users
#' may sometimes wish to load a file outside the "accepts" format list by
#' manually changing it during the upload process. If they are not, a
#' [nist_shinyalert] modal is displayed prompting the user to upload a file in
#' one of the requested formats.
#'
#' @param filename CHR scalar name of the file uploaded to the shiny server
#' @param accepts CHR vector of acceptable file formats
#' @param show_alert LGL scalar indicating whether or not to show an alert, set
#'   FALSE to return the status of the check
#'
#' @return Whether or not all required values are present.
#'
#' @usage req(valid_file_format(input$file_upload, c(".csv", ".xls")))
#' 
valid_file_format <- function(filename, accepts, show_alert = TRUE) {
  accepts <- sort(unique(accepts))
  valid <- tolower(tools::file_ext(filename)) %in% tolower(gsub("\\.", "", accepts))
  if (!valid && show_alert) {
    nist_shinyalert(
      title = "Unsupported file format",
      type = "error",
      text = glue::glue("Please choose a file with a{ifelse(substr(accepts[1], 1, 1) %in% vowels, 'n', '')} {gsub(' and ', ' or ', format_list_of_names(accepts))} extension.")
    )
  }
  return(valid)
}

#' Ensure complete form entry
#'
#' This input validation check ensures the current session's input object
#' includes non-NA, non-NULL, and non-blank values similarly to [shiny::req] and
#' [shiny::validate] but can be called with a predefined list of input names to
#' check. Typically this is used for validate form entry completion. Call this
#' function prior to reading form entries to ensure that all values requested by
#' name in in `values` are present. If they are not, a [nist_shinyalert] modal
#' is displayed prompting the user to complete the form.
#'
#' @param input The session input object
#' @param values CHR vector of input object names to require
#' @param show_alert LGL scalar indicating whether or not to show an alert, set
#'   FALSE to return the status of the check
#'
#' @return Whether or not all required values are present.
#'
#' @usage req(complete_form_entry(input, c("need1", "need2")))
#' 
complete_form_entry <- function(input, values, show_alert = TRUE) {
  provided_vals <- reactiveValuesToList(input)[values]
  valid <- !any(
    any(is.na(provided_vals)),
    any(provided_vals == ""),
    any(is.null(provided_vals))
  )
  if (!valid && show_alert) {
    nist_shinyalert(
      title = "Input error",
      type = "error",
      text = "Please provide values for all choices on this form."
    )
  }
  return(valid)
}

#' Call [shinyalert::shinyalert] with specific styling
#'
#' This pass through function serves only to call [shinyalert::shinyalert] with
#' parameters defined by this function, and can be used for additional styling
#' that may be necessary. It is used solely for consistency sake.
#'
#' @inheritParams shinyalert::shinyalert
#' @seealso shinyalert::shinyalert
#'
#' @param ... Additional named parameters to be passed to shinyalert.
#'   Unrecognized ones will be ignored.
#'
#' @return None, shows a shinyalert modal
#'
#' @usage nist_shinyalert("test", "info", shiny::h3("test"))
#' 
nist_shinyalert <- function(title, type, text, className = "nist_shinyalert", html = TRUE, closeOnClickOutside = TRUE, immediate = TRUE, ...) {
  kwargs <- append(as.list(environment()), list(...))
  kwargs <- kwargs[names(kwargs) %in% names(formals(shinyalert::shinyalert))]
  kwargs$text <- tagList(div(class = "nist_shinyalert_text", text))
  do.call(shinyalert, args = kwargs)
}
