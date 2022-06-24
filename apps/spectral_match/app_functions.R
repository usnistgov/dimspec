valid_file_format <- function(filename, accepts, from_shiny = TRUE) {
  accepts <- sort(unique(accepts))
  valid <- tolower(tools::file_ext(filename)) %in% tolower(gsub("\\.", "", accepts))
  if (!valid && from_shiny) {
    shinyalert(
      title = "Unsupported file format",
      type = "error",
      showCancelButton = FALSE,
      showConfirmButton = TRUE,
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      immediate = TRUE,
      text = glue::glue("Please choose a file with a{ifelse(substr(accepts[1], 1, 1) %in% vowels, 'n', '')} {gsub(' and ', ' or ', format_list_of_names(accepts))} extension.")
    )
  }
  return(valid)
}

complete_form_entry <- function(input, values, from_shiny = TRUE) {
  provided_vals <- reactiveValuesToList(input)[values]
  valid <- !any(
    any(is.na(provided_vals)),
    any(provided_vals == ""),
    any(is.null(provided_vals))
  )
  if (!valid && from_shiny) {
    shinyalert(
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
#' @return None, fires a shinyalert modal
#' 
nist_shinyalert <- function(title, type, text, className = "nist_shinyalert", html = TRUE, closeOnClickOutside = TRUE, immediate = TRUE, ...) {
  kwargs <- as.list(environment())
  kwargs <- kwargs[names(kwargs) %in% names(formals(shinyalert::shinyalert))]
  text <- div(class = "nist_shinyalert_text", text)
  do.call(shinyalert, args = kwargs)
}
