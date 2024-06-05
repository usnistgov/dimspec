#' Format a file name as an HTML element ID
#'
#' This is often useful to provide feedback to the user about the files they've
#' provided to a shiny application in a more informative manner, as IDs produced
#' here are suitable to build dynamic UI around. This can serve as the base ID
#' for tooltips, additional information, icons, etc. and produce everything
#' necessary in one place for any number of files.
#'
#' @param filename CHR vector of file names
#'
#' @return CHR vector of the same size as filename
#' @export
#'
#' @examples
#' format_html_id(list.files())
#' 
format_html_id <- function(filename) {
  text <- tools::file_path_sans_ext(filename) %>%
    str_squish() %>%
    str_trim() %>%
    str_remove_all("\\(|\\)") %>%
    str_replace_all(" ", "_")
  return(text)
}

#' Create the JS to append an icon to an HTML element by its ID
#'
#' @param id CHR scalar of the HTML ID to which to append an icon
#' @param icon_name CHR scalar of the icon name, which must be understandable by
#'   the `shiny::icon` function; e.g. a font-awesome icon name.
#' @param icon_class CHR vector of classes to apply
#'
#' @return CHR scalar suitable to execute with `shinyjs::runJS`
#' @export
#'
#' @examples
#' append_icon_to("example", "r-project", "fa-3x")
#' 
append_icon_to <- function(id, icon_name, icon_class = NULL) {
  if (!is.null(icon_class)) icon_class <- paste(icon_class, collapse = " ")
  this_icon <- icon(name  = icon_name,
                    class = icon_class)
  glue('$("#{id}").append(\'{this_icon}\');')
}

#' Attach a superscript icon with a bsTooltip to an HTML element
#'
#' @param id CHR scalar of the HTML ID to which to append the icon
#' @param tooltip CHR scalar of the tooltip text
#' @param icon_name CHR scalar of the icon name, which must be understandable by
#'   the `shiny::icon` function; e.g. a font-awesome icon name (default:
#'   "question").
#' @param size CHR scalar of the general icon size as understandable by the
#'   font-awesome library (default: "xs")
#' @param icon_class CHR vector of classes to apply to the `<sup>` container,
#'   as defined in the current CSS (default: "info-tooltip primary")
#' @param ... Other named arguments to be passed to `shinyBS:bsTooltip`
#'
#' @return LIST of HTML tags for the desired help icon and its tooltip
#' @export
#'
#' @examples
#' add_help("example", "a tooltip")
#' 
#' @note The following CSS is typically defined to go with this.
#'  .info-tooltip {
#'    opacity: 30%;
#'    transition: opacity .25s;
#'  }
#'  .info-tooltip:hover {
#'    opacity: 100%;
#'  }
#'  .primary {
#'    color: #3c8dbc;
#'  }
#'  
add_help <- function(id, tooltip, icon_name = "question", size = "xs", icon_class = "info-tooltip primary", ...) {
  require(shinyBS)
  id <- paste0(id, "_tt-target")
  help_icon <- tags$sup(icon(icon_name, class = sprintf("fa-%s", size)),
                        class = icon_class,
                        id    = id)
  dots <- list(...)
  bs_kwargs <- dots[names(dots) %in% names(formals(bsTooltip))]
  option_kwargs <- dots[!names(dots) %in% names(formals(bsTooltip))]
  args <- c(
    list(id = id, title = tooltip),
    bs_kwargs,
    options = list(option_kwargs)
  )
  bsTT <- do.call("bsTooltip", args)
  return(tagList(help_icon, bsTT))
}

#' Convenience application of \code{add_help} using pipes directly in \code{UI.R}
#'
#' This may not work for certain widgets with heavily nested HTML. Note that
#' classes may be CSS dependent.

#' @note Most standard Shiny widgets are supported, but maybe not all.
#'
#' @param widget shiny.tag widget
#' @param tooltip CHR scalar of the tooltip text
#' @param ... Other named arguments to be passed to `add_help`
#'
#' @return The \code{widget} provided with a hover tooltip icon appended to it.
#' @export
#'
#' @usage
#' actionButton("example", "With Help") %>%
#'   with_help("Now with a question mark icon hosting a tooltip")
#' actionButton("example", "With Help") %>%
#'   with_help("Large and green", size = "xl", class = "success")
#'   
with_help <- function(widget, tooltip, ...) {
  id <- unlist(widget)[[grep("id", names(unlist(widget)))[1]]]
  helper <- add_help(id, tooltip, ...)
  if (!"children" %in% names(widget$children[[1]])) {
    widget <- span(id = paste0(id, "_span"), widget)
  }
  tmp <- widget$children[[1]]$children
  widget$children[[1]]$children <- c(widget$children[[1]]$children, helper)
  return(widget)
}

#' Remove the last icon attached to an HTML element
#'
#' @param id CHR scalar of the HTML ID from which to remove the last icon
#'
#' @return CHR scalar suitable to execute with `shinyjs::runJS`
#' @export
#'
#' @examples
#' append_icon_to("example", "r-project", "fa-3x")
#' remove_icon_from("example")
remove_icon_from <- function(id) {
  glue('$("#{id}").children().last().remove();')
}

#' Apply colors to DT objects by value in a column
#'
#' Adds a class to each node meeting the criteria defined elsewhere as project
#' object `table_bg_classes` as a list of colors with names matches values.
#'
#' @param table_names CHR vector of the names going into a table
#' @param look_for CHR vector of the column name to color by
#'
#' @return JS function to apply to a DT object by row
#' @export
#'
#' @usage dt_color_by(names(DT_table_data), "color_by")
dt_color_by <- function(table_names, look_for) {
  color_by <- which(tolower(table_names) == tolower(look_for)) - 1
  out <- JS(
    "function(row, data) {",
    glue::glue("var classes = {toJSON(table_bg_classes)};"),
    "$node = this.api().row(row).nodes().to$();",
    glue::glue("var class_to_add = classes[data[{color_by}]][0];"),
    "$node.addClass(class_to_add);",
    "}"
  )
  return(out)
}

#' Easily format multiple DT objects in a shiny project in the same manner
#'
#' This serves solely to reduce the amount of options fed into `DT::datatable`
#' by providing common defaults and transparent options. Parameters largely do
#' exactly what they say and will create a list `column_defs` suitable for use
#' as `datatable(... options = list(columnDefs = column_defs)`. Leave NULL to
#' ignore any aspect.
#'
#' @param dataframe data.frame to be converted to a DT::datatable object
#' @param hide_cols CHR vector of column names to hide
#' @param center_cols CHR vector of column names to center
#' @param narrow_cols CHR vector of column names to make `narrow_col_width` wide
#' @param narrow_col_width CHR scalar defining column width (default: "5\%")
#' @param medium_cols CHR vector of column names to make `medium_col_width` wide
#' @param medium_col_width CHR scalar defining column width (default: "10\%")
#' @param large_cols CHR vector of column names to make `large_col_width` wide
#' @param large_col_width CHR scalar defining column width (default: "15\%")
#' @param truncate_cols CHR vector of column names to truncate
#' @param truncate_width INT scalar of the position at which to truncate
#' @param date_cols CHR vector of column names identifying dates
#' @param date_col_width CHR scalar defining column width (default: "10\%")
#' @param selection_mode CHR scalar of the DT selection mode (default: "single")
#' @param callback JS custom callback to apply to the datatable widget
#' @param color_by_column CHR scalar of the column name by which to color rows
#' @param names_to CHR scalar of the name formatting modification to apply, as
#'   one of the options available in the `stringr` package (default: "title" to
#'   apply `stringr::str_to_title`)
#' @param filter_at CHR scalar of the position for the column filter as
#'   understood by `DT::datatable(..., filter = filter_at)`. (default: "top")
#' @param chr_to_factor BOOL scalar for whether or not to automatically convert
#'   character columns to factor columns (default: TRUE)
#' @param ... other named arguments to be passed to `DT::datatable`
#'
#' @return DT::datatable object formatted as requested
#' @export
#'
#' @note Truncation applies a JS function to retain the underlying information
#'   as a hover tooltip and truncates using ellipses.
#' @note Column name formatting relies on being able to parse `names_to` as a
#'   valid function of the form `sprintf("str_to_%s", names_to)`; currently
#'   recognized options include "lower", "upper", "title", and "sentence".
#' @note To apply a custom format, define these parameters as a list (e.g.
#'   "dt_format_options") and pass it, along with your dataframe, as
#'   do.call("dt_formatted", c(dataframe = df, dt_format_options))
dt_formatted <- function(dataframe,
                         show_rownames    = FALSE,
                         hide_cols        = NULL,
                         center_cols      = NULL,
                         narrow_cols      = NULL,
                         narrow_col_width = "5%",
                         medium_cols      = NULL,
                         medium_col_width = "10%",
                         large_cols       = NULL,
                         large_col_width  = "15%",
                         truncate_cols    = NULL,
                         truncate_width   = 20,
                         date_cols        = NULL,
                         date_col_width   = "10%",
                         selection_mode   = "single",
                         callback         = NULL,
                         color_by_column  = NULL,
                         names_to         = "title",
                         filter_at        = "top",
                         chr_to_factor    = TRUE,
                         page_length      = 10,
                         page_length_menu = c(10, 25, 50),
                         ...) {
  require(DT)
  require(stringr)
  require(jsonlite)
  if (is.null(callback)) {
    if (!is.null(color_by_column)) {
      callback <- dt_color_by(names(dataframe), color_by_column)
    }
  }
  col_defs      <- list()
  date_cols     <- which(names(dataframe) %in% date_cols) - 1
  if (length(date_cols) > 0) col_defs <- c(col_defs, 
                                           list(
                                             list(width     = date_col_width,
                                                  targets   = date_cols))
  )
  hide_cols     <- which(names(dataframe) %in% hide_cols) - 1
  if (length(hide_cols) > 0) col_defs <- c(col_defs,
                                           list(
                                             list(visible   = FALSE,
                                                  targets   = hide_cols))
  )
  center_cols   <- which(names(dataframe) %in% center_cols) - 1
  if (length(center_cols) > 0) col_defs <- c(col_defs,
                                             list(
                                               list(className = 'dt-center',
                                                    targets   = center_cols))
  )
  narrow_cols   <- which(names(dataframe) %in% narrow_cols) - 1
  if (length(narrow_cols) > 0) col_defs <- c(col_defs,
                                             list(
                                               list(width     = narrow_col_width,
                                                    targets   = narrow_cols))
  )
  medium_cols   <- which(names(dataframe) %in% medium_cols) - 1
  if (length(medium_cols) > 0) col_defs <- c(col_defs,
                                             list(
                                               list(width     = medium_col_width,
                                                    targets   = medium_cols))
  )
  large_cols    <- which(names(dataframe) %in% large_cols) - 1
  if (length(large_cols) > 0) col_defs <- c(col_defs,
                                            list(
                                              list(width     = large_col_width,
                                                   targets   = large_cols))
  )
  truncate_cols <- which(names(dataframe) %in% truncate_cols) - 1
  if (length(truncate_cols) > 0) {
    truncate_js   <- sprintf(
      "function(data, type, row, meta) {
    return type === 'display' && data.length > %s ?
    '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;}",
      truncate_width,
      truncate_width
    )
    col_defs <- c(col_defs,
                  list(
                    list(targets   = truncate_cols,
                         render    = JS(truncate_js)))
    )
  }
  if (names_to %in% c("lower", "upper", "title", "sentence")) {
    col_names <- names(dataframe) %>%
      str_replace_all("_", " ")
    col_names <- do.call(sprintf("str_to_%s", names_to), list(col_names))
  }
  if (chr_to_factor) {
    dataframe <- dataframe %>%
      mutate(across(where(is.character), as.factor))
  }
  DT::datatable(dataframe,
                selection = selection_mode,
                rownames  = show_rownames,
                filter    = filter_at,
                colnames  = col_names,
                options   = list(
                  autoWidth  = TRUE,
                  columnDefs = col_defs,
                  pageLength  = page_length,
                  lengthMenu  = page_length_menu,
                  rowCallback = callback
                ),
                ...
  )
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
  text <- div(class = "nist_shinyalert_text", text)
  if (packageVersion("shinyalert") < "3.1") text <- paste0(text)
  kwargs <- c(as.list(environment()), list(...))
  kwargs <- kwargs[names(kwargs) %in% names(formals(shinyalert::shinyalert))]
  do.call(shinyalert, args = kwargs)
}

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
