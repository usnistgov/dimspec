mod_download_all <- function() {
  modalDialog(
    title = "Match all Compounds and Fragments and Download",
    size = "l",
    easyClose = FALSE,
    div(class = "flex-container",
        # TODO include selection options on what to download
        downloadButton(outputId = "mod_download_all_save",
                       label = "Execute and Download") %>%
          with_help(tooltip = tooltip_text[["mod_download_all_save"]]),
        actionButton(inputId = "mod_download_all_cancel",
                     label = "Cancel",
                     icon = icon("close", verify_fa = FALSE)) %>%
          with_help(tooltip = tooltip_text[["mod_download_all_cancel"]])
    ),
    footer = NULL
  )
}