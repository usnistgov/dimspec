#' Get the current NIST PFAS suspect list.
#'
#' Downloads the current NIST suspect list of PFAS from the NIST Public Data
#' Repository to the current project directory.
#'
#' @param destfile file.path of location to save the downloaded file
#'
#' @return none
#' @export
#'
#' @examples
#' get_suspectlist()
get_suspectlist <- function(destfile = file.path("src", "compoundlist", "suspectlist.xlsx")) {
  url_file <- file.path('config', 'suspectlist_url.txt')
  if (!exists(url_file)) stop('Could not locate the "suspectlist_url.txt" file in the expected location.')
  url <- readLines(url_file, warn = FALSE) #this file must be configured
  download.file(url, destfile, mode = "wb")
  if (file.exists(destfile)) {
    print(paste("The suspect list has been successfully downloaded to", destfile))
  }
}

#' Open the NIST PDR entry for the current NIST PFAS suspect list
#'
#' This simply points your browser to the NIST public data repository for the
#' current NIST suspect list, where you can find additional information. Click
#' the download button in the left column of any file to download it.
#'
#' Requires the file "suspectlist_url.txt" to be present in the `config`
#' subdirectory of the current working directory.
#'
#' @return none
#' @export
#'
#' @examples
#' suspectlist_at_NIST()
suspectlist_at_NIST <- function() {
  url_file <- file.path('config', 'suspectlist_url.txt')
  if (!exists(url_file)) stop('Could not locate the "suspectlist_url.txt" file in the expected location.')
  url <- readLines(url_file, warn = FALSE)
  url <- gsub("\\/ds\\/", "\\/id\\/", url)
  url <- dirname(url)
  browseURL(url)
}
