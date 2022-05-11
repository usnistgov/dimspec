#' Converts a raw file into an mzML
#'
#' @param rawfile file path of the MS raw file to be converted
#' @param msconvert file path of the msconvert.exe file
#' @param config configuration settings for msconvert conversion to mzML
#' @param outdir directory path for the converted mzML file.
#'
#'
#' @examples

mzMLconvert <- function(rawfile, msconvert, config, outdir = getwd()) {
  if (file.exists(paste(outdir, "/convert_done.txt", sep = ""))) {file.remove(paste(outdir, "/convert_done.txt", sep = ""))}
  rawfile <- gsub("\\\\", "/", rawfile)
  config <- gsub("\\\\", "/", config)
  outdir <- gsub("\\\\", "/", outdir)
  if (!file.exists(msconvert)) {print("The MSConvert file location does not exist")}
  command <- paste(msconvert, rawfile, "-c", config, "-o", outdir)
  system(command)
  writeLines("1", con = paste(outdir, "/convert_done.txt", sep = ""))
}