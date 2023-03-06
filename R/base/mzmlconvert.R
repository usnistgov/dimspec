#' Converts a raw file into an mzML
#' 
#' @param rawfile file path of the MS raw file to be converted
#' @param msconvert file path of the msconvert.exe file, if NULL retrieves information from config directory
#' @param config configuration settings file for msconvert conversion to mzML, if NULL retrives information from config directory
#' @param outdir directory path for the converted mzML file.
#' 
#' @return CHR scalar path to the created file
#' 
#' @export
#' 
mzMLconvert <- function(rawfile, msconvert = NULL, config = NULL, outdir = getwd()) {
  rawfile <- paste0("\"", rawfile, "\"")
  if (is.null(msconvert)) {msconvert = readLines("config/msconvert_location.txt", warn = FALSE)}
  if (is.null(config)) {config = "\"config/msconvert_config.txt\""}
  if (file.exists(paste(outdir, "/convert_done.txt", sep = ""))) {file.remove(paste(outdir, "/convert_done.txt", sep = ""))}
  rawfile <- gsub("\\\\", "/", rawfile)
  config <- gsub("\\\\", "/", config)
  outdir <- gsub("\\\\", "/", outdir)
  if (!file.exists(eval(parse(text = msconvert)))) {stop("The MSConvert file location does not exist")}
  command <- paste(msconvert, rawfile, "-c", config, "-o", outdir)
  system(command)
  writeLines("1", con = paste(outdir, "/convert_done.txt", sep = ""))
  outputfile <- gsub("\\.[[:print:]]*$", ".mzML", basename(rawfile))
  if (file.exists(outputfile)) {
    paste0(getwd(), "/", gsub("\\.[[:print:]]*$", ".mzML", basename(rawfile)))
  }
}
