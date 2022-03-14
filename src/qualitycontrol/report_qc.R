#' Export QC result JSONfile into PDF
#'
#' @param jsonfile jsonfile file path
#' @param outputfile output pdf file path 
#'
#' @return generates reporting PDF
#' @export
#'
#' @examples
#' 

report_qc <- function(jsonfile = file.choose(),outputfile = gsub('.json', '.pdf', jsonfile, ignore.case = TRUE)) {
  rmarkdown::render('src/qualitycontrol/qcreport_template.Rmd', params = list(jsonfile = jsonfile), output_format = "pdf_document" ,output_file = outputfile)
}