mzMLconvert <- function(rawfile, msconvert, config, outdir = getwd()) {
  if (file.exists(paste(outdir, "/convert_done.txt", sep = ""))) {file.remove(paste(outdir, "/convert_done.txt", sep = ""))}
  rawfile <- gsub("\\\\", "/", rawfile)
  config <- gsub("\\\\", "/", config)
  outdir <- gsub("\\\\", "/", outdir)
  command <- paste(msconvert, rawfile, "-c", config, "-o", outdir)
  system(command)
  writeLines("1", con = paste(outdir, "/convert_done.txt", sep = ""))
}