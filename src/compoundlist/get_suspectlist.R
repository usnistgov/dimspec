get_suspectlist <- function(destfile = "src/compoundlist/suspectlist.xlsx") {
  url <- readLines('config/suspectlist_url.txt', warn = FALSE) #this file must be configured
  download.file(url, destfile, mode = "wb")
  if (file.exists(destfile)) {
    print(paste("The suspect list has been successfully downloaded to", destfile))
  }
}