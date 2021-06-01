# Start with clean environment
# - Remove environment variables
rm(list = ls())
# - Unload all packages
unload_packs <- c(
  names(sessionInfo()$loadedOnly),
  names(sessionInfo()$otherPkgs)
)
invisible(
  lapply(unload_packs, function(pkgs)
         function(pkgs) {
    detach(
      paste0('package:', pkgs),
      character.only = T,
      unload = T,
      force = T
    )
  })
)

# Ensure compliant environment
# - Verify required directory presence
if (!dir.exists("input")) {dir.create("input")}
if (!dir.exists("output")) {dir.create("output")}
if (!dir.exists("output/gather")) {dir.create("output/gather")}
if (!dir.exists("output/aggregate")) {dir.create("output/aggregate")}
if (!dir.exists("output/extract")) {dir.create("output/extract")}
if (!dir.exists("output/example")) {dir.create("output/example")}

# - Load required packages
packs       <- c("XML", "base64enc", "xlsx", "ggplot2", "logger", "tools", "stringi", "stringr", "magrittr", "readr")
packs_TRUE  <- which(packs %in% installed.packages())
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   dependencies = TRUE)
}
invisible(lapply(packs, library, character.only = TRUE))
rm(packs, unload_packs, packs_TRUE, packs_FALSE)

# - Source required files
source(file.path("config", "env.R"))
exclusions <- paste0(
  c(".RDS", "create_method_list", "generate_db", "compliance", "metadata_xml"),
  collapse = "|")
sources <- list.files('src', pattern = ".R", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(exclusions, sources)]
invisible(sapply(sources, source))
rm(sources, exclusions, fragments, exactmasschart)
