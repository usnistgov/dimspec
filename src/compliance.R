if (!dir.exists("input")) {dir.create("input")}
if (!dir.exists("output")) {dir.create("output")}

packs       <- c("XML", "base64enc", "xlsx", "ggplot2")

packs_TRUE  <- which(packs %in% installed.packages())
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   dependencies = TRUE)
}


lapply(packs, library, character.only = TRUE)
rm(packs, packs_TRUE, packs_FALSE)

sources <- list.files('src', pattern = ".R", full.names = TRUE, recursive = TRUE)
sources <- sources[-grep(".RDS", sources)]
sources <- sources[-which(sources == "src/compliance.R")]
sapply(sources, source)