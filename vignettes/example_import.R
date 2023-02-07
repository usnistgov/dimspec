# Turn off all aspects in env_glob.txt for a simplified interface. This isn't necessary but it does reduce some chatter.
# Source the compliance file.
source("R/compliance.R")

# Add a file parser to pull files into the session
pull_json <- function(file_list) {
  stopifnot(is.character(file_list), length(file_list) > 0, all(file.exists(file_list)))
  out <- lapply(file_list, function(x) fromJSON(read_file(x)))
  names(out) <- file_path_sans_ext(basename(file_list))
  return(out)
}
# Obtain a list of files to be imported
src_dir <- "//ganymede.campus.nist.gov/PFASDB"
grep("peakJSON", list.dirs(src_dir), value = TRUE)
# The following could be abstracted into a single list.files call using the next line here but we'll keep them separate for now.
# src_folders <- c("20230109_NISTDATA_BJP", "20230109_FIELDDATA_BJP", "20230109_PFAS150_BJP")
f_list1 <- list.files(path = file.path(src_dir, "20230109_NISTDATA_BJP", "peakJSON"), pattern = ".JSON$", full.names = TRUE)
length(f_list1) # 101
f_list2 <- list.files(path = file.path(src_dir, "20230109_FIELDDATA_BJP", "peakJSON"), pattern = ".JSON$", full.names = TRUE)
length(f_list2) # 151
f_list3 <- list.files(path = file.path(src_dir, "20230109_PFAS150_BJP", "peakJSON"), pattern = ".JSON$", full.names = TRUE)
length(f_list3) # 51 on 20 Jan 2023 - not yet finished

# Convert them to list objects in the session
to_import1 <- pull_json(f_list1)
length(to_import1) == length(f_list1)
to_import2 <- pull_json(f_list2)
length(to_import2) == length(f_list2)
to_import3 <- pull_json(f_list3)
length(to_import3) == length(f_list3)

# Check suitability
ver1 <- verify_import_requirements(to_import1)
cat(sprintf("Required data are %spresent.\n", ifelse(all(ver1$has_all_required), "", "not ")),
    sprintf("Full detail data are %spresent.\n", ifelse(all(ver1$has_full_detail), "", "not ")),
    sprintf("Extra data are %spresent.\n", ifelse(any(ver1$has_extra), "", "not ")),
    sprintf("Mismatched names are %spresent.\n", ifelse(any(ver1$has_name_mismatches), "", "not "))
)
ver2 <- verify_import_requirements(to_import2)
cat(sprintf("Required data are %spresent.\n", ifelse(all(ver2$has_all_required), "", "not ")),
    sprintf("Full detail data are %spresent.\n", ifelse(all(ver2$has_full_detail), "", "not ")),
    sprintf("Extra data are %spresent.\n", ifelse(any(ver2$has_extra), "", "not ")),
    sprintf("Mismatched names are %spresent.\n", ifelse(any(ver2$has_name_mismatches), "", "not "))
)
ver3 <- verify_import_requirements(to_import3)
cat(sprintf("Required data are %spresent.\n", ifelse(all(ver3$has_all_required), "", "not ")),
    sprintf("Full detail data are %spresent.\n", ifelse(all(ver3$has_full_detail), "", "not ")),
    sprintf("Extra data are %spresent.\n", ifelse(any(ver3$has_extra), "", "not ")),
    sprintf("Mismatched names are %spresent.\n", ifelse(any(ver3$has_name_mismatches), "", "not "))
)

# We will assume a full rebuild of the database and populate it with PFAS default lists of chemicals and normalization values.
# Skip this step to import into an existing database.
build_db(populate_with = "populate_pfas.sql")
# Create an install name for this database
make_install_code(new_name = "NIST PFAS (official)")

# If not connected already, establish the connection.
manage_connection()

# These files all include the NIST PFAS ID as an integer, but for consistency we want to instead match the alias held in the database: template is "NISTPFAS00XXXX"
to_import1 <- lapply(
  to_import1,
  function(x) {
    x$compounddata$id[1] <- x$compounddata$id[1] %>%
      as.character() %>%
      str_remove_all("NISTPFAS") %>%
      as.integer() %>%
      as.character() %>%
      str_pad(6, "left", "0") %>%
      str_c("NISTPFAS", .)
    return(x)
    }
  )

# Begin import (can test with the first member of `to_import` if you like.)
full_import(import_object = to_import1,
            stop_if_missing_recommended = FALSE,
            include_if_missing_recommended = TRUE)
