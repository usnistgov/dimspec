# These functions apply ONLY to the development stage of the project. All rely
# on leveraging development-assist packages (e.g. [usethis], [devtools]) to some
# degree. Not sure if these will work as part of a package, since an active
# project has a directory separate from that of the installed package by design.

#' Open and edit project files
#'
#' Project files are organized in several topical directories depending on their
#' purpose as part of the package. For example, several project control
#' variables are set to establish the session global environment in the "config"
#' directory rather than the "R" directory.
#'
#' If a direct file match to name is not found, it will be searched for using a
#' recursive [list.files] allowing for regex matches (e.g. ".R$"). Directories
#' are similarly sought out within the project. Reasonable feedback is provided.
#'
#' This convenience function uses [usethis::edit_file] to open (or create if
#' `create_new` is TRUE) any given file in the project.
#'
#' @note If the directory and file cannot be found, and `create_new` is true,
#'   the directory will be placed within the project directory.
#'
#' @param name CHR scalar of the file name to open, accepts regex
#' @param dir CHR scalar of a directory name to search within
#' @param create_new LGL scalar of whether to create the file (similar
#'   functionality to [usethis]; default FALSE)
#'
#' @return None, opens a file for editing
#' @export
#' 
open_proj_file <- function(name, dir = NULL, create_new = FALSE) {
  if (exists("VERIFY_ARGUMENTS") && VERIFY_ARGUMENTS && exists("verify_args")) {
    conds <- list(
      name = list(c("mode", "character"), c("length", 1)),
      dir = list(c("mode", "character"), c("length", 1)),
      create_new = list(c("mode", "logical"), c("length", 1))
    )
    if (is.null(dir)) {
      args <- list(name, create_new)
      conds$dir <- NULL
    } else {
      args <- list(name, dir, create_new)
    }
    arg_check <- verify_args(
      args = args,
      conditions = conds
    )
    stopifnot(arg_check$valid)
  }
  open <- rlang::is_interactive()
  if (create_new) {
    if (tools::file_ext(name) == "") {
      new_fname <- paste0(name, ".R")
    }
  }
  if (file.exists(name)) {
    usethis::edit_file(usethis::proj_path(name), open = open)
  } else if (is.null(dir)) {
    match_name <- grep(pattern = name, list.files(path = here::here(), recursive = TRUE, full.names = TRUE), value = TRUE)
    if (length(match_name) == 1) {
      usethis::edit_file(match_name, open = open)
    } else if (length(match_name) == 0) {
      if (exists("log_it")) {
        log_it(ifelse(create_new, "warn", "error"),
               sprintf('Could not locate a file matching the pattern "%s" in this project.', name))
      }
      if (create_new) {
        usethis::edit_file(usethis::proj_path(new_fname), open = open)
      }
    } else {
      if (exists("log_it")) {
        log_it("error", sprintf('Multiple files matching the pattern "%s" were found in this project. Please be more specific or provide the "dir" argument. Perhaps it was one of these?', name))
        cat(paste0(match_name, collapse = "\n"))
      }
    }
  } else if (file.exists(here::here(dir, name))) {
    usethis::edit_file(usethis::proj_path(dir, name), open = open)
  } else {
    all_dirs <- list.dirs()
    if (any(grepl(dir, all_dirs))) {
      match_dir <- all_dirs[grepl(dir, all_dirs)]
      if (length(match_dir) > 1) {
        if (exists("log_it")) {
          log_it("error", sprintf('Multiple directories matching the pattern "%s" were found. Please be more specific.', dir))
          cat(paste0(match_dir, collapse = "\n"))
          }
      }
    } else {
      if (exists("log_it")) {
        log_it("warn", sprintf('Could not locate a directory matching "%s". Looking in all project directories.', dir))
      }
      match_dir <- "."
    }
    match_name <- list.files(path = match_dir, pattern = name, recursive = TRUE, full.names = TRUE)
    if (length(match_name) == 1) {
      usethis::edit_file(usethis::proj_path(match_name), open = open)
    } else if (length(match_name) == 0) {
      if (exists("log_it")) {
        log_it(ifelse(create_new, "warn", "error"),
               sprintf('Could not locate a file matching the pattern "%s" in the "%s" directory.', name, dir))
      }
      if (create_new) {
        usethis::edit_file(usethis::proj_path(dir, new_fname), open = open)
      }
    } else {
      if (exists("log_it")) {
        log_it("error", sprintf('Multiple files matching the pattern "%s" were found in the "%s" directory. Please be more specific.', name, dir))
        cat(paste0(match_name, collapse = "\n"))
      }
    }
  }
}

#' Convenience shortcut to open and edit session environment variables
#'
#' Calls [open_proj_file] for either the R, global, or logging environment
#' settings containing the most common settings dictating project behavior.
#'
#' @param name CHR scalar, one of "R", "global", or "logging".
#'
#' @return None, opens a file for editing
#' @export
#' 
open_env <- function(name = c("R", "global", "logging", "rdkit", "shiny", "plumber")) {
  name <- switch(
    match.arg(name),
    "R" = "env_R.R",
    "global" = "env_glob.txt",
    "logging" = "env_logger.R",
    "rdkit" = "env_py.R",
    "shiny" = "env_shiny.R",
    "plumber" = "env_plumb.R"
  )
  open_proj_file(name)
}
