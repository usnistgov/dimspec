# Database operations and inspection -------------------------------------------

#' Get table definition from SQLite
#'
#' Given a database connection (`con`). Get more information about the
#' properties of (a) database table(s) directly from `PRAGMA table_info()`
#' rather than e.g. [DBI::dbListFields()]. Set `get_sql` to `TRUE` to include
#' the direct schema using sqlite_master; depending on formatting this may or
#' may not be directly usable though some effort has been made to remove
#' formatting characters (e.g. line feeds, tabs, etc) if stringr is available.
#'
#' Note that the package `stringr` is required for formatting returns that
#' include either `get_sql` or `pretty` as TRUE.
#'
#' @param db_table CHR vector name of the table(s) to inspect
#' @param conn connection object (default: con)
#' @param get_sql BOOL scalar of whether or not to return the schema sql
#'   (default FALSE)
#' @param pretty BOOL scalar for whether to return "pretty" SQL that includes
#'   human readability enhancements; if this is set to TRUE (the default), it is
#'   recommended that the output is fed through `cat` and, in the case of
#'   multiple tables
#'
#' @return data.frame object representing the SQL PRAGMA expression
#' @export
#'
#' @examples
pragma_table_def <- function(db_table, conn = con, get_sql = FALSE, pretty = TRUE) {
  require(dplyr)
  # Argument validation relies on verify_args
  log_it("trace", sprintf('Getting table definition for "%s".', format_list_of_names(db_table)))
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db_table = list(c("mode", "character"), c("n>=", 1)),
        conn     = list(c("length", 1)),
        get_sql  = list(c("mode", "logical"), c("length", 1)),
        pretty   = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "pragma_table_def"
    )
    stopifnot(arg_check$valid)
  }
  # Ensure table exists
  db_table <- existing_tables(db_table, conn)
  
  # Define function scope
  func <- ifelse(get_sql,
                 "select name, type, sql from sqlite_master where name = '%s'",
                 "PRAGMA table_xinfo('%s')"
  )
  # Basic return
  out <- lapply(db_table,
                function(x) {
                  tmp <- dbGetQuery(conn, sprintf(func, x))
                  tmp$table_name <- x
                  return(tmp)
                }
  )
  # Shape up the return
  if (get_sql) {
    out <- dplyr::bind_rows(out) %>%
      select(table_name, type, sql)
    if ("stringr" %in% installed.packages()) {
      if (!pretty) {
        out$sql <- stringr::str_replace_all(out$sql, "\n|\t", " ")
      }
      out$sql <- out$sql %>%
        stringr::str_replace_all(" {2,5}", " ")
    } else {
      warning("Package 'stringr' is required for better formatting of SQL returns.")
    }
  } else {
    out <- dplyr::bind_rows(out) %>%
      select(cid, table_name, name:pk)
  }
  # Return data frame object representing SQLite db_table schema(s)
  return(out)
}

#' Explore properties of an SQLite table
#'
#' Add functionality to `pragma_table_def` by filtering on column properties
#' such as required and primary key fields. This provides some flexibility to
#' searching table properties without sacrificing the full details of table
#' schema. Parameter `get_sql` is forced to FALSE; only information available
#' via PRAGMA is searched by this function.
#'
#' This is intended to support validation during database communications with an
#' SQLite connection, especially for application (e.g. `shiny` development) by
#' allowing for programmatic inspection of datbase columns by name and property.
#'
#' @param db_table CHR vector name of the table(s) to inspect
#' @param db_conn connection object (default: con)
#' @param condition CHR vector matching specific checks, must be one of
#'   c("required", "has_default", "is_PK") for constraints where a field must
#'   not be null, has a default value defined, and is a primary key field,
#'   respectively. (default: NULL)
#' @param name_like CHR vector of character patterns to match against column
#'   names via grep. If length > 1, will be collapsed to a basic OR regex (e.g.
#'   c("a", "b") becomes "a|b"). As regex, abbreviations and wildcards will
#'   typically work, but care should be used in that case. (default: NULL)
#' @param data_type CHR vector of character patterns to match against column
#'   data types via grep. If length > 1 will be collapsed to a basic "OR" regex
#'   (e.g. c("int", "real") becomes "int|real"). As regex, abbreviations and
#'   wildcards will typically work, but care should be used in that case.
#'   (default: NULL)
#' @param include_comments LGL scalar of whether to include comments in the
#'   return data frame (default: FALSE)
#' @param names_only LGL scalar of whether to include names meeting defined
#'   criteria as a vector return value  (default: FALSE)
#'
#' @return
#' @export
#'
#' @examples
pragma_table_info <- function(db_table,
                              db_conn          = con,
                              condition        = NULL,
                              name_like        = NULL,
                              data_type        = NULL,
                              include_comments = FALSE,
                              names_only       = FALSE) {
  # Argument validation relies on verify_args
  log_it("trace", glue('Getting table definition for "{db_table}".'))
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db_table, db_conn, include_comments, names_only),
      conditions = list(
        db_table         = list(c("mode", "character"), c("n>=", 1)),
        db_conn          = list(c("length", 1)),
        include_comments = list(c("mode", "logical"), c("length", 1)),
        names_only       = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn   = "pragma_table_info"
    )
    stopifnot(arg_check$valid)
  }
  # Ensure table exists
  db_table <- existing_tables(db_table, db_conn)
  # Get table properties
  out <- pragma_table_def(db_table = db_table, con = db_conn, get_sql = FALSE)
  # Set up condition checks
  valid_conditions <- c("required", "has_default", "is_PK")
  if (!is.null(condition)) {
    condition <- match.arg(condition, valid_conditions, several.ok = TRUE)
  }
  fns <- list(
    function(out) which(out$notnull == 1),
    function(out) which(!is.na(out$dflt_value)),
    function(out) which(out$pk == 1)
  )
  names(fns) <- valid_conditions
  check_columns <- c("notnull", "dflt_value", "pk")
  # Define which rows and columns to return
  out_rows <- 1:nrow(out)
  out_check <- integer(0)
  # Limit column outputs?
  limit <- lapply(c(condition, name_like, data_type),
                  function(x) !is.null(x)) %>%
    unlist() %>%
    any()
  # Get comments if any
  if (include_comments) {
    tmp <- pragma_table_def(db_table = db_table, conn = db_conn, get_sql = TRUE) %>%
      tidy_comments() %>%
      bind_rows()
    out <- out %>%
      bind_cols(tmp) %>%
      relocate(table_comment, .after = table_name)
  }
  # Do condition checks
  if (!is.null(condition)) {
    for (check in condition) {
      out_check <- c(out_check, fns[[check]](out))
    }
  }
  if (!is.null(name_like)) {
    name_like <- paste0(name_like, collapse = "|")
    out_check <- c(out_check, grep(name_like, out$name))
  }
  if (!is.null(data_type)) {
    data_type <- paste0(data_type, collapse = "|")
    out_check <- c(out_check, grep(toupper(data_type), out$type))
  }
  # Shape up the return
  if (limit) out_rows <- out_check %>% unique() %>% sort()
  out <- out[out_rows, ]
  if (names_only) {
    out <- out$name
  }
  # Return data frame object representing PRAGMA table_info columns matching the
  # requested properties
  return(out)
}

#' Build or rebuild the database from scratch
#'
#' This function will build or rebuild the NIST HRAMS database structure from
#' scratch, removing the existing instance. By default, most parameters are set
#' in the environment (at "./config/env_glob.txt") but any values can be passed
#' directly. This can be used to quickly spin up multiple copies with a clean
#' slate using different build files, data files, or return to the last stable
#' release.
#'
#' If sqlite3 and its command line interface are available on your platform,
#' that will be used (preferred method) but, if not, this function will read in
#' all the necessary files to directly create it using shell commands. The shell
#' method may not be universally applicable to certain compute environments or
#' may require elevated permissions.
#'
#' @param db CHR scalar of the database name (default: session value DB_NAME)
#' @param build_from CHR scalar of a SQL build script to use (default:
#'   environment value DB_BUILD_FILE)
#' @param populate LGL scalar of whether to populate with data from the file in
#'   `populate_with` (default: TRUE)
#' @param populate_with CHR scalar for the populate script (e.g.
#'   "populate_demo.sql") to during after the build is complete; (default:
#'   session value DB_DATA); ignored if `populate = FALSE`
#' @param archive LGL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default: FALSE), passed to [`remove_db()`]
#' @param sqlite_cli CHR scalar to use to look for installed sqlite3 CLI tools
#'   in the current system environment (default: session value SQLITE_CLI)
#' @param connect LGL scalar of whether or not to connect to the rebuilt
#'   database in the global environment as object `con`` (default: FALSE)
#'
#' @return None, check console for details
#' @export
#'
#' @examples
build_db <- function(db            = DB_NAME,
                     build_from    = DB_BUILD_FILE,
                     populate      = TRUE,
                     populate_with = DB_DATA,
                     archive       = FALSE,
                     sqlite_cli    = SQLITE_CLI,
                     connect       = FALSE) {
  require(glue)
  # Argument validation
  log_it("trace", glue('Starting build of "{db}".'))
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db            = list(c("mode", "character"), c("length", 1)),
        build_from    = list(c("mode", "character"), c("length", 1)),
        populate      = list(c("mode", "logical"), c("length", 1)),
        populate_with = list(c("mode", "character"), c("length", 1)),
        archive       = list(c("mode", "logical"), c("length", 1)),
        sqlite_cli    = list(c("mode", "character"), c("length", 1)),
        connect       = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "build_db"
    )
    stopifnot(arg_check$valid)
  }
  log_it("trace", glue('Attempting to connect to "{db_table}".'))
  manage_connection(db = db, reconnect = FALSE, disconnect = TRUE)
  build_file    <- list.files(pattern = build_from,
                              full.names = TRUE,
                              recursive = TRUE)
  # Populate with all data sources
  # -- RSQLite does not allow for the CLI [.DOT] commands
  # -- If sqlite3 CLI is installed, use that by preference
  sqlite_available <- length(Sys.which(sqlite_cli) > 1)
  # Do it the short way (with CLI)
  if (sqlite_available) {
    log_it("trace", glue('SQLite CLI under alias "{sqlite_cli}" is available. Building directly...'))
    require(glue)
    sqlite_call   <- glue('{sqlite_cli} {db}')
    if (!file.exists(build_file)) {
      stop(glue('Cannot locate file "{build_file}" in this directory.'))
    }
    build         <- glue('-cmd ".read {build_file}"')
    if (populate) {
      populate_file <- list.files(pattern = populate_with, full.names = TRUE, recursive = TRUE)
      if (!file.exists(populate_file)) {
        log_it("warn", glue('Cannot locate file "{populate_file}" in this directory; "{db}" will be created but not populated.'))
        populate_cmd <- ""
      } else {
        populate_cmd <- glue(' -cmd ".read {populate_file}"')
      }
    } else {
      populate_cmd <- ""
    }
    full_call     <- glue('{build}{populate_cmd}') %>%
      str_remove_all("\\./")
    full_call     <- glue('{sqlite_call} {full_call} -cmd ".exit"')
    log_it("trace", glue("Issuing shell command {full_call}"))
    if (file.exists(db)) remove_db(db = db, archive = archive)
    shell(full_call)
    log_it("info", glue('Finished attempted build of "{db}" as specified. Check console for any failure details.'))
  } else {
    # Do it the long way
    log_it("trace", glue('SQLite CLI under alias "{sqlite_cli}" is not available. Building through R...'))
    # -- Ensure packages are available
    reqs <- c("stringr", "magrittr", "readr", "DBI")
    packs_available <- reqs %in% installed.packages()
    if (!all(packs_available)) {
      stop("Some packages were not available. Please install packages:", paste0(reqs[!packs_available]))
    }
    invisible(lapply(reqs, require, character.only = TRUE))
    # -- Remove the existing database
    if (file.exists(db)) {
      log_it("trace", glue('Removing "{db}".'))
      remove_db(db = db, archive = archive)
    }
    # -- Create the build commands in R to pass through RSQLite::SQLite()
    log_it("trace", glue('Creating build statements.'))
    build_path <- list.files(pattern = build_file,
                             full.names = TRUE,
                             recursive = TRUE)
    if (!length(build_path) == 1) {
      stop(sprintf('Could not find build file "%s" in this directory.',
                   build_path))
    }
    build_statement <- create_fallback_build(build_path)
    build_statement <- build_statement[nchar(build_statement) > 1]
    # -- Create the database and read in the build statements
    log_it("trace", glue('Creating new database as "{db}".'))
    con <- dbConnect(RSQLite::SQLite(), db)
    log_it("trace", glue('Building "{db}"...'))
    invisible(
      lapply(build_statement,
             function(x) dbSendStatement(con, str_trim(x, "both")))
    )
    # Disconnect
    dbDisconnect(con)
  }
  if (connect) {
    log_it("trace", 'Connecting to "{db}".')
    manage_connection(db = db, reconnect = TRUE)
  }
}

#' Remove an existing database
#'
#' This is limited to only the current working directory and includes its
#' subdirectories. If you wish to retain a copy of the prior database, ensure
#' argument `archive = TRUE` (note the default is FALSE) to create a copy of the
#' requested database prior to rebuild; this is created in the same directory as
#' the found database and appends
#'
#' @param db CHR scalar name of the database to build (default: session value
#'   DB_NAME)
#' @param archive LGL scalar of whether to create an archive of the current
#'   database (if it exists) matching the name supplied in argument `db`
#'   (default: FALSE)
#'
#' @return None, check console for details
#' @export
#'
#' @examples
remove_db <- function(db = DB_NAME, archive = FALSE) {
  require(tools)
  log_it("trace", glue('Starting removal of "{db}"'))
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        db      = list(c("mode", "character"), c("length", 1)),
        archive = list(c("mode", "logical"), c("length", 1))
      )
    )
    stopifnot(arg_check$valid)
  }
  # Resolve database file location
  log_it("trace", glue('Finding database file "{db}"'))
  db_path  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
  db_path  <- db_path[basename(db_path) == db]
  if (length(db_path) == 0) {
    log_it("error", sprintf('Database "%s" does not exist in this directory tree.', db))
    return(NULL)
  } else if (length(db_path) > 1) {
    log_it("warn", glue('Multiple files found for "{db}" in this directory.'))
    # correct_path <- select.list(db_path, title = "Please select one.")
    correct_path <- resolve_multiple_values(db_path, db)
  }
  if (length(db_path) == 1) {
    # Ensure no current connection from R
    check <- try(manage_connection(db = db, reconnect = FALSE, disconnect = TRUE))
    if (class(check) == "try-error") stop("Unable to automatically stop the connection to '", db, "'.")
    if (archive) {
      now       <- format(Sys.time(), "%Y%m%d%H%M%S%Z")
      fname     <- file_path_sans_ext(db)
      new_fname <- gsub(fname,
                        sprintf("%s_archive_%s",fname, now),
                        db)
      file.copy(db_path, new_fname)
      log_it("success", sprintf('Archive created as "%s"', new_fname))
    }
    result <- try(file.remove(db_path))
    if (class(result) == "try-error") {
      log_it("error", sprintf('Database "%s" could not be removed; another connection is likely open.', db))
    } else {
      log_it("success", sprintf('Database "%s" removed.', db))
    }
  }
}

#' Check presence of a database table
#'
#' This convenience function checks for the existence of one or more `db_table`
#' objects in a database.
#'
#' @param db_table CHR vector of table names to check
#' @param conn connection object (default: con)
#'
#' @return CHR vector of existing tables
#' @export
#'
#' @examples
existing_tables <- function(db_table, db_conn = con) {
  tables_exist <- db_table %in% dbListTables(db_conn)
  if (!all(tables_exist)) {
    log_it("warn", sprintf('No table named "%s" was found in this schema.', db_table[!tables_exist]))
    db_table <- db_table[tables_exist]
  }
  if (length(db_table) == 0) stop("No valid tables were found in this schema.")
  return(db_table)
}

#' Create a data dictionary
#'
#' Get a list of tables and their defined columns with properties, including
#' comments, suitable as a data dictionary from a connection object amenable to
#' [odbc::dbListTables]. This function relies on [pragma_table_info].
#'
#' @param conn_obj connection object (default:con)
#'
#' @return LIST of length equal to the number of tables in `con` with attributes
#'   identifying which tables, if any, failed to render into the dictionary.
#' @export
#'
#' @examples
data_dictionary <- function(conn_obj = con) {
  tabls <- dbListTables(con)
  tabls <- tabls[-grep("sqlite_", tabls)]
  out <- vector('list', length(tabls))
  names(out) <- tabls
  failures <- character(0)
  for (tabl in tabls) {
    tmp <- try(pragma_table_info(db_table = tabl,
                                 db_conn = con,
                                 include_comments = TRUE))
    if (class(tmp) == "try-error") {
      log_it("warn", sprintf('Dictionary failure on table "%s"\n', tabl))
      failures <- c(failures, tabl)
    } else {
      log_it("success", sprintf('"%s" added to dictionary\n', tabl))
    }
    out[[tabl]] <- tmp
  }
  if (length(failures) > 0) {
    has_failures <- TRUE
    msg <- sprintf("Dictionary was not available for %s: %s",
                   ifelse(length(failures) > 1, "tables", "table"),
                   format_list_of_names(failures))
    log_it("warn", msg)
  } else {
    has_failures <- FALSE
    failures <- "Dictionary available for all tables."
    log_it("success", failures)
  }
  attr(out, "has_failures") <- has_failures
  attr(out, "failures")     <- failures
  return(out)
}

#' Tidy up table and field comments
#'
#' Creates more human-readable outputs after extracting the raw SQL used to
#' build entities and parsing out the comments as identified with the /* ... */
#' multi-line comment flag pair. Single line comments are not extracted. The
#' first comment is assumed to be the table comment. See examples in the
#' `config/sql_nodes` directory.
#'
#' @param obj result of calling [pragma_table_def] with `get_sql` = TRUE
#'
#' @return LIST of length equal to `obj` containing extracted comments
#' @export
#'
#' @examples
tidy_comments <- function(obj) {
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = list(obj),
      conditions = list(
        obj      = list(c("mode", "data.frame"))
      )
    )
    stopifnot(arg_check$valid)
  }
  if (!all(c("sql", "table_name") %in% names(obj))) {
    log_it("error", 'Object must contain both "sql" and "table_name" entries. Returning as provided.')
    out <- obj
  } else {
    comments <- obj$sql %>%
      str_remove_all("/\\* (Check constraints|Foreign key relationships) \\*/") %>%
      str_extract_all("/\\* [[:alnum:][:punct:]\\[\\]\\+ ]+ \\*/") %>%
      lapply(str_remove_all, "/\\* | \\*/")
    table_comments <- lapply(comments, function(x) x[1])
    field_comments <- lapply(comments, function(x) x[-1])
    out <- vector('list', nrow(obj))
    for (i in 1:length(out)) {
      out[[i]]$table_comment  <- table_comments[[i]]
      out[[i]]$field_comments <- field_comments[[i]]
    }
    names(out) <- obj$db_table
  }
  return(out)
}

#' Save the current data dictionary to disk
#'
#' Use [data_dictionary] and save the output to a local file. If `output_format`
#' is one of "data.frame" or "list", the resulting file will be saved as an RDS.
#' Parameter `output_file` will be used during the save process; relative paths
#' are fine and will be identified by the current working directory.
#'
#' @param conn_obj connection object (default: con)
#' @param output_format CHR scalar, one of (capitalization insensitive) "json",
#'   "csv", "data.frame", or "list" (default "json")
#' @param output_file CHR scalar indicating where to save the resulting file; an
#'   appropriate file name will be constructed if left NULL (default: NULL)
#' @param overwrite_existing LGL scalar indicating whether to overwrite an
#'   existing file whose name matches that determined from `output_file`
#'   (default: TRUE); file names will be appended with "(x)" sequentially if
#'   this is FALSE and a file with matching name exists.
#'
#' @return
#' @export
#'
#' @examples
save_data_dictionary <- function(conn_obj           = con,
                                 output_format      = "json",
                                 output_file        = NULL,
                                 overwrite_existing = TRUE) {
  # Argument validation
  if (exists("arg_check")) {
    arg_check <- verify_args(
      args       = list(output_format, overwrite_existing),
      conditions = list(
        output_format      = list(c("mode", "character"), c("length", 1)),
        overwrite_existing = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "build_db"
    )
    stopifnot(arg_check$valid)
  }
  logger <- "logger" %in% (.packages())
  output_format <- tolower(output_format)
  output_format <- match.arg(output_format,
                             c("json", "csv", "data.frame", "list"))
  if (!output_format %in% c("json", "csv")) {
    f_ext <- "RDS"
  } else {
    f_ext <- output_format
  }
  if (is.null(output_file)) {
    f_name <- sprintf("%s/%s_%s_data_dictionary.%s",
                      getwd(),
                      DB_TITLE,
                      DB_VERSION,
                      f_ext)
  } else {
    f_name <- output_file
  }
  i <- 0
  if (all(file.exists(f_name), overwrite_existing)) {
    file.remove(f_name)
  }
  while (file.exists(f_name)) {
    i <- i + 1
    f_name <- str_replace(f_name,
                          sprintf("_data_dictionary%s.%s", 
                                  ifelse(i == 1, "",
                                         sprintf(" (%s)", i - 1)),
                                  f_ext),
                          sprintf("_data_dictionary (%s).%s",
                                  i,
                                  f_ext)
    )
  }
  out <- data_dictionary(conn_obj)
  if (is.null(output_file)) {
    if (logger) {
      log_warn('No file name provided to "output_file", saving as "{f_name}"')
    } else {
      cat(sprintf('No file name provided to "output_file", saving as "%s"\n', f_name))
    }
  }
  if (attr(out, "has_failures")) {
    if (logger) {
      log_error('This dictionary failed on tables {format_list_of_names(out$failures)}')
    } else {
      stop(sprintf('This dictionary failed on tables %s',
                   format_list_of_names(out$failures)))
    }
  }
  switch(output_format,
         "json"       = out %>%
           jsonlite::toJSON() %>%
           write_file(f_name),
         "csv"        = out %>%
           dplyr::bind_rows() %>%
           readr::write_csv(f_name),
         "data.frame" = out %>%
           dplyr::bind_rows() %>%
           write_rds(f_name),
         "list"       = out %>%
           write_rds(f_name)
  )
}

#' Create a simple entity relationship map
#'
#' This will poll the database connection and create an entity relationship map
#' as a list directly from defined SQL statements used to build the table or
#' view. For each table object it returns a list of length three containing the
#' entity names that the table (1) `references` (i.e. has a foreign key to), (2)
#' is `referenced_by` (i.e. is a foreign key for), and (3) views where it is
#' `used_in_view`. These are names. This is intended for use as a mapping
#' shortcut when ER Diagrams are unavailable, or for quick reference within a
#' project, similarly to a dictionary relationship reference.
#'
#' SQL is generated from [pragma_table_def] with argument `get_sql` = TRUE and
#' ignores entities whose names start with "sqlite".
#'
#' @param conn connection object, specifically of class "SQLiteConnection" but
#'   not strictly enforced
#'
#' @return nested LIST object
#' @export
#'
#' @examples
er_map <- function(conn = con) {
  # Verify arguments
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        conn      = list(c("length", 1))
      ),
      from_fn    = "er_map"
    )
    stopifnot(arg_check$valid)
  }
  
  build_statements <- pragma_table_def(conn = conn, db_table = dbListTables(conn), get_sql = TRUE)
  build_statements <- build_statements[-grep("^sqlite", build_statements$table_name), ]
  n_tables <- nrow(build_statements)
  t_names  <- build_statements$table_name
  er_mask  <- matrix(rep(0, n_tables ^2), nrow = n_tables)
  rownames(er_mask) <- t_names
  colnames(er_mask) <- t_names
  view_mask <- er_mask
  er_type  <- str_extract_all(build_statements$sql, "CREATE [:word:]+") %>%
    lapply(str_remove_all, "CREATE ")
  tables_and_views <- which(unlist(er_type) %in% c("TABLE", "VIEW"))
  build_statements <- build_statements[tables_and_views, ]
  ref_tables <- str_extract_all(build_statements$sql, "REFERENCES [:word:]+") %>%
    lapply(str_remove_all, "REFERENCES ")
  refs     <- str_extract_all(build_statements$sql, "(FOREIGN KEY \\([:word:]+\\) )?REFERENCES [:word:]+\\([:word:]+\\)") %>%
    lapply(str_remove_all, "FOREIGN KEY \\(") %>%
    lapply(str_replace_all, "\\) ", " ")
  used_in  <- str_extract_all(build_statements$sql, "(JOIN|FROM) [:word:]+") %>%
    lapply(str_remove_all, "(JOIN|FROM) ")
  for (i in 1:n_tables) {
    z <- which(t_names %in% ref_tables[[i]])
    if (length(z) > 0) {
      er_mask[i, z] <- 1
    }
    z <- which(t_names %in% used_in[[i]])
    if (length(z) > 0) {
      view_mask[z, i] <- 1
    }
  }
  er_map   <- vector("list", n_tables) %>%
    setNames(t_names)
  for (i in 1:n_tables) {
    er_map[[i]] <- list(
      object_type         = er_type[[i]],
      references_tables   = if (er_type[[i]] == "TABLE") {
        t_names[as.logical(er_mask[i, ])]
      } else {
        t_names[as.logical(view_mask[, i])]
      },
      references          = refs[[i]],
      normalizes_tables   = t_names[as.logical(er_mask[, i])],
      used_in_view        = t_names[as.logical(view_mask[i, ])]
    )
  }
  direct_add <- names(
    er_map[
      unlist(map(er_map, function(x) all(
        x$object_type == "TABLE",
        length(x$references) == 0)))
    ]
  )
  dependent_add <- names(
    er_map[
      unlist(map(er_map, function(x) all(
        x$object_type == "TABLE",
        length(x$references) > 0)))
    ]
  )
  all_tables <- sort(c(direct_add, dependent_add))
  all_views  <- names(er_map)[!names(er_map) %in% all_tables]
  er_map$tables_without_dependency <- direct_add
  er_map$tables_with_normalization_dependency <- dependent_add
  er_map$is_table <- all_tables
  er_map$is_view  <- all_views
  return(er_map)
}

#' Check for, and optionally remove, a database connection object
#'
#' This function seeks to abstract connection management objects to a degree. It
#' seeks to streamline the process of connecting and disconnecting existing
#' connections as defined by function parameters. This release has not been
#' tested extensively drivers other than SQLite.
#'
#' @param db CHR scalar name of the database to check, defaults to the name
#'   supplied in config/env.R (default: session variable DB_NAME)
#' @param drv_pack CHR scalar of the package used to connect to this database
#'   (default: session variable DB_DRIVER)
#' @param conn_class CHR vector of connection object classes to check against.
#'   Note this may depend heavily on connection packages and must be present in
#'   the class names of the driver used. (default session variable DB_CLASS)
#' @param conn_name CHR scalar of the R environment object name to use for this
#'   connection (default: "con")
#' @param is_local LGL scalar indicating whether or not the referenced database
#'   is a local file, if not it will be treated as though it is either a DSN or
#'   a database name on your host server, connecting as otherwise defined
#' @param rm_objects LGL scalar indicating whether or not to remove objects
#'   identifiably connected to the database from the current environment. This
#'   is particularly useful if there are outstanding connections that need to be
#'   closed (default: TRUE)
#' @param reconnect LGL scalar indicating whether or not to connect if a
#'   connection does not exist; if both this and `disconnect` are true, it will
#'   first be disconnected before reconnecting. (default: TRUE)
#' @param disconnect LGL scalar indicating whether or not to terminate and
#'   remove the connection from the current global environment (default: TRUE)
#' @param ... named list of any other connection parameters required for your
#'   database driver (e.g. postgres username/password)
#'
#' @return None
#' @export
#'
#' @note For more complicated setups, it may be easier to use this function by
#'   storing parameters in a list and calling with [base::do.call()]
#'
#' @examples
manage_connection <- function(db          = DB_NAME,
                              drv_pack    = DB_PACKAGE,
                              drv         = DB_DRIVER,
                              conn_class  = DB_CLASS,
                              conn_name   = "con",
                              is_local    = TRUE,
                              rm_objects  = TRUE,
                              reconnect   = TRUE,
                              disconnect  = TRUE,
                              ...) {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(db, drv_pack, drv, conn_class, conn_name, is_local, rm_objects, reconnect, disconnect),
      conditions = list(
        db         = list(c("mode", "character"), c("length", 1)),
        drv_pack   = list(c("mode", "character"), c("length", 1)),
        drv        = list(c("mode", "character"), c("length", 1)),
        conn_class = list(c("mode", "character"), c("length", 1)),
        conn_name  = list(c("mode", "character"), c("length", 1)),
        is_local   = list(c("mode", "logical"), c("length", 1)),
        rm_objects = list(c("mode", "logical"), c("length", 1)),
        reconnect  = list(c("mode", "logical"), c("length", 1)),
        disconnect = list(c("mode", "logical"), c("length", 1))
      ),
      from_fn    = "manage_connection"
    )
    stopifnot(arg_check$valid)
  }
  require(drv_pack, character.only = TRUE)
  logger           <- "logger" %in% (.packages())
  global_env       <- as.list(.GlobalEnv)
  global_env_names <- names(global_env)
  connection_object_classes <- sprintf("%sConnection", conn_class)
  for (env in global_env_names) {
    connected      <- FALSE
    this_obj       <- global_env[[env]]
    this_obj_class <- class(this_obj)
    if (any(grepl(conn_class, this_obj_class))) {
      if (any(grepl(connection_object_classes, this_obj_class))) {
        connection <- try(this_obj@dbname)
      } else if (any(grepl(sprintf("^tbl_%s$", conn_class), this_obj_class))) {
        connection <- try(this_obj$src$con@dbname)
      } else if (any(grepl(sprintf("^%sResult$", drv), this_obj_class))) {
        connection <- try(this_obj@conn@dbname)
        dbClearResult(this_obj)
      } else if (class(this_obj) == conn_class) {
        connection <- "other"
      } else {
        connection <- ""
      }
      if (class(connection) == "try-error") {
        if (logger) log_warn('Could not automatically identify the connection properties for object "{env}". To avoid hanging connections, it should be removed explicitly.')
        connection <- ""
      }
      if (
        any(
          all(basename(connection) == db,
              this_obj_class %in% connection_object_classes),
          class(this_obj) == conn_class
        )
      ){
        connected <- dbIsValid(this_obj)
        log_it("trace", glue('Database "{db}" is currently open.'))
        if (all(connected, disconnect)) {
          check <- try(invisible(dbDisconnect(this_obj)))
          status <- ifelse(class(check)[1] == "try-error",
                           "unable to be disconnected",
                           "disconnected")
          log_it("trace", glue('Database "{db}" {status}.'))
          connected <- dbIsValid(this_obj)
        }
      }
      log_it("trace", glue('Closing and removing "{env}"...'))
      if (all(rm_objects, disconnect, !connection == "")) {
        rm(list = env, pos = ".GlobalEnv")
      }
    }
  }
  if (reconnect) {
    if (is_local) {
      # Resolve database file location
      log_it("trace", glue('Finding local database file "{db}"'))
      db_where  <- list.files(pattern = db, full.names = TRUE, recursive = TRUE)
      if (length(db_where) == 0) {
        stop(sprintf('Unable to locate "%s".', db))
      }
    }
    if (conn_name %in% global_env_names) {
      if (connected) dbDisconnect(sym(conn_name))
      rm(list = eval(sym(conn_name)), pos = ".GlobalEnv")
    }
    args <- list(db, ...)
    assign(x     = conn_name,
           value = try(do.call("dbConnect", args = c(drv = do.call(drv, list()), args))),
           envir = .GlobalEnv)
    if (class(eval(sym(conn_name))) == "try-error") {
      stop(sprintf('Unable to connect to "%s".\nCall attempted was "assign(x = %s, value = try(do.call("dbConnect", args = c(drv = do.call(%s, list()), %s)))',
                   db,
                   drv,
                   paste0(args, collapse = ", ")))
    } else {
      if (dbIsValid(eval(sym(conn_name)))) log_it("trace", glue('"{db}" connected as "{conn_name}".'))
    }
  }
}

#' Parse SQL build statements
#'
#' Reading SQL files directly into R can be problematic. This function is
#' primarily called in [create_fallback_build].
#'
#' All arguments `magicsplit`, `header`, and `section` provide flexibility in
#' the comment structure of the SQL file and accept regex for character matching
#' purposes.
#'
#' @param sql_statements CHR vector of SQL build statements from an SQL file.
#' @param magicsplit CHR scalar regex indicating some "magic" split point SQL
#'   comment to simplify the identification of discrete commands; will be used
#'   to split results (optional but highly recommended)
#' @param header CHR scalar regex indicating the format of header comments SQL
#'   comment to remove (optional)
#' @param section CHR scalar regex indicating the format of section comments SQL
#'   comment to remove (optional)
#'
#' @return LIST of parsed complete build commands as CHR vectors containing each
#'   line.
#' @export
#'
#' @examples
#' example_file <- "./config/sql_nodes/reference.sql"
#' if (file.exists(example_file)) {
#'   build_commands <- readr::read_file(example_file)
#'   sqlite_parse_build(build_commands)
#' }
sqlite_parse_build <- function(sql_statements,
                               magicsplit = "/\\*magicsplit\\*/",
                               header     = "/\\*\\=+\\r*\\n[[:print:][:cntrl:]]+\\r*\\n\\=+\\*/",
                               section    = "/\\* -* [[:print:][:cntrl:]]+ -* \\*/") {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sql_statements = list(c("mode", "character"), c("length", 1)),
        magicsplit     = list(c("mode", "character"), c("length", 1)),
        header         = list(c("mode", "character"), c("length", 1)),
        section        = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "sqlite_parse_build"
    )
    stopifnot(arg_check$valid)
  }
  log_it("trace", glue('Parsing \n{sql_statements}\n\tfor build commands.'))
  to_remove <- paste0(c(header, "\\t", "\\r"), collapse = "|")
  out        <- sql_statements %>%
    str_replace_all("CREATE ", paste0(magicsplit, "CREATE ")) %>%
    str_replace_all("\\.import ", paste0(magicsplit, ".import ")) %>%
    str_replace_all("DELETE FROM ", paste0(magicsplit, "DELETE FROM ")) %>%
    str_replace_all("\\;\\nINSERT ", paste0("; ", magicsplit, "\nINSERT ")) %>%
    str_split(magicsplit) %>%
    .[[1]] %>%
    str_remove_all(to_remove) %>%
    str_replace_all("\\n{2,10}", "\n") %>%
    str_replace_all(" {2,10}", " ") %>%
    str_split("\\n") %>%
    lapply(str_squish) %>%
    lapply(function(x) x[! x %in% c("", " ")])
  out        <- out[unlist(lapply(out, length)) > 0]
  return(out)
}

#' Parse SQL import statements
#'
#' In the absence of the sqlite command line interface (CLI), the [build_db]
#' process needs a full set of SQL statements to execute directly rather than
#' CLI dot commands. This utility function parses formatted SQL statements
#' containing CLI ".import" commands to create SQL INSERT statements. This
#' function is primarily called in [create_fallback_build].
#'
#' @param build_statements CHR vector of SQL build statements from an SQL file.
#'
#' @return LIST of parsed .import statements as full "INSERT" statements.
#' @export
#'
#' @examples
#' if (file.exists("./config/data/elements.csv")) {
#'   sqlite_parse_import(".import --csv --skip 1 ./config/data/elements.csv elements")
#' }
sqlite_parse_import <- function(build_statements) {
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        build_statements = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "sqlite_parse_import"
    )
    stopifnot(arg_check$valid)
  }
  regex_import  <- ".import (--[[:alnum:]]+)? "
  regex_skip    <- "(--skip [[:number:]]+) ?"
  regex_prefix  <- paste0(c(regex_import, regex_skip), collapse = "?")
  log_it("trace", glue('Parsing \n{build_statements}\n for import modification.'))
  out           <- lapply(build_statements,
                          function(x) {
                            if (grepl("\\.import", x)) {
                              data_type <- str_extract(x, regex_import) %>%
                                str_remove(".import( --)?") %>%
                                str_squish()
                              if (data_type == "") {
                                data_type_read <- FALSE
                                log_it("warn", 'No data type identified. Assuming a ".csv" extension.')
                                data_type <- "csv"
                              } else {
                                data_type_read <- TRUE
                              }
                              skip_rows <- str_extract(x, regex_skip) %>%
                                str_remove("--skip ") %>%
                                str_squish()
                              skip_rows <- try(as.numeric(skip_rows))
                              if (class(skip_rows) == 'try-error') {
                                log_it("warn", glue('Could not convert "{skip_rows}" to numeric in "{x}".'))
                                return(x)
                              }
                              tmp <- str_remove(x, regex_prefix) %>%
                                str_split(" ") %>%
                                .[[1]]
                              if (length(tmp) != 2) {
                                log_it("warn", glue('Could not parse "{x}" to identify both a target file and database target table.'))
                                return(x)
                              }
                              data_file <- tmp[1]
                              if (file.exists(data_file)) {
                                if ("readr" %in% installed.packages()) {
                                  require(readr)
                                  read_func <- sprintf("read_%s", data_type)
                                } else {
                                  read_func <- sprintf("read.%s", data_type)
                                }
                                if (exists(read_func)) {
                                  read_data <- try(
                                    suppressMessages(
                                      do.call(read_func,
                                              list(data_file))
                                    )
                                  )
                                  if ("try-error" %in% class(read_data)) {
                                    msg <- sprintf('Error reading file "%s" with function "%s".\n%s
                                                   Read command was "%s"; make sure file extensions match the file format and try again.',
                                                   data_file,
                                                   read_func,
                                                   ifelse(data_type_read,
                                                          "",
                                                          sprintf('Data type was assumed to be "%s".\n',
                                                                  data_type)),
                                                   x
                                    )
                                    log_it("warn", msg)
                                  }
                                } else {
                                  log_it("warn", glue('Cannot read file "{data_file}". Function "{read_func}" is not available.'))
                                  return(x)
                                }
                              } else {
                                log_it("warn", glue('Could not find file "{data_file}".'))
                                return(x)
                              }
                              target    <- tmp[2]
                              insert_values <- sprintf('("%s")',
                                                       read_data %>%
                                                         unite(col = "statement", sep = '", "') %>%
                                                         pull(statement) %>%
                                                         paste0(collapse = '"), ("')
                              )
                              insert_statement <- sprintf("INSERT INTO `%s` VALUES %s;",
                                                          target,
                                                          insert_values)
                              return(insert_statement)
                            } else {
                              return(x)
                            }
                          })
  return(out)
}

#' Create an SQL file for use without the SQLite CLI
#'
#' For cases where the SQLite Command Line Interface is not available, dot
#' commands used to simplify the database build pipeline are not usable. Call
#' this function to create a self-contained SQL build file that can be used in
#' [build_db] to build the database. The self-contained file will include all
#' "CREATE" and "INSERT" statements necessary by parsing lines including ".read"
#' and ".import" commands and directly reading referenced files.
#'
#' @param build_file CHR scalar name SQL build file to use. The default, NULL,
#'   will use the environment variable "DB_BUILD_FILE" if it is available.
#' @param populate LGL scalar of whether to populate data (default: TRUE)
#' @param populate_with CHR scalar name SQL population file to use. The default,
#'   NULL, will use the environment variable "DB_DATA" if it is available.
#' @param driver CHR scalar of the database driver class to use to correctly
#'   interpolate SQL commands (default: "SQLite")
#' @param out_file CHR scalar of the output file name and destination. The
#'   default, NULL, will write to a file named similarly to `build_file`
#'   suffixed with "_full".
#'
#' @return None: a file will be written at `out_file` with the output.
#' @export
#'
#' @examples
create_fallback_build <- function(build_file    = NULL,
                                  populate      = TRUE,
                                  populate_with = NULL,
                                  driver        = "SQLite",
                                  out_file      = NULL) {
  if (all(is.null(build_file), exists("DB_BUILD_FILE"))) build_file <- DB_BUILD_FILE
  if (all(is.null(populate_with), exists("DB_DATA"))) populate_with <- DB_DATA
  build_files <- list.files(pattern = build_file,
                            full.names = TRUE,
                            recursive = TRUE)
  if (length(build_files) > 1) {
    if (interactive()) {
      build_file <- resolve_multiple_values(build_files, build_file)
    } else {
      stop(msg)
    }
  } else if (length(build_files) == 1) {
    build_file <- build_files
  }
  if (is.null(out_file)) {
    out_file <- gsub(".sql", "_full.sql", build_file)
  } else {
    out_file <- out_file
  }
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = list(build_file, populate, populate_with, driver, out_file),
      conditions = list(
        build_file    = list(c("mode", "character"), c("length", 1)),
        populate      = list(c("mode", "logical"), c("length", 1)),
        populate_with = list(c("mode", "character"), c("length", 1)),
        driver        = list(c("mode", "character"), c("length", 1)),
        out_file      = list(c("mode", "character"), c("length", 1))
      ),
      from_fn    = "create_fallback_build"
    )
    stopifnot(arg_check$valid)
  }
  comments   <- "^/\\* [[:alnum:]+ \\.\\',/_]+ \\*/$"
  has_import <- FALSE
  if (!file.exists(build_file)) {
    stop(sprintf('Could not locate file "%s".', build_file))
  }
  build   <- read_file(build_file) %>%
    sqlite_parse_build()
  
  if (populate) {
    populate_file <- list.files(pattern = populate_with,
                                full.names = TRUE,
                                recursive = TRUE)
    populate <- read_file(populate_file) %>%
      sqlite_parse_build()
    build <- c(build, populate)
  }
  # Make SQL build statements
  build   <- unlist(build) %>%
    as.list()
  index_read   <- grep("^.read", build)
  temp_read    <- lapply(build[index_read],
                         function(x) {
                           node_file <- str_remove(x, ".read ")
                           read_file(node_file) %>%
                             sqlite_parse_build()
                         })
  source_read  <- str_remove(unlist(build[index_read]), ".read ")
  names(temp_read) <- source_read %>%
    basename() %>%
    file_path_sans_ext()
  for (i in 1:length(temp_read)) {
    source_comment <- sprintf("/* Sourced from ./%s */", source_read[i])
    temp_read[[i]] <- c(source_comment, temp_read[[i]])
  }
  build[index_read] <- temp_read %>%
    lapply(function(x) {
      lapply(x, function(y) {
        paste0(y, collapse = " ")
      })
    })
  build <- unlist(build) %>% as.list()
  index_import <- grep("^.import", build)
  temp_import  <- lapply(build[index_import],
                         function(x) {
                           if (grepl(comments, x)) return(x)
                           lapply(x, sqlite_parse_import)
                         }
  ) %>% unlist()
  check <- build
  build[index_import] <- temp_import
  build <- unlist(build) %>%
    paste0(collapse = "\n")
  write_file(build, out_file)
  log_it("info", sprintf('Fallback build file created as "%s".',
                         out_file))
}

# TODO early sketch for construction of automatic logging triggers
build_db_logging_triggers <- function(db = DB_NAME, connection = "con", log_table_name = "log") {
  # According to the current environment setup, exclude logging to save space
  if (!exists("DB_LOGGING")) {
    DB_LOGGING <- FALSE
  }
  if (DB_LOGGING) {
    # Require a connection object
    require(glue)
    require(magrittr)
    if (!exists(connection)) {
      con <- dbConnect(RSQLite::SQLite(), db)
    }
    tables  <- dbListTables(con)
    tables  <- tables[!tables == log_table_name]
    log_cols <- dbListFields(con, log_table_name)
    out <- character(0)
    for (table in tables) {
      table_cols <- dbGetQuery(con, glue("PRAGMA table_info({table})")) %>%
        filter(pk != 1, name != "id") %>%
        pull(name) %>%
        paste0(collapse = ", ")
      out <- c(out,
               glue(
                 "CREATE TRIGGER {table}_track_insert",
                 "  AFTER INSERT ON {table}",
                 "  BEGIN",
                 "    INSERT INTO {log_table_name} (affect_type, affects_table, affects_ids, executed_by)",
                 "    VALUES ('INSERT', '{table}', NEW.id);",
                 "  END;"
               )
      )
    }
  }
}

#' Convenience function to rebuild all database related files
#'
#' @note This requires references to be in place to the individual functions in
#'   the current environment.
#'
#' @return Files for the new database, fallback build, and data dictionary will
#'   be created in the project directory and objects will be created in the
#'   global environment for the database map (LIST "db_map") and current
#'   dictionary (LIST "db_dict")
#' @export
#'
#' @examples
update_all <- function() {
  manage_connection(reconnect = FALSE)
  build_db()
  manage_connection()
  create_fallback_build()
  save_data_dictionary()
  db_map <<- er_map()
  db_dict <<- jsonlite::read_json(
    list.files(pattern = "data_dictionary.json", full.names = TRUE)
  ) %>%
    lapply(bind_rows)
}

# Database utility functions ---------------------------------------------------

#' Add value(s) to a normalization table
#'
#' One of the most common database operations is to look up or add a value in a
#' normalization table. This utility function adds a single value and returns
#' its associated id by using [build_db_action]. This is only suitable for a
#' single value. If you need to bulk add multiple new values, use this with
#' something like [lapply].
#'
#' @param db_table CHR scalar of the normalization table's name
#' @param ... LIST values to add to the table. All values must be named.
#' @param conn connection object (default "con")
#'
#' @return NULL if unable to add the values, INT scalar of the new ID otherwise
#' @export
#'
#' @examples
add_normalization_value <- function(db_table, ..., conn = con) {
  new_values <- list(...)
  if (length(names(new_values)) != length(new_values)) {
    stop("All values provided to this function must be named.")
  }
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        new_values  = list(c("mode", "list"), c("n>=", 1)),
        db_table    = list(c("mode", "character"), c("length", 1)),
        conn        = list(c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  needed <- dbListFields(conn, db_table)
  needed <- needed[needed != "id"]
  new_values <- new_values[which(names(new_values) %in% needed)]
  if (!all(needed %in% names(new_values))) {
    msg <- sprintf('Not all values needed for table "%s" were supplied.', db_table)
    if (interactive()) {
      log_it("warn", paste0(msg, " Please provide values to continue."))
      for (need_this in needed) {
        if (!need_this %in% names(new_values)) {
          if (all(need_this == "acronym", "name" %in% names(new_values))) {
            auto_acro <- make_acronym(new_values$name)
            use_auto_acro <- select.list(
              choices = c("Yes", "No"),
              title = sprintf('\nUse the automatic acronym "%s" for normalization name "%s"?',
                              auto_acro,
                              new_values$name)
            )
            if (use_auto_acro == "Yes") {
              new_value <- auto_acro
            } else {
              new_value <- readline(
                sprintf("%s: ", need_this)
              )
            }
          } else {
            new_value <- readline(
              sprintf("%s: ", need_this)
            )
          }
          new_values <- c(
            new_values,
            setNames(new_value, need_this)
          )
        }
      }
    } else {
      log_it("error", msg)
      return(NULL)
    }
  }
  log_it("trace", sprintf('Addding normalization values to table "%s"', db_table))
  res <- try(
    build_db_action("insert", db_table, values = list(new_values))
  )
  if (class(res) == "try-error") {
    log_it(
      "warn",
      sprintf(
        'Unable to add normalization values (%s) to table "%s": %s',
        lapply(names(new_values),
               function(x) {
                 sprintf('%s = "%s"', x, new_values[[x]])
               }) %>%
          paste0(collapse = ", "),
        db_table,
        str_remove_all(res[[1]], "^[[:alpha:]]* : |\n")
      )
    )
  }
  this_id <- build_db_action("get_id", db_table, match_criteria = new_values, and_or = "AND")
  return(this_id)
}

#' Check for a value in a database table
#'
#' This convenience function simply checks whether a value exists in the
#' distinct values of a given column. Only one column may be searched at a time;
#' serialize it in other code to check multiple columns. It leverages the
#' flexibility of [build_db_action] to do the searching. The `values` parameter
#' will be fed directly and can accept the nested list structure defined in
#' [clause_where] for exclusions and like clauses.
#'
#' @param values CHR vector of the values to search
#' @param db_table CHR scalar of the database table to search
#' @param db_column CHR scalar of the column to search
#' @param case_sensitive LGL scalar of whether to match on a case sensitive
#'   basis (the default TRUE searches for values as-provided) or whether to
#'   coerce value matches by upper, lower, sentence, and title case matches
#' @param conn connection object (default: con)
#'
#' @return LIST of length 1-2 containing "exists" as a LGL scalar for whether
#'   the values were found, and "values" containing the result of the database
#'   call, a data.frame object containing matching rows or NULL if exists ==
#'   FALSE.
#' @export
#'
#' @examples
#'
#' ## Not run:
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' alphabet <- dplyr::tibble(lower = letters, upper = LETTERS)
#' dplyr::copy_to(con, alphabet)
#' check_for_value("A", "alphabet", "upper", con)
#' check_for_value("A", "alphabet", "lower", con)
#' check_for_value(letters[1:10], "alphabet", "lower", con)
#'
#' ## End(Not run)
check_for_value <- function(values, db_table, db_column, case_sensitive = TRUE, conn = con) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values         = list(c("mode", "character"), c("n>=", 1)),
        db_table       = list(c("mode", "character"), c("length", 1)),
        db_column      = list(c("mode", "character"), c("length", 1)),
        case_sensitive = list(c("mode", "logical"), c("length", 1)),
        conn           = list(c("length", 1))
      ),
      from_fn = "check_for_value"
    )
    stopifnot(arg_check$valid)
  }
  existing_values <- build_db_action(con            = conn,
                                     action         = "SELECT",
                                     table_name     = db_table,
                                     match_criteria = setNames(list(values), db_column),
                                     case_sensitive = case_sensitive,
                                     distinct       = TRUE)
  found <- nrow(existing_values) > 0
  out   <- list(
    exists = found,
    values = if (found) existing_values else NULL
  )
  return(out)
}

#' Utility function to resolve multiple choices interactively
#'
#' @param values CHR vector of possible values
#' @param search_value CHR scalar of the value to search
#' @param db_table CHR scalar of the table (for printing purposes only)
#'   (default: "")
#'
#' @return CHR scalar result of the user's choice
#' @export
#'
#' @examples
resolve_multiple_values <- function(values, search_value, db_table = "") {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        values       = list(c("n>=", 1)),
        search_value = list(c("length", 1)),
        db_table     = list(c("mode", "character"), c("length", 1))
      ),
      from_fn = "resolve_multiple_values"
    )
    stopifnot(arg_check$valid)
  }
  present  <- any(grepl(search_value, values))
  only_one <- sum(values %in% search_value) == 1
  if (present && only_one) {
    chosen_value <- search_value
  } else {
    if (length(values) > 1) {
      values <- c("(Abort)", sprintf("(New) %s", search_value), values)
      msg <- sprintf('\n%s values directly matching "%s" were identified%s.',
                     ifelse(all(present, !only_one), "Multiple", "No"),
                     search_value,
                     ifelse(db_table == "",
                            "",
                            sprintf(' in table "%s"', db_table))
      )
      if (interactive()) {
        chosen_value <- select.list(values, title = paste(msg, "Please select one."))
      } else {
        stop(msg)
      }
    }
  }
  return(chosen_value)
}

resolve_normalization_value <- function(this_value, db_table, case_sensitive = FALSE, conn = con, ...) {
  fields <- dbListFields(conn = conn, db_table)
  this_id <- integer(0)
  for (field in fields) {
    if (length(this_id) == 0) {
      tmp <- check_for_value(this_value, db_table, field, case_sensitive, conn)
      if (tmp$exists) {
        this_id <- tmp$values$id
      }
    }
  }
  if (length(this_id) == 1) {
    return(this_id)
  } else {
    if (interactive()) {
      if (length(this_id) == 0) {
        tmp$values <- build_db_action("select", db_table)
      }
      if ("id" %in% names(tmp$values)) {
        tmp$values <- tmp$values %>%
          select(-id)
      } else {
        tmp$values <- tmp$values[, -1]
      }
      these_choices <- tmp$values %>%
        unite(choices, everything(), sep = " - ") %>%
        pull(choices)
      if (length(these_choices) == 0) {
        this_selection <- paste("(New)", this_value)
      } else {
        this_selection <- resolve_multiple_values(these_choices, this_value, db_table = db_table)
      }
      if (str_detect(this_selection, "\\(New\\) ")) {
        this_selection <- str_remove(this_selection, "\\(New\\) ")
        to_add <- c(
          db_table = db_table,
          name = this_selection,
          list(...),
          conn = conn
        )
        this_id <- do.call(add_normalization_value, to_add)
      } else {
        this_id <- build_db_action("get_id", db_table, match_criteria = to_add)
      }
      return(this_id)
    } else {
      log_it("error", glue('Multiple entries in "{db_table}" match value "{this_value}'))
      return(NULL)
    }
  }
}

ref_table_from_map <- function(table_name, table_column, this_map = db_map, fk_refs_in = "references") {
  log_it("trace", sprintf('Getting normalization table reference for column  "%s" in table "%s"', table_column, table_name) )
  refs <- this_map[[table_name]][[fk_refs_in]]
  table_column <- sprintf("^%s", table_column)
  refers_to <- grep(table_column, refs, value = TRUE)
  if (length(refers_to) == 1) {
    refers_to <- refers_to %>%
      str_remove_all("^[[:alpha:]-_]* REFERENCES |\\([[:alpha:]]*\\)$") %>%
      str_trim()
  }
  return(refers_to)
}

# Operation specific functions -------------------------------------------------

#' Add a contributor programmatically or interactively. If values are fed to all
#' parameters then the user will be added automatically.
#'
#' @param user_value CHR scalar primarily used for programmatic use of this
#'   function, indicating the original value provided by a user interface (e.g.
#'   shiny or from another function). This is used only to provide an
#'   interactive prompt to show the user what value was initially fed into this
#'   function. (default "")
#' @param username CHR scalar of the desired username (default "")
#' @param contact CHR scalar of the contributor's contact point (default "")
#' @param first_name CHR scalar of the contributor's first name (default "")
#' @param last_name CHR scalar of the contributor's last name (default "")
#' @param affiliation CHR scalar of the contributor's affiliation (default "")
#' @param orcid CHR scalar of the contributor's ORCID (default ""), which must
#'   match the valid ORCID pattern of four sets of four alphanumeric characters
#'   separated by dashes (e.g. "1111-2222-3333-4444")
#' @param conn connection object (default: con)
#'
#' @return
#' @export
#'
#' @examples
add_contributor <- function(user_value  = "",
                            username    = "",
                            contact     = "",
                            first_name  = "",
                            last_name   = "",
                            affiliation = "",
                            orcid       = "",
                            conn        = con) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        user_value  = list(c("mode", "character"), c("length", 1)),
        username    = list(c("mode", "character"), c("length", 1)),
        contact     = list(c("mode", "character"), c("length", 1)),
        first_name  = list(c("mode", "character"), c("length", 1)),
        last_name   = list(c("mode", "character"), c("length", 1)),
        affiliation = list(c("mode", "character"), c("length", 1)),
        orcid       = list(c("mode", "character"), c("length", 1)),
        conn        = list(c("length", 1))
      ),
      from_fn = "add_contributor"
    )
    stopifnot(arg_check$valid)
  }
  if (interactive()) {
    cat(
      sprintf(
        'Would you like to add a user now?%s\n',
        ifelse(user_value == "",
               "",
               sprintf(' The value provided was "%s".',
                       user_value)
        )
      )
    )
    if (menu(c("Yes", "No")) != 1) {
      cat("You can always add a user later.\n")
      return(NULL)
    }
    while (username == "") username <- readline("Username (required): ")
    while (check_for_value(username, "contributors", "username")$exists) {
      cat("That username is already taken. Please select another.")
      username <- readline("Username (required): ")
    }
    while (contact == "") {
      contact     <- readline("Contact (email preferred, required): ")
      if (str_detect(contact, ";|delete|select|alter|drop|update")) {
        cat("Contact information may not include semicolons or database verbs (e.g. select, alter, etc.).")
        contact   <- ""
      }
    }
    while (first_name == "")  first_name  <- readline("First Name (required): ")
    while (last_name == "")   last_name   <- readline("Last Name (required): ")
    while (affiliation == "") affiliation <- readline("Affiliation (required): ")
    this_affiliation <- check_for_value(affiliation, "affiliations", "name", case_sensitive = FALSE)
    if (this_affiliation$exists) {
      affiliation <- this_affiliation$values$id
    }
    if (orcid == "")       orcid       <- readline("ORCID (0000-0000-0000-000X) (optional but strongly encouraged): ")
    while (all(!grepl("^([0-9]{4}-){3}[0-9]{3}[0-9X]$", orcid),
               nchar(orcid) > 1)) {
      log_it("error", 'Valid ORCIDs must be of the pattern "0000-0000-0000-000X" where "0" is a number and "X" may be a number or the character "X" (upper case only). Leave blank to skip.')
      orcid <- readline('ORCID (0000-0000-0000-000X) (optional but strongly encouraged): ')
    }
  } else {
    unfilled <- c(
      username    = username,
      contact     = contact,
      first_name  = first_name,
      last_name   = last_name,
      affiliation = affiliation
    )
    if (any(unfilled == "")) {
      log_it(
        "error",
        sprintf('Values must be provided for all required fields (%s were not provided).',
                format_list_of_names(names(unfilled)[unfilled == ""])
        )
      )
      return(NULL)
    }
  }
  if (
    any(
      is.null(orcid),
      is.na(orcid),
      orcid %in% c("", "null", "NULL", "NA")
    )
  ) {orcid <- "null"}
  build_db_action(
    action     = "INSERT",
    table_name = "contributors",
    conn       = conn,
    values     = c(
      username    = username,
      contact     = contact,
      first_name  = first_name,
      last_name   = last_name,
      affiliation = affiliation,
      orcid       = orcid
    )
  )
  user_id <- build_db_action(
    action         = "GET_ID",
    table_name     = "contributors",
    conn           = conn,
    match_criteria = list(username = username)
  )
  return(user_id)
}

#' Verify a sample class exists, and add it if needed
#'
#' This utility function leverages [add_normalization_value] for the
#' "norm_sample_classes" table to verify and add, if necessary, a sample class
#' name or ID. If an integer is provided, it is verified against existing IDs.
#' If a character scalar is provided it is checked against the sample class
#' names and the associated ID number is returned.
#'
#' If used in an interactive mode, the user has the option of selecting an
#' existing sample class, aborting, or creating a new entry with the value of
#' parameter `sample_class`.
#'
#' If used non-interactively, new classes can be added automatically by setting
#' `auto_add` to TRUE and using the associated sample class ID downstream. Note
#' this may cause disconnections if typos are introduced (e.g. "plasma" vs
#' "plamsa").
#'
#' @param sample_class CHR or NUM scalar of the sample class name or ID
#' @param conn connection object (default "con")
#' @param auto_add LGL scalar of whether to automatically add an entry for
#'   `sample_class` as provided if it does not exist (use with caution: default
#'   FALSE)
#'
#' @return NULL if no action is taken, or the new sample class ID
#' @export
#'
#' @examples
verify_sample_class <- function(sample_class, conn = con, auto_add = FALSE) {
  # Argument validation relies on verify_args
  if (exists("verify_args")) {
    arg_check <- verify_args(
      args       = as.list(environment()),
      conditions = list(
        sample_class = list(c("length", 1)),
        conn         = list(c("length", 1)),
        auto_add     = list(c("mode", "logical"), c("length", 1))
      )
    )
    if (!arg_check$valid) {
      stop(cat(paste0(arg_check$messages, collapse = "\n")))
    }
  }
  sample_classes <- check_for_value(sample_class, "norm_sample_classes", "name", case_sensitive = FALSE)
  if (sample_classes$exists) {
    sample_class <- sample_classes$values$id
    sample_classes <- sample_classes$values
  } else {
    # sample_class <- str_to_lower(sample_class)
    sample_classes <- tbl(conn, "norm_sample_classes") %>% collect()
  }
  if (is.numeric(sample_class)) {
    if (sample_class %in% sample_classes$id) {
      sample_class_id <- sample_class
      log_it("trace", sprintf('Sample class "%s" identified from provided integer "%s".',
                              sample_classes$name[sample_classes$id == sample_class],
                              sample_class))
    } else {
      sample_class_id <- NULL
      log_it("warn", sprintf('No sample class with ID = "%s" currently exists.',
                             sample_class))
    }
  } else if (is.character(sample_class)) {
    if (sample_class %in% sample_classes$name) {
      sample_class_id <- sample_classes$id[sample_classes$name == sample_class]
      log_it("trace", sprintf('Sample class id "%s" identified from direct name match to "%s".',
                              sample_class_id,
                              sample_class))
    } else {
      sample_class_id <- NULL
      log_it("warn", sprintf('No sample class with name "%s" currently exists.',
                             sample_class))
      if (interactive()) {
        new_flag <- "\\(New\\) "
        log_it("trace", 'Cannot automatically identify a sample class. Selecting a sample class interactively.')
        create_it  <- resolve_multiple_values(sample_classes$name, sample_class)
        if (str_detect(create_it, new_flag)) {
          create_it <- str_remove(create_it, new_flag)
          if (create_it != tolower(create_it)) {
            enter_as_lower <- select.list(title = "Standardize spelling to lower case?", choices = c("Yes", "No"))
            if (enter_as_lower == "Yes") {
              create_it <- str_to_lower(create_it)
            }
          }
          sample_class_id <- try(
            add_normalization_value("norm_sample_classes", name = create_it)
          )
          if (class(sample_class_id) == "try-error") {
            sample_class_id <- NULL
            log_it("error",
                   sprintf('Error adding normalization value "%s" to table "norm_sample_classes".',
                           create_it)
            )
          } else {
            log_it("success",
                   sprintf('Added "%s" as a normalization option to table "norm_sample_classes"',
                           create_it)
            )
          }
        } else if (create_it == "(Abort)") {
          sample_class_id <- NULL
          log_it("info", 'Sample class selection aborted.')
        } else {
          sample_class_id <- sample_classes$id[sample_classes$name == sample_class]
        }
      } else {
        if (auto_add) {
          sample_class_id <- try(add_normalization_value("norm_sample_classes", sample_class))
          if (class(sample_class_id) == "try-error") {
            sample_class_id <- NULL
            log_it("error",
                   sprintf('Error adding normalization value "%s" to table "norm_sample_classes".',
                           sample_class)
            )
          } else {
            log_it("success",
                   sprintf('Added "%s" as a normalization option to table "norm_sample_classes"',
                           sample_class)
            )
          }
        } else {
          sample_class_id <- NULL
          log_it("info", 'No new sample class added because "auto_add" is FALSE.')
        }
      }
    }
  } else {
    log_it("error", sprintf('Only character or numeric types are accepted for parameter "sample_class".'))
    sample_class_id <- NULL
  }
  return(sample_class_id)
}

#' Title
#'
#' @param contributor_text 
#' @param conn 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
verify_contributor <- function(contributor_text, conn = con, ...) {
  # Check contributor - do not check for internal id number
  # Prefer username
  db_contributors  <- tbl(conn, "contributors")
  contributor_properties <- c(list(...), user_value = contributor_text)
  possible_matches <- db_contributors %>%
    filter(username %in% contributor_text |
             contact %in% contributor_text |
             first_name %in% contributor_text |
             last_name %in% contributor_text
    ) %>%
    collect()
  contributor_exists <- nrow(possible_matches) > 0
  if (!contributor_exists) {
    log_it("warn", sprintf('No contributor matching "%s" was located.', contributor_text))
    if (interactive()) {
      log_it("trace", "Getting user information from an interactive session.")
      new_user <- do.call(add_contributor, contributor_properties)
      if (is.null(new_user)) stop("Could not verify this user.")
      return(new_user)
    } else {
      needed <- dbListFields(conn, "contributors")
      needed <- needed[needed != "id"]
      properties_present <- needed %in% names(contributor_properties)
      if (all(properties_present)) {
        do.call(add_contributor, contributor_properties)
      } else {
        stop(sprintf("Unable to automatically add contributor. Please provide contributor properties for %s as arguments to this function.",
                     format_list_of_names(needed[!properties_present])))
      }
    }
  } else {
    if (nrow(possible_matches) == 1) {
      return(possible_matches$id[1])
    } else {
      if (interactive()) {
        possibles <- possible_matches %>%
          left_join(tbl(conn, "affiliations") %>% collect(),
                    by = c("affiliation" = "id")) %>%
          select(-affiliation) %>%
          rename("affiliation" = "name") %>%
          mutate(Contributor = sprintf("%s %s at %s from %s",
                                       first_name,
                                       last_name,
                                       contact,
                                       affiliation))
        log_it("info", "Multiple contributors found.")
        verified_match <- resolve_multiple_values(possibles$Contributor, contributor_text)
        if (str_detect(verified_match, "\\(New\\) ")) {
          # cat("New contributors must be added manually at this time.")
          make_new <- select.list(choices = c("Yes", "No"), title = "Create a new user now?")
          if (make_new == "Yes") {
            id <- add_contributor(db_contributors, user_value = contributor_text)
            return(id)
          } else {
            return("Create a contributor manually to proceed.")
          }
        } else {
          return(possible_matches$id[which(possibles$Contributor == verified_match)])
        }
      } else {
        log_it("trace", "Non interactive session and multiple matches identified.")
        stop("Unable to discretely identify a contributor. Please try again.")
      }
    }
  }
}

# TODO - abstract the appropriate ms_n - 1 scan to associate with ms_data
associated_scan <- function(df, scan_time) {
  scan_msn  <- df$msn[which(df$scantime == scan_time)] - 1
  df$scantime[
    which(all(df$scantime < scan_time,
              df$msn == scan_msn))
  ]
}
