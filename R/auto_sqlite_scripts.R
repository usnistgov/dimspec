#' Build pairs of INSERT/UPDATE triggers to resolve foreign key relationships
#'
#' When building schema by script, it is often handy to enforce certain
#' behaviors on database transactions involving foreign keys, especially in
#' SQLite. Given a properly structured list object describing the mappings
#' between tables in a schema (e.g. one deriving from [er_map]), this function
#' will parse those for foreign key relationships.
#'
#' Primarily, this requires a list object referring to tables that contains in
#' each element a child element with the name provided in `references_in`. The
#' pre-pass parsing function [get_fkpk_relationships] is used to pull references
#' from
#' the full map is used.
#'
#' @note Tables in `db_map` that do not contain foreign key relationships will
#'   be dropped from the output list.
#'
#' @note This is largely a convenience function to programmatically apply
#'   [make_sql_triggers] to an entire schema. To skip tables with defined
#'   foreign key relationships for which triggers are undesirable, remove those
#'   tables from `db_map` prior to calling this function.
#'
#' @inheritParams get_fkpk_relationships
#'
#' @param create_insert_trigger LGL scalar indicating whether to build an insert
#'   trigger for each table (default: TRUE).
#' @param create_update_trigger LGL scalar indicating whether to build an update
#'   trigger for each table (default: FALSE).
#' @param save_to_file CHR scalar of a file in which to write the output, if any
#'   (default: NULL will return the resulting object to the R session)
#'
#' @return LIST object containing one element for each table in `db_map`
#'   containing foreign key references, with one child
#' @export
#'
#' @usage
#' build_triggers(er_map(db_conn = con))
#' 
build_triggers <- function(db_map, references_in = "references", create_insert_trigger = TRUE, create_update_trigger = TRUE, save_to_file = NULL, append = TRUE) {
  stopifnot(is.list(db_map),
            is.character(references_in),
            length(references_in) == 1,
            is.logical(create_insert_trigger),
            length(create_insert_trigger) == 1,
            is.logical(create_update_trigger),
            length(create_update_trigger) == 1,
            is.logical(append),
            length(append) == 1,
            any(is.null(save_to_file),
                is.character(save_to_file) & length(save_to_file) == 1))
  fk_relationships <- get_fkpk_relationships(db_map, references_in)
  actions <- c(ifelse(create_insert_trigger, "insert", NA),
               ifelse(create_update_trigger, "update", NA))
  actions <- actions[!is.na(actions)]
  if (length(actions) == 0) return(NULL)
  triggers <- lapply(names(fk_relationships),
                     function(x) {
                       y <- fk_relationships[[x]]
                       lapply(actions,
                              function(ta) {
                                sqlite_auto_trigger(
                                  target_table = x,
                                  fk_col = y$fk_col,
                                  norm_table = y$norm_table,
                                  pk_col = y$pk_col,
                                  val_col = y$val_col,
                                  action_occurs = "after",
                                  trigger_action = ta,
                                  table_action = "update"
                                )
                              })
                     }) %>%
    setNames(names(fk_relationships))
  if (is.null(save_to_file)) {
    return(triggers)
  } else {
    triggers %>%
      unlist() %>%
      unname() %>%
      paste0(collapse = "\n") %>%
      readr::write_file(save_to_file)
  }
}

#' Build SQL to create views on normalized tables in SQLite
#'
#' @inheritParams get_fkpk_relationships
#' @inheritParams sqlite_auto_view
#'
#' @param save_to_file CHR scalar name of a file path to save generated SQL
#'   (default: NULL will return a list object to the R session)
#' @param append LGL scalar on whether to appead to `save_to_file` (default:
#'   FALSE)
#'
#' @return LIST if `save_to_file = FALSE` or none
#' @export
#' 
#' @usage 
#' build_views(db_map = er_map(con), dictionary = data_dictionary(con))
#' 
build_views <- function(db_map, references_in = "references", dictionary = db_dict, drop_if_exists = FALSE, save_to_file = NULL, append = TRUE) {
  stopifnot(is.list(db_map),
            is.character(references_in),
            length(references_in) == 1,
            is.list(dictionary),
            is.logical(drop_if_exists),
            length(drop_if_exists) == 1,
            any(is.null(save_to_file),
                is.character(save_to_file) & length(save_to_file) == 1),
            is.logical(append),
            length(append) == 1)
  fk_relationships <- get_fkpk_relationships(db_map, references_in)
  table_names <- names(fk_relationships)
  dictionary <- dictionary[table_names]
  views <- lapply(table_names,
                  function(x) {
                    sqlite_auto_view(
                      table_pragma = dictionary[[x]],
                      target_table = x,
                      relationships = fk_relationships[[x]],
                      drop_if_exists = drop_if_exists)
                  }) %>%
    setNames(table_names)
  if (is.null(save_to_file)) {
    return(views)
  } else {
    views %>%
      unlist() %>%
      unname() %>%
      paste0(collapse = "\n") %>%
      readr::write_file(save_to_file)
  }
}

#' Extract foreign key relationships from a schema
#'
#' This convenience function is part of the automatic generation of SQL commands
#' building views and triggers from a defined schema. Its sole purpose is as a
#' pre-pass extraction of foreign key relationships between tables from an
#' object created by [db_map], which in turn relies on specific formatting in
#' the schema SQL definitions.
#'
#' @note This only functions for list objects formatted correctly. That is, each
#'   entry in [db_map] must contain an element with a name matching that
#'   provided to `references_in` which contains a character vector formatted as
#'   "table1 REFERENCES table2(pk_column)".
#'
#' @param db_map  LIST object containing descriptions of table mapping in an
#'   opinionated manner, generally generated by [er_map]. The expectation is a
#'   list of tables, with references in SQL form enumerated in a child element
#'   with a name matching `references_in`
#' @param references_in CHR scalar naming the child element containing SQL
#'   references statements of the form "fk_column REFERENCES table(pk_column)"
#'   (default: "references" is provided by [er_map])
#' @param dictionary LIST object containing the schema dictionary produced by
#'   [data_dictionary] fully describing table entities
#'
#' @return LIST of data frames with one element for each table with a foreign
#'   key defined
#' @export
#'
#' @usage
#' get_fkpk_relationships(er_map(db_conn = con))
#' 
get_fkpk_relationships <- function(db_map, references_in = "references", dictionary = db_dict) {
  stopifnot(is.character(references_in),
            is.list(db_map),
            length(references_in) == 1)
  if (references_in %in% names(db_map)) {
    db_map <- list(db_map)
    names(db_map) <- db_map[[1]]$object_name
  }
  fk_relationships <- lapply(db_map, function(x) if (references_in %in% names(x)) x[[references_in]])
  fk_relationships <- fk_relationships[sapply(fk_relationships, length) > 0]
  fk_relationships <- lapply(fk_relationships,
                             function(x) {
                               x %>%
                                 stringr::str_remove_all("\\)") %>%
                                 as.data.frame() %>%
                                 setNames("one") %>%
                                 tidyr::separate(one,
                                                 into = c("fk_col", "norm_table", "pk_col"),
                                                 sep = " REFERENCES |\\(") %>%
                                 # dplyr::filter(grepl("norm", .$norm_table)) %>%
                                 mutate(val_col = vector(mode = "list", length = nrow(.)))
                             })
  fk_relationships <- fk_relationships[sapply(fk_relationships, nrow) > 0]
  tables <- names(fk_relationships)
  fk_relationships <- lapply(tables,
                             function(x) {
                               norm_tables <- fk_relationships[[x]]
                               for (nt in 1:nrow(norm_tables)) {
                                 table_cols <- dictionary[[norm_tables$norm_table[[nt]]]]$name
                                 table_cols <- table_cols[table_cols != norm_tables$pk_col[nt]]
                                 norm_tables$val_col[nt] <- list(table_cols)
                               }
                               return(norm_tables)
                             }) %>%
    setNames(tables)
  return(fk_relationships)
}

#' Create a basic SQL trigger for handling foreign key relationships
#'
#' This creates a simple trigger designed to streamline foreign key compliance
#' for SQLite databases. Resulting triggers will check during table insert or
#' update actions that have one or more foreign key relationships defined as
#' `target_table.fk_col = norm_table.pk_col`. It is primarily for use in
#' controlled vocabulary lists where a single id is tied to a single value in
#' the parent table, but more complicated relationships can be handled.
#'
#' These are intended as native database backup support for when connections do
#' not change the default SQLite setting of PRAGMA foreign_keys = off.
#' Theoretically any trigger could be created, but should only be used with care
#' outside the intended purpose.
#'
#' Triggers created by this function will check all new INSERT and UPDATE
#' statements by checking provided values against their parent table keys. If an
#' index match is found no action will be taken on the parent table. If no match
#' is found, it is assumed this is a new normalized value and it will be added
#' to the normalization table and the resulting new key will be replaced in the
#' target table column.
#'
#' @note While this will work on any number of combinations, all triggers should
#'   be heavily inspected prior to use. The default case for this trigger is to
#'   set it for a single FK/PK relationship with a single normalization value.
#'   It will run on any number of normalized columns however trigger behavior
#'   may be unexpected for more complex relationships.
#'
#' @note If `or_ignore` is set to TRUE, errors in adding to the parent table
#'   will be ignored silently, possibly causing NULL values to be inserted into
#'   the target table foreign key column. For this reason it is recommended that
#'   the `or_ignore` parameter only be set to true to expand parent table
#'   entries, but it will only supply a single value for the new normalization
#'   table. If additional columns in the parent table must be populated (e.g.
#'   the parent table has two required columns "value" and "acronym"), it is
#'   recommended to take care of those prior to any action that would activate
#'   these triggers.
#'
#' @note Parameters are not checked against a schema (e.g. tables and columns
#'   exist, or that a relationships exists between tables). This function
#'   processes only text provided to it.
#'
#' @note Define individual relationships between `fk_col`, `norm_table`,
#'   `pk_col`, and `val_col` as necessary. Lengths for these parameters should
#'   match in a 1:1:1:1 manner to fully describe the relationships. If the
#'   schema of all tables listed in `norm_table` are close matches, e.g. all
#'   have two columns "id" and "value" then `pk_col` and `val_col` will be
#'   reused when only a single value is provided for them. That is, provided
#'   three `norm_table`(s) and one `pk_col` and one `val_col`, the arguments for
#'   `pk_col` and `val_col` will apply to each `norm_table`.
#'
#' @note The usage example is built on a hypothetical SQLite schema containing
#'   four tables, one of which ("test" - with columns "id", "col1", "col2", and
#'   "col3") defines foreign key relationships to the other three ("norm_col1",
#'   "norm_col2", and "norm_col3").
#'   
#' @seealso build_triggers
#'
#' @param target_table CHR scalar name of a table with a foreign key constraint.
#' @param fk_col CHR vector name(s) of the column(s) in `target_table` with
#'   foreign key relationship(s) defined.
#' @param norm_table CHR vector name(s) of the table(s) containing the primary
#'   key relationship(s).
#' @param pk_col CHR vector name(s) of the column(s) in `norm_table` containing
#'   the primary key(s) side of the relationship(s).
#' @param val_col CHR vector name(s) of the column(s) in `norm_table` containing
#'   values related to the primary key(s) of the relationship(s).
#' @param action_occurs CHR scalar on when to run the trigger, must be one of
#'   `c("before", "after", "instead")` ("instead" should only be used if
#'   `target_table` is a view - this restriction is not enforced).
#' @param trigger_action CHR scalar on what type of trigger this is (e.g. `when`
#'   = "after" and `trigger_action` = "insert" -> "AFTER INSERT INTO") and must
#'   be one of `c("insert", "update", "delete")`.
#' @param for_each CHR scalar for SQLite this must be only `row` - translated
#'   into a "FOR EACH ROW" clause. Set to any given noun for other SQL engines
#'   supporting other trigger transaction types (e.g. "FOR EACH STATEMENT"
#'   triggers)
#' @param table_action CHR scalar on what type of action to run when the trigger
#'   fires, must be one of `c("insert", "update", "delete")`.
#' @param filter_col CHR scalar of a filter column to override the final WHERE
#'   clause in the trigger. This should almost always be left as the default "".
#' @param filter_val CHR scalar of a filter value to override the final WHERE
#'   clause in the trigger. This should almost always be left as the default "".
#' @param or_ignore LGL scalar on whether to ignore insertions to normalization
#'   tables if an error occurs (default: TRUE, which can under certain
#'   conditions raise exceptions during execution of the trigger if more than a
#'   single value column exists in the parent table)
#' @param addl_actions CHR vector of additional target actions to add to
#'   `table_action` statements, appended to the end of the resulting "insert" or
#'   "update" actions to `target_table`. If multiple tables are in use, use
#'   positional matching in the vector (e.g. with three normalization tables,
#'   and additional actions to only the second, use c("", "additional actions",
#'   ""))
#'
#' @return CHR scalar of class glue containing the SQL necessary to create a
#'   trigger. This is raw text; it is not escaped and should be further
#'   manipulated (e.g. via dbplyr::sql()) as your needs and database
#'   communication pipelines dictate.
#' @export
#'
#' @usage sqlite_auto_trigger(target_table = "test", fk_col = c("col1", "col2",
#'   "col3"), norm_table = c("norm_col1", "norm_col2", "norm_col3"), pk_col =
#'   "id", val_col = "value", action_occurs = "after", trigger_action =
#'   "insert", table_action = "update")
#'   
sqlite_auto_trigger <- function(target_table,
                                fk_col,
                                norm_table,
                                pk_col,
                                val_col,
                                action_occurs = c("before", "after", "instead"),
                                trigger_action = c("insert", "update", "delete"),
                                for_each = "row",
                                table_action = c("insert", "update", "delete"),
                                filter_col = "",
                                filter_val = "",
                                or_ignore = TRUE,
                                addl_target_actions = NULL) {
  kwargs <- as.list(environment())
  kwargs <- kwargs[-grep("or_ignore|addl_target_actions|val_col", names(kwargs))]
  n_norm_tables <- length(norm_table)
  stopifnot(all(sapply(kwargs, is.character), sapply(kwargs, is.vector)))
  action_occurs <- match.arg(action_occurs)
  trigger_action <- match.arg(trigger_action)
  table_action <- match.arg(table_action)
  for_each <- match.arg(for_each, c("row", "statement"))
  if (length(val_col) == 1) val_col <- rep(val_col, times = length(norm_table))
  if (length(pk_col) == 1) pk_col <- rep(pk_col, times = length(norm_table))
  if (action_occurs == "instead") action_occurs <- "instead of"
  stopifnot(length(target_table) == 1,
            length(or_ignore) == 1,
            length(filter_col) == 1,
            length(filter_val) == 1,
            is.logical(or_ignore),
            ifelse(is.null(addl_target_actions), TRUE, is.character(addl_target_actions) && length(addl_target_actions) == 1),
            n_norm_tables > 1 &&
              n_norm_tables == length(fk_col) ||
              n_norm_tables == length(pk_col) ||
              n_norm_tables == length(val_col))
  trigger_title <- glue::glue("trigger_{target_table}_{tolower(action_occurs)}_{tolower(trigger_action)}")
  trigger_comment <- glue::glue("/* [autogenerated by sqlite_auto_trigger()] Trigger to control normalization compliance {action_occurs} {trigger_action} actions on table '{target_table}' */")
  clause_title <- glue::glue(
    "{trigger_title} {trigger_comment} {toupper(action_occurs)} {toupper(trigger_action)} ON {target_table} FOR EACH {toupper(for_each)}",
  )
  clause_when <- paste(
    "WHEN",
    paste0(
      glue::glue("NEW.{fk_col} NOT IN (SELECT {pk_col} FROM {norm_table})"),
      collapse = " OR ")
  )
  clause_norm_inserts <- character(0)
  clause_target_action <- character(0)
  for (i in 1:length(norm_table)) {
    clause_norm_inserts[i] <- glue::glue(
      "INSERT {ifelse(or_ignore, 'OR IGNORE ', '')}INTO {norm_table[i]} ({glue::glue_collapse(val_col[[i]], sep = ', ')}) VALUES (NEW.{glue_collapse(c(fk_col[i], rep('NULL', length(val_col[[i]]) - 1)), sep = ', ')})"
    )
    clause_target_action[i] <- glue::glue_collapse(
      glue::glue("{val_col[[i]]} = NEW.{fk_col[i]}"),
      sep = " OR "
    )
  }
  clause_norm_inserts <- glue_collapse(clause_norm_inserts, sep = "; ")
  clause_where <- paste(
    "WHERE",
    ifelse(filter_col == "" && filter_val == "",
           "ROWID = NEW.ROWID",
           sprintf("%s = %s", filter_col, filter_val == ""))
  )
  clause_target_action <- paste0(
    paste0(
      glue::glue(
        "{fk_col} = (iif(NEW.{fk_col} IN (SELECT {pk_col} FROM {norm_table}), NEW.{fk_col}, (SELECT {pk_col} FROM {norm_table} WHERE {clause_target_action} ORDER BY {pk_col} LIMIT 1)))",
      ),
      ifelse(is.null(addl_target_actions),
             "",
             addl_target_actions),
      collapse = ", "),
    " ", clause_where, ";")
  clause_table_action <- switch(
    tolower(table_action),
    "insert" = "INSERT OR IGNORE INTO",
    "update" = glue::glue("UPDATE {target_table} SET {clause_target_action}"),
    "delete" = glue::glue("DELETE FROM {target_table} {clause_where}")
  )
  clause_start <- paste(clause_title, clause_when)
  clause_actions <- paste(clause_norm_inserts, clause_table_action, sep = "; ")
  trigger_sql <- glue::glue("DROP TRIGGER IF EXISTS {trigger_title}; CREATE TRIGGER IF NOT EXISTS {clause_start} BEGIN {clause_actions} END;")
  return(trigger_sql)
}

#' Create a basic SQL view of a normalized table
#'
#' Many database viewers will allow links for normalization tables to get the
#' human-readable value of a normalized column. Instead it is often preferable
#' to build in views automatically that "denormalize" such tables for display or
#' use in an application. This function seeks to script the process of creating
#' those views. It examines the table definition from [pragma_table_info] and
#' will extract the primary/foreign key relationships to build a "denormalized"
#' view of the table using [get_fkpk_relationships] which requires a database
#' map created from [er_map] and data dictionary created from [data_dictionary].
#'
#' TODO for v2: abstract the relationships call by looking for objects in the
#' current session.
#'
#' @note No schema checking is performed by this function, but rather relies on
#'   definitions from other functions.
#'
#' @seealso build_views
#' @seealso pragma_table_info
#' @seealso get_fkpk_relationships
#' @seealso er_map
#' @seealso data_dictionary
#'
#' @note This example will run slowly if the database map [er_map] and
#'   dictionary [data_dictionary] haven't yet been called. If they exist in your
#'   session, use those as arguments to get_fkpk_relationships.
#'
#' @param table_pragma data.frame object from [pragma_table_info] for a given
#'   table name in the database
#' @param target_table CHR scalar name of the database table to build for, which
#'   should be present in the relationship definition
#' @param relationships data.frame object describing the foreign key
#'   relationships for `target_table`, which should generally be the result of a
#'   call to [get_fkpk_relationships]
#' @param drop_if_exists LGL scalar indicating whether to include a "DROP VIEW"
#'   prefix for the generated view statement; as this has an impact on schema,
#'   no default is set
#'
#' @return CHR scalar of class glue containing the SQL necessary to create a
#'   "denormalized" view. This is raw text; it is not escaped and should be
#'   further manipulated (e.g. via dbplyr::sql()) as your needs and database
#'   communication pipelines dictate.
#' @export
#'
#' @usage sqlite_auto_view(table_pragma = pragma_table_info("contributors"),
#'   target_table = "contributors", relationships =
#'   get_fkpk_relationships(db_map = er_map(con), dictionary =
#'   data_dictionary(con)), drop_if_exists = FALSE)
#'   
sqlite_auto_view <- function(table_pragma, target_table, relationships, drop_if_exists) {
  stopifnot(is.logical(drop_if_exists), length(drop_if_exists) == 1)
  if (target_table %in% names(relationships)) {
    relationships <- relationships[[target_table]]
  }
  tmp <- table_pragma %>%
    left_join(relationships, by = c("name" = "fk_col")) %>%
    select(name, norm_table:val_col) %>%
    unnest(c(val_col), keep_empty = TRUE) %>%
    mutate(lhs = str_c(target_table, name, sep = "."),
           rhs_val = str_c(norm_table, val_col, sep = "."),
           rhs_join = str_c(norm_table, pk_col, sep = "."),
           select_comment = str_c("\n\t\t\t/* ",
                                  ifelse(is.na(norm_table), "Direct use column", "Normalized value column"),
                                  " '",
                                  ifelse(is.na(norm_table), name, val_col),
                                  "' from table '",
                                  ifelse(is.na(norm_table), target_table, norm_table),
                                  "'. */"),
           sql_select = str_c("\n\t\t",
                              ifelse(is.na(norm_table), lhs, rhs_val),
                              " AS ",
                              ifelse(is.na(norm_table), name, str_replace(rhs_val, "\\.", "_")),
                              ", ", 
                              select_comment),
           sql_join = ifelse(is.na(norm_table),
                             NA,
                             str_c("LEFT JOIN ",
                                   ifelse(is.na(norm_table),
                                          NA,
                                          norm_table),
                                   " ON ", lhs, " = ", rhs_join))
    )
  tmp$sql_select[length(tmp$sql_select)] <- gsub(", \n\t\t\t/\\*", " \n\t\t\t/\\*", tmp$sql_select[length(tmp$sql_select)])
  norm_tables <- format_list_of_names(unique(tmp$norm_table[!is.na(tmp$norm_table)]), add_quotes = TRUE)
  view_comment <- glue::glue('/* [autogenerated by sqlite_auto_view()] View of "{target_table}" normalized by {norm_tables}. */')
  target_columns <- paste0(tmp$sql_select, collapse = " \t")
  norm_tables_join <- paste0(unique(tmp$sql_join[!is.na(tmp$sql_join)]), collapse = " \n\t")
  drop_statement <- ifelse(drop_if_exists,
                           glue::glue("DROP VIEW IF EXISTS view_{target_table}; "),
                           "")
  view_sql <- glue::glue("{drop_statement}\nCREATE VIEW IF NOT EXISTS view_{target_table} AS \n\t\t{view_comment} \n\tSELECT \t{target_columns} \n\tFROM {target_table} \n\t{norm_tables_join};")
  return(view_sql)
}
