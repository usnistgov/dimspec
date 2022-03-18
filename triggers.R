#' Create a basic SQL trigger for handling foreign key relationships
#'
#' This creates a simple trigger designed to streamline foreign key compliance
#' for SQLite databases. Resulting triggers will check during table insert or
#' update actions that have one or more foreign key relationships defined as
#' `target_table.fk_col = norm_table.pk_col`. It is primarily for use in
#' controlled vocabulary lists where a single id is tied to a single value in
#' the parent table.
#'
#' Triggers created by this function will check all new INSERT and UPDATE
#' statements by checking provided values against their parent table keys. If an
#' index match is found no action will be taken on the parent table. If no match
#' is found, it is assumed this is a new normalized value and it will be added
#' to the table and the resulting new key will be replaced in the target table
#' column.
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
#' @note The example is built on a simple SQLite schema containing four tables,
#'   one of which ("test" - with columns "id", "col1", "col2", and "col3")
#'   defines foreign key relationships to the other three ("norm_col1",
#'   "norm_col2", and "norm_col3"); this schema can be added into a "sandbox"
#'   database and is located at ./examples/sql/schema_make_sql_triggers.sql
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
#'
#' @return CHR scalar of class glue containing the SQL necessary to create a
#'   trigger. This is raw text; it is not escaped and should be further
#'   manipulated (e.g. via dbplyr::sql()) as your needs and database
#'   communication pipelines dictate.
#' @export
#'
#' @examples
#' make_sql_trigger(target_table = "test", fk_col = c("col1", "col2", "col3"), norm_table = c("norm_col1", "norm_col2", "norm_col3"), pk_col = "id", val_col = "value", action_occurs = "after", trigger_action = "insert", table_action = "update")
make_sql_trigger <- function(target_table,
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
  kwargs <- kwargs[-grep("or_ignore|addl_target_actions", names(kwargs))]
  n_norm_tables <- length(norm_table)
  stopifnot(all(sapply(kwargs, is.character), sapply(kwargs, is.vector)))
  action_occurs <- match.arg(action_occurs)
  trigger_action <- match.arg(trigger_action)
  table_action <- match.arg(table_action)
  for_each <- match.arg(for_each, c("row", "statement"))
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
  clause_title <- glue::glue(
    "{trigger_title} {toupper(action_occurs)} {toupper(trigger_action)} ON {target_table} FOR EACH {toupper(for_each)}",
  )
  clause_when <- paste(
    "WHEN",
    paste0(
      glue::glue("NEW.{fk_col} NOT IN (SELECT {pk_col} FROM {norm_table})"),
      collapse = " OR ")
  )
  clause_norm_inserts <- paste(
    glue::glue(
      "INSERT {ifelse(or_ignore, 'OR IGNORE ', '')}INTO {norm_table} ({val_col}) VALUES (iif(NEW.{fk_col} IN (select {pk_col} FROM {norm_table}), (SELECT {val_col} FROM {norm_table} LIMIT 1), NEW.{fk_col}));",
    ),
    collapse = " ")
  clause_where <- paste(
    "WHERE",
    ifelse(filter_col == "" && filter_val == "",
           "ROWID = NEW.ROWID",
           sprintf("%s = %s", filter_col, filter_val == ""))
  )
  clause_target_action <- paste0(
    paste0(
      glue::glue(
        "{fk_col} = (iif(NEW.{fk_col} IN (SELECT {pk_col} FROM {norm_table}), NEW.{fk_col}, (SELECT {pk_col} FROM {norm_table} WHERE {val_col} = NEW.{fk_col})))",
      ),
      ifelse(is.null(addl_target_actions),
             "",
             addl_target_actions),
      collapse = ", "),
    " ", clause_where, ";")
  clause_table_action <- switch(
    tolower(table_action),
    "insert" = "INSERT OR IGNORE INTO",
    "update" = sprintf("UPDATE %s SET %s", target_table, clause_target_action),
    "delete" = sprintf("DELETE FROM %s %s", target_table, clause_target_action)
  )
  clause_start <- paste(clause_title, clause_when)
  clause_actions <- paste(clause_norm_inserts, clause_table_action)
  trigger_sql <- glue::glue("DROP TRIGGER IF EXISTS {trigger_title}; CREATE TRIGGER IF NOT EXISTS {clause_start} BEGIN {clause_actions} END;")
  return(trigger_sql)
}

#' Build pairs of INSERT/UPDATE triggers to resolve foreign key relationships
#'
#' When building schema by script, it is often handy to enforce certain
#' behaviors on database transactions involving foreign keys, especially in
#' SQLite. Given a properly structured list object describing the mappings
#' between tables in a schema (e.g. one deriving from [er_map]), this function
#' will parse those for relationships.
#'
#' Primarily, this requires a list object referring to tables that contains in
#' each element a child element with the name provided in `references_in`.
#'
#' @note Tables in `db_map` that do not contain foreign key relationships will
#'   be dropped from the output list.
#'
#' @note This is largely a convenience function to programmatically apply
#'   [make_sql_triggers] to an entire schema. To skip tables with defined
#'   foreign key relationships for which triggers are undesirable, remove those
#'   tables from `db_map` prior to calling this function.
#'
#' @param db_map LIST object containing descriptions of table mapping in an
#'   opinionated manner, generally generated by [er_map]. The expectation is a
#'   list of tables, with references in SQL form enumerated in a child element
#'   with a name matching `references_in`
#' @param references_in CHR scalar naming the child element containing SQL
#'   references statements of the form "fk_column REFERENCES table(pk_column)"
#'   (default: "references" is provided by [er_map])
#' @param create_insert_trigger LGL scalar indicating whether to build an insert
#'   trigger for each table (default: TRUE).
#' @param create_insert_trigger LGL scalar indicating whether to build an update
#'   trigger for each table (default: FALSE).
#' TODO
#' @param save_to_file CHR scalar of a file in which to write the output, if any
#'   (default: NULL will return the resulting object to the R session)
#'
#' @return LIST object containing one element for each table in `db_map`
#'   containing foreign key references, with one child
#' @export
#'
#' @examples
#' \dontrun{build_triggers(er_map(db_conn = con))}
build_triggers <- function(db_map, references_in = "references", create_insert_trigger = TRUE, create_update_trigger = TRUE, save_to_file = NULL) {
  require(magrittr)
  stopifnot(is.character(references_in),
            is.list(db_map),
            length(references_in) == 1)
  if (references_in %in% names(db_map)) db_map <- list(db_map)
  fk_relationships <- lapply(db_map, function(x) if (references_in %in% names(x)) x[[references_in]])
  fk_relationships <- fk_relationships[sapply(fk_relationships, length) > 0]
  tables <- names(fk_relationships)
  fk_relationships <- lapply(fk_relationships,
                             function(x) {
                               x %>%
                                 stringr::str_remove_all("\\)") %>%
                                 as.data.frame() %>%
                                 setNames("one") %>%
                                 tidyr::separate(one,
                                                 into = c("fk_col", "norm_table", "pk_col"),
                                                 sep = " REFERENCES |\\(") %>%
                                 dplyr::filter(grepl("norm", .$norm_table))
                             })
  fk_relationships <- fk_relationships[sapply(fk_relationships, nrow) > 0]
  tables <- names(fk_relationships)
  fk_relationships <- lapply(names(fk_relationships),
                             function(x) {
                               norm_tables <- fk_relationships[[x]]
                               for (nt in 1:nrow(norm_tables)) {
                                 table_cols <- db_dict[[norm_tables$norm_table[[nt]]]]$name
                                 table_cols <- table_cols[table_cols != norm_tables$pk_col[nt]]
                                 norm_tables$val_col[nt] <- table_cols[1]
                                 return(norm_tables)
                               }
                             }) %>%
    setNames(tables)
  triggers <- lapply(names(fk_relationships),
                     function(x) {
                       y <- fk_relationships[[x]]
                       lapply(c("insert", "update"),
                              function(ta) {
                                make_sql_trigger(
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
    setNames(tables)
  if (is.null(save_to_file)) {
    return(triggers)
  } else {
    # TODO Write save to file logic
    # if (!file.exists(save_to_file)) {
    #   file.create(save_to_file)
    # }
  }
}