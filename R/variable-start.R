#' Variable start helper functions
#'
#' These functions help in processing and validating variable start values,
#' particularly for handling table-based variables.
#'
#' @name variable-start
NULL

# This file has helper functions for the variableStart column and its values
#' Checks whether a variable start is a table
#'
#' @param feeder_var string The variable start to check
#' @return logical TRUE if the variable is a table feeder, FALSE otherwise
#' @export
#' @examples
#' # Check if a variable is a table feeder
#' is_table_feeder_var("$table:my_lookup") # Returns TRUE
#' is_table_feeder_var("[regular_var]") # Returns FALSE
#' is_table_feeder_var("$table:values") # Returns TRUE
is_table_feeder_var <- function(feeder_var) {
    return(grepl(pkg.env$recode.key.tables, feeder_var))
}

#' Returns the name of the table for a table start variable
#'
#' @param table_feeder_var string The table variable start
#' @return string The extracted table name
#' @export
#' @examples
#' # Extract table names from table feeder variables
#' get_table_name("$table:lookup_codes") # Returns "lookup_codes"
#' get_table_name("$table:reference") # Returns "reference"
#' get_table_name("$table:values") # Returns "values"
get_table_name <- function(table_feeder_var) {
    return(trimws(gsub(pkg.env$recode.key.tables, "", table_feeder_var)))
}
