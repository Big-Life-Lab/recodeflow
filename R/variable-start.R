# This file has helper functions for the variableStart column and its values


#' Checks whether a variable start is a table
#'
#' @param feeder_var string The variable start to check
#'
#' @return bool
#' @export
#'
#' @examples
is_table_feeder_var <- function(feeder_var) {
  return(grepl(pkg.env$recode.key.tables, feeder_var))
}

#' Returns the name of the table for a table start variable
#'
#' @param table_feeder_var string The table variable start
#'
#' @return string
#' @export
#'
#' @examples
get_table_name <- function(table_feeder_var) {
  return(trimws(gsub(pkg.env$recode.key.tables, "", table_feeder_var)))
}
