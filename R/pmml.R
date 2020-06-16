#' Creates a PMML string from an xFlow document
#'
#' @param var_details_sheet A data frame representing a variable details sheet,
#' @param vars_sheet A data frame representing a variables sheet.
#' @param vars_sheet A string containing the name of the database that holds
#' the start variables. Should match up with one of the databases in the
#' databaseStart column.
#' @param vars_to_convert A vector of strings containing the names of variables
#' from the variable column in the variable details sheet that should be.
#' converted to PMML. Passing in an empty vector will convert all the variables.
#'
#' @return A string continaing the PMML string
#' @export
#'
#' @examples
pmml.xflow_to_pmml <- function(var_details_sheet, vars_sheet, db_name, vars_to_convert) {
  pmml_str <- ""

  return("")
}
