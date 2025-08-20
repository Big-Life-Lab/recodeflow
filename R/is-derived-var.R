#' Whether a variable in a variables details sheet is a derived variable
#'
#' @param variable_details_row A data frame with a single row which will be
#' checked
#' @return A boolean
#' @keywords internal
is_derived_var <- function(variable_details_row) {
  derived_var_regex <- "DerivedVar::\\[(.+?)\\]|DerivedVar::\\[\\]"
  return(length(grep(
    derived_var_regex, variable_details_row[1, pkg.env$columns.variableStart]
  )) > 0)
}
