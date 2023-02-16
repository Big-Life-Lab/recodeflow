#' Returns the variables needed to calculate a derived variable
#'
#' @param derived_var string The name of the derived variable
#' @param variable_details_sheet data.frame A data frame containing a
#' variable details sheet
#'
#' @return list of named lists. Each named list represents one variable. The
#' named list has the following members:
#' 1. name: string. The name of the variable
#' 2. type: string. The type of the variable. Can have one of the following
#' values:
#'  * variable: Represents the name of a variable
#'  * constant: Represents a constant for example a string
#'  * table: Represents the name of a reference table
#' @export
#'
#' @examples
get_derived_from_vars <-
  function(derived_var, variable_details_sheet) {
    # 1. Get the string of derived from variables from the variableStart column
    # For example, if the column value is DerivedVar::[ADL_01, ADL_02], this
    # will extract "ADL_01, ADL_02"

    # Get all the variable details rows for this derived variable
    derived_var_variable_details_rows <-
      variable_details_sheet[get_var_details_row_indices(variable_details_sheet, derived_var),]
    # Get the value of the variableStart column with the DerivedVar string
    derived_from_vars_str <-
      derived_var_variable_details_rows[1, pkg.env$columns.VariableStart]
    # Extract the string we want
    derived_from_string <- regmatches(derived_from_vars_str,
                                      regexec(derived_var_regex, derived_from_vars_str))[[1]][2]

    # 2. Get all the derived from variables for this derived variable
    # Each derived from variable is separated from the next by a comma
    # We will create a named list for each variable that represents the type
    # of variable it is. There are three types of variables:
    # 1. variable names
    # 2. string constants: Identified by double quotes
    # 3. reference tables: Identified by a tables:: prefix
    derived_from_vars <- list()
    derived_from_var_names <-
      trimws(strsplit(derived_from_string, ",")[[1]])
    for (derived_from_var_name in derived_from_var_names) {
      constant_regex <- "\".*\""
      table_derived_from_regex <- glue::glue("{pkg.env$recode.key.tables}.*")
      if (grepl(constant_regex, derived_from_var_name)) {
        derived_from_vars <- add_new_derived_from_var(
          derived_from_vars,
          gsub('"', "", derived_from_var_name),
          derived_from_var_type$constant
        )
      }
      else if (grepl(table_derived_from_regex, derived_from_var_name)) {
        derived_from_vars <- add_new_derived_from_var(
          derived_from_vars,
          gsub(pkg.env$recode.key.tables, "", derived_from_var_name),
          derived_from_var_type$table
        )
      }
      else {
        derived_from_vars <- add_new_derived_from_var(
          derived_from_vars,
          derived_from_var_name,
          derived_from_var_type$variable
        )
      }
    }

    return(derived_from_vars)
  }

add_new_derived_from_var <- function(
    derived_from_vars,
    name,
    type
) {
  derived_from_vars[[length(derived_from_vars) + 1]] <- list(
    name = name,
    type = type
  )
  return(derived_from_vars)
}

derived_from_var_type <- list(
  constant = "constant",
  table = "table",
  variable = "variable"
)
