#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Margins as character vector.
#'
#' @examples
get_margins <- function (chars) {
  return (trimws(strsplit(chars, ":")[[1]]))
}

#' Check if a character object can be converted to a number.
#'
#' @param chars Character object.
#'
#' @return Whether `chars` can be converted to a numeric value.
#'
#' @examples
is_numeric <- function(chars) {
  return (suppressWarnings(!is.na(as.numeric(chars))))
}

#' Check if recFrom is a range for a variable details row.
#'
#' @param var_details_row Variable details sheet row.
#'
#' @return Whether recFrom is a range.
#'
#' @examples
is_rec_from_range <- function(var_details_row) {
  return (grepl(":", var_details_row$recFrom, fixed=TRUE))
}


#' Get all variable details rows for a variable and database combination.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param var_name Variable name.
#' @param db_name Database name.
#'
#' @return All variable details rows for the variable and database combination.
#'
#' @examples
get_var_details_rows <- function (var_details_sheet, var_name, db_name) {
  var_name_indices <- get_var_details_row_indices(var_details_sheet, var_name)
  db_indices <- which(grepl(db_name, var_details_sheet$databaseStart), arr.ind = TRUE)
  intersect_indices <- intersect(var_name_indices, db_indices)
  return (var_details_sheet[intersect_indices,])
}

#' Get all variable details row indices for a variable.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param var_name Variable name.
#'
#' @return All variable details row indices for a variable.
#'
#' @examples
get_var_details_row_indices <- function (var_details_sheet, var_name) {
  return (which(var_details_sheet$variable == var_name, arr.ind = TRUE))
}

#' Get variable row from variable sheet.
#'
#' @param var_name Variable name.
#' @param vars_sheet Variable sheet data frame.
#'
#' @return Variable row.
#'
#' @examples
get_var_sheet_row <- function (var_name, vars_sheet) {
  return (vars_sheet[which(vars_sheet$variable == var_name, arr.ind = TRUE)[1],])
}

#' Get variable name from variableStart using database name.
#'
#' @param var_details_row A variable details row.
#' @param db_name Name of database to extract from.
#'
#' @return character The name of the start variable.
#'
#' @examples
get_start_var_name <- function(var_details_row, db_name) {
  # Create regex using database name, database variable start infix, followed by variable start
  var_regex <- paste0(db_name, pkg.env$db_var_start_infix, "(.+?)[,?]")

  # Get regex match for variable start
  match <- regexec(var_regex, var_details_row$variableStart)

  # Extract variable start from column
  return (regmatches(var_details_row$variableStart, match)[[1]][2])
}

#' Get data type for variable type.
#'
#' @param var_details_rows All variable details rows for the variable.
#' @param var_type Variable type
#'
#' @return `var_type` data type.
#'
#' @examples
get_variable_type_data_type <- function (var_details_rows, var_type, is_start_var) {
  is_categorical <- var_type %in% c(pkg.env$var_details_cat, pkg.env$var_cat)
  if (is_categorical) {
    char_var_details_rows <- ifelse(is_start_var,
                                    var_details_rows[!is_numeric(var_details_rows$recFrom), ],
                                    var_details_rows[!is_numeric(var_details_rows$recTo), ])
    if (length(char_var_details_rows) > 0) return (pkg.env$node_attr.dataType.string)
    return (pkg.env$node_attr.dataType.integer)
  }
  return (pkg.env$node_attr.dataType.float)
}
