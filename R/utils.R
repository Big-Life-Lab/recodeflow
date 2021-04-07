#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Margins as character vector.
#'
#' @examples
get_margins <- function (chars) {
  trimmed_chars <- trimws(chars)
  is_singleton <- is_numeric(trimmed_chars)
  if (is_singleton) return (c(trimmed_chars, trimmed_chars))

  is_range <- grepl(pkg.env$margin_separator, trimmed_chars, fixed = TRUE)
  if (!is_range) return (c(0, 0))

  margins <- strsplit(substr(trimmed_chars, 2, nchar(trimmed_chars) - 1), pkg.env$margin_separator)[[1]]
  return (margins)
}

#' Get closure type for a margin.
#'
#' @param chars Character vector.
#'
#' @return Closure type.
#'
#' @examples
get_margin_closure <- function (chars) {
  if (is_left_open(chars)) {
    if (is_right_open(chars)) return (pkg.env$node_attr.closure.open)
    return (pkg.env$node_attr.closure.leftOpen)
  }
  if (is_right_open(chars)) return (pkg.env$node_attr.closure.rightOpen)
  return (pkg.env$node_attr.closure.closed)
}

#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Whether the left endpoint of an interval is open.
#'
#' @examples
is_left_open <- function (chars) {
  trimmed_chars <- trimws(chars)
  return (substr(trimmed_chars, 1, 1) == "(")
}

#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Whether the right endpoint of an interval is open.
#'
#' @examples
is_right_open <- function (chars) {
  trimmed_chars <- trimws(chars)
  return (substr(trimmed_chars, nchar(trimmed_chars), nchar(trimmed_chars) + 1) == ")")
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
  margins <- get_margins(var_details_row$recFrom)
  # only consider margins as a range if the endpoints are different
  return (margins[1] != margins[2])
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
  # The value of the variableStart column for this variable details row
  start_variables = var_details_row$variableStart

  # The regex that will be used to pluck the name of the start variable from
  # list of start variables and their databases. For example, if the db_name
  # is cchs2001_p and the list is cchs2001_p::RACA_6A, this regex will pluck out
  # RACA_6A
  db_var_regex <- paste0(db_name, pkg.env$db_var_start_infix, "(.+?)[,?]")
  # Get regex match for variable start
  db_var_regex_match <- regexec(db_var_regex, start_variables)
  # Get the start variable for the db passed in the db_name parameter
  start_var_for_db <- regmatches(start_variables, db_var_regex_match)[[1]][2]

  # The regex to pluck out the default start variable to use. In the column
  # of start variables this will be encoded as [RACA_6A]. This regex will
  # pluck out RACA_6A
  default_var_regex <- "\\[(.+?)\\]"
  default_var_match <- regexec(default_var_regex, start_variables)
  default_var <- regmatches(start_variables, default_var_match)[[1]][2]

  # If there was a start variable for the passed database, return it
  if(!is.na(start_var_for_db) & nchar(start_var_for_db) != 0) {
    return(start_var_for_db)
  }
  # Otherwise, if there was a default start variable return it
  else if(nchar(default_var) != 0) {
    return(default_var)
  }
  # Otherwise, throw an error saying we could not find a start variable
  else {
    stop(paste(
      "No start variable found for database ",
      db_name,
      "for column ",
      start_variables
    ))
  }
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
