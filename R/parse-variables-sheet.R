#' Parse and validate a variables sheet
#'
#' @param variables_sheet A data frame containing the variables sheet
#'
#' @return If validation succeeds, returns the parsed variables sheet.
#' If validation fails, returns a named list with the following fields:
#' * **success**: set to FALSE
#' * **errors**: the list of errors where each item is a named list. The fields
#'   in the list depend on the type of error but the following two fields will
#'   always be there:
#'   * **type**: The error ID
#'   * **message**: The human readable error message
#' @export
parse_variables_sheet <- function(variables_sheet) {
  input_errors <- .validate_variables_sheet_arg(variables_sheet)
  if (length(input_errors) > 0) {
    return(list(
      success = FALSE,
      errors = input_errors
    ))
  }

  validation_errors <- .validate_derived_variables(variables_sheet)
  if (length(validation_errors) > 0) {
    return(list(
      success = FALSE,
      errors = validation_errors
    ))
  }

  class(variables_sheet) <- c("variables_sheet", class(variables_sheet))
  return(variables_sheet)
}

#' Run basic checks for the variables sheet used in the parse_variables_sheet
#' function
#'
#' @param variables_sheet
#'
#' @return A list of errors (empty if no errors found)
#' @keywords internal
.validate_variables_sheet_arg <- function(variables_sheet) {
  if (!checkmate::test_data_frame(variables_sheet)) {
    return(list(.create_invalid_input_type_error(variables_sheet)))
  }

  required_cols <- c(
    pkg.env$columns.variable,
    pkg.env$columns.variableStart
  )
  variables_sheet_cols <- colnames(variables_sheet)
  required_cols_validation <- checkmate::test_names(
    variables_sheet_cols, must.include = required_cols)
  if (!required_cols_validation) {
    missing_cols <- required_cols[!required_cols %in% variables_sheet_cols]
    actual_cols <- variables_sheet_cols
    return(list(
      .create_missing_required_columns_error(missing_cols, actual_cols)))
  }

  return(list())
}

#' Create an invalid input type error object
#'
#' @param variables_sheet The invalid input that was passed
#'
#' @return A list containing the error object with type, actual_type, and
#' message fields
#' @keywords internal
.create_invalid_input_type_error <- function(variables_sheet) {
  actual_type <- typeof(variables_sheet)
  return(list(
    type = "invalid_input_type",
    actual_type = actual_type,
    message = paste0(
      "variables_sheet must be a data.frame, not a ",
      actual_type
    )
  ))
}

#' Create a missing required columns error object
#'
#' @param missing_cols A character vector of column names that are missing
#' @param actual_cols A character vector of column names that are present
#'
#' @return A list containing the error object with type, missing_cols,
#' actual_cols, and message fields
#' @keywords internal
.create_missing_required_columns_error <- function(missing_cols, actual_cols) {
  return(list(
    type = "missing_required_columns",
    missing_cols = missing_cols,
    actual_cols = actual_cols,
    message = paste0(
      "variables_sheet is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". Columns found: ",
      paste(actual_cols, collapse = ", ")
    )
  ))
}

#' Create an invalid dependency error object
#'
#' @param row The row number in the variables sheet where the error occurred
#' @param db_column_var The database column variable that was incorrectly used
#'
#' @return A list containing the error object with type, row, message, and
#' db_column_var fields
#' @keywords internal
.create_invalid_dependency_error <- function(row, db_column_var) {
  return(list(
    type = "invalid_dependency",
    row = row,
    db_column_var = db_column_var,
    message = glue::glue(
      "Derived variable at row {row} uses database column ",
      "'{db_column_var}' directly. Derived variables must only use ",
      "non-derived or other derived variables."
    )
  ))
}

#' Validate the derived variables in a variables sheet
#'
#' @param variables_sheet A data frame containing the variables sheet
#'
#' @return A list of errors (empty if no errors found)
#' @keywords internal
.validate_derived_variables <- function(variables_sheet) {
  if (nrow(variables_sheet) == 0) {
    return(list())
  }

  errors <- seq_len(nrow(variables_sheet)) |>
    purrr::map(function(i) {
      row <- variables_sheet[i, ]

      if (!is_derived_var(row)) {
        return(NULL)
      }

      variable_start <- row[[pkg.env$columns.variableStart]]
      start_vars <- .extract_start_variables(variable_start)
      current_errors <- start_vars |>
        purrr::keep(.is_database_column_reference) |>
        purrr::map(function(start_var) {
          return(.create_invalid_dependency_error(i, start_var))
        })
      return(current_errors)
    }) |>
    purrr::compact() |>
    purrr::flatten()
  return(errors)
}

#' Extract start variables from a variableStart column value
#'
#' @param variable_start The variableStart string
#'
#' @return A character vector of start variable names
#' @keywords internal
.extract_start_variables <- function(variable_start) {
  derived_var_pattern <- "DerivedVar::\\[(.+?)\\]|DerivedVar::\\[\\]"
  matches <- regmatches(
    variable_start, regexec(derived_var_pattern, variable_start))

  if (length(matches[[1]]) < 2) {
    return(character(0))
  }

  content <- matches[[1]][2]

  if (is.na(content) || nchar(trimws(content)) == 0) {
    return(character(0))
  }

  start_vars <- purrr::map(
    strsplit(content, ",")[[1]],
    trimws
  )
  return(start_vars)
}

#' Check if a start variable is a database column
#'
#' @param start_var The start variable string to check
#'
#' @return TRUE if it matches the database::column pattern, FALSE otherwise
#' @keywords internal
.is_database_column_reference <- function(start_var) {
  database_column_pattern <- "\\w+::\\w+"
  return(grepl(database_column_pattern, start_var))
}
