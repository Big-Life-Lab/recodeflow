#' Returns the start variables for a variable and database
#'
#' @param variable the name of the variable
#' @param database the name of the database
#' @param variables_sheet a data.frame containing the variables sheet for
#' the project
#' @param variable_details_sheet a data.frame containing the variable details
#' sheet for the project
#' @returns the list of start variables. Each start variable is encoded as a
#' named list with the following fields:
#' name: The name of the start variable
#' type: The type of the start variable which can be one of the following
#' values:
#'     database: The start variable is part of the original data from the
#'     database
#'     table:
#'     non-derived: A non-derived recoded variable
#'     derived: A derived recoded variable
#' @export
get_start_variables <- function(
    variable,
    database,
    variables_sheet,
    variable_details_sheet) {
  .validate_params(variable, database, variables_sheet, variable_details_sheet)

  variable_sheet_rows <- variables_sheet[
    variables_sheet[[pkg.env$columns.variable]] == variable &
      grepl(database, variables_sheet[[pkg.env$columns.databaseStart]]),
  ]
  if (nrow(variable_sheet_rows) == 0) {
    cli::cli_abort(c(
      "{.arg variables_sheet} is missing rows for the {.arg variable}
      and {.arg database} argument",
      "x" = "Found 0 rows for the variable {variable} and database {database}"
    ))
  }

  variable_starts <- variable_sheet_rows[[pkg.env$columns.variableStart]]
  start_variables <- purrr::reduce(
    variable_starts,
    function(start_variables, variable_start) {
      current_start_variables <- if (
        is_derived_var(data.frame(variableStart = c(variable_start)))
      ) {
        .get_derived_variable_start_variables(
          variable, database, variables_sheet, variable_details_sheet
        )
      } else {
        .get_non_derived_variable_start_variables(
          variable, database, variable_start
        )
      }
      if (is.null(current_start_variables)) {
        cli::cli_abort(c(
          "Can't figure out the start variables for a variableStart value",
          "i" = "The variable start value is {variable_start}"
        ))
      }
      if (is.null(start_variables)) {
        return(current_start_variables)
      }
      if (
        !.are_start_variables_equal(start_variables, current_start_variables)
      ) {
        cli::cli_abort(c(
          "Start variables must be the same for all rows of a variable for a
          database",
          "i" = "Found start variables {start_variables}",
          "i" = "Found other start variables {current_start_variables}",
          "x" = "These should be the same"
        ))
      }
      return(current_start_variables)
    },
    .init = NULL
  )
  return(start_variables)
}

#' Get the type of the start variable for a derived variable
#'
#' @param variable the name of the start variable
#' @param database the name of the database where the start variable is
#' @param variables_sheet a data.frame containing variables sheet for the
#' project
#'
#' @return One of the following string:
#' derived: For a start variable that is a derived variable
#' non-derived: For a start variable that is a non-derived variable
#'
#' @keywords internal
.get_derived_start_variable_type <- function(
    variable,
    database,
    variables_sheet) {
  variable_sheet_rows <- variables_sheet[
    variables_sheet[[pkg.env$columns.variable]] == variable &
      grepl(database, variables_sheet[[pkg.env$columns.databaseStart]]),
  ]
  if (nrow(variable_sheet_rows) == 0) {
    cli::cli_abort(c(
      "The variables sheet must have rows for the start variable of a derived
      variable for the passed database parameter",
      "i" = "0 rows found for the variable {variable} for the database
             {database}"
    ))
  }

  variable_start <- unique(
    variable_sheet_rows[[pkg.env$columns.variableStart]]
  )[1]
  if (is_derived_var(data.frame(variableStart = c(variable_start)))) {
    return("derived")
  } else {
    return("non-derived")
  }
}

#' Check a string parameter
#' Checks if the parameter value is a scalar string and if it has at least
#' one character
#'
#' @param x the value to check
#' @param param_name the name of the parameter
#'
#' @returns TRUE if the value passes all checks. Throws errors otherwise.
#' @keywords internal
.test_required_string_param <- function(x, param_name) {
  if (!checkmate::test_string(x)) {
    cli::cli_abort(c(
      "{.arg {param_name}} must be a scalar string",
      "x" = "{x} is a {typeof(x)}"
    ))
  }

  if (nchar(x) == 0) {
    cli::cli_abort(c(
      "{.arg {param_name}} must have at least 1 character",
      "x" = "{x} has {nchar(x)}"
    ))
  }

  return(TRUE)
}

#' Check a data.frame parameter
#'
#' @param x the value to check
#' @param required_cols the list of column names that the value has to have
#' @param param_name the name of the parameter
#'
#' @returns TRUE if the value passes all checks. Throws errors otherwise.
#' @keywords internal
.test_data_frame_param <- function(x, required_cols, param_name) {
  if (!checkmate::test_data_frame(x)) {
    cli::cli_abort(c(
      "{.arg {param_name}} must be a data.frame",
      "x" = "{.arg x} is a {typeof(x)}"
    ))
  }

  if (!checkmate::test_names(colnames(x), must.include = required_cols)) {
    missing_cols <- required_cols[!required_cols %in% names(x)]
    cli::cli_abort(c(
      "{.arg {param_name}} is missing required columns",
      "x" = "{missing_cols} are missing"
    ))
  }

  return(TRUE)
}

#' Validates the parameters to the get_start_variables function
#'
#' @param variable
#' @param database
#' @param variables_sheet
#' @param variable_details_sheet
#'
#' @returns NULL if validation passes. Throws errors otherwise.
#' @keywords internal
.validate_params <- function(
    variable,
    database,
    variables_sheet,
    variable_details_sheet) {
  .test_required_string_param(variable, "variable")

  .test_required_string_param(database, "database")

  .test_data_frame_param(
    variables_sheet,
    c(
      pkg.env$columns.variable,
      pkg.env$columns.databaseStart,
      pkg.env$columns.variableStart
    ),
    "variables_sheet"
  )

  .test_data_frame_param(
    variable_details_sheet,
    c(
      pkg.env$columns.variable,
      pkg.env$columns.databaseStart
    ),
    "variable_details_sheet"
  )
}

#' Check if the two list of start variables are the same
#' The structure of the list should match the return of the get_start_variables
#' function
#'
#' @param x
#' @param y
#'
#' @returns a boolean
#' @keywords internal
.are_start_variables_equal <- function(x, y) {
  if (length(x) != length(y)) {
    return(FALSE)
  }

  for (current_x in x) {
    found_in_y <- purrr::detect(y, function(current_y) {
      return(
        current_y$name == current_x$name & current_y$type == current_x$type
      )
    })
    if (is.null(found_in_y)) {
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Gets the start variables for a derived variable for a database
#'
#' @param variable the name of the derived variable
#' @param database the name of the database
#' @param variables_sheet a data.frame containing the variables sheet for
#' the project
#' @param variable_details_sheet a data.frame containing the variable details
#' sheet for the project
#'
#' @returns a list containing the start variables. The structure follows
#' that of the get_start_variables function.
#' @keywords internal
.get_derived_variable_start_variables <- function(
    variable,
    database,
    variables_sheet,
    variable_details_sheet) {
  # For derived variables where the variableStart is different for different
  # databases, the mapping from a database to its variableStart is not in the
  # variables sheet but in the variable details sheet
  details_rows <- variable_details_sheet[
    variable_details_sheet[[pkg.env$columns.variable]] == variable &
      grepl(database, variable_details_sheet[[pkg.env$columns.databaseStart]]),
  ]
  if (nrow(details_rows) == 0) {
    cli::cli_abort(c(
      "The variable details sheet must have rows for the variable parameter",
      "i" = "No rows found for the variable {variable}"
    ))
  }

  variable_details_variable_starts <-
    details_rows[[pkg.env$columns.variableStart]]
  start_variables <- purrr::reduce(
    variable_details_variable_starts,
    function(start_variables, variable_start) {
      feeder_vars <- get_feeder_vars(variable_start, database)
      current_start_variables <- purrr::map(feeder_vars, function(feeder_var) {
        if (is_table_feeder_var(feeder_var)) {
          table_name <- get_table_name(feeder_var)
          return(list(name = table_name, type = "table"))
        } else {
          # Look up the variable in the variables sheet to determine its
          # actual type
          var_type <- .get_derived_start_variable_type(
            feeder_var, database, variables_sheet
          )
          return(list(name = feeder_var, type = var_type))
        }
      }) %>% unname(.) # feeder_vars is a named vector which are added by
      # purrr::map to the result. This removes them.
      if (is.null(start_variables)) {
        return(current_start_variables)
      }
      if (
        !.are_start_variables_equal(start_variables, current_start_variables)
      ) {
        cli::cli_abort(c(
          "Start variables must be the same for all rows of a variable for a
          database",
          "i" = "Found start variables {start_variables}",
          "i" = "Found other start variables {current_start_variables}",
          "x" = "These should be the same"
        ))
      }
      return(current_start_variables)
    },
    .init = NULL
  )
  return(start_variables)
}

#' Get the start variables for a non-derived variable for a database
#'
#' @param variable the name of the non-derived variable
#' @param database the name of the database
#' @param variable_start the variableStart column value for the variable
#'
#' @returns a list containing the start variables. The structure follows
#' that of the return of the get_start_variables function.
.get_non_derived_variable_start_variables <- function(
    variable,
    database,
    variable_start) {
  tryCatch(
    {
      start_variables <- purrr::map(
        get_data_variable_name(
          database,
          data.frame(),
          data.frame(variableStart = c(variable_start)),
          variable
        ),
        function(var) {
          return(list(name = gsub("\\[|\\]", "", var), type = "database"))
        }
      )
      return(start_variables)
    },
    error = function(e) {
      if (!grepl("The row", e$message)) {
        stop(e)
      }
      return(NULL)
    }
  )
}
