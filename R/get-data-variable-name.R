#' @title Get Data Variable Name
#'
#' @name get_data_variable_name
#'
#' @description Retrieves the name of the column inside data to
#' use for calculations
#'
#' @param data_name name of the database being checked
#' @param data database being checked
#' @param row the row from variable details that contains
#' information on this variable
#' @param recoded_varname the name of the recoded variable
#'
#' @return the data equivalent of var_name
#' @keywords internal
get_data_variable_name <-
  function(data_name, data, row, recoded_varname) {
    varstart_col <- pkg.env$columns.variableStart
    result <- character()

    # a comma-delimited string of variable names
    varstart_names <- as.character(row[[varstart_col]])

    db_prefix = paste0(data_name, "::")
    has_db_var <- grepl(db_prefix, varstart_names)
    has_default_var <- grepl("\\[", varstart_names)

    if (has_db_var) {
      varstart_names_list <- trimws(strsplit(varstart_names, ",")[[1]])

      # find exact var name
      for (name in varstart_names_list) {
        if (startsWith(name, db_prefix)) {
          result <- strip_prefix(name)
          break
        }
      }

    } else if (has_default_var) {
      # At this point there are no db-vars for `data_name`, but there may be
      # variables for unknown databases. Now we'll check for default vars, and
      # take the first one.
      result <- stringr::str_match(varstart_names, "\\[(.*?)\\]")[, 2]

    } else {
      # no db-vars and no default-vars
      stop(
        paste(
          "The row", row, "for the variable", recoded_varname,
          "does not contain the database being checked (", data_name, ")",
          "in its variable start. The default is also missing.",
          "Please double check if this variable should have",
          data_name, "included in its databaseStart"
        )
      )
    }

    result <- trimws(result)
    return(result)
  }
