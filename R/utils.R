#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Margins as character vector.
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
is_numeric <- function(chars) {
  return (suppressWarnings(!is.na(as.numeric(chars))))
}

#' Check if recFrom is a range for a variable details row.
#'
#' @param var_details_row Variable details sheet row.
#'
#' @return Whether recFrom is a range.
is_rec_from_range <- function(var_details_row) {
  margins <- get_margins(var_details_row[[pkg.env$columns.recFrom]])
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
get_var_details_row_indices <- function (var_details_sheet, var_name) {
  return (which(var_details_sheet$variable == var_name, arr.ind = TRUE))
}

#' Get variable row from variable sheet.
#'
#' @param var_name Variable name.
#' @param vars_sheet Variable sheet data frame.
#'
#' @return Variable row.
get_var_sheet_row <- function (var_name, vars_sheet) {
  return (vars_sheet[which(vars_sheet$variable == var_name, arr.ind = TRUE)[1],])
}

#' Get variable name from variableStart using database name.
#'
#' @param var_details_row A variable details row.
#' @param db_name Name of database to extract from.
#'
#' @return character The name of the start variable.
get_start_var_name <- function(var_details_row, db_name) {
  # The value of the variableStart column for this variable details row
  start_variables = var_details_row$variableStart

  # The regex that will be used to pluck the name of the start variable from a
  # start variable string. For example, if the db_name is cchs2001_p and the
  # list is cchs2001_p::RACA_6A,this regex will pluck out RACA_6A
  start_variable_with_db_regex <- paste0(db_name, "::(.+?)$")
  # Regex to pluck out the start variable from a default variable string
  # For example, if the string is [ADL_01], this regex will pull out ADL_01
  default_start_var_regex <- "\\[(.+?)\\]"
  # Split the start variable column into each start variable string
  # For example, db1::var1, db2::var2, [var3] would be split into
  # [db1::var1, db2::var2, [var3]]
  start_variables_split <- strsplit(start_variables, ",")
  # The name of the start variable for the passed db_name argument
  start_variable_for_db <- NA
  # The name of the default start variable for this start variable string
  default_start_var <- NA
  # Go through each of the start variable strings to find either one for this
  # db or a default one
  for(i in seq_len(length(start_variables_split[[1]]))) {
    current_start_variable_str <- start_variables_split[[1]][i]

    # Get regex match for variable start
    db_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(start_variable_with_db_regex, current_start_variable_str))
    possible_db_start_var <- db_var_regex_matches[[1]][2]
    # If we found a start variable for this db then assign it and break out
    # of the loop since we don't care about the default var
    if(!is.na(possible_db_start_var)) {
      start_variable_for_db <- possible_db_start_var
      break
    }

    # Find the matches for the default var regex in this start variable string
    default_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(default_start_var_regex, current_start_variable_str))
    # Either the name of the default var or NA if it does not match
    possible_default_var <- default_var_regex_matches[[1]][2]
    if(!is.na(possible_default_var)) {
      default_start_var <- possible_default_var
    }
  }

  if(!is.na(start_variable_for_db)) {
    return(start_variable_for_db)
  } else if(!is.na(default_start_var)) {
    return(default_start_var)
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
#' @param is_start_var boolean if the passed variable is variable start
#'
#' @return `var_type` data type.
get_variable_type_data_type <- function (var_details_rows, var_type, is_start_var) {
  is_categorical <- var_type %in% c(pkg.env$var_details_cat, pkg.env$var_cat)
  if (is_categorical) {
    char_var_details_rows <- ifelse(is_start_var,
                                    var_details_rows[!is_numeric(var_details_rows[[pkg.env$columns.recFrom]]), ],
                                    var_details_rows[!is_numeric(var_details_rows[[pkg.env$columns.recTo]]), ])
    if (length(char_var_details_rows) > 0) return (pkg.env$node_attr.dataType.string)
    return (pkg.env$node_attr.dataType.integer)
  }
  return (pkg.env$node_attr.dataType.float)
}

#' ID role creation
#'
#' Creates ID row for rec_with_table
#'
#' @param data the data that the ID role row is created for
#' @param id_role_name name for the role that ID is created from
#' @param database_name the name of the database
#' @param variables variables sheet containing variable information
#'
#' @return data with the ID row attached
create_id_row <- function(data, id_role_name, database_name, variables){
  # Flag to check for presence of data_name before execusion
  keep_data_name <- FALSE
  if(!is.null(data[["data_name"]])){
    keep_data_name <- TRUE
  }
  # Check for role or variables
  id_cols <- c()
  if(!is.null(id_role_name$feeder_vars)){
    id_cols <- append(id_cols,id_role_name$feeder_vars)
  }else if (!is.null(id_role_name$feeder_roles)){
    id_cols <- append(id_cols, select_vars_by_role(roles = id_role_name$feeder_roles, variables = variables))
  }else {
    message("id_role_name does not contain feeder_roles or feeder_vars.
                  No ID column was created")
  }
  if("data_name" %in% id_role_name$feeder_vars && is.null(data[["data_name"]])){
    data[["data_name"]] <- database_name
  }
  # tmp_column is a column name it cannot be passed as a string as it breaks the unite function
  tmp_data <- tidyr::unite(data = data, tmp_column, sep = "_", id_cols)
  data[[id_role_name$var_name]] <- tmp_data$tmp_column

  # Remove data_name if it was generated and not present before
  if((!is.null(data[["data_name"]]) && (!keep_data_name))){
    data[["data_name"]] <- NULL
  }

  return(data)
}

#' Create label list element
#'
#' A data labeling utility function for creating individual variable labels
#'
#' @param variable_rows all variable details rows containing 1 variable information
#'
#' @return a list containing labels for the passed variable
create_label_list_element <- function(variable_rows) {
  ret_list <- list(
    # Variable type
    type = NULL,
    # Variable value units
    unit = NULL,
    # variable label long
    label_long = NULL,
    # variable label
    label = NULL,
    # Variable value label
    values = c(),
    # Variable value label long
    values_long = c()
  )
  first_row <- variable_rows[1, ]
  ret_list$type <-
    as.character(first_row[[pkg.env$columns.ToType]])
  ret_list$unit <-
    as.character(first_row[[pkg.env$columns.Units]])
  ret_list$label_long <-
    as.character(first_row[[pkg.env$columns.VariableLabel]])
  ret_list$label <-
    as.character(first_row[[pkg.env$columns.label]])
  if (is_equal(ret_list$type, pkg.env$columns.value.CatType)) {
    for (row_index in seq_len(nrow(variable_rows))) {
      single_row <- variable_rows[row_index, ]
      # Verify type stays the same
      if (!is_equal(
        ret_list$type,
        as.character(single_row[[pkg.env$columns.ToType]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.env$columns.Variable]]),
            "does not contain all identical",
            pkg.env$columns.ToType,
            "variable cant change variable type for different values"
          )
        )
      }
      # Verify unit is identical
      if (!is_equal(
        ret_list$unit,
        as.character(single_row[[pkg.env$columns.Units]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.env$columns.Variable]]),
            "does not contain all identical",
            pkg.env$columns.Units,
            "variable cant change unit type for different values"
          )
        )
      }
      # Verify variable label is identical
      if (!is_equal(
        ret_list$label_long,
        as.character(single_row[[pkg.env$columns.VariableLabel]])
      )) {
        stop(
          paste(
            as.character(single_row[[pkg.env$columns.Variable]]),
            "does not contain all identical",
            pkg.env$columns.VariableLabel,
            "variable cant change variableLabel for different values. VAL1:",
            ret_list$label_long,
            "VAL2:",
            as.character(single_row[[pkg.env$columns.VariableLabel]])
          )
        )
      }
      value_being_labeled <-
        as.character(single_row[[pkg.env$columns.recTo]])
      value_being_labeled <-
        format_recoded_value(value_being_labeled, ret_list$type)
      ret_list$values[[as.character(single_row[[
        pkg.env$columns.CatLabel]])]] <-
        value_being_labeled
      ret_list$values_long[[as.character(single_row[[
        pkg.env$columns.CatLabelLong]])]] <-
        value_being_labeled
    }
  }

  return(ret_list)
}

#' @title label_data
#'
#' @description Attaches labels to the data_to_label to preserve metadata
#'
#' @param label_list the label list object that contains extracted labels
#' from variable details
#' @param data_to_label The data that is to be labeled
#' @importFrom sjlabelled set_labels set_label set_label<-
#'
#' @return Returns labeled data
label_data <- function(label_list, data_to_label) {
  for (variable_name in names(label_list)) {
    if (is.na(label_list[[variable_name]]$type)) {
      warning(
        paste(
          variable_name,
          "is missing from variable_details or variables
          (if it was also passed) please verify correct spelling"
        )
      )
      next()
    }
    if (label_list[[variable_name]]$type == pkg.env$columns.value.CatType) {
      if (class(data_to_label[[variable_name]]) != "factor") {
        data_to_label[[variable_name]] <-
          factor(data_to_label[[variable_name]])
      }
      data_to_label[[variable_name]] <-
        sjlabelled::set_labels(data_to_label[[variable_name]],
                               labels = unlist(label_list[[variable_name]]$values)
        )
      attr(data_to_label[[variable_name]], "labels_long") <-
        label_list[[variable_name]]$values_long
    } else {
      if (class(data_to_label[[variable_name]]) == "factor") {
        data_to_label[[variable_name]] <-
          as.numeric(levels(data_to_label[[variable_name]])
                     [data_to_label[[variable_name]]])
      } else {
        tmp <- as.numeric(data_to_label[[variable_name]])
        if(sum(is.na(tmp))!= length(tmp)){
          data_to_label[[variable_name]] <-
            as.numeric(data_to_label[[variable_name]])
        }
      }
    }
    sjlabelled::set_label(data_to_label[[variable_name]]) <-
      label_list[[variable_name]]$label
    attr(data_to_label[[variable_name]], "unit") <-
      label_list[[variable_name]]$unit
    attr(data_to_label[[variable_name]], "label_long") <-
      label_list[[variable_name]]$label_long
  }

  return(data_to_label)
}

#' Vars selected by role
#'
#' Selects variables from variables sheet based on passed roles
#'
#' @param roles a vector containing a single or multiple roles to match by
#' @param variables the variables sheet containing variable info
#'
#' @return a vector containing the variable names that match the passed roles
select_vars_by_role  <- function(roles, variables){
  # Reduce row looping by only looping over only unique combinations
  unique_roles <- unique(variables[[pkg.env$columns.Role]])
  valid_patern <- c()
  for (role_patern in unique_roles) {
    # Split by commas to avoid partial matches being false positives
    role_list <- strsplit(role_patern, ",")[[1]]
    for (role in role_list){
      role <- trimws(role)
      if(role %in% roles){
        valid_patern <- append(valid_patern, role_patern)
      }
    }
  }

  # Catch roles not being present in variables
  if(length(valid_patern)<1){
    stop(paste0(roles, " is not present in variabes"))
  }
  ret <- variables[variables[[pkg.env$columns.Role]] %in% valid_patern, pkg.env$columns.Variable]
  if(is.data.frame(ret)){
    ret <- as.character(ret[[pkg.env$columns.Variable]])
  }else{
    ret <- as.character(ret)
  }

  return(ret)
}

#' @title Set Data Labels
#' @description sets labels for passed database, Uses the names of final
#' variables in variable_details/variables_sheet as well as the labels contained
#' in the passed dataframes
#'
#' @param data_to_label newly transformed dataset
#' @param variable_details variable_details.csv
#' @param variables_sheet variables.csv
#'
#' @return labeled data_to_label
#'
#' @export
set_data_labels <-
  function(data_to_label,
           variable_details,
           variables_sheet = NULL) {
    # Extract variables in the data
    variable_names <- unique(colnames(data_to_label))
    # extract only relevant variable info
    if (!is.null(variable_details)) {
      variable_details[[pkg.env$columns.Variable]] <- sapply(
        variable_details[[pkg.env$columns.Variable]], trimws)
      variable_details <-
        variable_details[variable_details[[pkg.env$columns.Variable]]
                         %in% variable_names, ]
      if (is.null(variables_sheet)) {
        variable_details[[pkg.env$columns.label]] <- NA
        variable_details[[pkg.env$columns.VariableLabel]] <-
          NA
      }
    }
    if (!is.null(variables_sheet)) {
      variables_sheet[[pkg.env$columns.Variable]] <- sapply(
        variables_sheet[[pkg.env$columns.Variable]], trimws)
      variables_sheet <-
        variables_sheet[variables_sheet[[pkg.env$columns.Variable]] %in%
                          variable_names, ]
      variable_details <-
        update_variable_details_based_on_variable_sheet(
          variable_sheet = variables_sheet,
          variable_details = variable_details
        )
    }
    label_list <- NULL
    for (variable_name in variable_names) {
      rows_to_process <-
        variable_details[variable_details[[
          pkg.env$columns.Variable]] == variable_name, ]
      label_list[[variable_name]] <-
        create_label_list_element(rows_to_process)
    }
    data_to_label <- label_data(label_list, data_to_label)

    return(data_to_label)
  }
