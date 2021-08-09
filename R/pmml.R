#' Creates a PMML document from variable and variable details sheets for specified database.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param vars_sheet A data frame representing a variables sheet.
#' @param db_name A string containing the name of the database that holds
#'  the start variables. Should match up with one of the databases in the
#'  databaseStart column.
#' @param vars_to_convert A vector of strings containing the names of variables
#'  from the variable column in the variable details sheet that should be
#'  converted to PMML. Passing in an empty vector will convert all the variables.
#' @param custom_function_files Optional vector of strings containing the paths
#' to the R files with custom functions references in the variable details sheet.
#'
#' @return A PMML document.
#'
#' @examples
#' var_details_sheet <-
#'data.frame(
#'  "variable" = rep(c("A", "B", "C"), each = 3),
#'  "dummyVariable" = c("AY", "AN", "ANA", "BY", "BN", "BNA", "CY", "CN", "CNA"),
#'  "toType" = rep("cat", times = 9),
#'  "databaseStart" = rep("tester", times = 9),
#'  "variableStart" = rep(
#'    c("tester::startA", "tester::startB", "tester::startC"),
#'    each = 3
#'  ),
#'  "fromType" = rep("cat", times = 9),
#'  "recTo" = rep(c("1", "2", "NA::a"), times = 3),
#'  "numValidCat" = rep("2", times = 9),
#'  "catLabel" = rep(c("Yes", "No", "Not answered"), times = 3),
#'  "catLabelLong" = rep(c("Yes", "No", "Not answered"), times =
#'                         3),
#'  "recFrom" = rep(c("1", "2", "9"), times = 3),
#'  "catStartLabel" = rep(c("Yes", "No", "Not answered"), times =
#'                          3),
#'  "variableStartShortLabel" = rep(c("Group A", "Group B", "Group C"), each =
#'                                    3),
#'  "variableStartLabel" = rep(c("Group A", "Group B", "Group C"), each =
#'                               3),
#'  "units" = rep("NA", times = 9),
#'  "notes" = rep("This is not real data", times = 9)
#')
#'vars_sheet <-
#'  data.frame(
#'    "variable" = c("A", "B", "C"),
#'    "label" = c("Group A", "Group B", "Group C"),
#'    "labelLong" = c("Group A", "Group B", "Group C"),
#'    "section" = rep("tester", times=3),
#'    "subject" = rep("tester",times = 3),
#'    "variableType" = rep("Categorical", times=3),
#'    "databaseStart" = rep("tester", times = 3),
#'    "units" = rep("NA", times = 3),
#'    "variableStart" = c("tester::startA", "tester::startB", "tester::startC")
#'  )
#' db_name <- "tester"
#' vars <- c("A", "B", "C")
#'
#' actual_pmml <- recode_to_pmml(
#' var_details_sheet,
#' vars_sheet,
#' db_name,
#' vars
#' )
#'
#' @export
recode_to_pmml <- function(var_details_sheet, vars_sheet, db_name, vars_to_convert = NULL, custom_function_files = NULL) {
  doc <- XML::xmlNode(pkg.env$node_name.pmml, namespaceDefinitions=c(pkg.env$node_namespace.pmml), attrs=c(version=pkg.env$node_attr.pmml_version))
  dict <- XML::xmlNode(pkg.env$node_name.data_dict)
  recognized_vars_to_convert <- c(character(0))

  # 2. Get the vector of variables names that we will need to add to the
  # PMML document
  all_vars_to_convert <- c()
  # The initial list variables from which we will use to get all the other ones
  initial_vars_to_convert <- vars_to_convert
  # If the user did not pass it then the initial list is the variables
  # from the variables sheet
  if(is.null(vars_to_convert)) {
    initial_vars_to_convert <- vars_sheet$variable
  }
  # Go through each variable, find all the variables required to derive it
  # until we reach the end and then add them all to the master list
  for(var_to_convert in initial_vars_to_convert) {
    all_vars_to_convert <- c(
      all_vars_to_convert,
      var_to_convert,
      get_all_start_vars(
        var_to_convert,
        db_name,
        var_details_sheet
      )
    )
  }
  all_vars_to_convert <- unique(all_vars_to_convert)

  # 3. Get the list of DefineFunction nodes which represents all the
  # custom functions parsed from the files in the custom_function_files argument
  custom_function_nodes <- list()
  # Parse the custom_function_files argument if it was provided
  if (!is.null(custom_function_files)) {
    # Iterate through all the custom function files
    for (custom_function_file_index in seq_len(length(custom_function_files))) {
      # Convert the current one to a PMML string and parse it using the XML library
      custom_function_file_pmml_string <-
        pmml::get_pmml_string_from_r_file(custom_function_files[custom_function_file_index],
                                          src_file = TRUE)
      custom_function_file_pmml <-
        XML::xmlTreeParse(custom_function_file_pmml_string)

      # Get the LocalTransformations node which has all the DefineFunction
      # nodes which we will need to add to the TransformationDictionary node
      local_transformations_node <-
        custom_function_file_pmml$doc$children[[pkg.env$node_name.pmml]][[pkg.env$node_name.local_transformations]]
      # Iterate through each DefineFunction node in the LocalTransformations node
      for (define_function_index in seq_len(length(local_transformations_node))) {
        custom_function_nodes[[length(custom_function_nodes) + 1]] <- local_transformations_node[[define_function_index]]
      }
    }
  }

  for (var_to_convert in all_vars_to_convert) {
    var_details_rows <- var_details_sheet[get_var_details_row_indices(var_details_sheet, var_to_convert),]
    if (nrow(var_details_rows) == 0) {
      print(paste0("Skipping ", var_to_convert, ": No rows with that name found in var_details_sheet."))
      next
    }

    var_db_details_rows <- get_var_details_rows(var_details_sheet, var_to_convert, db_name)

    data_field <- NA
    # If it is not a derived variable then we can add the start variable for
    # this variable to the DataDictionary node
    if (!is_derived_var(var_db_details_rows)) {
      if (nrow(var_db_details_rows) > 0) {
        var_start_name <-
          get_start_var_name(var_db_details_rows[1, ], db_name)

        # If the start variable for thie current variable is in the
        # variables columns then it needs to be added as a DerivedVariable
        # and not as a DataField. 
        if(var_start_name %in% var_details_sheet[[pkg.env$columns.Variable]]) {
          recognized_vars_to_convert <- c(
              recognized_vars_to_convert, var_start_name)
        } else {
          data_field <-
            build_data_field_for_start_var(var_start_name, var_db_details_rows)
        }

        recognized_vars_to_convert <-
          c(recognized_vars_to_convert, var_to_convert)
      } else {
        data_field <- build_data_field_for_var(var_to_convert, vars_sheet)
      }

      if (is.null(data_field) | is.na(data_field))
        print(paste0("Skipping ", var_to_convert, ": Unable to determine fromType."))
      else
        dict <- XML::append.xmlNode(dict, data_field)
    }
    # If it is a derived variable, the start variables are variables in the
    # Variable column and should not be added as DataField nodes. They will
    # be added as DerivedField nodes in the next section.
    # Add this derived variable to the vector of variables names that need to
    # be converted to a DerivedField
    else {
      recognized_vars_to_convert <-
        c(recognized_vars_to_convert, var_to_convert)
    }
  }

  number_of_fields <- XML::xmlSize(dict)
  XML::xmlAttrs(dict) <- c(numberOfFields=number_of_fields)

  if (number_of_fields == 0) {
    print("Unable to recognize any requested variables.")
  } else {
    custom_function_names <- c()
    for(custom_function_node in custom_function_nodes) {
      custom_function_names <- c(
        custom_function_names,
        XML::xmlGetAttr(custom_function_node, pkg.env$node_attr.DefineFunction.name)
      )
    }
    recognized_vars_to_convert <- unique(recognized_vars_to_convert)
    trans_dict <- build_trans_dict(vars_sheet, var_details_sheet, recognized_vars_to_convert, db_name, custom_function_names)

    # Iterate through each DefineFunction node in the LocalTransformations node
    for (custom_function_node in custom_function_nodes) {
      # Add the current one to the TransformationDictionary node
      trans_dict <-
        XML::addChildren(trans_dict, custom_function_node)
    }

    doc <- XML::append.xmlNode(doc, dict, trans_dict)
  }

  return (doc)
}

derived_var_regex <- "DerivedVar::\\[(.+?)\\]|DerivedVar::\\[\\]"

#' Whether this variable details row is for a derived variable
#'
#' @param variable_details_row data.frame One row from a variable details sheet
#'
#' @return boolean True if it is a derived variable, false otherwise
#' @export
#'
#' @examples
is_derived_var <- function(variable_details_row) {
  return(length(grep(
    derived_var_regex, variable_details_row[1, pkg.env$columns.VariableStart]
  )) > 0)
}

#' Returns all the variable names, including the ones from children variables
#' that a variable depends on
#'
#' @param derived_var string The name of the variable
#' @param variable_details_sheet data.frame A data frame that contains an entire
#' variables details sheet
#'
#' @return vector of strings Contains the names of the derived from variables
#' @export
#'
#' @examples
get_all_start_vars <-
  function(var_name, db_name, variable_details_sheet) {
    all_start_vars <- c()

    variable_details_rows <- variable_details_sheet[get_var_details_row_indices(variable_details_sheet, var_name), ]

    if(is_derived_var(variable_details_rows)) {
      current_derived_from_vars <-
          get_start_vars_for_derived_var(var_name, variable_details_sheet)

      all_start_vars <- c(
        all_start_vars,
        current_derived_from_vars
      )

      # Get the variables that the children depend on if they any of them
      # are a derived variable
      # This is the list of all the variables that the derived variable depends
      # on
      # Go through each child variable for this derived variable
      for (derived_from_var in current_derived_from_vars) {
        # Get all the variable details rows for this child variable
        variable_details_row_for_current_var <-
          variable_details_sheet[get_var_details_row_indices(variable_details_sheet, derived_from_var),]
        # If it is a derived variable get all the variables it depends on and
        # add it to the master list
        if (is_derived_var(variable_details_row_for_current_var)) {
          all_start_vars <- c(
            all_start_vars,
            get_all_start_vars(
              derived_from_var,
              db_name,
              variable_details_sheet
            )
          )
        }
      }
    } else {
      start_var_name <- get_start_var_name(variable_details_rows, db_name)

      # If the start variable for this variable is in the variables
      # column then it has start variables also. Get them and add them
      # to the master list
      if(start_var_name %in% variable_details_sheet[pkg.env$pkg.env$columns.Variable]) {
        all_start_vars <- c(
          all_start_vars,
          start_var_name,
          get_all_start_vars(start_var_name, db_name, variable_details_sheet)
        )
      } else {
        all_start_vars <- c(all_start_vars, start_var_name)
      }
    }

    # 3. Return the vector of derived from variables removing all duplicates
    return(unique(all_start_vars))
  }

#' Returns the immediate derived from variables for a derived variable
#'
#' @param derived_var string The name of the derived variable
#' @param variable_details_sheet data.frame A data frame containing a
#' variable details sheet
#'
#' @return vector of strings Contains the derived from variables
#' @export
#'
#' @examples
get_start_vars_for_derived_var <-
  function(derived_var, variable_details_sheet) {
    # 1. Get the string of derived from variables from the variableStart column
    # For example, if the column value is DerivedVar::[ADL_01, ADL_02], this
    # will extract "ADL_01, ADL_02"

    # Get all the variable details rows for this derived variable
    derived_var_variable_details_rows <-
      variable_details_sheet[get_var_details_row_indices(variable_details_sheet, derived_var), ]
    # Get the value of the variableStart column with the DerivedVar string
    derived_from_vars_str <-
      derived_var_variable_details_rows[1, pkg.env$columns.VariableStart]
    # Extract the string we want
    derived_from_string <- regmatches(derived_from_vars_str,
                                      regexec(derived_var_regex, derived_from_vars_str))[[1]][2]

    # 2. Split the string into its variable names and trim each one before
    # returning it

    # Each variable name is seperated by a comma
    return(trimws(strsplit(derived_from_string, ",")[[1]]))
  }
