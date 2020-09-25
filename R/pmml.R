#' Creates a PMML document from an xFlow document.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param vars_sheet A data frame representing a variables sheet.
#' @param db_name A string containing the name of the database that holds
#'  the start variables. Should match up with one of the databases in the
#'  databaseStart column.
#' @param vars_to_convert A vector of strings containing the names of variables
#'  from the variable column in the variable details sheet that should be
#'  converted to PMML. Passing in an empty vector will convert all the variables.
#'
#' @return A PMML document.
#'
#' @examples
#'
#' @export
xflow_to_pmml <- function(var_details_sheet, vars_sheet, db_name, vars_to_convert = NULL) {
  doc <- XML::xmlNode("PMML", namespaceDefinitions=c("http://www.dmg.org/PMML-4_4"), attrs=c(version="4.4"))
  dict <- XML::xmlNode("DataDictionary")
  recognized_vars_to_convert <- c(character(0))

  if (is.null(vars_to_convert)) vars_to_convert <- vars_sheet$variable

  for (var_to_convert in vars_to_convert) {
    var_details_rows <- var_details_sheet[get_var_details_row_indices(var_details_sheet, var_to_convert),]
    if (nrow(var_details_rows) == 0) {
      print(paste0("Skipping ", var_to_convert, ": No rows with that name found in var_details_sheet."))
      next
    }

    var_db_details_rows <- get_var_details_rows(var_details_sheet, var_to_convert, db_name)

    if (nrow(var_db_details_rows) > 0) {
      var_start_name <- get_start_var_name(var_db_details_rows[1,], db_name)
      data_field <- build_data_field_for_start_var(var_start_name, var_db_details_rows)
      data_field <- add_data_field_children_for_start_var(data_field, var_db_details_rows)
      recognized_vars_to_convert <- c(recognized_vars_to_convert, var_to_convert)
    } else {
      data_field <- build_data_field_for_var(var_to_convert, vars_sheet)
    }

    if (is.null(data_field)) print(paste0("Skipping ", var_to_convert, ": Unable to determine fromType."))
    else dict <- XML::append.xmlNode(dict, data_field)
  }

  number_of_fields <- XML::xmlSize(dict)
  XML::xmlAttrs(dict) <- c(numberOfFields=number_of_fields)

  if (number_of_fields == 0) {
    print("Unable to recognize any requested variables.")
  } else {
    trans_dict <- build_trans_dict(vars_sheet, var_details_sheet, recognized_vars_to_convert, db_name)
    doc <- XML::append.xmlNode(doc, dict, trans_dict)
  }

  return (doc)
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
get_var_sheet_row <- function (var_name, var_details_sheet) {
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
    if (length(char_var_details_rows) > 0) return ("string")
    return ("integer")
  }
  return ("float")
}
