#' Creates a PMML document from an document.
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
recode_to_pmml <- function(var_details_sheet, vars_sheet, db_name, vars_to_convert = NULL) {
  doc <- XML::xmlNode(pkg.env$node_name.pmml, namespaceDefinitions=c(pkg.env$node_namespace.pmml), attrs=c(version=pkg.env$node_attr.pmml_version))
  dict <- XML::xmlNode(pkg.env$node_name.data_dict)
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
