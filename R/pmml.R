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
