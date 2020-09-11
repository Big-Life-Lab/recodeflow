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
    var_details_rows <- get_var_details_rows(var_details_sheet, var_to_convert, db_name)
    first_var_details_row <- var_details_rows[1,]
    var_start_name <- get_start_var_name(first_var_details_row, db_name)

    if (is.na(var_start_name)) {
      print(paste("Unable to find start variable for", var_to_convert, "for database", db_name))
    } else {
      data_field <- build_data_field_for_start_var(var_start_name, var_details_rows)

      if (is.null(data_field)) {
        print(paste("Unable to determine fromType for", var_to_convert))
      } else {
        recognized_vars_to_convert <- c(recognized_vars_to_convert, var_to_convert)
        data_field <- add_data_field_children_for_start_var(data_field, var_details_rows)
        dict <- XML::append.xmlNode(dict, data_field)
      }
    }
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

#' Get all variable details rows for a variable.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param var_name Variable name.
#' @param db_name Database name.
#'
#' @return All variable details rows for the variable.
#'
#' @examples
get_var_details_rows <- function (var_details_sheet, var_name, db_name) {
  var_name_indices <- which(var_details_sheet$variable == var_name, arr.ind = TRUE)
  db_indices <- which(grepl(db_name, var_details_sheet$databaseStart), arr.ind = TRUE)
  intersect_indices <- intersect(var_name_indices, db_indices)
  return (var_details_sheet[intersect_indices,])
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

#' Build DataField node for start variable.
#'
#' @param var_name Variable name.
#' @param var_details_rows All variable details rows for the `var_name` variable.
#'
#' @return DataField node with optype and dataType according to `fromType`.
#'
#' @examples
build_data_field_for_start_var <- function(var_name, var_details_rows) {
  first_var_details_row <- var_details_rows[1,]
  if (first_var_details_row$fromType == pkg.env$var_details_cat) {
    optype <- "categorical"
  } else if(first_var_details_row$fromType == pkg.env$var_details_cont) {
    optype <- "continuous"
  } else {
    stop(paste("Unable to determine optype for"), var_name)
  }

  data_type <- get_variable_type_data_type(var_details_rows, first_var_details_row$fromType, is_start_var = TRUE)

  return (XML::xmlNode("DataField", attrs=c(name=var_name,
                                       displayName=first_var_details_row$variableStartShortLabel,
                                       optype=optype,
                                       dataType=data_type)))
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

#' Add DataField child nodes for start variable.
#'
#' @param data_field DataField node to attach child nodes.
#' @param var_details_rows Variable details rows associated with current variable.
#'
#' @return Updated DataField node.
#'
#' @examples
add_data_field_children_for_start_var <- function(data_field, var_details_rows) {
  var_details_row <- var_details_rows[1,]
  extension_node <- XML::xmlNode("Extension", attrs=c(name="variableStartLabel", value=var_details_row$variableStartLabel))
  data_field <- XML::append.xmlNode(data_field, extension_node)

  for (index in 1:nrow(var_details_rows)) {
    var_details_row <- var_details_rows[index,]
    if (var_details_row$fromType == pkg.env$var_details_cat) data_field <- attach_cat_value_nodes_for_start_var(var_details_row, data_field)
    else data_field <- attach_cont_value_nodes_for_start_var(var_details_row, data_field)
  }

  return (data_field)
}

#' Attach categorical value nodes to DataField node for start variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#'
#' @examples
attach_cat_value_nodes_for_start_var <- function(var_details_row, data_field) {
  if (var_details_row$recTo == pkg.env$NA_invalid) property <- "invalid"
  else if (var_details_row$recTo == pkg.env$NA_missing) property <- "missing"
  else property <- "valid"

  if(grepl(":", var_details_row$recFrom, fixed=TRUE)) {
    data_field <- attach_range_value_nodes(var_details_row, data_field)
  } else if (is_numeric(var_details_row$recFrom)) {
    value_node <- XML::xmlNode("Value", attrs=c(value=var_details_row$recFrom, displayValue=var_details_row$catLabel, property=property))
    data_field <- XML::append.xmlNode(data_field, value_node)
  }

  return (data_field)
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

#' Attach continuous Value nodes for start variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#'
#' @examples
attach_cont_value_nodes_for_start_var <- function(var_details_row, data_field) {
  if (var_details_row$recTo == pkg.env$rec_to_copy) {
    margins <- get_margins(var_details_row$recFrom)
    interval_node <- XML::xmlNode("Interval", attrs=c(closure="closedClosed", leftMargin=margins[1], rightMargin=margins[2]))
    data_field <- XML::append.xmlNode(data_field, interval_node)
  } else if(grepl(":", var_details_row$recFrom, fixed=TRUE)) {
    data_field <- attach_range_value_nodes(var_details_row, data_field)
  } else {
    data_field <- attach_cat_value_nodes_for_start_var(var_details_row, data_field)
  }

  return (data_field)
}

#' Attach Value nodes to DataField node. Used when `recFrom` has a value range.
#'
#' @param var_details_row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#'
#' @examples
attach_range_value_nodes <- function(var_details_row, data_field) {
  range <- eval(parse(text=var_details_row$recFrom))
  cat_start_labels <- trimws(strsplit(var_details_row$catStartLabel, ";")[[1]])

  for (index in 1:length(range)) {
    is_missing <- var_details_row$recTo %in% pkg.env$all_NAs
    property <- ifelse(is_missing, pkg.env$missing, var_details_row$recTo)

    value_node <- XML::xmlNode("Value", attrs=c(value=range[index], displayValue=cat_start_labels[index], property=property))
    data_field <- XML::append.xmlNode(data_field, value_node)
  }

  return (data_field)
}

#' Build a TransformationDictionary node.
#'
#' @param vars_sheet Variable sheet data frame.
#' @param var_details_sheet Variable details sheet data frame.
#' @param var_names Vector of variable names.
#' @param db_name Database name.
#'
#' @return TransformationDictionary node.
#'
#' @examples
build_trans_dict <- function(vars_sheet, var_details_sheet, var_names, db_name) {
  trans_dict <- XML::xmlNode("TransformationDictionary")

  for (var_name in var_names) {
    trans_dict <- XML::append.xmlNode(trans_dict, build_derived_field_node(vars_sheet, var_details_sheet, var_name, db_name))
  }

  return (trans_dict)
}

#' Build DerivedField node.
#'
#' @param vars_sheet Variables sheet data frame.
#' @param var_details_sheet Variable details sheet data frame.
#' @param var_name Variable name.
#' @param db_name Database name.
#'
#' @return DerivedField node.
#'
#' @examples
build_derived_field_node <- function(vars_sheet, var_details_sheet, var_name, db_name) {
  var_row <- vars_sheet[which(vars_sheet$variable == var_name, arr.ind = TRUE)[1],]
  var_details_rows <- get_var_details_rows(var_details_sheet, var_name, db_name)
  data_type <- get_variable_type_data_type(var_details_rows, var_row$variableType, is_start_var = FALSE)

  derived_field_node <- XML::xmlNode("DerivedField", attrs=c(name=var_name, displayName=var_row$label, optype=tolower(var_row$variableType), dataType=data_type))
  label_long_node <- XML::xmlNode("Extension", attrs=c(name="labelLong", value=var_row$labelLong))
  units_node <- XML::xmlNode("Extension", attrs=c(name="units", value=var_row$units))
  derived_field_node <- XML::append.xmlNode(derived_field_node, label_long_node, units_node)
  derived_field_node <- attach_derived_field_child_nodes(derived_field_node, var_details_sheet, var_name, db_name)

  return (derived_field_node)
}

#' Attach child nodes to DerivedField node.
#'
#' @param derived_field_node DerivedField node to attach child nodes.
#' @param var_details_sheet Variable details sheet data frame.
#' @param var_name Variable name.
#' @param db_name Database name.
#'
#' @return Updated DerivedField node.
#'
#' @examples
attach_derived_field_child_nodes <- function(derived_field_node, var_details_sheet, var_name, db_name) {
  var_details_rows <- get_var_details_rows(var_details_sheet, var_name, db_name)

  derived_field_node <- attach_apply_nodes(var_details_rows, derived_field_node, db_name)

  for (index in 1:nrow(var_details_rows)) {
    var_details_row <- var_details_rows[index,]

    if (is_numeric(var_details_row$recTo)) {
      value_node <- XML::xmlNode("Value", attrs=c(value=var_details_row$recTo, displayValue=var_details_row$catLabel))
      extension_node <- XML::xmlNode("Extension", attrs=c(name="catLabelLong", value=var_details_row$catLabelLong))
      value_node <- XML::append.xmlNode(value_node, extension_node)
      derived_field_node <- XML::append.xmlNode(derived_field_node, value_node)
    }
  }

  return (derived_field_node)
}

#' Attach Apply nodes to a parent node.
#'
#' @param var_details_rows Variable details rows associated with a variable.
#' @param parent_node An XML node.
#' @param db_name Database name.
#'
#' @return Updated parent node.
#'
#' @examples
attach_apply_nodes <- function(var_details_rows, parent_node, db_name) {
  var_details_row <- var_details_rows[1,]
  remaining_rows <- var_details_rows[-1,]
  if (nrow(remaining_rows) == 0) return (parent_node)

  if (is_numeric(var_details_row$recFrom)) {
    field_node <- build_variable_field_ref_node(var_details_row, db_name)
    const_equal_node <- XML::xmlNode("Constant", attrs=c(dataType="integer"), value=var_details_row$recFrom)
    const_val_node <- XML::xmlNode("Constant")

    if (var_details_row$recTo %in% pkg.env$all_NAs) {
      XML::xmlAttrs(const_val_node) <- c(missing="true")
    } else {
      XML::xmlAttrs(const_val_node) <- c(dataType="integer")
      XML::xmlValue(const_val_node) <- var_details_row$recFrom
    }

    if_node <- XML::xmlNode("Apply", attrs=c("function"="if"),
                            XML::xmlNode("Apply", attrs=c("function"="equals"),
                                         field_node,
                                         const_equal_node))

    if_node <- XML::append.xmlNode(if_node, const_val_node)

    return (XML::append.xmlNode(parent_node, attach_apply_nodes(remaining_rows, if_node, db_name)))
  } else if (grepl(":", var_details_row$recFrom, fixed=TRUE)) {
    margins <- get_margins(var_details_row$recFrom)
    const_node_gt <- XML::xmlNode("Constant", attrs=c(dataType="integer"), value=margins[1])
    const_node_lt <- XML::xmlNode("Constant", attrs=c(dataType="integer"), value=margins[2])
    field_node <- build_variable_field_ref_node(var_details_row, db_name)

    or_node_1 <- XML::append.xmlNode(XML::xmlNode("Apply", attrs=c("function"="or")),
                                     XML::xmlNode("Apply", attrs=c("function"="greaterThan"), field_node, const_node_gt),
                                     XML::xmlNode("Apply", attrs=c("function"="equals"), field_node, const_node_gt))
    or_node_2 <- XML::append.xmlNode(XML::xmlNode("Apply", attrs=c("function"="or")),
                                     XML::xmlNode("Apply", attrs=c("function"="lessThan"), field_node, const_node_lt),
                                     XML::xmlNode("Apply", attrs=c("function"="equals"), field_node, const_node_lt))

    and_node <- XML::append.xmlNode(XML::xmlNode("Apply", attrs=c("function"="and")), or_node_1, or_node_2)
    if_node <- XML::append.xmlNode(XML::xmlNode("Apply", attrs=c("function"="if")), and_node)

    if (var_details_row$recTo %in% pkg.env$all_NAs) {
      missing_node <- XML::xmlNode("Constant", attrs=c(missing="true"))
      if_node <- XML::append.xmlNode(if_node, missing_node, missing_node)
    } else if (var_details_row$recTo == pkg.env$rec_to_copy) {
      if_node <- XML::append.xmlNode(if_node, field_node)
    }

    return(XML::append.xmlNode(parent_node, attach_apply_nodes(remaining_rows, if_node, db_name)))
  } else {
    return (XML::append.xmlNode(parent_node, XML::xmlNode("Constant", attrs=c(missing="true"))))
  }
}

#' Build FieldRef node for variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param db_name Database name.
#'
#' @return FieldRef node.
#'
#' @examples
build_variable_field_ref_node <- function (var_details_row, db_name) {
  return (XML::xmlNode("FieldRef", attrs=c(field=get_start_var_name(var_details_row, db_name))))
}

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
