#' Creates a PMML string from an xFlow document.
#'
#' @param var_details_sheet A data frame representing a variable details sheet.
#' @param vars_sheet A data frame representing a variables sheet.
#' @param db_name A string containing the name of the database that holds
#' the start variables. Should match up with one of the databases in the
#' databaseStart column.
#' @param vars_to_convert A vector of strings containing the names of variables
#' from the variable column in the variable details sheet that should be.
#' converted to PMML. Passing in an empty vector will convert all the variables.
#'
#' @return A string containing the PMML string.
#' @export
#'
#' @examples
pmml.xflow_to_pmml <- function(var_details_sheet, vars_sheet, db_name, vars_to_convert) {
  doc <- XML::xmlNode("PMML", attrs=c(xmlns="http://www.dmg.org/PMML-4_4", version="4.4"))
  dict <- XML::xmlNode("DataDictionary")

  for (var_to_convert in vars_to_convert) {
    indices <- which(var_details_sheet$variable == var_to_convert, arr.ind = TRUE)
    var_details_rows <- var_details_sheet[indices,]

    var_start_name <- get_start_var_name(var_details_rows, db_name)
    data_field <- build_data_field_for_start_var(var_start_name, var_details_rows)
    data_field <- add_data_field_children_for_start_var(data_field, var_details_rows)
    dict <- XML::append.xmlNode(dict, data_field)
  }

  xmlAttrs(dict) <- c(numberOfFields=xmlSize(dict))
  doc <- XML::append.xmlNode(doc, dict, build_trans_dict(vars_sheet, var_details_sheet, vars_to_convert, db_name))
  return (doc)
}

#' Get variable name from variableStart using database name.
#'
#' @param var_details_rows Named vector as variable details sheet row.
#' @param db_name Name of database to extract from.
#'
#' @return Variable name according to database name.
#' @export
#'
#' @examples
get_start_var_name <- function(var_details_rows, db_name) {
  var_details_row = var_details_rows[1,]
  var_prefix <- paste(db_name, "::", sep="")
  var_regex <- paste(var_prefix, "(.+?)[,?]", sep="")
  var_index <- attr(regexpr(var_regex, var_details_row$variableStart, TRUE), "match.length")
  return (substr(var_details_row$variableStart, nchar(var_prefix) + 1, var_index - 1))
}

#' Build DataField node for start variable.
#'
#' @param var_name Variable name.
#' @param var_details_rows Variable details rows for `var_name` variable.
#'
#' @return DataField node with optype and dataType according to `fromType`.
#' @export
#'
#' @examples
build_data_field_for_start_var <- function(var_name, var_details_rows) {
  var_details_row <- var_details_rows[1,]
  if (var_details_row$fromType == "cat") {
    optype <- "categorical"
    data_type <- "integer"
  } else if(var_details_row$fromType == "cont") {
    optype <- "continuous"
    data_type <- "float"
  }

  return (XML::xmlNode("DataField", attrs=c(name=var_name,
                                       displayName=var_details_row$variableStartShortLabel,
                                       optype=optype,
                                       dataType=data_type)))
}

#' Add DataField child nodes.
#'
#' @param data_field DataField node to attach child nodes.
#' @param var_details_rows Variable details rows associated with current variable.
#'
#' @return Updated DataField node.
#' @export
#'
#' @examples
add_data_field_children_for_start_var <- function(data_field, var_details_rows) {
  var_details_row <- var_details_rows[1,]
  extension_node <- XML::xmlNode("Extension", attrs=c(name="variableStartLabel", value=var_details_row$variableStartLabel))
  data_field <- XML::append.xmlNode(data_field, extension_node)

  for (index in 1:nrow(var_details_rows)) {
    row <- var_details_rows[index,]
    if (row$toType == "cat") data_field <- attach_cat_value_nodes_for_start_var(row, data_field)
    else data_field <- attach_cont_value_nodes_for_start_var(row, data_field)
  }

  return (data_field)
}

#' Attach categorical value nodes to DataField node.
#'
#' @param row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#' @export
#'
#' @examples
attach_cat_value_nodes_for_start_var <- function(row, data_field) {
  if (row$recTo == "NA::a") property <- "invalid"
  else if (row$recTo == "NA::b") property <- "missing"
  else property <- "valid"

  if(grepl(":", row$recFrom, fixed=TRUE)) {
    data_field <- attach_missing_value_nodes(row, data_field)
  } else if (suppressWarnings(!is.na(as.numeric(row$recFrom)))) {
    value_node <- XML::xmlNode("Value", attrs=c(value=row$recFrom, displayValue=row$catLabel, property=property))
    data_field <- XML::append.xmlNode(data_field, value_node)
  }

  return (data_field)
}

#' Attach continuous Value nodes.
#'
#' @param row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#' @export
#'
#' @examples
attach_cont_value_nodes_for_start_var <- function(row, data_field) {
  if (row$recTo == "copy") {
    margins <- trimws(strsplit(row$recFrom, ":")[[1]])
    interval_node <- XML::xmlNode("Interval", attrs=c(closure="closedClosed", leftMargin=margins[1], rightMargin=margins[2]))
    data_field <- XML::append.xmlNode(data_field, interval_node)
  } else if(grepl(":", row$recFrom, fixed=TRUE)) {
    data_field <- attach_missing_value_nodes(row, data_field)
  } else {
    data_field <- attach_cat_value_nodes_for_start_var(row, data_field)
  }

  return (data_field)
}

#' Attach Value nodes to DataField node. Used when `recFrom` has a range of missing values.
#'
#' @param row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
#' @export
#'
#' @examples
attach_missing_value_nodes <- function(row, data_field) {
  range <- eval(parse(text=row$recFrom))
  cat_start_labels <- trimws(strsplit(row$catStartLabel, ";")[[1]])

  for (index in range) {
    label <- cat_start_labels[index - range[1] + 1]
    value_node <- XML::xmlNode("Value", attrs=c(value=index, displayValue=label, property="missing"))
    data_field <- XML::append.xmlNode(data_field, value_node)
  }

  return (data_field)
}

#' Build a TransformationDictionary node.
#'
#' @param vars_sheet Variable sheet data frame.
#' @param var_details_sheet Variable details sheet data frame.
#' @param vars_to_convert Vector of variable names to convert.
#' @param db_name Database name.
#'
#' @return TransformationDictionary node.
#' @export
#'
#' @examples
build_trans_dict <- function(vars_sheet, var_details_sheet, vars_to_convert, db_name) {
  trans_dict <- XML::xmlNode("TransformationDictionary")

  for (var_to_convert in vars_to_convert) {
    trans_dict <- XML::append.xmlNode(trans_dict, build_derived_field_node(vars_sheet, var_details_sheet, var_to_convert, db_name))
  }

  return (trans_dict)
}

#' Build DerivedField node.
#'
#' @param vars_sheet Variable sheet data frame.
#' @param var_details_sheet Variable details sheet data frame.
#' @param vars_to_convert Vector of variable names to convert.
#' @param db_name Database name.
#'
#' @return DerivedField node.
#' @export
#'
#' @examples
build_derived_field_node <- function(vars_sheet, var_details_sheet, var_to_convert, db_name) {
  indices <- which(vars_sheet$variable == var_to_convert, arr.ind = TRUE)
  row <- vars_sheet[indices[1],]
  data_type <- ifelse(row$variableType == "Categorical", "integer", "float")

  derived_field_node <- XML::xmlNode("DerivedField", attrs=c(name=var_to_convert, displayName=row$label, optype=tolower(row$variableType), dataType=data_type))
  label_long_node <- XML::xmlNode("Extension", attrs=c(name="labelLong", value=row$labelLong))
  units_node <- XML::xmlNode("Extension", attrs=c(name="units", value=row$units))
  derived_field_node <- XML::append.xmlNode(derived_field_node, label_long_node, units_node)
  derived_field_node <- attach_derived_field_nodes(derived_field_node, var_details_sheet, var_to_convert, db_name)

  return (derived_field_node)
}

#' Attach child nodes to DerivedField node.
#'
#' @param derived_field_node DerivedField node to attach child nodes.
#' @param var_details_sheet Variable details sheet data frame.
#' @param vars_to_convert Vector of variable names to convert.
#' @param db_name Database name.
#'
#' @return Updated DerivedField node.
#' @export
#'
#' @examples
attach_derived_field_nodes <- function(derived_field_node, var_details_sheet, var_to_convert, db_name) {
  indices <- which(var_details_sheet$variable == var_to_convert, arr.ind = TRUE)
  var_details_rows <- var_details_sheet[indices,]

  derived_field_node <- attach_apply_nodes(var_details_rows, derived_field_node, db_name)

  for (index in 1:nrow(var_details_rows)) {
    details_row <- var_details_rows[index,]

    if (suppressWarnings(!is.na(as.numeric(details_row$recTo)))) {
      value_node <- XML::xmlNode("Value", attrs=c(value=details_row$recTo, displayValue=details_row$catLabel))
      extension_node <- XML::xmlNode("Extension", attrs=c(name="catLabelLong", value=details_row$catLabelLong))
      value_node <- XML::append.xmlNode(value_node, extension_node)
      derived_field_node <- XML::append.xmlNode(derived_field_node, value_node)
    }
  }

  return (derived_field_node)
}

#' Attach Apply nodes to a parent node.
#'
#' @param var_details_rows Variable details rows associated with current variable.
#' @param parent_node An XML node.
#' @param db_name Database name.
#'
#' @return Updated parent node.
#' @export
#'
#' @examples
attach_apply_nodes <- function(var_details_rows, parent_node, db_name) {
  details_row <- var_details_rows[1,]
  remaining_rows <- var_details_rows[-1,]
  if (nrow(remaining_rows) == 0) return (parent_node)

  if (suppressWarnings(!is.na(as.numeric(details_row$recFrom)))) {
    if (details_row$recTo %in% c("NA::a", "NA::B")) const_val_node <- XML::xmlNode("Constant", attrs=c(missing="true"))
    else const_val_node <- XML::xmlNode("Constant", attrs=c(missing="true"), value=details_row$recFrom)

    if_node <- XML::xmlNode("Apply", attrs=c("function"="if"),
                            XML::xmlNode("Apply", attrs=c("function"="equals"),
                                         XML::xmlNode("FieldRef", attrs=c(field=get_start_var_name(details_row, db_name))),
                                         XML::xmlNode("Constant", attrs=c(dataType="integer"), value=details_row$recFrom)),
                       const_val_node)

    return (XML::append.xmlNode(parent_node, attach_apply_nodes(remaining_rows, if_node, db_name)))
  } else if (grepl(":", details_row$recFrom, fixed=TRUE)) {
    margins <- trimws(strsplit(details_row$recFrom, ":")[[1]])
    field_node <- XML::xmlNode("FieldRef", attrs=c(field=get_start_var_name(details_row, db_name)))
    const_node_gt <- XML::xmlNode("Constant", attrs=c(dataType="integer"), value=margins[1])
    const_node_lt <- XML::xmlNode("Constant", attrs=c(dataType="integer"), value=margins[2])

    and_node <- XML::xmlNode("Apply", attrs=c("function"="and"))
    or_node_1 <- XML::xmlNode("Apply", attrs=c("function"="or"))
    or_node_2 <- XML::xmlNode("Apply", attrs=c("function"="or"))
    gt_node <- XML::xmlNode("Apply", attrs=c("function"="greaterThan"), field_node, const_node_gt)
    lt_node <- XML::xmlNode("Apply", attrs=c("function"="lessThan"), field_node, const_node_lt)
    equals_node_1 <- XML::xmlNode("Apply", attrs=c("function"="equals"), field_node, const_node_gt)
    equals_node_2 <- XML::xmlNode("Apply", attrs=c("function"="equals"), field_node, const_node_lt)
    missing_node <- XML::xmlNode("Constant", attrs=c(missing="true"))

    or_node_1 <- XML::append.xmlNode(or_node_1, gt_node, equals_node_1)
    or_node_2 <- XML::append.xmlNode(or_node_2, lt_node, equals_node_2)

    and_node <- XML::append.xmlNode(and_node, or_node_1, or_node_2)
    if_node <- XML::append.xmlNode(XML::xmlNode("Apply", attrs=c("function"="if")), and_node, missing_node, missing_node)
    parent_node <- XML::append.xmlNode(parent_node, if_node)

    return (attach_apply_nodes(remaining_rows, parent_node, db_name))
  } else {
    return (XML::append.xmlNode(parent_node, XML::xmlNode("Constant", attrs=c(missing="true"))))
  }
}
