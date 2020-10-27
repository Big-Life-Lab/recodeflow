#' Build DataField node for start variable.
#'
#' @param var_name Variable name.
#' @param var_details_rows All variable details rows for the `var_name` variable.
#'
#' @return DataField node with optype and dataType according to `fromType`.
build_data_field_for_start_var <- function(var_name, var_details_rows) {
  first_var_details_row <- var_details_rows[1,]
  if (first_var_details_row$fromType == pkg.env$var_details_cat) {
    optype <- pkg.env$node_attr.optype.categorical
  } else if(first_var_details_row$fromType == pkg.env$var_details_cont) {
    optype <- pkg.env$node_attr.optype.continuous
  } else {
    stop(paste("Unable to determine optype for"), var_name)
  }

  data_type <- get_variable_type_data_type(var_details_rows, first_var_details_row$fromType, is_start_var = TRUE)
  data_field <- XML::xmlNode(pkg.env$node_name.data_field, attrs=c(name=var_name,
                                                  displayName=first_var_details_row$variableStartShortLabel,
                                                  optype=optype,
                                                  dataType=data_type))

  return (add_data_field_children_for_start_var(data_field, var_details_rows))
}

#' Build DataField node for variable.
#'
#' @param var_name Variable name.
#' @param vars_sheet Variable sheet data frame.
#'
#' @return DataField node for variable.
build_data_field_for_var <- function(var_name, vars_sheet) {
  var_row <- get_var_sheet_row(var_name, vars_sheet)
  data_field_node <- XML::xmlNode(pkg.env$node_name.data_field, attrs=c(name=var_name, displayName=var_row$label))
  extension_node <- XML::xmlNode(pkg.env$node_name.extension, attrs=c(name=pkg.env$node_attr.name.label_long, value=var_row$labelLong))
  return (XML::append.xmlNode(data_field_node, extension_node))
}

#' Add DataField child nodes for start variable.
#'
#' @param data_field DataField node to attach child nodes.
#' @param var_details_rows Variable details rows associated with current variable.
#'
#' @return Updated DataField node.
add_data_field_children_for_start_var <- function(data_field, var_details_rows) {
  var_details_row <- var_details_rows[1,]
  extension_node <- XML::xmlNode(pkg.env$node_name.extension, attrs=c(name=pkg.env$node_attr.name.var_start_label, value=var_details_row$variableStartLabel))
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
attach_cat_value_nodes_for_start_var <- function(var_details_row, data_field) {
  if (var_details_row$recTo == pkg.env$NA_invalid) property <- pkg.env$node_attr.property.invalid
  else if (var_details_row$recTo == pkg.env$NA_missing) property <- pkg.env$node_attr.property.missing
  else property <- pkg.env$node_attr.property.valid

  if(is_rec_from_range(var_details_row)) {
    data_field <- attach_range_value_nodes(var_details_row, data_field)
  } else if (is_numeric(var_details_row$recFrom)) {
    value_node <- XML::xmlNode(pkg.env$node_name.value, attrs=c(value=var_details_row$recFrom, displayValue=var_details_row$catStartLabel, property=property))
    data_field <- XML::append.xmlNode(data_field, value_node)
  }

  return (data_field)
}

#' Attach continuous Value nodes for start variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
attach_cont_value_nodes_for_start_var <- function(var_details_row, data_field) {
  if (var_details_row$recTo == pkg.env$rec_to_copy) {
    margins <- get_margins(var_details_row$recFrom)
    interval_node <- XML::xmlNode(pkg.env$node_name.interval, attrs=c(closure=pkg.env$node_attr.closure.closedClosed, leftMargin=margins[1], rightMargin=margins[2]))
    data_field <- XML::append.xmlNode(data_field, interval_node)
  } else if(is_rec_from_range(var_details_row)) {
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
attach_range_value_nodes <- function(var_details_row, data_field) {
  range <- eval(parse(text=var_details_row$recFrom))
  cat_start_labels <- trimws(strsplit(var_details_row$catStartLabel, ";")[[1]])

  for (index in 1:length(range)) {
    is_missing <- var_details_row$recTo %in% pkg.env$all_NAs
    property <- ifelse(is_missing, pkg.env$missing, var_details_row$recTo)

    value_node <- XML::xmlNode(pkg.env$node_name.value, attrs=c(value=range[index], displayValue=cat_start_labels[index], property=property))
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
build_trans_dict <- function(vars_sheet, var_details_sheet, var_names, db_name) {
  trans_dict <- XML::xmlNode(pkg.env$node_name.trans_dict)

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
build_derived_field_node <- function(vars_sheet, var_details_sheet, var_name, db_name) {
  var_row <- get_var_sheet_row(var_name, vars_sheet)
  var_details_rows <- get_var_details_rows(var_details_sheet, var_name, db_name)
  data_type <- get_variable_type_data_type(var_details_rows, var_row$variableType, is_start_var = FALSE)

  derived_field_node <- XML::xmlNode(pkg.env$node_name.derived_field, attrs=c(name=var_name, displayName=var_row$label, optype=tolower(var_row$variableType), dataType=data_type))
  label_long_node <- XML::xmlNode(pkg.env$node_name.extension, attrs=c(name=pkg.env$node_attr.name.label_long, value=var_row$labelLong))
  units_node <- XML::xmlNode(pkg.env$node_name.extension, attrs=c(name=pkg.env$node_attr.name.units, value=var_row$units))
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
attach_derived_field_child_nodes <- function(derived_field_node, var_details_sheet, var_name, db_name) {
  var_details_rows <- get_var_details_rows(var_details_sheet, var_name, db_name)

  derived_field_node <- attach_apply_nodes(var_details_rows, derived_field_node, db_name)

  for (index in 1:nrow(var_details_rows)) {
    var_details_row <- var_details_rows[index,]

    if (is_numeric(var_details_row$recTo)) {
      value_node <- XML::xmlNode(pkg.env$node_name.value, attrs=c(value=var_details_row$recTo, displayValue=var_details_row$catLabel))
      extension_node <- XML::xmlNode(pkg.env$node_name.extension, attrs=c(name=pkg.env$node_attr.name.cat_label_long, value=var_details_row$catLabelLong))
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
attach_apply_nodes <- function(var_details_rows, parent_node, db_name) {
  var_details_row <- var_details_rows[1,]
  remaining_rows <- var_details_rows[-1,]
  if (nrow(remaining_rows) == 0) return (parent_node)

  if (is_numeric(var_details_row$recFrom)) {
    field_node <- build_variable_field_ref_node(var_details_row, db_name)
    const_equal_node <- XML::xmlNode(pkg.env$node_name.constant, attrs=c(dataType=pkg.env$node_attr.dataType.integer), value=var_details_row$recFrom)
    const_val_node <- XML::xmlNode(pkg.env$node_name.constant)

    if (var_details_row$recTo %in% pkg.env$all_NAs) {
      XML::xmlAttrs(const_val_node) <- c(missing=pkg.env$node_attr.missing.true)
    } else {
      XML::xmlAttrs(const_val_node) <- c(dataType=pkg.env$node_attr.dataType.integer)
      XML::xmlValue(const_val_node) <- var_details_row$recFrom
    }

    if_node <- XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.if),
                            XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.equals),
                                         field_node,
                                         const_equal_node))

    if_node <- XML::append.xmlNode(if_node, const_val_node)

    return (XML::append.xmlNode(parent_node, attach_apply_nodes(remaining_rows, if_node, db_name)))
  } else if (is_rec_from_range(var_details_row)) {
    margins <- get_margins(var_details_row$recFrom)
    const_node_gt <- XML::xmlNode(pkg.env$node_name.constant, attrs=c(dataType=pkg.env$node_attr.dataType.integer), value=margins[1])
    const_node_lt <- XML::xmlNode(pkg.env$node_name.constant, attrs=c(dataType=pkg.env$node_attr.dataType.integer), value=margins[2])
    field_node <- build_variable_field_ref_node(var_details_row, db_name)

    or_node_1 <- XML::append.xmlNode(XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.or)),
                                     XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.greater_than), field_node, const_node_gt),
                                     XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.equals), field_node, const_node_gt))
    or_node_2 <- XML::append.xmlNode(XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.or)),
                                     XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.less_than), field_node, const_node_lt),
                                     XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.equals), field_node, const_node_lt))

    and_node <- XML::append.xmlNode(XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.and)), or_node_1, or_node_2)
    if_node <- XML::append.xmlNode(XML::xmlNode(pkg.env$node_name.apply, attrs=c("function"=pkg.env$node_attr.function.if)), and_node)

    if (var_details_row$recTo %in% pkg.env$all_NAs) {
      missing_node <- XML::xmlNode(pkg.env$node_name.constant, attrs=c(missing=pkg.env$node_attr.missing.true))
      if_node <- XML::append.xmlNode(if_node, missing_node, missing_node)
    } else if (var_details_row$recTo == pkg.env$rec_to_copy) {
      if_node <- XML::append.xmlNode(if_node, field_node)
    }

    return(XML::append.xmlNode(parent_node, attach_apply_nodes(remaining_rows, if_node, db_name)))
  } else {
    return (XML::append.xmlNode(parent_node, XML::xmlNode(pkg.env$node_name.constant, attrs=c(missing=pkg.env$node_attr.missing.true))))
  }
}

#' Build FieldRef node for variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param db_name Database name.
#'
#' @return FieldRef node.
build_variable_field_ref_node <- function (var_details_row, db_name) {
  return (XML::xmlNode(pkg.env$node_name.field_ref, attrs=c(field=get_start_var_name(var_details_row, db_name))))
}
