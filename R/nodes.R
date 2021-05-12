#' Build DataField node for start variable.
#'
#' @param var_name Variable name.
#' @param var_details_rows All variable details rows for the `var_name` variable.
#'
#' @return DataField node with optype and dataType according to `fromType`.
build_data_field_for_start_var <-
  function(var_name, var_details_rows) {
    first_var_details_row <- var_details_rows[1, ]
    if (first_var_details_row[[pkg.env$columns.variablesDetails.typeStart]] == pkg.env$var_details_cat) {
      optype <- pkg.env$node_attr.optype.categorical
    } else if (first_var_details_row[[pkg.env$columns.variablesDetails.typeStart]] == pkg.env$var_details_cont) {
      optype <- pkg.env$node_attr.optype.continuous
    } else {
      stop(paste("Unable to determine optype for"), var_name)
    }

    data_type <-
      get_variable_type_data_type(var_details_rows, first_var_details_row[[pkg.env$columns.variablesDetails.typeStart]], is_start_var = TRUE)
    data_field <-
      XML::xmlNode(
        pkg.env$node_name.data_field,
        attrs = c(
          name = var_name,
          displayName = first_var_details_row$variableStartShortLabel,
          optype = optype,
          dataType = data_type
        )
      )

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
  data_field_node <-
    XML::xmlNode(pkg.env$node_name.data_field,
                 attrs = c(name = var_name, displayName = var_row$label))
  extension_node <-
    XML::xmlNode(
      pkg.env$node_name.extension,
      attrs = c(
        name = pkg.env$node_attr.name.label_long,
        value = var_row$labelLong
      )
    )
  return (XML::append.xmlNode(data_field_node, extension_node))
}

#' Add DataField child nodes for start variable.
#'
#' @param data_field DataField node to attach child nodes.
#' @param var_details_rows Variable details rows associated with current variable.
#'
#' @return Updated DataField node.
add_data_field_children_for_start_var <-
  function(data_field, var_details_rows) {
    var_details_row <- var_details_rows[1, ]
    extension_node <-
      XML::xmlNode(
        pkg.env$node_name.extension,
        attrs = c(
          name = pkg.env$node_attr.name.var_start_label,
          value = var_details_row$variableStartLabel
        )
      )
    data_field <- XML::append.xmlNode(data_field, extension_node)

    for (index in 1:nrow(var_details_rows)) {
      var_details_row <- var_details_rows[index, ]
      if (var_details_row[[pkg.env$columns.variablesDetails.typeStart]] == pkg.env$var_details_cat)
        data_field <-
          attach_cat_value_nodes_for_start_var(var_details_row, data_field)
      else
        data_field <-
          attach_cont_value_nodes_for_start_var(var_details_row, data_field)
    }

    return (data_field)
  }

#' Attach categorical value nodes to DataField node for start variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param data_field DataField node to attach Value nodes.
#'
#' @return Updated DataField node.
attach_cat_value_nodes_for_start_var <-
  function(var_details_row, data_field) {
    if (is_rec_from_range(var_details_row)) {
      data_field <- attach_range_value_nodes(var_details_row, data_field)
    }
    # If the start variable value in this row is for "else", do nothing
    # since this isnt a category
    else if(var_details_row[[pkg.env$columns.recFrom]] == pkg.env$variable_details$columns.recFrom.elseValue) {

    }
    else {
      if (var_details_row[[pkg.env$columns.recTo]] == pkg.env$NA_invalid)
        property <- pkg.env$node_attr.property.invalid
      else if (var_details_row[[pkg.env$columns.recTo]] == pkg.env$NA_missing)
        property <- pkg.env$node_attr.property.missing
      else
        property <- pkg.env$node_attr.property.valid

      value_node <-
        XML::xmlNode(
          pkg.env$node_name.value,
          attrs = c(
            value = var_details_row[[pkg.env$columns.recFrom]],
            displayValue = var_details_row$catStartLabel,
            property = property
          )
        )
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
attach_cont_value_nodes_for_start_var <-
  function(var_details_row, data_field) {
    if (is_rec_from_range(var_details_row)) {
      if (var_details_row[[pkg.env$columns.recTo]] %in% pkg.env$all_NAs) {
        return (attach_range_value_nodes(var_details_row, data_field))
      }

      margins <-
        get_margins(var_details_row[[pkg.env$columns.recFrom]])
      closure <-
        get_margin_closure(var_details_row[[pkg.env$columns.recFrom]])
      property <- pkg.env$node_attr.property.valid
      interval_node <- XML::xmlNode(
        pkg.env$node_name.interval,
        attrs = c(
          closure = closure,
          leftMargin = margins[1],
          rightMargin = margins[2],
          property = property
        )
      )

      data_field <- XML::append.xmlNode(data_field, interval_node)
    } else {
      data_field <-
        attach_cat_value_nodes_for_start_var(var_details_row, data_field)
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
  margins <- get_margins(var_details_row[[pkg.env$columns.recFrom]])
  range <- margins[1]:margins[2]
  cat_start_labels <-
    trimws(strsplit(
      var_details_row$catStartLabel,
      pkg.env$cat_start_label_separator
    )[[1]])

  for (index in 1:length(range)) {
    is_missing <-
      var_details_row[[pkg.env$columns.recTo]] %in% pkg.env$all_NAs
    property <-
      ifelse(is_missing, pkg.env$missing, var_details_row[[pkg.env$columns.recTo]])

    value_node <-
      XML::xmlNode(
        pkg.env$node_name.value,
        attrs = c(
          value = range[index],
          displayValue = cat_start_labels[index],
          property = property
        )
      )
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
#' @param custom_function_names vector of strings. Holds the names of functions
#' parsed from custom function files.
#' @return TransformationDictionary node.
#' @export
build_trans_dict <-
  function(vars_sheet,
           var_details_sheet,
           var_names,
           db_name,
           custom_function_names) {
    trans_dict <- XML::xmlNode(pkg.env$node_name.trans_dict)

    for (var_name in var_names) {
      trans_dict <-
        XML::append.xmlNode(
          trans_dict,
          build_derived_field_node(vars_sheet, var_details_sheet, var_name, db_name, custom_function_names)
        )
    }

    return (trans_dict)
  }

#' Build DerivedField node.
#'
#' @param vars_sheet Variables sheet data frame.
#' @param var_details_sheet Variable details sheet data frame.
#' @param var_name Variable name.
#' @param db_name Database name.
#' @param custom_function_names vector of strings. Holds the names of functions
#' parsed from custom function files.
#'
#' @return DerivedField node.
build_derived_field_node <-
  function(vars_sheet,
           var_details_sheet,
           var_name,
           db_name,
           custom_function_names) {
    var_row <- get_var_sheet_row(var_name, vars_sheet)
    var_details_rows <-
      get_var_details_rows(var_details_sheet, var_name, db_name)
    data_type <-
      get_variable_type_data_type(var_details_rows, var_row$variableType, is_start_var = FALSE)

    derived_field_node <-
      XML::xmlNode(
        pkg.env$node_name.derived_field,
        attrs = c(
          name = var_name,
          displayName = var_row$label,
          optype = tolower(var_row$variableType),
          dataType = data_type
        )
      )
    label_long_node <-
      XML::xmlNode(
        pkg.env$node_name.extension,
        attrs = c(
          name = pkg.env$node_attr.name.label_long,
          value = var_row$labelLong
        )
      )
    units_node <-
      XML::xmlNode(
        pkg.env$node_name.extension,
        attrs = c(
          name = pkg.env$node_attr.name.units,
          value = var_row$units
        )
      )
    derived_field_node <-
      XML::append.xmlNode(derived_field_node, label_long_node, units_node)
    derived_field_node <-
      attach_derived_field_child_nodes(derived_field_node, var_details_sheet, var_name, db_name, custom_function_names)

    return (derived_field_node)
  }

#' Attach child nodes to DerivedField node.
#'
#' @param derived_field_node DerivedField node to attach child nodes.
#' @param var_details_sheet Variable details sheet data frame.
#' @param var_name Variable name.
#' @param db_name Database name.
#' @param custom_function_names vector of strings. Holds the names of functions
#' parsed from custom function files.
#'
#' @return Updated DerivedField node.
attach_derived_field_child_nodes <-
  function(derived_field_node,
           var_details_sheet,
           var_name,
           db_name,
           custom_function_names) {
    added_NAs <- c(character(0))
    var_details_rows <-
      get_var_details_rows(var_details_sheet, var_name, db_name)

    derived_field_node <-
      attach_apply_nodes(var_details_rows, derived_field_node, db_name, custom_function_names)

    for (index in 1:nrow(var_details_rows)) {
      var_details_row <- var_details_rows[index, ]

      if (is_numeric(var_details_row[[pkg.env$columns.recTo]])) {
        derived_field_node <- XML::append.xmlNode(derived_field_node,
                                                  build_derived_field_value_node(var_details_row))
      }
    }

    for (index in 1:nrow(var_details_rows)) {
      var_details_row <- var_details_rows[index, ]

      if (var_details_row[[pkg.env$columns.recTo]] %in% added_NAs)
        next

      if (var_details_row[[pkg.env$columns.recTo]] %in% pkg.env$all_NAs) {
        derived_field_node <- XML::append.xmlNode(derived_field_node,
                                                  build_derived_field_value_node(var_details_row))

        added_NAs <-
          c(added_NAs, var_details_row[[pkg.env$columns.recTo]])
      }
    }

    return (derived_field_node)
  }

#' Build Value node for DerivedField node.
#'
#' @param var_details_row Variable details sheet row.
#'
#' @return Value node.
#'
#' @examples
build_derived_field_value_node <- function(var_details_row) {
  extension_node <- XML::xmlNode(
    pkg.env$node_name.extension,
    attrs = c(
      name = pkg.env$node_attr.name.cat_label_long,
      value = var_details_row$catLabelLong
    )
  )

  value_node <- XML::xmlNode(
    pkg.env$node_name.value,
    attrs = c(value = var_details_row[[pkg.env$columns.recTo]], displayValue =
                var_details_row$catLabel),
    extension_node
  )

  return (value_node)
}

#' Attach Apply nodes to a parent node.
#'
#' @param all_var_details_rows Variable details rows associated with a variable.
#' @param parent_node An XML node.
#' @param db_name Database name.
#' @param custom_function_names vector of strings. Holds the names of functions
#' parsed from custom function files.
#'
#' @return Updated parent node.
attach_apply_nodes <-
  function(all_var_details_rows, parent_node, db_name, custom_function_names) {
    var_details_row <- all_var_details_rows[1, ]
    remaining_rows <- all_var_details_rows[-1, ]
    if (nrow(var_details_row) == 0)
      return (parent_node)

    # If this set of rows is for parsing a DerivedVar
    if(is_derived_var(all_var_details_rows)) {
      # First get the name of the custom function used to derive
      # this variable
      function_name_regex <- "Func::(.{0,})"
      rec_to_column <- all_var_details_rows[1, pkg.env$columns.recTo]
      derived_var_function_name <- regmatches(
        rec_to_column,
        regexec(function_name_regex, rec_to_column)
      )[[1]][2]

      # Check to make sure the function for this derived variable is
      # in the list of parsed custom functions. If it is not, throw an
      # error
      if(!derived_var_function_name %in% custom_function_names) {
        derived_var_name <- all_var_details_rows[1, pkg.env$columns.Variable]
        stop(paste("No custom function found for derved variable", derived_var_name, "with name", derived_var_function_name))
      }

      # Get the list of variables this derived variable is derived from
      # and convert them all to FieldRef XML nodes
      derived_from_vars <- get_start_vars_for_derived_var(all_var_details_rows[1, pkg.env$columns.Variable], all_var_details_rows)
      field_ref_nodes <- list()
      # Go through each derived from variable and create a field ref node for
      # each one
      for(derived_from_var in derived_from_vars) {
        field_ref_attrs <- c()
        field_ref_attrs[[pkg.env$node_attr.FieldRef.field]] <- derived_from_var
        field_ref_nodes[[length(field_ref_nodes) + 1]] <- XML::xmlNode(
          pkg.env$node_name.field_ref,
          attrs = field_ref_attrs
        )
      }

      # Create the Apply XML node for this derived variable and add the
      # FieldRef nodes created in the previous section to it
      apply_node_attrs <- c()
      apply_node_attrs[[pkg.env$node_attr.Apply.function]] <- derived_var_function_name
      apply_node <- XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = apply_node_attrs
      )
      for(field_ref_node in field_ref_nodes){
        apply_node <- XML::addChildren(apply_node, field_ref_node)
      }

      # Add the Apply node to the parent node and return it
      return (XML::append.xmlNode(
        parent_node,
        apply_node
      ))
    }
    else if (is_rec_from_range(var_details_row)) {
      apply_node <-
        build_ranged_derived_field_apply_node(var_details_row, db_name)
      return (XML::append.xmlNode(
        parent_node,
        attach_apply_nodes(remaining_rows, apply_node, db_name, custom_function_names)
      ))
    }
    else if (var_details_row[[pkg.env$columns.recFrom]] == pkg.env$variable_details$columns.recFrom.elseValue) {
      rec_to_node <- build_node_for_rec_to(var_details_row, db_name)

      return (XML::append.xmlNode(parent_node, rec_to_node))
    }
    else {
      apply_node <-
        build_single_rec_from_derived_field_apply_node(var_details_row, db_name)
      return (XML::append.xmlNode(
        parent_node,
        attach_apply_nodes(remaining_rows, apply_node, db_name, custom_function_names)
      ))
    }
  }

#' Build Apply node with singleton numeric for DerivedField node.
#'
#' @param var_details_row Variable details sheet row.
#' @param db_name Database name.
#'
#' @return Apply node for DerivedField node.
#'
#' @examples
build_single_rec_from_derived_field_apply_node <-
  function (var_details_row, db_name) {
    field_node <-
      build_variable_field_ref_node(var_details_row, db_name)

    const_equal_node <-
      XML::xmlNode(
        pkg.env$node_name.constant,
        attrs = c(dataType = get_data_type(var_details_row[[pkg.env$columns.recFrom]])),
        value = var_details_row[[pkg.env$columns.recFrom]]
      )

    const_val_node <- XML::xmlNode(pkg.env$node_name.constant)
    if (var_details_row[[pkg.env$columns.recTo]] %in% pkg.env$all_NAs) {
      const_val_node <- build_missing_const_node(var_details_row)
    } else {
      XML::xmlAttrs(const_val_node) <-
        c(dataType = get_data_type(var_details_row[[pkg.env$columns.recTo]]))

      XML::xmlValue(const_val_node) <-
        var_details_row[[pkg.env$columns.recTo]]
    }

    apply_node <-
      XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = c("function" = pkg.env$node_attr.function.if),
        XML::xmlNode(
          pkg.env$node_name.apply,
          attrs = c("function" = pkg.env$node_attr.function.equals),
          field_node,
          const_equal_node
        )
      )

    apply_node <- XML::append.xmlNode(apply_node, const_val_node)

    return (apply_node)
  }

#' Build Apply node with interval nodes for DerivedField node.
#'
#' @param var_details_row Variable details sheet row.
#' @param db_name Database name.
#'
#' @return Apply node with intervals for DerivedField node.
#'
#' @examples
build_ranged_derived_field_apply_node <-
  function (var_details_row, db_name) {
    margins <- get_margins(var_details_row[[pkg.env$columns.recFrom]])

    field_node <-
      build_variable_field_ref_node(var_details_row, db_name)

    const_node_gt <-
      XML::xmlNode(
        pkg.env$node_name.constant,
        attrs = c(dataType = pkg.env$node_attr.dataType.integer),
        value = margins[1]
      )
    const_node_lt <-
      XML::xmlNode(
        pkg.env$node_name.constant,
        attrs = c(dataType = pkg.env$node_attr.dataType.integer),
        value = margins[2]
      )

    apply_node_gt <-
      XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = c("function" = pkg.env$node_attr.function.greater_than),
        field_node,
        const_node_gt
      )
    apply_node_lt <-
      XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = c("function" = pkg.env$node_attr.function.less_than),
        field_node,
        const_node_lt
      )

    if (is_left_open(var_details_row[[pkg.env$columns.recFrom]])) {
      left_node <- apply_node_gt
    } else {
      or_node <-
        XML::xmlNode(pkg.env$node_name.apply,
                     attrs = c("function" = pkg.env$node_attr.function.or))
      equals_node <-
        XML::xmlNode(
          pkg.env$node_name.apply,
          attrs = c("function" = pkg.env$node_attr.function.equals),
          field_node,
          const_node_gt
        )

      left_node <-
        XML::append.xmlNode(or_node, apply_node_gt, equals_node)
    }

    if (is_right_open(var_details_row[[pkg.env$columns.recFrom]])) {
      right_node <- apply_node_lt
    } else {
      or_node <-
        XML::xmlNode(pkg.env$node_name.apply,
                     attrs = c("function" = pkg.env$node_attr.function.or))
      equals_node <-
        XML::xmlNode(
          pkg.env$node_name.apply,
          attrs = c("function" = pkg.env$node_attr.function.equals),
          field_node,
          const_node_lt
        )

      right_node <-
        XML::append.xmlNode(or_node, apply_node_lt, equals_node)
    }

    and_node <-
      XML::append.xmlNode(XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = c("function" = pkg.env$node_attr.function.and)
      ),
      left_node,
      right_node)
    apply_node <-
      XML::append.xmlNode(XML::xmlNode(
        pkg.env$node_name.apply,
        attrs = c("function" = pkg.env$node_attr.function.if)
      ), and_node)

    if (var_details_row[[pkg.env$columns.recTo]] %in% pkg.env$all_NAs) {
      const_node <- build_missing_const_node(var_details_row)
      apply_node <-
        XML::append.xmlNode(apply_node, const_node)
    } else if (var_details_row[[pkg.env$columns.recTo]] == pkg.env$rec_to_copy) {
      apply_node <- XML::append.xmlNode(apply_node, field_node)
    } else if (is_numeric(var_details_row[[pkg.env$columns.recTo]])) {
      const_val_node <-
        XML::xmlNode(
          pkg.env$node_name.constant,
          attrs = c(dataType = pkg.env$node_attr.dataType.integer),
          value = var_details_row[[pkg.env$columns.recTo]]
        )
      apply_node <- XML::append.xmlNode(apply_node, const_val_node)
    }

    return (apply_node)
  }

#' Build FieldRef node for variable.
#'
#' @param var_details_row Variable details sheet row.
#' @param db_name Database name.
#'
#' @return FieldRef node.
build_variable_field_ref_node <-
  function (var_details_row, db_name) {
    return (XML::xmlNode(
      pkg.env$node_name.field_ref,
      attrs = c(field = get_start_var_name(var_details_row, db_name))
    ))
  }


#' Build Constant node for a missing value for a variable.
#'
#' @param var_details_row Variable details sheet row.
#'
#' @return Constant node.
#'
#' @examples
build_missing_const_node <- function (var_details_row) {
  return (XML::xmlNode(
    pkg.env$node_name.constant,
    value = var_details_row[[pkg.env$columns.recTo]],
    attrs = c(dataType = pkg.env$node_attr.dataType.string)
  ))
}


#' Returns the PMML dataType attribute to use for a value
#'
#' @param value The variable whose value we need the dataType attribute for
#'
#' @return string The valye of the dataType attribute
#' @export
#'
#' @examples
get_data_type <- function(value) {
  # Supress the "NAs introduced by coercion" warning that comes when the
  # value cannot be converted to a number
  if(suppressWarnings(!is.na(as.numeric(value)))) {
    return(pkg.env$node_attr.dataType.integer)
  } else {
    return(pkg.env$node_attr.dataType.string)
  }
}


#' Constructs the PMML node to use for a value in a recEnd column
#'
#' @param var_details_row The row in a variable details sheet whose recEnd
#' column we will parse
#' @param db_name The database of the start variable
#'
#' @return list Contains the PMML node which can be either a Constant or
#' FieldRef node
#'
#' @export
#'
#' @examples
build_node_for_rec_to <- function(var_details_row, db_name) {
  rec_to <- var_details_row[[pkg.env$columns.recTo]]
  if (rec_to %in% pkg.env$all_NAs) {
    return(build_missing_const_node(var_details_row))
  } else if (rec_to == pkg.env$rec_to_copy) {
    return(build_variable_field_ref_node(var_details_row, db_name))
  } else {
    return(XML::xmlNode(
      pkg.env$node_name.constant,
      value = var_details_row[[pkg.env$columns.recTo]],
      attrs = c(dataType = get_data_type(var_details_row[[pkg.env$columns.recTo]]))
    ))
  }
}
