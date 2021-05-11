pkg.env <- new.env(parent = emptyenv())

pkg.env$NA_invalid <- "NA::a"
pkg.env$NA_missing <- "NA::b"
pkg.env$all_NAs <- c(pkg.env$NA_invalid, pkg.env$NA_missing)

pkg.env$margin_separator <- ","
pkg.env$cat_start_label_separator <- ";"

pkg.env$missing <- "missing"

pkg.env$db_var_start_infix <- "::"

pkg.env$rec_to_copy <- "copy"

pkg.env$var_details_cat <- "cat"
pkg.env$var_details_cont <- "cont"

pkg.env$var_cat <- "Categorical"

pkg.env$node_name.pmml <- "PMML"
pkg.env$node_name.data_dict <- "DataDictionary"
pkg.env$node_name.trans_dict <- "TransformationDictionary"
pkg.env$node_name.data_field <- "DataField"
pkg.env$node_name.extension <- "Extension"
pkg.env$node_name.constant <- "Constant"
pkg.env$node_name.value <- "Value"
pkg.env$node_name.interval <- "Interval"
pkg.env$node_name.derived_field <- "DerivedField"
pkg.env$node_name.apply <- "Apply"
pkg.env$node_name.field_ref <- "FieldRef"
pkg.env$node_name.local_transformations <- "LocalTransformations"
pkg.env$node_name.define_function <- "DefineFunction"

pkg.env$node_namespace.pmml <- "http://www.dmg.org/PMML-4_4"

pkg.env$node_attr.pmml_version <- "4.4"
pkg.env$node_attr.name.var_start_label <- "variableStartLabel"
pkg.env$node_attr.name.label_long <- "labelLong"
pkg.env$node_attr.name.units <- "units"
pkg.env$node_attr.name.cat_label_long <- "catLabelLong"

pkg.env$node_attr.closure.closed <- "closedClosed"
pkg.env$node_attr.closure.leftOpen <- "openClosed"
pkg.env$node_attr.closure.open <- "openOpen"
pkg.env$node_attr.closure.rightOpen <- "closedOpen"

pkg.env$node_attr.missing.true <- "true"
pkg.env$node_attr.optype.categorical <- "categorical"
pkg.env$node_attr.optype.continuous <- "continuous"

pkg.env$node_attr.function.if <- "if"
pkg.env$node_attr.function.and <- "and"
pkg.env$node_attr.function.or <- "or"
pkg.env$node_attr.function.greater_than <- "greaterThan"
pkg.env$node_attr.function.less_than <- "lessThan"
pkg.env$node_attr.function.equals <- "equal"

pkg.env$node_attr.dataType.integer <- "integer"
pkg.env$node_attr.dataType.string <- "string"
pkg.env$node_attr.dataType.float <- "float"

pkg.env$node_attr.property.valid <- "valid"
pkg.env$node_attr.property.invalid <- "invalid"
pkg.env$node_attr.property.missing <- "missing"

pkg.env$node_attr.DefineFunction.name <- "name"

pkg.env$node_attr.FieldRef.field <- "field"

pkg.env$node_attr.Apply.function <- "function"

pkg.env$columns.Variable <- "variable"
pkg.env$columns.VariableLabel <- "labelLong"
pkg.env$columns.label <- "label"
pkg.env$columns.DatabaseStart <- "databaseStart"
pkg.env$columns.VariableStart <- "variableStart"
pkg.env$columns.recTo <- "recEnd"
pkg.env$columns.recFrom <- "recStart"
pkg.env$columns.Notes <- "notes"
pkg.env$columns.VariableType <- "variableType"
pkg.env$columns.variablesDetails.typeStart <- "typeStart"
pkg.env$columns.Units <- "units"
pkg.env$columns.value.CatType <- "cat"
pkg.env$columns.ToType <- "typeEnd"
pkg.env$columns.CatLabel <- "catLabel"
pkg.env$columns.CatLabelLong <- "catLabelLong"
pkg.env$columns.Role <- "role"

pkg.env$variable_details$columns.recFrom.elseValue <- "else"

pkg.env$recode.key.id.from <- "id_from::"
pkg.env$recode.key.func <- "Func::"
pkg.env$recode.key.map <- "map::"
pkg.env$recode.key.derived.var <- "DerivedVar::"
