pkg.env <- new.env(parent = emptyenv())

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
variable_details_columns <- list(
  template_variable = list(
    name = "templateVariable",
    values = list(
      no = "No",
      yes = "Yes"
    )
  )
)

pkg.env$recode.key.id.from <- "id_from::"
pkg.env$recode.key.func <- "Func::"
pkg.env$recode.key.map <- "map::"
pkg.env$recode.key.derived.var <- "DerivedVar::"
pkg.env$recode.key.tables <- "tables::"
