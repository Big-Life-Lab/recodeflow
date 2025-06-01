pkg.env <- new.env(parent = emptyenv())

pkg.env$columns.variable <- "variable"
pkg.env$columns.variableLabel <- "labelLong"
pkg.env$columns.label <- "label"
pkg.env$columns.databaseStart <- "databaseStart"
pkg.env$columns.variableStart <- "variableStart"
pkg.env$columns.recTo <- "recEnd"
pkg.env$columns.recFrom <- "recStart"
pkg.env$columns.notes <- "notes"
pkg.env$columns.variableType <- "variableType"
pkg.env$columns.variablesDetails.typeStart <- "typeStart"
pkg.env$columns.units <- "units"
pkg.env$columns.value.catType <- "cat"
pkg.env$columns.toType <- "typeEnd"
pkg.env$columns.catLabel <- "catLabel"
pkg.env$columns.catLabelLong <- "catLabelLong"
pkg.env$columns.role <- "role"

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
