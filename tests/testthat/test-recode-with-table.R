context("recode_with_table")

test_that("Non-derived variables can be created from derived variables", {
  data <- data.frame(
    start_variable_one = c(1)
  )
  variables <- data.frame(
    variable = c("non_derived_variable_one", "derived_variable", "non_derived_variable_two"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[non_derived_variable_one]", "derived_variable")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("non_derived_variable_one", "derived_variable", "non_derived_variable_two"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[non_derived_variable_one]", "DerivedVar::[derived_variable]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "Func::derived_variable", "copy"),
    numValidCategories = c(1,"N/A",1),
    recStart = c("else", "N/A", "else"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  # Custom function for the derived variable
  derived_variable <- function(non_derived_variable_one) {
    return(non_derived_variable_one)
  }
  .GlobalEnv[["derived_variable"]] <- derived_variable

  actual_output <- recodeflow::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name
  )

  expected_output <- data.frame(
    non_derived_variable_one = c(1),
    derived_variable = c(1),
    non_derived_variable_two = c(1)
  )
  attr(expected_output$non_derived_variable_one, "unit") <- "N/A"
  attr(expected_output$non_derived_variable_one, "label_long") <- ""
  attr(expected_output$non_derived_variable_two, "unit") <- "N/A"
  attr(expected_output$non_derived_variable_two, "label_long") <- ""

  expect_equal(actual_output, expected_output)

  on.exit(rm("derived_variable", envir = .GlobalEnv))
})

test_that("Tables work with custom functions", {
  data <- data.frame(
    start_var = c(1)
  )
  variables <- data.frame(
    variable = c("derived_variable_one", "derived_variable_two"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_var]", "DerivedVar::[derived_variable_one, tables::table_one]")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("derived_variable_one", "derived_variable_two"),
    typeEnd = c("cont", "cont"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_var]", "DerivedVar::[derived_variable_one, tables::table_one]"),
    typeStart = c("N/A", "N/A"),
    recEnd = c("copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A"),
    recStart = c("else", "N/A"),
    catLabel = c("", ""),
    catLabelLong = c("", "")
  )
  tables <- list(
    table_one = data.frame(
      derived_variable_one = c(1),
      derived_variable_two = c(2)
    )
  )
  # Custom function for the derived variable
  func_1 <- function(derived_variable_one, table_one) {
    return(table_one[table_one$derived_variable_one == derived_variable_one,]$derived_variable_two)
  }
  .GlobalEnv[["func_1"]] <- func_1

  actual_output <- recodeflow::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name,
    tables = tables
  )

  expected_output <- data.frame(
    derived_variable_one = c(1),
    derived_variable_two = c(2)
  )
  attr(expected_output$derived_variable_one, "unit") <- "N/A"
  attr(expected_output$derived_variable_one, "label_long") <- ""

  expect_equal(actual_output, expected_output)

  on.exit(rm("func_1", envir = .GlobalEnv))
})
