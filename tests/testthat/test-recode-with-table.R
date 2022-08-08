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

