context("recode_with_table custom variables")

test_that("Should correct recode non-derived custom variables", {
  variables <- data.frame(
    variable = c("variable_one"),
    label = c(""),
    labelLong = c(""),
    units = c("N/A"),
    variableType = c("Categorical"),
    databaseStart = c("database_one"),
    variableStart = c("[start_variable_one]")
  )
  variable_details <- data.frame(
    variable = c("custom_variable_one", "custom_variable_one", "variable_one"),
    customVariable = c("Yes", "Yes", "custom_variable_one"),
    typeEnd = c("cat", "cat", "cat"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("N/A", "N/A", "[start_variable_one]"),
    typeStart = c("cat", "cat", "cat"),
    recEnd = c("1", "2", "N/A"),
    numValidCat = c("2", "2", "N/A"),
    recStart = c("1", "else", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  database_name <- "database_one"
  data <- data.frame(
    start_variable_one = c(1,3)
  )

  expected_data <- data.frame(
    variable_one = c(1,2)
  )
  expected_data$variable_one <- as.factor(expected_data$variable_one)

  actual_data <- recodeflow:::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name
  )
  attributes(expected_data$variable_one) <- attributes(actual_data$variable_one)

  expect_equal(actual_data, expected_data)
})

test_that("Should correct recode multiple non-derived custom variables", {
  variables <- data.frame(
    variable = c("variable_one", "variable_two"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Categorical", "Categorical"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_variable_one]", "[start_variable_two]")
  )
  variable_details <- data.frame(
    variable = c("custom_variable_one", "custom_variable_one", "custom_variable_two", "custom_variable_two", "variable_one", "variable_two"),
    customVariable = c("Yes", "Yes", "Yes", "Yes", "custom_variable_one", "custom_variable_two"),
    typeEnd = c("cat", "cat", "cat", "cat", "cat", "cat"),
    databaseStart = c("database_one", "database_one", "database_one", "database_one", "database_one", "database_one"),
    variableStart = c("N/A", "N/A", "N/A", "N/A", "[start_variable_one]", "[start_variable_two]"),
    typeStart = c("cat", "cat", "cat", "cat", "cat", "cat"),
    recEnd = c("1", "2", "1", "2", "N/A", "N/A"),
    numValidCat = c("2", "2", "2", "2", "N/A", "N/A"),
    recStart = c("1", "else", "1", "else", "N/A", "N/A"),
    catLabel = c("", "", "", "", "", ""),
    catLabelLong = c("", "", "", "", "", "")
  )
  database_name <- "database_one"
  data <- data.frame(
    start_variable_one = c(1,3),
    start_variable_two = c(1,3)
  )

  expected_data <- data.frame(
    variable_one = c(1,2),
    variable_two = c(1,2)
  )
  expected_data$variable_one <- as.factor(expected_data$variable_one)
  expected_data$variable_two <- as.factor(expected_data$variable_two)

  actual_data <- recodeflow:::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name
  )
  attributes(expected_data$variable_one) <- attributes(actual_data$variable_one)
  attributes(expected_data$variable_two) <- attributes(actual_data$variable_two)

  expect_equal(actual_data, expected_data)
})

test_that("Should correctly recode derivec custom variables", {
  variables <- data.frame(
    variable = c("variable_one"),
    label = c(""),
    labelLong = c(""),
    units = c("N/A"),
    variableType = c("Categorical"),
    databaseStart = c("database_one"),
    variableStart = c("[start_variable_one]")
  )
  variable_details <- data.frame(
    variable = c("custom_variable_one", "custom_variable_one", "variable_one"),
    customVariable = c("Yes", "Yes", "custom_variable_one"),
    typeEnd = c("cat", "cat", "cat"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("N/A", "N/A", "DerivedVar::[start_variable_one]"),
    typeStart = c("cat", "cat", "cat"),
    recEnd = c("Func::func1", "1", "N/A"),
    numValidCat = c("1", "1", "N/A"),
    recStart = c("N/A", "N/A", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  .GlobalEnv[["func1"]] <- function(start_variable_one) {
    return(1)
  }
  database_name <- "database_one"
  data <- data.frame(
    start_variable_one = c(3)
  )

  expected_data <- data.frame(
    variable_one = c(1)
  )
  expected_data$variable_one <- as.factor(expected_data$variable_one)

  actual_data <- recodeflow::rec_with_table(
    data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name
  )

  attributes(expected_data$variable_one) <- attributes(actual_data$variable_one)
  expect_equal(actual_data, expected_data)
})
