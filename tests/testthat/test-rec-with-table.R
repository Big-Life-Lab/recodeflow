context("Integration Tests")

test_that("Normal variables that depend on derived variables are properly recoded", {
  data <- data.frame(
    var_one_start = c(1, 2, 3)
  )

  variables_sheet <- read.csv(
    file.path("../../assets/tests/integration/rec-with-table/variables-sheet.csv"),
    fileEncoding = "UTF-8-BOM")

  variable_details_sheet <- read.csv(
    file.path(getwd(), "../../assets/tests/integration/rec-with-table/variable-details-sheet.csv"),
    fileEncoding = "UTF-8-BOM")

  recoded_data <- recodeflow::rec_with_table(
    data,
    database_name = "db_one",
    variables = variables_sheet,
    variable_details = variable_details_sheet,
    custom_function_path = file.path(getwd(), "../../assets/tests/integration/rec-with-table/custom-functions.R")
  )

  expected_recoded_data <- data.frame(
    var_one = data$var_one_start,
    var_two = data$var_one_start,
    var_three = data$var_one_start
  )
  expect_equal(recoded_data, expected_recoded_data)
})
