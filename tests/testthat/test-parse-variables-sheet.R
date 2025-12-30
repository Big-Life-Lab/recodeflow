test_that("Should return the parsed variables sheet when there are non-derived
          variables using database variables", {
  vars_sheet <- data.frame(
    variable = c("age", "sex", "height"),
    variableStart = c(
      "[age]", "cchs2001_p::sex", "[height], cchs2001_p::height"
    )
  )

  expected_result <- data.frame(vars_sheet)
  class(expected_result) <- c("variables_sheet", "data.frame")

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return the parsed variables sheet when there are derived
          variables using non-derived variables", {
  vars_sheet <- data.frame(
    variable = c("height", "weight", "BMI"),
    variableStart = c("[height]", "[weight]", "DerivedVar::[height, weight]")
  )

  expected_result <- data.frame(vars_sheet)
  class(expected_result) <- c("variables_sheet", "data.frame")

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return the parsed variables sheet when there are derived
          variables using other derived variables", {
  vars_sheet <- data.frame(
    variable = c("age", "height", "weight", "BMI", "BMI_x_age"),
    variableStart = c(
      "[age]", "height", "weight", "DerivedVar::[height, weight]",
      "DerivedVar::[BMI, age]"
    )
  )

  expected_result <- data.frame(vars_sheet)
  class(expected_result) <- c("variables_sheet", "data.frame")

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when there are derived variables using
          database columns", {
  # Tests for:
  # 1. Derived variable using a non-derived and a database column
  # 2. Derived variable using a database column
  # 3. Derived variable using non-derived variables
  # 4. Derived variable using no start variables
  vars_sheet <- data.frame(
    variable = c(
      "height", "BMI", "DrinkerType", "freq_cig", "SmokerType",
      "Empty"
    ),
    variableStart = c(
      "[height]", "DerivedVar::[height, cchs2001_p::weight]",
      "DerivedVar::[cchs2001_p::freq_drinks]", "[freq_cig]",
      "DerivedVar::[freq_cig]",
      "DerivedVar::[]"
    )
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_dependency_error(2, "cchs2001_p::weight"),
      .create_invalid_dependency_error(3, "cchs2001_p::freq_drinks")
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when the variableStart column is missing from
          the variables sheet", {
  vars_sheet <- data.frame(
    variable = c("age", "sex")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_missing_required_columns_error(c("variableStart"), c("variable"))
    )
  )
  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when the variable column is missing from the
          variables sheet", {
  vars_sheet <- data.frame(
    variableStart = c("[age]", "[sex]")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_missing_required_columns_error(c("variable"), c("variableStart"))
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should not fail when the variable sheet has no rows", {
  vars_sheet <- data.frame(
    variable = character(0),
    variableStart = character(0)
  )

  expected_result <- data.frame(vars_sheet)
  class(expected_result) <- c("variables_sheet", "data.frame")

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return an error when the variables sheet is not a data frame",
          {
  null_input <- NULL
  expected_result1 <- list(
    success = FALSE,
    errors = list(.create_invalid_input_type_error(null_input))
  )
  actual_result1 <- parse_variables_sheet(null_input)
  expect_equal(actual_result1, expected_result1)

  list_input <- list(a = 1)
  expected_result2 <- list(
    success = FALSE,
    errors = list(.create_invalid_input_type_error(list_input))
  )
  actual_result2 <- parse_variables_sheet(list_input)
  expect_equal(actual_result2, expected_result2)

  vector_input <- c("a", "b")
  expected_result3 <- list(
    success = FALSE,
    errors = list(.create_invalid_input_type_error(vector_input))
  )
  actual_result3 <- parse_variables_sheet(vector_input)
  expect_equal(actual_result3, expected_result3)

  numeric_input <- 42
  expected_result4 <- list(
    success = FALSE,
    errors = list(.create_invalid_input_type_error(numeric_input))
  )
  actual_result4 <- parse_variables_sheet(numeric_input)
  expect_equal(actual_result4, expected_result4)
})

test_that("Integration test with PBC variables sheet", {
  pbc_vars <- read.csv("../../inst/extdata/pbc_variables.csv")

  actual_result <- parse_variables_sheet(pbc_vars)

  expected_result <- data.frame(pbc_vars)
  class(expected_result) <- c("variables_sheet", "data.frame")

  expect_equal(actual_result, expected_result)
})

