test_that("Should return the parsed variables sheet when there are non-derived
          variables using database variables", {
  vars_sheet <- data.frame(
    variable = c("age", "sex", "height"),
    databaseStart = c("cchs2001_p", "cchs2001_p", "cchs2001_p"),
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
    databaseStart = c("db1", "db1", "db1"),
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
    databaseStart = c("db1", "db1", "db1", "db1", "db1"),
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
    databaseStart = rep("cchs2001_p", 6),
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
    variable = c("age", "sex"),
    databaseStart = c("db1", "db1")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_missing_required_columns_error(
        c("variableStart"), c("variable", "databaseStart"))
    )
  )
  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when the variable column is missing from the
          variables sheet", {
  vars_sheet <- data.frame(
    variableStart = c("[age]", "[sex]"),
    databaseStart = c("db1", "db1")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_missing_required_columns_error(
        c("variable"), c("variableStart", "databaseStart"))
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when the databaseStart column is missing from
          the variables sheet", {
  vars_sheet <- data.frame(
    variable = c("age", "sex"),
    variableStart = c("[age]", "[sex]")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_missing_required_columns_error(
        c("databaseStart"), c("variable", "variableStart"))
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should not fail when the variable sheet has no rows", {
  vars_sheet <- data.frame(
    variable = character(0),
    databaseStart = character(0),
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

test_that("Should return errors when variableStart references a database not
          declared in databaseStart", {
  vars_sheet <- data.frame(
    variable = c("age", "sex"),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    variableStart = c("[age]", "cchs2003_p::sex")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_database_reference_error(2, "cchs2003_p", "cchs2001_p")
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return one error per missing database when multiple
          undeclared databases are referenced", {
  vars_sheet <- data.frame(
    variable = c("sex"),
    databaseStart = c("cchs2001_p"),
    variableStart = c("cchs2003_p::sex, cchs2005_p::sex")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_database_reference_error(1, "cchs2003_p", "cchs2001_p"),
      .create_invalid_database_reference_error(1, "cchs2005_p", "cchs2001_p")
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should pass when databaseStart lists multiple databases and
          variableStart references a subset", {
  vars_sheet <- data.frame(
    variable = c("sex"),
    databaseStart = c("cchs2001_p, cchs2003_p, cchs2005_p"),
    variableStart = c("cchs2003_p::sex, cchs2005_p::sex")
  )

  expected_result <- data.frame(vars_sheet)
  class(expected_result) <- c("variables_sheet", "data.frame")

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when databaseStart is NA but variableStart
          contains database references", {
  vars_sheet <- data.frame(
    variable = c("age"),
    databaseStart = NA_character_,
    variableStart = c("cchs2001_p::age")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_database_reference_error(
        1, "cchs2001_p", character(0))
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should validate databases referenced via the
          db::[DerivedVar::[...]] nested form", {
  vars_sheet <- data.frame(
    variable = c("RACDPAL"),
    databaseStart = c("cchs2003_p"),
    variableStart = c(
      paste0(
        "cchs2001_p::[DerivedVar::[RAC_1, RAC_2A]], ",
        "cchs2003_p::RACCDPAL"
      )
    )
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_database_reference_error(1, "cchs2001_p", "cchs2003_p")
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Should not flag database references that appear inside a
          DerivedVar::[...] block (those are caught by invalid_dependency)", {
  # Row 1 has a derived var that references a database column inline and also
  # references a database that is not declared in databaseStart. Only the
  # invalid_dependency error should be raised; no invalid_database_reference
  # error should be raised for the undeclared database.
  vars_sheet <- data.frame(
    variable = c("BMI"),
    databaseStart = c("cchs2001_p"),
    variableStart = c("DerivedVar::[height, cchs2003_p::weight]")
  )

  expected_result <- list(
    success = FALSE,
    errors = list(
      .create_invalid_dependency_error(1, "cchs2003_p::weight")
    )
  )

  actual_result <- parse_variables_sheet(vars_sheet)

  expect_equal(actual_result, expected_result)
})

test_that("Integration test with PBC variables sheet", {
  pbc_vars <- read.csv("../../inst/extdata/pbc_variables.csv")

  actual_result <- parse_variables_sheet(pbc_vars)

  expected_result <- data.frame(pbc_vars)
  class(expected_result) <- c("variables_sheet", "data.frame")

  expect_equal(actual_result, expected_result)
})

