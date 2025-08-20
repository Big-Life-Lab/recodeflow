test_that("get_start_variables works with non-derived variables", {
  variables_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("study1_db, study2_db, study3_db"),
    variableStart = c(
      "study1_db::age_months, study2_db::age_years, [age_default]"
    )
  )

  variable_details <- data.frame(
    variable = c("age", "age", "age"),
    databaseStart = c("study1_db, study2_db, study3_db"),
    variableStart = c(
      "study1_db::age_months, study2_db::age_years, [age_default]"
    )
  )

  # Test study1_db gets age_months (database-specific)
  actual1 <- get_start_variables(
    "age", "study1_db", variables_sheet, variable_details
  )
  expected1 <- list(list(name = "age_months", type = "database"))
  expect_equal(actual1, expected1)

  # Test study2_db gets age_years (database-specific)
  actual2 <- get_start_variables(
    "age", "study2_db", variables_sheet, variable_details
  )
  expected2 <- list(list(name = "age_years", type = "database"))
  expect_equal(actual2, expected2)

  # Test study3_db gets age_default (default variable)
  actual3 <- get_start_variables(
    "age", "study3_db", variables_sheet, variable_details
  )
  expected3 <- list(list(name = "age_default", type = "database"))
  expect_equal(actual3, expected3)
})

test_that("get_start_variables works with derived variables", {
  variables_sheet <- data.frame(
    variable = c(
      "nutrition_score",
      "age_clean",
      "bmi_derived",
      "activity_score",
      "weight_var",
      "height_var"
    ),
    databaseStart = c(
      "study1_db,
      study2_db,
      study3_db",
      "study1_db",
      "study2_db",
      "study1_db",
      "study3_db",
      "study3_db"
    ),
    variableStart = c(
      "DerivedVar::[age_clean, tables::nutrition_lookup, activity_score],
       DerivedVar::[bmi_derived, tables::food_table],
       DerivedVar::[weight_var, height_var]",
      "[age_raw]",
      "DerivedVar::[height_raw, weight_raw]",
      "DerivedVar::[steps_var, calories_var]",
      "[weight_raw]",
      "[height_raw]"
    )
  )

  variable_details <- data.frame(
    variable = c(
      "nutrition_score",
      "nutrition_score",
      "nutrition_score",
      "age_clean",
      "bmi_derived",
      "activity_score",
      "weight_var",
      "height_var"
    ),
    databaseStart = c(
      "study1_db",
      "study2_db",
      "study3_db",
      "study1_db",
      "study2_db",
      "study1_db",
      "study3_db",
      "study3_db"
    ),
    variableStart = c(
      "DerivedVar::[age_clean, tables::nutrition_lookup, activity_score]",
      "DerivedVar::[bmi_derived, tables::food_table]",
      "DerivedVar::[weight_var, height_var]",
      "[age_raw]",
      "DerivedVar::[height_raw, weight_raw]",
      "DerivedVar::[steps_var, calories_var]",
      "[weight_raw]",
      "[height_raw]"
    )
  )

  actual1 <- get_start_variables(
    "nutrition_score", "study1_db", variables_sheet, variable_details
  )
  expected1 <- list(
    list(name = "age_clean", type = "non-derived"),
    list(name = "nutrition_lookup", type = "table"),
    list(name = "activity_score", type = "derived")
  )
  expect_equal(actual1, expected1)

  # Test study2_db gets bmi_derived (derived) and food_table (table)
  actual2 <- get_start_variables(
    "nutrition_score", "study2_db", variables_sheet, variable_details
  )
  expected2 <- list(
    list(name = "bmi_derived", type = "derived"),
    list(name = "food_table", type = "table")
  )
  expect_equal(actual2, expected2)

  actual3 <- get_start_variables(
    "nutrition_score", "study3_db", variables_sheet, variable_details
  )
  expected3 <- list(
    list(name = "weight_var", type = "non-derived"),
    list(name = "height_var", type = "non-derived")
  )
  expect_equal(actual3, expected3)
})

test_that("get_start_variables throws error for non-existent variable", {
  variables_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  variable_details <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  expect_error(
    get_start_variables("height", "db1", variables_sheet, variable_details),
    "`variables_sheet` is missing rows for the `variable` and `database`"
  )
})

test_that("get_start_variables throws error for non-existent database", {
  variables_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  variable_details <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  expect_error(
    get_start_variables("age", "db2", variables_sheet, variable_details),
    "`variables_sheet` is missing rows for the `variable` and `database`"
  )
})

test_that(
  "get_start_variables throws error for conflicting variableStart values",
  {
    variables_sheet <- data.frame(
      variable = c("age", "age"),
      databaseStart = c("db1", "db1"),
      variableStart = c("[age_years]", "[age_months]")
    )

    variable_details <- data.frame(
      variable = c("age", "age"),
      databaseStart = c("db1", "db1"),
      variableStart = c("[age_years]", "[age_months]")
    )

    expect_error(
      get_start_variables("age", "db1", variables_sheet, variable_details),
      "Start variables must be the same for all rows of a variable for a"
    )
  }
)

test_that("get_start_variables validates input arguments", {
  variables_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  variable_details <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  # Test non-character variable
  expect_error(
    get_start_variables(123, "db1", variables_sheet, variable_details),
    "`variable` must be a scalar string"
  )

  # Test multiple variable names
  expect_error(
    get_start_variables(
      c("age", "height"), "db1", variables_sheet, variable_details
    ),
    "`variable` must be a scalar string"
  )

  expect_error(
    get_start_variables("", "db1", variables_sheet, variable_details),
    "`variable` must have at least 1 character"
  )

  # Test non-character database
  expect_error(
    get_start_variables("age", 123, variables_sheet, variable_details),
    "`database` must be a scalar string"
  )

  # Test non-data.frame variables_sheet
  expect_error(
    get_start_variables("age", "db1", "not_a_dataframe", variable_details),
    "`variables_sheet` must be a data.frame"
  )

  # Test non-data.frame variable_details
  expect_error(
    get_start_variables("age", "db1", variables_sheet, "not_a_dataframe"),
    "`variable_details_sheet` must be a data.frame"
  )
})

test_that("get_start_variables validates required columns", {
  # Missing variableStart column
  incomplete_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("db1")
  )

  variable_details <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("[age_years]")
  )

  expect_error(
    get_start_variables("age", "db1", incomplete_sheet, variable_details),
    "`variables_sheet` is missing required columns"
  )
})

test_that("get_start_variables throws error for unrecognized format", {
  variables_sheet <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("some_unrecognized_format")
  )

  variable_details <- data.frame(
    variable = c("age"),
    databaseStart = c("db1"),
    variableStart = c("some_unrecognized_format")
  )

  expect_error(
    get_start_variables("age", "db1", variables_sheet, variable_details),
    "Can't figure out the start variables for a variableStart value"
  )
})
