context("Regression tests for recode_to_pmml")

test_that("RT-1: The PMML file is correctly generated if the else row is not at the end", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-1/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-1/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-1/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})

test_that("RT-2: The start variables names are trimmed in the PMML file", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})

test_that("RT-3: Duplicate Value nodes are not added to DerivedField nodes in the PMML file", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})

# Fixes a bug where if the databaseStart column for the first row in the
# variable details file for a variable did not contain the database argument
# it would throw an error rather than move onto the next rows.
test_that("RT-4: It succeeds when the first variable details row does not have required database", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-2/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})

# Bug where if the databaseStart column had a value database_one_a and the database
# argument was database_one it would fail. This was because we were using the
# wrong regex with grepl. Instead of matching the entire word (database_one) we
# were looking for any string that contained the database argument.
test_that("RT 5: It succeeds when the variable details file has a part of the database name in its databaseStart column", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-5/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-5/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-5/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})

# The code was throwing a hard to understand error when no details was found for
# a variable that was present in the variables sheet.
test_that("RT 6: Should throw an error when no details is present for a variable found in the variables sheet", {
  variable_details_sheet <- read.csv(
    "../../assets/tests/recode-to-pmml/regression/rt-6/variable-details.csv",
    fileEncoding = "UTF-8-BOM"
  )
  variables_sheet <- read.csv(
    "../../assets/tests/recode-to-pmml/regression/rt-6/variables.csv",
    fileEncoding = "UTF-8-BOM"
  )
  db_name <- 'database_one'
  vars <- NULL
  custom_function_files <- c()

  expect_error(
    recode_to_pmml(
      variable_details_sheet,
      variables_sheet,
      db_name,
      vars,
      custom_function_files
    ),
    "No rows found for variable variable_one in the variable details sheet. The variable was found in the variables sheet."
  )
})

test_that("RT-7: Should handle the case where a non-derived variable does not have an else row", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/rt-7/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-7/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/rt-7/variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    list(),
    expected_pmml_file_path
  )
})
