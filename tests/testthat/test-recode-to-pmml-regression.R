context("Regression tests for recode_to_pmml")

test_that("The PMML file is correctly generated if the else row is not at the end", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/else-row/else-row-expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/else-row/else-row-variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/else-row/else-row-variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    expected_pmml_file_path
  )
})

test_that("The start variables names are trimmed in the PMML file", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/trim-variable-start/trim-variable-start-expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/trim-variable-start/trim-variable-start-variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/trim-variable-start/trim-variable-start-variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    expected_pmml_file_path
  )
})

test_that("Duplicate Value nodes are not added to DerivedField nodes in the PMML file", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/duplicate-value-nodes/duplicate-value-nodes-expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/duplicate-value-nodes/duplicate-value-nodes-variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/duplicate-value-nodes/duplicate-value-nodes-variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    expected_pmml_file_path
  )
})

# Fixes a bug where if the databaseStart column for the first row in the
# variable details file for a variable did not contain the database argument
# it would throw an error rather than move onto the next rows.
test_that("It succeeds when the first variable details row does not have required database", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/no-database-var-details-row/no-database-var-details-row-expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/no-database-var-details-row/no-database-var-details-row-variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/no-database-var-details-row/no-database-var-details-row-variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    expected_pmml_file_path
  )
})

# Bug where if the databaseStart column had a value database_one_a and the database
# argument was database_one it would fail. This was because we were using the
# wrong regex with grepl. Instead of matching the entire word (database_one) we
# were looking for any string that contained the database argument.
test_that("RT 4: It succeeds when the variable details file has a part of the database name in its databaseStart column", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/regression/recode-to-pmml-rt-4/recode-to-pmml-rt-4-expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/regression/recode-to-pmml-rt-4/recode-to-pmml-rt-4-variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/regression/recode-to-pmml-rt-4/recode-to-pmml-rt-4-variables.csv"
  db_name <- "database_one"
  vars <- NULL
  custom_function_files <- c()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    expected_pmml_file_path
  )
})
