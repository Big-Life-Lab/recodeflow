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
