context("Integration Tests")
library("XML")

test_that("The PMML file is correctly generated", {
  expected_pmml_file_path <- "../../assets/tests/integration/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/integration/cchsflow MSW - variable_details.csv"
  variables_sheet_path <- "../../assets/tests/integration/cchsflow MSW - variables.csv"
  db_name <- "cchs2001_p"
  vars <- c("ADL_01", "ALW_2A1", "DHHGAGE_cont", "variable_four", "variable_five", "variable_nine", "variable_ten", "variable_eleven")
  custom_function_files <- c(
    "../../assets/tests/integration/custom-functions-1.R",
    "../../assets/tests/integration/custom-functions-2.R"
  )

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

test_that("RP-2: tables are correctly recoded", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/features/rp-2/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/features/rp-2/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/features/rp-2/variables.csv"
  db_name <- "database_one"
  vars <- c("variable_one", "derived_variable")
  custom_function_files <- c(
    "../../assets/tests/recode-to-pmml/features/rp-2/custom-functions.R"
  )
  table_paths <- list(
    "table_one" = "../../assets/tests/recode-to-pmml/features/rp-2/table-one.csv"
  )

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    table_paths,
    expected_pmml_file_path
  )
})

test_that("RP-3: string arguments in derived var are correctly converted", {
  expected_pmml_file_path <- "../../assets/tests/recode-to-pmml/features/rp-3/expected-pmml.xml"

  variable_details_sheet_path <- "../../assets/tests/recode-to-pmml/features/rp-3/variable-details.csv"
  variables_sheet_path <- "../../assets/tests/recode-to-pmml/features/rp-3/variables.csv"
  db_name <- "database_one"
  vars <- c("variable_one", "derived_variable")
  custom_function_files <- c(
    "../../assets/tests/recode-to-pmml/features/rp-3/custom-functions.R"
  )
  table_paths <- list()

  run_recode_to_pmml_test(
    variable_details_sheet_path,
    variables_sheet_path,
    db_name,
    vars,
    custom_function_files,
    table_paths,
    expected_pmml_file_path
  )
})
