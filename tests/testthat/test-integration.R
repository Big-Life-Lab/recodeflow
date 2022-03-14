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
    expected_pmml_file_path
  )
})
