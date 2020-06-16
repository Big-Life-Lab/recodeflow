context("Integration Tests")

test_that("The PMML file is correctly generated", {
  expected_pmml_file <- "../../assets/tests/integration/expected-pmml.xml"
  expected_pmml <- readChar(
    expected_pmml_file,
    file.info(expected_pmml_file)$size
  )

  var_details_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variable_details.csv")
  vars_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variables.csv")
  db_name <- "cchs2001_p"
  vars <- c("ADL_01", "ALW_2A1")

  actual_pmml <- pmml.xflow_to_pmml(
    vars_details_sheet,
    vars_sheet,
    db_name,
    vars
  )

  expect_equal(actual_pmml, expected_pmml)
})
