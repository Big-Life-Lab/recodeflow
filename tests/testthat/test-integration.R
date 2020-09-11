context("Integration Tests")
library("XML")

test_that("The PMML file is correctly generated", {
  expected_pmml_file <- "../../assets/tests/integration/expected-pmml.xml"
  expected_pmml_string <- trimws(readChar(
    expected_pmml_file,
    file.info(expected_pmml_file)$size
  ))

  var_details_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variable_details.csv")
  vars_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variables.csv")
  db_name <- "cchs2001_p"
  vars <- c("ADL_01", "ALW_2A1")

  actual_pmml <- xflow_to_pmml(
    var_details_sheet,
    vars_sheet,
    db_name,
    vars
  )

  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  expect_equal(actual_pmml_string, expected_pmml_string)
})
