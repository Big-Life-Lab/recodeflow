context("Integration Tests")
library("XML")

test_that("The PMML file is correctly generated", {
  expected_pmml_file <- "../../assets/tests/integration/expected-pmml.xml"
  expected_pmml_string_lines <- readLines(
    expected_pmml_file,
    file.info(expected_pmml_file)$size
  )
  expected_pmml_string <- paste(expected_pmml_string_lines, collapse = "\n")

  var_details_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variable_details.csv", stringsAsFactors = FALSE)
  vars_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variables.csv", stringsAsFactors = FALSE)
  db_name <- "cchs2001_p"
  vars <- c("ADL_01", "ADL_02", "ADL_05", "ALW_2A1")

  actual_pmml <- xflow_to_pmml(
    var_details_sheet,
    vars_sheet,
    db_name,
    vars
  )

  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  expect_equal(actual_pmml_string, expected_pmml_string)
})
