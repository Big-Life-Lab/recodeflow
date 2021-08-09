context("Integration Tests")
library("XML")

test_that("The PMML file is correctly generated", {
  expected_pmml_file <- "../../assets/tests/integration/expected-pmml.xml"
  expected_pmml_string <- XML::toString.XMLNode(XML::xmlTreeParse(expected_pmml_file)[[1]]$children$PMML)

  var_details_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variable_details.csv",
                                stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8-BOM")
  vars_sheet <- read.csv("../../assets/tests/integration/cchsflow MSW - variables.csv", stringsAsFactors = FALSE)
  db_name <- "cchs2001_p"
  vars <- c("ADL_01", "ALW_2A1", "DHHGAGE_cont", "variable_four", "variable_five", "variable_nine", "variable_ten", "variable_eleven")

  actual_pmml <- recode_to_pmml(
    var_details_sheet,
    vars_sheet,
    db_name,
    vars,
    custom_function_files = c(
      "../../assets/tests/integration/custom-functions-1.R",
      "../../assets/tests/integration/custom-functions-2.R")
  )

  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  expect_equal(actual_pmml_string, expected_pmml_string)
})
