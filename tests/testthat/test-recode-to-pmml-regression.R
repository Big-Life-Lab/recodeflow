context("Regression tests for recode_to_pmml")

test_that("The PMML file is correctly generated if the else row is not at the end", {
  expected_pmml_file <- "../../assets/tests/recode-to-pmml/regression/else-row-expected-pmml.xml"
  expected_pmml_string <- XML::toString.XMLNode(XML::xmlTreeParse(expected_pmml_file)[[1]]$children$PMML)

  var_details_sheet <- read.csv("../../assets/tests/recode-to-pmml/regression/else-row-variable-details.csv",
                                stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8-BOM")
  vars_sheet <- read.csv("../../assets/tests/recode-to-pmml/regression/else-row-variables.csv", stringsAsFactors = FALSE)
  db_name <- "database_one"
  vars <- vars_sheet$variable

  actual_pmml <- recode_to_pmml(
    var_details_sheet,
    vars_sheet,
    db_name,
    vars
  )

  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  expect_equal(actual_pmml_string, expected_pmml_string)
})
