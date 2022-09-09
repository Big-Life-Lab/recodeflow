run_recode_to_pmml_test <- function(
  variable_details_sheet_path,
  variables_sheet_path,
  db_name,
  vars,
  custom_function_files,
  table_paths,
  expected_pmml_file_path
) {
  expected_pmml_string <- XML::toString.XMLNode(
    XML::xmlTreeParse(expected_pmml_file_path)[[1]]$children$PMML
  )

  variable_details_sheet <- read.csv(
    variable_details_sheet_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  variables_sheet <- read.csv(
    variables_sheet_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  if(is.null(vars)) {
    vars <- variables_sheet$variable
  }
  actual_pmml <- recode_to_pmml(
    variable_details_sheet,
    variables_sheet,
    db_name,
    vars,
    custom_function_files,
    table_paths
  )
  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  expect_equal(actual_pmml_string, expected_pmml_string)
}
