write_temp_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  write.csv(x, path)
  return(path)
}

# This test was taken from the HUIPoRT project and uses the development
# dataset from it
test_that("Integration test", {
  variables_sheet <- read.csv(
    "./integration-assets/variables.csv", fileEncoding = "UTF-8-BOM")
  variable_details_sheet <- read.csv(
    "./integration-assets/variable-details-sheet.csv",
    fileEncoding = "UTF-8-BOM"
  )
  raw_data_paths <- list.files(
    "./integration-assets/data", pattern = ".RData", full.names = TRUE)
  actual_data <- raw_data_paths %>%
    purrr::map(function(raw_data_path) {
      raw_data_name <- load(raw_data_path)
      
      recoded_data <- rec_with_table(
        get(raw_data_name),
        database_name = raw_data_name,
        variables = variables_sheet,
        variable_details = variable_details_sheet,
        id_role_name = "id",
        custom_function_path = "./integration-assets/custom-functions.R",
        notes = FALSE,
        append_non_db_columns = TRUE 
      )

      return(recoded_data)
    }) %>%
    purrr::list_rbind()

  expect_snapshot_file(write_temp_csv(actual_data), "integration.csv")
})
