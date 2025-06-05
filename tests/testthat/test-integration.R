snapshot_file_name <- 'integration.RData'

write_temp_rdata <- function(x) {
  path <- tempfile(fileext = ".RData")
  save(x, file = path)
  return(path)
}

get_snapshot_file_path <- function(file_name) {
  return(file.path('./_snaps/integration/', file_name))
}

compare_snapshot <- function() {
  env_load <- new.env()

  expected_data_name <- load(
    get_snapshot_file_path(snapshot_file_name), envir = env_load)
  expected_data <- get(expected_data_name, env_load)

  actual_data_name <- load(
    get_snapshot_file_path('integration.new.RData'), envir = env_load)
  actual_data <- get(actual_data_name, env_load)

  print(waldo::compare(expected_data, actual_data))
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

  # The following code fixes the issue that testthat does not have an in-built
  # way to diff .RData files.
  tryCatch({
      expect_snapshot_file(
        write_temp_rdata(actual_data),
        snapshot_file_name
      )
    },
    error = function(e) {
      compare_snapshot()
      stop(e)
    }
  )
})
