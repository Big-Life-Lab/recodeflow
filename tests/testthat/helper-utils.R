#' Does the setup and cleanup for a custom function to be used in a
#' rec_with_table test
#'
#' @param custom_function the custom function
#' @param env the environment in which the test is executed. You should not
#' need to set this.
#' @returns NULL
#' @examples
#' test_that("Custom function works", {
#'   BMI.func <- function(height, weight) {
#'     return(weight / (height * height))
#'   }
#'   setup_custom_function(BMI.func)
#'
#'   variable_details <- data.frame(
#'     variable = c("height", "weight", "BMI"),
#'     typeEnd = c("cont", "cont", "cont"),
#'     databaseStart = c("health", "health", "health"),
#'     variableStart = c("
#'       [height_raw]", "[weight_raw]", "DerivedVar::[height, weight]"
#'     ),
#'     typeStart = c("cont", "cont", "N/A"),
#'     recEnd = c("copy", "copy", "Func::BMI.func"),
#'     numValidCategories = c("N/A", "N/A", "N/A"),
#'     recStart = c("else", "else", "N/A"),
#'     catLabel = c("N/A", "N/A", "N/A"),
#'     catLabelLong = c("N/A", "N/A", "N/A")
#'   )
#'
#'   data <- data.frame(
#'     height_raw = c(180),
#'     weight_raw = c(50)
#'   )
#'
#'   expect_no_error(rec_with_table(
#'     data = data,
#'     variables = c("height", "weight", "BMI"),
#'     variable_details = variable_details,
#'     database_name = "health"
#'   ))
#' })
setup_custom_function <- function(custom_function, env = parent.frame()) {
  original_name <- deparse(substitute(custom_function))
  .GlobalEnv[[original_name]] <- custom_function 
  withr::defer(rm(list = original_name, envir = .GlobalEnv), env = env)
}
