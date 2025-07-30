#' Does the setup and cleanup for a custom function to be used in a
#' rec_with_table test
#'
#' @param custom_function the custom function
#' @param env the environment in which the test is executed. You should not
#' need to set this.
#' @returns NULL
setup_custom_function <- function(custom_function, env = parent.frame()) {
  original_name <- deparse(substitute(custom_function))
  .GlobalEnv[[original_name]] <- custom_function 
  withr::defer(rm(list = original_name, envir = .GlobalEnv), env = env)
}
