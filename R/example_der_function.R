#' example_der_fun caluclates chol*bili
#' @param chol the row value for chol
#' @param bili the row value for bili
#' @export
example_der_fun <- function(chol, bili){
  # as numeric is used to coerce in case categorical numeric variables are used.
  # Warning either chol or bili being NA will result in NA return
  example_der <- as.numeric(chol)*as.numeric(bili)

  return(example_der)
}
