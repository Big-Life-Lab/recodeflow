#' Extract margins from character vector.
#'
#' @param chars Character vector.
#'
#' @return Margins as character vector.
#'
#' @examples
get_margins <- function (chars) {
  return (trimws(strsplit(chars, ":")[[1]]))
}

#' Check if a character object can be converted to a number.
#'
#' @param chars Character object.
#'
#' @return Whether `chars` can be converted to a numeric value.
#'
#' @examples
is_numeric <- function(chars) {
  return (suppressWarnings(!is.na(as.numeric(chars))))
}
