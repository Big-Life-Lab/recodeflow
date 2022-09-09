derived_var <- function(variable_one, table_one) {
  return(table_one[table_one$a == variable_one, ]$b)
}
