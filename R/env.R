pkg.env <- new.env(parent = emptyenv())

pkg.env$NA_invalid <- 'NA::a'
pkg.env$NA_missing <- 'NA::b'
pkg.env$all_NAs <- c(pkg.env$NA_invalid, pkg.env$NA_missing)

pkg.env$missing <- 'missing'

pkg.env$db_var_start_infix <- '::'

pkg.env$rec_to_copy <- 'copy'

pkg.env$var_details_cat <- 'cat'
pkg.env$var_details_cont <- 'cont'

pkg.env$var_cat <- 'Categorical'
