library(readr)
library(dplyr)

pbc_variables <- as_tibble(read_csv(
  "inst/extdata/pbc_variables.csv"
  ))

usethis::use_data(pbc_variables, overwrite = TRUE)
