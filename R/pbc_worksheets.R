library(yaml)
library(readr)
library(usethis)

# import the pbc_database.yaml file.
pbc_database <- read_yaml("inst/extdata/pbc_database.yaml")
usethis::use_data(pbc_database, overwrite = TRUE)

# import the pbc_variables.csv file.
pbc_variables <- read_csv("inst/extdata/pbc_variabes.csv")
usethis::use_data(pbc_variables, overwrite = TRUE)

# import the pbc_variable_details.csv file.
pbc_variable_details <- read_csv("inst/extdata/pbc_variable_details.csv")
usethis::use_data(pbc_variable_details, overwrite = TRUE)
