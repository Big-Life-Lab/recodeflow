## code to prepare dataset goes here
library(yaml)
library(readr)
library(usethis)
library(survival)

# ------ pbc.rda data ------
# Get PBC data from survival package
data(pbc, package = "survival")

# Save both the original pbc data 
usethis::use_data(pbc, overwrite = TRUE)

# ------ import the pbc_metadata.yaml file ------
pbc_metadata <- read_yaml("inst/extdata/pbc_metadata.yaml")
usethis::use_data(pbc_metadata, overwrite = TRUE)

# ------ import the pbc_variables.csv file --------
pbc_variables <- read_csv("inst/extdata/pbc_variables.csv",
    show_col_types = FALSE,
    col_types = cols(
        variable = col_character(),
        label = col_character(),
        labelLong = col_character(),
        subject = col_character(),
        section = col_character(),
        variableType = col_character(),
        databaseStart = col_character(),
        units = col_character(),  # changed from logical
        variableStart = col_character(),
        notes = col_logical(),
        description = col_logical()
    )
)

# Check for any parsing problems
if(nrow(problems(pbc_variables)) != 0) {
    print("Problems with pbc_variables:")
    print(problems(pbc_variables))
}

usethis::use_data(pbc_variables, overwrite = TRUE)

# ------- import the pbc_variable_details.csv file ------
pbc_variable_details <- read_csv("inst/extdata/pbc_variable_details.csv",
    show_col_types = FALSE,
    col_types = cols(
        variable = col_character(),
        dummyVariable = col_character(),
        typeEnd = col_character(),
        typeStart = col_character(),
        databaseStart = col_character(),
        variableStart = col_character(),
        variableStartLabel = col_character(),
        numValidCat = col_double(),
        recEnd = col_character(),
        catLabel = col_character(),
        catLabelLong = col_character(),
        units = col_character(),
        recStart = col_character(),
        catLabelLong = col_character(),
        variableStartShortLabel = col_character(),
        notes = col_character(),
        .default = col_character()  # for any remaining columns
    )
)

# Check for any parsing problems
if(nrow(problems(pbc_variable_details)) != 0) {
    print("Problems with pbc_variable_details:")
    print(problems(pbc_variable_details))
}

usethis::use_data(pbc_variable_details, overwrite = TRUE)