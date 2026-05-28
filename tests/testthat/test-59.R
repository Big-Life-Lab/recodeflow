library(devtools)

load_all('.')

data <- data.frame(
    start_a = c(1)
)

db <- "database_one"

variables <- data.frame(
    variable = c("a", "b", "c", "d"),
    label = c("", "", "", ""),
    labelLong = c("", "", "", ""),
    units = c("N/A", "N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Continuous", "Continuous")
)

variable_details <- data.frame(
    variable = c("a", "b", "c", "d"),
    typeEnd = c("cont", "cont", "cont", "cont"),
    databaseStart = c(db, db, db, db),
    variableStart = c("[start_a]", "DerivedVar::[a]", "DerivedVar::[b]", "[c]"),
    recEnd = c("copy", "Func::b", "Func::c", "copy"),
    recStart = c("else", "else", "else", "else")
)

.GlobalEnv$b <- function(x) {
    return(x+1)
}

.GlobalEnv$c <- function(x) {
    return(x+1)
}

recoded_data <- rec_with_table(
    data, variables, db, variable_details)
print(recoded_data)
