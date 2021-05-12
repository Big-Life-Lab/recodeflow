# Testing converting a categorical start variable to a continuous variable

- Tested by the rows with start variable `DHHAGAGE` and end variable `DHHGAGE_cont`

# Testing converting a continuous start variable to a categorical variable

- Tested by the rows with start variable `RACA_6A` and end variable `ADL_01`

# Testing converting continuous start variable to continuous start variable

- Tested by the rows with start variable `ALCA_5A1` and end variable `ALW_2A1`

# Testing the converting interval notation

## Closed-closed interval

- Row for variable `ALW_2A1` with recTo value 1

## Closed-open interval

- Row for variable `ALW_2A1` with recTo value 2

## Open-open interval

- Row for variable `ALW_2A1` with recTo value 3

## Open-closed interval

- Row for variable `ALW_2A1` with recTo value 4

# Test the conversion when there is only a default var in the variableStart column

- Row for variable `DHHGAGE_cont`

# Test the conversion when there is only variable with a database in the variableStart column

- Row for variable `variable_four`

# Test a variable which is a DerivedVar

- Test adding a DerivedVar to the XML document
  - Rows for `variable_five`. There are 4 rows, however only the first two will be added to the database since the last two are for a different start database.
  - `variable_six` is also part of this test since `variable_five` depends on it but its not part of the variables argument. This tests whether the function automatically adds variables it depends on but are not part of the variable argument
- Test adding a DerivedVar when the custom function for it has not been added to the document
  - Row for variable `variable_seven`

# Test adding a variable whose start variable is a variable in the variable column

- Rows for `variable_eight` and `variable_nine`

# Test conversion when recFrom column has strings

- Rows for `variable_ten`

# Test conversion when recFrom column is else

- Test conversion when `recTo` column is copy
  - Rows for `variable_eleven`
