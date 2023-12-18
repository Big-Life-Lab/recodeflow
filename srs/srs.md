# recodeflow Software Requirements Specification

## Goals

- To provide a reproducible, transparent, and streamlined process to
  recode and transform data, especially in the context of preparing it
  for analysis in healthcare research.

## Who is this product for?

- People who code in R and want to harmonize health survey results and
  health care administrative data

  - Analysts, with various levels of programming expertise, with the
    Doug Manuel or Peter Tanuseputro Lab

- Could be extended to any user who is interested in working in a
  reproducible manner since the software is technically data
  set-agnostic

## rec_with_table

- Re-code data by directly calling this main function (described in
  detail below)

- Can be used with any variables/details sheet, including those
  contained in “flow” packages

### Arguments

*The function should take the following arguments:*

Required:

- `data`: Data to be re-coded (data frame or list of data frames)

- `variables`: Name(s) of variable(s) to which to re-code (character
  vector or data frame, if supplying a variables sheet)

- `variable_details`: Content of the details sheet containing the
  re-coding instructions (data frame)

Optional:

- `database_name`: Name(s) of the database(s) to re-code (character)
- `else_value`: Value used to replace “else” (character, integer,
  complex, logical)
- `append_to_data`: Flag to indicate whether to append the re-coded data
  to the original data (logical)
- `log`: Flag to indicate whether to print log messages (logical)
- `notes`: Flag to indicate whether to print the contents of the `notes`
  column of the details sheet
- `var_labels`: Value used to populate `label` column of the details
  sheet (named vector)
- `custom_function_path`: Path to the source file that contains function
  definitions for derived variables (character)
- `attach_data_name`: Flag to indicate whether to create a new column in
  the re-coded data and populate it with the value of `database_name`
  (logical)
- `id_role_name`: Argument passed to helper function to create an ID for
  each row (?)
- `name_of_environment_to_load`: Name of package to load the variables
  and details sheets from (character)
- `append_non_db_columns`: Flag to indicate whether to include variables
  in the details sheet where `databaseStart == database_name`, but was
  not re-coded, in the re-coded data.; these columns will be filled in
  with NA (logical)
- `tables`: Name(s) of reference table(s) to use in re-coding (list)

### Feature 1: re-coding a non-derived variable

#### *Feature 1.1: A value to a type of NA*

Subset of sample details sheet (from cchsflow):

      variable recEnd recStart
    1  CCC_105      1        1
    2  CCC_105      2        2
    3  CCC_105  NA::a        6
    4  CCC_105  NA::b    [7,9]
    5  CCC_105  NA::b     else

Sample data (before re-coding):

      CCC_120
    1       6
    2       6
    3       6
    4       6
    5       6
    6       6

Sample data (after re-coding):

      CCC_120
    1      NA
    2      NA
    3      NA
    4      NA
    5      NA
    6      NA

- **When** the user wants to re-code a non-derived variable (CCC_105)
- **And** one of the re-coding rules for the variable is to re-code a
  number to a missing value
- **Then** each observation in the original data that matches the value
  in `recStart` (6) should be re-coded as the corresponding value in
  `recEnd` (a tagged missing value) in the new data

#### *Feature 1.2.1: A closed interval to a type of NA*

Subset of sample details sheet:

      variable recEnd recStart
    1   ADL_01      1    [3,4]
    2   ADL_01      2    [1,2]
    3   ADL_01  NA::a        6
    4   ADL_01  NA::b    [7,9]
    5   ADL_01  NA::b     else

Sample data (before re-coding):

    # A tibble: 6 × 1
      ADL_005
        <dbl>
    1       7
    2       7
    3       8
    4       8
    5       9
    6       9

Sample data (after re-coding):

    # A tibble: 6 × 1
      ADL_005
        <dbl>
    1      NA
    2      NA
    3      NA
    4      NA
    5      NA
    6      NA

- **When** the user wants to re-code a non-derived variable (ADL_01)
- **And** one of the re-coding rules for the variable is to re-code a
  closed interval to a missing value
- **Then** each observation in the original data that is contained in
  the interval in `recStart` (7 ≤ x ≤ 9) should be re-coded as the
  corresponding value in `recEnd` (a tagged missing value) in the new
  data

#### *Feature 1.2.2 An open interval to a type of NA*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code an
  open interval to a missing value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as the
  corresponding value in `recEnd` (a tagged missing value) in the new
  data

#### *Feature 1.2.3 A left-closed, right-open interval to a type of NA*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code a
  left-closed, right-open interval to a missing value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a ≤ x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as the
  corresponding value in `recEnd` (a tagged missing value) in the new
  data

#### *Feature 1.2.4 A left-open, right-closed interval to a type of NA*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code a
  left-open, right-closed interval to a missing value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as the
  corresponding value in `recEnd` (a tagged missing value) in the new
  data

#### *Feature 1.3: An “else” to a type of NA*

Subset of sample details sheet:

      variable recEnd recStart
    1   ADL_01      1    [3,4]
    2   ADL_01      2    [1,2]
    3   ADL_01  NA::a        6
    4   ADL_01  NA::b    [7,9]
    5   ADL_01  NA::b     else

Sample data (before re-coding):

    # A tibble: 5 × 1
      ADL_005
        <int>
    1       8
    2       7
    3       8
    4       9
    5       9

Sample data (after re-coding):

    # A tibble: 5 × 1
      ADL_005
        <dbl>
    1      NA
    2      NA
    3      NA
    4      NA
    5      NA

- **When** the user wants to re-code a non-derived variable (ADL_01)
- **And** one of the re-coding rules for the variable is to re-code
  “else” to a missing value
- **Then** each observation in the original data that has not already
  been captured by another re-coding rule should be re-coded as the
  corresponding value in `recEnd` (a tagged missing value) in the new
  data

#### *Feature 1.4: A value to another value*

Subset of sample details sheet:

          variable recEnd recStart
    1  SLPG01_cont    1.5        1
    2  SLPG01_cont    3.5        2
    3  SLPG01_cont    4.5        3
    4  SLPG01_cont    5.5        4
    5  SLPG01_cont    6.5        5
    6  SLPG01_cont    7.5        6
    7  SLPG01_cont    8.5        7
    8  SLPG01_cont    9.5        8
    9  SLPG01_cont   10.5        9
    10 SLPG01_cont   11.5       10
    11 SLPG01_cont     13       11
    12 SLPG01_cont  NA::a       96
    13 SLPG01_cont  NA::b  [97,99]
    14 SLPG01_cont  NA::b     else

Sample data (before re-coding):

      SLPG005
    1       2
    2       3
    3       3
    4       3
    5       3
    6       3

Sample data (after re-coding):

      SLPG005
    1     3.5
    2     4.5
    3     4.5
    4     4.5
    5     4.5
    6     4.5

- **When** the user wants to re-code a non-derived variable
  (SLPG01_cont)
- **And** one of the re-coding rules for the variable is to re-code a
  number to another number
- **Then** each observation in the original data that matches the value
  in `recStart` (e.g., 2) should be re-coded as the corresponding value
  in `recEnd` (e.g., 3.5) in the new data

#### *Feature 1.5.1: From a closed interval to a value*

Subset of sample details sheet:

       variable recEnd recStart
    1 DHHGAGE_5      1    [1,3]
    2 DHHGAGE_5      2    [4,7]
    3 DHHGAGE_5      3   [8,11]
    4 DHHGAGE_5      4  [12,15]
    5 DHHGAGE_5      5       16
    6 DHHGAGE_5  NA::a       96
    7 DHHGAGE_5  NA::b  [97,99]
    8 DHHGAGE_5  NA::b     else

Sample data (before re-coding):

    # A tibble: 6 × 1
      DHHGAGE
        <dbl>
    1      11
    2      12
    3      13
    4      14
    5      15
    6      16

Sample data (after re-coding):

    # A tibble: 6 × 1
      DHHGAGE
        <dbl>
    1       3
    2       4
    3       4
    4       4
    5       4
    6       5

- **When** the user wants to re-code a non-derived variable (DHHGAGE_5)
- **And** one of the re-coding rules for the variable is to re-code a
  closed interval to a value
- **Then** each observation in the original data that is contained in
  the interval in `recStart` (e.g., 4 ≤ x ≤ 7) should be re-coded as the
  corresponding value in `recEnd` (e.g., 2) in the new data

#### *Feature 1.5.2: An open interval to a value*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code an
  open interval to a value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as the
  corresponding value in `recEnd` in the new data

#### *Feature 1.5.3: A left-closed, right-open interval to a value*

Subset of sample details sheet:

        variable recEnd  recStart
    1  CCC_102_A      1    [0,12)
    2  CCC_102_A      2   [12,15)
    3  CCC_102_A      3   [15,20)
    4  CCC_102_A      4   [20,25)
    5  CCC_102_A      5   [25,30)
    6  CCC_102_A      6   [30,35)
    7  CCC_102_A      7   [35,40)
    8  CCC_102_A      8   [40,45)
    9  CCC_102_A      9   [45,50)
    10 CCC_102_A     10   [50,55)
    11 CCC_102_A     11   [55,60)
    12 CCC_102_A     12   [60,65)
    13 CCC_102_A     13   [65,70)
    14 CCC_102_A     14   [70,75)
    15 CCC_102_A     15   [75,80)
    16 CCC_102_A     16   [80,96)
    17 CCC_102_A  NA::a       996
    18 CCC_102_A  NA::b [997,999]
    19 CCC_102_A  NA::b      else

Sample data (before re-coding):

      CCC_102
    1      58
    2      56
    3      77
    4      35
    5      69
    6      60

Sample data (after re-coding):

      CCC_102 case_when(...)
    1      58             11
    2      56             11
    3      77             15
    4      35              7
    5      69             13
    6      60             12

- **When** the user wants to re-code a non-derived variable (CCC_102_A)

- **And** one of the re-coding rules for the variable is to re-code a
  left-closed, right-open interval to a value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (e.g., 35 ≤ x \< 40) should be re-coded as
  the corresponding value in `recEnd` (e.g., 7) in the new data

#### *Feature 1.5.4: A left-open, right-closed interval to a value*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code a
  left-open, right-closed interval to a value

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x ≤ b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as the
  corresponding value in `recEnd` in the new data

#### *Feature 1.6: An “else” to value*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to re-code
  “else” to a value

- **Then** each observation in the original data that has not already
  been captured by another re-coding rule should be re-coded as the
  corresponding value in `recEnd` in the new data

#### *Feature 1.7: A value to “copy”*

Subset of sample details sheet:

      variable recEnd  recStart
    1   FVC_6D  NA::a   [1,120]
    2   FVC_6D   copy       996
    3   FVC_6D  NA::b [997,999]
    4   FVC_6D  NA::b      else

Sample data (before re-coding):

      FVC_6D
    1    996
    2    999
    3    996
    4    996
    5    996
    6    996

Sample data (after re-coding):

      FVC_6D
    1    996
    2    999
    3    996
    4    996
    5    996
    6    996

- **When** the user wants to re-code a non-derived variable (FVC_6C)
- **And** one of the re-coding rules for the variable is to “copy” a
  value
- **Then** each observation in the original data that matches the value
  in `recStart` (996) should be re-coded as itself in the new data

#### *Feature 1.8.1: A closed interval to “copy”*

Subset of sample details sheet:

      variable recEnd recStart
    1 PAYDVDYS   copy    [0,7]
    2 PAYDVDYS  NA::a       96
    3 PAYDVDYS  NA::b  [97,99]
    4 PAYDVDYS  NA::b     else

Sample data (before re-coding):

    # A tibble: 6 × 1
      PAYDVDYS
         <dbl>
    1        0
    2        2
    3        3
    4        4
    5        6
    6        7

Sample data (after re-coding):

    # A tibble: 6 × 1
      PAYDVDYS
         <dbl>
    1        0
    2        2
    3        3
    4        4
    5        6
    6        7

- **When** the user wants to re-code a non-derived variable (PAYDVDYS)
- **And** one of the re-coding rules for the variable is to “copy”
  values that fall in an interval
- **Then** each observation in the original data that is contained in
  the interval in `recStart` (0 ≤ x ≤ 7) should be re-coded as itself in
  the new data

#### *Feature 1.8.2: An open interval to “copy”*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to “copy”
  values that fall in an open interval

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as itself in the
  new data

#### *Feature 1.8.3: A left-closed, right-open interval to “copy”*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to “copy”
  values that fall in a left-closed, right-open interval

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a ≤ x \< b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as itself in the
  new data

#### *Feature 1.8.4: A left-open, right-closed interval to “copy”*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to “copy”
  values that fall in a left-open, right-closed interval

- **Then** each observation in the original data that is contained in
  the interval in `recStart` (a \< x ≤ b, where a and b are the lower
  and upper bounds, respectively) should be re-coded as itself in the
  new data

#### *Feature 1.9: An “else” to “copy”*

- This re-code pair was not observed in the sample details sheet
  provided in the cchsflow package, but a user may encounter this
  combination

- **When** the user wants to re-code a non-derived variable

- **And** one of the re-coding rules for the variable is to “copy”
  “else”

- **Then** each observation in the original data that has not already
  been captured by another re-coding rule should be re-coded as itself
  in the new data

### Feature 2: re-coding a derived variable

- TBA

### Feature 3: re-coding a non-derived variable based on a derived variable

- TBA

### Feature 4: re-coding a derived-variable based on a derived variable

- TBA

### Feature 5: re-coding a variable with shared factor levels

- TBA

### Behavior

- In addition to specifying the variable(s) to which to re-code, the
  user passes the raw data, the database of origin, and details sheet to
  the function

- When re-coding multiple variables:

  - The order in which variables that do not rely on derived variables
    for re-coding are handled **does not** matter; therefore, these
    variables can be re-coded in **parallel**

  - The order in which variables that rely on derived variables for
    re-coding are handled **does** matter; therefore, these variables
    must be re-coded in **series** (order can be determined with the
    help of a dependency graph)

- For each variable, validate input data

  - Check that the database and variable names are present in the
    details sheet (otherwise, the re-coding instructions are missing)

  - Check that the details sheet has been filled out in a consistent
    manner (e.g., units are the same)

- Re-code according to instructions laid out in details sheet

  - If an error is detected, “exit gracefully”

- Return re-coded data; optionally, print out log messages indicating
  which variables were transformed successfully and which ones were not
