test_that("strip_prefix removes the prefix", {
  expect_equal(strip_prefix("x"), "x")
  expect_equal(strip_prefix("p::x"), "x")
  expect_equal(strip_prefix("p::[x, y]"), "[x, y]")
})

test_that("strip_brackets removes the brackets", {
  expect_equal(strip_brackets("x"), "x")
  expect_equal(strip_brackets("[x]"), "x")
  expect_equal(strip_brackets("[x, y]"), "x, y")
})

test_that("When the start variable is a derived variable, it should correctly recode", {
  data <- data.frame(
    start_variable_one = c(1)
  )
  variables <- data.frame(
    variable = c("non_derived_variable_one", "derived_variable", "non_derived_variable_two"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[non_derived_variable_one]", "derived_variable")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("non_derived_variable_one", "derived_variable", "non_derived_variable_two"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[non_derived_variable_one]", "DerivedVar::[derived_variable]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "Func::derived_variable", "copy"),
    numValidCategories = c(1,"N/A",1),
    recStart = c("else", "N/A", "else"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )

  derived_variable <- function(non_derived_variable_one) {
    return(non_derived_variable_one)
  }
  setup_custom_function(derived_variable)

  actual_output <- rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name
  )

  expected_output <- data.frame(
    non_derived_variable_one = c(1),
    derived_variable = c(1),
    non_derived_variable_two = c(1)
  )
  attr(expected_output$non_derived_variable_one, "unit") <- "N/A"
  attr(expected_output$non_derived_variable_one, "label_long") <- ""
  attr(expected_output$non_derived_variable_two, "unit") <- "N/A"
  attr(expected_output$non_derived_variable_two, "label_long") <- ""

  expect_equal(actual_output, expected_output)
})

test_that("When the start variable is a derived variable, it should Should correctly recode when the derived variable is categorical", {
  variables <- data.frame(
    variable = c("variable_one", "derived_variable_one", "variable_two"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Categorical", "Continuous"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]", "DerivedVar::[derived_variable_one]")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("variable_one", "derived_variable_one", "variable_two", "variable_two"),
    typeEnd = c("cont", "cat", "cont", "cont"),
    databaseStart = c("database_one", "database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]", "DerivedVar::[derived_variable_one]", "DerivedVar::[derived_variable_one]"),
    typeStart = c("cont", "cont", "cat", "cat"),
    recEnd = c("copy", "Func::derived_variable", "2", "copy"),
    numValidCategories = c("N/A", 1, "N/A", "N/A"),
    recStart = c("else", "N/A", "1", "else"),
    catLabel = c("", "", "", ""),
    catLabelLong = c("", "", "", "")
  )

  derived_variable <- function(non_derived_variable_one) {
    return(1)
  }
  setup_custom_function(derived_variable)

  data <- data.frame(
    start_variable_one = c(1)
  )

  expected_output <- data.frame(
    variable_one = c(1),
    derived_variable_one = c(1),
    variable_two = c(2)
  )
  expected_output$derived_variable_one <- as.factor(expected_output$derived_variable_one)
  attr(expected_output$variable_one, "unit") <- "N/A"
  attr(expected_output$variable_one, "label_long") <- ""
  attr(expected_output$variable_two, "unit") <- "N/A"
  attr(expected_output$variable_two, "label_long") <- ""

  actual_output <- rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name,
    tables = list()
  )

  expect_equal(actual_output, expected_output)
})

test_that("Tables work with custom functions", {
  data <- data.frame(
    start_var = c(1)
  )
  variables <- data.frame(
    variable = c("derived_variable_one", "derived_variable_two"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_var]", "DerivedVar::[derived_variable_one, tables::table_one, tables::table_two]")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("derived_variable_one", "derived_variable_two"),
    typeEnd = c("cont", "cont"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_var]", "DerivedVar::[derived_variable_one, tables::table_one, tables::table_two]"),
    typeStart = c("N/A", "N/A"),
    recEnd = c("copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A"),
    recStart = c("else", "N/A"),
    catLabel = c("", ""),
    catLabelLong = c("", "")
  )
  tables <- list(
    table_one = data.frame(
      derived_variable_one = c(1),
      derived_variable_two = c(2)
    ),
    table_two = data.frame()
  )

  func_1 <- function(derived_variable_one, table_one, table_two) {
    return(table_one[table_one$derived_variable_one == derived_variable_one,]$derived_variable_two)
  }
  setup_custom_function(func_1)

  actual_output <- rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = database_name,
    tables = tables
  )

  expected_output <- data.frame(
    derived_variable_one = c(1),
    derived_variable_two = c(2)
  )
  attr(expected_output$derived_variable_one, "unit") <- "N/A"
  attr(expected_output$derived_variable_one, "label_long") <- ""

  expect_equal(actual_output, expected_output)
})

test_that("Recode correctly when the start variable for a database is a derived variable", {
  variables <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_one"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Cotinuous"),
    databaseStart = c("database_one", "database_two", "database_one;database_two"),
    variableStart = c("[start_variable_one]","[start_variable_two]", "database_one::DerivedVar::[variable_one], [DerivedVar::[variable_two]]")
  )
  variable_details <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_one"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_two", "database_one"),
    variableStart = c("[start_variable_one]","[start_variable_two]", "database_one::DerivedVar::[variable_one], [DerivedVar::[variable_two]]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A", "N/A"),
    recStart = c("else", "else", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  data <- data.frame(
    start_variable_one = c(1)
  )
  database_name <- "database_one"
  tables <- list()

  func_1 <- function(variable) {
    return(variable)
  }
  setup_custom_function(func_1)

  actual_output <- rec_with_table(
    data = data,
    variables = c("variable_one", "derived_variable_one"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )

  expected_output <- data.frame(
    start_variable_one = c(1),
    variable_one = c(1),
    derived_variable_one = c(1)
  )
  attr(expected_output$variable_one, "unit") <- character(0)
  attr(expected_output$variable_one, "label_long") <- NA_character_

  expect_equal(actual_output, expected_output)
})

test_that("Recode correctly when the start variable for a database is the default derived variable", {
  variables <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_one"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Continuous"),
    databaseStart = c("database_one", "database_two", "database_one;database_two"),
    variableStart = c("[start_variable_one]","[start_variable_two]", "database_one::DerivedVar::[variable_one], [DerivedVar::[variable_two]]")
  )
  variable_details <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_one"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_two", "database_one;database_two"),
    variableStart = c("[start_variable_one]","[start_variable_two]", "database_one::DerivedVar::[variable_one], [DerivedVar::[variable_two]]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A", "N/A"),
    recStart = c("else", "else", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "","")
  )
  data <- data.frame(
    start_variable_two = c(1)
  )
  database_name <- "database_two"
  tables <- list()

  func_1 <- function(variable_one) {
    return(1)
  }
  setup_custom_function(func_1)

  actual_output <- rec_with_table(
    data = data,
    variables = c("variable_two", "derived_variable_one"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )

  expected_output <- data.frame(
    start_variable_two = c(1),
    variable_two = c(1),
    derived_variable_one = c(1)
  )
  attr(expected_output$variable_two, "unit") <- character(0)
  attr(expected_output$variable_two, "label_long") <- NA_character_

  expect_equal(actual_output, expected_output)
})

test_that("Correctly recodes when the start variable has only one derived var", {
  variables <- data.frame(
    variable = c("variable_one", "derived_variable_one"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Continuous", "Continuous"),
    databaseStart = c("database_one;database_two", "database_one;database_two"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]")
  )
  variable_details <- data.frame(
    variable = c("variable_one", "derived_variable_one"),
    typeEnd = c("cont", "cont"),
    databaseStart = c("database_one;database_two", "database_one;database_two"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]"),
    typeStart = c("cont", "cont"),
    recEnd = c("copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A"),
    recStart = c("else", "N/A"),
    catLabel = c("", ""),
    catLabelLong = c("", "")
  )
  data <- data.frame(
    start_variable_one = c(1)
  )
  database_name <- "database_one"
  tables <- list()

  func_1 <- function(variable_one) {
    return(1)
  }
  setup_custom_function(func_1)

  actual_output <- rec_with_table(
    data = data,
    variables = c("variable_one", "derived_variable_one"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )

  expected_output <- data.frame(
    start_variable_one = c(1),
    variable_one = c(1),
    derived_variable_one = c(1)
  )
  attr(expected_output$variable_one, "unit") <- character(0)
  attr(expected_output$variable_one, "label_long") <- NA_character_

  expect_equal(actual_output, expected_output)
})

test_that("Should pass in the function arguments one at a time", {
  variables <- data.frame(
    variable = c("variable_one", "derived_variable"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]")
  )
  variable_details <- data.frame(
    variable = c("variable_one", "derived_variable"),
    typeEnd = c("cont", "cont"),
    databaseStart = c("database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[variable_one]"),
    typeStart = c("cont", "cont"),
    recEnd = c("copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A"),
    recStart = c("else", "N/A"),
    catLabel = c("", ""),
    catLabelLong = c("", "")
  )
  data <- data.frame(
    start_variable_one = c(1, 2)
  )
  database_name <- "database_one"
  tables <- list()

  func_1 <- function(variable_one) {
    if(length(variable_one) > 1) {
      return(2)
    }
    return(1)
  }
  setup_custom_function(func_1)

  expected_output <- data.frame(
    start_variable_one = c(1, 2),
    variable_one = c(1, 2),
    derived_variable = c(1, 1)
  )
  attr(expected_output$variable_one, "unit") <- character(0)
  attr(expected_output$variable_one, "label_long") <- NA_character_

  actual_output <- rec_with_table(
    data = data,
    variables = c("variable_one", "derived_variable"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )
  expect_equal(actual_output, expected_output)
})

test_that("When a variable has a start variable that is not in the variables argument but is in the data, it should continue to recode the variable", {
  data <- data.frame(
    start_variable_one = c(1),
    variable_one = c(2)
  )
  variables <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_two"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Cotinuous"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]","[variable_one]", "DerivedVar::[variable_one]")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("variable_one", "variable_two", "derived_variable_two"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]","[variable_one]", "DerivedVar::[variable_one]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "copy", "Func::func_1"),
    numValidCategories = c("N/A", "N/A", "N/A"),
    recStart = c("else", "else", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  tables <- list()

  func_1 <- function(variable_one) {
    return(variable_one)
  }
  setup_custom_function(func_1)

  actual_output <- recodeflow::rec_with_table(
    data = data,
    variables = c("variable_two", "derived_variable_two"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )

  expected_output <- data.frame(
    start_variable_one = c(1),
    variable_one = c(2),
    variable_two = c(2),
    derived_variable_two = c(2)
  )
  attr(expected_output$variable_two, "unit") <- character(0)
  attr(expected_output$variable_two, "label_long") <- NA_character_

  expect_equal(actual_output, expected_output)
})

test_that("Correctly recodes derived variable that depends on a derived variable even when the variables argument and variables sheet have them in the wrong order i.e. the dependant variables comes before", {
  variables <- data.frame(
    variable = c("variable_one", "derived_variable_two", "derived_variable_one"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "N/A"),
    variableType = c("Continuous", "Continuous", "Continuous"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[derived_variable_one]", "DerivedVar::[variable_one]")
  )
  variable_details <- data.frame(
    variable = c("variable_one", "derived_variable_two", "derived_variable_one"),
    typeEnd = c("cont", "cont", "cont"),
    databaseStart = c("database_one", "database_one", "database_one"),
    variableStart = c("[start_variable_one]", "DerivedVar::[derived_variable_one]", "DerivedVar::[variable_one]"),
    typeStart = c("cont", "cont", "cont"),
    recEnd = c("copy", "Func::func_2", "Func::func_1"),
    numValidCategories = c("N/A", "N/A", "N/A"),
    recStart = c("else", "N/A", "N/A"),
    catLabel = c("", "", ""),
    catLabelLong = c("", "", "")
  )
  data <- data.frame(
    start_variable_one = c(1)
  )
  database_name <- "database_one"
  tables <- list()

  func_1 <- function(variable_one) {
    return(1)
  }
  setup_custom_function(func_1)
  func_2 <- function(variable_one) {
    return(2)
  }
  setup_custom_function(func_2)

  actual_output <- recodeflow::rec_with_table(
    data = data,
    variables = c("variable_one", "derived_variable_two", "derived_variable_one"),
    variable_details = variable_details,
    database_name = database_name,
    tables = tables,
    append_to_data = TRUE
  )

  expected_output <- data.frame(
    start_variable_one = c(1),
    variable_one = c(1),
    derived_variable_one = c(1),
    derived_variable_two = c(2)
  )
  attr(expected_output$variable_one, "unit") <- character(0)
  attr(expected_output$variable_one, "label_long") <- NA_character_

  expect_equal(actual_output, expected_output)
})

test_that("The function should not error when a tibble is passed in", {
  data <- tibble::as_tibble(data.frame(
    a = c(1)
  ))
  variables <- data.frame(
    variable = c("b"),
    label = c(""),
    labelLong = c(""),
    units = c("N/A"),
    variableType = c("Continuous"),
    databaseStart = c("database_one"),
    variableStart = c("[a]")
  )
  database_name <- "database_one"
  variable_details <- data.frame(
    variable = c("b"),
    typeEnd = c("cont"),
    databaseStart = c("database_one"),
    variableStart = c("[a]"),
    typeStart = c("cont"),
    recEnd = c("copy"),
    numValidCategories = c("N/A"),
    recStart = c("else"),
    catLabel = c(""),
    catLabelLong = c("")
  )
  recoded_data <- rec_with_table(
    data, variables, database_name, variable_details)
  expected_data <- tibble::as_tibble(data.frame(b = c(1)))
  attr(expected_data$b, "label_long") <- ""
  attr(expected_data$b, "unit") <- "N/A"
  expect_equal(recoded_data, expected_data)
})

test_that(
  "derived variables work with constant numeric values as a dependency",
  {
    variables <- data.frame(
      variable = c(
        "num_hours_jogging",
        "num_hours_basketball",
        "mets_jogging",
        "mets_basketball"
      ),
      label = c("", "", "", ""),
      labelLong = c("", "", "", ""),
      units = c("N/A", "N/A", "N/A", "N/A"),
      variableType = c("Continuous", "Continuous", "Continuous", "Continuous"),
      databaseStart = c("db_one", "db_one", "db_one", "db_one"),
      variableStart = c(
        "[num_hours_jogging]",
        "[num_hours_basketball]",
        "DerivedVar::[num_hours_jogging, 7]",
        "DerivedVar::[num_hours_basketball, 6.5]"
      )
    )
    variable_details <- data.frame(
      variable = c(
        "num_hours_jogging",
        "num_hours_basketball",
        "mets_jogging",
        "mets_basketball"
      ),
      typeEnd = c("cont", "cont", "cont", "cont"),
      databaseStart = c("db_one", "db_one", "db_one", "db_one"),
      variableStart = c(
        "[num_hours_jogging]",
        "[num_hours_basketball]",
        "DerivedVar::[num_hours_jogging, 7]",
        "DerivedVar::[num_hours_basketball, 6.5]"
      ),
      typeStart = c("cont", "cont", "cont", "cont"),
      recEnd = c("copy", "copy", "Func::get_mets", "Func::get_mets"),
      numValidCategories = c("N/A", "N/A", "N/A", "N/A"),
      recStart = c("else", "else", "N/A", "N/A"),
      catLabel = c("", "", "", ""),
      catLabelLong = c("", "", "", "")
    )
    data <- data.frame(
      num_hours_jogging = c(2),
      num_hours_basketball = c(5)
    )
    database_name <- "db_one"
    tables <- list()
    get_mets <- function(num_hours_activity, activity_met_value) {
      return(num_hours_activity * activity_met_value)
    }
    setup_custom_function(get_mets)

    actual_output <- recodeflow::rec_with_table(
      data = data,
      variables = variables$variable,
      variable_details = variable_details,
      database_name = database_name,
      tables = tables
    )

    expected_output <- data.frame(
      num_hours_jogging = c(2),
      num_hours_basketball = c(5),
      mets_jogging = c(14),
      mets_basketball = c(32.5)
    )
    attr(expected_output$num_hours_jogging, "label_long") <- NA_character_
    attr(expected_output$num_hours_jogging, "unit") <- character(0)
    attr(expected_output$num_hours_basketball, "label_long") <- NA_character_
    attr(expected_output$num_hours_basketball, "unit") <- character(0)

    expect_equal(actual_output, expected_output)
  }
)

test_that(
  "derived variables work with constant string values as a dependency",
  {
    variables <- data.frame(
      variable = c(
        "num_hours_jogging",
        "num_hours_basketball",
        "mets_jogging",
        "mets_basketball"
      ),
      label = c("", "", "", ""),
      labelLong = c("", "", "", ""),
      units = c("N/A", "N/A", "N/A", "N/A"),
      variableType = c("Continuous", "Continuous", "Continuous", "Continuous"),
      databaseStart = c("db_one", "db_one", "db_one", "db_one"),
      variableStart = c(
        "[num_hours_jogging]",
        "[num_hours_basketball]",
        "DerivedVar::[num_hours_jogging, 'jogging', tables::mets_map]",
        "DerivedVar::[num_hours_basketball, \"basketball\", tables::mets_map]"
      )
    )
    variable_details <- data.frame(
      variable = c(
        "num_hours_jogging",
        "num_hours_basketball",
        "mets_jogging",
        "mets_basketball"
      ),
      typeEnd = c("cont", "cont", "cont", "cont"),
      databaseStart = c("db_one", "db_one", "db_one", "db_one"),
      variableStart = c(
        "[num_hours_jogging]",
        "[num_hours_basketball]",
        "DerivedVar::[num_hours_jogging, 'jogging', tables::mets_map]",
        "DerivedVar::[num_hours_basketball, \"basketball\", tables::mets_map]"
      ),
      typeStart = c("cont", "cont", "cont", "cont"),
      recEnd = c("copy", "copy", "Func::get_mets", "Func::get_mets"),
      numValidCategories = c("N/A", "N/A", "N/A", "N/A"),
      recStart = c("else", "else", "N/A", "N/A"),
      catLabel = c("", "", "", ""),
      catLabelLong = c("", "", "", "")
    )
    data <- data.frame(
      num_hours_jogging = c(2),
      num_hours_basketball = c(5)
    )
    database_name <- "db_one"
    tables <- list(
      mets_map = data.frame(
        activity = c("jogging", "basketball"),
        mets = c(7, 6.5)
      )
    )
    # Custom function for the derived variable
    get_mets <- function(num_hours_activity, activity_type, mets_map) {
      activity_met_value <- mets_map[mets_map$activity == activity_type, ]
      return(num_hours_activity * activity_met_value$mets)
    }
    setup_custom_function(get_mets)

    actual_output <- recodeflow::rec_with_table(
      data = data,
      variables = variables$variable,
      variable_details = variable_details,
      database_name = database_name,
      tables = tables
    )

    expected_output <- data.frame(
      num_hours_jogging = c(2),
      num_hours_basketball = c(5),
      mets_jogging = c(14),
      mets_basketball = c(32.5)
    )
    attr(expected_output$num_hours_jogging, "label_long") <- NA_character_
    attr(expected_output$num_hours_jogging, "unit") <- character(0)
    attr(expected_output$num_hours_basketball, "label_long") <- NA_character_
    attr(expected_output$num_hours_basketball, "unit") <- character(0)

    expect_equal(actual_output, expected_output)
  }
)
