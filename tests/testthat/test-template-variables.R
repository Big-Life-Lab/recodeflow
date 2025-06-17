langmap <- c(
  'English' = '1',
  'French' = '2',
  'Mandarin' = '3',
  'Hindi' = '4',
  'not applicable' = 'NA(a)',
  'missing' = 'NA(b)'
)

readfile <- function(name) {
  p <- "recodeflow"
  fe <- "UTF-8"
  return (read.csv(system.file(name, package = p), fileEncoding = fe))
}

test_that("template variables work", {
  db <- "database_one"

  variables <- data.frame(
    variable = c("primary_lang", "secondary_lang"),
    label = c("", ""),
    labelLong = c("", ""),
    units = c("N/A", "N/A"),
    variableType = c("Categorical", "Categorical"),
    databaseStart = c(db, db)
  )

  variable_details <- readfile("template-variable-variable-details.csv")

  data <- readfile("example-dataset.csv")

  actual <- recodeflow:::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = db
  )

  expected <- readfile("recoded-dataset.csv")
  expected <- subset(expected, select = -c(id))

  levels(expected$primary_lang) <- c('1','2','3','4')
  attr(expected$primary_lang, 'label_long') <- c('')
  attr(expected$primary_lang, 'labels') <- langmap
  attr(expected$primary_lang, 'labels_long') <- as.list(langmap)
  attr(expected$primary_lang, 'unit') <- 'N/A'

  expected$secondary_lang <- as.factor(expected$secondary_lang)
  attr(expected$secondary_lang, 'label_long') <- c('')
  attr(expected$secondary_lang, 'labels') <- langmap
  attr(expected$secondary_lang, 'labels_long') <- as.list(langmap)
  attr(expected$secondary_lang, 'unit') <- 'N/A'

  expect_equal(actual, expected)
})

test_that("derived template variables work", {
  db <- "database_one"
  variables <- data.frame(
    variable = c("primary_lang"),
    label = c(""),
    labelLong = c(""),
    units = c("N/A"),
    variableType = c("Categorical"),
    databaseStart = c(db),
    variableStart = c("[PL]")
  )
  variable_details <- data.frame(
    variable = c("lang", "lang", "lang", "lang", "primary_lang"),
    templateVariable = c("Yes", "Yes", "Yes", "Yes", "lang"),
    typeEnd = c("cat", "cat", "cat", "cat", "cat"),
    databaseStart = c(db, db, db, db, db),
    variableStart = c("N/A", "N/A", "N/A", "N/A", "DerivedVar::[PL]"),
    typeStart = c("cat", "cat", "cat", "cat", "cat"),
    recEnd = c("Func::enumerate_lang", "1", "2", "NA::b", "N/A"),
    numValidCat = c(2, 2, 2, 2, "N/A"),
    recStart = c("N/A", "N/A", "N/A", "N/A", "N/A"),
    catLabel = c("", "", "", "", ""),
    catLabelLong = c("", "", "", "", "")
  )

  .GlobalEnv[["enumerate_lang"]] <- function(lang) {
    if (lang == "English") {
      return("1");
    } else if (lang == "French") {
      return("2");
    } else {
      return("NA::b")
    }
  }

  data <- data.frame(
    PL = c("English", "French", "Other")
  )

  expected_data <- data.frame(
    primary_lang = factor(c(1, 2, "NA::b"), levels = )
  )

  actual_data <- recodeflow::rec_with_table(
    data,
    variables = variables,
    variable_details = variable_details,
    database_name = db
  )

  expect_equal(actual_data, expected_data)
})

test_that("template variables work together with non-template vars", {
  db <- "database_one"
  variables <- data.frame(
    variable = c("primary_lang", "secondary_lang", "age"),
    label = c("", "", ""),
    labelLong = c("", "", ""),
    units = c("N/A", "N/A", "years"),
    variableType = c("Categorical", "Categorical", "Continuous"),
    databaseStart = c(db, db, db)
  )
  variable_details <- data.frame(
    variable = c("lang", "lang", "primary_lang", "secondary_lang", "age"),
    templateVariable = c("Yes", "Yes", "lang", "lang", "No"),
    typeEnd = c("cat", "cat", "cat", "cat", "cont"),
    databaseStart = c(db, db, db, db, db),
    variableStart = c("N/A", "N/A", "[start_primary_lang]",
                      "[start_secondary_lang]", "[start_age]"),
    typeStart = c("cat", "cat", "cat", "cat", "cont"),
    recStart = c("english", "french", "N/A", "N/A", "else"),
    recEnd = c("1", "2", "N/A", "N/A", "copy"),
    numValidCat = c("2", "2", "N/A", "N/A", "N/A"),
    catLabel = c("", "", "", "", ""),
    catLabelLong = c("", "", "", "", "")
  )
  data <- data.frame(
    start_primary_lang = c("english", "french"),
    start_secondary_lang = c("french", "N/A"),
    start_age = c(20, 30)
  )

  expected <- data.frame(
    age = c(20, 30),
    primary_lang = factor(c(1, 2)),
    secondary_lang = factor(c(2, "N/A"), levels=c(2)) # FIXME: shouldn't `1` be included in levels?
  )
  attr(expected$age, "label_long") <- ""
  attr(expected$age, "unit") <- c("years")

  attr(expected$primary_lang, "label_long") <- ""
  attr(expected$primary_lang, "unit") <- c("N/A")
  attr(expected$primary_lang, "labels") <- c(1, 2)
  names(attr(expected$primary_lang, "labels")) <- c('', '')
  attr(expected$primary_lang, "labels_long") <- list('1', '2')
  names(attr(expected$primary_lang, "labels_long")) <- c('', '')

  attr(expected$secondary_lang, "label_long") <- ""
  attr(expected$secondary_lang, "unit") <- c("N/A")
  attr(expected$secondary_lang, "labels") <- c(1, 2)
  names(attr(expected$secondary_lang, "labels")) <- c('', '')
  attr(expected$secondary_lang, "labels_long") <- list('1', '2')
  names(attr(expected$secondary_lang, "labels_long")) <- c('', '')

  actual <- recodeflow:::rec_with_table(
    data = data,
    variables = variables,
    variable_details = variable_details,
    database_name = db
  )

  expect_equal(actual, expected)
})
