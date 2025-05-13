test_that("Integration test", {
 create_study_data <- function(variables_sheet, variables_details_sheet, huiport_config) {
  harmonized_data <- NULL

  # The environment in which we will load all the data to ensure it does not
  # pollute the global environment
  data_env <- new.env()

  # SurveyCycle is manually derived since recodeflow does not support
  # the data_name argument in a derived function
  unsupported_variables <- c('SurveyCycle')
  untransformed_variables <- variables_sheet %>%
    dplyr::filter(transformationType == "N/A") %>%
    dplyr::filter(!variable %in% unsupported_variables)
  # The vector of cchs database names
  data_names <- names(huiport_config$data)
  # This for loop will populate the cchs_data variable with all the cchs
  # databases we will be using
  for(data_index in seq_along(data_names)) {
    # Get the name of the current cchs dataset
    data_name <- data_names[[data_index]]

    print(paste("Start harmonization for", data_name))
    # Load the dataset into the created `data_env` environment getting its
    # name.
    loaded_data_name <- load(huiport_config$data[[data_name]], env = data_env)

    current_harmonized_data <- rec_with_table(
      get(loaded_data_name, envir = data_env),
      variables = untransformed_variables,
      database_name = data_name,
      variable_details = variables_details_sheet,
      id_role_name = "id",
      custom_function_path = "./integration-assets/custom-functions.R",
      notes = FALSE,
      append_non_db_columns = TRUE
    )
    current_harmonized_data <- current_harmonized_data %>%
      dplyr::mutate(SurveyCycle = Vectorize(SurveyCycle.fun)(data_name))

    # If the harmonized_data has not been intialized then set to the
    # current one.
    # Otherwise row append the current one to the harmonize_data
    if (is.null(harmonized_data)) {
      harmonized_data <- current_harmonized_data
    } else {
      harmonized_data <-
        dplyr::bind_rows(harmonized_data, current_harmonized_data)
    }

    rm(list = loaded_data_name, envir = data_env)

    print(paste("Done harmonization for", data_name))
  }

  harmonized_data <- fix_na_c(harmonized_data)

  for(variable_name in colnames(harmonized_data)) {
    variables_sheet_row <- variables_sheet[variables_sheet$variable == variable_name, ]
    if(variables_sheet_row[1, "variableType"] == "Categorical") {
      harmonized_data[[variable_name]] <- as.factor(harmonized_data[[variable_name]])
    } else if(variables_sheet_row[1, "variableType"] == "Continuous") {
      harmonized_data[[variable_name]] <- as.numeric(harmonized_data[[variable_name]])
    }
  }

  return(harmonized_data)
 }
  fix_na_c <- function(data) {
    is_regular_na <- function(x) {
      return(is.na(x) & !haven::is_tagged_na(x))
    }
    data <- data %>%
      dplyr::mutate(dplyr::across(
        where(is.factor),
        ~ dplyr::case_when(
          is_regular_na(.x) ~ add_na_c_level(.x),
          TRUE ~ .x
        )
      )) %>%
      dplyr::mutate(dplyr::across(
        where(is.character),
        ~ dplyr::case_when(
          is_regular_na(.x) ~ "NA(c)",
          TRUE ~ .x
        )
      )) %>%
      dplyr::mutate(dplyr::across(
        where(is.numeric),
        ~ ifelse(is_regular_na(.x), haven::tagged_na("c"), .x)
      ))
    return(data)
  }

  add_na_c_level <- function(x) {
    x <- factor(
      x,
      levels = c(levels(x), "NA(c)")
    )
    return(tidyr::replace_na(x, "NA(c)"))
  }
 variables_sheet <- read.csv("./integration-assets/variables.csv", fileEncoding = "UTF-8-BOM")
 variable_details_sheet <- read.csv("./integration-assets/variable-details-sheet.csv", fileEncoding = "UTF-8-BOM")
 huiport_config <- list(
    "data" = list(
      "cchs2001_p"= "./integration-assets/cchs2001_p.RData",
      "cchs2003_p"= "./integration-assets/cchs2003_p.RData",
      "cchs2005_p"= "./integration-assets/cchs2005_p.RData",
      "cchs2007_2008_p"= "./integration-assets/cchs2007_2008_p.RData",
      "cchs2009_2010_p"= "./integration-assets/cchs2009_2010_p.RData",
      "cchs2012_p"= "./integration-assets/cchs2012_p.RData",
      "cchs2013_2014_p"= "./integration-assets/cchs2013_2014_p.RData"
    )
  )
  actual_data <- create_study_data(variables_sheet, variable_details_sheet, huiport_config)
})
