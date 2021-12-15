# Removing Note for undeclared variable. This is necessary because it is used
# in the magritr package for piping however it is not exported so cran check throws a note.
. <- NULL

#' @title Checks whether two values are equal including NA
#' @description Compared to the base "==" operator in R, this function returns true if the two values are NA
#' whereas the base "==" operator returns NA
#'
#' @param v1 variable 1
#' @param v2 variable 2
#'
#' @return boolean value of whether or not v1 and v2 are equal
#'
#' @examples
#' is_equal(1,2)
#' # FALSE
#'
#' is_equal(1,1)
#' # TRUE
#'
#' 1==NA
#' # NA
#'
#' is_equal(1,NA)
#' # FALSE
#'
#' NA==NA
#' # NA
#'
#' is_equal(NA,NA)
#' # TRUE
#' @export
is_equal <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  # anything compared to NA equals NA
  # replaces all instanses of NA with FALSE
  same[is.na(same)] <- FALSE

  return(same)
}

#' Recode with Table
#'
#' Creates new variables by recoding variables in a dataset using the rules
#' specified in a variables details sheet
#'
#' The \href{https://github.com/Big-Life-Lab/recodeflow/blob/master/inst/extdata/PBC-variableDetails.csv}{variable_details}
#'  dataframe needs the following columns:
#'  \describe{
#'   \item{variable}{Name of the new variable created. The name of the new
#'   variable can be the same as the original variable if it does not change the original variable definition}
#'   \item{typeEnd}{type the new variable
#'   \emph{cat = categorical, cont = continuous}}
#'   \item{databaseStart}{Names of the databases that the original variable can
#'   come from. Each database name should be seperated by a comma. For eg.,
#'   "cchs2001_p, cchs2003_p,cchs2005_p,cchs2007_p"}
#'   \item{variableStart}{Names of the original variables within each database
#'   specified in the databaseStart column. For eg. ,
#'   "cchs2001_p::RACA_6A,cchs2003_p::RACC_6A,ADL_01". The final variable
#'   specified is the name of the variable for all other databases specified in
#'   databaseStart but not in this column. For eg., ADL_01 would be the original
#'   variable name in the cchs2005_p and cchs2007_p databases.}
#'   \item{typeStart}{variable type of start variable.
#'   \emph{cat = categorical or factor variable}
#'   \emph{cont = continuous variable (real number or integer)}}
#'   \item{recEnd}{Value to recode to}
#'   \item{recStart}{Value/range being recoded from}
#'  }
#'  Each row in the \emph{variables details} sheet encodes the rule for recoding
#'  value(s) of the original variable to a category in the new variable. The
#'  categories of the new variable are encoded in the \emph{recTo} column and the
#'  value(s) of the original variable that recode to this new value are encoded
#'  in the \emph{recFrom} column. These recode columns follow a syntax
#'  similar to the \emph{sjmisc::rec()} function. Whereas in the \emph{sjmisc::rec()}
#'  function the recoding rules are in one string, in the variables details sheet
#'  they are encoded over multiple rows and columns (recFrom an recTo). For eg.,
#'  a recoding rule in the sjmisc function would like like "1=2;2=3" whereas
#'  in the variables details sheet this would be encoded over two rows with
#'  recFrom and recTo values of the first row being 1 and 2 and similarly for
#'  the second row it would be 2 and 3. The rules for describing recoding
#'  pairs are shown below:
#'   \describe{
#'     \item{recode pairs}{Each recode pair is a row}
#'     \item{multiple values}{Multiple values from the old variable that should
#'     be recoded into a new category of the new variable should be separated
#'     with a comma. e.g.,
#'     \emph{recStart = "1,2"; recEnd = 1}} will recode values of 1 and 2 in the
#'     original variable to 1 in the new variable
#'     \item{value range}{A value range is indicated by a colon, e.g.
#'     \emph{recStart= "1:4"; recEnd = 1} will recode all values from 1 to 4 into 1}
#'     \item{\emph{min} and \emph{max}}{minimum and maximum values
#'     are indicated by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}),
#'      e.g. \emph{recFrom = "min:4"; recTo = 1} will recode all values from the
#'      minimum value of the original variable to 4 into 1}
#'     \item{\emph{"else"}}{All other values, which have not been specified yet,
#'      are indicated by \emph{else}, e.g. \emph{recFrom = "else"; recTo = NA}
#'      will recode all other values (not specified in other rows) of the
#'      original variable to "NA")}
#'     \item{\emph{"copy"}}{the \emph{else} token can be combined with
#'     \emph{copy}, indicating that all remaining, not yet recoded values should
#'      stay the same (are copied from the original value), e.g.
#'      \emph{recStart = "else"; recEnd = "copy"}}
#'     \item{\emph{NA}'s}{\emph{NA} values are allowed both for the original
#'     and the new variable, e.g.
#'     \emph{recStart "NA"; recEnd = 1. or "recStart = "3:5"; recEnd = "NA"}
#'     (recodes all NA into 1,
#'     and all values from 3 to 5 into NA in the new variable)}
#' }
#'
#' @param data A dataframe containing the variables to be recoded.
#' Can also be a named list of dataframes.
#' @param variables Character vector containing the names of the new variables
#' to recode to or a dataframe containing a variables sheet.
#' @param database_name A String containing the name of the database containing
#' the original variables which should match up with a database from the
#' databaseStart column in the variables details sheet. Should be a character
#' vector if data is a named list where each vector item matches a name in the
#' data list and also matches with a value in the databaseStart column of a
#' variable details sheet.
#' @param variable_details A dataframe containing the specifications
#' for recoding.
#' @param else_value Value (string, number, integer, logical or NA) that is used
#' to replace any values that are outside the specified ranges
#' (no rules for recoding).
#' @param append_to_data Logical, if \code{TRUE} (default), the newly
#' created variables will be appended to the original dataset.
#' @param log Logical, if \code{FALSE} (default), a log containing information
#' about the recoding will not be printed.
#' @param notes Logical, if \code{FALSE} (default), will not print the
#' content inside the `Note`` column of the variable being recoded.
#' @param var_labels labels vector to attach to variables in variables
#' @param custom_function_path string containing the path to the file
#' containing functions to run for derived variables. This file will be sourced
#' and its functions loaded into the R environment.
#' @param attach_data_name logical to attach name of database to end table
#' @param id_role_name name for the role to be used to generate id column
#' @param name_of_environment_to_load Name of package to load variables and variable_details from
#' @param append_non_db_columns boolean determening if data not present in this cycle should be appended as NA
#'
#' @return a dataframe that is recoded according to rules in variable_details.
#' @importFrom haven tagged_na
#' @importFrom stringr str_match
#' @importFrom dplyr rowwise select do
#' @importFrom magrittr %>%
#' @examples
#' var_details <-
#'   data.frame(
#'     "variable" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
#'     "dummyVariable" = c("NA", "status0", "status1","status2", "trt1","trt2","NA","sexM","sexF", "ascites0", "ascites1","hepato0","hepato1","spiders0","spiders1","edema0.0","edema0.5","edema1.0",rep("NA",times = 9), "stage1", "stage2","stage3","stage4"),
#'     "typeEnd" = c("cont", rep("cat", times = 3), rep("cat", times = 2), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4)),
#'     "databaseStart" = rep("tester1, tester2", times = 31),
#'     "variableStart" = c("[time]", rep("[status]", times = 3), rep("[trt]", times = 2), "[age]", rep("[sex]", times = 2), rep("[ascites]", times = 2), rep("[hepato]", times = 2), rep("[spiders]", times = 2), rep("[edema]", times = 3), "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]", rep("[stage]", times = 4)),
#'     "typeStart" = c("cont", rep("cat", times = 3), rep("cat", times = 2), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4)),
#'     "recEnd" = c("copy", "0", "1","2", "1","2","copy","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("copy",times = 9), "1", "2","3","4"),
#'     "catLabel" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex m","sex f", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
#'     "catLabelLong" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex m","sex f", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
#'     "recStart" = c("else", "0", "1","2", "1","2","else","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("else",times = 9), "1", "2","3","4"),
#'     "catStartLabel" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex m","sex f", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
#'     "variableStartShortLabel" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
#'     "variableStartLabel" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
#'     "units" = rep("NA", times = 31),
#'     "notes" = rep("This is sample survival pbc data", times = 31)
#'   )
#' var_sheet <-
#'   data.frame(
#'     "variable" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
#'     "label" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
#'     "labelLong" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
#'     "section" = rep("tester", times=19),
#'     "subject" = rep("tester",times = 19),
#'     "variableType" = c("cont", "cat", "cat", "cont","cat", "cat", "cat","cat", "cat", rep("cont", times = 9), "cat"),
#'     "databaseStart" = rep("tester1, tester2", times = 19),
#'     "units" = rep("NA", times = 19),
#'     "variableStart" = c("[time]","[status]", "[trt]", "[age]", "[sex]", "[ascites]","[hepato]","[spiders]","[edema]", "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]","[stage]")
#'   )
#' library(survival)
#' tester1 <- survival::pbc[1:209,]
#' tester2 <- survival::pbc[210:418,]
#' db_name1 <- "tester1"
#' db_name2 <- "tester2"
#'
#' rec_sample1 <- rec_with_table(data = tester1,
#' variables = var_sheet,
#' variable_details = var_details,
#' database_name = db_name1)
#'
#' rec_sample2 <- rec_with_table(data = tester2,
#' variables = var_sheet,
#' variable_details = var_details,
#' database_name = db_name2)
#'
#' @export

rec_with_table <-
  function(data,
           variables = NULL,
           database_name = NULL,
           variable_details = NULL,
           else_value = NA,
           append_to_data = FALSE,
           log = FALSE,
           notes = TRUE,
           var_labels = NULL,
           custom_function_path = NULL,
           attach_data_name = FALSE,
           id_role_name = NULL,
           name_of_environment_to_load = NULL,
           append_non_db_columns = FALSE) {
    # Convert passed id_role_name to list in case its a string.
    # This makes it work with the select_vars_by_role function.
    # if (!is.list(id_role_name)) {
    #   id_role_name <- list(id_role_name)
    # }

    # If custom_function_path is passed, source it to load all the custom
    # functions in the file into the R environment
    if (!is.null(custom_function_path)) {
      source(custom_function_path)
    }

    # If the user passed the name_of_environment_to_load parameter and the
    # variables_details or variables parameters are NULL, load them from the
    # environment they passed in
    if (!is.null(name_of_environment_to_load)) {
      if (is.null(variable_details)) {
        message(
          paste0(
            "No variable_details detected.
              Loading",
            name_of_environment_to_load,
            "variable_details"
          )
        )
        data(variable_details,
             package = name_of_environment_to_load,
             envir = environment())
      }

      if (is.null(variables)) {
        message(
          paste0(
            "No variables detected.
              Loading" ,
            name_of_environment_to_load,
            "variables"
          )
        )
        data(variables, package = name_of_environment_to_load,
             envir = environment())
      }

    }
    # Otherwise if either variables or the variable_details is NULL and they did
    # not pass in an environment to load them from, throw an error
    else if (is.null(variables) || is.null(variable_details)) {
      stop(
        "No name_of_environment_to_load was passed to load variables and variable details from,
           as well as no variables or variable details was passed. Stopping execution"
      )
    }

    # If the user did not pass in a database_name parameter, then use the name
    # of the variable that holds the data parameter as database_name
    if (is.null(database_name)) {
      message("Using the passed data variable name as database_name")
      database_name <- deparse(substitute(data))
    }

    # If the passed data parameter is a list, then make sure that the
    # each data in the list has a database name in the database_name parameter
    # by checking their length
    if (class(data) == "list" &&
        length(database_name) == length(data)) {
      # Iterate through each database name and recode the corresponding
      # data
      for (data_name in database_name) {
        # Verify that the passed name exists in the passed data
        if (!is.null(data[[data_name]])) {
          data[[data_name]] <- recode_call(
            variables = variables,
            data = data[[data_name]],
            database_name = database_name,
            print_note = notes,
            else_value = else_value,
            variable_details = variable_details,
            append_to_data = append_to_data,
            append_non_db_columns = append_non_db_columns,
            log = log,
            var_labels = var_labels
          )
        } else {
          stop(
            paste(
              "The data",
              data_name,
              "is missing from the passed list please verify the names are
              correct in the data list and the database_name list"
            )
          )
        }
      }
    }
    # If the data is a dataframe then the user only wants to recode one dataset.
    # Make sure theres only one database name passed in.
    else if ("data.frame" %in% class(data) &&
               length(database_name) == 1) {
      data <- recode_call(
        variables = variables,
        data = data,
        database_name = database_name,
        print_note = notes,
        else_value = else_value,
        variable_details = variable_details,
        append_to_data = append_to_data,
        append_non_db_columns = append_non_db_columns,
        log = log,
        var_labels = var_labels
      )
      if (attach_data_name) {
        data[["data_name"]] <- database_name
      }
      if (!is.null(id_role_name)) {
        data <- create_id_row(data, id_role_name, database_name)
      }
    } else {
      stop(
        paste(
          "The passed number of data does not match the passed number of
          data_names. Please verify that the number of databases matches the number
          of passed names.
          Aborting operation!"
        ),
        call. = FALSE
      )
    }

    return(data)
  }

# Creates inputs and runs recode functions
recode_call <-
  function(variables,
           data,
           database_name,
           print_note,
           else_value,
           variable_details,
           append_to_data,
           append_non_db_columns,
           log,
           var_labels) {
    # Trim the values in the Variable column of variables details
    variable_details[[pkg.env$columns.Variable]] <-
      trimws(variable_details[[pkg.env$columns.Variable]])

    # If the variables parameter is a variables sheet then update
    # the variables details sheet based on it
    if ("data.frame" %in% class(variables)) {
      # Trim the values in the variables columns of the variables sheet
      variables[[pkg.env$columns.Variable]] <-
        trimws(variables[[pkg.env$columns.Variable]])
      variable_details <-
        update_variable_details_based_on_variable_sheet(variable_sheet = variables,
                                                        variable_details = variable_details)
    }
    # Otherwise its a vector containing the variables to recode
    else {
      # Create empty columns to later populate from variables sheet
      if (is.null(variable_details[[pkg.env$columns.VariableLabel]])) {
        variable_details[[pkg.env$columns.VariableLabel]] <- NA
      }
      if (is.null(variable_details[[pkg.env$columns.label]])) {
        variable_details[[pkg.env$columns.label]] <- NA
      }

      # The variables in the variables details sheet
      vars_being_recoded <-
        as.character(unique(variable_details[[pkg.env$columns.Variable]]))
      if (length(vars_being_recoded) != length(variables)) {
        missing_vars <- setdiff(variables, vars_being_recoded)
        warning(
          paste(
            missing_vars,
            "is missing from variable details therefore cannot be recoded"
          )
        )
      }
    }

    # Update the labels for variables in the variables details sheet with the
    # ones in the var_labels parameter if the user passed them
    if (!is.null(var_labels)) {
      # Make the var_labels is a named list
      if (is.null(names(var_labels))) {
        stop(
          "The passed labels was not a named vector please follow
          the c(var_name = varLalbel) format"
        )
      }

      # If the label column in variables details is a factor, convert it to
      # a character
      if (is.factor(variable_details[[pkg.env$columns.label]])) {
        variable_details[[pkg.env$columns.label]] <-
          as.character(variable_details[[pkg.env$columns.label]])
      }

      # For each list item in var_labels, find the rows in the variables details
      # for that variable and update its label value to the list item's value.
      for (var_name in names(var_labels)) {
        variable_details[variable_details[[pkg.env$columns.Variable]] == var_name,
                         pkg.env$columns.label] <-
          var_labels[[var_name]]
      }
    }

    # Vector containing all variable names present in variable details
    all_possible_var_names <-
      unique(as.character(variable_details[[pkg.env$columns.Variable]]))

    select_variables <- ""
    if(is.data.frame(variables)){
      select_variables <- as.character(variables[[pkg.env$columns.Variable]])
    }else{
      select_variables <- variables
    }

    # The variables details rows whose database start column has the database_name
    # parameter
    names_of_all_variables_detected <-
      variable_details[grepl(database_name, variable_details[[pkg.env$columns.DatabaseStart]])& as.character(variable_details[[pkg.env$columns.Variable]]) %in% select_variables,]

    rec_data <-
      recode_columns(
        data = data,
        variables_details_rows_to_process = names_of_all_variables_detected,
        data_name = database_name,
        log = log,
        print_note = print_note,
        else_default = else_value
      )

    # If this flag is set then,
    # Add all the variables in variables details whose database start matches
    # the database_name parameter, but was not recoded, to the recoded data
    # and set it to NA
    # For e.g., if the user wants to recode ADL_01 for cchs2001, and the
    # variables details includes rows for ADL_02 with cchs2001 as database start,
    # add it to the recoded data if this flag is set.
    if (append_non_db_columns) {
      missed_variables <-
        all_possible_var_names[!all_possible_var_names %in%
                                 unique(as.character(names_of_all_variables_detected[,
                                                                                     pkg.env$columns.Variable]))]
      for (missed_variable_name in missed_variables) {
        rec_data[[missed_variable_name]] <- NA
      }
    }

    if (append_to_data) {
      return(cbind(data, rec_data))
    }

    return(rec_data)
  }

#' @title Get Data Variable Name
#'
#' @name get_data_variable_name
#'
#' @description Retrieves the name of the column inside data to
#' use for calculations
#'
#' @param data_name name of the database being checked
#' @param data database being checked
#' @param row_being_checked the row from variable details that contains
#' information on this variable
#' @param variable_being_checked the name of the recoded variable
#'
#' @return the data equivalent of variable_being_checked
get_data_variable_name <-
  function(data_name,
           data,
           row_being_checked,
           variable_being_checked) {
    data_variable_being_checked <- character()
    var_start_names <-
      as.character(row_being_checked[[pkg.env$columns.VariableStart]])

    if (grepl(data_name, var_start_names)) {
      var_start_names_list <- as.list(strsplit(var_start_names, ",")[[1]])
      # Find exact var Name
      for (var_name in var_start_names_list) {
        if (grepl(data_name, var_name)) {
          # seperate dataname from the var name
          data_variable_being_checked <-
            as.list(strsplit(var_name, "::")[[1]])[[2]]
        }
      }
      # Check for default variable name
    } else if (grepl("\\[", var_start_names)) {
      # Strip default var name tags: []
      data_variable_being_checked <-
        stringr::str_match(var_start_names, "\\[(.*?)\\]")[, 2]
    } else {
      stop(
        paste(
          "The row
          ",
          row,
          "for the variable",
          variable_being_checked,
          "
          Does not contain the database being checked(",
          data_name,
          ") in its variable start the default is also missing.
          Please double check if this variable should have this",
          data_name,
          "included in its databaseStart"
        )
      )
    }
    data_variable_being_checked <-
      trimws(data_variable_being_checked)

    return(data_variable_being_checked)
  }

#' recode_columns
#'
#' Recodes columns from passed row and returns just table with those columns
#' and same rows as the data
#'
#' @param data The source database
#' @param variables_details_rows_to_process rows from variable details that are applicable
#' to this DB
#' @param data_name Name of the database being passed
#' @param log The option of printing log
#' @param print_note the option of printing the note columns
#' @param else_default default else value to use if no else is present
#'
#' @return Returns recoded and labeled data
recode_columns <-
  function(data,
           variables_details_rows_to_process,
           data_name,
           log,
           print_note,
           else_default) {
    # Split variables to process into recode map and func
    map_variables_to_process <-
      variables_details_rows_to_process[grepl(pkg.env$recode.key.map, variables_details_rows_to_process[[pkg.env$columns.recTo]]),]
    if (nrow(map_variables_to_process) > 0) {
      stop(paste0(pkg.env$recode.key.map, "variables were detected however this feature is not yet supported"))
    }

    id_variables_to_process <-
      variables_details_rows_to_process[grepl(pkg.env$recode.key.id.from, variables_details_rows_to_process[[pkg.env$columns.recTo]]),]

    func_variables_to_process <-
      variables_details_rows_to_process[grepl(pkg.env$recode.key.derived.var, variables_details_rows_to_process[[pkg.env$columns.VariableStart]]),]

    non_derived_keys <- c(pkg.env$recode.key.func,pkg.env$recode.key.map,pkg.env$recode.key.id.from)
    rec_variables_to_process <-
      variables_details_rows_to_process[(!grepl(
        paste(non_derived_keys,collapse="|"),
        variables_details_rows_to_process[[pkg.env$columns.recTo]]
      )) &
        (!grepl(pkg.env$recode.key.derived.var,
                variables_details_rows_to_process[[pkg.env$columns.VariableStart]])),]

    label_list <- list()
    # Set interval if none is present
    valid_intervals <- c("[,]", "[,)", "(,], (,)")
    interval_default <- valid_intervals[[1]]
    recoded_data <- data[, 0]

    # Loop through the rows of recode vars
    while (nrow(rec_variables_to_process) > 0) {
      variable_being_checked <-
        as.character(rec_variables_to_process[1,
                                              pkg.env$columns.Variable])
      rows_being_checked <-
        rec_variables_to_process[rec_variables_to_process[[pkg.env$columns.Variable]] == variable_being_checked,]
      first_row <- rows_being_checked[1,]
      # The name of the variable start
      data_variable_being_checked <-
        get_data_variable_name(
          data_name = data_name,
          row_being_checked = first_row,
          variable_being_checked = variable_being_checked,
          data = data
        )
      if (is.null(data[[data_variable_being_checked]])) {
        warning(
          paste(
            "Data",
            data_name,
            "does not contain the variable",
            data_variable_being_checked
          )
        )
        next
      }
      # Check for From column duplicates
      all_from_values_for_variable <-
        rows_being_checked[[pkg.env$columns.recFrom]]
      if (length(unique(all_from_values_for_variable)) != length(all_from_values_for_variable)) {
        for (single_from in all_from_values_for_variable) {
          # Check if value is repeated more then once
          if (sum(all_from_values_for_variable == single_from) > 1) {
            stop(
              paste(
                single_from,
                "was detected more then once in",
                variable_being_checked,
                "please make sure only one from value is being recoded"
              )
            )
          }
        }
      }

      # Set factor for all recode values
      label_list[[variable_being_checked]] <-
        create_label_list_element(rows_being_checked)
      else_value <-
        as.character(rows_being_checked[rows_being_checked[[pkg.env$columns.recFrom]] == "else",
                                        pkg.env$columns.recTo])
      if (length(else_value) == 1 &&
          !is_equal(else_value, "character(0)")) {
        else_value <-
          format_recoded_value(else_value, label_list[[variable_being_checked]]$type)
        if (is_equal(else_value, "copy")) {
          recoded_data[variable_being_checked] <-
            data[data_variable_being_checked]
        } else {
          recoded_data[variable_being_checked] <- else_value
        }
        # Catch multiple else rows
      } else if (length(else_value) > 1) {
        stop(
          paste(
            variable_being_checked,
            " contains",
            length(else_value),
            "rows of else only one else value is allowed"
          )
        )
      }
      else {
        recoded_data[variable_being_checked] <- else_default
      }
      num_else_rows <- nrow(recoded_data)
      # Remove else rows from rows_being_checked
      rows_being_checked <-
        rows_being_checked[!rows_being_checked[[pkg.env$columns.recFrom]] == "else",]
      if (nrow(rows_being_checked) > 0) {
        log_table <- rows_being_checked[, 0]
        log_table$value_to <- NA
        log_table$From <- NA
        log_table$rows_recoded <- NA
        levels(recoded_data[[variable_being_checked]]) <-
          c(levels(recoded_data[[variable_being_checked]]),
            levels(rows_being_checked[[pkg.env$columns.recTo]]))

        # Range overlap setup
        checked_data_rows <- c(rep(FALSE, nrow(data)))

        for (row in seq_len(nrow(rows_being_checked))) {
          row_being_checked <- rows_being_checked[row,]
          # If cat go check for label and obtain it

          # regardless obtain unit and attach

          # find var name for this database
          data_variable_being_checked <-
            get_data_variable_name(
              data_name = data_name,
              row_being_checked = row_being_checked,
              variable_being_checked = variable_being_checked,
              data = data
            )

          # Recode the variable
          from_values <- list()
          # Check for presence of interval in the recFrom column
          # Catches any values as long as they are surrounded by the specific intervals
          if (grepl("\\[*\\]|\\[*\\)|\\(*\\]|\\(*\\)",
                    as.character(row_being_checked[[pkg.env$columns.recFrom]]))) {
            # This splits the value in 2 parts [1] being first half the interval and value and [2] being second half and closing interval
            from_values <-
              strsplit(as.character(row_being_checked[[pkg.env$columns.recFrom]]), ",")[[1]]
            # This just trims white space from both in case a space is present before or after the ,
            from_values[[1]] <- trimws(from_values[[1]])
            from_values[[2]] <- trimws(from_values[[2]])
            # Extracts the interval brackets themselves
            interval_left <- substr(from_values[[1]], 1, 1)
            interval_right <-
              substr(from_values[[2]],
                     nchar(from_values[[2]]),
                     nchar(from_values[[2]]))
            interval <- paste0(interval_left, ",", interval_right)
            # Remove the interval leaving only values
            from_values[[1]] <-
              gsub("\\[|\\]|\\(|\\)", "", from_values[[1]])
            from_values[[2]] <-
              gsub("\\[|\\]|\\(|\\)", "", from_values[[2]])
          } else {
            temp_from <-
              as.character(row_being_checked[[pkg.env$columns.recFrom]])
            from_values[[1]] <- temp_from
            from_values[[2]] <- from_values[[1]]
          }
          value_to_recode_to <-
            as.character(row_being_checked[[pkg.env$columns.recTo]])
          if (from_values[[1]] == from_values[[2]]) {
            interval <- "[,]"
          } else if (!interval %in% valid_intervals) {
            message(
              paste(
                "For variable",
                variable_being_checked,
                "invalid interval was passed.\nDefault interval will be used:",
                interval_default
              )
            )
            interval <- interval_default
          }

          valid_row_index <- compare_value_based_on_interval(
            compare_columns = data_variable_being_checked,
            data = data,
            left_boundary = from_values[[1]],
            right_boundary = from_values[[2]],
            interval = interval
          )

          # Check for range duplicates
          if(all(!checked_data_rows[valid_row_index])){
            checked_data_rows[valid_row_index] <- TRUE
          }else{
            stop(paste0("Overlapping ranges detected for variable", variable_being_checked))
          }

          # Start construction of dataframe for log
          log_table[row, "value_to"] <- value_to_recode_to
          log_table[row, "From"] <-
            as.character(row_being_checked[[pkg.env$columns.recFrom]])
          log_table[row, "rows_recoded"] <-
            sum(valid_row_index, na.rm = TRUE)
          num_else_rows <-
            num_else_rows - log_table[row, "rows_recoded"]

          value_to_recode_to <-
            format_recoded_value(value_to_recode_to, label_list[[variable_being_checked]]$type)
          if (is_equal(value_to_recode_to, "copy")) {
            value_to_recode_to <-
              data[valid_row_index, data_variable_being_checked]
          }
          recoded_data[valid_row_index, variable_being_checked] <-
            value_to_recode_to
          if (print_note &&
              !is.null(row_being_checked[[pkg.env$columns.Notes]]) &&
              !is_equal(row_being_checked[[pkg.env$columns.Notes]],
                        "") &&
              !is.na(row_being_checked[[pkg.env$columns.Notes]])) {
            message(
              "NOTE for ",
              variable_being_checked,
              ": ",
              as.character(row_being_checked[[pkg.env$columns.Notes]])
            )
          }
        }
        # if log was requested print it
        if (log) {
          message(
            "The variable ",
            data_variable_being_checked,
            " was recoded into ",
            variable_being_checked,
            " for the database ",
            data_name,
            " the following recodes were made: "
          )
          if (length(else_value) > 0) {
            extra_row <- nrow(log_table) + 1
            log_table[extra_row , "value_to"] <- else_value
            log_table[extra_row , "From"] <-
              "else"
            log_table[extra_row , "rows_recoded"] <-
              num_else_rows
          }
          # Reset rowCount to avoid confusion
          rownames(log_table) <- NULL

          print(log_table)
        }
      }
      rec_variables_to_process <-
        rec_variables_to_process[!rec_variables_to_process[[pkg.env$columns.Variable]] == variable_being_checked,]
    }

    # Process funcVars
    while (nrow(func_variables_to_process) > 0) {
      first_row <- func_variables_to_process[1,]
      first_row_variable_name <-
        as.character(first_row[[pkg.env$columns.Variable]])
      # get name of var pass to
      derived_return <-
        recode_derived_variables(
          variable_being_processed = first_row_variable_name,
          recoded_data = recoded_data,
          variables_details_rows_to_process = func_variables_to_process,
          log = log,
          print_note = print_note,
          else_default = else_default,
          label_list = label_list,
          var_stack = c()
        )
      label_list <- derived_return$label_list
      recoded_data <- derived_return$recoded_data
      func_variables_to_process <-
        derived_return$variables_details_rows_to_process
    }

    #Process Id Vars
    top_function_frame <- parent.frame(2)
    while (nrow(id_variables_to_process) > 0) {
      # Extract type of id creation
      current_id <- id_variables_to_process[1,]
      id_variables_to_process <- id_variables_to_process[-1,]
      id_creation_function <-
        as.character(current_id[[pkg.env$columns.recTo]])
      id_creation_function <-
        strsplit(id_creation_function, "::")[[1]][[2]]
      id_creation_function <- trimws(id_creation_function)

      # Extract the variables
      id_feeder_vars <-
        as.character(current_id[[pkg.env$columns.VariableStart]])
      id_feeder_vars <- strsplit(id_feeder_vars, "::")[[1]][[2]]
      id_feeder_vars <-  gsub("\\[|\\]", "", id_feeder_vars)
      id_feeder_vars <- strsplit(id_feeder_vars, ",")[[1]]
      tmp_feeder_vars <- c()
      for (single_var in id_feeder_vars) {
        single_var <- trimws(single_var)
        tmp_feeder_vars <- append(tmp_feeder_vars, single_var)
      }

      # Extract Id Name
      id_name <-
        as.character(current_id[[pkg.env$columns.Variable]])

      # Create id_object to append at the end
      tmp_list <-
        list(var_name = id_name, feeder_vars = tmp_feeder_vars)
      top_function_frame$id_role_name <-
        append(top_function_frame$id_role_name, tmp_list)


    }

    # Populate data Labels
    recoded_data <-
      label_data(label_list = label_list, data_to_label = recoded_data)

    return(recoded_data)
  }

#' Compare Value Based On Interval
#'
#' Compare values on the scientific notation interval
#'
#' @param left_boundary the min value
#' @param right_boundary the max value
#' @param data the data that contains values being compared
#' @param compare_columns The columns inside data being checked
#' @param interval The scientific notation interval
#'
#' @return a boolean vector containing true for rows where the
#' comparison is true
compare_value_based_on_interval <-
  function(left_boundary,
           right_boundary,
           data,
           compare_columns,
           interval) {
    valid_row_index <- vector()
    # In case of strings this handles it the suppress warnings is to get rid of the warning when string is checked as numeric
    if (suppressWarnings(is.na(as.numeric(left_boundary)))) {
      valid_row_index <-
        data[[compare_columns]] %in% data[[compare_columns]][which(left_boundary == data[[compare_columns]])]
    } else {
      if (interval == "[,]") {
        valid_row_index <-
          data[[compare_columns]] %in% data[[compare_columns]][which(as.numeric(left_boundary) <= data[[compare_columns]] &
                                                                       data[[compare_columns]] <= as.numeric(right_boundary))]
      } else if (interval == "[,)") {
        valid_row_index <-
          data[[compare_columns]] %in% data[[compare_columns]][which(as.numeric(left_boundary) <= data[[compare_columns]] &
                                                                       data[[compare_columns]] < as.numeric(right_boundary))]
      } else if (interval == "(,]") {
        valid_row_index <-
          data[[compare_columns]] %in% data[[compare_columns]][which(as.numeric(left_boundary) < data[[compare_columns]] &
                                                                       data[[compare_columns]] <= as.numeric(right_boundary))]
      } else if (interval == "(,)") {
        valid_row_index <-
          data[[compare_columns]] %in% data[[compare_columns]][which(as.numeric(left_boundary) < data[[compare_columns]] &
                                                                       data[[compare_columns]] < as.numeric(right_boundary))]
      } else {
        stop("Invalid Argument was passed")
      }
    }

    return(valid_row_index)
  }

# Does the following updates on the passed variable details sheet
# Removes variables which are not present in the variables sheet
# Updates the columns, VariableType, label, variableLabel and Units from
# from the variables sheets
update_variable_details_based_on_variable_sheet <-
  function(variable_sheet, variable_details) {
    # Remove the columns from the variables details sheet that we will
    # update from the variables sheet
    variable_details <-
      variable_details[,!(
        names(variable_details) %in% c(
          pkg.env$columns.variablesDetails.typeStart,
          pkg.env$columns.label,
          pkg.env$columns.VariableLabel,
          pkg.env$columns.Units
        )
      )]

    # Only keep the columns in the variables sheet that we will use to
    # update the variables details sheet
    variable_sheet <-
      variable_sheet[, c(
        pkg.env$columns.Variable,
        pkg.env$columns.VariableType,
        pkg.env$columns.label,
        pkg.env$columns.VariableLabel,
        pkg.env$columns.Units
      )]

    # Update the variables details sheet by joining on the Variable column
    # in both sheets
    variable_details <-
      merge(
        variable_details,
        variable_sheet,
        by.x = pkg.env$columns.Variable,
        by.y = pkg.env$columns.Variable,
        all.x = TRUE
      )

    # Remove variables not present in variable_sheet
    variable_details <-
      variable_details[variable_details[[pkg.env$columns.Variable]] %in%
                         variable_sheet[[pkg.env$columns.Variable]],]

    return(variable_details)
  }

#' Recode NA formatting
#'
#' Recodes the NA depending on the var type
#'
#' @param cell_value The value inside the recTo column
#' @param var_type the toType of a variable
#'
#' @return an appropriately coded tagged NA
format_recoded_value <- function(cell_value, var_type) {
  recode_value <- NULL
  if (grepl("NA", cell_value)) {
    na_value_list <- strsplit(cell_value, ":")[[1]]
    if (is_equal(var_type, pkg.env$columns.value.CatType)) {
      recode_value <- paste("NA(", na_value_list[[3]], ")", sep = "")
    } else {
      recode_value <- haven::tagged_na(as.character(na_value_list[[3]]))
    }
  } else {
    if (!is_equal(var_type, pkg.env$columns.value.CatType) &&
        !is_equal(cell_value, "copy")) {
      cell_value <- as.numeric(cell_value)
    }
    recode_value <- cell_value
  }

  return(recode_value)
}

recode_derived_variables <-
  function(recoded_data,
           variable_being_processed,
           variables_details_rows_to_process,
           log,
           print_note,
           else_default,
           label_list,
           var_stack) {
    if (nrow(variables_details_rows_to_process) <= 0) {
      stop(paste(
        variable_being_processed,
        "is missing from variable_details"
      ))
    }
    var_stack <- c(var_stack, variable_being_processed)
    # obtain rows to process and updated variables to Process
    variable_rows <-
      variables_details_rows_to_process[variables_details_rows_to_process[[pkg.env$columns.Variable]] == variable_being_processed,]
    fun_variable_rows <-
      variable_rows[grepl(pkg.env$recode.key.func, variable_rows[[pkg.env$columns.recTo]]),]
    variables_details_rows_to_process <-
      variables_details_rows_to_process[variables_details_rows_to_process[[pkg.env$columns.Variable]] != variable_being_processed,]
    for (row_num in seq_len(nrow(fun_variable_rows))) {
      # Check for presence of feeder variables in data and in the
      # variable being processed stack
      feeder_vars <-
        as.list(strsplit(as.character(variable_rows[row_num,][[pkg.env$columns.VariableStart]]), "::"))[[1]][[2]]
      # Extract the variable names used in the function
      feeder_vars <- gsub("\\[|\\]", "", feeder_vars)
      feeder_vars <- as.list(strsplit(feeder_vars, ","))[[1]]
      feeder_vars <- sapply(feeder_vars, trimws)
      used_feeder_vars <- feeder_vars
      # Verify that those variables have been recoded
      feeder_vars <- setdiff(feeder_vars, names(recoded_data))

      # Check if the variable has a function to recode
      non_func_missing_variables <-
        setdiff(feeder_vars, unique(as.character(variables_details_rows_to_process[[pkg.env$columns.Variable]])))
      if (length(non_func_missing_variables) > 0) {
        warning(
          paste(
            variable_being_processed,
            "could not be derived because",
            feeder_vars,
            "was never specified nor is it a function variable,
            therefore it was not recoded \n"
          )
        )
        var_stack <-
          var_stack[!(var_stack == variable_being_processed)]

        label_list[[as.character(variable_being_processed)]] <-
          assign(variable_being_processed, create_label_list_element(variable_rows))

        return(
          list(
            var_stack = var_stack,
            label_list = label_list,
            recoded_data = recoded_data,
            variables_details_rows_to_process = variables_details_rows_to_process
          )
        )
      }
      # Check for presense in var_stack
      if (length(intersect(feeder_vars, var_stack)) > 0) {
        conflict_vars <- intersect(feeder_vars, var_stack)
        stop(
          paste(
            conflict_vars,
            "is required to create",
            variable_being_processed,
            "but",
            variable_being_processed,
            "is needed to create",
            conflict_vars
          )
        )
      }

      # Update var_stack and recurse to get the feeder vars
      for (one_feeder in feeder_vars) {
        # Need to check recoded data again in case a recursion added it
        if (!one_feeder %in% names(recoded_data)) {
          derived_return <-
            recode_derived_variables(
              variable_being_processed = one_feeder,
              recoded_data = recoded_data,
              variables_details_rows_to_process = variables_details_rows_to_process,
              log = log,
              print_note = print_note,
              else_default = else_default,
              label_list = label_list,
              var_stack = var_stack
            )
          var_stack <- derived_return$var_stack
          label_list <- derived_return$label_list
          recoded_data <- derived_return$recoded_data
          variables_details_rows_to_process <-
            derived_return$variables_details_rows_to_process
        }
      }

      # Obtain the function for each row

      row_being_checked <- fun_variable_rows[row_num,]
      func_cell <-
        as.character(row_being_checked[[pkg.env$columns.recTo]])
      function_being_used <-
        as.list(strsplit(func_cell, "::"))[[1]][[2]]

      column_value <-
        recoded_data %>%
        dplyr::rowwise() %>%
        dplyr::select(used_feeder_vars) %>%
        dplyr::do(
          column_being_added = calculate_custom_function_row_value(
            .,
            variable_names = used_feeder_vars,
            custom_function_name = function_being_used
          )
        )
      # Set type of var
      if (as.character(row_being_checked[[pkg.env$columns.ToType]]) !=
          pkg.env$columns.value.CatType) {
        column_value <-
          as.numeric(unlist(column_value[["column_being_added"]]))
      } else{
        column_value <-
          as.factor(unlist(column_value[["column_being_added"]]))
      }
      recoded_data[[variable_being_processed]] <-
        column_value

      var_stack <-
        var_stack[!(var_stack == variable_being_processed)]
    }

    return(
      list(
        var_stack = var_stack,
        label_list = label_list,
        recoded_data = recoded_data,
        variables_details_rows_to_process = variables_details_rows_to_process
      )
    )
  }

calculate_custom_function_row_value <-
  function(row_values,
           variable_names,
           custom_function_name) {
    row_values <- unname(row_values)
    custom_function_return_value <-
      do.call(get(custom_function_name), row_values)

    return(custom_function_return_value)
  }
