get_feeder_vars <- function(derived_start_variable, database_name) {
  feeder_vars_capture_group <- "(.{0,}?)"

  # Regex for when the derived variable is the same for all databases
  # For example, DerivedVar::[var_one]
  single_derived_var_regex <- paste(
    pkg.env$recode.key.derived.var, "\\[", feeder_vars_capture_group, "\\]",
    sep = ""
  )
  # Regex to get the derived variables for a certain databases
  # For example, database_one::DerivedVar::[var_one]
  database_derived_var_regex <- paste(
    database_name, "::", single_derived_var_regex,
    sep = ""
  )
  # Regex to get the derived variables for the default derived variable
  # For example, [DerivedVar::[var_one]]
  default_derived_var_regex <- paste(
    "\\[", single_derived_var_regex, "\\]",
    sep = ""
  )

  feeder_var_string <- NA
  if(grepl(database_derived_var_regex, derived_start_variable)) {
    feeder_var_string <- regmatches(
      derived_start_variable,
      regexec(database_derived_var_regex, derived_start_variable)
    )[[1]][2]
  }
  else if(grepl(default_derived_var_regex, derived_start_variable)) {
    feeder_var_string <- regmatches(
      derived_start_variable,
      regexec(default_derived_var_regex, derived_start_variable)
    )[[1]][2]
  }
  else if(grepl(single_derived_var_regex, derived_start_variable)) {
    feeder_var_string <- regmatches(
      derived_start_variable,
      regexec(single_derived_var_regex, derived_start_variable)
    )[[1]][2]
  }
  if(is.na(feeder_var_string)) {
    return(NA)
  }
  feeder_var_string <- strip_brackets(feeder_var_string)

  feeder_vars <- as.list(strsplit(feeder_var_string, ","))[[1]]
  feeder_vars <- sapply(feeder_vars, trimws)
  return(feeder_vars)
}

