source("./integration-assets/cchsflow-functions.R")

pack_years_fun <-
  function(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, SMKG203_cont,
           SMKG207_cont, SMK_204, SMK_05B,
           SMK_208, SMK_05C, SMKG01C_cont, SMK_01A) {
    # Age verification
    if (is.na(DHHGAGE_cont)) {
      return(tagged_na("b"))
    } else if (DHHGAGE_cont < 0) {
      return(tagged_na("b"))
    }

    # PackYears for Daily Smoker
    pack_years <-
      if_else2(
        SMKDSTY_A == 1, pmax(((DHHGAGE_cont - SMKG203_cont) *
                              (SMK_204 / 20)), 0.0137),
        # PackYears for Occasional Smoker (former daily)
        if_else2(
          SMKDSTY_A == 2, pmax(((DHHGAGE_cont - SMKG207_cont -
                                 time_quit_smoking) * (SMK_208 / 20)), 0.0137) +
            ((pmax((SMK_05B * SMK_05C / 30), 1) / 20) * time_quit_smoking),
          # PackYears for Occasional Smoker (never daily)
          if_else2(
            SMKDSTY_A == 3, (pmax((SMK_05B * SMK_05C / 30), 1) / 20) *
              (DHHGAGE_cont - SMKG01C_cont),
            # PackYears for former daily smoker (non-smoker now)
            if_else2(
              SMKDSTY_A == 4, pmax(((DHHGAGE_cont - SMKG207_cont -
                                     time_quit_smoking) *
                                    (SMK_208 / 20)), 0.0137),
              # PackYears for former occasional smoker (non-smoker now) who
              # smoked at least 100 cigarettes lifetime
              if_else2(
                SMKDSTY_A == 5 & SMK_01A == 1, 0.0137,
                # PackYears for former occasional smoker (non-smoker now) who
                # have not smoked at least 100 cigarettes lifetime
                if_else2(
                  SMKDSTY_A == 5 & SMK_01A == 2, 0.007,
                  # Non-smoker
                  if_else2(SMKDSTY_A == 6, 0,
                           # Account for NA(a)
                           if_else2(SMKDSTY_A == "NA(a)", tagged_na("a"),
                                    tagged_na("b"))
                  )
                )
              )
            )
          )
        )
      )
    return(pack_years)
  }

SurveyCycle.fun <- function(data_name) {
  switch(
    data_name,
    cchs2001_p = {
      return(1)
    },
    cchs2003_p = {
      return(2)
    },
    cchs2005_p = {
      return(3)
    },
    cchs2007_2008_p = {
      return(4)
    },
    cchs2009_2010_p = {
      return(5)
    },
    cchs2012_p = {
      return(6)
    },
    cchs2013_2014_p = {
      return(7)
    }
  )

  stop(paste(
    "Unknown data_name argument when creating SurveyCycle variable",
    data_name
  ))
}

#' Uses the transformation by [Smithson and Verkuilen](https://pubmed.ncbi.nlm.nih.gov/16594767/)
#' to bring a variable within the open interval (0,1) on HUI3. This allows it
#' to then be used as an outcome in a beta regression model.
#'
#' @param huidhsi HUI3
#' @param n The sample size of the dataset
#'
#' @return
#' @export
#'
#' @examples
hui_beta_transform <- function(
  huidhsi,
  n
) {
  a <- -0.36
  b <- 1
  huidhsi_1 <- (huidhsi - a)/(b - a)

  n <- 453137
  huidhsi_2 <- (huidhsi_1*(n - 1) + 0.5)/n

  return(huidhsi_2)
}

hui_beta_back_transformed <- function(hui_beta_transformed, n) {
  n <- 453137
  a <- -0.36
  b <- 1

  first_step <- hui_beta_transformed*n
  second_step <- first_step - 0.5
  third_step <- second_step/(n - 1)
  fourth_step <- third_step * (b - a)
  fifth_step <- fourth_step + a
  return(fifth_step)
}

hui_arc_sine_transform <- function(huidhsi) {
  first_term <- 2
  second_term <- (huidhsi + 0.36)/(1 + 0.36)
  third_term <- 1
  return(asin(first_term * second_term - third_term))
}

hui_arc_sine_back_transformed <- function(hui_arc_sine) {
  first_step <- sin(hui_arc_sine)
  second_step <- first_step + 1
  third_step <-  second_step * (1 + 0.36)
  fourth_step <- third_step/2
  fifth_step <- fourth_step - 0.36
  return(fifth_step)
}

ALWDWKY_HUI <- function(ALCDTTM, ALWDWKY) {
  return(ifelse(ALCDTTM == 3, 0, ALWDWKY))
}

' @title resp_condition_fun1
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. 3 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2009-2014) that only use COPD and Emphysema as
#'  a combined variable. Asthma is used across CCHS cycles as a separate
#'  variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_031 variable indicating if respondent has asthma
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # Using resp_condition_fun1() to create values across CCHS cycles
#' # (2009-2014) resp_condition_fun1() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2009_2010 <- suppressWarnings(rec_with_table(
#'   cchs2009_2010_p,  c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2009_2010)
#'
#' resp2011_2012 <- suppressWarnings(rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2011_2012)
#'
#' combined_resp <-
#'  suppressWarnings(merge_rec_data(resp2009_2010, resp2011_2012))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun2}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun1 <-
  function(DHHGAGE_cont, CCC_091, CCC_031) {
    resp_condition <-
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
           (CCC_091 == 1 | CCC_031 == 1)), 1,
        if_else2(
          ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
             (CCC_091 == 1 | CCC_031 == 1)), 2,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
               (CCC_091 == 2 | CCC_031 == 2)), 3,
            if_else2(
              ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
                 (CCC_091 == 2 & CCC_031 == 2)), 3,
              if_else2((CCC_091 == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)",
                       "NA(b)")
            )
          )
        )
      )
    return(resp_condition)
  }

#' @title resp_condition_fun2
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function is for CCHS cycles (2005-2007) that use COPD &
#'  Emphysema as separate variables, as well as Bronchitis. Asthma is used
#'  across CCHS cycles as a separate variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_91E variable indicating if respondent has emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'
#' @param CCC_031 variable indicating if respondent has asthma
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#'
#' # Using resp_condition_fun2() to create values across CCHS cycles
#' # (2005-2007) resp_condition_fun2() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2005 <- suppressWarnings(rec_with_table(
#'   cchs2005_p, c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2005)
#'
#' resp2007_2008 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p,  c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2007_2008)
#'
#' combined_resp <- suppressWarnings(merge_rec_data(resp2005, resp2007_2008))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun2 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F, CCC_91A, CCC_031) {
    resp_condition <-
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
           (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1 | CCC_031 == 1)), 1,
        if_else2(
          ((DHHGAGE_cont >= 30 & DHHGAGE_cont < 35) &
             (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1 | CCC_031 == 1)), 2,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
               (CCC_91A == 1 | CCC_031 == 1)), 2,
            if_else2(
              ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
                 (CCC_91A == 2 & CCC_031 == 2)), 3,
              if_else2(
                ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 30) &
                   (CCC_91E == 2 & CCC_91F == 2 & CCC_91A == 2 &
                      CCC_031 == 2)), 3,
                if_else2((CCC_91E == "NA(a)" & CCC_91F == "NA(a)" &
                            CCC_91A == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)",
                         "NA(b)")
              )
            )
          )
        )
      )
    return(resp_condition)
  }

#' @title resp_condition_fun3
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function for CCHS cycles (2001-2003) that use COPD and
#'  Emphysema as a combined variable, as well as Bronchitis. Asthma is used
#'  across CCHS cycles as a separate variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'
#' @param CCC_031 variable indicating if respondent has asthma
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # Using resp_condition_fun3() to create values across CCHS cycles
#' # (2001-2003) resp_condition_fun3() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2001 <- suppressWarnings(rec_with_table(
#'   cchs2001_p, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2001)
#'
#' resp2003 <- suppressWarnings(rec_with_table(
#'   cchs2003_p,c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2003)
#'
#' combined_resp <- suppressWarnings(merge_rec_data(resp2001, resp2003))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun2}}
#'
#' @export
resp_condition_fun3 <-
  function(DHHGAGE_cont, CCC_091, CCC_91A, CCC_031) {
    resp_condition <-
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
           (CCC_091 == 1 | CCC_91A == 1 | CCC_031 == 1)), 1,
        if_else2(
          ((DHHGAGE_cont >= 30 & DHHGAGE_cont < 35) &
             (CCC_091 == 1 | CCC_91A == 1 | CCC_031 == 1)), 2,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
               (CCC_91A == 1 | CCC_031 == 1)), 2,
            if_else2(
              ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
                 (CCC_91A == 2 & CCC_031 == 2)), 3,
              if_else2(
                ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 30) &
                   (CCC_091 == 2 & CCC_91A == 2 & CCC_031 == 2)), 3,
                if_else2((CCC_091 == "NA(a)" & CCC_91A == "NA(a)" &
                            CCC_031 == "NA(a)"),
                         "NA(a)", "NA(b)")
              )
            )
          )
        )
      )
    return(resp_condition)
  }

#' @title Number of chronic conditions (5 chronic conditions)
#'
#' @description This function generates a derived variable (number_conditions)
#'  that counts the number of chronic conditions a respondent has. This function
#'  takes 5 CCHS-defined conditions (heart disease, cancer, stroke, bowel
#'  disorder, and arthritis), and well one derived variable (respiratory
#'  condition) to count the number of conditions a respondent has.
#'
#' @param CCC_121 variable indicating if respondent has heart disease (1 =
#'  respondent has heart disease, 2 = respondent does not have heart disease)
#'
#' @param CCC_131 variable indicating if respondent has active cancer (1 =
#'  respondent has active cancer, 2 =  respondent does not have active cancer)
#'
#' @param CCC_151 variable indicating if respondent suffers from the effects
#'  of a stroke (1 = respondent suffers from stroke effects, 2 = respondent
#'  does not suffer from stroke effects)
#'
#' @param CCC_171 variable indicating if respondent has a bowel disorder (1 =
#'  respondent has bowel disorder, 2 = respondent does not have a bowel
#'  disorder)
#'
#' @param resp_condition_der derived variable indicating if respondent has a
#'  respiratory condition (1 = respondent is over the age of 35 and has
#'  a respiratory condition, 2 = respondent is under the age of 35 and has a
#'  respiratory conditions, 3 = respondent does not have a respiratory
#'  condition). See \code{\link{resp_condition_fun1}} for
#'  documentation on how variable was derived.
#'
#' @param CCC_051 variable indicating if respondent has arthritis or
#'  rheumatism (1 = respondent has arthritis or rheumatism, 2 = respondent does
#'  not have arthritis or rheumatism)
#'
#' @details mood disorder (CCC_280) was not asked to respondents in the 2001
#'  CCHS survey cycle. This mean respondents in this cycle will only be able to
#'  have a maximum of 6 chronic conditions as opposed to 7 for respondents in
#'  other cycles. \code{\link{multiple_conditions_fun2}} is used for CCHS cycles
#'  from 2003 to 2014.
#'
#' @return A categorical variable indicating the number of chronic conditions
#'  a respondent has. Respondents with 5 or more conditions are grouped in the
#'  "5+" category.
#'
#' @examples
#'  # Using rec_with_table() to generate multiple_conditions in a CCHS
#'  # cycle.
#'
#'  # multiple_conditions_fun1() is specified in variable_details.csv along with
#'  # the CCHS variables and cycles included.
#'
#'  # To generate multiple_conditions, use rec_with_table() and specify the
#'  # multiple_conditions, along with the variables that are derived from it.
#'  # Since resp_condition_der is also a derived variable, you will have to
#'  # specify the variables that are derived from it. In this example, data
#'  # from the 2001 CCHS will be used, so DHHGAGE_cont, CCC_091, and CCC_91A,
#'  # and CCC_031 will be specified along with resp_condition_der.
#'
#' library(cchsflow)
#'  conditions_2001 <- suppressWarnings(rec_with_table(cchs2001_p,
#'  c("DHHGAGE_cont", "CCC_091",
#'  "CCC_91A", "CCC_031", "CCC_121","CCC_131","CCC_151", "CCC_171","CCC_280",
#'  "resp_condition_der","CCC_051", "number_conditions")))
#'
#'  head(conditions_2001)
#'
#'  # Generating multiple_conditions with user inputted values
#'  # Let's say you are an individual that has heart disease, bowel disorder,
#'  # and arthritis. multiple_conditions_fun1() can be used to count the number
#'  # of chronic conditions you have
#'
#' library(cchsflow)
#'  num_conditions <- multiple_conditions_fun1(CCC_121 = 1, CCC_131 = 2,
#'  CCC_151 = 2, CCC_171 = 1, resp_condition_der = 3, CCC_051 = 1)
#'
#' print(num_conditions)
#'
#' @seealso \code{\link{multiple_conditions_fun2}}
#' @export
multiple_conditions_fun1 <-
  function(CCC_121, CCC_131, CCC_151, CCC_171, resp_condition_der, CCC_051){

    suppressWarnings({
      # Convert variables to numeric
      CCC_121 <- as.numeric(CCC_121)
      CCC_131 <- as.numeric(CCC_131)
      CCC_151 <- as.numeric(CCC_151)
      CCC_171 <- as.numeric(CCC_171)
      resp_condition_der <- as.numeric(resp_condition_der)
      CCC_051 <- as.numeric(CCC_051)
    })

    # set invalid/NA values to 0
    CCC_121 <- if_else2(CCC_121 %in% (1:2), CCC_121, 0)
    CCC_131 <- if_else2(CCC_131 %in% (1:2), CCC_131, 0)
    CCC_151 <- if_else2(CCC_151 %in% (1:2), CCC_151, 0)
    CCC_171 <- if_else2(CCC_171 %in% (1:2), CCC_171, 0)
    resp_condition_der <- if_else2(resp_condition_der %in% (1:3),
                                   resp_condition_der, 0)
    CCC_051 <- if_else2(CCC_051 %in% (1:2), CCC_051, 0)

    # adjust resp_condition to yes = 1, no = 2
    resp_condition_der <- if_else2(resp_condition_der %in% c(1:2), 1, 2)

    # Calculate number of conditions based on yes
    conditions <-
      (CCC_121%%2) + (CCC_131%%2) + (CCC_151%%2) +(CCC_171%%2) +
      (resp_condition_der%%2) + (CCC_051%%2)

    if_else2(conditions>= 5, "5+", conditions)
  }

#' @title Number of chronic conditions (6 chronic conditions)
#'
#' @description This function generates a derived variable (number_conditions)
#'  that counts the number of chronic conditions a respondent has. This function
#'  takes 6 CCHS-defined conditions (heart disease, cancer, stroke, bowel
#'  disorder, mood disorder and arthritis), and well one derived variable
#'  (respiratory condition) to count the number of conditions a respondent has.
#'
#' @param CCC_121 variable indicating if respondent has heart disease (1 =
#'  respondent has heart disease, 2 = respondent does not have heart disease)
#'
#' @param CCC_131 variable indicating if respondent has active cancer (1 =
#'  respondent has active cancer, 2 =  respondent does not have active cancer)
#'
#' @param CCC_151 variable indicating if respondent suffers from the effects
#'  of a stroke (1 = respondent suffers from stroke effects, 2 = respondent
#'  does not suffer from stroke effects)
#'
#' @param CCC_171 variable indicating if respondent has a bowel disorder (1 =
#'  respondent has bowel disorder, 2 = respondent does not have a bowel
#'  disorder)
#'
#' @param CCC_280 variable indicating if respondent has a mood disorder (1 =
#'  respondent has a mood disorder, 2 = respondent does not have a mood
#'  disorder. Note, variable was not asked to respondents in the 2001 CCHS
#'  survey cycle.
#'
#' @param resp_condition_der derived variable indicating if respondent has a
#'  respiratory condition. (1 = respondent is over the age of 35 and has
#'  a respiratory condition, 2 = respondent is under the age of 35 and has a
#'  respiratory conditions, 3 = respondent does not have a respiratory
#'  condition). See \code{\link{resp_condition_fun1}} for
#'  documentation on how variable was derived.
#'
#' @param CCC_051 variable indicating if respondent has arthritis or
#'  rheumatism (1 = respondent has arthritis or rheumatism, 2 = respondent does
#'  not have arthritis or rheumatism)
#'
#' @details mood disorder (CCC_280) was not asked to respondents in the 2001
#'  CCHS survey cycle. This mean respondents in this cycle will only be able to
#'  have a maximum of 6 chronic conditions as opposed to 7 for respondents in
#'  other cycles. \code{\link{multiple_conditions_fun1}} is used for CCHS cycles
#'  from 2003 to 2014.
#'
#' @return A categorical variable indicating the number of chronic conditions
#'  a respondent has. Respondents with 5 or more conditions are grouped in the
#'  "5+" category.
#'
#' @examples
#'  # Using rec_with_table() to generate multiple_conditions in a CCHS
#'  # cycle.
#'
#'  # multiple_conditions_fun2() is specified in variable_details.csv along with
#'  # the CCHS variables and cycles included.
#'
#'  # To generate multiple_conditions, use rec_with_table() and specify the
#'  # multiple_conditions, along with the variables that are derived from it.
#'  # Since resp_condition_der is also a derived variable, you will have to
#'  # specify the variables that are derived from it. In this example, data
#'  # from the 2010 CCHS will be used, so DHHGAGE_cont, CCC_091, and CCC_031
#'  # will be specified along with resp_condition_der.
#'
#' library(cchsflow)
#'  conditions_2009_2010 <- suppressWarnings(rec_with_table(cchs2009_2010_p,
#'  c("DHHGAGE_cont", "CCC_091",
#'  "CCC_031", "CCC_121","CCC_131","CCC_151", "CCC_171","CCC_280",
#'  "resp_condition_der","CCC_051", "number_conditions")))
#'
#'  head(conditions_2009_2010)
#'
#'  # Generating multiple_conditions with user inputted values
#'  # Let's say you are an individual that has heart disease, bowel disorder,
#'  # and arthritis. multiple_conditions_fun2() can be used to count the number
#'  # of chronic conditions you have
#'
#' library(cchsflow)
#'  num_conditions <- multiple_conditions_fun2(CCC_121 = 1, CCC_131 = 2,
#'  CCC_151 = 2, CCC_171 = 1, CCC_280 = 2, resp_condition_der = 3, CCC_051 = 1)
#'
#' print(num_conditions)
#'
#' @seealso \code{\link{multiple_conditions_fun1}}
#' @export
multiple_conditions_fun2 <-
  function(CCC_121, CCC_131, CCC_151, CCC_171, CCC_280, resp_condition_der,
           CCC_051){
    suppressWarnings({
      # Convert variables to numeric
      CCC_121 <- as.numeric(CCC_121)
      CCC_131 <- as.numeric(CCC_131)
      CCC_151 <- as.numeric(CCC_151)
      CCC_171 <- as.numeric(CCC_171)
      CCC_280 <- as.numeric(CCC_280)
      resp_condition_der <- as.numeric(resp_condition_der)
      CCC_051 <- as.numeric(CCC_051)
    })

    # set invalid/NA values to 0
    CCC_121 <- if_else2(CCC_121 %in% (1:2), CCC_121, 0)
    CCC_131 <- if_else2(CCC_131 %in% (1:2), CCC_131, 0)
    CCC_151 <- if_else2(CCC_151 %in% (1:2), CCC_151, 0)
    CCC_171 <- if_else2(CCC_171 %in% (1:2), CCC_171, 0)
    CCC_280 <- if_else2(CCC_280 %in% (1:2), CCC_280, 0)
    resp_condition_der <- if_else2(resp_condition_der %in% (1:3),
                                   resp_condition_der, 0)
    CCC_051 <- if_else2(CCC_051 %in% (1:2), CCC_051, 0)

    # adjust resp_condition to yes = 1, no = 2
    resp_condition_der <- if_else2(resp_condition_der %in% c(1:2), 1, 2)

    # Calculate number of conditions based on yes
    conditions <-
      (CCC_121%%2) + (CCC_131%%2) + (CCC_151%%2) +(CCC_171%%2) +
                  (CCC_280%%2) + (resp_condition_der%%2) + (CCC_051%%2)

    if_else2(conditions>= 5, "5+", conditions)
  }

#' @title COPD_Emph_der_fun1
#'
#' @description This is one of 2 functions used to create a derived variable
#'  (COPD_Emph_der) that determines if a respondents has either COPD or
#'  Emphysema. 2 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2005-2008) that use COPD and Emphysema as
#'  a combined variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_91E variable indicating if respondent has Emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @return a categorical variable (COPD_Emph_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # COPD_Emph_der_fun1() to create values across CCHS cycles
#' # (2005-2008) COPD_Emph_der_fun1() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform COPD_Emph_der, use rec_with_table() for each CCHS cycle
#' # and specify COPD_Emph_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine COPD_Emph_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' COPD2005 <- suppressWarnings(rec_with_table(
#'   cchs2005_p,  c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' head(COPD2005)
#'
#' COPD2007_2008 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p, c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' tail(COPD2007_2008)
#'
#' combined_COPD <- suppressWarnings(merge_rec_data(COPD2005, COPD2007_2008))
#'
#' head(combined_COPD)
#' tail(combined_COPD)
#' @seealso \code{\link{COPD_Emph_der_fun2}}
#'
#' @export
#'
COPD_Emph_der_fun1 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F) {
    COPD_Emph <-
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
          (CCC_91E == 1 | CCC_91F == 1)), 1,
        if_else2(
          ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
             (CCC_91E == 1 | CCC_91F == 1)), 2,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
               (CCC_91E == 2 & CCC_91F == 2)), 3,
            if_else2(
              ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
                 (CCC_91E == 2 & CCC_91F == 2)), 3,
              if_else2(
                (CCC_91E == "NA(a)" & CCC_91F == "NA(a)"), "NA(a)", "NA(b)")
            )
          )
        )
      )
    return(COPD_Emph)
  }

#' @title COPD_Emph_der_fun2
#'
#' @description This is one of 2 functions used to create a derived variable
#'  (COPD_Emph_der) that determines if a respondents has either COPD or
#'  Emphysema. 2 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2001-2003, 2009-2014) that use COPD and
#'  Emphysema as a combined variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @return a categorical variable (COPD_Emph_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # COPD_Emph_der_fun2() to create values across CCHS cycles
#' # (2001-2003, 2009-2014) COPD_Emph_der_fun2() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform COPD_Emph_der, use rec_with_table() for each CCHS cycle
#' # and specify COPD_Emph_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine COPD_Emph_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' COPD2001 <- suppressWarnings(rec_with_table(
#'   cchs2001_p,  c(
#'     "DHHGAGE_cont", "CCC_091",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' head(COPD2001)
#'
#' COPD2014 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p, c(
#'     "DHHGAGE_cont", "CCC_091",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' tail(COPD2014)
#'
#' combined_COPD <- suppressWarnings(merge_rec_data(COPD2001, COPD2014))
#'
#' head(combined_COPD)
#' tail(combined_COPD)
#' @seealso \code{\link{COPD_Emph_der_fun2}}
#'
#' @export
#'

COPD_Emph_der_fun2 <-
  function(DHHGAGE_cont, CCC_091) {
    COPD_Emph <-
      if_else2(
        (DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
          (CCC_091 == 1), 1,
        if_else2(
          ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
             (CCC_091 == 1)), 2,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
               (CCC_091 == 2)), 3,
            if_else2(
              ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
                 (CCC_091 == 2)), 3,
              if_else2(
                (CCC_091 == "NA(a)"),
                "NA(a)", "NA(b)"
              )
            )
          )
        )
      )
    return(COPD_Emph)
  }
