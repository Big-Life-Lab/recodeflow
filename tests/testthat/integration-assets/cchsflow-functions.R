# utility-functions

if_else2 <- function(x, a, b) {
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

# adl

adl_score_5_fun <-
  function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {

    # Create vector of ADL input variables
    all_adl_vector <- c(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)
    # Count the number of missing values in vector
    count_missing_adl <- sum(all_adl_vector == "NA(b)")
    # Count the number of not applicable values in vector
    count_not_applicable_adl <- sum(all_adl_vector == "NA(a)")
    # Count the number of ADLs that require help (value of 2)
    count_adl <- sum(all_adl_vector == 2)


    # If the individual had missing data for any of the variables then set
    # the score to missing
    # If the individual has "not applicable" for any of the variables, then
    # set the score to "not applicable"
    # Otherwise set the score for each individual to the count of the number
    # of tasks they needed help with
    total_num_adls <- 5
    ADL_score_5 <-
      ifelse(
        count_not_applicable_adl >= 1,
        "NA(a)",
        ifelse(
          count_missing_adl >= 1,
          "NA(b)",
          count_adl
        )
      )

    return(ADL_score_5)
  }

# alcohol

binge_drinker_fun <-
  function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6,
           ALW_2A7) {
    # If respondents had alcohol in the last week
    if_else2(ALW_1 == 1,
      # Males with at least one day with 5 or more drinks
      if_else2((DHH_SEX == 1 & (ALW_2A1 >= 5 | ALW_2A2 >= 5 | ALW_2A3 >=5 |
                                ALW_2A4 >= 5 | ALW_2A5 >= 5 | ALW_2A6 >= 5 |
                                ALW_2A7 >= 5)), 1,
      # Males with no days with 5 or more drinks
      if_else2((DHH_SEX == 1 & (ALW_2A1 %in% (0:4) & ALW_2A2 %in% (0:4) &
                                ALW_2A3 %in% (0:4) & ALW_2A4 %in% (0:4) &
                                ALW_2A5 %in% (0:4) & ALW_2A6 %in% (0:4) &
                                ALW_2A7 %in% (0:4))), 2,
      # Females with at least one day with 4 or more drinks
      if_else2((DHH_SEX == 2 & (ALW_2A1 >= 4 | ALW_2A2 >= 4 | ALW_2A3 >= 4 |
                                ALW_2A4 >= 4 | ALW_2A5 >= 4 | ALW_2A6 >= 4 |
                                ALW_2A7 >= 4)), 1,
      # Females with no days with 4 or more drinks
      if_else2((DHH_SEX == 2 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                                ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                                ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                                ALW_2A7 %in% (0:3))), 2, "NA(b)")))),
      # Respondents who didn't indicate they had alcohol in the last week
      "NA(a)")
  }

low_drink_score_fun <-
  function(DHH_SEX, ALWDWKY){
    ## Step 1
    # How many standard drinks did you have in a week?
    step1<-
      if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995),
      if_else2(ALWDWKY %in% (0:10), 0,
      if_else2(DHH_SEX == 1 & ALWDWKY > 10 & ALWDWKY <= 15, 0,
      if_else2(DHH_SEX == 2 & ALWDWKY > 10 & ALWDWKY <= 15, 1,
      if_else2(DHH_SEX == 1 & ALWDWKY > 15 & ALWDWKY <= 20, 1,
      if_else2(DHH_SEX == 2 & ALWDWKY > 15 & ALWDWKY <= 20, 3,
      if_else2(ALWDWKY >20, 3, NA)))))),
      NA)

    ## Categorical score
    low_drink_score <-
      # Low risk
      if_else2(step1 == 0, 1,
      # Marginal risk
      if_else2(step1 %in% (1:2), 2,
      # Medium risk
      if_else2(step1 %in% (3:4), 3,
      # High risk
      if_else2(step1 %in% (5:9), 4, tagged_na("(b)")))))

    return(low_drink_score)
  }

low_drink_long_fun <-
  function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
           ALW_2A5, ALW_2A6, ALW_2A7){
    # Test if inputs are in valid range
    if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995) & ALC_1 %in% (1:2) &
               ALW_1 %in% (1:2) & ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995) &
               ALW_2A3 %in% (0:995) & ALW_2A4 %in% (0:995) &
               ALW_2A5 %in% (0:995) & ALW_2A6 %in% (0:995) &
               ALW_2A7 %in% (0:995),
    # Increased long term risk from due to drinking (1)
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in%(4:995) | ALW_2A2 %in%(4:995) |
                             ALW_2A3 %in%(4:995) | ALW_2A4 %in%(4:995) |
                             ALW_2A5 %in%(4:995) | ALW_2A6 %in%(4:995) |
                             ALW_2A7 %in%(4:995) | ALWDWKY %in%(16:995)), 1,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in%(3:995) | ALW_2A2 %in%(3:995) |
                             ALW_2A3 %in%(3:995) | ALW_2A4 %in%(3:995) |
                             ALW_2A5 %in%(3:995) | ALW_2A6 %in%(3:995) |
                             ALW_2A7 %in%(3:995) | ALWDWKY %in%(11:995)), 1,
    # No increased long term health risks due to drinking (2)
    # Includes those who did not drink in past 7 days or past 12 months
    if_else2(ALC_1 == 2 |ALW_1 ==2, 2,
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                             ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                             ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                             ALW_2A7 %in% (0:3)) & ALWDWKY %in% (0:15), 2,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in% (0:2) & ALW_2A2 %in% (0:2) &
                             ALW_2A3 %in% (0:2) & ALW_2A4 %in% (0:2) &
                             ALW_2A5 %in% (0:2) & ALW_2A6 %in% (0:2) &
                             ALW_2A7 %in% (0:2)) & ALWDWKY %in% (0:10), 2,
             "NA(b)"))))),
    "NA(b)")
  }

low_drink_short_fun <-
  function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
           ALW_2A5, ALW_2A6, ALW_2A7){
    # Test if inputs are in valid range
    if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995) & ALC_1 %in% (1:2) &
               ALW_1 %in% (1:2) & ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995) &
               ALW_2A3 %in% (0:995) & ALW_2A4 %in% (0:995) &
               ALW_2A5 %in% (0:995) & ALW_2A6 %in% (0:995) &
               ALW_2A7 %in% (0:995),
    # Increased short term risk from due to drinking (1)
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in%(5:995) | ALW_2A2 %in%(5:995) |
                             ALW_2A3 %in%(5:995) | ALW_2A4 %in%(5:995) |
                             ALW_2A5 %in%(5:995) | ALW_2A6 %in%(5:995) |
                             ALW_2A7 %in%(5:995) | ALWDWKY %in%(16:995)), 1,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in%(4:995) | ALW_2A2 %in%(4:995) |
                             ALW_2A3 %in%(4:995) | ALW_2A4 %in%(4:995) |
                             ALW_2A5 %in%(4:995) | ALW_2A6 %in%(4:995) |
                             ALW_2A7 %in%(4:995) | ALWDWKY %in%(11:995)), 1,
    # No increased short term health risks due to drinking (2)
    # Includes those who did not drink in past 7 days or past 12 months
    if_else2(ALC_1 == 2 |ALW_1 ==2, 2,
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in% (0:4) & ALW_2A2 %in% (0:4) &
                             ALW_2A3 %in% (0:4) & ALW_2A4 %in% (0:4) &
                             ALW_2A5 %in% (0:4) & ALW_2A6 %in% (0:4) &
                             ALW_2A7 %in% (0:4)) & ALWDWKY %in% (0:15), 2,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                             ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                             ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                             ALW_2A7 %in% (0:3)) & ALWDWKY %in% (0:10), 2,
                                                          "NA(b)"))))),
    "NA(b)")
  }

# diet

diet_score_fun <-
  function(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, DHH_SEX) {
    all_diet_score_parameters <- c(
      FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI
    )
    if(sum(haven::is_tagged_na(all_diet_score_parameters, "a")) > 0) {
      return(haven::tagged_na("a"))
    }
    if(sum(haven::is_tagged_na(all_diet_score_parameters, "b")) > 0) {
      return(haven::tagged_na("b"))
    }

    if("NA(b)" %in% c(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI)) {
      return("NA(b)")
    }

    # Total fruit and vegetables, excluding fruit juice
      total_fruitveg <-
        if_else2(!is.na(FVCDFRU) & !is.na(FVCDSAL) & !is.na(FVCDPOT) &
                   !is.na(FVCDCAR)  & !is.na(FVCDVEG), FVCDFRU + FVCDSAL +
                   FVCDPOT + FVCDCAR + FVCDVEG, NA)

    # Maximum total fruit and vegetables = 8
      max_fruitveg <-
        if_else2(is.na(total_fruitveg), NA,
                 if_else2(total_fruitveg>8, 8, total_fruitveg))

    # High potato intake flag
      daily_pot_limit <-
        if_else2(DHH_SEX==1, 1,
                 if_else2(DHH_SEX==2, 5/7, NA))
      FVCDPOT_high <-
        if_else2(is.na(FVCDPOT), NA,
                 if_else2(FVCDPOT>=(daily_pot_limit), 1, 0))

    # No carrot intake flag
      FVCDCAR_nil <-
        if_else2(is.na(FVCDCAR), NA,
                 if_else2(FVCDCAR==0, 1, 0))

    # High juice intake flag
      FVCDJUI_high <-
        if_else2(is.na(FVCDJUI), NA,
                 if_else2(FVCDJUI <=1, 0, FVCDJUI - 1))

    diet_raw_score <- if_else2(!is.na(max_fruitveg) & !is.na(FVCDPOT_high) &
                     !is.na(FVCDCAR_nil) & !is.na(FVCDJUI_high),  2 +
                       max_fruitveg - (2*FVCDPOT_high) - (2*FVCDCAR_nil) -
                       (2*FVCDJUI_high), NA)

    diet_score <- if_else2(diet_raw_score <0, 0,
              if_else2(diet_raw_score >10, 10,
                      if_else2(!is.na(diet_raw_score), diet_raw_score,
                               tagged_na("b"))))
    return(diet_score)
  }

diet_score_fun_cat <-
  function(diet_score){
    # Poor diet
    if_else2(diet_score >=0 & diet_score < 2, 1,
    # Fair diet
    if_else2(diet_score >=2 & diet_score < 8, 2,
    # Adequate diet
    if_else2(diet_score >=8 & diet_score <= 10, 3,
    # No response
    if_else2(diet_score == tagged_na("a"), "NA(a)", "NA(b)"))))
  }

# bmi

bmi_fun <-
  function(HWTGHTM, HWTGWTK) {
    if_else2(
      (!is.na(HWTGHTM)) & (!is.na(HWTGWTK)), (HWTGWTK / (HWTGHTM * HWTGHTM)),
      tagged_na("b")
    )
  }

bmi_fun_cat <-
  function(HWTGBMI_der){
    # Underweight
    if_else2(HWTGBMI_der < 18.5, 1,
    # Normal weight
    if_else2(HWTGBMI_der >= 18.5 & HWTGBMI_der < 25, 2,
    # Overweight
    if_else2(HWTGBMI_der >= 25 & HWTGBMI_der < 30, 3,
    # Obese
    if_else2(HWTGBMI_der >= 30, 4,
    # No response
    if_else2(HWTGBMI_der == tagged_na("a"), "NA(a)", "NA(b)")))))
  }

# percent-time-canada

pct_time_fun <-
  function(DHHGAGE_cont, SDCGCBG, SDCGRES) {
    if (is_equal(SDCGCBG, 1)) {
      return(100)
    }
    DHHGAGE_cont <- if_else2(DHHGAGE_cont > 0, DHHGAGE_cont,
                             return(tagged_na("b")))
    SDCGRES <- if_else2(SDCGRES == 1, 4.5,
                        if_else2(SDCGRES == 2, 15, return(tagged_na("b"))))

    if_else2(SDCGCBG == 2, (SDCGRES / DHHGAGE_cont * 100), tagged_na("b"))
  }

pct_time_fun_cat <-
  function(pct_time_der){
    if_else2(pct_time_der >= 0 & pct_time_der <= 10, 1,
    if_else2(pct_time_der > 10 & pct_time_der <= 20, 2,
    if_else2(pct_time_der > 20 & pct_time_der <= 30, 3,
    if_else2(pct_time_der > 30 & pct_time_der <= 40, 4,
    if_else2(pct_time_der > 40 & pct_time_der <= 50, 5,
    if_else2(pct_time_der > 50 & pct_time_der <= 60, 6,
    if_else2(pct_time_der > 60 & pct_time_der <= 70, 7,
    if_else2(pct_time_der > 70 & pct_time_der <= 80, 8,
    if_else2(pct_time_der > 80 & pct_time_der <= 90, 9,
    if_else2(pct_time_der > 90 & pct_time_der <= 100, 10,
    if_else2(pct_time_der == tagged_na("a"), "NA(a)", "NA(b)")))))))))))
  }

age_cat_fun <- function(DHHGAGE_cont) {
  age_cat <-
    if_else2(
      dplyr::between(DHHGAGE_cont, 12, 14), 1,
      if_else2(
        dplyr::between(DHHGAGE_cont, 15, 17), 2,
        if_else2(
          dplyr::between(DHHGAGE_cont, 18, 19), 3,
          if_else2(
            dplyr::between(DHHGAGE_cont, 20, 24), 4,
            if_else2(
              dplyr::between(DHHGAGE_cont, 25, 29), 5,
              if_else2(
                dplyr::between(DHHGAGE_cont, 30, 34), 6,
                if_else2(
                  dplyr::between(DHHGAGE_cont, 35, 39), 7,
                  if_else2(
                    dplyr::between(DHHGAGE_cont, 40, 44), 8,
                    if_else2(
                      dplyr::between(DHHGAGE_cont, 45, 49), 9,
                      if_else2(
                        dplyr::between(DHHGAGE_cont, 50, 54), 10,
                        if_else2(
                          dplyr::between(DHHGAGE_cont, 55, 59), 11,
                          if_else2(
                            dplyr::between(DHHGAGE_cont, 60, 64), 12,
                            if_else2(
                              dplyr::between(DHHGAGE_cont, 65, 69), 13,
                              if_else2(
                                dplyr::between(DHHGAGE_cont, 70, 74), 14,
                                if_else2(
                                  dplyr::between(DHHGAGE_cont, 75, 79), 15,
                                  if_else2((DHHGAGE_cont >= 80), 16,
                                           "NA(b)")
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  return(age_cat)
}

pct_time_fun_cat <-
  function(pct_time_der){
    if_else2(pct_time_der >= 0 & pct_time_der <= 10, 1,
    if_else2(pct_time_der > 10 & pct_time_der <= 20, 2,
    if_else2(pct_time_der > 20 & pct_time_der <= 30, 3,
    if_else2(pct_time_der > 30 & pct_time_der <= 40, 4,
    if_else2(pct_time_der > 40 & pct_time_der <= 50, 5,
    if_else2(pct_time_der > 50 & pct_time_der <= 60, 6,
    if_else2(pct_time_der > 60 & pct_time_der <= 70, 7,
    if_else2(pct_time_der > 70 & pct_time_der <= 80, 8,
    if_else2(pct_time_der > 80 & pct_time_der <= 90, 9,
    if_else2(pct_time_der > 90 & pct_time_der <= 100, 10,
    if_else2(haven::is_tagged_na(pct_time_der, "a"), "NA(a)", "NA(b)")))))))))))
  }

# RACDPAL

RACDPAL_fun <- function(RAC_1, RAC_2A, RAC_2B, RAC_2C){
  # Check to see if all values are in range
  if_else2((RAC_1 %in% 1:3) & (RAC_2A %in% 1:3) & (RAC_2B %in% 1:4) &
             (RAC_2C %in% 1:3),
           # Check if respondents said reductions impacted them often
           if_else2(RAC_1 == 2 | RAC_2A == 2 | RAC_2B == 2 | RAC_2C == 2, 2,
                    # Check if respondents said reductions impacted them
                    # sometimes
                    if_else2(RAC_1 == 1 | RAC_2A == 1 | RAC_2B == 1 |
                               RAC_2C == 1, 1,
                             # Check if respondents said reductions never
                             # impacted them
                             if_else2(RAC_1 == 3 & RAC_2A == 3 &
                                        (RAC_2B %in% 3:4) &
                                        RAC_2C == 3, 3, "NA(b)"))), "NA(b)"
  )
}

# smoking

time_quit_smoking_fun <- function(SMK_09A_B, SMKG09C) {
  SMKG09C_cont <-
    if_else2(
      SMKG09C == 1, 4,
      if_else2(
        SMKG09C == 2, 8,
        if_else2(SMKG09C == 3, 12,
                 if_else2(SMKG09C == "NA(a)", tagged_na("a"), tagged_na("b")
                 )
        )
      )
    )
  tsq_ds <-
    if_else2(
      SMK_09A_B == 1, 0.5,
      if_else2(
        SMK_09A_B == 2, 1.5,
        if_else2(
          SMK_09A_B == 3, 2.5,
          if_else2(SMK_09A_B == 4, SMKG09C_cont,
                   if_else2(SMK_09A_B == "NA(a)", tagged_na("a"), tagged_na("b")
                   )
          )
        )
      )
    )
  return(tsq_ds)
}

smoke_simple_fun <-
  function(SMKDSTY_cat5, time_quit_smoking) {

    # Nested function: current smoker status
    derive_current_smoker <- function(SMKDSTY_cat5) {
      smoker <-
        ifelse(SMKDSTY_cat5 %in% c(1, 2), 1,
               ifelse(SMKDSTY_cat5 %in% c(3, 4, 5), 0,
                      ifelse(SMKDSTY_cat5 == "NA(a)", "NA(a)", "NA(b)")))
      return(smoker)
    }
    smoker <- derive_current_smoker(SMKDSTY_cat5)

    # Nested function: ever smoker status
    derive_ever_smoker <- function(SMKDSTY_cat5) {
      eversmoker <-
        ifelse(SMKDSTY_cat5 %in% c(1, 2, 3, 4), 1,
               ifelse(SMKDSTY_cat5 == 5, 0,
                      ifelse(SMKDSTY_cat5 == "NA(a)", "NA(a)", "NA(b)")))
      return(eversmoker)
    }
    eversmoker <- derive_ever_smoker(SMKDSTY_cat5)

    # smoke_simple 0 = non-smoker
    smoke_simple <-
      ifelse(smoker == 0 & eversmoker == 0, 0,
      # smoke_simple 1 = current smoker
        ifelse(smoker == 1 & eversmoker == 1, 1,
      # smoke_simple 2 = former daily smoker quit =<5 years or former occasional
      # smoker
          ifelse(smoker == 0 & eversmoker == 1 & time_quit_smoking <= 5 |
                   SMKDSTY_cat5 == 4, 2,
      # smoke_simple 3 = former daily smoker quit > 5 years
            ifelse(smoker == 0 & eversmoker == 1 & time_quit_smoking > 5,
                   3,
                   ifelse(smoker == "NA(a)" & eversmoker == "NA(a)" &
                            time_quit_smoking == "NA(a)", "NA(a)", "NA(b)")))))
    return(smoke_simple)
  }

pack_years_fun_cat <- function(pack_years_der){
  pack_years_cat <-
    if_else2(pack_years_der == 0, 1,
    if_else2(pack_years_der > 0 & pack_years_der <= 0.01, 2,
    if_else2(pack_years_der > 0.01 & pack_years_der <= 3.0, 3,
    if_else2(pack_years_der > 3.0 & pack_years_der <= 9.0, 4,
    if_else2(pack_years_der > 9.0 & pack_years_der <= 16.2, 5,
    if_else2(pack_years_der > 16.2 & pack_years_der <= 25.7, 6,
    if_else2(pack_years_der > 25.7 & pack_years_der <= 40.0, 7,
    if_else2(pack_years_der > 40.0, 8,
    if_else2(pack_years_der == tagged_na("a"), "NA(a)", "NA(b)")))))))))

  return(pack_years_cat)
}

# immigration

immigration_fun <-
  function(SDCFIMM, SDCGCBG, SDCGCGT, SDCGRES){
    # White Canada-born
    if_else2(SDCFIMM == 2 & SDCGCBG ==1 & SDCGCGT ==1, 1,
    # Non-white Canadian born
    if_else2(SDCFIMM == 2 & SDCGCBG ==1 & SDCGCGT ==2, 2,
    # White immigrant born outside of Canada (0-9 years in Canada)
    if_else2(SDCFIMM == 1 & SDCGCBG ==2 & SDCGCGT ==1 & SDCGRES ==1, 3,
    # Non-white immigrant born outside of Canada (0-9 years in Canada)
    if_else2(SDCFIMM == 1 & SDCGCBG ==2 & SDCGCGT ==2 & SDCGRES ==1, 4,
    # White immigrant born outside of Canada (10+ years in Canada)
    if_else2(SDCFIMM == 1 & SDCGCBG ==2 & SDCGCGT ==1 & SDCGRES ==2, 5,
    # Non-white immigrant born outside of Canada (10+ years in Canada)
    if_else2(SDCFIMM == 1 & SDCGCBG ==2 & SDCGCGT ==2 & SDCGRES ==2, 6,

    if_else2(SDCFIMM =="NA(a)"|SDCGCBG =="NA(a)"|SDCGCGT =="NA(a)"
             |SDCGRES =="NA(a)","NA(a)", "NA(b)"
             )))))))
  }
