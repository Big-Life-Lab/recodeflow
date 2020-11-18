var_details <-
  data.frame(
    "variable" = rep(c("A", "B", "C"), each = 3),
    "dummyVariable" = c("AY", "AN", "ANA", "BY", "BN", "BNA", "CY", "CN", "CNA"),
    "toType" = rep("cat", times = 9),
    "databaseStart" = rep("tester", times = 9),
    "variableStart" = rep(
      c("tester::startA", "tester::startB", "tester::startC"),
      each = 3
    ),
    "fromType" = rep("cat", times = 9),
    "recTo" = rep(c("1", "2", "NA::a"), times = 3),
    "numValidCat" = rep("2", times = 9),
    "catLabel" = rep(c("Yes", "No", "Not answered"), times = 3),
    "catLabelLong" = rep(c("Yes", "No", "Not answered"), times =
                           3),
    "recFrom" = rep(c("1", "2", "9"), times = 3),
    "catStartLabel" = rep(c("Yes", "No", "Not answered"), times =
                            3),
    "variableStartShortLabel" = rep(c("Group A", "Group B", "Group C"), each =
                                      3),
    "variableStartLabel" = rep(c("Group A", "Group B", "Group C"), each =
                                 3),
    "units" = rep("NA", times = 9),
    "notes" = rep("This is not real data", times = 9)
  )
var_sheet <-
  data.frame(
    "variable" = c("A", "B", "C"),
    "label" = c("Group A", "Group B", "Group C"),
    "labelLong" = c("Group A", "Group B", "Group C"),
    "section" = rep("tester", times=3),
    "subject" = rep("tester",times = 3),
    "variableType" = rep("Categorical", times=3),
    "databaseStart" = rep("tester", times = 3),
    "units" = rep("NA", times = 3),
    "variableStart" = c("tester::startA", "tester::startB", "tester::startC")
  )

sample_data <-
  data.frame(
    "startA" = sample(1:3, 100, replace = TRUE),
    "startB" = sample(1:3, 100, replace = TRUE),
    "startC" = sample(1:3, 100, replace = TRUE)
  )
