var_details <-
  data.frame(
    "variable" = c("time", rep("status", times = 3), rep("trt", times = 3), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4),
                   "example_der", rep("agegrp5", times = 11), rep("agegrp10", times = 6), rep("age_cont", times = 17)),
    "toType" = c("cont", rep("cat", times = 3), rep("cat", times = 3), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4),
                 "cont", rep("cat", times = 11), rep("cat", times = 6), rep("cont", times = 17)),
    "fromType" = c("cont", rep("cat", times = 3), rep("cat", times = 3), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4),
                   "cont", rep("cat", times =11), rep("cat", times = 6), rep("cat", times = 17)),
    "databaseStart" = c(rep("tester1, tester2", times = 33),
                        rep("tester1", times = 11), rep("tester2", times = 6),  rep("tester1", times = 11), rep("tester2", times = 6)),
    "variableStart" = c("[time]", rep("[status]", times = 3), rep("[trt]", times = 3), "[age]", rep("[sex]", times = 2), rep("[ascites]", times = 2), rep("[hepato]", times = 2), rep("[spiders]", times = 2), rep("[edema]", times = 3), "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]", rep("[stage]", times = 4),
                        "DerivedVar::[chol, bili]", rep("tester1::agegrp", times = 11), rep("tester2::agegrp", times = 6), rep("tester1::agegrp", times = 11), rep("tester2::agegrp", times = 6)),
    "variableStartShortLabel" = c("time", rep("status", times = 3), rep("treatment", times = 3), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4),
                                  "example_der", rep("agegrp", times =11), rep("agegrp", times = 6), rep("agegrp", times = 17)),
    "numValidCat" = c("N/A", rep("3", times = 3), rep("3", times = 3), "N/A", rep("2", times = 8), rep("3", times = 3), rep("N/A", times = 9), rep("4", times = 4),
                      "N/A", rep("11", times = 11), rep("6", times = 6), rep("N/A", times = 17)),
    "recTo" = c("copy", "0", "1","2", "1","2","3", "copy","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("copy",times = 9), "1", "2","3","4",
                "Func::example_der_fun", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "1", "2", "3", "4", "5", "6", "27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "25", "35", "45", "55", "65", "75"),
    "recFrom" = c("else", "0", "1","2", "1","2","3", "else","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("else",times = 9), "1", "2","3","4",
                  "else", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "1", "2", "3", "4", "5", "6", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "1", "2", "3", "4", "5", "6"),
    "catLabel" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "none", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4",
                   "N/A", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "25", "35", "45", "55", "65", "75"),
    "catLabelLong" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "no treatment", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4",
                       "N/A", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "25", "35", "45", "55", "65", "75"),
    "units" = c("days", rep("N/A", times = 6), "years", rep("N/A", times = 11), rep("mg/dl", times = 2), "g/dl", "ug/dl", rep("U/L", times = 2), "mg/dl", rep("N/A", times = 6),
                "mg/dl", rep("years", times = 11), rep("years", times = 6), rep("years", times = 17)),

    "catStartLabel" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "none", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4",
                        "N/A", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
                               "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79"),
    "notes" = c(rep("This is sample survival pbc data", times = 33), rep("adapted from sample survival pbc data", times = 34))
  )

var_sheet <-
  data.frame(
    "variable" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage",
                   "example_der", "agegrp5", "agegrp10", "age_cont", "age_cont"),
    "label" = c("time","status","treatment", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage",
                "example_der", "agegrp5", "agegrp10", "age_cont", "age_cont"),
    "labelLong" = c("number of days between registration and the earlier of death, treatment or end of study", "status at end of study", "treatment", "age", "sex", "prescence of ascites", "prescence of hepatomegaly or enlarged liver",
                    "prescence of spiders", "edema", "bilirunbin concentration (blood)", "cholestral concentration (blood)", "albumin concentration (blood)", "copper concentration (urine)", "alkaline phosphotase concentration (blood)",
                    " aspartate aminotransferase concentration (blood)", "triglycerides concentration (blood)", "platelet count", "standarized blood clotting time", "histologic stage of disease",
                    "example of dervived function: concentration of cholestral * concentration of bilirunbin", "five year age groups", "ten year age groups", "continous age created from age groups", "continous age created from age groups"),
     "subject" = c(rep("study",times = 3), rep("demographic",times = 2), rep("physical symptom",times = 4), rep("lab test",times = 10),
                  "derived", rep("demographics", times =2), rep("demographics", times =2)),
    "section" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage",
                  "example", "age", "age", "age", "age"),
    "variableType" = c("cont", "cat", "cat", "cont","cat", "cat", "cat","cat", "cat", rep("cont", times = 9), "cat",
                       "cont", rep("cat", times = 2), rep("cont", times = 2)),
    "databaseStart" = c(rep("tester1, tester2", times = 20), "tester1", "tester2", "tester1", "tester2"),
    "units" = c("days", rep("N/A", times = 2), "years", rep("N/A", times = 5), rep("mg/dl", times = 2), "g/dl", "ug/dl", rep("U/L", times = 2), "mg/dl", rep("N/A", times = 3),
                "mg/dl", rep("years", times = 2), rep("years", times = 2)),
    "variableStart" = c("[time]","[status]", "[trt]", "[age]", "[sex]", "[ascites]","[hepato]","[spiders]","[edema]", "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]","[stage]",
                        "[example_der]", "tester1::agegrp", "tester2::agegrp", "tester1::agegrp", "tester2::agegrp")
  )


library(survival)
test1 <- survival::pbc[1:209,]
test2 <- survival::pbc[210:418,]

#Adapting the data for How To examples. Breaking cont age variable into categories - 5 and 10 year age groups.
range(test1$age)
agegrp <- cut(test1$age, breaks = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80), right = FALSE)
agegrp <- as.numeric(agegrp)
tester1 <- cbind(test1, agegrp)


range(test2$age)
agegrp <- cut(test2$age, breaks = c(20, 30, 40, 50, 60, 70, 80), right = FALSE)
agegrp <- as.numeric(agegrp)
tester2 <- cbind(test2, agegrp)





