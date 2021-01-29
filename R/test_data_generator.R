var_details <-  data.frame(
  "variable" = c(rep("time",times = 2) , rep("status", times = 4), rep("trt", times = 5), rep("age", times = 2), rep("sex", times = 3), rep("ascites", times = 4), rep("hepato", times = 4), rep("spiders", times = 4), rep("edema", times = 4),
                 rep("bili", times = 2), rep("chol", times =3), rep("albumin", times =2), rep("copper", times = 3),  rep("alk.phos", times=3), rep("ast", times = 3),  rep("trig", times=3), rep("platelet", times = 3), rep("protime", times = 3), rep("stage", times = 6),
                 "example_der", rep("agegrp5", times = 12), rep("agegrp10", times = 7), rep("age_cont", times = 19)),

  "toType" = c(rep("cont", times = 2), rep("cat", times = 4), rep("cat", times = 5), rep("cont", times = 2), rep("cat", times = 3), rep("cat", times = 4), rep("cat", times = 4), rep("cat", times = 4), rep("cat", times = 4),
               rep("cont", times = 2), rep("cont", times = 3), rep("cont", times = 2), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cat", times = 6),
               "cont", rep("cat", times = 12), rep("cat", times = 7), rep("cont", times = 19)),

  "fromType" = c(rep("cont", times = 2), rep("cat", times = 4), rep("cat", times = 5), rep("cont", times = 2), rep("cat", times = 3), rep("cat", times = 4), rep("cat", times = 4), rep("cat", times = 4), rep("cat", times = 4),
                 rep("cont", times = 2), rep("cont", times = 3), rep("cont", times = 2), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cont", times = 3), rep("cat", times = 6),
                 "cont", rep("cat", times =12), rep("cat", times = 7), rep("cat", times = 19)),

  "databaseStart" = c(rep("tester1, tester2", times = 64),
                      rep("tester1", times = 12), rep("tester2", times = 7),  rep("tester1", times = 12), rep("tester2", times = 7)),

  "variableStart" = c(rep("[time]", times = 2), rep("[status]", times = 4), rep("[trt]", times = 5), rep("[age]",times = 2), rep("[sex]", times = 3), rep("[ascites]", times = 4), rep("[hepato]", times = 4), rep("[spiders]", times = 4), rep("[edema]", times = 4),
                      rep("[bili]", times = 2), rep("[chol]", times =3), rep("[albumin]", times =2), rep("[copper]", times =3), rep("[alk.phos]", times = 3), rep("[ast]", times = 3), rep("[trig]", times = 3), rep("[platelet]", times =3), rep("[protime]", times = 3),  rep("[stage]", times = 6),
                      "DerivedVar::[chol, bili]", rep("tester1::agegrp", times = 12), rep("tester2::agegrp", times = 7), rep("tester1::agegrp", times = 12), rep("tester2::agegrp", times = 7)),

  "variableStartShortLabel" = c(rep("time", times = 2), rep("status", times = 4), rep("treatment", times = 5), rep("age", times = 2), rep("sex", times = 3), rep("ascites", times = 4), rep("hepato", times = 4), rep("spiders", times = 4), rep("edema", times = 4),
                                rep("bili", times =2), rep("chol", times = 3), rep("albumin", times = 2), rep("copper", times =3), rep("alk.phos", times = 3), rep("ast", times = 3), rep("trig", times = 3), rep("platelet", times = 3), rep("protime", times = 3), rep("stage", times = 6),
                                "example_der", rep("agegrp", times =12), rep("agegrp", times = 7), rep("agegrp", times = 19)),

  "numValidCat" = c(rep("N/A", times = 2), rep("3", times = 4), rep("3", times = 5), rep("N/A", times =2), rep("2", times = 3), rep("2", times = 4), rep("2", times = 4), rep("2", times = 4), rep("3", times = 4),
                    rep("N/A", times = 2), rep("N/A", times = 3), rep("N/A", times =2), rep("N/A", times = 3), rep("N/A", times = 3), rep("N/A", times = 3), rep("N/A", times = 3), rep("N/A", times = 3), rep("N/A", times = 3), rep("4", times = 6),
                    "N/A", rep("11", times = 12), rep("6", times = 7), rep("N/A", times = 19)),

  "recTo" = c(c("copy", "NA::b"), c("0", "1","2", "NA::b"), c("1","2","3","NA::a", "NA::b"), c("copy","NA::b"), c("m","f", "NA::b"), c("0", "1","NA::a", "NA::b"), c("0", "1","NA::a", "NA::b"),c("0", "1","NA::a", "NA::b"), c("0.0","0.5","1.0", "Na::b"),
              c("copy", "Na::b"),  c("copy", "Na::a", "Na::b"), c("copy", "Na::b"), c("copy", "Na::a", "Na::b"), c("copy", "Na::a", "Na::b"), c("copy", "Na::a", "Na::b"), c("copy", "Na::a", "Na::b"), c("copy", "Na::a", "Na::b"), c("copy", "Na::a", "Na::b"), c("1", "2","3","4", "Na::a", "NA::b"),
              "Func::example_der_fun", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "Na::b"), c("1", "2", "3", "4", "5", "6", "Na::b"), c("27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "Na::b"), c("25", "35", "45", "55", "65", "75", "Na::b")),

  "recFrom" = c(c("[0,4556]","else"), c("0", "1","2", "else"), c("1","2","3", "NA", "else"), c("[25,79]", "else"), c("m","f", "else"), c("0", "1", "NA", "else"), c("0", "1","NA", "else"), c("0", "1","NA", "else"), c("0.0","0.5","1.0", "else"),
                c("[0,28]", "else"), c("[120, 1775]", "NA", "else"), c("[1.960, 4.64]", "else"), c("[4,588]", "NA", "else"), c("[289, 13863]", "NA", "else"), c("[26, 458]", "NA", "else"), c("[33, 598]", "NA", "else"),  c("[62, 721]", "NA", "else"), c("[9,18]", "NA", "else"), c("1", "2","3","4","Na","else"),
                "else", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "Na::b"), c("1", "2", "3", "4", "5", "6", "else"), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "Na::b"), c("1", "2", "3", "4", "5", "6", "Na::b")),

  "catLabel" = c(c("N/A", "missing"), c("censored", "transplant","dead", "missing"), c("D-penicillmain","Placebo", "none", "N/A", "missing"), rep("N/A", times = 2), c("Male","Female", "missing"), c("No ascites", "Yes ascites", "NA", "missing"), c("No hepatomegaly","yes hepatomegaly", "NA", "missing"), c("no spiders","yes spiders", "NA", "missing"), c("edema 0","edema 0.5","edema 1", "missing"),
                 c("bili", "missing"), c("chol", "N/A", "missing"), c("albumin", "missing"), c("cooper", "N/A", "missing"), c("alk.phos", "N/A", "missing"), c("ast", "N/A", "missing"), c("trig", "N/A", "missing"), c("platelet", "N/A", "missing"), c("protime", "N/A", "missing"), c("stage 1", "stage 2","stage 3","stage 4", "NA", "missing"),
                 "N/A", c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "missing"), c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "missing"), c("27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "missing"), c("25", "35", "45", "55", "65", "75", "missing")),

  "catLabelLong" = c(c("N/A", "missing"), c("censored", "transplant","dead", "missing"), c("D-penicillmain","Placebo", "no treatment", "N/A", "missing"), rep("N/A", times = 2),  c("Male","Female", "missing"), c("No ascites", "Yes ascites", "NA", "missing"), c("No hepatomegaly","yes hepatomegaly", "NA", "missing"), c("no spiders","yes spiders", "NA", "missing"), c("edema 0","edema 0.5","edema 1", "missing"),
                     c("N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "N/A", "missing"), c("N/A", "N/A", "missing"), c("stage 1", "stage 2","stage 3","stage 4", "NA", "missing"),
                     "N/A", c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "missing"), c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "missing"), c("27", "32", "37", "42", "47", "52", "57", "62", "67", "72", "77", "missing"), c("25", "35", "45", "55", "65", "75", "missing")),

  "units" = c(rep("days", times = 2), rep("N/A", times = 4), rep("N/A", times = 5), rep("years", time = 2), rep("N/A", times = 3), rep("N/A", times = 4), rep("N/A", times = 4), rep("N/A", times = 4), rep("N/A", times = 4),
              rep("mg/dl", times = 2), rep("mg/dl", times = 3), rep("g/dl", times = 2), rep("ug/dl", times = 3), rep("U/L", times = 3), rep("U/L", times = 3), rep("mg/dl", times = 3), rep("N/A", times = 3), rep("N/A", times = 3), rep("N/A", times = 6),
              "mg/dl", rep("years", times = 12), rep("years", times = 7), rep("years", times = 19)),

  "catStartLabel" = c(c("time", "N/A"), c("censored", "transplant","dead", "else"), c("D-penicillmain","Placebo", "none", "N/A", "else"), c("age", "else"), c("Male","Female", "else"), c("No ascites", "Yes ascites", "NA", "else"), c("No hepatomegaly","yes hepatomegaly", "NA", "else"), c("no spiders","yes spiders", "NA", "else"), c("edema 0","edema 0.5","edema 1", "else"),
                      c("bili", "N/A"), c("chol", "N/A", "else"), c("albumin", "else"), c("copper", "N/A", "else"), c("alk.phos", "N/A", "else"), c("ast", "N/A", "else"), c("trig", "N/A", "else"), c("platelet", "N/A", "else"), c("protime", "N/A", "else"),  c("stage 1", "stage 2","stage 3","stage 4", "N/A", "missing"),
                      "N/A", c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "else"), c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "else"),
                      c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79","else"), c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "else")),

  "notes" = c(rep("This is sample survival pbc data", times = 64), rep("adapted from sample survival pbc data", times = 38))
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



