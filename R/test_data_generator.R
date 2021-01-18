var_details <-
  data.frame(
    "variable" = c("time", rep("status", times = 3), rep("trt", times = 3), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4), "example_der"),
    "dummyVariable" = c("NA", "status0", "status1","status2", "trt1","trt2", "trt3", "NA","sexM","sexF", "ascites0", "ascites1","hepato0","hepato1","spiders0","spiders1","edema0.0","edema0.5","edema1.0",rep("NA",times = 9), "stage1", "stage2","stage3","stage4", "N/A"),
    "toType" = c("cont", rep("cat", times = 3), rep("cat", times = 3), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4), "cont"),
    "databaseStart" = rep("tester1, tester2", times = 33),
    "variableStart" = c("[time]", rep("[status]", times = 3), rep("[trt]", times = 3), "[age]", rep("[sex]", times = 2), rep("[ascites]", times = 2), rep("[hepato]", times = 2), rep("[spiders]", times = 2), rep("[edema]", times = 3), "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]", rep("[stage]", times = 4), "DerivedVar::[chol, bili]"),
    "fromType" = c("cont", rep("cat", times = 3), rep("cat", times = 3), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4), "cont"),
    "recTo" = c("copy", "0", "1","2", "1","2","3", "copy","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("copy",times = 9), "1", "2","3","4", "Func::example_der_fun"),
    "numValidCat" = c("N/A", rep("3", times = 3), rep("3", times = 3), "N/A", rep("2", times = 8), rep("3", times = 3), rep("N/A", times = 9), rep("4", times = 4), "N/A"),
    "catLabel" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "none", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4", "N/A"),
    "catLabelLong" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "no treatment", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4", "N/A"),
    "recFrom" = c("else", "0", "1","2", "1","2","3", "else","m","f", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("else",times = 9), "1", "2","3","4", "else"),
    "catStartLabel" = c("N/A", "censored", "transplant","dead", "D-penicillmain","Placebo", "none", "N/A", "Male","Female", "No ascites", "Yes ascites","No hepatomegaly","yes hepatomegaly","no spiders","yes spiders","edema 0","edema 0.5","edema 1",rep("N/A",times = 9), "stage 1", "stage 2","stage 3","stage 4", "N/A"),
    "variableStartShortLabel" = c("time", rep("status", times = 3), rep("treatment", times = 3), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4), "example_der"),
    "variableStartLabel" = c("number of days between registration and the earlier of death, treatment or end of study", rep("status at end of study", times = 3), rep("treatment", times = 3), "age", rep("sex", times = 2), rep("prescence of ascites", times = 2), rep("prescence of hepatomegaly or enlarged liver", times = 2), rep("prescence of spiders", times = 2), rep("edema", times = 3), "bilirunbin concentration (blood)", "cholestral concentration (blood)", "albumin concentration (blood)", "copper concentration (urine)", "alkaline phosphotase concentration (blood)", " aspartate aminotransferase concentration (blood)", "triglycerides concentration (blood)", "platelet count", "standarized blood clotting time", rep("histologic stage of disease", times = 4), "example of dervived function: concentration of cholestral * concentration of bilirunbin"),
    "units" = c("days", rep("N/A", times = 6), "years", rep("N/A", times = 11), rep("mg/dl", times = 2), "g/dl", "ug/dl", rep("U/L", times = 2), "mg/dl", rep("N/A", times = 6), "mg/dl"),
    "notes" = rep("This is sample survival pbc data", times = 33)
  )
var_sheet <-
  data.frame(
    "variable" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage", "example_der"),
    "label" = c("time","status","treatment", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage", "example_der"),
    "labelLong" = c("number of days between registration and the earlier of death, treatment or end of study", "status at end of study", "treatment", "age", "sex", "prescence of ascites", "prescence of hepatomegaly or enlarged liver", "prescence of spiders", "edema", "bilirunbin concentration (blood)", "cholestral concentration (blood)", "albumin concentration (blood)", "copper concentration (urine)", "alkaline phosphotase concentration (blood)", " aspartate aminotransferase concentration (blood)", "triglycerides concentration (blood)", "platelet count", "standarized blood clotting time", "histologic stage of disease", "example of dervived function: concentration of cholestral * concentration of bilirunbin"),
    "subject" = c(rep("study",times = 3), rep("demographic",times = 2), rep("physical symptom",times = 4), rep("lab test",times = 10), "derived"),
    "section" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage", "example"),
    "variableType" = c("cont", "cat", "cat", "cont","cat", "cat", "cat","cat", "cat", rep("cont", times = 9), "cat", "cont"),
    "databaseStart" = rep("tester1, tester2", times = 20),
    "units" = c("days", rep("N/A", times = 2), "years", rep("N/A", times = 5), rep("mg/dl", times = 2), "g/dl", "ug/dl", rep("U/L", times = 2), "mg/dl", rep("N/A", times = 3), "mg/dl"),
    "variableStart" = c("[time]","[status]", "[trt]", "[age]", "[sex]", "[ascites]","[hepato]","[spiders]","[edema]", "[bili]", "[chol]", "[albumin]", "[copper]", "[alk.phos]", "[ast]", "[trig]", "[platelet]", "[protime]","[stage]", "[example_der]")
  )

library(survival)
tester1 <- survival::pbc[1:209,]
tester2 <- survival::pbc[210:418,]


