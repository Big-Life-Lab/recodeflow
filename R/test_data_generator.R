var_details <-
  data.frame(
    "variable" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
    "dummyVariable" = c("NA", "status0", "status1","status2", "trt1","trt2","NA","sexM","sexF", "ascites0", "ascites1","hepato0","hepato1","spiders0","spiders1","edema0.0","edema0.5","edema1.0",rep("NA",times = 9), "stage1", "stage2","stage3","stage4"),
    "toType" = c("cont", rep("cat", times = 3), rep("cat", times = 2), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4)),
    "databaseStart" = rep("tester", times = 31),
    "variableStart" = c("tester::time", rep("tester::status", times = 3), rep("tester::trt", times = 2), "tester::age", rep("tester::sex", times = 2), rep("tester::ascites", times = 2), rep("tester::hepato", times = 2), rep("tester::spiders", times = 2), rep("tester::edema", times = 3), "tester::bili", "tester::chol", "tester::albumin", "tester::copper", "tester::alk.phos", "tester::ast", "tester::trig", "tester::platelet", "tester::protime", rep("tester::stage", times = 4)),
    "fromType" = c("cont", rep("cat", times = 3), rep("cat", times = 2), "cont", rep("cat", times = 2), rep("cat", times = 2), rep("cat", times = 2),rep("cat", times = 2), rep("cat", times = 3), rep("cont", times = 9), rep("cat", times = 4)),
    "recTo" = c("copy", "0", "1","2", "1","2","copy","M","F", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("copy",times = 9), "1", "2","3","4"),
    "catLabel" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex M","sex F", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
    "catLabelLong" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex M","sex F", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
    "recFrom" = c("else", "0", "1","2", "1","2","else","M","F", "0", "1","0","1","0","1","0.0","0.5","1.0",rep("else",times = 9), "1", "2","3","4"),
    "catStartLabel" = c("", "status 0", "status 1","status 2", "trt 1","trt 2","","sex M","sex F", "ascites 0", "ascites 1","hepato 0","hepato 1","spiders 0","spiders 1","edema 0.0","edema 0.5","edema 1.0",rep("",times = 9), "stage 1", "stage 2","stage 3","stage 4"),
    "variableStartShortLabel" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
    "variableStartLabel" = c("time", rep("status", times = 3), rep("trt", times = 2), "age", rep("sex", times = 2), rep("ascites", times = 2), rep("hepato", times = 2), rep("spiders", times = 2), rep("edema", times = 3), "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", rep("stage", times = 4)),
    "units" = rep("NA", times = 31),
    "notes" = rep("This is sample survival pbc data", times = 31)
  )
var_sheet <-
  data.frame(
    "variable" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
    "label" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
    "labelLong" = c("time","status","trt", "age","sex","ascites","hepato", "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet", "protime", "stage"),
    "section" = rep("tester", times=19),
    "subject" = rep("tester",times = 19),
    "variableType" = c("cont", "cat", "cat", "cont","cat", "cat", "cat","cat", "cat", rep("cont", times = 9), "cat"),
    "databaseStart" = rep("tester", times = 19),
    "units" = rep("NA", times = 19),
    "variableStart" = c("tester::time","tester::status", "tester::trt", "tester::age", "tester::sex", "tester::ascites","tester::hepato","tester::spiders","tester::edema", "tester::bili", "tester::chol", "tester::albumin", "tester::copper", "tester::alk.phos", "tester::ast", "tester::trig", "tester::platelet", "tester::protime","tester::stage")
  )

sample_data <-
  data.frame(
    "startA" = sample(1:3, 100, replace = TRUE),
    "startB" = sample(1:3, 100, replace = TRUE),
    "startC" = sample(1:3, 100, replace = TRUE)
  )
