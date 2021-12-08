rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(plyr)
library(tidyverse)
library(ranger)


for (d in c(30, 90)) {
  for (i in 0:4) {
    train.raw = read.csv(paste0("train4_", d, "_raw", i, ".csv"))
    test.raw = read.csv(paste0("test4_", d, "_raw", i, ".csv"))
    
    train = read.csv(paste0("train4_", d, "_imp", i, ".csv"))
    test = read.csv(paste0("test4_", d, "_imp", i, ".csv"))
    
    assign(paste0("train.raw", d, "_", i), train.raw, envir = globalenv())
    assign(paste0("test.raw", d, "_", i), test.raw, envir = globalenv())
    
    assign(paste0("train", d, "_", i), train, envir = globalenv())
    assign(paste0("test", d, "_", i), test, envir = globalenv())
  }
}

importance30 = data.frame()
importance90 = data.frame()
for (d in c(30, 90)) {
  for (i in 0:4) {
    train = get(paste0("train", d, "_", i), envir = globalenv())
    
    set.seed(0)
    if (d == 30) {
      rg = ranger(ThirtyMo ~ ., data = train,
                  classification = T,
                  importance = "impurity",
                  always.split.variables = c("creatinine_value_mg_dl", "bilirubin_value_mg_dl", "sodium", "inr"))
      importance = rg$variable.importance
      
      importance30 = bind_rows(importance30, importance)
    } else {
      rg = ranger(NinetyMo ~ ., data = train,
                  classification = T,
                  importance = "impurity",
                  always.split.variables = c("creatinine_value_mg_dl", "bilirubin_value_mg_dl", "sodium", "inr"))
      importance = rg$variable.importance
      
      importance90 = bind_rows(importance90, importance)
    }
    
    assign(paste0("importance", d, "_", i), importance, envir = globalenv())
  }
}

for (d in c(30, 90)) {
  for (i in 0:4) {
    importance = get(paste0("importance", d, "_", i), envir = globalenv())
    train = get(paste0("train", d, "_", i), envir = globalenv())
    test = get(paste0("test", d, "_", i), envir = globalenv())
    
    importance = importance[order(importance, decreasing = T)]
    importance.sel = importance[1:11]
    
    if (d == 30) {
      train.sel = train[c(names(importance.sel), "ThirtyMo")]
      test.sel = test[c(names(importance.sel), "ThirtyMo")]
    } else {
      train.sel = train[c(names(importance.sel), "NinetyMo")]
      test.sel = test[c(names(importance.sel), "NinetyMo")]
    }
    
    assign(paste0("train.sel", d, "_", i), train.sel, envir = globalenv())
    assign(paste0("test.sel", d, "_", i), test.sel, envir = globalenv())
  }
}

for (d in c(30, 90)) {
  for (i in 0:4) {
    train.raw = get(paste0("train.raw", d, "_", i), envir = globalenv())
    test.raw = get(paste0("test.raw", d, "_", i), envir = globalenv())
    
    train.sel = get(paste0("train.sel", d, "_", i), envir = globalenv())
    test.sel = get(paste0("test.sel", d, "_", i), envir = globalenv())
    
    write_csv(train.raw, paste0("train4_", d, "_raw", i, ".csv"))
    write_csv(test.raw, paste0("test4_", d, "_raw", i, ".csv"))
    
    write_csv(train.sel, paste0("train4_", d, "_sel", i, ".csv"))
    write_csv(test.sel, paste0("test4_", d, "_", i, ".csv"))
  }
}

importance30 = bind_rows(importance30, colMeans(importance30))
rownames(importance30)[nrow(importance30)] = "Mean"
importance30 = importance30[order(importance30[nrow(importance30), ], decreasing = T)]
importance30.sel = importance30[, 1:11]

importance90 = bind_rows(importance90, colMeans(importance90))
rownames(importance90)[nrow(importance90)] = "Mean"
importance90 = importance90[order(importance90[nrow(importance90), ], decreasing = T)]
importance90.sel = importance90[, 1:11]

