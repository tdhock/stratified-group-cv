library(data.table)
train_dt <- fread("train_fold.csv")
train_dt[, table(fold, AdoptionSpeed)]
(count_dt <- train_dt[, .(N=.N), by=.(RescuerID, fold)][order(N)])
count_dt[, .(folds=.N), by=RescuerID][, table(folds)]
