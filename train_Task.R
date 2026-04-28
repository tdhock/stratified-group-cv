library(data.table)
train_dt <- fread("train.csv")
train_dt[1]
train_task <- mlr3::as_task_classif(train_dt, target="AdoptionSpeed")
train_task$col_roles$stratum <- "AdoptionSpeed"
train_task$col_roles$group <- "RescuerID"
cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
cv$instantiate(train_task)
cv$instance$fold.dt[, table(stratum, fold)]
