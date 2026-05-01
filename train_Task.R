library(data.table)
train_dt <- fread("train_fold.csv")
train_dt[1]
train_task <- mlr3::as_task_classif(train_dt, target="AdoptionSpeed")
train_task$col_roles$stratum <- "AdoptionSpeed"
train_task$col_roles$group <- "RescuerID"
cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
folds <- length(unique(train_dt$fold))
cv$param_set$values$folds <- folds
cv$instantiate(train_task)
RSS <- function(st, f){
  stab <- as.numeric(table(st))/folds
  sftab <- table(st, f)
  dtab <- stab-sftab
  c(RSS=sum(dtab^2), mean.sd=mean(apply(sftab,1,sd)))
}
rbind(
  R=cv$instance$fold.dt[, RSS(stratum, fold)],
  py=train_dt[, RSS(AdoptionSpeed, fold)])

res_list <- list()
for(algo in c("RSS", "Wasikowski", "WasikowskiLinearMemory")){
  fun_name <- paste0("stratified_group_cv_", algo, "_interface")
  fun <- getFromNamespace(fun_name, "mlr3resampling")
  fold.vec <- cv$instance$group.row.dt[, fun(stratum-1L, cumsum(c(FALSE, diff(g_ord)!=0)), folds)]+1L
  res_list[[algo]] <- RSS(cv$instance$group.row.dt$stratum, fold.vec)
}
do.call(rbind, res_list)
