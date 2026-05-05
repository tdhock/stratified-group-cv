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
  print(sftab <- table(st, f))
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


ytab <- as.numeric(table(train_dt$AdoptionSpeed))/folds
gtab <- train_dt[, table(RescuerID, AdoptionSpeed)]
sort_by_list <- list(
  neg.SD = -apply(gtab, 1, sd),
  RSS = colSums((t(gtab)-ytab)^2))
seed_counts_list <- list()
seed_stats_list <- list()
for(algo in c("RSS", "Wasikowski")){
  fun_name <- paste0("stratified_group_cv_", algo, "_interface")
  fun <- getFromNamespace(fun_name, "mlr3resampling")
  for(seed in 1:100){
    set.seed(seed)
    rand_ord <- sample(nrow(gtab))
    for(Sort in names(sort_by_list)){
      sort_vec <- sort_by_list[[Sort]]
      group.dt <- setkey(data.table(
        sort_vec, rand_ord, group=rownames(gtab)
      ))[, g_input := .I-1L][]
      cpp_in <- train_dt[, .(stratum=AdoptionSpeed, group=RescuerID)][group.dt, on="group"]
      cpp_in[, fold := fun(stratum, g_input, folds)+1L]
      stat.dt <- as.data.table(t(cpp_in[, RSS(stratum, fold)]))
      counts_per_fold <- dcast(cpp_in[, Stratum := paste0("s",stratum)], fold ~ Stratum, length)
      seed_counts_list[[paste(algo, seed, Sort)]] <- data.table(algo, seed, Sort, counts_per_fold)
      seed_stats_list[[paste(algo, seed, Sort)]] <- data.table(algo, seed, Sort, stat.dt)
    }
  }
}
seed_counts <- rbindlist(seed_counts_list)
seed_stats <- rbindlist(seed_stats_list)

fwrite(seed_counts, "train_Task_counts.csv")
fwrite(seed_stats, "train_Task_stats.csv")
