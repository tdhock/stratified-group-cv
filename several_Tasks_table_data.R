library(data.table)
(data.csv.vec <- Sys.glob("data/*.csv"))
list2dt <- function(fold_list, set_name){
  data.table(fold=seq_along(fold_list))[, {
    task_row <- fold_list[[fold]][[set_name]]
    data.table(task_row, task_dt[task_row])
  }, by=fold]
}
result.dt.list <- list()
data.i <- 2
folds=4
algo="RSS"
for(data.i in seq_along(data.csv.vec)){
  data.csv <- data.csv.vec[[data.i]]
  data.name <- gsub("data/|.csv", "", data.csv)
  task_dt <- fread(data.csv)
  train_task <- mlr3::as_task_classif(task_dt, target="target")
  train_task$col_roles$stratum <- "target"
  train_task$col_roles$group <- "groupID"
  for(folds in 4)for(seed in 1)for(algo in c("random", "RSS", "Wasikowski")){
    cat(sprintf("data=%s folds=%d seed=%d algo=%s\n", data.name, folds, seed, algo))
    set.seed(seed)
    mult.dt <- task_dt[, .(
      rows=.N
    ), by=.(groupID,target)][, .(
      targets=.N
    ), by=groupID][targets>1]
    fold.dt <- if(algo=="random"){
      unique(task_dt[, .(
        groupID
      )])[
      , fold := sample(rep(1:folds, length.out=.N))
      ][task_dt, on="groupID"]
    }else if(algo=="bioLeak"){
      lobj <- bioLeak::make_split_plan(
        task_dt,
        outcome = "target",
        mode = "subject_grouped",
        group = "groupID",
        v = folds,
        stratify=TRUE,
        seed = seed)
      list2dt(lobj@indices, "test")
    }else if(algo=="rsample"){
      if(nrow(mult.dt)){
        data.table(fold=NA_integer_, task_dt)
      }else{
        rtib <- rsample::group_vfold_cv(
          task_dt,
          group="groupID",
          v=folds,
          strata="target",
          balance="observations")#or groups
        for(split_i in 1:nrow(rtib)){
          rtib$splits[[split_i]][["out_id"]] <- setdiff(
            1:nrow(task_dt),
            rtib$splits[[split_i]][["in_id"]])
        }
        list2dt(rtib$splits, "out_id")
      }
    }else if(algo=="origami"){
      if(nrow(mult.dt)){
        data.table(fold=NA_integer_, task_dt)
      }else{
        fold_list <- with(task_dt, origami::make_folds(
          cluster_ids=groupID,
          strata_ids=target,
          V=folds
        ))
        list2dt(fold_list, "validation_set")
      }
    }else{
      cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
      cv$param_set$values$folds <- folds
      cv$param_set$values$group_stratum_algo <- algo
      cv$instantiate(train_task)
      cv$instance$fold.dt[, .(groupID=group, target, fold)]
    }
    rows.per.group.fold <- fold.dt[, .(rows=.N), by=.(groupID,fold)]
    bad.dt <- rows.per.group.fold[, .(
      folds=.N
    ), by=groupID][folds>1]
    rows.per.group.fold[bad.dt, on="groupID"]
    result.dt.list[[paste(
      data.name, folds, seed, algo
    )]] <- fold.dt[, data.table(
      data.name, folds, seed, algo, rows=.N
    ), by=.(stratum=target, fold)]
  }
}
(result.dt <- rbindlist(result.dt.list))
setkey(result.dt, data.name, algo, stratum, fold)
fwrite(result.dt, "several_Tasks_table_data.csv")
