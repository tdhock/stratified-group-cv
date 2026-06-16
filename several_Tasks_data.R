library(data.table)
RSS <- function(st, f){
  if(all(is.na(f)))return(c(RSS=NA, RMSE=NA, mean.sd=NA, zeros=NA))
  stab <- as.numeric(table(st))/folds
  cat(sprintf("data=%s folds=%d seed=%d algo=%s\n", data.name, folds, seed, algo))
  print(sftab <- table(st, f))
  dtab <- stab-sftab
  sq.err <- dtab^2
  c(RSS=sum(sq.err), RMSE=sqrt(mean(sq.err)), mean.sd=mean(apply(sftab,1,sd)), zeros=sum(sftab==0))
}
data.csv.vec <- Sys.glob("data/*.csv")
result.dt.list <- list()
for(data.i in seq_along(data.csv.vec)){
  data.csv <- data.csv.vec[[data.i]]
  data.name <- gsub("data/|.csv", "", data.csv)
  task_dt <- fread(data.csv)
  train_task <- mlr3::as_task_classif(task_dt, target="target")
  train_task$col_roles$stratum <- "target"
  train_task$col_roles$group <- "groupID"
  for(folds in 2:10)for(seed in 1:10)for(algo in c("random", "RSS", "Wasikowski", "origami")){
    set.seed(seed)
    fold.dt <- if(algo=="random"){
      unique(task_dt[, .(
        groupID
      )])[
      , fold := sample(rep(1:folds, length.out=.N))
      ][task_dt, on="groupID"]
    }else if(algo=="origami"){
      tryCatch({
        fold_list <- with(task_dt, origami::make_folds(
          cluster_ids=groupID,
          strata_ids=target,
          V=folds
        ))
        data.table(fold=seq_along(fold_list))[
        , .(row=fold_list[[fold]]$valid)
        , by=fold][order(row), data.table(fold, task_dt)]
      }, error=function(e){
        data.table(fold=NA_integer_, task_dt)
      })
    }else{
      cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
      cv$param_set$values$folds <- folds
      cv$param_set$values$group_stratum_algo <- algo
      cv$instantiate(train_task)
      cv$instance$fold.dt
    }
    result.dt.list[[paste(
      data.name, folds, seed, algo
    )]] <- fold.dt[, data.table(
      data.name, folds, seed, algo,
      t(RSS(target, fold))
    )]
  }
}

(result.dt <- rbindlist(result.dt.list))
fwrite(result.dt, "several_Tasks_data.csv")
result.dt[data.name=="respiratory" & folds==5]
result.dt[data.name=="PetAdoption" & folds==10]
result.dt[data.name=="five" & folds==4]
result.dt[data.name=="five" & folds==3]
result.dt[data.name=="five" & folds==2]
