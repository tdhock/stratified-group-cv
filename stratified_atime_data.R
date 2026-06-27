remotes::install_github("tdhock/mlr3resampling")
reticulate::conda_list()
reticulate::use_condaenv("2023-08-deep-learning")
library(data.table)
library(ggplot2)
train_dt <- fread("data/Laribi2024.csv")
train_dt[1]
reticulate::py_run_string("from stratified_group import stratified_group_k_fold, for_split")
main <- reticulate::import_main()

zfac <- function(x)as.integer(factor(x))-1L
main$nfolds <- nfolds <- 5L
cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
cv$param_set$values$folds <- nfolds
(Nvec=unique(as.integer(10^seq(1, log10(nrow(train_dt)), length.out=20))))
alist <- atime::atime(
  N=Nvec,
  times=10,
  seconds.limit=Inf,
  setup={
    Ndt <- train_dt[(seq(1, N)-1L) %% .N + 1L][, let(
      group = zfac(groupID),
      stratum=zfac(target)
    )]
    Ntask = mlr3::as_task_classif(Ndt, target="target")
    Ntask$col_roles$stratum = "target"
    Ntask$col_roles$group = "groupID"
    main$Ndf <- Ndt
    set.seed(1)
    Ndt[, let(
      random_order = sample(.N),
      stratum_fac = factor(stratum)
    )]
  },
  verbose=TRUE,
  "R+cpp Wasikowski"={
    cv$param_set$values$group_stratum_algo <- "Wasikowski"
    cv$instantiate(Ntask)
  },
  "R+cpp RSS"={
    cv$param_set$values$group_stratum_algo <- "RSS"
    cv$instantiate(Ntask)
  },
  "R+cpp sort+assign"={
    Ndt[, let(
      neg_sd = -sd(table(stratum_fac)),
      g_ord = min(random_order)
    ), by=group][
      order(neg_sd, g_ord), 
      mlr3resampling:::stratified_group_cv_Wasikowski_interface(
        stratum, cumsum(c(FALSE, diff(g_ord)!=0)), nfolds
      )+1L]
  },
  "cpp assign RSS"=mlr3resampling:::stratified_group_cv_RSS_interface(Ndt$stratum, Ndt$group, nfolds),
  "cpp assign Wasikowski"=mlr3resampling:::stratified_group_cv_Wasikowski_interface(Ndt$stratum, Ndt$group, nfolds),
  "Python full Kaggle"=reticulate::py_run_string("stratified_group_k_fold(Ndf.stratum.values, Ndf.group.values, nfolds)"),
  "Python full sklearn"=reticulate::py_run_string("for_split(Ndf.stratum.values, Ndf.group.values)"),
  "Python overhead"=reticulate::py_run_string("None"))
plot(alist)

ntrain <- nrow(train_dt)
save(alist, ntrain, file="stratified_atime.RData")

