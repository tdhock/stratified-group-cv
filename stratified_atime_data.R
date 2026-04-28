remotes::install_github("tdhock/mlr3resampling@3d6980138432e2b0ddf2557161378b757c490fe5")
reticulate::use_condaenv("2023-08-deep-learning")
library(data.table)
library(ggplot2)
train_dt <- fread("train.csv")[order(RescuerID)]
train_dt[1]
reticulate::py_run_string("from stratified_group import stratified_group_k_fold")
main <- reticulate::import_main()

zfac <- function(x)as.integer(factor(x))-1L
main$nfolds <- nfolds <- 5L
alist <- atime::atime(
  N=10^seq(1, 6, by=0.2),
  setup={
    Ndt <- train_dt[(seq(1, N)-1L) %% .N + 1L][, let(
      group = zfac(RescuerID),
      stratum=zfac(AdoptionSpeed)
    )]
    main$Ndf <- Ndt
  },
  seconds.limit=0.1,
  cpp=mlr3resampling:::stratified_group_cv_interface(Ndt$stratum, Ndt$group, nfolds),
  py=reticulate::py_run_string("stratified_group_k_fold(Ndf.stratum.values, Ndf.group.values, nfolds)"),
  None=reticulate::py_run_string("None"))

set.seed(123)
n = 1e7
sim_dt <- data.table(
  PID = sample(seq(1, n/10), n, replace = TRUE),   # ID for Grouping
  target = ifelse(rbinom(n, size = 1, prob = 0.2) == 1, "A", "B")
)[order(PID)]
alist.sim <- atime::atime(
  N=10^seq(1, 7, by=0.2),
  setup={
    Ndt <- sim_dt[(seq(1, N)-1L) %% .N + 1L][, let(
      group = zfac(PID),
      stratum=zfac(target)
    )]
    main$Ndf <- Ndt
  },
  seconds.limit=0.1,
  cpp=mlr3resampling:::stratified_group_cv_interface(Ndt$stratum, Ndt$group, nfolds),
  py=reticulate::py_run_string("stratified_group_k_fold(Ndf.stratum.values, Ndf.group.values, nfolds)"),
  None=reticulate::py_run_string("None"))

ntrain <- nrow(train_dt)
save(alist, alist.sim, ntrain, file="stratified_atime.RData")

