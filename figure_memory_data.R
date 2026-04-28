## https://github.com/tdhock/mlr3resampling/pull/89
remotes::install_github("tdhock/mlr3resampling@f1854c80247bfb5fdaa427edbf05917aa20dfaa9")
library(data.table)
library(ggplot2)
train_dt <- fread("train.csv")[order(RescuerID)]
train_dt[1]

zfac <- function(x)as.integer(factor(x))-1L
nfolds <- 5L
set.seed(123)
n = 1e7
sim_dt <- data.table(
  PID = sample(seq(1, n/10), n, replace = TRUE),   # ID for Grouping
  target = sample(seq(1,n/1000), n, replace=TRUE)
)[order(PID)]
alist.sim <- atime::atime(
  N=10^seq(1, 7, by=0.2),
  setup={
    Ndt <- sim_dt[(seq(1, N)-1L) %% .N + 1L][, let(
      group = zfac(PID),
      stratum=zfac(target),
      sfac=factor(target)
    )]
  },
  seconds.limit=0.1,
  kaggle=mlr3resampling:::stratified_group_cv_kaggle_interface(Ndt$stratum, Ndt$group, nfolds),
  new=Ndt[
  , v := var(table(sfac)), by=group
  ][order(-v, group), mlr3resampling:::stratified_group_cv_new_interface(stratum, cumsum(c(FALSE,diff(group)!=0)), nfolds)])

plot(alist.sim)

ntrain <- nrow(train_dt)
save(alist.sim, ntrain, file="figure_memory.RData")

