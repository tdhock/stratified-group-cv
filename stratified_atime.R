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
alist$unit.col.vec <- c(seconds="median")
plot(alist)

arefs <- atime::references_best(alist)
plot(arefs)

apred <- predict(arefs, seconds=0.01)
overhead <- geom_text(aes(
  x=Inf, y=5e-5, label="R-Python overhead<1ms"),
  hjust=1)
xlog <- scale_x_log10("N = number of rows input to fold generation function")
ylog <- scale_y_log10("Time to compute folds with group+strata constraint\nmedian line, min/max band")
(gg <- plot(apred)+
   geom_vline(color="grey",xintercept=nrow(train_dt))+
   ggtitle("Kaggle pet adoption data, 5-fold CV with group+strata constraint")+
   overhead+
   facet_null()+
   ylog+
   geom_text(aes(
     x=nrow(train_dt), y=0,
     label=sprintf("rows in CSV=%d", nrow(train_dt))),
     hjust=1, vjust=-0.5, color="grey50")+
   xlog)

png("stratified_atime_kaggle.png", width=6, height=6, units="in", res=200)
print(gg)
dev.off()

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
plot(alist.sim)

arefs.sim <- atime::references_best(alist.sim)
plot(arefs.sim)

apred.sim <- predict(arefs.sim)
(gg <- plot(apred.sim)+
   ggtitle("Simulated data, 5-fold CV with group+strata constraint")+
   overhead+
   facet_null()+
   ylog+
   xlog+
   overhead)

png("stratified_atime_sim.png", width=6, height=6, units="in", res=200)
print(gg)
dev.off()
