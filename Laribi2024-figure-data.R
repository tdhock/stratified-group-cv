library(data.table)
Laribi <- fread("data/Laribi2024.csv")[, let(
  t01 = target-1L,
  g = groupID-1L,
  d = c(NA,diff(groupID))
)][]
fun.list <- list()
algos <- c("RSS", "WasikowskiLimitedMemory", "Wasikowski")
for(algo in algos){
  fun.name <- sprintf("stratified_group_cv_%s_interface", algo)
  fun.list[[algo]] <- getFromNamespace(fun.name, "mlr3resampling")
}
(expr.list <- atime::atime_grid(
  list(algo=algos),
  expr.param.sep="\n",
  assign_folds={
    fold <- dt[, fun.list[[algo]](t01, g, N.folds)]
    data.frame(folds=max(fold)+1)
  }))

ares <- atime::atime(
  setup={
    N.folds <- N
    dt <- Laribi
  },
  seconds.limit=1,
  expr.list=expr.list)
plot(ares)

show.refs <- atime:::references_funs[c("N", "N^2")]
aref <- atime::references_best(ares, show.refs)
plot(aref)

pred <- predict(aref)
plot(pred)

vary.rows <- atime::atime(
  N=as.integer(10^seq(1, log10(nrow(Laribi)), length.out = 10)),
  setup={
    N.folds <- 10
    dt <- Laribi[1:N]
  },
  seconds.limit = 1,
  verbose=TRUE,
  expr.list=expr.list)
plot(vary.rows)

results <- list(
  folds=ares,
  rows=vary.rows)
saveRDS(results, "Laribi2024-figure-data.rds")
