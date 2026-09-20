library(data.table)
all_dt <- fread("data/Laribi2024.csv")
ares <- atime::atime_versions(
  N=2^seq(3, 20),
  pkg.path = "~/R/mlr3resampling",
  setup={
    N_dt <- all_dt[seq_len(N)]
    train_task <- mlr3::as_task_classif(N_dt, target="target")
    train_task$col_roles$stratum <- "target"
    train_task$col_roles$group <- "groupID"
  },
  expr={
    cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
    cv$param_set$values$folds <- 4
    cv$param_set$values$group_stratum_algo <- "RSS"
    cv$instantiate(train_task)
  },
  seconds.limit=1,
  cpp="47213a1e0df2c5dd7cfc603be3fac29f91c61171",
  R="38b0c2ab61f091a0506eccffa5abd8954621c8d6")
plot(ares)

saveRDS(ares, "figure_atime_cpp_data.rds")
