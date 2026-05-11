library(data.table)
all_dt <- nc::capture_first_glob(
  "data/*.csv",
  "data/",
  data.name=".*?",
  ".csv")

meta.dt <- all_dt[, {
  gtab <- table(groupID)
  data.table(
    strata=length(unique(target)),
    rows=.N,
    groups=length(gtab),
    min.rows.per.group=min(gtab),
    max.rows.per.group=max(gtab))
}, by=data.name]
fwrite(meta.dt, "data_meta.csv")
