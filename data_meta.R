library(data.table)
all_dt <- nc::capture_first_glob(
  "data/*.csv",
  "data/",
  data.name=".*?",
  ".csv")

meta.dt <- all_dt[, {
  gtab <- table(groupID)
  strat.dt <- .SD[, .(
    rows=.N
  ), by=.(groupID,target)][, .(
    strata=.N
  ), by=groupID]
  data.table(
    strata=length(unique(target)),
    rows=.N,
    groups=length(gtab),
    min.strata.per.group=min(strat.dt$strata),
    max.strata.per.group=max(strat.dt$strata),
    min.rows.per.group=min(gtab),
    max.rows.per.group=max(gtab))
}, by=data.name][order(-rows)]
comma <- function(x)data.table(x, chr=format(x, big.mark=",", scientific=FALSE, trim=TRUE))[, factor(chr, unique(chr[order(x)]))]
meta.dt[, let(
  Rows = comma(rows),
  Groups = comma(groups)
)]
for(numerator in c("Rows", "Strata")){
  mlist <- list()
  for(stat in c("min","max")){
    mlist[[stat]] <- meta.dt[[sprintf(
      "%s.%s.per.group", stat, tolower(numerator)
    )]]
  }
  set(
    meta.dt,
    j=sprintf("%s/Group", numerator),
    value=with(mlist, ifelse(
      min==max, min,
      sprintf("%s–%s", min, comma(max)))))
}
fwrite(meta.dt, "data_meta.csv")

library(xtable)
xt <- xtable(meta.dt[, .(
  Data=data.name, 
  Rows, Groups, `Rows/Group`, Strata=strata, `Strata/Group`
)])
print(xt, include.rownames=FALSE)
