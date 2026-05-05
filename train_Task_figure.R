library(data.table)
stats_dt <- fread("train_Task_stats.csv")
stats_long <- melt(stats_dt, measure.vars = c("RSS","mean.sd"), variable.name="metric")
compare_algos <- dcast(stats_long, seed + Sort + metric ~ algo)
compare_algos[Wasikowski<RSS]
compare_algos[Wasikowski==RSS]
compare_Sorts <- dcast(stats_long, seed + algo + metric ~ Sort)
compare_Sorts[neg.SD<RSS]
compare_Sorts[neg.SD==RSS]
compare_Sorts[neg.SD>RSS]

stats_long[, let(
  min.value = min(value),
  max.value = max(value),
  n.min = sum(min(value)==value),
  n=.N
), by=.(seed,metric)]
stats_long[n.min==1 & min.value==value]
stats_long[algo=="RSS" & Sort=="RSS" & value!=min.value]
stats_long[algo=="RSS" & Sort=="RSS"][, table(n.min)]

sbest <- stats_long[, .(
  min.value = min(value),
  max.value = max(value),
  n.min = sum(min(value)==value),
  n=.N,
  best=paste(paste(algo,Sort)[min(value)==value], collapse=", ")
), by=.(seed,metric)][order(n.min)]
print(sbest,nrow=200)
sbest[, .(seeds=.N), by=.(metric, best)]
ubest <- unique(sbest[,.(metric,min.value,max.value,best)])
ubest[, .(configurations=.N), by=.(metric, best)]
fbest <- sbest[ubest, on=.NATURAL, mult="first"]

counts_dt <- fread("train_Task_counts.csv")
counts_dt[unique(fbest[, .(seed)]), on=.NATURAL]

select.dt <- rowwiseDT(
  algo=, Sort=, seed=,
  "RSS","RSS",10,
  "Wasikowski","neg.SD",10)
selcount <- counts_dt[select.dt,on=.NATURAL]
split(selcount[,paste0("s",0:4),with=F], selcount$algo)
stats_dt[select.dt,on=.NATURAL][,.(RSS, mean.sd)]

train_dt <- fread("train_fold.csv")[, stratum := paste0("speed.",AdoptionSpeed)]
gtab <- train_dt[, table(RescuerID, AdoptionSpeed)]
gmat <- matrix(gtab,,ncol(gtab),dimnames=list(NULL,colnames(gtab)))
data.table(RescuerID=rownames(gtab), speed=gmat, sd=apply(gtab,1,sd))[order(-sd)]
(scounts <- train_dt[, table(fold, stratum)])
(ytab <- table(train_dt$stratum))
nfold <- nrow(scounts)
(ideal <- as.numeric(ytab/nfold))
rbind(total=ytab, ideal_per_fold=ideal)
t(t(scounts)-ideal)
