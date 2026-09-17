data(AZtrees, package="mlr3resampling")
library(data.table)
options(datatable.print.keys=FALSE, datatable.print.class=FALSE)
AZtrees[, .(region3, polygon, y)]
compare.list <- list()
trees <- function(..., cv=NULL, folds=3L){
  trees_task <- mlr3::as_task_classif(AZtrees, target="y")
  role_list <- list(...)
  for(role in names(role_list)){
    trees_task$col_roles[[role]] <- role_list[[role]]
  }
  if(is.null(cv))cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  cv$param_set$values$folds <- folds
  cv$instantiate(trees_task)
  role_dt <- data.table(AZtrees)
  for(fold in 1:folds){
    set(role_dt, cv$test_set(fold), "fold", fold)
  }
  setkey(role_dt, polygon, fold)
  in_list <- list(
    rows=role_dt,
    groups=role_dt[, {
      ufold <- sort(unique(fold))
      data.table(
        fold=if(length(ufold)==1){
          paste(ufold)
        }else if(all(diff(ufold)==1)){
          sprintf("%d–%d", min(ufold), max(ufold))
        }else{
          paste(ufold, collapse=",")
        }
      )
    }, by=.(y,polygon)])
  out_list <- list()
  for(unit in names(in_list)){
    in_dt <- in_list[[unit]]
    otab <- rbind(in_dt[, table(y, fold)], TOTAL=in_dt[, table(fold)])
    if(folds==3){
      otab <- otab[, intersect(c("1","2","3","1–2", "2–3", "1,3", "1–3"),colnames(otab))]
    }
    out_list[[unit]] <- otab
  }
  role_list$cv <- cv$id
  role_list$folds <- folds
  default_names <- c("stratum", "group")
  role_list[setdiff(default_names, names(role_list))] <- NA
  role_list <- role_list[order(names(role_list))]
  cname <- paste(
    sprintf("%s=%s", names(role_list), role_list),
    collapse=", "
  )
  compare.list[[cname]] <<- do.call(cbind, out_list)
  out_list
}
set.seed(1)
trees()
trees()
trees(cv=mlr3::rsmp("cv"))
trees(stratum="y")
trees(stratum="y", cv=mlr3::rsmp("cv"))
trees(group="polygon")
trees(group="polygon")
trees(group="polygon", cv=mlr3::rsmp("cv"))
more.folds <- 5
trees(group="polygon", folds=more.folds)
trees(group="polygon", folds=more.folds, cv=mlr3::rsmp("cv"))
trees(stratum="y", group="polygon")
trees(stratum="y", group="polygon")
compare.list[order(gsub("[^0-9]", "", names(compare.list)))]
show.names <- c(
  `cv=cv, folds=3, group=NA, stratum=NA`="Trivial algorithm, neither stratification nor groups",
  `cv=cv, folds=3, group=NA, stratum=y`="Trivial algorithm, stratification (no groups)",
  `cv=cv, folds=3, group=polygon, stratum=NA`="Trivial algorithm, grouped data (no stratification)",
  `cv=same_other_sizes_cv, folds=3, group=polygon, stratum=NA`="Proposed algorithm, grouped data (no stratification)",
  `cv=same_other_sizes_cv, folds=3, group=polygon, stratum=y`="Proposed algorithm, groups and stratification")
(show.list <- setNames(compare.list[names(show.names)], show.names))
library(xtable)
in.tab <- do.call(rbind, c(lapply(show.list, data.table), fill=TRUE))
xt <- xtable(in.tab)
print(xt, include.rownames=FALSE)
for(tab.i in seq_along(show.list)){
  in.tab <- show.list[[tab.i]]
  xt <- xtable(in.tab)
}
