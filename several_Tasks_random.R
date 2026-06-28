library(data.table)
library(ggplot2)
data.csv.vec <- Sys.glob("data/Laribi2024.csv")
data.csv.vec <- grep("Laribi", Sys.glob("data/*.csv"), value=TRUE, invert=TRUE)
list2dt <- function(fold_list, set_name){
  data.table(fold=seq_along(fold_list))[, {
    task_row <- fold_list[[fold]][[set_name]]
    data.table(task_row, task_dt[task_row])
  }, by=fold]
}
for(data.i in seq_along(data.csv.vec)){
  seg.dt.list <- list()
  lab.dt.list <- list()
  g.dt.list <- list()
  data.csv <- data.csv.vec[[data.i]]
  data.name <- gsub("data/|.csv", "", data.csv)
  task_dt <- fread(data.csv)
  train_task <- mlr3::as_task_classif(task_dt, target="target")
  train_task$col_roles$stratum <- "target"
  train_task$col_roles$group <- "groupID"
  folds <- 3
  fold.dt.list <- list()
  for(algo in c("random", "RSS", "Wasikowski", "origami", "rsample","bioLeak")){
    for(seed in 1:2){
      set.seed(seed)
      mult.dt <- task_dt[, .(
        rows=.N
      ), by=.(groupID,target)][, .(
        targets=.N
      ), by=groupID][targets>1]
      fold.dt.list[[seed]] <- data.table(seed, if(algo=="random"){
        unique(task_dt[, .(
          groupID
        )])[
        , fold := sample(rep(1:folds, length.out=.N))
        ][task_dt, on="groupID"]
      }else if(algo=="bioLeak"){
        lobj <- bioLeak::make_split_plan(
          task_dt,
          outcome = "target",
          mode = "subject_grouped",
          group = "groupID",
          v = folds,
          stratify=TRUE,
          seed = seed)
        list2dt(lobj@indices, "test")
      }else if(algo=="rsample"){
        if(nrow(mult.dt)){
          data.table(fold=NA_integer_, task_dt)
        }else{
          rtib <- rsample::group_vfold_cv(
            task_dt,
            group="groupID",
            v=folds,
            strata="target",
            balance="observations")#or groups
          for(split_i in 1:nrow(rtib)){
            rtib$splits[[split_i]][["out_id"]] <- setdiff(
              1:nrow(task_dt),
              rtib$splits[[split_i]][["in_id"]])
          }
          list2dt(rtib$splits, "out_id")
        }
      }else if(algo=="origami"){
        if(nrow(mult.dt)){
          data.table(fold=NA_integer_, task_dt)
        }else{
          fold_list <- with(task_dt, origami::make_folds(
            cluster_ids=groupID,
            strata_ids=target,
            V=folds
          ))
          list2dt(fold_list, "validation_set")
        }
      }else{
        cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
        cv$param_set$values$folds <- folds
        cv$param_set$values$group_stratum_algo <- algo
        cv$instantiate(train_task)
        cv$instance$fold.dt[, .(groupID=group, target, fold)]
      })
    }
    (fold.dt <- rbindlist(fold.dt.list, use.names=TRUE))
    group.dt <- dcast(fold.dt, seed + fold ~ groupID, any, value.var="seed")[, let(
      label = sprintf("f%d s%s", fold, seed)
    )][]
    group.long <- melt(group.dt, measure.vars=patterns("[0-9]"))
    group.mat <- as.matrix(group.dt[, .SD, .SDcols=patterns("[0-9]")])
    rownames(group.mat) <- group.dt$label
    dmat <- dist(group.mat)
    hcl <- hclust(dmat)
    dlist <- ggdendro::dendro_data(hcl)
    ldt <- data.table(algo, dlist$labels)
    lab.dt.list[[algo]] <- ldt
    seg.dt.list[[algo]] <- data.table(algo, dlist$segments)
    g.dt.list[[algo]] <- data.table(algo, group.long)[ldt, on=.NATURAL]
  }
  seg.dt <- rbindlist(seg.dt.list)
  lab.dt <- rbindlist(lab.dt.list)
  g.dt <- rbindlist(g.dt.list)[, let(
    pair.i = (x+1) %/% 2,
    p01 = paste0("seed",(x+1) %% 2)
  )][]
  g.wide <- dcast(g.dt, algo + pair.i + variable ~ p01)[, same := seed0==seed1]
  g.all <- g.dt[
    value==TRUE,
    .(pairs=length(unique(pair.i))),
    by=.(algo, variable)
  ][
  , pair := ifelse(pairs==1, "same", "different")
  ][]
  tile.dt <- g.dt[g.all, on=.NATURAL]
  pair.dt <- g.wide[, .(
    prop.same=sum(same, na.rm=TRUE)/.N,
    nrow=.N
  ), by=.(algo,pair.i)]
  all.pair.dt <- g.all[, .(
    prop.same=sum(pair=="same")/.N,
    nrow=.N
  ), by=algo]
  if(TRUE){
    gg <- ggplot()+
      ggtitle(sprintf("%d groups in %s", all.pair.dt$nrow[1], data.name))+
      theme_bw()+
      facet_grid(algo ~ facet, scales="free_x")+
      geom_segment(aes(
        y, x,
        xend=yend,
        yend=xend),
        data=data.table(facet="tree", seg.dt))+
      geom_text(aes(
        -Inf, x, label=label),
        hjust=0,
        data=data.table(facet="tree", lab.dt))+
      geom_tile(aes(
        as.integer(variable), x, fill=value, color=pair),
        data=data.table(facet="groups", tile.dt[value==TRUE]))+
      ## geom_text(aes(
      ##   Inf, pair.i*2, label=sprintf("%.1f%% same in pair %d", prop.same*100, pair.i)),
      ##   hjust=1,
      ##   vjust=1,
      ##   data=data.table(facet="groups", pair.dt))+
      geom_text(aes(
        -Inf, Inf, label=sprintf(" %.1f%% groups in same pair", prop.same*100)),
        hjust=0,
        vjust=1.1,
        data=data.table(facet="groups", all.pair.dt))+
      scale_fill_manual(
        "present",
        values=c(
          "FALSE"=NA,
          "TRUE"="orange"))+
      scale_color_manual(
        guide=guide_legend(override.aes=list(linewidth=1, fill="orange")),
        values=c(
          "different"=NA,
          "same"="grey50"))+
      ylab("fold and seed")+
      xlab("")
    data.png <- sprintf("several_Tasks_random_%s.png", data.name)
    png(data.png, width=10, height=6, units="in", res=200)
    print(gg)
    dev.off()
  }
}
