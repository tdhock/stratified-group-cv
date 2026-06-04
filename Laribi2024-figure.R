library(data.table)
library(ggplot2)
atime::references_best
results <- readRDS("Laribi2024-figure-data.rds")

show.refs <- atime:::references_funs[c("N", "N^2")]
show.folds <- results$folds
show.folds$measurements <- show.folds$measurements[
  algo %in% c("Wasikowski", "RSS")
][, algorithm := ifelse(algo=="RSS", "RSS (proposed)", "Wasikowski (previous)")][]
show.folds$by.vec <- c("algorithm", results$folds$by.vec)
show.folds$unit.col.vec <- c(seconds="median")
aref <- atime::references_best(show.folds, show.refs)
gg <- plot(aref)+
  facet_null()+
  ## facet_grid(
  ##   . ~ algorithm,
  ##   scales="free",
  ##   space="free",
  ##   labeller=label_both)+
  scale_x_log10(breaks=2^seq(1,20))

ref.dt <- aref[["plot.references"]]
ref.color <- "grey"
meas <- aref$meas
pred <- predict(aref)
gg <- ggplot2::ggplot()+
  ##ggplot2::facet_grid(unit ~ expr.name, scales="free")+
  ggplot2::theme_bw()
if(nrow(ref.dt[unit=="seconds"]) || nrow(meas[unit=="seconds"])){
  hline.df <- with(aref, data.frame(seconds.limit, unit="seconds"))
  gg <- gg+
    ggplot2::geom_text(ggplot2::aes(
      0, seconds.limit, label=" 1 second"),
      color="grey50",
      hjust=0,
      vjust=-0.5,
      data=hline.df)+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)
}
max.N <- max(results$rows$measurements$N)
gg <- gg+
  theme(legend.position="none")+
  ggplot2::geom_ribbon(ggplot2::aes(
    N, ymin=min, ymax=max, group=expr.name, fill=algorithm),
    data=meas[unit=="seconds"],
    alpha=0.5)+
  ggplot2::geom_line(ggplot2::aes(
    N, empirical, group=expr.name, color=algorithm),
    linewidth=2,
    data=meas)+
  ggplot2::geom_line(ggplot2::aes(
    N, reference, group=paste(expr.name, fun.name)),
    color=ref.color,
    linewidth=1,
    data=ref.dt)+
  ggplot2::scale_y_log10(
    sprintf(
      "Seconds to compute fold assignment in N=%s data\n(Median line and min/max band over 10 timings)",
      format(max.N, big.mark=",", scientific=FALSE, trim=TRUE)))+
  coord_cartesian(ylim=c(0.01, 5))
gg.png <- gg+
  ggplot2::scale_x_log10("K = number of folds")+
  directlabels::geom_dl(ggplot2::aes(
    N, unit.value, color=algorithm, label=sprintf("%s\nK=%d @ 1 sec", algorithm, as.integer(N))),
    data=pred$prediction,
    method=directlabels::polygon.method("top", offset.cm=1))+
  directlabels::geom_dl(ggplot2::aes(
    N, reference, label=sub("N", "K", fun.name), label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color=ref.color,
    method="bottom.polygons")
png("Laribi2024-figure-refs.png", width=8, height=4, units="in", res=200)
print(gg.png)
dev.off()

gg.tikz <- gg+
  ggplot2::scale_x_log10("$K$ = number of folds")+
  directlabels::geom_dl(ggplot2::aes(
    N, unit.value, color=algorithm, label=sprintf("%s\n$K=%d$ @ 1 sec", algorithm, as.integer(N))),
    data=pred$prediction,
    method=directlabels::polygon.method("top", offset.cm=1))+
  directlabels::geom_dl(ggplot2::aes(
    N, reference, label=sprintf("$O(%s)$", sub("N", "K", fun.latex)), label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color=ref.color,
    method="bottom.polygons")
tikzDevice::tikz("Laribi2024-figure-refs.tex", width=8, height=4.5, standAlone = TRUE)
print(gg.tikz)
dev.off()
system("pdflatex Laribi2024-figure-refs")
system("evince Laribi2024-figure-refs.pdf")

plot(pred)

plot(results$rows)
show.refs <- atime:::references_funs[c("N^2", "N \\log N")]
show.rows <- results$rows
show.rows$measurements <- show.rows$measurements[
  algo %in% c("Wasikowski", "RSS")
][, algorithm := ifelse(algo=="RSS", "RSS (proposed)", "Wasikowski (previous)")][]
show.rows$by.vec <- c("algorithm", results$rows$by.vec)
show.rows$unit.col.vec <- c(seconds="median")
aref <- atime::references_best(show.rows, show.refs)
plot(aref)

ref.dt <- aref[["plot.references"]]
ref.color <- "grey"
meas <- aref$meas
gg <- ggplot2::ggplot()+
  ##ggplot2::facet_grid(unit ~ expr.name, scales="free")+
  ggplot2::theme_bw()
if(nrow(ref.dt[unit=="seconds"]) || nrow(meas[unit=="seconds"])){
  hline.df <- with(aref, data.frame(seconds.limit, unit="seconds"))
  gg <- gg+
    ggplot2::geom_text(ggplot2::aes(
      0, seconds.limit, label=" 1 second"),
      color="grey50",
      hjust=0,
      vjust=-0.5,
      data=hline.df)+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)
}
gg <- gg+
  theme(legend.position="none")+
  ggplot2::geom_ribbon(ggplot2::aes(
    N, ymin=min, ymax=max, group=expr.name, fill=algorithm),
    data=meas[unit=="seconds"],
    alpha=0.5)+
  ggplot2::geom_line(ggplot2::aes(
    N, empirical, group=expr.name, color=algorithm),
    linewidth=2,
    data=meas)+
  ## ggplot2::geom_line(ggplot2::aes(
  ##   N, reference, group=paste(expr.name, fun.name)),
  ##   color=ref.color,
  ##   linewidth=1,
  ##   data=ref.dt)+
  ggplot2::scale_y_log10(
    sprintf(
      "Seconds to compute assignment for $K=10$ folds\n(Median line and min/max band over 10 timings)"))+
  coord_cartesian(ylim=c(0.0001, 5))
gg.png <- gg+
  ggplot2::scale_x_log10("N = number of rows")
  directlabels::geom_dl(ggplot2::aes(
    N, reference, label=fun.name, label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color=ref.color,
    method="bottom.polygons")
png("Laribi2024-figure-rows.png", width=8, height=4, units="in", res=200)
print(gg.png)
dev.off()

gg.tikz <- gg+
  ggplot2::scale_x_log10(
    "$N$ = number of rows",
    breaks=c(10^seq(1, 5), max.N),
    limits=c(10, 2e6))+
  directlabels::geom_dl(ggplot2::aes(
    N, empirical, color=algorithm, label=sprintf("%s\n%.3f sec", algorithm, empirical)),
    data=aref$measurements[N==max(N)],
    method=directlabels::polygon.method("right", offset.cm=0.3))
tikzDevice::tikz("Laribi2024-figure-rows.tex", width=8, height=4, standAlone = TRUE)
print(gg.tikz)
dev.off()
system("pdflatex Laribi2024-figure-rows")
system("evince Laribi2024-figure-rows.pdf")

