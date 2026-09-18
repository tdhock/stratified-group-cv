library(data.table)
library(ggplot2)
show.refs <- atime:::references_funs[c("N", "N^2")]
results <- readRDS("Laribi2024-figure-data.rds")

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
ref.color <- "black"
meas <- aref$meas
pred <- predict(aref)
gg <- ggplot2::ggplot()+
  ##ggplot2::facet_grid(unit ~ expr.name, scales="free")+
  ggplot2::theme_bw()
if(nrow(ref.dt[unit=="seconds"]) || nrow(meas[unit=="seconds"])){
  hline.df <- with(aref, data.frame(seconds.limit, unit="seconds"))
  gg <- gg+
    ggplot2::geom_text(ggplot2::aes(
      0, seconds.limit, label=sprintf(" %d sec.", pred$seconds.limit)),
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
    linewidth=3,
    data=meas)+
  ggplot2::geom_line(ggplot2::aes(
    N, reference, group=paste(expr.name, fun.name)),
    color=ref.color,
    linewidth=1,
    data=ref.dt)+
  ggplot2::scale_y_log10(
    sprintf(
      "Time (seconds)\nto compute fold assignment in N=%s data\n(Median line and min/max band over %d timings)",
      format(max.N, big.mark=",", scientific=FALSE, trim=TRUE),
      meas$n_itr[1]))+
  coord_cartesian(ylim=c(0.01, 50))
gg.png <- gg+
  ggplot2::scale_x_log10("K = number of folds")+
  directlabels::geom_dl(ggplot2::aes(
    N, unit.value, color=algorithm, label=sprintf("%s\nK=%d @ %s sec.", algorithm, as.integer(N), pred$seconds.limit)),
    data=pred$prediction,
    method=directlabels::polygon.method("top", offset.cm=1))+
  directlabels::geom_dl(ggplot2::aes(
    N, reference, label=sub("N", "K", fun.name),
    label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color="white",
    method="bottom.polygons")
png("Laribi2024-figure-refs.png", width=8, height=4, units="in", res=200)
print(gg.png)
dev.off()

xrot <- theme(axis.text.x=element_text(hjust=1, angle=30))
gg.tikz <- gg+
  ggplot2::scale_x_log10("$K$ = number of folds")+
  directlabels::geom_dl(ggplot2::aes(
    N, unit.value, color=algorithm, label=sprintf("%s\n$K=%d$ @ 1 sec.", algorithm, as.integer(N))),
    data=pred$prediction,
    method=directlabels::polygon.method("top", offset.cm=1))+
  directlabels::geom_dl(ggplot2::aes(
    N, reference, label=sprintf("$O(%s)$", sub("N", "K", fun.latex)), label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color="white",
    method="bottom.polygons")+ 
 xrot
input.width.in <- 6
tikzDevice::tikz("Laribi2024-figure-refs-input.tex", width=input.width.in, height=4.5, standAlone = FALSE)
print(gg.tikz)
dev.off()
tikzDevice::tikz("Laribi2024-figure-refs.tex", width=8, height=4.5, standAlone = TRUE)
print(gg.tikz)
dev.off()


hline.df <- with(aref, data.frame(seconds.limit, unit="seconds"))
max.N <- max(results$rows$measurements$N)
gg <- ggplot2::ggplot()+
  ##ggplot2::facet_grid(unit ~ expr.name, scales="free")+
  ggplot2::theme_bw()+
  ggplot2::geom_text(ggplot2::aes(
    0.5, seconds.limit, label=sprintf(" %d sec.", pred$seconds.limit)),
    color="grey50",
    hjust=0,
    vjust=1.5,
    data=hline.df)+
  ggplot2::geom_hline(ggplot2::aes(
    yintercept=seconds.limit),
    color="grey",
    data=hline.df)+
  theme(legend.position="none")+
  ggplot2::geom_ribbon(ggplot2::aes(
    N, ymin=min, ymax=max, group=expr.name, fill=algorithm),
    data=meas[unit=="seconds"],
    alpha=0.5)+
  ggplot2::geom_line(ggplot2::aes(
    N, empirical, group=expr.name, color=algorithm),
    linewidth=3,
    data=meas)+
  ggplot2::geom_line(ggplot2::aes(
    N, reference, group=paste(expr.name, fun.name)),
    color=ref.color,
    linewidth=1,
    data=ref.dt)+
  ggplot2::scale_y_log10(
    sprintf(
      "Time (seconds)\nto compute fold assignment in N=%s data\n(Median line and min/max band over %d timings)",
      format(max.N, big.mark=",", scientific=FALSE, trim=TRUE),
      meas$n_itr[1]))+
  coord_cartesian(ylim=c(0.01, 50))+
  ggplot2::scale_x_log10("$K$ = number of folds")+
  directlabels::geom_dl(ggplot2::aes(
    N, unit.value, color=algorithm,
    label=sprintf(
      "%s\n$K=%d$ @ %d sec.",
      algorithm,
      as.integer(N),
      pred$seconds.limit)),
    data=pred$prediction,
    method=list(
      directlabels::polygon.method(
        "top",
        offset.cm=0.5,
        padding.cm=0.1),
      directlabels::dl.add(y=-0.02)
    ))+
  directlabels::geom_dl(ggplot2::aes(
    N, reference,
    label=sprintf("$O(%s)$", sub("N", "K", fun.latex)),
    label.group=paste(fun.name, expr.name)),
    data=ref.dt,
    color="white",
    method="bottom.polygons")+ 
  xrot+
  theme(axis.text=element_text(size=10))+
  ggplot2::scale_y_log10(
    "Computation time (seconds)",
    breaks=10^seq(-2, 1),
    labels=scales::label_log())+
  coord_cartesian(xlim=c(0.5, 10000), ylim=c(0.01, 50))
tikz.width <- 3.3
tikzDevice::tikz("Laribi2024-figure-refs-small.tex", width=tikz.width, height=3, standAlone = TRUE)
print(gg)
dev.off()
system("pdflatex Laribi2024-figure-refs-small")
if(FALSE){
  tikzDevice::tikz("Laribi2024-figure-refs-small-input.tex", width=tikz.width, height=3, standAlone = FALSE)
  print(gg)
  dev.off()
  system("evince Laribi2024-figure-refs-small.pdf &")
}







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
      10, seconds.limit, label=sprintf(" %d sec.", aref$seconds.limit)),
      color="grey50",
      hjust=0,
      vjust=1.5,
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
      "Time (seconds)\nto compute assignment for $K=10$ folds\n(Median line and min/max band over %d timings)",
      meas$n_itr[1]))+
  coord_cartesian(ylim=c(0.0001, 5))
gg.png <- gg+
  ggplot2::scale_x_log10("N = number of rows")
png("Laribi2024-figure-rows.png", width=8, height=4, units="in", res=200)
print(gg.png)
dev.off()

gg.tikz <- gg+
  ggplot2::scale_x_log10(
    "$N$ = number of rows",
    breaks=c(10^seq(1, 5), max.N),
    limits=c(10, 2e6))+
  directlabels::geom_dl(ggplot2::aes(
    N, empirical, color=algorithm, label=sprintf("%s\n%.3f sec.", sub(" ", "\n", algorithm), empirical)),
    data=aref$measurements[N==max(N)],
    method=directlabels::polygon.method("right", offset.cm=0.3))+
  xrot
tikzDevice::tikz("Laribi2024-figure-rows-input.tex", width=input.width.in, height=4, standAlone = FALSE)
print(gg.tikz)
dev.off()
tikzDevice::tikz("Laribi2024-figure-rows.tex", width=8, height=4, standAlone = TRUE)
print(gg.tikz)
dev.off()
if(FALSE){
  system("pdflatex Laribi2024-figure-rows")
  system("evince Laribi2024-figure-rows.pdf")
}

gg.tikz <- gg+
  ggplot2::scale_x_log10(
    "$N$ = number of rows",
    breaks=c(10^seq(1, 5), max.N),
    limits=c(10, 2e6))+
  theme(axis.text.x=element_text(hjust=1, angle=60))+
  theme(axis.text=element_text(size=10))+
  coord_cartesian(
    xlim=c(10, 1e7))+
  directlabels::geom_dl(ggplot2::aes(
    N, empirical, color=algorithm,
    label=sprintf("%s\n%.3f sec.", sub(" ", "\n", algorithm), empirical)),
    data=aref$measurements[N==max(N)],
    method=directlabels::polygon.method("right", offset.cm=0.3))+
  ggplot2::scale_y_log10(
    "Computation time (seconds)",
    ##breaks=10^seq(-2, 1),
    labels=scales::label_log())
tikz.width <- 3.3
tikzDevice::tikz("Laribi2024-figure-rows-small.tex", width=tikz.width, height=3, standAlone = TRUE)
print(gg.tikz)
dev.off()
system("pdflatex Laribi2024-figure-rows-small")
if(FALSE){
  tikzDevice::tikz("Laribi2024-figure-rows-small-input.tex", width=tikz.width, height=3, standAlone = FALSE)
  print(gg.tikz)
  dev.off()
  system("evince Laribi2024-figure-rows-small.pdf &")
}

