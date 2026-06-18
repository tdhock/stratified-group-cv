library(ggplot2)
load("stratified_atime.RData")
requireNamespace("atime")
alist$unit.col.vec <- c(seconds="median")
plot(alist)

myround <- function(x){
  f=10^-floor(log10(x)-1)
  round(f*x)/f
}
alist$measurements[
, label := paste(expr.name, format(myround(median[N==max(N)])), "sec.")
, by=expr.name]
gg <- ggplot()+
  geom_ribbon(aes(
    N, median, ymin=min, ymax=max, fill=label),
    alpha=0.5,
    data=alist$measurements)+
  geom_line(aes(
    N, median, color=label),
    size=1,
    data=alist$measurements)+
  scale_x_log10(
    "N = Number of data rows",
    breaks=c(10^seq(1, 5), ntrain),
    limits=c(10, 1e8))+
  scale_y_log10(
    "Computation time (seconds)\n5-fold CV in Laribi2024 data\nmedian line and min/max band\nover 10 timings",
    breaks=10^seq(-5, 5))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, hjust=1))
(dl <- directlabels::direct.label(gg, list(cex=0.7, "right.polygons")))
png("stratified_atime.png", width=6, height=3, units="in", res=200)
print(dl)
dev.off()
