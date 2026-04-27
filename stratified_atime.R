library(ggplot2)
load("stratified_atime.RData")
requireNamespace("atime")
alist$unit.col.vec <- c(seconds="median")
plot(alist)

arefs <- atime::references_best(alist)
plot(arefs)

apred <- predict(arefs, seconds=0.01)
overhead <- geom_text(aes(
  x=Inf, y=5e-5, label="R-Python overhead<1ms"),
  hjust=1)
xlog <- scale_x_log10("N = number of rows input to fold generation function")
ylog <- scale_y_log10("Time to compute folds with group+strata constraint\nmedian line, min/max band")
(gg <- plot(apred)+
   geom_vline(color="grey",xintercept=ntrain)+
   ggtitle("Kaggle pet adoption data, 5-fold CV with group+strata constraint")+
   overhead+
   facet_null()+
   ylog+
   geom_text(aes(
     x=ntrain, y=0,
     label=sprintf("rows in CSV=%d", ntrain)),
     hjust=1, vjust=-0.5, color="grey50")+
   xlog)

png("stratified_atime_kaggle.png", width=6, height=6, units="in", res=200)
print(gg)
dev.off()

plot(alist.sim)

arefs.sim <- atime::references_best(alist.sim)
plot(arefs.sim)

apred.sim <- predict(arefs.sim)
(gg <- plot(apred.sim)+
   ggtitle("Simulated data, 5-fold CV with group+strata constraint")+
   overhead+
   facet_null()+
   ylog+
   xlog+
   overhead)

png("stratified_atime_sim.png", width=6, height=6, units="in", res=200)
print(gg)
dev.off()
