ares <- readRDS("figure_atime_cpp_data.rds")

aref <- atime::references_best(ares)
apred <- predict(aref)

png("figure_atime_cpp.png", width=6, height=3, units="in", res=200)
plot(apred)
dev.off()
