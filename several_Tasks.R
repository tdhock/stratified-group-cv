library(data.table)
library(ggplot2)
comma <- function(x)data.table(x, chr=format(x, big.mark=",", scientific=FALSE, trim=TRUE))[, factor(chr, unique(chr[order(x)]))]
meta.dt <- fread("data_meta.csv")[, let(
  "Rows/Group" = ifelse(
    min.rows.per.group==max.rows.per.group, min.rows.per.group,
    sprintf("%s–%s", min.rows.per.group, comma(max.rows.per.group))),
  Rows = comma(rows),
  Groups = comma(groups)
)]
afac <- function(a)factor(a, c("Wasikowski", "RSS"))
several_Tasks_raw <- fread("several_Tasks_data.csv")[, let(
  Data = data.name,
  Algo = afac(algo),
  Folds = factor(paste0("\n", folds), unique(paste0("\n", folds)))
)]
several_Tasks <- several_Tasks_raw[meta.dt, on="data.name"]
several_Tasks[, table(data.name, algo, useNA="always")]
gg <- ggplot()+
  theme(axis.text.x=element_text(angle=30, hjust=1))+
  geom_point(aes(
    RSS, algo, color=zeros==0),
    shape=1,
    data=several_Tasks)+
  facet_wrap(
    c("Data","folds"),
    scales="free",
    labeller=label_both,
    ncol=length(unique(several_Tasks$folds)))+
  scale_x_log10()
## png("several_Tasks_wrap.png", width=20, height=6, units="in", res=200)
print(gg)
## dev.off()


several_Tasks[Data=="respiratory" & folds==9]

clong <- melt(several_Tasks[algo!="random"], measure.vars=c("RSS", "mean.sd"))
(cwide <- dcast(clong, Data+folds+variable~algo, mean)[, let(
  diff=RSS-Wasikowski,
  lr = log10(RSS/Wasikowski)
)][])
(wider <- dcast(cwide, Data+folds~variable, value.var="lr"))
wider[sign(RSS)!=sign(mean.sd)]
gg <- ggplot()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, hjust=1))+
  geom_text(aes(
    afac("RSS"), Inf, label=sprintf("Diff %.1f", diff), color=factor(sign(diff))),
    data=cwide[Data=="respiratory"],
    angle=90,
    hjust=1,
    vjust=-0.5)+
  scale_color_manual("sign(Diff)", values=c(
    "0"="grey50",
    "1"="black",
    "-1"="red"))+
  scale_y_continuous("Evaluation metric to minimize (10 random seeds)")+
  scale_x_discrete("Algorithm", drop=FALSE)+
  geom_point(aes(
    Algo, value),
    shape=1,
    data=clong[Data == "respiratory"])+
  facet_grid(
    variable ~ folds,
    scales="free",
    labeller=label_both)
png("several_Tasks_respiratory.png", width=10, height=5, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    mean.sd, algo),
    shape=1,
    data=several_Tasks[Data != "five"])+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("Mean(SD) for 10 random group orderings")
png("several_Tasks_sd.png", width=10, height=6, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    RMSE, algo),
    shape=1,
    data=several_Tasks[Data != "five"])+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("RMSE = Root Mean Squared Error for 10 random group orderings")
png("several_Tasks_RMSE.png", width=10, height=6, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    RSS, algo),
    shape=1,
    data=several_Tasks[Data != "five"])+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("RSS = Residual Sum of Squares for 10 random group orderings")
png("several_Tasks.png", width=10, height=6, units="in", res=200)
print(gg)
dev.off()

