library(data.table)
library(ggplot2)
algo.dt <- rowwiseDT(
  algo=, algo.disp=,
  "RSS", "RSS (proposed)",
  "Wasikowski", "Wasikowski (previous)",
  "rsample", "rsample (previous)",
  "origami", "origami (previous)",
  "random", "random"
)[, Algorithm := factor(algo.disp, rev(algo.disp))][]
afac <- function(a)factor(a, c("Wasikowski", "RSS"))
several_Tasks_raw <- fread("several_Tasks_data.csv")[, let(
  Data = data.name,
  Algo = afac(algo),
  Folds = factor(paste0("\n", folds), unique(paste0("\n", folds)))
)]
meta.dt <- fread("data_meta.csv")[order(rows)][, Rows := factor(Rows, Rows)][]
several_Tasks <- several_Tasks_raw[meta.dt, on="data.name"][, leakage := bad.groups>0][algo.dt, on = "algo"]
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
    mean.sd, Algorithm),
    shape=1,
    data=several_Tasks[Data != "five"])+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + `Strata/Group` + Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("Mean(SD) for 10 random group orderings (for ties)")
png("several_Tasks_sd.png", width=10, height=6, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    RMSE, Algorithm),
    shape=1,
    data=several_Tasks[Data != "five"])+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + `Strata/Group` +Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("RMSE = Root Mean Squared Error for 10 random group orderings (for ties)")
png("several_Tasks_RMSE.png", width=10, height=6, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    RSS, Algorithm, color=leakage),
    shape=1,
    data=several_Tasks[Data != "five" & is.finite(RSS)])+
  scale_color_manual(
    "Data leakage",
    values=c(
    "TRUE"="deepskyblue",
    "FALSE"="black"))+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + `Strata/Group` +Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("RSS = Residual Sum of Squares for 10 random group orderings (for ties)")+
  theme(legend.position=c(0.31, 0.15))
png("several_Tasks.png", width=12, height=6.5, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_point(aes(
    RMSE, Algorithm, color=leakage),
    shape=1,
    data=several_Tasks[Data != "five" & is.finite(RSS) & (folds %% 2)==0])+
  scale_color_manual(
    "Data leakage",
    values=c(
    "TRUE"="red",
    "FALSE"="black"))+
  facet_grid(
    Folds ~ Rows + Groups + `Rows/Group` + strata + `Strata/Group` +Data,
    scales="free",
    labeller=label_both)+
  scale_x_log10("Root Mean Squared Error (RMSE) for 10 random group orderings (for ties)")+
  theme(
    legend.position=c(0.3, 0.15),
    legend.background=element_rect(fill="#ffffffcc"))
png("several_Tasks_even.png", width=8, height=4.5, units="in", res=200)
print(gg)
dev.off()

