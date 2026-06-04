library(data.table)
train_dt <- fread("train.csv") #from https://www.kaggle.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation/notebook

five <- rbind(
  data.table(g="f1g1", y=c(1,2,2,2)),
  ## switching above and below (tied sd) does not affect outcome.
  data.table(g="f2g2", y=c(2,2)),
  data.table(g="f3g1", y=c(1,2,2)),
  ## switching above and below (tied sd) makes sd heuristic optimal.
  data.table(g="f3g2", y=c(2)),
  data.table(g="f2g1", y=c(1,2)))

data(respiratory, package="geepack")
(resp_dt <- data.table(respiratory)[, person := paste(center, id)][])

data(AZtrees, package="mlr3resampling")

if(!file.exists("Laribi2024full.csv")){
  download.file("https://zenodo.org/records/12954673/files/dataset.csv?download=1", "Laribi2024full.csv")
}
Laribi <- fread("Laribi2024full.csv")

DataSet <- function(dt, yname, gname){
  ifac <- function(x)as.integer(factor(x))
  data.table(dt)[, .(target=ifac(get(yname)), groupID=ifac(get(gname)))]
}
data.list <- list(
  Laribi2024=DataSet(Laribi, "oym", "patient_id"),
  PetAdoption=DataSet(train_dt, "AdoptionSpeed", "RescuerID"),
  five=DataSet(five, "y", "g"),
  respiratory=DataSet(resp_dt, "outcome", "person"),
  AZtrees=DataSet(AZtrees, "y", "polygon"))

dir.create("data", showWarnings = FALSE)
for(data.name in names(data.list)){
  dt <- data.list[[data.name]]
  fwrite(dt, sprintf("data/%s.csv", data.name))
}
