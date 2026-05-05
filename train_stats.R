tdt <- fread("train.csv")
dim(tdt)
table(tdt$AdoptionSpeed)
length(table(tdt$RescuerID))
