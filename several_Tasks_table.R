library(data.table)
counts.long <- fread("several_Tasks_table_data.csv")
counts.list <- split(counts.long, counts.long$data)
out.list <- list()
for(data.name in names(counts.list)){
  counts.dt <- counts.list[[data.name]]
  out.list[[data.name]] <- dcast(
    counts.dt,
    algo + fold ~ stratum,
    value.var="rows")
}
out.list

dshow <- counts.long[
  #data.name %in% c("Laribi2024", "PetAdoption")
  data.name != "five"
]
ideal <- dshow[algo=="RSS", .(
  #total=sum(rows),
  rows=mean(rows)
), by=.(data.name, algo, stratum)][, let(
  algo = "ideal",
  fold = NA
)][, rank := rank(rows), by=data.name][]



din <- rbind(
  ideal,
  dshow[
    ideal[, .(data.name, stratum, rank)],
    on=.NATURAL
  ][, names(ideal), with=FALSE]
)[, d := factor(substring(data.name, 1, 1), c("r","A","P","L"))]
out.dt <- dcast(
  din[, algo.fold := paste(algo, fold)],
  algo.fold ~ d + rank,
  value.var="rows")
library(xtable)
onames <- grep("_", names(out.dt), value = TRUE)
xtin <- data.table(out.dt)[, (onames) := lapply(.SD, format), .SDcols=onames]
xt <- xtable(xtin)
xt.chr <- print(xt, include.rownames = FALSE)
out.chr <- gsub("([.]0+)", "\\\\textcolor{white}{\\1}", xt.chr)
bf.chr <- gsub("(47|48|49|50|51|168|169|160|171|172|1047|1049|1050|52944|52945|52946)", "\\\\textcolor{red}{\\1}", out.chr)
cat(bf.chr)
