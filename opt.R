library(data.table)
files <- rbind(
  data.table(g="f1g1", y=c(1,2)),
  data.table(g="f1g2", y=c(2,2)),
  data.table(g="f2g1", y=c(1,2,2)),
  data.table(g="f2g2", y=c(2)),
  data.table(g="f3g1", y=c(1,2,2,2)))
gtab <- files[, table(g, y)]
ytab <- files[, table(y)]
nstrat <- length(ytab)
nfolds <- 3
(nstar <- as.numeric(ytab/nfolds))
getrss <- function(guess)sum((guess-nstar)^2)
nhat <- zero <- matrix(0, nstrat, nfolds)
RSS <- getrss(nhat)
curr.gtab <- gtab
it.dt.list <- list()
getdt <- function(gt){
  rss.dt.list <- list()
  for(group in 1:nrow(gt)){
    gvec <- as.numeric(gt[group,])
    for(fold in seq_len(nfolds)){
      to.add <- zero
      to.add[,fold] <- gvec
      rss.dt.list[[paste(group, fold)]] <- data.table(
        group, fold,
        getrss=getrss(to.add+nhat),
        update=RSS+sum(((nhat[,fold]-nstar)*2+gvec)*gvec))
    }
  }
  rbindlist(rss.dt.list)
}
for(iteration in 1:nrow(gtab)){
  (rss.dt <- getdt(curr.gtab))
  chosen <- rss.dt[which.min(update)][, gname := rownames(curr.gtab)[group] ]
  nhat[,chosen$fold] <- nhat[,chosen$fold]+curr.gtab[chosen$group,]
  RSS <- chosen$update
  it.dt.list[[iteration]] <- data.table(iteration, chosen)
  iteration <- iteration+1
  curr.gtab <- curr.gtab[-chosen$group,,drop=FALSE]
}
(it.dt <- rbindlist(it.dt.list))

gord <- order(colSums((t(gtab)-nstar)^2))
sort.gtab <- gtab[gord,]
nhat <- zero
RSS <- getrss(nhat)
my.dt.list <- list()
for(iteration in 1:nrow(sort.gtab)){
  gvec <- sort.gtab[iteration,]
  fold.rss <- RSS+colSums(((nhat-nstar)*2+gvec)*gvec)
  chosen.fold <- which.min(fold.rss)
  newRSS <- fold.rss[chosen.fold]
  my.dt.list[[iteration]] <- data.table(
    iteration, chosen.fold, RSS, newRSS)
  RSS <- newRSS
  nhat[,chosen.fold] <- nhat[,chosen.fold] + gvec
}
(my.dt <- rbindlist(my.dt.list))
