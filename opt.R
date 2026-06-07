library(data.table)
## an ordering which is sub-optimal for sd sort method.
files <- rbind(
  data.table(g="f1g1", y=c(1,2,2,2)),
  ## switching above and below (tied sd) does not affect outcome.
  data.table(g="f2g2", y=c(2,2)),
  data.table(g="f3g1", y=c(1,2,2)),
  ## switching above and below (tied sd) makes sd heuristic optimal.
  data.table(g="f3g2", y=c(2)),
  data.table(g="f2g1", y=c(1,2))
)[, gfac := factor(g, unique(g))]
(gtab <- files[, table(gfac, y)])

(ytab <- files[, table(y)])
nstrat <- length(ytab)
nfolds <- 3
(nstar <- as.numeric(ytab/nfolds))
getrss <- function(guess)sum((guess-nstar)^2)
nhat <- zero <- matrix(0, nstrat, nfolds)
RSS <- getrss(nhat)
curr.gtab <- gtab
it.dt.list <- list()
## searches all groups and folds at each iteration (quadratic time).
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
        crit=sum(gvec*nhat[,fold]),
        orig=RSS+sum(2*nhat[,fold]*gvec + gvec^2 - 2*nstar*gvec),
        update=RSS+sum(((nhat[,fold]-nstar)*2+gvec)*gvec))
    }
  }
  rbindlist(rss.dt.list)
}
all.rss.dt.list <- list()
for(iteration in 1:nrow(gtab)){
  (rss.dt <- getdt(curr.gtab)[, is.min := crit==min(crit)])
  ch.i <- which.min(rss.dt$update)
  rss.dt[ch.i, chosen := TRUE]
  all.rss.dt.list[[iteration]] <- data.table(iteration, rss.dt)
  chosen <- rss.dt[ch.i][, gname := rownames(curr.gtab)[group] ]
  nhat[,chosen$fold] <- nhat[,chosen$fold]+curr.gtab[chosen$group,]
  RSS <- chosen$update
  it.dt.list[[iteration]] <- data.table(iteration, chosen)
  iteration <- iteration+1
  curr.gtab <- curr.gtab[-chosen$group,,drop=FALSE]
}
all.rss.dt <- rbindlist(all.rss.dt.list)
(it.dt <- rbindlist(it.dt.list))

## searches only folds at each iteration (linear time).
(rss.for.sort <- colSums((t(gtab)-nstar)^2))
gord <- order(rss.for.sort)
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

## searches only folds at each iteration (linear time),
## SD heuristic.
(sd.for.sort <- apply(gtab, 1, var))
gord.list <- list(
  ##c(2, 1, 3, 4, 5),
  sd_sub_opt=c(1, 2, 3, 4, 5),
  ##c(2, 1, 4, 3, 5),
  sd_opt=c(1, 2, 4, 3, 5),
  rss=c(1,3,2,5,4))
was.dt.list <- list()
for(gord.i in seq_along(gord.list))for(crit in c("SD", "RSS")){
  gord <- gord.list[[gord.i]]
  sort.gtab <- gtab[gord,]
  nhat <- zero
  for(iteration in 1:nrow(sort.gtab)){
    gvec <- sort.gtab[iteration,]
    rss.per.fold <- colSums(((nhat-nstar)*2+gvec)*gvec)
    sd.vec <- sapply(1:nfolds, function(k){
      nhat[,k] <- nhat[,k]+gvec
      mean(apply(nhat, 1, sd))
    })
    fold.ord <- if(crit=="SD")order(sd.vec, colSums(nhat)) else order(rss.per.fold)
    chosen.fold <- fold.ord[1]
    nhat[,chosen.fold] <- nhat[,chosen.fold] + gvec
    newRSS <- sum((nhat-nstar)^2)
    was.dt.list[[paste(gord.i, crit, iteration)]] <- data.table(
      ord_name=names(gord.list)[gord.i], crit,
      iteration, chosen.fold, newRSS, meanSD=sd.vec[chosen.fold])
  }
}
(was.dt <- rbindlist(was.dt.list))
was.dt[iteration==5]
##sd_sub_opt order sub-optimal for both crit.

##other orders optimal for both crit.

## sort is more important in this case.
