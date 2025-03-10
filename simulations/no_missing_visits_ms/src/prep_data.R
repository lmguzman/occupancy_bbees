prep.data.ms <- function(dd,
                         limit.to.visits='detected',
                         limit.to.sites='range') {

  if(limit.to.sites=='all' & limit.to.visits!='all') {
    cat('***\n')
    cat('Specifying limit.to.sites=\'all\' requires also setting limit.to.visits=\'all\'\n')
    cat('***\n')
    return(NULL)
  }

  ## keep only detected species:
  sp.keep <- which(apply(dd$Z, 1, sum)>0)

  ## which sites to keep will depend on limit.to.visits
  ##
  ## keep all sites, even those without any detections
  if(limit.to.visits=='all')
    site.keep <- 1:dd$nsite
  ## keep only sites that were visited
  if(limit.to.visits=='visits')
    site.keep <- which(apply(dd$vis.arr, 1, sum)>0)
  ## keep only sites that yielded a detection of at least one species
  if(limit.to.visits=='detected')
    site.keep <- which(apply(dd$X, 'site', sum)>0)
  ## if 'all' sites, set range for each species to TRUE for all sites
  if(limit.to.sites=='all')
    dd$sp.range[TRUE] <- TRUE
  
  ## subset based on the above
  dd$Z        <- dd$Z[sp.keep,site.keep,,drop=FALSE]
  dd$X        <- dd$X[sp.keep,site.keep,,,drop=FALSE]
  dd$sp.range <- dd$sp.range[sp.keep,site.keep,drop=FALSE]
  dd$vis.arr  <- dd$vis.arr[site.keep,,,drop=FALSE]
  dd$nsp      <- length(sp.keep)
  dd$nsite    <- length(site.keep)

  ## generate master index (to improve model efficiency (this prevents
  ## unnecessary iterating through all irrelevant sites and visits)
  get.indices <- function(sp) {
    ## visited array
    vis.arr <- dd$vis.arr
    ## if modelling all visits, set visit array to 1 everywhere
    if(limit.to.visits=='all') 
      vis.arr[TRUE] <- 1
    ## if modelling sites with detections only, create new visit array
    if(limit.to.visits=='detected') {
      nsp.detected <- apply(dd$X, 2:4, sum)
      vis.arr[TRUE] <- 1
      vis.arr[nsp.detected==0] <- 0
    }

    vis.arr[!dd$sp.range[sp,],,] <- 0
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    cbind(rep(sp,nrow(tmp)),tmp)
  }
  master.index <- do.call(rbind, lapply(1:dd$nsp, get.indices))
  colnames(master.index) <- c('sp','site','yr','visit')

  ## data structures to be returned
  list(X=dd$X[master.index],
       master.index=master.index,
       nsp=length(unique(master.index[,'sp'])),
       nsite=length(unique(master.index[,'site'])),
       nyr=length(unique(master.index[,'yr'])),
       nvisit=length(unique(master.index[,'visit'])),
       nind=nrow(master.index))
}

prep.data.ss <- function(dd) {
  ## use multi-sp prep to get data mostly in order

  ## because, for the single-species model, we want to model all
  ## species over all sites, we will just set the range of every
  ## species to all sites
  dd$sp.range[TRUE] <- TRUE
  my.data.ms <- prep.data.ms(dd=dd, limit.to.visits='all')
  
  ## pull out relevant pieces for a single species
  get.single.species <- function(sp) {
    keep <- my.data.ms$master.index[,'sp']==sp
    master.index <- my.data.ms$master.index[keep,,drop=FALSE]
    list(X=my.data.ms$X[keep],
         master.index=master.index,
         nsite=my.data.ms$nsite,
         nyr=my.data.ms$nyr,
         nvisit=my.data.ms$nvisit,
         nind=sum(keep))
  }

  ## and apply over all species
  sapply(1:my.data.ms$nsp, get.single.species, simplify=FALSE)
}
