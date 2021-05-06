
source('soroye_reanalysis/src/initialize.R')

## load data
data.dir <- 'soroye_reanalysis/data'
load(file.path(data.dir, 'for_model/occ.data.RData'), verbose=TRUE)
load(file.path(data.dir, 'for_model/ranges.RData'), verbose=TRUE)


run.model <- function(limit.to.cont, limit.to.visits) {

  ## subset ranges
  ranges <- ranges[[limit.to.cont]]
  
  ## source JAGS model
  source(sprintf('soroye_reanalysis/models/%s.R', model))
  model.txt <- sprintf('soroye_reanalysis/models/%s.txt', model)
  write.model(model.jags, con=model.txt)
  
  ## bundle data 
  ##
  ## subset to specified continent(s)
  sites.keep  <- which(continent %in% limit.to.cont)
  occ.arr     <- occ.arr[,sites.keep,,]
  sampmat     <- sampmat[sites.keep,,]
  quadID      <- quadID[sites.keep]
  
  ## subset to species that are present
  sp.keep <- apply(occ.arr, 'sp', sum)>0
  ## manually drop terrestris/lucorum from North America
  if(limit.to.cont=='AM') {
    if(revised.sp.set) {
      sp.keep['terrestris'] <- FALSE
      sp.keep['lucorum'] <- FALSE
    }
    if(!revised.sp.set) {
      sp.keep['distinguendus'] <- FALSE
    }
  }
  occ.arr <- occ.arr[sp.keep,,,]
  ranges <- ranges[sp.keep]
  
  ## species counts
  nsp.arr <- apply(occ.arr, c('site','yr','visit'), sum)
  ## create default visit array
  ##
  ## visit array depends on scenario
  if(limit.to.visits=='all')      vis.arr <- nsp.arr*0+1
  if(limit.to.visits=='detected') vis.arr <- (nsp.arr>0)*1

  ## only consider sites in a species' range
  get.range <- function(rr) match(intersect(rr, quadID), quadID)
  sites.by.sp <- sapply(ranges, get.range, simplify=FALSE)
  
  ## generate master index (to improve model efficiency (this prevents
  ## unnecessary iterating through all irrelevant sites and visits)
  get.indices <- function(sp) {
    outside.range <- setdiff(1:dim(occ.arr)['nsite'], sites.by.sp[[sp]])
    vis.arr[outside.range,,] <- 0
    tmp <- which(vis.arr==1, arr.ind=TRUE)
    cbind(sp=rep(sp,nrow(tmp)),tmp)
  }
  master.index <-
    do.call(rbind, lapply(1:dim(occ.arr)['nsp'], get.indices))

  X <- occ.arr[master.index]
  my.data <- list(X=X,
                  yr=master.index[,'yr'],
                  site=master.index[,'site'],
                  sp=master.index[,'sp'],
                  visit=master.index[,'visit'],
                  nsp=dim(occ.arr)['nsp'],
                  nsite=dim(occ.arr)['nsite'],
                  nvisit=dim(occ.arr)['nvisit'],
                  nyr=dim(occ.arr)['nyr'],
                  nind=nrow(master.index))
  
  ## rescale effort (not used in current models)
  sampled <- (sampmat>0)*1
  mean.sampmat <- mean(sampmat[sampled==1])
  sd.sampmat   <- sd(sampmat[sampled==1])
  my.data$sampmat <- (sampmat-(mean.sampmat))/sd.sampmat
  
  ## Initial values 
  Zst <- array(1,dim=c(my.data$nsp,my.data$nsite,my.data$nyr))
  make.inits <- function() {
    RNG <- parallel.seeds("base::BaseRNG", 1)
    c(list(Z=Zst), RNG[[1]])
  }
  inits1 <- make.inits()
  inits2 <- make.inits()
  inits3 <- make.inits()
  
  res <- run.jags(model=model.txt,
                  monitor=get.params(),
                  data=my.data,
                  inits=list(inits1,inits2,inits3),
                  n.chains=3,
                  burnin=n.burnin,
                  sample=floor(n.iter/n.thin),
                  thin=n.thin,
                  adapt=n.adapt,
                  method='parallel')

  ## store species IDs and site quadIDs
  my.data$sp.ID <- names(sites.by.sp)
  my.data$site.quadID <- quadID

  fn <- sprintf('%s_%s.RData',
                limit.to.visits,
                paste(limit.to.cont,collapse=';'))
  if(!revised.sp.set & limit.to.cont=='AM')
    fn <- sprintf('%s_%s_soroye_set.RData',
                  limit.to.visits,
                  paste(limit.to.cont,collapse=';'))
  
  save(my.data, res, file=file.path('soroye_reanalysis/saved', model, fn))
  NULL
}

revised.sp.set <- TRUE

model <- 'ms'

## MCMC settings 
n.burnin <- 1e3
n.adapt  <- 1e3
n.iter   <- 1e5
n.thin   <- 1e2

## only consider visits where at least one species was detected
run.model(limit.to.cont='EU', limit.to.visits='detected')
run.model(limit.to.cont='AM', limit.to.visits='detected')


