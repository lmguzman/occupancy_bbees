
source('soroye_reanalysis/src/initialize.R')

## continent over which models are run
continent <- 'AM;EU'


## ------------------------------------------------------------
## load model runs
dir <- file.path('soroye_reanalysis/saved/soroye/', continent)
files <- list.files(dir)
loaded <- lapply(file.path(dir, files),
                 function(x) {
                   load(x)
                   list(my.data=my.data, res=res)
                 })
names(loaded) <-  unlist(strsplit(files, split='.RData'))
## ------------------------------------------------------------

## ------------------------------------------------------------
## calculate soroye change in occupancy
get.delta.occ <- function(out, con) {
  
  sims.arr <- abind(out$res$mcmc, along=3)
  vars <- dimnames(sims.arr)[[2]]
  niter  <- dim(sims.arr)[1]
  nsite <- out$my.data$nsite
  nera <- out$my.data$nera
  nchain <- dim(sims.arr)[3]
  X <- out$my.data$X
  
  zz <- vars[grep('Z\\[', vars)]

  ## ------------------------------------------------------------
  Z.sims.array <- array(sims.arr[,zz,],
                        dim=c(niter=niter,
                              site=nsite,
                              era=nera,
                              chain=nchain))
  dim(Z.sims.array)
  Z.sims.array <- aperm(Z.sims.array, c(2,3,1,4))
  dimnames(Z.sims.array)
  ## add quadID
  dimnames(Z.sims.array)[[1]] <- out$my.data$quadID
  
  ## figure out which sites the species is present at
  ##
  ## first, limit to specified continent
  ##
  ## below can be deleted once new runs finish and contain my.data$continent
  load('soroye_reanalysis/data/for_model/occ.data.RData')
  sites.keep <- continent==con
  ## sites.keep <- my.data$continent==con
  X <- X[sites.keep,,,drop=FALSE]
  Z.sims.array <- Z.sims.array[sites.keep,,,,drop=FALSE]
  ## then limit to sites where a species was detected
  sites.present <- apply(X,1,sum)>0
  if(sum(sites.present)==0) return(NA)

  Z.sims.array.pres <- Z.sims.array[sites.present,,,,drop=FALSE]

  ## Soroye's approach
  Z.post.means <- apply(Z.sims.array.pres, 1:2, mean)
  occ.means <- colMeans(Z.post.means)
  return((occ.means[2]-occ.means[1])/occ.means[1])
}


delta.occ.AM <- sapply(loaded, get.delta.occ, con='AM')
delta.occ.EU <- sapply(loaded, get.delta.occ, con='EU')
delta.occ.AM <- delta.occ.AM[!is.na(delta.occ.AM)]
delta.occ.EU <- delta.occ.EU[!is.na(delta.occ.EU)]

mean(delta.occ.AM[-11])
mean(delta.occ.EU)

save(delta.occ.AM,
     delta.occ.EU,
     file='soroye_reanalysis/saved/for_figures/delta.occ.soroye.RData')

## FOR NORTH AMERICA (requires that we drop distinguendus)
hist(delta.occ.AM[names(delta.occ.AM)!='distinguendus'], col='red')
## mean and standard error
mean(delta.occ.AM[names(delta.occ.AM)!='distinguendus'])
se(delta.occ.AM[names(delta.occ.AM)!='distinguendus'])

## FOR EUROPE
hist(delta.occ.EU, col='red')
## mean and standard error
mean(delta.occ.EU)
se(delta.occ.EU)
