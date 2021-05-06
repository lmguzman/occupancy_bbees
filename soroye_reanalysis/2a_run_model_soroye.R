## Runs the model done in Soroye et al. 2020

data.dir <- 'soroye_reanalysis/data'

source('soroye_reanalysis/src/initialize.R')

## load data
load(paste0(data.dir, '/for_model/occ.data.RData'), verbose=TRUE)

## run for Europe, North America, or all together
run.case <- function(limit.to) {

  ## source JAGS model 
  source(sprintf('soroye_reanalysis/models/%s.R', model))
  model.txt <- sprintf('soroye_reanalysis/models/%s.txt', model)
  write.model(model.jags, con=model.txt)

  ## run a single species
  run.for.single.spp <- function(sp) {
    cat(sprintf('Running %s in %s\n', sp, paste(limit.to, collapse=';')))

    ## bundle data 
    ##
    ## subset to specified continent(s)
    sites.keep <- which(continent %in% limit.to)
    
    my.data <- list(X=occ.arr[sp,sites.keep,,],
                    sampmat=sampmat[sites.keep,,],
                    quadID=quadID[sites.keep],
                    continent=continent[sites.keep])

    ## if no detections on that continent, break out
    if(sum(my.data$X)==0) return(NULL)
    
    ## further constrain to sites with at least one detection
    
    my.data$nsite  <- dim(my.data$X)[['nsite']]
    my.data$nvisit <- dim(my.data$X)[['nvisit']]
    my.data$nyr    <- dim(my.data$X)[['nyr']]
    
    ## break out if no sites ...
    if(my.data$nsite==0)
      return(NA)

    ## Initial values 
    Zst <- apply(my.data$X, 1:2, max)
    my.inits <- function(){
      list(Z=Zst)
    }
    
    ## Parameters monitored 
    my.params <- get.params()

    ## drop un-needed variables (that I still want to return with
    ## my.data)
    my.data.jags <- my.data[c('X',
                              'sampmat',
                              'nsite',
                              'nvisit',
                              'nyr')]

    ## scale sampmat (depends on model)
  
      sampmat <- my.data.jags$sampmat
      mean.sampmat <- mean(sampmat)
      sd.sampmat   <- sd(sampmat)
      my.data.jags$sampmat <- (sampmat-(mean.sampmat))/sd.sampmat

    res <- run.jags(model=model.txt,
                    monitor=get.params(),
                    data=my.data.jags,
                    inits=my.inits,
                    n.chains=3,
                    burnin=n.burnin,
                    sample=floor(n.iter/n.thin),
                    thin=n.thin,
                    adapt=n.adapt,
                    method='rjags')

    if(is.null(res)) return(NULL)
    
    fn <- sprintf('%s.RData', sp)
    save(my.data, res,
         file=file.path('soroye_reanalysis/saved',
                        model,
                        paste(limit.to,collapse=';'),
                        fn))

    NULL
  }

  ## run models
  if(ncores==1)
    lapply(dimnames(occ.arr)[['sp']], run.for.single.spp)

  if(ncores>1)
    mclapply(dimnames(occ.arr)[['sp']], run.for.single.spp,
             mc.cores=ncores,
             mc.preschedule=FALSE)
  NULL
}

## MCMC settings 
n.burnin <- 1e3
n.adapt  <- 1e3
n.iter   <- 1e5
n.thin   <- 1e2
ncores <- 1

model <- 'soroye'

run.case(limit.to=c('AM','EU'))

