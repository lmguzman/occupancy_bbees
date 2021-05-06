run.model <- function(dd,
                      model,
                      n.iter=1e5,
                      n.burnin=1e3,
                      n.adapt=1e3,
                      n.thin=1e2) {

  model.txt <- sprintf('src/models/%s.txt', model)
  
  ## multi-species run (parallel)
  if(model=='ms') {
    jags.data <- list(X=dd$X,
                      yr=dd$master.index[,'yr'],
                      site=dd$master.index[,'site'],
                      sp=dd$master.index[,'sp'],
                      visit=dd$master.index[,'visit'],
                      nsp=dd$nsp,
                      nsite=dd$nsite,
                      nyr=dd$nyr,
                      nvisit=dd$nvisit,
                      nind=dd$nind)

    ## Initial values 
    ## Zst <- apply(jags.data$X, c(1, 2), max)
    Zst <- array(1,dim=c(dd$nsp,dd$nsite,dd$nyr))
    make.inits <- function() {
      RNG <- parallel.seeds("base::BaseRNG", 1)
      c(list(Z=Zst), RNG[[1]])
    }
    inits1 <- make.inits()
    inits2 <- make.inits()
    inits3 <- make.inits()
    
    jags.out <- run.jags(model=model.txt,
                         monitor=get.params(),
                         data=jags.data,
                         inits=list(inits1,inits2,inits3),
                         n.chains=3,
                         burnin=n.burnin,
                         sample=floor(n.iter/n.thin),
                         thin=n.thin,
                         adapt=n.adapt,
                         method='rjags')
  }

  ## single-species run (not parallel)
  if(model %in% c('ss', 'ss_soroye')) {

    jags.data <- list(X=dd$X,
                      yr=dd$master.index[,'yr'],
                      site=dd$master.index[,'site'],
                      nyr=dd$nyr,
                      nsite=dd$nsite,
                      nind=dd$nind)

    ## Initial values 
    ## Zst <- apply(jags.data$X, c(1, 2), max)
    Zst <- matrix(1,nrow=dd$nsite,ncol=dd$nyr)
    jags.inits <- function(){
      list(Z=Zst)
    }
    
    ## Parameters monitored 
    jags.params <- get.params()

    jags.out <- run.jags(model=model.txt,
                         monitor=get.params(),
                         data=jags.data,
                         inits=jags.inits,
                         n.chains=3,
                         burnin=n.burnin,
                         sample=floor(n.iter/n.thin),
                         thin=n.thin,
                         adapt=n.adapt,
                         method='rjags')
  }
  
  list(jags.data=jags.data, jags.out=jags.out)
}
