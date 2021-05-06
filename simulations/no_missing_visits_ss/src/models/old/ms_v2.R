model.jags <- function() {

  ## mean occupancy and detection
  mu.psi ~ dnorm(0,0.01)
  mu.p   ~ dnorm(0,0.01)
  
  ## random effect of species on occupancy and detection
  sigma.psi.sp ~ dunif(0,10)
  sigma.p.sp ~ dunif(0,10)
  tau.psi.sp <- 1/(sigma.psi.sp*sigma.psi.sp)
  tau.p.sp   <- 1/(sigma.p.sp*sigma.p.sp)
  for(sp in 1:nsp) {
    psi.sp[sp] ~ dnorm(0, tau.psi.sp)
    p.sp[sp]   ~ dnorm(0, tau.p.sp)
  }

  ## effect of year on occupancy and detection (random slopes)
  mu.psi.yr    ~ dnorm(0,0.01)
  mu.p.yr      ~ dnorm(0,0.01)
  sigma.psi.yr ~ dunif(0,10)
  sigma.p.yr   ~ dunif(0,10)
  tau.psi.yr <- 1/(sigma.psi.yr*sigma.psi.yr)
  tau.p.yr   <- 1/(sigma.p.yr*sigma.p.yr)
  for(sp in 1:nsp) {
    psi.yr[sp] ~ dnorm(mu.psi.yr, tau.psi.yr)
    p.yr[sp]   ~ dnorm(mu.p.yr,   tau.p.yr)
  }

  for(sp in 1:nsp) {
    for(yr in 1:nyr) {
      ## occupancy
      logit(psi[sp,yr]) <- mu.psi + psi.sp[sp] + psi.yr[sp]*(yr-1)
      ## detection
      logit(p[sp,yr])   <-   mu.p +   p.sp[sp] +   p.yr[sp]*(yr-1)
    }
  }

  for(sp in 1:nsp) {
    for(site in 1:nsite) {
      for(yr in 1:nyr) {
        Z[sp,site,yr] ~ dbern(psi[sp,yr])
      }
    }
  }
  
  ## likelihood
  for(ind in 1:nind) {
    p.eff[ind] <- Z[sp[ind],site[ind],yr[ind]]*p[sp[ind],yr[ind]]
    X[ind] ~ dbern(p.eff[ind])
  }
}


## specify the parameters to be monitored
get.params <- function()
  c('mu.psi',
    'mu.p',
    'psi.sp',
    'sigma.psi.sp',
    'p.sp',
    'sigma.p.sp',
    'psi.yr',
    'mu.psi.yr',
    'sigma.psi.yr',
    'p.yr',
    'mu.p.yr',
    'sigma.p.yr',
    'Z')
