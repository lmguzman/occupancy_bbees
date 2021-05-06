model.jags <- function() {

  ## mean occupancy and detection
  psi.sp ~ dnorm(0,0.01)
  p.sp   ~ dnorm(0,0.01)

  ## effect of year on occupancy and detection
  psi.yr ~ dnorm(0,0.01)
  p.yr   ~ dnorm(0,0.01)

  for(yr in 1:nyr) {
    ## occupancy
    logit(psi[yr]) <- psi.sp + psi.yr*(yr-1)
    ## detection
    logit(p[yr])   <-   p.sp +   p.yr*(yr-1)

    for(site in 1:nsite) {
      Z[site,yr] ~ dbern(psi[yr])
    }
  }
  
  ## likelihood
  for(ind in 1:nind) {
    p.eff[ind] <- Z[site[ind],yr[ind]]*p[yr[ind]]
    X[ind] ~ dbern(p.eff[ind])
  }
}

## specify the parameters to be monitored
get.params <- function()
  c('psi.sp',
    'p.sp',
    'psi.yr',
    'p.yr',
    'Z')
