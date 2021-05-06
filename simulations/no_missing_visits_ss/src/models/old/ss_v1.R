model.jags <- function() {

  ## mean occupancy and detection
  psi.sp ~ dnorm(0,0.01)
  p.sp   ~ dnorm(0,0.01)

  ## effect of year on occupancy and detection
  psi.yr ~ dnorm(0,0.01)
  p.yr   ~ dnorm(0,0.01)
  
  ## likelihood
  for(yr in 1:nyr) {

    ## occupancy
    logit(psi[yr]) <-
      psi.sp +
      psi.yr * year[yr]
    ## detection
    logit(p[yr]) <-
      p.sp +
      p.yr * year[yr]
    
    for(site in 1:nsite) {

      Z[site,yr] ~ dbern(psi[yr])
      p.eff[site,yr] <- Z[site,yr] * p[yr]

      ## Detection 
      for(visit in 1:nvisit) {
        X[site,yr,visit] ~ dbern(p.eff[site,yr])
      }
    }
  }
}

## specify the parameters to be monitored
get.params <- function()
  c('psi.sp',
    'p.sp',
    'psi.yr',
    'p.yr',
    'Z')
