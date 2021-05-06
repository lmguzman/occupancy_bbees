model.jags <- function() {

  ## repeat for each yr (with distinct priors)
  for(yr in 1:nyr) {
    
    ## specify priors
    alpha_psi[yr] ~ dnorm(0, 0.01)
    alpha_p[yr]   ~ dnorm(0, 0.01) 
    
    ## covar effects
    b_lp_samp[yr] ~ dnorm(0, 0.1)
    
    for(site in 1:nsite) { 

      ## occupancy
      logit(psi[site,yr]) <-
        alpha_psi[yr]
      Z[site,yr] ~ dbern(psi[site,yr])

      ## detection
      for(visit in 1:nvisit) {
        
        logit(p[site,yr,visit]) <-
          alpha_p[yr] +
          b_lp_samp[yr] * sampmat[site,yr,visit]
        mu_p[site,yr,visit] <- Z[site,yr] * p[site,yr,visit]
        X[site,yr,visit] ~ dbern(mu_p[site,yr,visit])
        
      }
    }
  }
}

## specify the parameters to be monitored
get.params <- function()
  c('alpha_psi',
    'alpha_p',
    'b_lp_samp',
    'Z')
