library(parallel)
source("simulations/src/initialize.R")

set.seed(1)

### Script for running the simulations

## No missing visits and multiple values of change in occupancy psi.yr

# parameters for the simulation

out_dir <- "simulations/outputs/no_missing_visits_ms/"

all_scenarios <- expand.grid(nsp          = 50,
                             nsite        = 2000,
                             nyr          = 2,
                             nvisit       = 3,
                             mu.psi       = 0,
                             sigma.psi.sp = 0.5,
                             mu.psi.yr    = c(-1, -0.5, 0, 0.5, 1),
                             sigma.psi.yr = 0.2,
                             mu.p         = -0.5,
                             sigma.p.sp   = 0.5,
                             p.yr         = -0.5,
                             sigma.p.site   = 1.5,
                             r            = 1:10)


run_all_simulation_1 <-function(s, all_scenarios){


  sp.ranges <-
  get.soroye.ranges(location='simulations/src/ranges.RData')
  
  sim.data <- make.data(nsp = as.numeric(nrow(sp.ranges)),
                        nsite = as.numeric(ncol(sp.ranges)),
                        nyr = all_scenarios[s,'nyr'],
                        nvisit = all_scenarios[s,'nvisit'],
                        mu.psi = all_scenarios[s,'mu.psi'],
                        sigma.psi.sp = all_scenarios[s,'sigma.psi.sp'],
                        mu.psi.yr = all_scenarios[s,'mu.psi.yr'],
                        sigma.psi.yr = all_scenarios[s,'sigma.psi.yr'],
                        mu.p = all_scenarios[s,'mu.p'],
                        sigma.p.sp = all_scenarios[s,'sigma.p.sp'],
                        p.yr = all_scenarios[s,'p.yr'],
                        sigma.p.site = all_scenarios[s,'sigma.p.site'],
                        missing.visits=FALSE,
                        sp.range=sp.ranges)
  
  save(sim.data,
       file=paste0(out_dir, "saved_v_psi/", 
"sim.data/","_r_",all_scenarios[s,'r'],"_mu.psi.yr_",all_scenarios[s,'mu.psi.yr'],"_mu.psi_",all_scenarios[s,'mu.psi'], ".RData"))
  
}

lapply(1:nrow(all_scenarios), run_all_simulation_1, all_scenarios = all_scenarios)

