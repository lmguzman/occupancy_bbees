library(parallel)
source("simulations/src/initialize.R")
set.seed(1)

### Script for running simulations

## Missing visits and multiple values of change in detection p.yr

# parameters for the simulation

out_dir <- "simulations/outputs/missing_visits_ms_ss/"

all_scenarios <- expand.grid(nsp          = 50,
                             nsite        = 2000,
                             nyr          = 2,
                             nvisit       = 3,
                             mu.psi       = 0,
                             sigma.psi.sp = 0.5,
                             mu.psi.yr    = 0,
                             sigma.psi.yr = 0.2,
                             mu.p         = -0.5,
                             sigma.p.sp   = 0.5,
                             p.yr      = c(-1, -0.5, 0, 0.5, 1),
                             sigma.p.site   = 1.5,
                             mu.v = 0,
                             mu.v.yr= c(0, -0.5),
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
                        mu.v = all_scenarios[s,'mu.v'],
                        mu.v.yr = all_scenarios[s,'mu.v.yr'],
                        missing.visits=TRUE,
                        sp.range=sp.ranges)
  
  save(sim.data,
       file=paste0(out_dir, "saved_v_p/", "sim.data/",
                   
"_r_",all_scenarios[s,'r'],"_p.yr_",all_scenarios[s,'p.yr'],"_mu.v.yr_",all_scenarios[s,'mu.v.yr'], 
".RData"))
  
}

lapply(1:nrow(all_scenarios), run_all_simulation_1, all_scenarios = 
all_scenarios)

