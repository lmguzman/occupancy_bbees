library(parallel)
library(stringr)
source("simulations/src/initialize.R")

set.seed(1)

### Script for running the multi-species occupancy model

## Missing visits and multiple values of change in occupancy probability psi.yr

## MS_range_visits

out_dir <- "simulations/outputs/no_missing_visits_ms/"

file_sim <- list.files(paste0(out_dir, "saved_v_psi/sim.data/"))

run_03 <- function(f){
  load(paste0(out_dir, "saved_v_psi/sim.data/", f))
  
  file_name <-unlist(str_split(f, "_"))
  
  c <- 'visits'

   model <- 'ms'
## source JAGS model
source(sprintf('simulations/missing_visits_ms_ss/src/models/%s.R', model))
write.model(model.jags, con=sprintf('simulations/missing_visits_ms_ss/src/models/%s.txt', model))
  
  my.data <- prep.data.ms(limit.to.visits=c, sim.data)
  
  model.out <- run.model(dd=my.data,
                         model='ms',
                         n.iter=1e3,
                         n.burnin=1e3,
                         n.adapt=1e3,
                         n.thin=1e1)
  
  save(model.out, my.data, sim.data,
       file=paste0(out_dir, "saved_v_psi/", "ms/", c, 
                   
"_r_",file_name[3],"_mu.psi.yr_",file_name[5],"_mu.v.yr_",file_name[7]))
}

mclapply(file_sim, run_03, mc.cores = 40)
