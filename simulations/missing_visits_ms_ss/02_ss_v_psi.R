library(parallel)
library(stringr)
source("simulations/src/initialize.R")
set.seed(1)

### Script for running the single-species occupancy model

## Missing visits and multiple values of change in occupancy probability psi.yr

## SS_all_all

out_dir <- "simulations/outputs/no_missing_visits_ms/"

file_sim <- list.files(paste0(out_dir, "saved_v_psi/sim.data/"))

run_02 <- function(f){
 
  load(paste0(out_dir, "saved_v_psi/sim.data/", f))
  
  file_name <-unlist(str_split(f, "_"))
  
  c <- 'all'
  
  model <- 'ss'
   
  source(sprintf('simulations/missing_visits_ms_ss/src/models/%s.R', model))
write.model(model.jags, con=sprintf('simulations/missing_visits_ms_ss/src/models/%s.txt', model))
  
  ## ------------------------------------------------------------
  ## run single-species models
  
  my.data <- prep.data.ss(sim.data)
  
  
  model.out <- lapply(my.data, function(x)
    run.model(dd=x,
              model=model,
              n.iter=1e3,
              n.burnin=1e3,
              n.adapt=1e3,
              n.thin=1e1))
  
  save(model.out, my.data, sim.data,
       file=paste0(out_dir, "saved_v_psi/", "ss/", c, 
                   
"_r_",file_name[3],"_mu.psi.yr_",file_name[5],"_mu.v.yr_",file_name[7]))
   
}

mclapply(file_sim, run_02, mc.cores = 40)
