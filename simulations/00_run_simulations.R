

## This script runs all of the simulations and the occupancy models

##########NOTE occupancy model scripts were run on a cluster with 40 cores!!!#######

## The way it is organized:
# First, the simulations are divided on whether they allowed for no missing visits or missing visits
# Second, the models run were either single species (SS) or multi species models (MS)
# Third, the simulations varied either detection probability p.yr through time, or change in occupancy psi.yr


########## No missing visits ########

#### SS_all_all####

#### varying p (detection) ####

# runs simulation
source("simulations/no_missing_visits_ss/01_simulation_v_p.R")

# runs single species occupancy model over all sites and all visits
source("simulations/no_missing_visits_ss/02_ss_v_p.R")


########## No missing visits ########

#### MS_all_all ####
#multi-species occupancy model over all sites and all visits

#### Varying p (detection) ####

# runs simulation
source("simulations/no_missing_visits_ms/01_simulation_v_p.R")

# runs multi species occupancy model over all sites and all visits
source("simulations/no_missing_visits_ms/03_ms_all_v_p.R")

#### Varying psi (occupancy probability) ####

# runs simulation
source("simulations/no_missing_visits_ms/01_simulation_v_psi.R")

# runs multi species occupancy model over all sites and all visits
source("simulations/no_missing_visits_ms/03_ms_all_v_psi.R")



########## Missing visits ########

#### Varying p (detection) ####

# runs simulation
source("simulations/missing_visits_ms_ss/01_simulation_v_p.R")

### SS_all_all ####

# runs single species occupancy model over all sites and all visits

source("simulations/missing_visits_ms_ss/02_ss_v_p.R")

#### MS_range_all ####

# runs multi species occupancy model over species ranges and all visits

source("simulations/missing_visits_ms_ss/03_ms_all_v_p.R")

#### MS_range_detected ####

# runs multi species occupancy model over species ranges and where at least 1 species was detected

source("simulations/missing_visits_ms_ss/03_ms_detected_v_p.R")

#### MS_range_visits ####

# runs multi species occupancy model over species ranges and over the true visits

source("simulations/missing_visits_ms_ss/03_ms_visits_v_p.R")




#### Varying psi (occupancy probability) ####

# runs simulation
source("simulations/missing_visits_ms_ss/01_simulation_v_psi.R")

### SS_all_all ####

# runs single species occupancy model over all sites and all visits

source("simulations/missing_visits_ms_ss/02_ss_v_psi.R")

#### MS_range_all ####

# runs multi species occupancy model over species ranges and all visits

source("simulations/missing_visits_ms_ss/03_ms_all_v_psi.R")

#### MS_range_detected ####

# runs multi species occupancy model over species ranges and where at least 1 species was detected

source("simulations/missing_visits_ms_ss/03_ms_detected_v_psi.R")

#### MS_range_visits ####

# runs multi species occupancy model over species ranges and over the true visits

source("simulations/missing_visits_ms_ss/03_ms_visits_v_psi.R")
