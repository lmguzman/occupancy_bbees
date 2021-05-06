library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(tidyr)

source('simulations/src/compile_results_fns.R')

## This script compiles all of the outputs of the simulations

## The way it is organized:
# First, the simulations are divided on whether they allowed for no missing visits or missing visits
# Second, the models run were either single species (SS) or multi species models (MS)
# Third, the simulations varied either detection probability p.yr through time, or change in occupancy psi.yr


########## No missing visits ########

#### SS_all_all####

#single species occupancy model over all sites and all visits

#### varying p (detection) ####

extract_ss("no_missing_visits_ss", "p")


########## No missing visits ########

#### MS_all_all ####
#multi-species occupancy model over all sites and all visits

#### Varying p (detection) ####

extract_ms("no_missing_visits_ms", "p")

#### Varying psi (occupancy probability) ####

extract_ms("no_missing_visits_ms", "psi")


########## Missing visits ########

#### MS_range_all ####
#### MS_range_detected ####
#### MS_range_visits ####

#### Varying p (detection) ####

extract_ms("missing_visits_ms_ss", "p")

#### Varying psi (occupancy probability) ####

extract_ms("missing_visits_ms_ss", "psi")


### SS_all_all ####

### Varying p (detection) ####

extract_ss("missing_visits_ms_ss", "p")

#### Varying psi (occupancy probability) ####

extract_ss("missing_visits_ms_ss", "psi")

