library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(tidyr)

## This script compiles all of the outputs of the simulations

## The way it is organized:
# First, the simulations are divided on whether they allowed for no missing visits or missing visits
# Second, the models run were either single species (SS) or multi species models (MS)
# Third, the simulations varied either detection probability p.yr through time, or change in occupancy psi.yr


#### Functions needed ####

##  set all the functions needed to compile outputs

source("simulations/src/initialize.R")

get.posterior <- function(x, param) {
  sims.mat <- do.call(rbind, x$jags.out$mcmc)
  c(mean = mean(sims.mat[,param]), quantile(sims.mat[,param], 0.025), quantile(sims.mat[,param], 0.975))
}

get.summ <- function(pars, jags.summary) {
  summ <- round(cbind(
    jags.summary$summary$statistics[pars,'Mean',drop=FALSE],
    jags.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=jags.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}

## variables of interest 
vars.of.interest <- c('mu.psi',
                      'sigma.psi.sp',
                      'mu.psi.yr',
                      'sigma.psi.yr',
                      'mu.p',
                      'sigma.p.sp',
                      'p.yr',
                      'sigma.p.site')



########## single species runs ########

extract_ss <- function(scenario, var){

  output_dir <- paste0("simulations/outputs/", scenario, "/saved_v_",var,"/ss/")

  files_var_p <- list.files(output_dir)
  
  percent_change <- data.frame() 
  
  species_change <- data.frame()
  
  for(f in files_var_p){
    
    ## load files and extract names from file name ##
    
    load(paste0(output_dir, f), verbose = 1)
    
    name_f <- unlist(str_split(f, "_"))
    
    case <- name_f[1]
    
    r <- name_f[3]
    
    var1 <- name_f[5]
    
    var2 <- str_remove(name_f[7], ".RData")
    
    ## get actual parameters ##
    
    actual.param <- (expit(sim.data$mu.psi + sim.data$mu.psi.yr*(sim.data$nyr-1))- expit(sim.data$mu.psi))/expit(sim.data$mu.psi)
    
    actual.param.sp <- ((expit(sim.data$psi.sp + sim.data$psi.yr*(sim.data$nyr-1)) - expit(sim.data$psi.sp))/expit(sim.data$psi.sp))*100
    
    actual.ch.sp.df <- data.frame(actual.change = actual.param.sp, sp = as.character(1:sim.data$nsp))
    
    ## extract Z matrix ##
    
    Z <- lapply(model.out, FUN = function(x) do.call(rbind, x$jags.out$mcmc)[,grep('Z\\[', colnames(x$jags.out$mcmc[[1]]))])
    
    ## calculate mean occupancy using pipeline by Soroye. 
    
    Z.mean <- Z %>% 
      map(~colMeans(.x)) %>% 
      map_df(~data.frame(oc = .x, site_time = names(.x)), .id = 'sp') %>% 
      separate(site_time, c('site', 'time'), sep = ',') %>% 
      mutate(site = str_extract(site, "\\d+"), time = str_extract(time, "\\d+")) %>% 
      filter(time %in% c('1', as.character(sim.data$nyr))) %>% 
      mutate(time = ifelse(time == '1', "baseline", 'current')) %>% 
      pivot_wider(names_from = time, values_from = oc) 
    
    ## Estimate percentage change in occupancy removing outliers ## 
    
    change_by_species <- Z.mean %>% 
      filter(baseline == 1 | current == 1) %>% 
      group_by(sp) %>% 
      summarise(mean_baseline= mean(baseline), mean_current= mean(current)) %>% 
      mutate(per_change = ((mean_current-mean_baseline)/mean_baseline)*100) %>% 
      mutate(var1 = var1, var2 = var2, r=r,  case = case) %>% 
      left_join(actual.ch.sp.df) %>% 
      filter(per_change > -100 & per_change < 100)
    
    mean_change_by_species = data.frame(mean = mean(change_by_species$per_change), 
                                        se = sd(change_by_species$per_change)/sqrt(length(change_by_species$per_change)),
                                        var1 = var1, var2 = var2, r=r,  case = case, 
                                        actual = actual.param, mean.actual = mean(change_by_species$actual.change), 
                                        se.actual = sd(change_by_species$actual.change)/sqrt(length(change_by_species$actual.change)))
    
    # bind results across multiple parameters 
    
    species_change <- bind_rows(species_change, change_by_species)
    
    percent_change <- bind_rows(percent_change, 
                                mean_change_by_species)
    
  }
  
  colnames(species_change)[colnames(species_change)=='var1'] <- name_f[4]
  colnames(species_change)[colnames(species_change)=='var2'] <- name_f[6]
  
  colnames(percent_change)[colnames(percent_change)=='var1'] <- name_f[4]
  colnames(percent_change)[colnames(percent_change)=='var2'] <- name_f[6]
  
  write.csv(percent_change, paste0("simulations/outputs/compiled_outputs/SS_all_all_community_var_",var,"_", scenario, ".csv"), row.names = FALSE)
  write.csv(species_change, paste0("simulations/outputs/compiled_outputs/SS_all_all_species_var_",var,"_", scenario,".csv"), row.names = FALSE)
  
}


########## multi-species runs########

extract_ms <- function(scenario, var){
  
  output_dir <- paste0("simulations/outputs/", scenario, "/saved_v_",var,"/ms/")
  
  file_names_ms <- list.files(output_dir)
  
  community_change <- data.frame()
  
  species_change <- data.frame()
  
  for(f in file_names_ms){
    
    #load file and extract the parameters from the name#
    
    load(paste0(output_dir, f), verbose=1)
    
    name_f <- unlist(str_split(f, "_"))
    
    case <- name_f[1]
    
    r <- name_f[3]
    
    var1 <- name_f[5]
    
    var2 <- str_remove(name_f[7], ".RData")
    
    ## extract jags model outputs ##
    
    jags.out <- model.out$jags.out
    jags.data <- model.out$jags.data
    
    ## create summary from jags output
    
    jags.summary <-  add.summary(jags.out, vars = c(vars.of.interest, 'psi.yr', 'psi.sp'))
    vars <- rownames(jags.summary$psrf$psrf)
    summ <- get.summ(vars, jags.summary)
    
    # extract actual values
    
    vals.act <- data.frame(actual.val = unlist(sim.data[vars.of.interest])) %>% 
      rownames_to_column('var.names') %>% 
      bind_rows(data.frame(var.names = "per_chage_bci", actual.val =   ((expit(sim.data$mu.psi + sim.data$mu.psi.yr) -  expit(sim.data$mu.psi))/expit(sim.data$mu.psi))*100))
    
    # summary of values of interest
    
    summ.est <- summ[vars.of.interest,]
    
    # extract chains for intercept and slope of psi
    
    mu.psi_chain <- sapply(1:3, function(x) jags.out$mcmc[[x]][,"mu.psi"])
    mu.psi.yr_chain <- sapply(1:3, function(x) jags.out$mcmc[[x]][,"mu.psi.yr"])
    
    # calculate percent change
    
    per_change_chain <- ((expit(mu.psi_chain + mu.psi.yr_chain) -  expit(mu.psi_chain))/expit(mu.psi_chain))*100
    
    per_chage_bci <- c(mean = mean(c(per_change_chain)), quantile(c(per_change_chain), 0.025), quantile(c(per_change_chain), 0.975), Rhat = NA)
    
    # create summary with model output and percent change 
    
    summary_df <- as.data.frame(rbind(summ.est, per_chage_bci)) %>% 
      rownames_to_column('var.names') %>% 
      left_join(vals.act) %>% 
      mutate(var1 = var1, var2 = var2, r=r,  case = case, sp = 'ms')
    
    # bind multiple simulation runs
    
    community_change <- rbind(community_change, summary_df)
    
    ## look at species-specific year effects
    
    psi.yr <- data.frame(mean = summ[vars[grep('psi.yr\\[', vars)],'mean']) %>%
      rownames_to_column('sp.names') %>%
      left_join(data.frame(sp.names = paste0("psi.yr[",1:sim.data$nsp,"]"), actua.val = sim.data$psi.yr)) %>%
      mutate(var1 = var1, var2 = var2, r=r,  case = case, sp = 'ms', var.names = 'psi.yr')
    
    
    ## look at species-specific effects
    
    psi.sp <- data.frame(mean = summ[vars[grep('psi.sp\\[', vars)],'mean']) %>%
      rownames_to_column('sp.names') %>%
      left_join(data.frame(sp.names = paste0("psi.sp[",1:sim.data$nsp,"]"), actua.val = sim.data$psi.sp)) %>%
      mutate(var1 = var1, var2 = var2, r=r,  case = case, sp = 'ms', var.names = 'psi.sp')
    
    ## calculate species specific percent change
    
    psi.sp.names <- colnames(jags.out$mcmc[[1]])[str_detect(colnames(jags.out$mcmc[[1]]), "psi.sp\\[\\d+\\]")]
    
    psi.sp.yr.names <- colnames(jags.out$mcmc[[1]])[str_detect(colnames(jags.out$mcmc[[1]]), "psi.yr\\[\\d+\\]")]
    
    psi.sp_chain <- do.call(rbind, lapply(1:3, function(x) jags.out$mcmc[[x]][,psi.sp.names]))
    psi.sp.yr_chain <- do.call(rbind, lapply(1:3, function(x) jags.out$mcmc[[x]][,psi.sp.yr.names]))
    
    per_change_sp_chain <- ((expit(psi.sp_chain + psi.sp.yr_chain) -  expit(psi.sp_chain))/expit(psi.sp_chain))*100
    
    per_change_sp_actual <- ((expit(sim.data$psi.sp + sim.data$psi.yr) -  expit(sim.data$psi.sp))/expit(sim.data$psi.sp))*100
    
    per_chage_sp_bci <- data.frame(mean = colMeans(per_change_sp_chain)) %>% 
      rownames_to_column('sp.names') %>%
      left_join(data.frame(sp.names = paste0("psi.sp[",1:sim.data$nsp,"]"), actua.val = per_change_sp_actual)) %>%
      mutate(var1 = var1, var2 = var2, r=r,  case = case, sp = 'ms', var.names = 'per_change')
    
    multi_sp <- rbind(psi.yr, psi.sp, per_chage_sp_bci)
    
    # bind outputs between runs 
    
    species_change <- rbind(species_change, multi_sp)
    
  }
  
  colnames(species_change)[colnames(species_change)=='var1'] <- name_f[4]
  colnames(species_change)[colnames(species_change)=='var2'] <- name_f[6]
  
  colnames(community_change)[colnames(community_change)=='var1'] <- name_f[4]
  colnames(community_change)[colnames(community_change)=='var2'] <- name_f[6]
  
  name_scenario <- ifelse(scenario == "no_missing_visits_ms", "all_all", "range")
  
  write.csv(community_change, paste0("simulations/outputs/compiled_outputs/MS_",name_scenario,"_community_var_",var,"_", scenario,".csv"), row.names = FALSE)
  write.csv(species_change, paste0("simulations/outputs/compiled_outputs/MS_",name_scenario,"_species_var_",var,"_", scenario,".csv"), row.names = FALSE)
  
}



