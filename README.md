# Using historical data to estimate bumble bee occurrence: variable trends across species provide little support for community-level declines

This repository contains code and data associated with “Using historical data to estimate bumble bee occurrence: variable trends across species provide little support for community-level declines.”

Guzman LM, Johnson SA, Mooers AO, M'Gonigle LK. 2021 Using historical data to estimate bumble bee occurrence: variable trends across species provide little support for community-level declines. Biological Conservation. 257: 109141 [DOI](https://www.sciencedirect.com/science/article/pii/S0006320721001932)

## In a nutshell

Bumble bees are important pollinators globally and many species of bumble bees are known to be declining. Therefore, quantifying such declines is of utmost importance for global ecosystem services. Historical bumble bee records in North America and Europe were used by [Soroye et al. (2020)](https://science.sciencemag.org/content/367/6478/685.abstract?casa_token=kOM7ctWB0twAAAAA:9QvGYDnuWb3X7kqq20HiXA-c5GGn91-rBTsW8kKawUzxu9vCmmLwZWonP_hgCMUtGRnLfmWqFiKYK1E) to estimate the magnitude of species’ declines. Differnet modelling choices can interact with the patterns in the underlying data in ways that we did not understand till now. 

In this manuscript we use simulated data to identify how modeling species occupancy across sites that extend beyond a species’ range or visits that never occurred bias inferences about historical changes in occupancy. We compare a suite of alternative models on this simulated data. We then apply the most robust model to the bumble bee dataset and provide revised estimates for the status of bumble bees in North America and Europe.

## Getting around this repository

This repository is organized in two sections. 

(i) [Simulations](https://github.com/lmguzman/occupancy_bbees/tree/master/simulations): First we simulate data that emulates historical museum records, where each species occupy potentially overlapping sites (species ranges). Among occupied sites, a species can only be detected at a site if that site is visited. We modelled site visitation in two ways. First we considered a scenario where each site is visited in each time interval in each era (no missing visits). Second, we considered a scenario where not every site received a visit in every time interval (missing visits). The simulation and output scripts are organized based on the missing visits or no missing visits scenarios. Then we compare the performance of 5 occupancy models: SS<sub>all,all</sub>, MS<sub>all,all</sub>, MS<sub>range,all</sub>, MS<sub>range,visits</sub>, MS<sub>range,detected</sub>. For full descriptions see the manuscript. 

(i) [Soroye_reanalysis](https://github.com/lmguzman/occupancy_bbees/tree/master/soroye_reanalysis): Second we use the best performing model MS<sub>range,detected</sub> to re-analyze the historical data used by Soroye et al. (2020). 
