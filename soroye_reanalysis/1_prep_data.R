## data prep:
data.dir <- 'soroye_reanalysis/data'

library(parallel)
library(raster)

## -------------------------------------------------------------------
## load and collate Peter's data
dataOccu <- readRDS(file.path(data.dir, 'original/dataOccu.RDS'))
continent <- c('AM', 'EU')[dataOccu[[1]][['continent']]+1]

nsite  <- dataOccu[[1]][['nsites']]
nyear  <- dataOccu[[1]][['nyears']]
nvisit <- dataOccu[[1]][['nsurveys']]

quadID    <- dataOccu[[1]][['quadID']]
sampmat   <- dataOccu[[1]][['sampmat']]
continent <- c('AM', 'EU')[dataOccu[[1]][['continent']]+1]

## create occupancy array
occ.mats <- sapply(dataOccu, function(x) x$y)
occ.arr <- array(occ.mats,
                 dim=c(nsite=dim(dataOccu[[1]]$y)[1],
                       nvisit=dim(dataOccu[[1]]$y)[2],
                       nyr=dim(dataOccu[[1]]$y)[3],
                       nsp=length(dataOccu)),
                 dimnames=list(site=quadID,
                               visit=paste('v',1:nvisit,sep='_'),
                               yr=paste('y',1:nyear,sep='_'),
                               sp=names(dataOccu)))

## check that things match with the original
table(occ.arr[,,,3]==dataOccu[[3]]$y)
table(occ.arr[,,,17]==dataOccu[[17]]$y)
## add matching dimension names to sampmat
names(dim(sampmat)) <- c('nsite', 'nvisit', 'nyr')
dimnames(sampmat) <- dimnames(occ.arr)[1:3]

  ## info about which bees are on which continents
  ##
  ## NOTES to resolve here:
  ##
  ## norvegicus is described as being present on both continents, but
  ## does not have any detections in North America
  ##
  ## terrestris is North American, but listed as BOTH and has two
  ## detections in Europe - probably mis-identifications
  dataBeeContinent <- read.csv(paste0(data.dir, '/original/bombus_err_obs.csv'))
  ## re-order the latter
  dataBeeContinent <-
    dataBeeContinent[match(names(dataOccu), dataBeeContinent[,'species']),]
  ## exclude_from == 1 -> bee is only in europe
  ## exclude_from == 2 -> bee is only in NA
  ## exclude_from == NA -> bee is in both continents
  dataBeeContinent$continent <- rep('BOTH', nrow(dataBeeContinent))
  dataBeeContinent$continent[dataBeeContinent$exclude_from_cont==1] <- 'EU'
  dataBeeContinent$continent[dataBeeContinent$exclude_from_cont==2] <- 'AM'
  spInfo <- dataBeeContinent[,c('species','continent')]
  ## put in correct order
  spInfo <- spInfo[match(dimnames(occ.arr)[[4]],spInfo$species),]
  ## check that order is correct
  spInfo$species==dimnames(occ.arr)[[4]]

## -------------------------------------------------------------------

## put occ.arr and sampmat in a more intuitive format
occ.arr <- aperm(occ.arr, c(4,1,3,2))
sampmat <- aperm(sampmat, c(1,3,2))

save(nsite,
     nyear,
     nvisit,
     quadID,
     continent,
     spInfo,
     sampmat,
     occ.arr,
     file=file.path(data.dir, 'for_model/occ.data.RData'))
