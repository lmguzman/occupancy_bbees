## data prep:
library(maptools)
library(parallel)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(spatstat)

data.dir <- 'soroye_reanalysis/data'

prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

## ------------------------------------------------------------
## load raster data and convert to polygons
continent <- readRDS(file.path(data.dir, 'original/bombus_continent.RDS'))
projection(continent)
dd.sites <- rasterToPolygons(continent)
names(dd.sites@data) <- 'continent'
dd.sites@data$quadID <- which(!is.na(values(continent)))
dd.sites@data$continent <- c('AM','EU')[dd.sites@data$continent]
## ------------------------------------------------------------

## ------------------------------------------------------------
## load bumble bee data
data.original <- readRDS(file.path(data.dir, 'original/bombus_clean.RDS'))

## put into spatial points data-frame
coords <- data.original[,c('longitude', 'latitude')]
dd.attr <- data.original[,c('species', 'continent')]
dd.bumble <- SpatialPointsDataFrame(coords=coords,
                                    data=dd.attr,
                                    proj4string=CRS("+proj=longlat +datum=WGS84"))
## ------------------------------------------------------------

## ------------------------------------------------------------
## re-project both
dd.sites <- spTransform(dd.sites, CRS(prj))
dd.bumble <- spTransform(dd.bumble, CRS(prj))
## ------------------------------------------------------------

## ------------------------------------------------------------
## save both
save(dd.sites, dd.bumble,
     file=file.path(data.dir,'for_model/bumble.sites.gis.Rdata'))
## ------------------------------------------------------------

## ------------------------------------------------------------
## calculate and save ranges
tab.bees.all <- sort(names(table(dd.bumble$species)))

get.range <- function(bee, cont) {
  cat(bee, cont, '\n')
  
  dd.continent <- dd.sites[dd.sites@data$continent==cont,]
  dd.focal.bee <- dd.bumble[dd.bumble$species==bee,]

  tmp <- over(dd.focal.bee, dd.continent)
  keep <- !is.na(tmp[,'continent'])
  if(!any(keep)) return(NULL)

  ## subset to focal bee
  dd.focal.bee <- dd.focal.bee[keep,]

  ## create convex hull around sites
  region <- gConvexHull(dd.focal.bee)

  ## figure out which sites intersect the convex hull
  keep <- !is.na(over(dd.continent, region))
  dd.continent.keep <- dd.continent[keep,]

  dd.continent.keep$quadID
}
ranges <- list(AM=sapply(tab.bees.all, get.range, cont='AM'),
               EU=sapply(tab.bees.all, get.range, cont='EU'))
save(ranges, file=file.path(data.dir,'for_model/ranges.RData'))
## ------------------------------------------------------------
