
source('soroye_reanalysis/src/initialize.R')

library(maptools)
library(parallel)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(spatstat)

library(RColorBrewer)

prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

load('soroye_reanalysis/data/for_model/bumble.sites.gis.Rdata', verbose=TRUE)
load('soroye_reanalysis/data/for_model/ranges.Rdata', verbose=TRUE)
load('soroye_reanalysis/data/for_model/occ.data.RData', verbose=TRUE)
det.arr <- apply(occ.arr, 2:4, function(x) any(x>0)*1)

dd.countries <- readOGR(dsn='soroye_reanalysis/data/shapefiles/Countries_WGS84/Countries_WGS84.shp')

## re-project
dd.countries <- spTransform(dd.countries, CRS(prj))
dd.sites <- spTransform(dd.sites, CRS(prj))
dd.bumble <- spTransform(dd.bumble, CRS(prj))

## bounding box for North America
dd.box.AM <- bbox2SP(n=77,
                     e=-55,
                     s=13,
                     w=-165,
                     proj4string=CRS(prj))

## bounding box for EUROPE
dd.box.EU <- bbox2SP(n=73,
                     e=42,
                     s=30,
                     w=-26,
                     proj4string=CRS(prj))

dd.AM <- intersect(dd.countries, dd.box.AM)
## dd.AM <- dd.countries[dd.countries$CNTRY_NAME %in%
##                       c('Canada',
##                         'United States',
##                         'Mexico'),]
dd.EU <- intersect(dd.countries, dd.box.EU)

## create detection info
dir.AM <- file.path('soroye_reanalysis/saved/ms')
dir.EU <- file.path('soroye_reanalysis/saved/ms')
fn.AM <- 'detected_AM'
fn.EU <- 'detected_EU'

## calculate mean detection for an average species
get.p.site <- function() {
  sims.arr <- aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
  p.arr <- array(NA, dim=c(my.data$nsite,
                           my.data$nyr))
  for(site in 1:my.data$nsite) {
    qID <- my.data$site.quadID[site]
    if(qID %in% my.data$site.quadID) {
      p.arr[site,] <- c(0,0)
    }
    if(qID %in% quadID) {
      p.arr[site,] <- apply(det.arr[match(qID, quadID),,,drop=FALSE],
                            2, sum)
    }
  }
  p.arr
}

## North America
load(sprintf('%s/%s.RData', dir.AM, fn.AM), verbose=TRUE)
my.data.AM <- my.data
res.AM <- res
p.arr.AM <- get.p.site()/3

## Europe
load(sprintf('%s/%s.RData', dir.EU, fn.EU), verbose=TRUE)
my.data.EU <- my.data
res.EU <- res
p.arr.EU <- get.p.site()/3

make.fig <- function() {
  
  g <- function() {

    layout(matrix(1:4, 2, 2, byrow=TRUE))
    par(oma=c(0.1,0.1,0.1,0.1), mar=c(0,0,1,1),
        mgp=c(-1,0,0), tcl=0, cex.axis=1, cex.main=1, pty='s', xpd=TRUE)

    make.panel <- function(continent, era) {
      
      if(continent=='AM') {
        dd.cont <- dd.AM
        p.arr <- p.arr.AM
        my.data <- my.data.AM
      }
      if(continent=='EU') {
        dd.cont <- dd.EU
        p.arr <- p.arr.EU
        my.data <- my.data.EU
      }

      dd.sites.cont <- dd.sites[dd.sites$quadID %in% my.data$site.quadID,]
      ind <- match(dd.sites.cont$quadID, my.data$site.quadID)

      cols <- brewer.pal(9, 'BuGn')
      
      title <- ifelse(continent=='AM', c('Historical', 'Current')[era], '')


      if(continent=='AM') {
        xmin <- extent(dd.cont)[1]
        xmax <- extent(dd.cont)[2]
      }
      if(continent=='EU') {
        xmin <- extent(dd.cont)[1]#-10
        xmax <- extent(dd.cont)[2]#+10
      }

      plot(dd.cont,
           xlim=c(xmin,xmax),
           col=cols[8],
           border=cols[8],
           main=title,
           cex.main=1.5,
           lwd=0.1)
      col <- grey(1-p.arr[ind,era])
      col[p.arr[ind,era]==0] <- brewer.pal(9, 'OrRd')[7]
      
      plot(dd.sites.cont, add=TRUE, lwd=0.2, border=col, col=col)

      if(continent=='EU' & era==1)
        legend(x=par('usr')[1] + (par('usr')[2]-par('usr')[1])*1.05,
               y=par('usr')[3] + (par('usr')[4]-par('usr')[3])*0.6,
               legend=3:0,
               pch=rep(15,4),
               col=id(col),
               cex=1.5,
               pt.cex=3,
               bty='n', inset=c(-0.5,0),
               xpd=NA)
    }
    make.panel(continent='AM', era=1)
    make.panel(continent='AM', era=2)
    make.panel(continent='EU', era=1)
    make.panel(continent='EU', era=2)
    
  }
  
  pdf.f(g, 'soroye_reanalysis/figures/visitation-in-space.pdf',
        height=10, width=10)
}
make.fig()
