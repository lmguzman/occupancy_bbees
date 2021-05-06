
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

dd.countries <- readOGR(dsn='soroye_reanalysis/data/shapefiles/Countries_WGS84/Countries_WGS84.shp')

## re-project
dd.countries <- spTransform(dd.countries, CRS(prj))
dd.sites <- spTransform(dd.sites, CRS(prj))
dd.bumble <- spTransform(dd.bumble, CRS(prj))

## bounding box for North America
dd.box <- bbox2SP(n=77,
                  e=-55,
                  s=13,
                  w=-165,
                  proj4string=CRS(prj))

dd.AM <- intersect(dd.countries, dd.box)
## dd.AM <- dd.countries[dd.countries$CNTRY_NAME %in%
##                       c('Canada',
##                         'United States',
##                         'Mexico'),]
dd.sites.AM <- dd.sites[dd.sites$continent=='AM',]

cols <- brewer.pal(9, 'BuGn')

make.fig <- function() {
  
  g <- function() {

    par(oma=c(0.1,0.1,0.1,0.1), mar=c(0, 0, 0, 0),
        mgp=c(1,0.2,0), tcl=0, cex.axis=1, cex.main=1, pty='s')
    plot(dd.AM, border=cols[8], col=cols[8], lwd=0.1)
    plot(dd.sites.AM, add=TRUE, lwd=0.2, col=gray(0.9), border='black')
    
    ## add affinis
    sp <- 'affinis'
    ## sp <- 'bohemicus'
    ## sp <- 'occidentalis'
    ## sp <- 'terrestris'
    ## sp <- 'lucorum'
    ## sp <- 'cryptarum'
    ## sp <- 'distinguendus'
    ## sp <- 'vandykei'
    ## sp <- 'fervidus'

    dd.affinis <- dd.sites[dd.sites$quadID %in%
                           ranges[['AM']][[sp]],]
    plot(dd.affinis, add=TRUE, lwd=0.2, col='red', border='white')
    plot(dd.bumble[dd.bumble$species==sp,],
         col='blue', cex=0.3, pch=16, add=TRUE)

    legend(x=par('usr')[1] + (par('usr')[2]-par('usr')[1])*0.05,
           y=par('usr')[3] + (par('usr')[4]-par('usr')[3])*0.5,
           legend=c('Site outside range',
                    'Site inside range',
                    'Species detected'),
           pch=c(15,15,16),
           col=c(gray(0.9),'red','blue'),
           cex=0.7,
           pt.cex=c(1,1,0.7),
           bty='n',
           xpd=NA)

  }
  
  pdf.f(g, 'soroye_reanalysis/figures/sp-ranges.pdf',
        height=5, width=5)
}
make.fig()
