
source('soroye_reanalysis/src/initialize.R')

## ------------------------------------------------------------
## specify cases for multi-species components of plot

dir.AM <- 'soroye_reanalysis/saved/ms'
dir.EU <- 'soroye_reanalysis/saved/ms'

## case:
fn.AM <- 'detected_AM'
fn.EU <- 'detected_EU'

## ------------------------------------------------------------

## ------------------------------------------------------------
## load single-species output
load('soroye_reanalysis/saved/for_figures/delta.occ.soroye.RData', verbose=T)
## drop distinguendus for North America
delta.occ.AM.ss <- delta.occ.AM[names(delta.occ.AM)!='distinguendus']
delta.occ.EU.ss <- delta.occ.EU
## ------------------------------------------------------------

## ------------------------------------------------------------
## load multi-species runs and create quantities needed for plot

## calculate species-specific changes in occupancy
get.delta.occ <- function(sims.arr) {

  ## calculate delta.occ for figure
  psi.arr <- array(NA, dim=c(iter=dim(sims.arr)[1],
                             chain=dim(sims.arr)[2],
                             sp=my.data$nsp,
                             yr=2))
  for(sp in 1:my.data$nsp) {
    for(yr in 1:2) {
      mu.psi <- sims.arr[,,grep('^mu.psi$', vars)]
      psi.sp <- sims.arr[,,grep(sprintf('psi.sp\\[%d\\]',sp),vars)]
      psi.yr <- sims.arr[,,grep(sprintf('psi.yr\\[%d\\]',sp),vars)]
      psi.arr[,,sp,yr] <- mu.psi + psi.sp + psi.yr*(yr-1)
    }
  }

  ## expit before mean
  e.psi.arr <- expit(psi.arr)
  prop.psi.diff <- (e.psi.arr[,,,2]-e.psi.arr[,,,1])/e.psi.arr[,,,1]
  get.mean.bci <- function(x)
    c(mean=mean(x), quantile(x, p=c(0.025,0.975)))
  delta.occ <- apply(prop.psi.diff, 3, get.mean.bci)
  colnames(delta.occ) <- unique(my.data$sp.ID)
  delta.occ
}

## calculate mean/BCI for change in occupancy
get.delta.psi <- function(sims.arr) {
  psi.yr.1 <- expit(sims.arr[,,'mu.psi'])
  psi.yr.2 <- expit(sims.arr[,,'mu.psi'] + sims.arr[,,'mu.psi.yr'])
  psi.diff.chains <- (psi.yr.2-psi.yr.1)/psi.yr.1
  ## quantile(psi.diff.chains, p=c(0.025,0.975))
  c(mean=mean(psi.diff.chains),
    quantile(psi.diff.chains, p=c(0.025,0.975)))*100
}

fn <- fn.AM
load(sprintf('%s/%s.RData', dir.AM, fn), verbose=TRUE)
sims.arr <- aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
vars <- dimnames(sims.arr)[[3]]
delta.occ.AM.ms <- NA
delta.occ.AM.ms <- get.delta.occ(sims.arr)
psi.mean.bci.AM <- get.delta.psi(sims.arr)

fn <- fn.EU
load(sprintf('%s/%s.RData', dir.EU, fn), verbose=TRUE)
sims.arr <- aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
vars <- dimnames(sims.arr)[[3]]
delta.occ.EU.ms <- NA
delta.occ.EU.ms <- get.delta.occ(sims.arr)
psi.mean.bci.EU <- get.delta.psi(sims.arr)

## ------------------------------------------------------------

## ------------------------------------------------------------
## colours
col.ss <- make.transparent('darkgoldenrod3', alpha=0.5)
col.ms <- make.transparent('green4', alpha=0.5)

col.ss <- make.transparent('gray', alpha=1)
col.ms <- make.transparent('green4', alpha=0.5)
## col.ms <- make.transparent('red', alpha=0.5)
## ------------------------------------------------------------

make.fig <- function() {
  
  g <- function() {

    par(oma=c(1,1,0,0.5), mar=c(0.2, 2, 0, 0),
        mgp=c(1,0.2,0), tcl=0, cex.axis=1, cex.main=1, pty='s')

    breaks <- seq(from=-100,to=100,length=30)
    xlim <- c(-100,100)
    
    layout(matrix(1:2, 1, 2, byrow=TRUE))
    hist(delta.occ.AM.ss*100, breaks=breaks,
         col=col.ss, lty='blank',
         xlim=xlim, ylim=c(0,10), las=1,
         xlab='', ylab='', main='')
    title('North America', line=0.5)
    lines(x=c(0,0), y=c(0,14), col='black', lty=2)

    ## add mean + confidence interval
    points(x=-45.6,y=9.52,pch=16,cex=0.75,col=col.ss)
    arrows(x0=-45.6-3.4,
           y0=9.5,
           x1=-45.6+3.4,
           y1=9.5,
           col=col.ss,
           code=0, angle=90, length=0.02, lwd=1)

    if(length(delta.occ.AM.ms)>1) {
      hist(delta.occ.AM.ms['mean',]*100,
           breaks=breaks,
           col=col.ms, lty='blank', add=TRUE)

      ## add mean + BCI
      points(x=psi.mean.bci.AM['mean'],y=9.75,pch=16,cex=0.75,col=col.ms)
      arrows(x0=psi.mean.bci.AM['2.5%'],
             y0=9.75,
             x1=psi.mean.bci.AM['97.5%'],
             y1=9.75,
             col=col.ms,
             code=0, angle=90, length=0.02, lwd=1)
    }    

    hist(delta.occ.EU.ss*100,
         breaks=breaks,
         col=col.ss, lty='blank',
         xlim=xlim, ylim=c(0,10), las=1,
         xlab='', ylab='', main='')
    title('Europe', line=0.5)
    lines(x=c(0,0), y=c(0,14), col='black', lty=2)

    ## add mean + confidence interval
    points(x=-16.5,y=9.52,pch=16,cex=0.75,col=col.ss)
    arrows(x0=-16.5-4.9,
           y0=9.5,
           x1=-16.5+4.9,
           y1=9.5,
           col=col.ss,
           code=0, angle=90, length=0.02, lwd=1)

    if(length(delta.occ.EU.ms)>1) {
      hist(delta.occ.EU.ms['mean',]*100, breaks=breaks,
           col=col.ms, lty='blank', add=TRUE)

      ## add mean + BCI
      points(x=psi.mean.bci.EU['mean'],y=9.75,pch=16,cex=0.75,col=col.ms)
      arrows(x0=psi.mean.bci.EU['2.5%'],
             y0=9.75,
             x1=psi.mean.bci.EU['97.5%'],
             y1=9.75,
             col=col.ms,
             code=0, angle=90, length=0.02, lwd=1)
    }
    
    ## legend('topright',
    ##        legend=c('Survival',
    ##                 'Recruitment'),
    ##        lty=c(1,2),
    ##        bty='n')

    mtext('Number of species',
          side=2, line=-0.2, at=0.5, outer=TRUE)
    mtext('Change in probability of site occupancy (%)',
          side=1, line=-1, at=0.6, outer=TRUE)
  }
  
  pdf.f(g, 'soroye_reanalysis/figures/delta-occ.pdf',
        height=3.75, width=5.5)
}
make.fig()
