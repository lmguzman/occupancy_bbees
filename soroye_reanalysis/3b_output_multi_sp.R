
source('soroye_reanalysis/src/initialize.R')

## --------------------------------------------------
## specify case

dir <- 'soroye_reanalysis/saved/ms'

fn <- 'detected_AM'
fn <- 'detected_EU'

## --------------------------------------------------
 
## --------------------------------------------------
## load data and create summary (skip if summary exists)
load(sprintf('%s/%s.RData', dir, fn), verbose=TRUE)

## ## create my own summary without gelman statistics, etc
## summ <- make.summary(res)

## create full summary
res.summary <-  add.summary(res)
get.summ <- function(pars) {
  summ <- round(cbind(
    res.summary$summary$statistics[pars,'Mean',drop=FALSE],
    res.summary$summary$quantiles[pars,c('2.5%', '97.5%'),drop=FALSE],
    Rhat=res.summary$psrf$psrf[pars,1]
  ), digits=3)
  colnames(summ)[1] <- 'mean'
  summ
}
vars <- rownames(res.summary$psrf$psrf)
summ <- get.summ(vars)

save(my.data,
     res,
     summ,
     file=file.path(dir, sprintf('%s_with_summ.RData', fn)))
## --------------------------------------------------

## --------------------------------------------------
load(sprintf('%s/%s_with_summ.RData', dir, fn), verbose=TRUE)

sims.arr <- aperm(sapply(res$mcmc, I, simplify='array'), c(1,3,2))
vars <- dimnames(sims.arr)[[3]]
## --------------------------------------------------

## --------------------------------------------------
## run-time info
res$burnin
res$sample
res$thin
sprintf('%2.1f hours', as.numeric(res$timetaken/60/60))
## --------------------------------------------------

## --------------------------------------------------
## examine summary
summ.paper <- summ[c('mu.psi',
                     'sigma.psi.sp',
                     'mu.psi.yr',
                     'sigma.psi.yr',
                     'mu.p',
                     'sigma.p.sp',
                     'p.yr',
                     'sigma.p.site'),]
summ.paper


## --------------------------------------------------
## plot chains
plot.chains(rr=res, 'mu.psi')
plot.chains(rr=res, 'mu.p')
plot.chains(rr=res, 'mu.psi.yr')
plot.chains(rr=res, 'p.yr')
plot.chains(rr=res, 'sigma.psi.sp')
plot.chains(rr=res, 'sigma.psi.yr')
plot.chains(rr=res, 'sigma.p.sp')
plot.chains(rr=res, 'sigma.p.site')
## --------------------------------------------------


## --------------------------------------------------
## calculate mean change in occupancy
occ.yr.1 <- expit(sims.arr[,,'mu.psi'])
occ.yr.2 <- expit(sims.arr[,,'mu.psi'] + sims.arr[,,'mu.psi.yr'])
psi.diff.chains <- (occ.yr.2-occ.yr.1)/occ.yr.1
psi.mean.bci <- c(mean=mean(psi.diff.chains),
                  quantile(psi.diff.chains, p=c(0.025,0.975)))
## calculate mean change in detectability
p.yr.1 <- expit(sims.arr[,,'mu.p'])
p.yr.2 <- expit(sims.arr[,,'mu.p'] + sims.arr[,,'p.yr'])
p.diff.chains <- (p.yr.2-p.yr.1)/p.yr.1
p.mean.bci <- c(mean=mean(p.diff.chains),
                quantile(p.diff.chains, p=c(0.025,0.975)))
## --------------------------------------------------

psi.mean.bci
p.mean.bci
