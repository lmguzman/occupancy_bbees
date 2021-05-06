## function to get Peter's ranges for use in simulation
get.soroye.ranges <- function(location) {
  load(location,verbose=TRUE)
  ## remove two species from North America
  ranges[['AM']][['terrestris']] <- list(NULL)
  ranges[['AM']][['lucorum']] <- list(NULL)

  ranges.comb <-
    mapply(function(a,b) c(a,b),
           a=ranges[['AM']], b=ranges[['EU']])
  nsp <- length(ranges.comb)
  max.site <- max(unlist(ranges.comb))
  sp.ranges <- matrix(0,nrow=nsp,ncol=max.site)
  names(dim(sp.ranges)) <- list('sp','site')
  for(ii in 1:nsp) sp.ranges[ii,unlist(ranges.comb[[ii]])] <- 1
  sp.ranges[,colSums(sp.ranges)>0]==1
}
