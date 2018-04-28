
defineModule(sim, list(
  name = "scfmDisturbanceDriver",
  description = "generate parameters for the generic percolation model",
  keywords = "fire",
  authors=c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", scfmDisturbanceDriver = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmDisturbanceDriver.Rmd"),
  reqdPkgs = list("stats"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Caching the module"),
    defineParameter("returnInterval", "numeric", 1, NA, NA, "Years for scaling rates")
    ),
  inputObjects = bind_rows(
    expectsInput(objectName="scfmRegimePars", objectClass="list", desc="canonical regime description"),
    expectsInput(objectName="landscapeAttr", objectClass="list", desc="details of the current landscape structure"),
    expectsInput(objectName="cTable2", objectClass="data.frame", desc="lame-ass calibration data")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "scfmPars", objectClass = "list", desc = "parameters for threeStageFireModel")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.scfmDisturbanceDriver = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {

      sim <- Init(sim)

    },
       warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim){

  # if(is.null(sim$cTable2)){
  #   fn <- file.path(dataPath(sim), "FiresN1000MinFiresSize2NoLakes.csv")
  #   sim$cTable2 <- read.csv(fn)
  # } #shouldnt be necessary
  
  y <- sim$cTable2$y
  x <- sim$cTable2$p

  m.lw <- stats::lowess(y~x,iter=2)
  if (any(diff(m.lw$y)<0))
    warning("scfmDriver: lowess curve non-monotone. Proceed with caution")
  targetSize <- sim$scfmRegimePars$xBar/sim$landscapeAttr$cellSize - 1 
  pJmp <- approx(m.lw$y,m.lw$x,targetSize,rule=2)$y
  w<-sim$landscapeAttr$nNbrs
  w<-w/sum(w)
  hatPE<-sim$scfmRegimePars$pEscape
  #browser()
  #This won't actually work unless sim$nNbrs is 8
  minP0 <- hatP0(hatPE,sim$nNbrs)
  maxP0 <- hatP0(hatPE,floor(sum(w*0:8)))
  if (minP0 < maxP0){ 
    foo<-stats::optimise(escapeProbDelta,
                         interval=c(minP0, maxP0),
                         tol=1e-6,
                         w=w,
                         hatPE=hatPE)
    adjP0 <- foo$minimum
  }
  else
    adjP0 <- minP0
  
  #it is almost obvious that the true minimum must occur within the interval specified in the 
  #call to optimise, but I have not proved it, nor am I certain that the function being minimised is 
  #monotone.
  
  rate <- sim$scfmRegimePars$ignitionRate * sim$landscapeAttr$cellSize * P(sim)$returnInterval 
  pIgnition <- rate #approximate Poisson arrivals as a Bernoulli process at cell level.
  #for Poisson rate << 1, the expected values are the same, partially accounting
  #for multiple arrivals within years. Formerly, I used a poorer approximation
  #where 1-p = P[x==0 | lambda=rate] (Armstrong and Cumming 2003).
  #See the .Rmd file for details.

  if (pIgnition >= 1 || pIgnition < 0)
    stop("illegal estimate of igntition probability")
  if (pIgnition > 0.01)
    warning("Poisson variance approximation will be poor")
  
  sim$scfmPars<-list(pSpread=pJmp,
                     p0=adjP0,
                     naiveP0=minP0, 
                     pIgnition=pIgnition,
                     maxBurnCells=as.integer(round(sim$scfmRegimePars$emfs/sim$landscapeAttr$cellSize)))

  return(invisible(sim))
  
  }


.inputObjects <- function(sim) {

  if (!suppliedElsewhere("cTable2",sim)){
    fn <- file.path(dataPath(sim), "FiresN1000MinFiresSize2NoLakes.csv")
    sim$cTable2 <- read.csv(fn)
  }
  
  return(invisible(sim))
}

