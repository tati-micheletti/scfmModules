defineModule(sim, list(
  name = "scfmSpread",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve", "George"), "Cumming", email="stevec@sbf.ulaval.ca", role="aut")),
  childModules = character(),
  version = numeric_version("1.2.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmSpread.Rmd"),
  reqdPkgs = list("raster","data.table","magrittr"),
  parameters = rbind(
    defineParameter("pSpread","numeric", 0.23, 0, 1, desc="Percolation spread probability"),
    defineParameter("pOverRide","numeric", NA, 0, 1, desc="Set it in the pars no matter the driver says"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="Time interval between burn events"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time at which to initiate burning"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "Time of first Plot event, or NA"),
    defineParameter(".statsInitialTime", "numeric", 0, NA, NA, "Time of first Plot event, or NA"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "Timeunits beteen plot events")
  ),
  inputObjects = 
    bind_rows(
      expectsInput(objectName = "scfmPars", objectClass = "list", desc = "unified parameter list", sourceURL = NA),
      expectsInput(objectName = "spreadStateE", objectClass = "data.table", desc = "spread State produced by prior call", sourceURL = NA),
      expectsInput(objectName = "ignitionLoci", objectClass = "numeric", desc = "ignitions points for stand-alone execution", sourceURL = NA),
      expectsInput(objectName = "nNbrs", objectClass = "numeric", desc = "adjacenc spec, 4 or 8 only.", sourceURL = NA),
      expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "map of flammable cells", sourceURL = NA)
    ),
  outputObjects = bind_rows(
    createsOutput(objectName = "burnMap", objectClass = "RasterLayer", desc = "table of active initial spread cells", sourceURL = NA),
    createsOutput(objectName = "spreadState", objectClass = "data.table", desc = "table of active initial spread cells", sourceURL = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.scfmSpread = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime, "scfmSpread", "burn")
      sim <- scheduleEvent(sim, P(sim)$.statsInitialTime, "scfmSpread", "stats")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmSpread", "plot")
    },
    plot = {
      sim <- sPlot(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "scfmSpread", "plot")
    },
    stats = {
      sim <- Stats(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "scfmSpread", "stats")
    },
    burn = {
      sim <- burnEmUp(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "scfmSpread", "burn")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


Init <- function(sim) {
 
  if (!suppliedElsewhere("flammableMap", sim)){
    
    stop("Need to give me something!") # [ IMPROVE ] Improve message error / handling with tryCatch?
    
  }
  
  sim$burnMap <- sim$flammableMap * 0  # 0 * NA = NA
  
  sim$spreadState <- data.table(NULL)
  setColors(sim$burnMap,n=4) <- c("grey95", "green", "yellow", "red")
  
  if (!is.na(P(sim)$.statsInitialTime)) # [ IMPROVE ] also don't get why this is related to initial time / add warning?
    
    sim$burnStats <- list()
  
  return(invisible(sim))
  
}


.inputObjects <- function(sim){
  
  if (!suppliedElsewhere("nNbrs",sim)){
    
    sim$nNbrs <- 8
    
    warning("nNbrs set to 8 in scfmSpread..inputObjects")
  }
  
  return(invisible(sim))
}


