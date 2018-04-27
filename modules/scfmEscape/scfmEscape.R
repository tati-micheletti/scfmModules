# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "scfmEscape",
  description = "This Escapes fire(s) from an initial set of loci returned by an ignition module,\ 
                 and readies the results for use by scfmSpread",
  keywords = c("fire Escape"),
  authors = c(person(c("Steve", "G"), "Cumming", 
                     email="stevec@sbf.ulaval.ca", role=c("aut"))),
  childModules = character(),
  version = numeric_version("1.0.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmEscape.Rmd"),
  reqdPkgs = list("raster","data.table","magrittr"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("p0", "numeric", 0.5, 0, 1, "probability of an ignition spreading to an unburned immediate neighbour"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time to start starting fires"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "interval between plot events"),
    defineParameter("returnInterval", "numeric", 1, NA, NA, "This specifies the time interval between Escape events"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Caching the module")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "scfmPars", objectClass = "list", desc = "unified parameter list", sourceURL = NA),
    expectsInput(objectName = "ignitionLoci", objectClass = "numeric", desc = "vector of ignition cells", sourceURL = NA),
    expectsInput(objectName = "nNbrs", objectClass = "numeric", desc = "adjacenc spec, 4 or 8 only.", sourceURL = NA),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "map of flammable cells", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "escapeMap", objectClass = "RasterLayer", desc = "table of active initial spread cells", sourceURL = NA),
    createsOutput(objectName = "spreadStateE", objectClass = "data.table", desc = "table of active initial spread cells", sourceURL = NA)
  )
))

doEvent.scfmEscape = function(sim, eventTime, eventType, debug = FALSE){
  switch (
  eventType, 
  init = {

    sim <- Init(sim)
    sim <- scheduleEvent(sim, P(sim)$startTime, "scfmEscape", "escape")
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmEscape", "plot")
  },
  plot = {
    sim <- ePlot(sim)
    sim <- scheduleEvent(sim, time(sim)+P(sim)$.plotInterval, "scfmEscape", "plot")
  },
  escape = {
    sim <- Escape(sim)
    sim <- scheduleEvent(sim, time(sim)+P(sim)$returnInterval, "scfmEscape", "escape")
  },
  warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initilization

Init <- function(sim) {
 
  sim$spreadStateE <- data.table(NULL)
  
  if (!is.na(P(sim)$.plotInitialTime)){ # [ IMPROVE ] don't understand why this is here with plotInitialTime?
    sim$escapeMap <- raster(sim$flammableMap)
    sim$escapeMap[] <- 0

    }
  
  return(invisible(sim))

}

.inputObjects <- function(sim){
  
  if (!("nNbrs" %in% names(objs(sim)))){
    
    sim$nNbrs <- 8
    
  }
  
  return(invisible(sim))
}