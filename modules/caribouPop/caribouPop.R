

defineModule(sim, list(
  name = "caribouPop",
  description = "Caribou population model. Calculates changes in boreal caribou herd size based on Sorensen model (2008)", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Antoine", "Adde", email = "antoine.adde.1@ulaval.ca", role = c("aut", "cre")),
  authors = person("Steve", "Cumming", email = "stevec.boreal@gmail.com", role = c("aut", "cre")),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", caribouPop = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "caribouPop.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("N0", "numeric", NA_real_, 0, 5000, "Initial herd size"),
    defineParameter(".statsInitialTime", "numeric", 0, NA_real_, NA_real_, "This describes the simulation time at which the first stat event should occur"),
    defineParameter(".statsInterval", "numeric", 1, NA_real_, NA_real_, "This describes the simulation time interval between stat events"),
    defineParameter(".plotInterval", "numeric", 1, NA_real_, NA_real_, "This describes the simulation time interval between stat events"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName="disturbanceMap", objectClass = "RasterLayer", desc="state of burned or harvestd cells"),
    expectsInput(objectName="landscapeAttr", objectClass = "list", desc="landscape parameters from scfm {Regime|Driver}")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "SorensenStats", objectClass ="data.frame", desc = "lambda and herd size table"),
    createsOutput(objectName = "Nt", objectClass ="numeric", desc = "Caribou population size in time t")
  )
))

doEvent.caribouPop = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- statsInit(sim)
      sim <- scheduleEvent(sim, P(sim)$.statsInitialTime, "caribouPop", "stats")  #get the initial values
      sim <- scheduleEvent(sim, P(sim)$.statsInitialTime, "caribouPop", "plot")  #get the initial values
    
    },
      stats = {
        
        sim <- Stats(sim)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.statsInterval, "caribouPop", "stats")
  
    },
      plot = {
        
        sim$caribouPopGrowth <- popDynamics(stats = sim$SorensenStats)
        
        Plot(sim$caribouPopGrowth, title = "Caribou population dynamics")
        
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "caribouPop", "plot")
        
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if(!suppliedElsewhere("disturbanceMap", sim)|!suppliedElsewhere("landscapeAttr", sim)){
    stop("Disturbance map or landscape attributes not supplied. Can't calculate population dynamics")
  }
  
    return(invisible(sim))
}
