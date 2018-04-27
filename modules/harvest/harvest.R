defineModule(sim,  list(
  name = "harvest",
  description = "Simulates block level forest harvest scheduling to reach AAC targets.",
  keywords = c("harvesting", "cut block", "adjacency constraints"),
  authors = c(person(c("Steven", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "harvest.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("minVolume", "numeric", 50, 0, NA, "Minimum volume in m^3/ha for eligibility to harvest"),
    defineParameter("minAge", "numeric", 40, 0, NA, "Minimum age to harvest"),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, 0, "waddya think?"),
    defineParameter("minBlockSize", "numeric", 20, 1, NA, "minimum block size (ha)"),
    defineParameter("greenUpPeriod", "numeric", 30, 0, 30, "For how many years do cut blocks affect adjacent areas?"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, 
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "forest Age structure" ),
    expectsInput(objectName = "yieldTables", objectClass = "list", desc = "what it is"),
    expectsInput(objectName = "strataMap", objectClass = "Rasterayer", desc = "map assigning cells to yield tables"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", 
                 desc = "info about landscape we are running on; e.g. cellSize"),
    expectsInput(objectName = "annualCut", objectClass = "list", desc = "m^3/yr per yield Class")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "harvestStateMap", objectClass = "RasterLayer", desc = "areas locked up by spatial constraints"),
    createsOutput(objectName = "volMap", objectClass = "RasterLayer", desc = "volume denisty per cell"),
    createsOutput(objectName = "harvestStats", objectClass = "data.frame" , desc = "record of harvest activity by strata"),
    createsOutput(objectName = "cutCells", objectClass = "numeric", desc = "vector of cells harvested in current period")
  )
))



## event types
#   - type `init` is required for initialiazation

doEvent.harvest = function(sim, eventTime, eventType) {
  switch(eventType,
     init = {
       
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime,"harvest","harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "harvest", "plot")
    },
    plot = {
      Plot(sim$harvestStateMap, legendRange=0:1)
      #Plot(sim$disturbanceMap, legendRange=0:3,zero.color="white")
      sim <- scheduleEvent(sim, time(sim)+P(sim)$.plotInterval, "harvest", "plot")
    },
    harvest = {
      sim <- Harvest(sim) 
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval,"harvest","harvest")
   }, 
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  sim$harvestStats <- data.frame(rep(list(numeric(0)),length(sim$yieldTables[[1]]))) #again, use the 1st and only region
  
  #sim$disturbanceMap<-raster(sim$strataMap) * 0 
  #setColors(sim$disturbanceMap, n=3) <- c("white", "blue","red")
  if(!suppliedElsewhere("strataMap", sim)){stop("No strata map provided. This would normally come from module 'vegMapToStrataMap")}
  sim$harvestStateMap <- raster::raster(sim$strataMap)
  sim$harvestStateMap[] <- sim$strataMap[]*0
  setColors(sim$harvestStateMap, n=10) <- c("white","black")
  
  sim$volMap <- raster::raster(sim$strataMap) 
  sim$volMap[] <- sim$strataMap[]*0
  sim$merchCells <- list()    #of cells currently in the list for each stratum
  sim$cutCells <- numeric()

  return(invisible(sim))
}

.inputObjects <- function(sim){
  
  if(!suppliedElsewhere("strataMap", sim)){
    stop("No strata map provided. This would normally come from module 'vegMapToStrataMap") # [ IMPROVE ] add a strataMap for the module to work alone
  }
}
