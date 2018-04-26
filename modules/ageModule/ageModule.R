defineModule(sim, list(
  name = "ageModule",
  description = "Creates and maintains a raster called ageMap",
  keywords = c("forest age", "modelling course", "Lab 5"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.9.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "ageModule.Rmd"),
  reqdPkgs = list("raster","RColorBrewer"),
  parameters = rbind(
    defineParameter("initialAge", "numeric", 99.0, 0, 1e4, desc="initial age"),
    defineParameter("maxAge","numeric", 250, 0, 2**16-1, desc="maximum age for plotting"),
    defineParameter("returnInterval", "numeric", 1, NA, NA, desc="Time interval between aging aevents"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time at which to initiate aging"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc="This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName="flammableMap", objectClass = "RasterLayer", desc="Template map"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "ageMap to update"),
    expectsInput(objectName = "ageMapInit", objectClass ="RasterLayer", desc = "national 1km2 ageMap")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ageMap",  objectClass = "RasterLayer", desc="Duh"),
    createsOutput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "")
  )
))

doEvent.ageModule = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "ageModule", "age")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "ageModule", "plot")
    },
    age = {
      sim <- Age(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "ageModule", "age")
    },
    plot = {
      Plot(sim$ageMap, legendRange=c(0,P(sim)$maxAge))
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,"ageModule", "plot")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  
# we will use our colour choices, not whatever may have come with the loaded map.
  
  cols <- length(which(!is.na(unique(getValues(sim$ageMap)))))
  sim$ageMap <- setColors(sim$ageMap,n=cols,colorRampPalette(c("LightGreen", "DarkGreen"))(cols))
  
  return(invisible(sim))
}


.inputObjects <- function(sim){
  
  if (!suppliedElsewhere("ageMap",sim)){
    
    if(file.exists(file.path(dataPath(sim),"ageMapCropped.tif"))){
      
      tryCatch(sim$ageMap <- prepInputs(targetFile = asPath(file.path(dataPath(sim), "ageMapCropped.tif")),
                                        destinationPath = asPath(file.path(dataPath(sim)))),
               finally = {sim$ageMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=as.integer(runif(200*200)*150))
               warning("Age map was not supplied in 'ageModule/data' folder, creating a random raster.")})
      
    } else {
    
    tryCatch(sim$ageMap <- prepInputs(targetFile = asPath(file.path(dataPath(sim), "can_age04_1km.tif")),
                             destinationPath = asPath(file.path(dataPath(sim))),
                             studyArea = ifelse(suppliedElsewhere(sim$studyArea), sim$studyArea, NULL),
                             rasterToMatch = ifelse(suppliedElsewhere(sim$vegMap), sim$vegMap, NULL)), 
             finally = {sim$ageMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=as.integer(runif(200*200)*150))
               warning("Age map was not supplied in 'ageModule/data' folder, creating a random raster.")})
    }
  }
   
   
  if (!suppliedElsewhere("ageMapInit",sim)){
    
    suppliedElsewhere("ageMap",sim)
    
    sim$ageMapInit <- sim$ageMap
  }
  
    
  if (all(!suppliedElsewhere("flammableMap", sim)&suppliedElsewhere("ageMap", sim))){
    
    sim$flammableMap <- sim$ageMap   #this, on the other hand, had better exist
    #    sim$ageMap[] <- P(sim)$initialAge ===> Not sure what this does 
    
  }
  
  else {
    
    if (!suppliedElsewhere("flammableMap", sim)&!suppliedElsewhere("ageMap", sim)){
    
    sim$flammableMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=0)
    warning("Age map was not supplied, creating a random raster.")
    }
  }

  
  return(invisible(sim))
}
