stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="scfmLandcoverInit",
  description="Takes the LCC05 classification of 39 land cover classes, and reclassifies it to flammable and inflammable [1,0]",
  keywords=c("fire", "LCC05", "land cover classification 2005", "BEACONs"),
  childModules=character(),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  documentation = list("README.txt", "scfmLandcoverInit.Rmd"),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="Interval between plotting"), #usually, once is enough
    defineParameter("nonFlammClasses", "numeric", c(36, 37, 38, 39), NA, NA, desc="which classes don't burn"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, desc="Should Cache the processed vegMap?")
    ),
  inputObjects=bind_rows(
    expectsInput(objectName="nNbrs", objectClass = "numeric", desc="raster cell neighborhood defaults to 8"),
    expectsInput(objectName="vegMap", objectClass = "RasterLayer", desc="vegetation template LCC05"),
    expectsInput(objectName="Mask", objectClass = "RasterLayer", desc="NA mask of study area")
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "flammableMap",  objectClass = "RasterLayer", desc="which pixels can burn: coded 0"),
    createsOutput(objectName = "landscapeAttr",  objectClass = "list", desc="landscape parameters from scfm{Regime|Driver}")
  )
))

doEvent.scfmLandcoverInit = function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    
    init =  {
      
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmLandcoverInit", "plot")

     },
    
    plot = {
      
      #Plot(sim$vegMap, new=TRUE, title = "Vegetation Map") # We don't need it as it doesn't change!
      # Plot(sim$flammableMap) # this is failing probably due to a bug in Plot
                                                           # EJM is working on it 20160224
      
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "scfmLandcoverInit", "plot")
      
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  )
  return(invisible(sim))
}


Init = function(sim) {

  # these classes are LCC05 specific

  #nonFlammClasses <- c(36, 37, 38, 39)  #should be a parameter.
  # oldClass <- 0:39
  # newClass <- ifelse(oldClass %in% P(sim)$nonFlammClasses,1,0)   #1 codes for non flammable  #see mask argument for SpaDES::spread()
  # flammableTable <- cbind(oldClass, newClass)
  # sim$flammableMap <-raster::ratify(raster::reclassify(sim$flammableMap, flammableTable),count=TRUE)
  
  reclassNonFlamm <- matrix(c(1, 38, 0, 38, 39, 1, 39, 40, 0), byrow = TRUE, ncol = 3)    # [ IMPROVE ] Pass argument from global

  # 36 - Urban and Built-up
  # 37 - Water bodies
  # 38 - Mixes of water and land
  # 39 - Snow/ ice
  
  sim$flammableMap <- raster::reclassify(x = sim$flammableMap, rcl = reclassNonFlamm, 
                                         include.lowest = TRUE, right = FALSE)
    sim$flammableMap <- setColors(sim$flammableMap,n=2,colorRampPalette(c("skyblue", "red"))(2))
    
    #  sim$flammableMap <- raster::mask(sim$flammableMap, sim$studyArea)
  
  # if (suppliedElsewhere("Mask",sim)){
  #   if (class(sim$Mask) == "RasterLayer"&
  #       raster::extent(sim$Mask)==raster::extent(sim$flammableMap)&
  #       identical(raster::crs(sim$Mask),raster::crs(sim$flammableMap))){
  #     sim$flammableMap <- sim$flammableMap * sim$Mask # NAs in Mask should      
  #   } #should also check they conform.
  #   else
  #     stop("Invalid class for Mask: require RasterLayer in the same projection and extent as flammableMap")
  # }
  
  #the count options should cause that "a column with frequencies is added. 

  sim <- genFireMapAttr(sim)
  
  return(invisible(sim))
}


.inputObjects <- function(sim){
  
  if (!("nNbrs" %in% names(objs(sim)))){
    
    warning("Setting 'nNbrs' to 8 in scfmLandcoverInit..inputObjects")
    sim$nNbrs <- 8
  }
  
  return(invisible(sim))
}
