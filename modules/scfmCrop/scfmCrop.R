
stopifnot(packageVersion("SpaDES") >= "0.99.0")
defineModule(sim, list(
  name="scfmCrop",
  description="A translator module. Crops, reprojects and mask all necessary layers to\ 
      specifed extent. Selects or subsets all spatialised data e.g. fires.\ 
      Use cropped RasterLayer, defined by a spatial polygon that defines the area of interest",
  keywords=c("translator", "lcc05", "Land Cover Classification", "vegetation","Canadian National Fire Database"),
  childModules=character(),
  authors=c(person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut")),
            person(c("Eliot", "J","B"), "McIntire", email="emcintir@nrcan.gc.ca", role=c("aut")),
            person("Pierre", "Vernier", email="pierre.vernier@gmail.com")
            ),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster","rgeos","sp"),
  documentation = list("README.txt", "scfmCrop.Rmd"),
  parameters=rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, desc="Use cache or not.")
    ),
  inputObjects=bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons", desc = "study area"),
    expectsInput(objectName = "vegMapInit", objectClass = "RasterLayer", desc = "national landcover map LCC05"),
    expectsInput(objectName = "ageMapInit", objectClass ="RasterLayer", desc = "national 1km2 ageMap"),
    expectsInput(objectName = "url.studyArea", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.vegMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "tempPath.studyArea", objectClass = "character", desc = "Temporary path to downloaded study area", sourceURL = NA),
    expectsInput(objectName = "tempPath.vegMap", objectClass = "character", desc = "Temporary path to downloaded vegetation map", sourceURL = NA),
    expectsInput(objectName = "numTypes", objectClass = "numeric", desc = "Number of strata", sourceURL = NA)
    
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = ""),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = ""),
    createsOutput(objectName = "studyArea", objectClass = "shapefile", desc = ""),
    createsOutput(objectName = "templateRaster", objectClass = "RasterLayer", desc = ""),
    createsOutput(objectName = "strataMap", objectClass = "shapefile", desc = "Number of strata"),
    createsOutput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "")
  )
))

doEvent.scfmCrop = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    
    sim <- Init(sim)
    
  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                  "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}


Init <- function(sim) {

 #  vegProjection <- crs(sim$vegMapInit)
 #  
 #  if (is.na(crs(sim$studyArea)))            #in case it was sampled from the vegmap.
 #    crs(sim$studyArea) <- vegProjection
 #  
 #  simProjection <- crs(sim$studyArea)         #this would probably be set to be the same as the veg map at an earlier stage.
 #  
 #  #if(ncell(sim$vegMap)>5e5) beginCluster(min(parallel::detectCores(),6))
 #  
 #  #Project the study area into each input raster, then crop and mask; 
 #  #Then project result back into the sim projection.
 #  #browser()
 #  studyAreaTmp <- spTransform(sim$studyArea, CRSobj = vegProjection)
 #  sim$vegMap <-  crop(sim$vegMapInit, studyAreaTmp)
 #  crs(sim$vegMap) <- vegProjection
 #  sim$vegMap <- mask(sim$vegMap, studyAreaTmp) #É
 #  #sim$vegMap <- projectRaster(sim$vegMap, crs=simProjection, method="ngb", to=sim$vegMapInit)
 #  sim$Mask <- sim$vegMap
 #  sim$Mask[] <- ifelse(is.na(sim$vegMap[]), NA, 1)
 #  
 #  tmp <- getColors(sim$vegMapInit)[[1]]        # mask removes colors!
 #  setColors(sim$vegMap, n=length(tmp)) <- tmp  # so put them back.
 #  
 #  ageProjection <- crs(sim$ageMapInit)
 #  studyAreaTmp <- spTransform(sim$studyArea, CRSobj =ageProjection)
 #  sim$ageMap <-  crop(sim$ageMapInit, studyAreaTmp)
 # # crs(sim$ageMap) <- ageProjection
 #  sim$ageMap <- mask(sim$ageMap,studyAreaTmp)
 #  sim$ageMap <- projectRaster(sim$ageMap,to=sim$vegMap,method="ngb")
 #  
  #endCluster()
  
  if(sum(raster::getValues(sim$ageMap), na.rm = TRUE)==0) stop("There are no age data provided with input age map")
  if(sum(raster::getValues(sim$vegMap), na.rm = TRUE)==0) stop("There are no vegatation data provided with input vegatation map")
  
  return(invisible(sim))
}

.inputObjects <- function(sim){
  
  sim$templateRaster <- Cache(prepInputs, url = sim$url.vegMap, # [ IMPROVE ] Add tryCatch() and return warning that no template could be downloaded, look for it in inputs, it needs to have the specific name "templateRaster.tif"?
                              destinationPath = asPath(sim$tempPath.vegMap))
  
  if(!file.exists(file.path(inputPath(sim), "studyArea.shp"))){
    sim$studyArea <- Cache(prepInputs, url = sim$url.studyArea,
                           destinationPath = sim$tempPath.studyArea)#,rasterToMatch = sim$templateRaster) # RasterToMatch temporarily not working
    sim$studyArea <- projectInputs(studyArea, raster::crs(sim$templateRaster))
    sim$studyArea <- sim$studyArea[sim$studyArea$ECODISTRIC==339,]
    
#    rgdal::writeOGR(obj = sim$studyArea, dsn = file.path(inputPath(sim), "studyArea.shp"), layer = "studyArea.shp", driver = "ESRI Shapefile")
    
  } else {
    
    sim$studyArea <- prepInputs(targetFile = asPath(file.path(inputPath(sim), "studyArea.shp")),
                           destinationPath = asPath(file.path(inputPath(sim))))#rasterToMatch = sim$templateRaster)
  }
  
  if(suppliedElsewhere("templateRaster", sim)){
  sim$vegMap <- Cache(raster::crop, sim$templateRaster, sim$studyArea) # # [ IMPROVE ] Update to postProc
  sim$vegMap <- Cache(raster::mask, sim$vegMap, sim$studyArea) # # [ IMPROVE ] Update to postProc
  } else {stop("Couldn't find template map")}
  
  if(!suppliedElsewhere("sim$areaInHa")){
    sim$areaInHa <- pi
    warning("Using 'pi' hectares as raster resolution")
  }
  
  # Changing resolution
  tempRes <- vegMap
  newRes <- sqrt(10^4 * sim$areaInHa)
  raster::res(tempRes) <- c(newRes,newRes)
  sim$vegMap <- raster::resample(templateRaster, tempRes, method="ngb") %>%
    raster::mask(studyArea)
  sim$templateRaster <- sim$vegMap
  
  if (!suppliedElsewhere("vegMapInit",sim)){
    suppliedElsewhere("vegMap",sim)
      sim$vegMapInit <- sim$vegMap
    } else stop("VegMap not supplied an/or not available online")
  
  if (!suppliedElsewhere("ageMap",sim)){

    if(file.exists(file.path(dataPath(sim), "ageMapCropped.tif"))){
      
      sim$ageMap <- raster::raster(file.path(dataPath(sim), "ageMapCropped.tif"))

    } else {
      
    # ORIGINAL TRIAL
    
    # tryCatch({sim$ageMap <- prepInputs(targetFile = asPath(file.path(dataPath(sim), "can_age04_1km.tif")),
    #                                   destinationPath = asPath(file.path(dataPath(sim))),
    #                                   studyArea = sADF,
    #                                   rasterToMatch = rTm)},
    #          finally = {sim$ageMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=as.integer(runif(200*200)*150))
    #          warning("Age map was not supplied, creating a random raster.")})

    #=====================================================
    
    # NOT WORKING: TEMP FILE / URL
    
    # sA <- if(suppliedElsewhere(sim$studyArea)) SpatialPolygons(sim$studyArea@polygons,proj4string=sim$studyArea@proj4string) else NULL
    # sADF <- if(suppliedElsewhere(sim$studyArea)) sim$studyArea else NULL
    # rTm <- if(suppliedElsewhere(sim$vegMap)) sim$vegMap else NULL

  #  browser() #Error in match.arg(algo) : 'arg' must be NULL or a character vector
    
    # sim$ageMap <- prepInputs(url = "https://drive.google.com/file/d/1lwszwnFjZ3DQ3BBQ7ikiAlN6FXyy2uNX/view?usp=sharing",
    #                          targetFile = file.path(tempdir(), "ageMap", "NA_TREEAGE_1096/data", "can_age04_1km.tif"),
    #                          destinationPath = file.path(tempdir(), "ageMap", "NA_TREEAGE_1096/data"),
    #                          studyArea = sADF,
    #                          rasterToMatch = rTm)
    
    
    #=====================================================
    
    # NOT WORKING: LOCAL FILE WITH / WITHOUT asPath()
    
    # sim$ageMap <- prepInputs(targetFile = asPath(file.path(dataPath(sim), "can_age04_1km.tif")),
    #                          destinationPath = asPath(file.path(dataPath(sim))),
    #                          studyArea = sADF,
    #                          rasterToMatch = rTm)
    
    #=====================================================
    
    # Workaround

    sim$ageMap <- raster::raster(asPath(file.path(dataPath(sim), "can_age04_1km.tif")))

    cutlinePath <- file.path(inputPath(sim), "studyArea.shp")
    
      gdalUtils::gdalwarp(srcfile = file.path(dataPath(sim), "can_age04_1km.tif"), # Raster file path
               dstfile = file.path(dataPath(sim), "ageMapCropped.tif"), # Cropped raster file name
               overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
               cutline = cutlinePath, # Shapefile path to use for masking
               dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
               s_srs= as.character(crs(sim$ageMap)), #Projection from the source raster file
               t_srs= as.character(crs(sim$vegMap)), # Projection for the cropped file, it is possible to change projection here
               multi = TRUE, # Use multithreaded warping implementation.
               of = "GTiff", # Select the output format
               crop_to_cutline = TRUE, # Crop the raster to the shapefile
               tr = res(sim$vegMap)) # Raster resolution, not sure it needs to be the same from original raster
      
      sim$ageMap <- raster::raster(asPath(file.path(dataPath(sim), "ageMapCropped.tif")))
    
    #=====================================================
      }
  
  }
  
  if (!suppliedElsewhere("ageMapInit",sim)){
    suppliedElsewhere("ageMap",sim)
    sim$ageMapInit <- sim$ageMap
  } else stop("ageMap not supplied, and ageMapInit failed to be created.")
  
  if (suppliedElsewhere("ageMap", sim)){

    sim$flammableMap <- sim$ageMap   #this, on the other hand, had better exist
    #    sim$ageMap[] <- P(sim)$initialAge ===> Not sure what this does 
  }
      
 else {
    
    sim$ageMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=0)
    sim$flammableMap <- sim$ageMap
    warning("Age map was not supplied, creating a random raster.")
    
  }
  
#  sim$strataMap <- randomPolygons(sim$ageMap,sim$numTypes)
   # sim$Mask <- sim$vegMap # Might be sed in the scfmLandcoverInit 
   # sim$Mask[] <- ifelse(is.na(sim$vegMap[]), NA, 1) # Might be sed in the scfmLandcoverInit 
  
  return(invisible(sim))
}
