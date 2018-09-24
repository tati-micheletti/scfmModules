
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
  reqdPkgs=list("raster","rgeos","sp", "SpaDES.tools"),
  documentation = list("README.txt", "scfmCrop.Rmd"),
  parameters=rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, desc="Use cache or not.")
    ),
  inputObjects=bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons", desc = "study area", sourceURL = sim$url.studyArea),
    expectsInput(objectName = "vegMap", objectClass = "RasterLayer", desc = "vegetation map", sourceURL = sim$url.vegMap),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "age map"),
    expectsInput(objectName = "vegMapInit", objectClass = "RasterLayer", desc = "national landcover map LCC05"),
    expectsInput(objectName = "ageMapInit", objectClass ="RasterLayer", desc = "national 1km2 ageMap"),
    expectsInput(objectName = "url.studyArea", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.vegMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "numTypes", objectClass = "numeric", desc = "Number of strata", sourceURL = NA),
    expectsInput(objectName = "areaInHa", objectClass = "numeric", desc = "Resolution for raster pixels in hactares", sourceURL = NA),
    expectsInput(objectName = "templateRaster", objectClass = "RasterLayer", desc = "Template raster", sourceURL = sim$url.vegMap),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "flammable Map raster", sourceURL = NA),
    expectsInput(objectName = "polyMatrix", objectClass = "matrix", desc = "Matrix for random polygon", sourceURL = NA),
    expectsInput(objectName = "areaSize", objectClass = "numeric", desc = "Area size for random polygon", sourceURL = NA)
    
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = "vegetation map")
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
  
  if(sum(raster::getValues(sim$ageMap), na.rm = TRUE)==0) stop("There are no age data provided with input age map")
  if(sum(raster::getValues(sim$vegMap), na.rm = TRUE)==0) stop("There are no vegatation data provided with input vegatation map")
  
   return(invisible(sim))
}

.inputObjects <- function(sim){
  
  if(!suppliedElsewhere("templateRaster", sim)){
  sim$templateRaster <- Cache(prepInputs, url = sim$url.vegMap, # [ IMPROVE ] Add tryCatch() and return warning that no template could be downloaded, look for it in inputs, it needs to have the specific name "templateRaster.tif"?
                              destinationPath = dataPath(sim))
  }
  if(all(!suppliedElsewhere("studyArea", sim) & !file.exists(file.path(inputPath(sim), "studyArea.shp")))){
    # sim$studyArea <- Cache(prepInputs, url = sim$url.studyArea, destinationPath = dataPath(sim))#,rasterToMatch = sim$templateRaster) # RasterToMatch temporarily not working
    set.seed(1983)
    sim$studyArea <- randomPolygon(x = sim$polyMatrix, hectares = sim$areaSize)
    # sim$studyArea <- projectInputs(studyArea, raster::crs(sim$templateRaster))
    # sim$studyArea <- sim$studyArea[sim$studyArea$ECODISTRIC==339,]
    
    # rgdal::writeOGR(obj = sim$studyArea, dsn = file.path(dataPath(sim), "studyArea.shp"), layer = "studyArea.shp", driver = "ESRI Shapefile")
    
  } else {
    if(!suppliedElsewhere("studyArea", sim)){
    sim$studyArea <- Cache(prepInputs, targetFile = file.path(dataPath(sim), "studyArea.shp"),
                           destinationPath = asPath(file.path(inputPath(sim))))#rasterToMatch = sim$templateRaster)
  }
 }
  if(suppliedElsewhere("templateRaster", sim)){
  sim$vegMap <- Cache(raster::crop, sim$templateRaster, sim$studyArea) # # [ IMPROVE ] Update to postProc
  sim$vegMap <- Cache(raster::mask, sim$vegMap, sim$studyArea) # # [ IMPROVE ] Update to postProc
  } else {stop("Couldn't find template map")}
  
  if(!suppliedElsewhere("areaInHa", sim)){
    sim$areaInHa <- pi
    warning("Using 'pi' hectares as raster resolution")
  }
  
  # Changing resolution
  tempRes <- sim$vegMap
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

    # Mannually create an ageMap # [ IMPROVE ] Exclude to NA areas from vegMap that are not forests.
     
    vecReclass <- c(15, 2, 34, 35)
    sim$ageMap <- sim$vegMap

    c15 <- which(sim$ageMap[]==15)
    c2 <- which(sim$ageMap[]==2)
    c34 <- which(sim$ageMap[]==34)
    c35 <- which(sim$ageMap[]==35)
    
    set.seed(2016)
    gausMapBase <- gaussMap(sim$ageMap, scale = 8, var = 120, method = "RMexp")
   
    sim$ageMap <- Cache(raster::mask, gausMapBase, sim$studyArea)
    sim$ageMap[c15] <- 15
    sim$ageMap[c2] <- 2
    sim$ageMap[c34] <- 34
    sim$ageMap[c35] <- 35
    
  }
  
  # else {
      
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

    # sim$ageMap <- raster::raster(file.path(dataPath(sim), "can_age04_1km.tif"))
    # 
    # cutlinePath <- file.path(inputPath(sim), "studyArea.shp")
    # 
    #   gdalUtils::gdalwarp(srcfile = file.path(dataPath(sim), "can_age04_1km.tif"), # Raster file path
    #            dstfile = file.path(dataPath(sim), "ageMapCropped.tif"), # Cropped raster file name
    #            overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
    #            cutline = cutlinePath, # Shapefile path to use for masking
    #            dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
    #            s_srs= as.character(crs(sim$ageMap)), #Projection from the source raster file
    #            t_srs= as.character(crs(sim$vegMap)), # Projection for the cropped file, it is possible to change projection here
    #            multi = TRUE, # Use multithreaded warping implementation.
    #            of = "GTiff", # Select the output format
    #            crop_to_cutline = TRUE, # Crop the raster to the shapefile
    #            tr = res(sim$vegMap)) # Raster resolution, not sure it needs to be the same from original raster
    #   
    #   sim$ageMap <- raster::raster(file.path(dataPath(sim), "ageMapCropped.tif"))
    
    #=====================================================
  #     }
  # 
  # }
  
  if (!suppliedElsewhere("ageMapInit",sim)){
    suppliedElsewhere("ageMap",sim)
    sim$ageMapInit <- sim$ageMap
  } else stop("ageMap not supplied, and ageMapInit failed to be created.")
  
  if (suppliedElsewhere("vegMap", sim)){ 

    sim$flammableMap <- sim$vegMap  #this, on the other hand, had better exist
        #    sim$ageMap[] <- P(sim)$initialAge ===> Not sure what this does 
  }
      
 else {
    
    sim$ageMap <- raster(raster::extent(0,49,0,49),nrow=200, ncol=200, vals=0)
    sim$flammableMap <- sim$vegMap
    warning("Age map was not supplied, creating a random raster.")
    
  }
  
#  sim$strataMap <- randomPolygons(sim$ageMap,sim$numTypes)
   # sim$Mask <- sim$vegMap # Might be sed in the scfmLandcoverInit 
   # sim$Mask[] <- ifelse(is.na(sim$vegMap[]), NA, 1) # Might be sed in the scfmLandcoverInit 
  
  return(invisible(sim))
}
