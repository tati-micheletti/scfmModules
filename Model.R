# Model

lp ~ B0 + B1 * L_CUT +  # Station in recently harvested cutblock (≤30 years) proportion ==> HarvestStateMap > 0
                           B2 * L_YDEC + # Station in young deciduous forest (31–90 years) proportion ==> Age map + LCC05
                           B3 * L_ODEC + # Station in old deciduous forest (>90 years) proportion ==> Age map = LCC05
                           B4 * L_MIX + # Station in mixed deciduous/conifer forest (>30 years) proportion ==> Age map + LCC05
                           B5 * L_CC + # Area weighted mean forest crown closure ==> ???
                           B6 * L_WDIS + # Distance from station centre to nearest river or lake ==> ??? just 0 for all?
                             B7 * N_CUT + # Station in recently harvested cutblock (≤30 years) proportion ==> HarvestStateMap > 0
                             B8 * N_LATE + # Proportion in old forest (90+ year) proportion ==> Age map (independently of class?)
                             B9 * N_DEC + # Proportion in deciduous forest ==> LCC05 (independently of age?)
                             B10 * N_MIX + # Station in mixed deciduous/conifer forest (>30 years) proportion ==> Age map
                             B11 * N_SB + # Presence of black spruce forest ==> Y/N? and LCC doesn't separate the type of conifers...??? 
                             B12 * N_RICH # Number of habitat classes in neighbourhood ==> LCC05

p <- exp(lp)/1 + exp(lp)
# B... : Get from table Appendix 1
# Values for the variables get from rasters Age Map, Harvest State Map...
# loadYieldTable.rmd has the strata categories

# FOR THE NEIGHBORHOOD
ac <- adjacent(habitatClassMap, distance = 5, directions = 8, include = FALSE, pairs = TRUE)
#ac is the index and 24 habitat class in the neighborhood 
actab <- tabulate(ac[,2]/24)
N_LATE <- sum(ageMap[ac[,1]]>90)/24
N_RICH <- sum(actab>0)

Modules : 
  c("scfmCrop", "scfmLandcoverInit", 
  "scfmRegime", "scfmDriver", "scfmIgnition", "scfmEscape", "scfmSpread", 
  "ageModule", "mapBurns",
  "loadYieldTables", "vegMapToStrataMap", "Hanzlik", "harvest", 
  "stateVars", "caribou",
  "habitatClassMap", "birdsAlberta") 
  
# Need to write: "vegMapToStrataMap", "habitatClassMap", "birdsAlberta"
  
#  Caribou gets all from the disturbance map ("stateVars")
#  "birdsAlberta" needs "habitatClassMap" and "ageMap"

  
  # TO DO:
  # 
  # Bring ecodistrics to prepInputs from : http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip [OK]
  # Use the shapefile from FID 339 [OK]
  # use prepInputs on vegMap and ageMaps (ageMap bring from file) [OK] OBS: AgeMap didn't work! Loaded from folder with raster()
  # Need to upload data to folders that don't have it: ie. NFDB_point on scfmRegime [ OK ]
  # Make resolution of pi ha (3.14ha) ==> res = sqrt(10^4 * pi) [ OK ]
  # Alter scfmCrop to make the cropping using prepInputs [ OK ]
  # Then check if all the others work [ Waiting for propInputs() to work again]
  # Then make vegMapToStrataMap (should convert vegMap to the strataMap)
  # Then make the habitatClassMap
  # Finish with the birdsAlberta module:
  #     it should only receive inputs from "ageMap" and "habitatClassMap" and should use the linear function to predict probability presence.
  # CHECK WITH STEVE: Caribou module / L_CC values (Steve will come up with the rules for this)

## Changing Resolution === WORKING HERE!
library(SpaDES)
url.vegMap = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"
url.studyArea = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"
tempPath.vegMap = file.path(tempdir(), "vegMap")
tempPath.studyArea = file.path(tempdir(), "studyArea")

templateRaster <- prepInputs(url = url.vegMap, destinationPath = asPath(tempPath.vegMap))
studyArea <- prepInputs(url = url.studyArea,
                   destinationPath = tempPath.studyArea)#, rasterToMatch = templateRaster) #rasterToMatch not working
studyArea <- projectInputs(studyArea, raster::crs(templateRaster))
studyArea <- studyArea[studyArea$ECODISTRIC==339,]

# studyArea <- raster::shapefile(file.path(getwd(), "inputs", "studyArea.shp"))
vegMap <- raster::crop(templateRaster, studyArea)
vegMap <- raster::mask(vegMap, studyArea)

# tempRes <- vegMap
# newRes <- sqrt(10^4 * pi)
# raster::res(tempRes) <- c(newRes,newRes)
# vegMap <- raster::resample(templateRaster, tempRes, method="ngb") %>%
#    raster::mask(studyArea)

# +===========================================================+

# Strata / vegMapToStrataMap
# 
# library(SpaDES)
# library(raster)

# 1. Aw     Pure Aspen _Populus tremuloides_ 
# 2. Aw/Sw  Aspen with a white spruce _Picea glauca_ understory (never used)
# 3. AwSw   20-50% Sw (this would be based on AVI or Phase 3 inventory calls)
# 4. SwAw   50-80% Sw (as above)
# 5. Sw    "pure" white spruce, >80% Sw.
# 6. Sb     Black Spruce _Picea mariana_
# 7. Pj     Jack Pine _Pinus banksiana_ 
# 8. AwPj   Aspen Pine mixtures (20-50% pine, I think)

# 1. Pure deciduous
# 2, 3, 4, 8. Mixed
# 5, 7. Pure pine
# 6. Black spruce
# 
# cutlinePath <- file.path(getwd(), "inputs", "studyArea.shp")
# ageMap <- raster::raster(file.path(getwd(), "modules", "scfmCrop/data", "can_age04_1km.tif"))
# 
# gdalUtils::gdalwarp(srcfile = file.path(getwd(), "modules", "scfmCrop/data", "can_age04_1km.tif"), # Raster file path
#                     dstfile = file.path(getwd(), "modules", "scfmCrop/data", "ageMapCropped.tif"), # Cropped raster file name
#                     overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
#                     cutline = cutlinePath, # Shapefile path to use for masking
#                     dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
#                     s_srs= as.character(crs(ageMap)), #Projection from the source raster file
#                     t_srs= as.character(crs(vegMap)), # Projection for the cropped file, it is possible to change projection here
#                     multi = TRUE, # Use multithreaded warping implementation.
#                     of = "GTiff", # Select the output format
#                     crop_to_cutline = TRUE, # Crop the raster to the shapefile
#                     tr = res(vegMap)) # Raster resolution, not sure it needs to be the same from original raster
# 
# ageMap <- raster::raster(file.path(getwd(), "modules", "scfmCrop/data", "ageMapCropped.tif"))

#====================================================

# Not needed for now

# numTypes <- 8
# ext <- extent(ageMap)[]
# resolution <- res(ageMap)[1]
# 
# library(SpaDES)
# 
# resolution <- sqrt(pi*10^4)
# ext <- c(-682368.7,-484031.2,7512969.0,7686846.7)
# ext2 <- extent(0, 1000, 0, 1000)
# 
# ras1 <- randomPolygons(ras = raster(extent(0, 1000, 0, 1000), res = resolution, vals = 0), numTypes = 8) # WORKING
# ras2 <- randomPolygons(ras = raster(ext, res = resolution, vals = 0), numTypes = 8) # NOT WORK
# ras3 <- randomPolygons(ras = raster(ext2, res = resolution, vals = 0), numTypes = 8) # NOT WORKING
# ras4 <- randomPolygons(ras = raster(extent(-682368.7,-484031.2,7512969.0,7686846.7), res = resolution, vals = 0), numTypes = 8) #NOT WORKING
# 
# Plot(ras1)
# Plot(ras2)
# Plot(ras3)
# Plot(ras4)
# 
# # WORKING
# resolution <- res(ageMap)[1]
# ext <- extent(0, 1000, 0, 1000)
# 
# for (i in 1:100){
#   
#   set.seed(i)
#   ras <- randomPolygons(ras = raster(ext, res = resolution, vals = 0), numTypes = 8)
#   if(length(unique(raster::getValues(ras)))>1){
#   SEED <- i
#   plot(ras)
#   break
#   } else {
#       next
#     }
# }
# 
# 
# # NOT WORKING
# resolution <- res(ageMap)[1]
# ext <- extent(ageMap)
# 
# for (i in 1:1000){
#   
#   set.seed(i)
#   ras <- randomPolygons(ras = raster(ext, res = resolution, vals = 0), numTypes = 8)
#   if(length(unique(raster::getValues(ras)))>1){
#     SEED <- i
#     plot(ras)
#     break
#   } else {
#     next
#   }
# }
# 
# 
# # FOR IT TO WORK, I HAVE TO IDENTIFY WHEN THERE IS ONLY ONE VALUE ON THE STRATAMAP AND REDO IT!


# LOAD MAPS TO TEST
vegMap <- raster::raster(file.path(getwd(),"modules","")
