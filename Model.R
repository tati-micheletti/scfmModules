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
  # Need to upload data to folders that don't have it: ie. NFDB_point on scfmRegime
  # Make resolution of pi ha (3.14ha)
  # Alter scfmCrop to make the cropping using prepInputs
  # Then check if all the others work
  # Then make vegMapToStrataMap (should convert vegMap to the strataMap)
  # Then make the habitatClassMap
  # Finish with the birdsAlberta module:
  #     it should only receive inputs from "ageMap" and "habitatClassMap" and should use the linear function to predict probability presence.
  # CHECK WITH STEVE: Caribou module / L_CC values (Steve will come up with the rules for this)

