clearPlot()

# Habitat
Plot(results[[1]]@.envir$ageMap, title = "Age Map")
Plot(results[[1]]@.envir$disturbanceMap, title = "Disturbance Map")
Plot(results[[1]]@.envir$habitatMap, title = "Habitat Map")
clearPlot()

# Fire
Plot(results[[1]]@.envir$flammableMap)
Plot(results[[1]]@.envir$burnMap, title = "Fire Map")


# Harvest
Plot(results[[1]]@.envir$harvestStateMap, title = "Harvest Map")
Plot(results[[1]]@.envir$volMap = "Volume Map")
Plot(results[[1]]@.envir$strataMap, title = "Strata Map")
results[[1]]@.envir$harvestStats
results[[1]]@.envir$yieldTables


# Birds
Plot(results[[1]]@.envir$birdAbundance$COWA, title = "COWA")
Plot(results[[1]]@.envir$birdAbundance$MOWA, title = "MOWA")
Plot(results[[1]]@.envir$birdAbundance$RBNU, title = "RBNU")
Plot(results[[1]]@.envir$birdAbundance$TEWA, title = "TEWA")
Plot(results[[1]]@.envir$birdAbundance$YWAR, title = "YWAR")
results[[1]]@.envir$covarParams # Covariate parameters (Vernier et al. 2008)
results[[1]]@.envir$covarTable # Covariates per pixel (10 years interval) 
clearPlot()

# Caribou
Plot(results[[1]]@.envir$SorensenStats$Nt, title = "Caribou Population Size")
Plot(results[[1]]@.envir$SorensenStats$Lambda, title = "Caribou Population Growth")
clearPlot()


