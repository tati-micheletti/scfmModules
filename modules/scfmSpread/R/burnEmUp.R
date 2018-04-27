burnEmUp <- function(sim){   #name is a homage to Walters and Hillborne
  
  if (is.na(P(sim)$pOverRide)){
    pSpread <- if ("scfmPars" %in% names(objs(sim)))
      sim$scfmPars$pSpread
    else
      P(sim)$pSpread
  }
  else
    pSpread <- P(sim)$pOverRide
  
  pMap <- sim$flammableMap
  pMap <- (!pMap) * pSpread
  
  
  maxSize <- if ("scfmPars" %in% names(objs(sim)))
    sim$scfmPars$maxBurnCells
  else
    ncell(sim$burnMap)*0.9
  
  sim$spreadState <- data.table(NULL) #ensure always in a determinate state
  useSSE <- "spreadStateE" %in% names(objs(sim)) &&  nrow(sim$spreadStateE) > 0
  if (useSSE || ("ignitionLoci" %in% names(objs(sim)) && length(sim$ignitionLoci) > 0)){
    sim$spreadState <- SpaDES.tools::spread(sim$flammableMap,
                                            spreadProb = pMap,
                                            spreadState = if (useSSE) sim$spreadStateE else NA,
                                            #
                                            #start = if (useSSE) NA else sim$ignitionLoci,
                                            #asRaster = FALSE,
                                            loci = if (useSSE) NA else sim$ignitionLoci,
                                            #loci = sim$ignitionLoci,
                                            directions = sim$nNbrs,
                                            maxSize = maxSize,
                                            returnIndices = TRUE,
                                            id = TRUE)

    
    values(sim$burnMap)[sim$spreadState[,indices]] <- 3 
    if (suppliedElsewhere("spreadStateE", sim)){
      values(sim$burnMap)[sim$spreadStateE[,indices]] <- 2 #mark the escapes speciaxly
    }
    if (suppliedElsewhere("ignitionLoci", sim)){
      values(sim$burnMap)[sim$ignitionLoci] <- 1           #mark the initials specialy
    }
  }
  return(invisible(sim))
}