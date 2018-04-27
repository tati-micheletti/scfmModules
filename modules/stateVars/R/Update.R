Update <- function(sim){

  # UPDTAE DISTURBANCE MAP
  
  dtidx <- which(sim$dtMap[] > 0)
  dtval <- sim$dtMap[dtidx]
  dtval <- dtval - 1
  sim$dtMap[dtidx] <- dtval
  unMark <- dtidx[which(dtval == 0)]
  sim$disturbanceMap[unMark] <- 0
  
  
  if (is.data.table(sim$spreadState) && nrow(sim$spreadState) > 0){   #existant and non-empty?
    idx <- sim$spreadState[,indices]                                  #then scfmSpread will define "indices"     
    raster::values(sim$disturbanceMap)[idx] <- 1
    raster::values(sim$dtMap)[idx] <- P(sim)$persistTimes[1]
  }      
  
  if (is.numeric(sim$cutCells) && length(sim$cutCells) > 0){
    adjx <- raster::adjacent(sim$disturbanceMap, sim$cutCells, pairs=FALSE)
    raster::values(sim$disturbanceMap)[adjx] <- 3
    raster::values(sim$dtMap)[adjx] <- P(sim)$persistTimes[3] 
    raster::values(sim$disturbanceMap)[sim$cutCells] <- 2 #do these after, because overlap when cutting blocks of cells.
    raster::values(sim$dtMap)[sim$cutCells] <- P(sim)$persistTimes[2]
    
    # UPDTAE AGE MAP
    raster::values(sim$ageMap)[sim$cutCells] <- 0
    
    # UPDTAE HABITAT MAP
    
    browser()
    #  raster::values(sim$habitatMap)[sim$cutCells] <- 0
    
  }
  
  
  sim <- heightFromAge(sim)
  
  return(invisible(sim))
}