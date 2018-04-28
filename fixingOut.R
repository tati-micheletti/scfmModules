# Fixing functions outside

mySimOut$covarParams <- invisible(createParams()) # [ IMPROVE ] This can come from a model afterwards # IF anything goes wrong, take out invisible

mySimOut$covar <- data.table::data.table(matrix(0,
                                           ncol = length(colnames(mySimOut$covarParams[[1]])[2:length(colnames(mySimOut$covarParams[[1]]))]), 
                                           nrow = ncell(mySimOut$habitatMap)))



getNeighborhoodCovars <- function(covarTable = sim$covar,
                                  disturbanceMap = sim$disturbanceMap,
                                  ageMap = sim$ageMap,
                                  habitatMap = sim$habitatMap,
                                  vegMap = sim$vegMap){
  
  
  browser()
  
  library(data.table)
  #ROUND NEIGHBORHOOD
  # distMatrix <- matrix(c(0, rep(1, 3), 0, rep(1, 7), 0, rep(1, 7), 0, rep(1, 3), 0), ncol = 5) #Doesn't work for adjacent
  
  distMatrix <- matrix(c(rep(1, 12), 0, rep(1,12)), ncol = 5)
  
  cellsN <- data.table::data.table(adjacent(habitatMap, 
                                            cells = c(1:ncell(habitatMap)), 
                                            directions = distMatrix, 
                                            include = FALSE, 
                                            pairs = TRUE, 
                                            id = FALSE))
  # NUMERATORS
  extrHabitat <- habitatMap[cellsN$to]
  extrAge <- ageMap[cellsN$to]
  extrDisturbance <- disturbanceMap[cellsN$to]
  
  cellsN$habitat <- extrHabitat
  cellsN$age <- extrAge
  cellsN$disturbance <- extrDisturbance
  
  countPerNeig <- cellsN[, .N, by = from] # Summary Table
  
  #FROM HABITAT: Of the 8 cells that are neighbours of cell 1, how many are 4, how many are 5, how many are 9...?  
  #FROM AGE: Of the 8 cells that are neighbours of cell 1, how many are <30, how many are 30<x<91, >91?
  #FROM DISTURBANCE: Of the 8 cells that are neighbours of cell 1, how many are =2?... CHECK GREEN BOOK TO KNOW WHAT I NEED...
  
  countPerNeig <- 
  
  # actab <- tabulate(distMatrix)
  # actab <- tabulate(ac[,2]/24)
  
  N_LATE <- sum(ageMap[ac[,1]]>90)/24
  N_RICH <- sum(actab>0)
  
  
  # 
  # 
  # # TO SUBSTITUTE IN COVAR TABLE
  # 
  # invisible({
  #   
  #   if(!"ID" %in% names(covarTable)){
  #     covarTable$ID <- seq(1:nrow(covarTable))
  #   }
  #   naCells <- which(is.na(habitatMap[]))
  #   covarTable <- covarTable[ID %in% naCells, names(covarTable)[1:12] := NA_real_]
  #   
  #   # L_CUT
  #   if(!length(which(disturbanceMap[]==2 | ageMap[]<31))==0){
  #     L_CUT.rows <- which(disturbanceMap[]==2 | ageMap[]<31)
  #     covarTable[ID %in% L_CUT.rows, L_CUT := 1L]
  #   }
  #   
  #   # L_ODEC
  #   if(!length(which(disturbanceMap[]==0 & ageMap[]>90 & habitatMap[]==5))==0){
  #     L_ODEC.rows <- which(disturbanceMap[]==0 & ageMap[]>90 & habitatMap[]==5) 
  #     covarTable[ID %in% L_ODEC.rows, L_ODEC := 1L]  
  #   }
  #   
  #   # L_MIX (old)
  #   if(!length(which(disturbanceMap[]==0 & ageMap[]>90 & habitatMap[]==9))==0){
  #     L_MIX.rows <- which(disturbanceMap[]==0 & ageMap[]>90 & habitatMap[]==9)
  #     covarTable[ID %in% L_MIX.rows, L_MIX := 1L]
  #   }
  #   
  #   # L_YDEC
  #   if(!length(which(disturbanceMap[]==0 & (ageMap[]>30 & ageMap[]<91) & habitatMap[]==4))==0){
  #     L_YDEC.rows <- which(disturbanceMap[]==0 & (ageMap[]>30 & ageMap[]<91) & habitatMap[]==4)
  #     covarTable[ID %in% L_YDEC.rows, L_YDEC := 1L]  
  #   }
  #   
  #   # L_MIX (young)
  #   if(!length(which(disturbanceMap[]==0 & (ageMap[]>30 & ageMap[]<91) & habitatMap[]==9))==0){
  #     L_MIX.rows <- which(disturbanceMap[]==0 & (ageMap[]>30 & ageMap[]<91) & habitatMap[]==9)
  #     covarTable[ID %in% L_MIX.rows, L_MIX := 1L]
  #   }
  #   
  #   # L_CC
  #   closed <- which(vegMap[] %in% c(1:5))
  #   medium <- which(vegMap[] %in% c(6:7, 12))
  #   low <- which(vegMap[] %in% c(8:11, 13:15, 25))
  #   covarTable[ID %in% closed, L_CC := 0.8]
  #   covarTable[ID %in% medium, L_CC := 0.6]
  #   covarTable[ID %in% low, L_CC := 0.3]
  #   
  #   #L_WDIS: already 0
  #   
  # })
  # 
  return(invisible(covarTable))
}

nc <- reproducible::Cache(getNeighborhoodCovars,covarTable = mySimOut$covar, 
                   disturbanceMap = mySimOut$disturbanceMap,
                   ageMap = mySimOut$ageMap,
                   habitatMap = mySimOut$habitatMap,
                   vegMap = mySimOut$vegMap)