# Fixing functions outside

mySimOut$covarParams <- invisible(createParams()) # [ IMPROVE ] This can come from a model afterwards # IF anything goes wrong, take out invisible

mySimOut$covar <- data.table::data.table(matrix(0,
                                           ncol = length(colnames(mySimOut$covarParams[[1]])[2:length(colnames(mySimOut$covarParams[[1]]))]), 
                                           nrow = ncell(mySimOut$habitatMap)))

names(mySimOut$covar) <- colnames(mySimOut$covarParams[[1]])[2:length(colnames(mySimOut$covarParams[[1]]))]


getNeighborhoodCovars <- function(covarTable = sim$covar,
                                  disturbanceMap = sim$disturbanceMap,
                                  ageMap = sim$ageMap,
                                  habitatMap = sim$habitatMap,
                                  vegMap = sim$vegMap){
  
  library(data.table)
  
  distMatrix <- matrix(c(rep(1, 12), 0, rep(1,12)), ncol = 5)
  
  cellsN <- data.table::data.table(raster::adjacent(habitatMap, 
                                            cells = c(1:ncell(habitatMap)), 
                                            directions = distMatrix, 
                                            include = FALSE, 
                                            pairs = TRUE, 
                                            id = FALSE))
  
  extrHabitat <- habitatMap[cellsN$to]
  extrAge <- ageMap[cellsN$to]
  extrDisturbance <- disturbanceMap[cellsN$to]
  
  cellsN$habitat <- extrHabitat
  cellsN$age <- extrAge
  cellsN$disturbance <- extrDisturbance
  
  countPerNeig <- cellsN[, .N, by = from] # Summary Table
  
  cropMap <- vegMap[]*0
  cropMap[] <- ifelse(cropMap[]==0, 1, NA)
  
  N_CUT <-  cellsN[, sum(disturbance==2 | age<31, na.rm = TRUE), by = from]
  N_CUT <- N_CUT$V1/countPerNeig$N
  N_CUT <- N_CUT*cropMap

  N_LATE <-  cellsN[, sum(disturbance==0 & age>90 & habitat==5, na.rm = TRUE), by = from]
  N_LATE <- N_LATE$V1/countPerNeig$N
  N_LATE <- N_LATE*cropMap
  
  N_DEC <-  cellsN[, sum(disturbance==0 & (habitat==4 | habitat==5), na.rm = TRUE), by = from]
  N_DEC <- N_DEC$V1/countPerNeig$N
  N_DEC <- N_DEC*cropMap
  
  N_MIX <-  cellsN[, sum(disturbance==0 & habitat==9, na.rm = TRUE), by = from]
  N_MIX <- N_MIX$V1/countPerNeig$N
  N_MIX <- N_MIX*cropMap
  
  N_SB <-  cellsN[, sum(disturbance==0 & habitat==7, na.rm = TRUE), by = from]
  N_SB <- ifelse(N_SB$V1>0,1,0)
  N_SB <- N_SB*cropMap
  
  N_RICH <-  cellsN[, length(unique(habitat)), by = from]
  N_RICH <- N_RICH$V1
  N_RICH <- N_RICH*cropMap
  
  covarTable$N_CUT <- N_CUT
  covarTable$N_DEC <- N_DEC
  covarTable$N_MIX <- N_MIX
  covarTable$N_LATE <- N_LATE
  covarTable$N_SB <- N_SB
  covarTable$N_RICH <- N_RICH
  
  # PUT RESULTS INTO covarTable
  
  ############ CODE 1 ################# - Alternative
  
  # cv <- sapply(pixel, function(x){
  #   
  #   subSet <- countPerNeig[from==x,]
  #   CUT <- sum(subSet$disturbance==2 | subSet$age<31, na.rm = TRUE)
  #   LATE <- sum(subSet$disturbance==0 & subSet$age>90 & subSet$habitat==5, na.rm = TRUE)
  #   DEC <- sum(subSet$disturbance==0 & (subSet$habitat==4 | subSet$habitat==5), na.rm = TRUE)
  #   MIX <- sum(subSet$disturbance==0 & subSet$habitat==9, na.rm = TRUE)
  #   dnom <- countPerNeig[from==x, N]
  #   
  #   N_CUT <- CUT/dnom
  #   N_LATE <- LATE/dnom
  #   N_DEC <- DEC/dnom
  #   N_MIX <- MIX/dnom
  #   N_SB <- ifelse(sum(subSet$disturbance==0 & subSet$habitat==7)>0, 1, 0)
  #   N_RICH <- length(unique(subSet$habitat))
  # 
  #   rw <- c(N_CUT, N_LATE, N_DEC, N_MIX, N_SB, N_RICH)
  #   
  #   return(rw)
  # })
  # 
  
  
  # check covarTable
  # place cv in covarTable
  
  
  ############ CODE 2 #################
  
# N_CUT <- sapply(pixel, function(x){
#   
#       subSet <- countsType[from==x,]
#       nom <- sum(subSet$disturbance==2 | subSet$age<31)
#       dnom <- countPerNeig[from==x, N]
#       N_CUT <- nom/dnom
#       
#       return(N_CUT)
#     })
# 
# N_LATE <- sapply(pixel, function(x){
#   
#   subSet <- countsType[from==x,]
#   nom <- sum(subSet$disturbance==0 & subSet$age>90 & subSet$habitat==5)
#   dnom <- countPerNeig[from==x, N]
#   N_LATE <- nom/dnom
#   
#   return(N_LATE)
# })
# 
# N_DEC <- sapply(pixel, function(x){
#   
#   subSet <- countsType[from==x,]
#   nom <- sum(subSet$disturbance==0 & (subSet$habitat==4 | subSet$habitat==5))
#   dnom <- countPerNeig[from==x, N]
#   N_DEC <- nom/dnom
#   
#   return(N_DEC)
# })
# 
# N_MIX <- sapply(pixel, function(x){
#   
#   subSet <- countsType[from==x,]
#   nom <- sum(subSet$disturbance==0 & subSet$habitat==9)
#   dnom <- countPerNeig[from==x, N]
#   N_MIX <- nom/dnom
#   
#   return(N_MIX)
# })
# 
# N_SB <- sapply(pixel, function(x){
#   
#   subSet <- countsType[from==x,]
#   nom <- sum(subSet$disturbance==0 & subSet$habitat==7)
#   N_SB <- ifelse(nom>0, 1, 0)
#   
#   return(N_SB)
# })
# 
# N_RICH <- sapply(pixel, function(x){
#   
#   subSet <- countsType[from==x,]
#   N_RICH <- length(unique(subSet$habitat))
#   
#   return(N_RICH)
# })
# 
# 
# covarTable[, N_CUT := N_CUT]
# covarTable[, N_LATE := N_LATE]
# covarTable[, N_DEC := N_DEC]
# covarTable[, N_MIX := N_MIX]
# covarTable[, N_SB := N_SB]
# covarTable[, N_RICH := N_RICH]



  return(invisible(covarTable))
}

a <- getNeighborhoodCovars(covarTable = mySimOut$covar,
                                       disturbanceMap = mySimOut$disturbanceMap,
                                       ageMap = mySimOut$ageMap,
                                       habitatMap = mySimOut$habitatMap,
                                       vegMap = mySimOut$vegMap)
