Stats <- function(disturbanceMap = sim$disturbanceMap, 
                  landscapeAttr = sim$landscapeAttr,
                  maxCaribPop = sim$maxCaribPop,
                  SorensenStats = sim$SorensenStats
                  ) {
  
  burn <- length(which(disturbanceMap[] == 1))
  cut <- length(which(disturbanceMap[] == 2))
  adjcut <- length(which(disturbanceMap[] == 3))
  
  pBurn <- (burn/landscapeAttr$nFlammable)*100
  pCut <- (cut/landscapeAttr$nFlammable)*100
  pAdjcut <- (adjcut/landscapeAttr$nFlammable)*100
  
  pIND <- pCut + pAdjcut
  
  lambda <- 1.19 - (0.0032* pIND)- (0.0030 * pBurn)
  Nt <- SorensenStats$Nt[nrow(SorensenStats)]
  Nt <-  lambda * Nt

  # if(length(maxCaribPop)==0){
  #   maxCaribPop <- 69343.93 # Hard coded just in case... We can see later if it is necessary [ IMPROVE ] 
  # }
  
  if(length(Nt)==0){ # Time 0
    Nt <- maxCaribPop
  }
  
  if(Nt > maxCaribPop){
    Nt <- maxCaribPop
  }
  
  tmpVec <- c(lambda,Nt)
  SorensenStats[nrow(SorensenStats) + 1, ] <- tmpVec
  
  return(invisible(SorensenStats))
}