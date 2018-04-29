Stats <- function(sim) {
  
  burn <- sum(which(mySim$disturbanceMap[] == 1))
  cut <- sum(which(mySim$disturbanceMap[] == 2))
  adjcut <- sum(which(mySim$disturbanceMap[] == 3))
  
  pBurn <- (burn/sim$landscapeAttr$nFlammable)*100 # burned in the past 40 years
  pCut <- (cut/sim$landscapeAttr$nFlammable)*100
  pAdjcut <- (adjcut/sim$landscapeAttr$nFlammable)*100
  
  pIND <- pCut + pAdjcut
  
  lambda <- 1.19 - (0.0032* pIND)- (0.0030 * pBurn)
  sim$Nt <-  lambda * sim$Nt
  
  if(sim$Nt > maxCaribPop){
    sim$Nt <- maxCaribPop
  }
  
  tmpVec <- c(lambda,sim$Nt)
  sim$SorensenStats[nrow(sim$SorensenStats) + 1, ] <- tmpVec

  return(invisible(sim))
}