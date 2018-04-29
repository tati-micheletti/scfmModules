statsInit <- function(sim){
  
  if (is.na(P(sim)$N0)|is.null(P(sim)$N0)){ # 6 females per 100km2
    P(sim)$N0 <- (sim$landscapeAttr$burnyArea/100)*6
    maxCaribPop <- (sim$landscapeAttr$burnyArea/100)*6
  }
  
  sim$Nt <- P(sim)$N0
  sim$SorensenStats <- data.frame(Lambda = numeric(0), Nt = numeric(0))
  
  return(invisible(sim))
}