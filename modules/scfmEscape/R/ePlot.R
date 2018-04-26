ePlot <- function(sim){
  
  values(sim$escapeMap) <- 0
  values(sim$escapeMap)[sim$spreadStateE[,indices]] <- 2 #this reference method is believed to be faster
  values(sim$escapeMap)[sim$ignitionLoci] <- 1          #mark the initials specialy
  Plot(sim$escapeMap)
  
  return(invisible(sim))
}