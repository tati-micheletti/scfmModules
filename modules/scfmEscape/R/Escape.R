Escape <- function(sim){
  #browser()
  sim$spreadStateE <- data.table(NULL) #ensure always in a determinate state
  if (length(sim$ignitionLoci)> 0){
    #note that ifesles won't work once these things are nonscalars.
    p0 <- if ("scfmPars" %in% names(objs(sim)))
      sim$scfmPars$p0
    else
      P(sim)$p0
    # browser()
    #print(paste("Year",time(sim), "loci = ", length(sim$ignitionLoci)))
    
    pMap <- sim$flammableMap
    pMap <- (!pMap) * p0
    
    sim$spreadStateE <- SpaDES.tools::spread(landscape=sim$flammableMap,
                                             loci=sim$ignitionLoci,
                                             iterations=1,
                                             spreadProb=pMap,
                                             mask=sim$flammableMap,
                                             directions=sim$nNbrs,
                                             returnIndices=TRUE, 
                                             id=TRUE)
  }
  return(invisible(sim))
}