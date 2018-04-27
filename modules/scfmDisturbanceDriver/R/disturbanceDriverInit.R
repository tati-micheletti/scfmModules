disturbanceDriverInit = function(sim) {
  
  #this table contains calibration data for several landscape sizes
  #and several min fire sizes (1 or 2 cells), organised by collumn.
  #The data were made by Steve Cumming in June 2013 for a whole other purpose.
  #I chose the one that seems most appropriate to me
  #browser()
  
  y <- log(sim$spreadCalibrationData[,paste("ls",1e3,"fs",2,sep="")])
  x <- sim$spreadCalibrationData$pjmp
  m.glm <- glm(x~I(log(y)))
  mfs <- sim$fireRegimeParameters$xBar/sim$fireMapAttr$cellSize #mean size escaped fires in cells
  pJmp<-sum(m.glm$coeff*c(1,log(mfs)))
  
  w <- sim$fireMapAttr$nNbrs
  w <- w/sum(w)
  hatPE <- sim$fireRegimeParameters$pEscape
  foo <- optimise(sim$escapeProbDelta,
                interval=c(hatP0(hatPE,globals(sim)$neighbours),hatP0(hatPE,floor(sum(w*0:8)))),
                tol=1e6,
                w=w,
                hatPE=hatPE)
  #do some sanity tests to ensure convergence
  #also, it is almost obvious that the true minimum must occurr within the interval specified in the 
  #call to optimise, but I have not proved it, nor am I certain that the function being minimised is 
  #monotone.
  
  rate <- sim$fireRegimeParameters$rate * sim$fireMapAttr$cellSize #fireRegimeModel and this module must agree on 
  #an annual time step. How to test / enforce?
  pIgnition <- rate #approximate Poisson arrivals as a Bernoulli process at cell level.
  #for Poisson rate << 1, the expected values are the same, partially accounting
  #for multiple arrivals within years. Formerly, I used a poorer approximation
  #where 1-p = P[x==0 | lambda=rate] (Armstrong and Cumming 2003).
  
  
  sim$disturbanceGeneratorParameters<-list(pJmp=pJmp,p0=foo$minimum,
                                           naiveP0=sim$hatP0(sim$fireRegimeParameters$pEscape,8), 
                                           pIgnition=pIgnition)
  
  return(invisible(sim))
}