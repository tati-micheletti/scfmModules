genFireMapAttr <- function(sim){
  #browser()
  #we assume the raster is a regular grid, not in lat-long
  #cellSize <- mean(raster::values(area(sim$flammableMap,na.rm=TRUE)),
  #                 na.rm=TRUE)*100 #area returns km^2!
  
  cellSize <- prod(res(sim$flammableMap))/1e4  #copied below from template sim$vegMap
  
  if (is.na(cellSize))
    stop("scfmLandcoverInit: cellSize is NA")
  
  nFlammable<-table(values(sim$flammableMap), useNA="no")["0"] 
  if (sim$nNbrs==8)
    w<-matrix(c(1,1,1,1,0,1,1,1,1),nrow=3,ncol=3)
  else if (sim$nNbrs==4)
    w<-matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
  else 
    stop("illegal global neighbours spec")
  tmp <- focal(sim$flammableMap, w, na.rm=TRUE) #default function is sum(...,na.rm)
  x <- values(tmp)
  x <- x[raster::values(sim$flammableMap)==0] #only count neighbours for flammable cells!
  x <- sim$nNbrs - x  #need to invert, because we are counting the nonflamy 1's
  nv <- table(x,useNA="no")
  nNbrs <- rep(0,9) #guard against the terrible chance that 
  #not all nNbrs values are realised on the landscape. 
  nNbrs[as.integer(names(nv))+1] <- nv
  names(nNbrs) <- 0:8
  sim$landscapeAttr<-list(cellSize=cellSize,nFlammable=nFlammable,
                          burnyArea=cellSize*nFlammable, nNbrs=nNbrs)
  return(invisible(sim))
}