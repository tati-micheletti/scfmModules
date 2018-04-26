Stats <- function(sim){
  
  #browser()
  if (nrow(sim$spreadState) > 0){
    x <- sim$spreadState[,.N,by=id]
    x <- x$N
  }
  else
    x <- numeric(0)
  
  #can't use zero as an index. 
  sim$burnStats[[as.character(time(sim))]] <- x
  
  return(invisible(sim))
}