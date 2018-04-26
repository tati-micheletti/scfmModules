Age <- function(sim){
  oldest = if (is.na(P(sim)$maxAge)) 2^31-1 else P(sim)$maxAge
  sim$ageMap <- setValues(sim$ageMap, pmin(oldest, 
                                           getValues(sim$ageMap)+P(sim)$returnInterval))
  return(invisible(sim))
}