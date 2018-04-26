sPlot <- function(sim){
  Plot(sim$burnMap, title="Fire map", legendRange=c(0,3), new=TRUE)
  return(invisible(sim))
}
