escapeProbDelta<-function(p0,w,hatPE){
  abs(sum(w*(1-(1-p0)**(0:8)))-hatPE)  
}