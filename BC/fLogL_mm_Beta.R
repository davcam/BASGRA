flogL_mm <- function(sims,data,data_min=0,data_max=1,cv=0.2) {
  scsims      <- (sims-data_min) / (data_max-data_min)
  scmode      <- (data-data_min) / (data_max-data_min)
  aplusb      <- max( 3, (1-scmode)/(scmode*cv^2)-1 ) # Approximation
  a           <- scmode * (aplusb-2) + 1
  b           <- aplusb - a
  sum( dbeta(scsims,a,b,log=T) )
}
