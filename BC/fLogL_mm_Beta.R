flogL_mm <- function(sims,data,data_min,data_max)
{
   scsims      <- sims     / abs(data)
   scdata_min  <- data_min / abs(data)
   scdata_max  <- data_max / abs(data)
   scdata      <- data     / abs(data)
   aa          <- 1. + 4 * ( (scdata-scdata_min) / (scdata_max-scdata_min) )
   bb          <- 6. - aa 
   scfraction  <- (scsims-scdata_min) / (scdata_max-scdata_min)
   sum( dbeta(scfraction,aa,bb,log=T) )
}
