## 1. INITIALISATION ##
   source('initialise_BASGRA_Saerheim_00_early_Gri.R')

## 2. RUNNING ##
   output <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]

## 3. OUTPUT ##
   nrows = 2 ; ncols = 2 ;
   plotNameList <- c("DAVTMP","CLV","LAI","TILTOT")
   plotOutputs(nrows,ncols,plotNameList)

   nrows = 3 ; ncols = 3 ;
   plotNameList <- c(
   "DAVTMP",
   "CLV","CLVD","CLVH","CRES","CRT","CST","CSTUB",
   "DRYSTOR","Fdepth","LAI","LT50","O2","PHEN",
   "ROOTD","Sdepth","TANAER","TILG","TILV",
   "WAL","WAPL","WAPS","WAS","WETSTOR",
   "DM","RES","LERG","NELLVG","RLEAF","SLA","TILTOT",
   "FRTILG")
   plotOutputs(nrows,ncols,plotNameList)   
