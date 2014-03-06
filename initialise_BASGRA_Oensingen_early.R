## initialise_BASGRA_Saerheim_00_early_Gri.R ##

## 1. MODEL LIBRARY FILE ##
   source("initialise_BASGRAN_general.R")

## 2. TIME PERIOD OF SIMULATION ##
   year_start           <- as.integer(2005)
   doy_start            <- as.integer(1)
   doy_finish           <- as.integer(1095)
   NDAYS                <- doy_finish - doy_start + as.integer(1)
   
## 3. WEATHER AND MANAGEMENT ##
   weatherfile          <- 'input/OE_daily_weather_2005_2007.dat'
   .Fortran('read_weather_oensingen', weatherfile,doy_start,NDAYS)
   harvestdys <- read.table('../Oensingen/OE_doyharv_2005_2007.csv',header=T,sep=',')
   harvestdays          <- as.integer(harvestdys$doy)
   fert <- read.table('../Oensingen/OE_fertilization_2005_2007.csv',header=T,sep=',')
   fertilizer_days          <- as.integer(fert$fertdoy)
   fert_min <- fert$fertmin.kg.ha.
   fert_orgn <- fert$fertnorg.kg.ha.

## 4. PARAMETERISATION ##
   parameters_default   <- read.table('input/parameters_ryegrass.txt',header=T,row.names=1)
   .Fortran('set_params', harvestdays, fertilizer_days,fert_min,fert_orgn,parameters_default[,1])
  
## 5. NUMBER OF OUTPUT VARIABLES ##
   y                    <- matrix(1,NDAYS,NOUT)
