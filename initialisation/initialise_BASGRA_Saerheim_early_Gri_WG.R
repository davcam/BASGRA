## initialise_BASGRA_Saerheim_early_Gri_WG.R ##

## 1. GENERAL INITIALISATION ##
   dyn.load("BASGRA_WG.DLL")
   source('initialisation/initialise_BASGRA_general.R')

## 2. SITE CONDITIONS ##
   year_start     <- as.integer(  1)
   doy_start      <- as.integer(124)
   NDAYS          <- as.integer(242)
   file_weather   <- 'weather/AP_BCM_AB1_2050_year1.txt'
   file_params    <- 'parameters/parameters.txt'
     parcol       <- 6
   days_harvest[1,] <- c( 1, 150 )
   days_harvest[2,] <- c( 1, 216 )
   days_harvest[3,] <- c( 1, 253 )
   
## 3. CREATE HARVEST CALENDAR AND WEATHER INPUT ##
   days_harvest   <- as.integer(days_harvest)
   matrix_weather <- read_weather_WG(year_start,doy_start,NDAYS,file_weather)
   
## 4. CREATE VECTOR "PARAMS" ##
   df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
   params         <- df_params[,parcol]
   
## 5. CREATE EMPTY MATRIX y ##
   y              <- matrix(0,NDAYS,NOUT)
