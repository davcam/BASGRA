## initialise_BASGRA_Saerheim_0102_late_Gri.R ##

## 1. GENERAL INITIALISATION ##
   dyn.load("BASGRA.DLL")
   source('initialisation/initialise_BASGRA_general.R')

## 2. SITE CONDITIONS ##
   year_start     <- as.integer(2000)
   doy_start      <- as.integer(228)
   NDAYS          <- as.integer(868)
   file_weather   <- 'weather/weather_0102_Saerheim_format_bioforsk.txt'
   file_params    <- 'parameters/parameters.txt'
     parcol       <- 7  
   days_harvest[1,] <- c( 2001, 186 )
   days_harvest[2,] <- c( 2001, 248 )
   days_harvest[3,] <- c( 2002, 178 )
   days_harvest[4,] <- c( 2002, 239 )
   
   ## 3. CREATE HARVEST CALENDAR AND WEATHER INPUT ##
   days_harvest   <- as.integer(days_harvest)
   matrix_weather <- read_weather_Bioforsk(year_start,doy_start,NDAYS,file_weather)
   
   ## 4. CREATE VECTOR "PARAMS" ##
   df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
   params         <- df_params[,parcol]
   
   ## 5. CREATE EMPTY MATRIX y ##
   y              <- matrix(0,NDAYS,NOUT)
   