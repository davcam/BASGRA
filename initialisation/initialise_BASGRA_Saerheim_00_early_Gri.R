## initialise_BASGRA_Saerheim_00_early_Gri.R ##

## 1. GENERAL INITIALISATION ##
   dyn.load("BASGRA.DLL")
   source('initialisation/initialise_BASGRA_general.R')

## 2. SITE CONDITIONS ##
   year_start     <- as.integer(1999)
   doy_start      <- as.integer(227)
   NDAYS          <- as.integer(489)
   file_weather   <- 'weather/weather_00_Saerheim_format_bioforsk.txt'
   file_params    <- 'parameters/parameters.txt'
     parcol       <- 6
   days_harvest[1,] <- c( 2000, 150 )
   days_harvest[2,] <- c( 2000, 216 )
   
## 3. CREATE HARVEST CALENDAR AND WEATHER INPUT ##
   days_harvest   <- as.integer(days_harvest)
   matrix_weather <- read_weather_Bioforsk(year_start,doy_start,NDAYS,file_weather)
   
## 4. CREATE VECTOR "PARAMS" ##
   df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
   params         <- df_params[,parcol]
   
## 5. CREATE EMPTY MATRIX y ##
   y              <- matrix(0,NDAYS,NOUT)
