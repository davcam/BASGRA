## initialise_BASGRA_Saerheim_1_early_Gri.R ##

## 1. GENERAL INITIALISATION ##
   source('initialise_BASGRA_general.R')

## 2. TIME PERIOD OF SIMULATION ##
   year_start           <- as.integer(1)
   doy_start            <- as.integer(124)
   NDAYS                <- as.integer(242)
   
## 3. WEATHER AND MANAGEMENT ##
#   weatherfile <- 'input/weather_00_Saerheim_format_bioforsk.txt'
   weatherfile <- 'input/AP_BCM_AB1_2050_year1.txt'
   .Fortran('read_weather', weatherfile,year_start,doy_start,NDAYS)
   harvestdays          <- as.integer(c(150,216,253))

## 4. PARAMETERISATION ##
   dataframe_parameters <- read.table('input/parameters_default.txt',header=T,sep="\t",row.names=1)
   parcol               <- 6
   parameters_default   <- dataframe_parameters[,parcol]
   .Fortran('set_params', harvestdays, parameters_default)
   
## 5. OUTPUT ##
   y                    <- matrix(1,NDAYS,NOUT)
