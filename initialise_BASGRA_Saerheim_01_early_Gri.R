## initialise_BASGRA_Saerheim_01_early_Gri.R ##

## 1. GENERAL INITIALISATION ##
   source('initialise_BASGRA_general.R')

## 2. TIME PERIOD OF SIMULATION ##
   year_start           <- as.integer(2001)
   doy_start            <- as.integer(112)
   NDAYS                <- as.integer(254)
   
## 3. WEATHER AND MANAGEMENT ##
   weatherfile          <- 'input/weather_01_Saerheim_format_bioforsk.txt'
   .Fortran('read_weather', weatherfile,year_start,doy_start,NDAYS)
   harvestdays          <- as.integer(c(164,-1,-1))

## 4. PARAMETERISATION ##
   dataframe_parameters <- read.table('input/parameters_default.txt',header=T,sep="\t",row.names=1)
   parcol               <- 7
   parameters_default   <- dataframe_parameters[,parcol]
   .Fortran('set_params', harvestdays, parameters_default)
   
## 5. OUTPUT ##
   y                    <- matrix(1,NDAYS,NOUT)
