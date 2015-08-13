## run_batch_BASGRA.R ##

################################################################################
## FUNCTIONS USED IN THE FOLLOWING EXAMPLES
################################################################################

## write_output_csv
   write_output_csv <- function(prefix,var) {
     write.table(output,
                 file=paste(prefix,as.character(var),".csv",sep=""),
                 row.names=F,
                 col.names=sapply(1:length(outputNameList),
                                  function(j) paste(outputNameList[j],
                                                    outputUnitList[j]) ),
                 sep="," )
     return()
  }

## print_final_value
   print_final_value <- function(output_onscreen) {
     cat("RUN",as.character(i),
         "\n",
         "Final value of",output_onscreen,"=",
         as.character(output[NDAYS,which(as.character(outputNameList)==output_onscreen)]),
         "\n")
     return()     
   }

################################################################################
## EXAMPLE 1.1: VARYING THE STARTING YEAR ##
################################################################################
   source('initialise_BASGRA_Saerheim_1_early_Gri_weathergen.R')
   year_range <- c(1,2)

   for (i in year_range) {
     year_start       <- as.integer( i )
     .Fortran('read_weather', weatherfile,year_start,doy_start,NDAYS)
     output           <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]
     write_output_csv("res_",i)
     print_final_value("TILTOT")
   }
   
################################################################################
##  EXAMPLE 1.2: ANALYSIS OF MODEL OUTPUT IN A SINGLE CSV-FILE
################################################################################
   csv_filename = 'res_1.csv'
   
   tb2005 <- read.table( csv_filename, header=T, sep="," ) # Reading the file into a dataframe
   summary( tb2005 )              # Descriptive stats of each variable
   names( tb2005 )                # The names of all variables
   attach( tb2005 )               # Making every variable name directly accessible
   plot( Time..y., TILTOT..m.2. ) # Plotting two variables against each other
   summary( TILTOT..m.2. )        # Descriptive stats of one variable
   n <- dim( tb2005 )[1]          # Length of time series
   TILTOT..m.2.[n]                # Final value of one time series
   detach( tb2005 )               # Releasing the dataframe
   
################################################################################
## EXAMPLE 1.3: ANALYSIS OF MODEL OUTPUT IN MULTIPLE CSV-FILES
################################################################################
   year_range <- c(1,2)
   
   par(mfrow=c(1,2))
   for (i in year_range) {
     filename=paste("res_",as.character(i),".csv",sep="")
     tb <- read.table( filename, header=T, sep="," )
     attach( tb )
     plot( Time..y., TILTOT..m.2. )
     detach( tb )
   }