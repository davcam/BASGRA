## 1. INITIALISATION ##
   source('initialise_BASGRA_Oensingen_early.R')

## 2. RUNNING ##
   output <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]

## 3. PRINTING AND PLOTTING ##
   print(date())
 # Displaying the final value of the tenth output variable (normally LAI)
 # Note that output[,1] is time, so output[,n] is variable number n-1
   print(format(output[NDAYS,11],digits=15))
 # Producing plots of the first 9 output variables (output[,1] is time)
   pdf('ryegrass-plot.pdf')
   par(mfrow=c(3,3))
   ttitles <- c('doy','CLV','CLVD','CLVH','CRES','CRT','CSTUB','LAI','ROOTD','TILG','TILV','YIELD','YIELDN','ET','WAL')
   for (i in seq(5,18)){
       plot(output[,1],output[,i],main=ttitles[i-3])
     }
   dev.off()
