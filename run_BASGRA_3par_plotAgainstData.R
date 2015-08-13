## This file requires to be available a txt file called 'BASGRA_par.txt'.
## The txt-file should have the same structure and number of rows as
## 'parameters_default.txt': parameter names in the first column and then
## AT LEAST THREE columns with parameter values.

## UNCOMMENT INITIALISATION AND DATA FILE FOR ONLY ONE SITE !!
#   source( 'initialise_BASGRA_Holt_0506_winter_Gri.R' )
#   data_all <- read.table( 'BC/data_calibration_Holt_0506_winter_Gri.txt', header=F,sep="" )
   source( 'initialise_BASGRA_Saerheim_00_early_Gri.R' )
   data_all <- read.table( 'BC/data_calibration_Saerheim_00_early_Gri.txt', header=F,sep="" )

## SETTINGS FOR THE MEASUREMENT-UNCERTAINTIES
     cv_default <- 0.5
     cv_DM      <- 0.05
     cv_LAI     <- 0.1
     cv_TILTOT  <- 0.2
     sd_LT50    <- 5
     min_FRTILG <- 0.3
     max_FRTILG <- 0.9
   
## PREPARING DATA FOR LATER PLOTTING
# Split the data into those for which the uncertainty is given as +- data_sd
# and those for which it is given as a range [data_min,data_max]
   data       <- data_all[which(data_all$V1!='FRTILG'),]
   data_mm    <- data_all[which(data_all$V1=='FRTILG'),]

   data_name  <- data[,1]
   data_time  <- data[,2]
   data_value <- data[,3]
   data_sd    <- abs(data_value)                             * cv_default
   data_sd[which(data_name=='DM')]     <-
                 abs(data_value[which(data_name=='DM')])     * cv_DM
   data_sd[which(data_name=='LAI')]    <-
                 abs(data_value[which(data_name=='LAI')])    * cv_LAI
   data_sd[which(data_name=='TILTOT')] <-
                 abs(data_value[which(data_name=='TILTOT')]) * cv_TILTOT
   data_sd[which(data_name=='LT50')]   <-                      sd_LT50
   
   if(dim(data_mm)[1]>0) {
     data_mm_name  <- data_mm[,1]
     data_mm_time  <- data_mm[,2]
     data_mm_value <- data_mm[,3]
     data_mm_min   <- rep( 0, length(data_mm_name) )
     data_mm_max   <- rep( 1, length(data_mm_name) )
     data_mm_min[which(data_mm_name=='FRTILG')] <-             min_FRTILG
     data_mm_max[which(data_mm_name=='FRTILG')] <-             max_FRTILG
   }

## LINKING DATA TO MODEL OUTPUTS
 # The data_index gives the model output number for each data point
   data_index       <- sapply( 1:length(data_name),
             function(i) which(as.character(outputNameList)==data_name[i]) )
   if(dim(data_mm)[1]>0) {
     data_mm_index <- sapply( 1:length(data_mm_name),
                         function(i) which(as.character(outputNameList)==data_mm_name[i]) )
   }

## READING IN THE THREE PARAMETER VECTORS
   dataframe_par    <- read.table('BASGRA_par.txt',header=T,sep="\t",row.names=1)
   
## RUNNING THE MODEL WITH PARAMETER VECTOR 1
   parnewcol        <- 1
   par1             <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par1)
   output1          <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## RUNNING THE MODEL WITH PARAMETER VECTOR 2
   parnewcol        <- 2
   par2             <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par2)
   output2          <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## RUNNING THE MODEL WITH PARAMETER VECTOR 3
   parnewcol        <- 3
   par3             <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par3)
   output3          <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## CREATE PLOT
   pdf('output_3par_Data.pdf')
   outputsMeasured  <- unique(data_index)
   if(dim(data_mm)[1]>0) {
     outputsMeasured_mm <- unique(data_mm_index)
     noutputsMeasured   <- length(outputsMeasured) + length(outputsMeasured_mm)
   } else {
     noutputsMeasured   <- length(outputsMeasured)
   }
   nrowsPlots       <- ceiling(sqrt(noutputsMeasured))
   ncolsPlots       <- ceiling(noutputsMeasured/nrowsPlots)
   par(mfrow=c(nrowsPlots,ncolsPlots))
   
   for (p in outputsMeasured) {
     datap   <- which( data_name == as.character(outputNameList[p]) )
     ucl     <- data_value[datap] + data_sd[datap]
     lcl     <- data_value[datap] - data_sd[datap]
     g_range <- range(output1[,p],output2[,p],output3[,p],ucl,lcl)
     plot(output1[,1],output1[,p],
          xlab="",ylab=outputNameList[p],type='l',col='red',lwd=3,ylim=g_range)
     points(output2[,1],output2[,p],type='l',col='black',lwd=3)
     points(output3[,1],output3[,p],type='l',col='blue',lwd=3)
     points(year_start+(data_time[datap]-0.5)/366,data_value[datap],col='blue',lwd=2, cex=2)
     arrows(year_start+(data_time[datap]-0.5)/366,ucl,year_start+(data_time[datap]-0.5)/366,lcl,
                             col='blue',lwd=1,angle=90,code=3,length=0.05)
   }
   
   if(dim(data_mm)[1]>0) {
     for (p in outputsMeasured_mm) {
       datap   <- which( data_mm_name == as.character(outputNameList[p]) )
       ucl     <- data_mm_max[datap]
       lcl     <- data_mm_min[datap]
       g_range <- range(output1[,p],output2[,p],output3[,p],ucl,lcl)
       plot(output1[,1],output1[,p],
            xlab="",ylab=outputNameList[p],type='l',col='red',lwd=3,ylim=g_range)
       points(output2[,1],output2[,p],type='l',col='black',lwd=3)
       points(output3[,1],output3[,p],type='l',col='blue',lwd=3)
       points(year_start+(data_mm_time[datap]-0.5)/366,data_mm_value[datap],col='blue',lwd=2, cex=2)
       arrows(year_start+(data_mm_time[datap]-0.5)/366,ucl,
              year_start+(data_mm_time[datap]-0.5)/366,lcl,
              col='blue',lwd=1,angle=90,code=3,length=0.05)
     }
   }
   
   dev.off()