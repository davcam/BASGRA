## This file requires to be available a txt file called 'BASGRA_par.txt'.
## The txt-file should have the same structure and number of rows as
## 'parameters_default.txt': parameter names in the first column and then
## AT LEAST THREE columns with parameter values.


## UNCOMMENT ONLY ONE OF THE FOLLOWING INITIALISATION FILES !!
#   source('initialise_BASGRA_Holt_0506_winter_Gri.R')
   source('initialise_BASGRA_Saerheim_00_early_Gri.R')
   
## READING THE PARAMETER VECTORS IN 'BASGRA_par.txt'
   dataframe_par <- read.table('BASGRA_par.txt',header=T,sep="\t",row.names=1)

## RUNNING THE MODEL WITH PARAMETER VECTOR 1
   parnewcol      <- 1
   par1 <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par1)
   output1        <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## RUNNING THE MODEL WITH PARAMETER VECTOR 2
   parnewcol      <- 2
   par2 <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par2)
   output2        <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## RUNNING THE MODEL WITH PARAMETER VECTOR 3
   parnewcol      <- 3
   par3 <- dataframe_par[,parnewcol]
   .Fortran('set_params', harvestdays, par3)
   output3        <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## CREATE PLOTS
   pdf('output_3par_AllOutputs.pdf')
   par(mfrow=c(4,4))
   par(mar=c(2, 2, 2, 1))
   for (p in 4:NOUT) {
     g_range <- range(output1[,p],output2[,p],output3[,p])
     plot(output1[,1],output1[,p],xlab="",main=outputNameList[p],type='l',
                                                   col='red',lwd=3,ylim=g_range)
     points(output2[,1],output2[,p],type='l',col='black',lwd=3)
     points(output3[,1],output3[,p],type='l',col='blue',lwd=3)
   }
   
   dev.off()
