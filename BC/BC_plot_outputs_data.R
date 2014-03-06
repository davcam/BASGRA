## Calculate model output for the prior mode
   .Fortran( 'set_params_BC', parmod )
   outputPriorMode <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## Calculate model output for the MAP parameter vector
   .Fortran( 'set_params_BC', parMAP * abs(parmod) )
   outputMAP       <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## Calculate model output for the MaxL parameter vector
   .Fortran( 'set_params_BC', parMaxL * abs(parmod) )
   outputMaxL      <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]

## Calculate model output for a sample from the posterior
 # Take a sample (of size nSample) from the chain generated using MCMC
   nSample      <- 100
   nStep        <- (nChain-nBI) / nSample
   outputSample <- array( 0, c(nSample,NDAYS,NOUT) )
   ii           <- 0   
   for (j in seq(nBI+nStep, nChain, nStep)) {
     ii <- ii+1
     .Fortran( 'set_params_BC', pChain[j,] * abs(parmod) )
     outputSample[ii,,] <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]
   } # end of sample loop
 # Analyse the posterior output sample: calculate quantiles 5% and 95%
   q5  <- sapply( 1:NDAYS, function(i) sapply(1:NOUT,function(j)quantile(outputSample[,i,j],0.05)) )
   q95 <- sapply( 1:NDAYS, function(i) sapply(1:NOUT,function(j)quantile(outputSample[,i,j],0.95)) )

## Create plots
   pdf('BC_outputs_data.pdf')
   
   outputsMeasured <- unique(data_index)
   #if(dim(data_mm)[1]>0) {
   #  outputsMeasured_mm <- unique(data_mm_index)
   #  noutputsMeasured   <- length(outputsMeasured) + length(outputsMeasured_mm)
   #} else {
   noutputsMeasured   <- length(outputsMeasured)
   #}
   nrowsPlots       <- ceiling(sqrt(noutputsMeasured))
   ncolsPlots       <- ceiling(noutputsMeasured/nrowsPlots)
   par(mfrow=c(nrowsPlots,ncolsPlots))
   
   for (p in outputsMeasured) {
     datap   <- which( data_name == as.character(outputNameList[p]) )
     ucl     <- data_value[datap] + data_sd[datap]
     lcl     <- data_value[datap] - data_sd[datap]
     g_range <- range(outputPriorMode[,p],outputMAP[,p],outputMaxL[,p],q5[p],q95[p],ucl,lcl)
     plot(outputPriorMode[,1],outputPriorMode[,p],
          xlab="", ylab=paste(outputNameList[p],outputUnitList[p]),
          type='l',col='red',lwd=3,ylim=g_range)
     points(outputMAP[,1],outputMAP[,p],type='l',col='black',lwd=3)
     points(outputMaxL[,1],outputMaxL[,p],type='l',col='blue',lwd=2)
     points(q5       [1,],q5       [p,],type='l',col='black',lwd=2, lty=3)
     points(q95      [1,],q95      [p,],type='l',col='black',lwd=2, lty=3)
     points(year_start+(data_time[datap]-0.5)/366, data_value[datap], col='blue',lwd=2, cex=1,pch=20)
     arrows(year_start+(data_time[datap]-0.5)/366,ucl,
            year_start+(data_time[datap]-0.5)/366,lcl,
            col='blue',lwd=1,angle=90,code=3,length=0.05)
   }
   
   ## if(dim(data_mm)[1]>0) {
   ##   for (p in outputsMeasured_mm) {
   ##     datap   <- which( data_mm_name == as.character(outputNameList[p]) )
   ##     ucl     <- data_mm_max[datap]
   ##     lcl     <- data_mm_min[datap]
   ##     g_range <- range(outputPriorMode[,p],outputMAP[,p],outputMaxL[,p],q5[p],q95[p],ucl,lcl)
   ##     plot(outputPriorMode[,1],outputPriorMode[,p],
   ##          xlab="", ylab=paste(outputNameList[p],outputUnitList[p]),
   ##          type='l',col='red',lwd=3,ylim=g_range)
   ##     points(outputMAP [,1],outputMAP [,p],type='l',col='black',lwd=3)
   ##     points(outputMaxL[,1],outputMaxL[,p],type='l',col='blue' ,lwd=2)
   ##     points(q5        [1,],q5        [p,],type='l',col='black',lwd=2, lty=3)
   ##     points(q95       [1,],q95       [p,],type='l',col='black',lwd=2, lty=3)
   ##     points(year_start+(data_mm_time[datap]-0.5)/366, data_mm_value[datap], col='blue',lwd=2, cex=2)
   ##     arrows(year_start+(data_mm_time[datap]-0.5)/366,ucl,
   ##            year_start+(data_mm_time[datap]-0.5)/366,lcl,
   ##            col='blue',lwd=1,angle=90,code=3,length=0.05)
   ##   }
   ## }
   
   dev.off()
