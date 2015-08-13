## Preparing for plotting ##
 # Read the parameter names
   titles <- dataframe_parameters_BC[,1]
 # We will write the plots to a pdf file:
   pdf('BC_parameters_priorbeta_histograms.pdf')

## Histogram plots ##
   par(mfrow=c(5,4))
   par(mar=c(2, 2, 2, 1))
   nbreaks <- 20
   for (i in seq(1,np)){
        hist(pChain[nBI:nChain,i]*abs(parmod[i]),xlab="",ylab="",main=titles[i],breaks=nbreaks,freq=FALSE,xlim=c(parmin[i],parmax[i]))
        parseq <- seq(parmin[i],parmax[i],(parmax[i]-parmin[i])/100)
        points(parseq,dbeta((parseq-parmin[i])/(parmax[i]-parmin[i]),aa[i],bb[i])/(parmax[i]-parmin[i]),type='l',col='red')
   }

## Closing ##
   dev.off()
