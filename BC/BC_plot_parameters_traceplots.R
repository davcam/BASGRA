## Preparing for plotting ##
 # Read the parameter names
   titles <- dataframe_parameters_BC[,1]
 # We will write the plots to a pdf file
   pdf('BC_parameters_traceplots.pdf')

## Parameter trace plots ##
   par(mfrow=c(5,4))
   par(mar=c(2, 2, 2, 1))
   for (i in seq(1,np)){
        plot(pChain[nBI:nChain,i]*abs(parmod[i]),type='l',xlab="",ylab="",main=titles[i])
   }

## Closing ##
   dev.off()
