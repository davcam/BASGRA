## Preparing for plotting ##
 # Read the parameter names
   titles     <- parname_BC
   
 # We will write the plots to a pdf file
   pagew <- 11 ; pageh <- 8
   pdf( paste("BC_parameters_traceplots",format(Sys.time(),"_%H_%M.pdf"),sep=""),
        paper="A4r", width=pagew, height=pageh)
   
## Parameter trace plots ##
   nrowsPlots <- ceiling( sqrt((np_BC+1)*pageh/pagew) )
   ncolsPlots <- ceiling( (np_BC+1)/nrowsPlots )
   par( mfrow = c(nrowsPlots,ncolsPlots) )
   par(mar=c(2, 2, 2, 1))
   for (i in seq(1,np_BC)){
        plot( pChain[,i] * sc[i],
		      type='l', xlab="", ylab="",
          main=paste(titles[i],parsites_BC[i]), cex.main=1 )
        abline( v=nBurnin, col='red', lwd=2, lty=2 )
   }
   plot(1,type='n', axes=FALSE, xlab="", ylab="")
   plot_colors <- c("black","red")
   legend("bottomright", c("Parameter trace","End of burn-in"),
          bty="n", lty=c(1,2), lwd=c(1,2), col=plot_colors, title = "LEGEND:")


## Closing ##
   dev.off()
