## Running the MCMC ##
   covPar <- vcovProp
   nAccepted <- 0
   cat("Iteration","|","% Accepted","|","logPrior0","|","logL0","\n")
   for (j in 2:nChain)
     {
     # Give intermediate output to the screen for tracking the progress of the MCMC
       jint <- min(1000,round(nChain/20))
       if (j%%jint == 0) cat("Iteration",j,"|",round(100*nAccepted/(j-nBI)),"%","|",logPrior0,"|",logL0,"\n")
     # Select the candidate parameter vector and calculate its prior probability
       candidatepValues  <- mvrnorm(n=1, pValues, fPropGelman*covPar, tol=1e-6, empirical=FALSE)
       reflectionFromMin <- pmin(0.,candidatepValues-parmin/abs(parmod))
       reflectionFromMax <- pmax(0.,candidatepValues-parmax/abs(parmod))
       candidatepValues  <- candidatepValues - 2.*reflectionFromMin - 2.*reflectionFromMax
       pBetaValues       <- ( candidatepValues[1:np]         - parmin[1:np]/abs(parmod[1:np]) ) /
                            ( parmax[1:np]/abs(parmod[1:np]) - parmin[1:np]/abs(parmod[1:np]) )
       logPrior1Beta     <- sum( dbeta(pBetaValues,aa,bb,log=T) )
       logPrior1         <- logPrior1Beta
     # Unscale parameters and run the model
       Sim_candidatepValues <- candidatepValues*abs(parmod)
       .Fortran('set_params_BC', Sim_candidatepValues)
       output               <- .Fortran('basgra', NDAYS,NOUT,y)[[3]]
     # Select the model outputs for which there are measurements and calculate likelihood
       output_calibr <- diag( output[output_calibr_rows, data_index] )
       logL1         <- flogL( output_calibr, data_value, data_sd )
     # Check whether the candidate parameter vector is accepted, and extend the parameter chain.
       logalpha          <- logPrior1 + logL1 - (logPrior0 + logL0)
       if (log(runif(1,0,1)) < logalpha)
         {
           pValues   <- candidatepValues
           logPrior0 <- logPrior1
           logL0     <- logL1
           if (logL0 > logMaxL) {
               logMaxL <- logL0
               parMaxL <- pValues }
           if ((logPrior0 + logL0) > logMAP) { 
               logMAP  <- logPrior0 + logL0
               parMAP  <- pValues }
           if (j > nBI)
             { nAccepted <- nAccepted + 1 }
         }
       pChain[j,] <- pValues
       if (j == (nBI+1000))
           {
            avePar <- apply(pChain[(nBI+1):j,], 2, mean)
            covPar <- cov(pChain[(nBI+1):j,],pChain[(nBI+1):j,])
           }
         if (j > (nBI+1000))
           {
        for (l in 1:np)
              covPar[1:np,l] <- (j-nBI-2.)/(j-nBI-1.)*covPar[1:np,l] + 
                             1./(j-nBI)*( pChain[j,1:np]*pChain[j,l] - 
                                          pChain[j,1:np]*avePar[l] -
                                          avePar[1:np]*pChain[j,l] +
                                          avePar[1:np]*avePar[l] )
    
    #-----  update the mean vector
            avePar <- pChain[j,]/(j-nBI) + (j-nBI-1)/(j-nBI)*avePar
           }
    
     }
