## Running the MCMC ##
   nAccepted <- 0
   cat("Iteration","|","% Accepted","|","logPrior0","|","logL0","\n")
   for (j in 2:nChain)
     {
     # Give intermediate output to the screen for tracking the progress of the MCMC
       jint <- min(1000,round(nChain/20))
       if (j%%jint == 0) cat("Iteration",j,"|",round(100*nAccepted/(j-nBI)),"%","|",logPrior0,"|",logL0,"\n")
     # Select the candidate parameter vector and calculate its prior probability
       candidatepValues  <- mvrnorm(n=1, pValues, vcovProp, tol=1e-6, empirical=FALSE)
       reflectionFromMin <- pmin(0.,candidatepValues-parmin/abs(parmod))
       reflectionFromMax <- pmax(0.,candidatepValues-parmax/abs(parmod))
       candidatepValues  <- candidatepValues - 2.*reflectionFromMin - 2.*reflectionFromMax
       pBetaValues       <- ( candidatepValues[1:np]         - parmin[1:np]/abs(parmod[1:np]) ) /
                            ( parmax[1:np]/abs(parmod[1:np]) - parmin[1:np]/abs(parmod[1:np]) )
       logPrior1Beta     <- sum( dbeta(pBetaValues,aa,bb,log=T) )
       logPrior1         <- logPrior1Beta
     # Unscale parameters, run the model for each site and calculate likelihood
       logL1 <- 0 ; for (s in 1:nSites) {
         source(sitesettings_filenames[s])
         Sim_candidatepValues <- candidatepValues*abs(parmod)
         .Fortran('set_params_BC', Sim_candidatepValues)
         output             <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]
         doy_simstart       <- output[1,3]
         output_calibr_rows <- data_time[[s]] - doy_simstart + 1
         output_calibr      <- if(length(data_value[[s]])==1) {
                          output[output_calibr_rows, data_index[[s]]]
           } else { diag( output[output_calibr_rows, data_index[[s]]] ) }
         if(dim(database_mm[[s]])[1]>0) {
           output_mm_calibr_rows <- data_mm_time[[s]] - doy_simstart + 1
           output_mm_calibr      <- if(length(data_mm_value[[s]])==1) {
                            output[output_mm_calibr_rows, data_mm_index[[s]]]
             } else { diag( output[output_mm_calibr_rows, data_mm_index[[s]]] ) }
         }
         logL1s      <- flogL(output_calibr,data_value[[s]],data_sd[[s]])
         logL1s_mm   <- 0
         if(dim(database_mm[[s]])[1]>0) {
           logL1s_mm <- flogL_mm(output_mm_calibr,data_mm_value[[s]],data_mm_min[[s]],data_mm_max[[s]])
         }
         logL1       <- logL1 + logL1s + logL1s_mm
       }
     # Check whether the candidate parameter vector is accepted, and extend the parameter chain.
       logalpha          <- logPrior1 + logL1 - (logPrior0 + logL0)
       if (log(runif(1,0,1)) < logalpha) {
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
     }