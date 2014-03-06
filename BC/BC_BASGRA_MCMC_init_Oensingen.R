## MEASUREMENTS ##
   datao       <- read.table('BC/data_calibration_Oensingen_2005_2007_noN.txt',colClasses=c("character","character","numeric","numeric"),header=F,sep="")
   data <- datao[which(datao[,3]>0.0),]
   data_end <- length(data[,1])
   data_name  <- data[1:data_end,1]
   ll <- length(data[,1])
   data_time  <- c(strptime(data[1:data_end,2], "%d/%m/%Y")$yday+1) + (c(strptime(data[1:data_end,2], "%d/%m/%Y")$year)-105)*365 
   data_value <- data[1:data_end,3]
   data_sd    <- data[1:data_end,4]
   data_mm    <- 0

## LINKING DATA TO MODEL OUTPUTS
 # The data_index gives the model output number for each data point
   data_index     <- sapply( 1:length(data_name),
             function(i) which(as.character(outputNameList)==data_name[i]) )

## LIKELIHOOD ##
   source('BC/fLogL_Sivia.R')
   
## PRIOR DISTRIBUTION FOR THE PARAMETERS ##
   dataframe_parameters_BC <- read.table('BC/parameters_BC_ryenew.txt',header=F,sep="")
   parmin                  <- dataframe_parameters_BC[,2]
   parmod                  <- dataframe_parameters_BC[,3]
   parmax                  <- dataframe_parameters_BC[,4]
   np                      <- length(parmod)
 # We scale all parameters by dividing with the absolute value of their mode
   scparmin <- parmin/abs(parmod)
   scparmax <- parmax/abs(parmod)
   scparmod <- parmod/abs(parmod)
 # We use the beta distribution with parameters aa and bb estimated as follows
   aa       <- 1. + 4 * ((scparmod[1:np]-scparmin[1:np])/(scparmax[1:np]-scparmin[1:np]))
   bb       <- 6. - aa 

## INITIALISING THE CHAIN ##
   nChain     <- as.integer(100000)
   nBI        <- as.integer(nChain/10)
   pChain     <- matrix(0, nrow=nChain, ncol=np)
 # We start the chain at the mode of the prior parameter distribution
   pValues    <- scparmod
   pChain[1,] <- pValues

## PROPOSAL DISTRIBUTION ##
 # Load library MASS, which has a routine for multivariate normal random number generation
   library(MASS)
   vcovProp    <- matrix(0,np,np)
   stddev_beta <- sqrt((aa*bb)/((1+aa+bb)*(aa+bb)**2.))
   stddev_beta <- stddev_beta*(scparmax[1:np]-scparmin[1:np])
   fPropGelman <- 2.38^2/np # Proposal scaling factor suggested by Gelman et al. (1996)
   fPropTuning <- 0.2       # Proposal tuning factor needed because Gelman suggestion seems too big
   vcovProp    <- diag(stddev_beta^2) * fPropGelman * fPropTuning

## FIRST RUN OF THE MODEL ##
   Sim_pValues        <- pValues*abs(parmod)
   .Fortran('set_params_BC', Sim_pValues)
 # run model and put y into output
   output             <- .Fortran('BASGRA', NDAYS,NOUT,y)[[3]]
 # Model output at the dates of measurement 
   doy_simstart       <- output[1,3]
   output_calibr_rows <- data_time - doy_simstart + 1
   output_calibr      <- diag( output[output_calibr_rows, data_index] )

 # Value of the prior at the start point of the chain
   pBetaValues        <- (pValues[1:np]-scparmin[1:np]) / (scparmax[1:np]-scparmin[1:np])
   logPrior0Beta      <- sum( dbeta(pBetaValues,aa,bb,log=T) )
   logPrior0          <- logPrior0Beta
 # Value of the likelihood at the start point of the chain
   logL0              <- flogL(output_calibr,data_value,data_sd)
 # The first value of the Maximum Likelihood and MAP parameter vectors
   parMaxL            <- pValues
   logMaxL            <- logL0
   parMAP             <- pValues
   logMAP             <- logPrior0 + logL0

