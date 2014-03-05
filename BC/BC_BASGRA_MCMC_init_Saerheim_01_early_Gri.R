## SETTINGS FOR THE MEASUREMENT-UNCERTAINTIES
     cv_default <- 0.5
     cv_DM      <- 0.05
     cv_LAI     <- 0.1
     cv_TILTOT  <- 0.2
     sd_LT50    <- 5
     min_FRTILG <- 0.3
     max_FRTILG <- 0.9

## MEASUREMENTS ##
   data_all   <- read.table('BC/data_calibration_Saerheim_01_early_Gri.txt',header=F,sep="")
# Split the data into those for which the likelihood will depend on data +- uncertainty
# and those for which the likelihood will depend on [data_min,data_max]
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
 # The outputNameList, which is defined in the generic initialisation file
 # "initialise_BASGRA_general.R", lists the names of all model outputs.
 # Note that the names used in the files with calibration data must be the
 # same as the names in the outputNameList.
 # The data_index gives the model output number for each data point:
   data_index <- sapply( 1:length(data_name),
                         function(i) which(as.character(outputNameList)==data_name[i]) )
   if(dim(data_mm)[1]>0) {
     data_mm_index <- sapply( 1:length(data_mm_name),
                         function(i) which(as.character(outputNameList)==data_mm_name[i]) )
   }
## LIKELIHOOD ##
   source('BC/fLogL_Sivia.R')   # This defines the function flogL used below
   if(dim(data_mm)[1]>0) {
     source('BC/fLogL_mm_Beta.R') # This defines the function flogL_mm used below
   } 
## PRIOR DISTRIBUTION FOR THE PARAMETERS ##
   dataframe_parameters_BC <- read.table('BC/parameters_BC.txt',header=F,sep="")
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
   nChain     <- as.integer(1000)
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
   doy_simstart          <- output[1,3]
   output_calibr_rows    <- data_time - doy_simstart + 1
   output_calibr         <- if(length(data_value)==1) {
                    output[output_calibr_rows, data_index]
     } else { diag( output[output_calibr_rows, data_index] ) }
   if(dim(data_mm)[1]>0) {
     output_mm_calibr_rows <- data_mm_time - doy_simstart + 1
     output_mm_calibr      <- if(length(data_mm_value)==1) {
                      output[output_mm_calibr_rows, data_mm_index]
       } else { diag( output[output_mm_calibr_rows, data_mm_index] ) }
   }
   
   # Value of the prior at the start point of the chain
   pBetaValues        <- (pValues[1:np]-scparmin[1:np]) / (scparmax[1:np]-scparmin[1:np])
   logPrior0Beta      <- sum( dbeta(pBetaValues,aa,bb,log=T) )
   logPrior0          <- logPrior0Beta
 # Value of the likelihood at the start point of the chain
   logL0_             <- flogL(output_calibr,data_value,data_sd)
   logL0_mm           <- 0
   if(dim(data_mm)[1]>0) {
     logL0_mm           <- flogL_mm(output_mm_calibr,data_mm_value,data_mm_min,data_mm_max)
   }
   logL0              <- logL0_ + logL0_mm
 # The first value of the Maximum Likelihood and MAP parameter vectors
   parMaxL            <- pValues
   logMaxL            <- logL0
   parMAP             <- pValues
   logMAP             <- logPrior0 + logL0

