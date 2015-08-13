## BC_BASGRA_MCMC_init_MULTISITE_Gri.R ##

## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c('initialise_BASGRA_Saerheim_00_early_Gri.R',
                               'initialise_BASGRA_Saerheim_00_late_Gri.R',
                               'initialise_BASGRA_Saerheim_01_early_Gri.R',
                               'initialise_BASGRA_Saerheim_01_late_Gri.R',
                               'initialise_BASGRA_Saerheim_02_early_Gri.R',
                               'initialise_BASGRA_Saerheim_02_late_Gri.R')
   sitedata_filenames     <- c('BC/data_calibration_Saerheim_00_early_Gri.txt',
                               'BC/data_calibration_Saerheim_00_late_Gri.txt',
                               'BC/data_calibration_Saerheim_01_early_Gri.txt',
                               'BC/data_calibration_Saerheim_01_late_Gri.txt',
                               'BC/data_calibration_Saerheim_02_early_Gri.txt',
                               'BC/data_calibration_Saerheim_02_late_Gri.txt')
   nSites                 <- length(sitedata_filenames)
     cv_default <- rep(0.5 ,nSites)
     cv_DM      <- rep(0.05,nSites)
     cv_LAI     <- rep(0.1 ,nSites)
     cv_TILTOT  <- rep(0.2 ,nSites)
     sd_LT50    <- rep(5   ,nSites)
     min_FRTILG <- rep(0.3 ,nSites)
     max_FRTILG <- rep(0.9 ,nSites)

## MEASUREMENTS ##
   database <- list() ; database_mm <- list()
   s <- 0 ; while (s < nSites) {
     s <- s + 1
     nextdataset_all <- read.table(sitedata_filenames[s],header=F,sep="")
       nextdataset     <- nextdataset_all[which(nextdataset_all$V1!='FRTILG'),]
       nextdataset_mm  <- nextdataset_all[which(nextdataset_all$V1=='FRTILG'),]
     database    <- c(database   ,list(nextdataset   ))
     database_mm <- c(database_mm,list(nextdataset_mm))
   }

   data_name    <- list() ; data_time    <- list() ; data_value    <- list()
   data_mm_name <- list() ; data_mm_time <- list() ; data_mm_value <- list()
   data_sd      <- list() ; data_mm_min  <- list() ; data_mm_max   <- list()
   s <- 0 ; while (s < nSites) {
     s <- s + 1
     
     data_name [[s]] <- database[[s]][,1]
     data_time [[s]] <- database[[s]][,2]
     data_value[[s]] <- database[[s]][,3]
     data_sd[[s]]    <- abs(database[[s]][,3])                         * cv_default[s]
     data_sd[[s]][which(data_name[[s]]=='DM')]     <-
                 abs(data_value[[s]][which(data_name[[s]]=='DM')])     * cv_DM[s]
     data_sd[[s]][which(data_name[[s]]=='LAI')]    <-
                 abs(data_value[[s]][which(data_name[[s]]=='LAI')])    * cv_LAI[s]
     data_sd[[s]][which(data_name[[s]]=='TILTOT')] <-
                 abs(data_value[[s]][which(data_name[[s]]=='TILTOT')]) * cv_TILTOT[s]
     data_sd[[s]][which(data_name[[s]]=='LT50')]   <-                    sd_LT50[s]
     
     data_mm_name [[s]] <- database_mm[[s]][,1]
     data_mm_time [[s]] <- database_mm[[s]][,2]
     data_mm_value[[s]] <- database_mm[[s]][,3]
     data_mm_min[[s]]   <- rep( 0, length(data_mm_name[[s]]) )
     data_mm_max[[s]]   <- rep( 1, length(data_mm_name[[s]]) )
     data_mm_min[[s]][which(data_mm_name[[s]]=='FRTILG')] <- min_FRTILG[s]
     data_mm_max[[s]][which(data_mm_name[[s]]=='FRTILG')] <- max_FRTILG[s]
   }

## LINKING DATA TO MODEL OUTPUTS
 # The outputNameList, which is defined in the generic initialisation file
 # "initialise_BASGRA_general.R", lists the names of all model outputs.
 # Note that the names used in the files with calibration data must be the
 # same as the names in the outputNameList.
 # The data_index gives the model output number for each data point:
   data_index <- list() ; data_mm_index <- list()
   s <- 0 ; while (s < nSites) {
     s <- s + 1
     nextdata_index     <- sapply( 1:length(data_name[[s]])   , function(i)
                                   which(as.character(outputNameList)==data_name[[s]][i]) )
     nextdata_mm_index  <- sapply( 1:length(data_mm_name[[s]]), function(i)
                                   which(as.character(outputNameList)==data_mm_name[[s]][i]) )
     data_index[[s]]    <- nextdata_index
     data_mm_index[[s]] <- nextdata_mm_index
   }

## LIKELIHOOD ##
   source('BC/fLogL_Sivia.R')
   source('BC/fLogL_mm_Beta.R')
   
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
   fPropTuning <- 0.01        # Proposal tuning factor needed because Gelman suggestion seems too big
   vcovProp    <- diag(stddev_beta^2) * fPropGelman * fPropTuning

## PRIOR AT THE START OF THE CHAIN ##
   Sim_pValues        <- pValues*abs(parmod)
   .Fortran('set_params_BC', Sim_pValues)
   pBetaValues        <- (pValues[1:np]-scparmin[1:np]) / (scparmax[1:np]-scparmin[1:np])
   logPrior0Beta      <- sum( dbeta(pBetaValues,aa,bb,log=T) )
   logPrior0          <- logPrior0Beta
   
## FIRST RUN OF THE MODEL FOR EACH SITE, WITH CALCULATION OF LIKELIHOOD ##
   logL0 <- 0 ; for (s in 1:nSites) {
     source(sitesettings_filenames[s])
     .Fortran('set_params_BC', Sim_pValues)
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
     logL0s       <- flogL(output_calibr,data_value[[s]],data_sd[[s]])
     logL0s_mm    <- 0
     if(dim(database_mm[[s]])[1]>0) {
       logL0s_mm  <- flogL_mm(output_mm_calibr,data_mm_value[[s]],data_mm_min[[s]],data_mm_max[[s]])
     }
     logL0        <- logL0 + logL0s + logL0s_mm
   }
   
 # The first value of the Maximum Likelihood and MAP parameter vectors
   parMaxL            <- pValues
   logMaxL            <- logL0
   parMAP             <- pValues
   logMAP             <- logPrior0 + logL0
