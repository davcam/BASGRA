## SITE CONDITIONS
   list_params       <- sitelist ; list_matrix_weather <- sitelist
   list_days_harvest <- sitelist ; list_NDAYS          <- sitelist
   for (s in 1:nSites) {
     source( sitesettings_filenames[s] )
     list_params      [[s]] <- params       ; list_matrix_weather[[s]] <- matrix_weather
     list_days_harvest[[s]] <- days_harvest ; list_NDAYS         [[s]] <- NDAYS   
   } 
   
## MEASUREMENTS ##
   database <- sitelist      ; database_mm <- sitelist
   ndata    <- rep(0,nSites) ; ndata_mm    <- rep(0,nSites)
   for (s in 1:nSites) {
     dataset_all      <- read.table(sitedata_filenames[s],header=F,sep="")
     database   [[s]] <- dataset_all[which(dataset_all$V1!='FRTILG'),]
     database_mm[[s]] <- dataset_all[which(dataset_all$V1=='FRTILG'),]
     ndata[s]         <- dim(database   [[s]])[[1]]
     ndata_mm[s]      <- dim(database_mm[[s]])[[1]]
   }

   data_name  <- sitelist ; data_mm_name  <- sitelist
   data_value <- sitelist ; data_mm_value <- sitelist
   data_sd    <- sitelist ; data_mm_min   <- sitelist ; data_mm_max <- sitelist ; data_mm_cv <- sitelist
   data_year  <- sitelist ; data_mm_year  <- sitelist 
   data_doy   <- sitelist ; data_mm_doy   <- sitelist
   
   for (s in 1:nSites) {
     data_name [[s]] <-     database[[s]][,1]
     data_year [[s]] <-     database[[s]][,2]
     data_doy  [[s]] <-     database[[s]][,3]
     data_value[[s]] <-     database[[s]][,4]
     data_sd   [[s]]        <- abs(database[[s]][,4])         * cv_default[s]
	 i_DM                   <- which(data_name[[s]]=='DM'    )
	 i_LAI                  <- which(data_name[[s]]=='LAI'   )
	 i_TILTOT               <- which(data_name[[s]]=='TILTOT')
	 i_YIELD                <- which(data_name[[s]]=='YIELD' )
	 i_LT50                 <- which(data_name[[s]]=='LT50'  )
     data_sd[[s]][i_DM]     <- max( abs(data_value[[s]][i_DM])     * cv_DM[s]    , sd_DM_min[s]     )
     data_sd[[s]][i_LAI]    <- max( abs(data_value[[s]][i_LAI])    * cv_LAI[s]   , sd_LAI_min[s]    )
     data_sd[[s]][i_TILTOT] <- max( abs(data_value[[s]][i_TILTOT]) * cv_TILTOT[s], sd_TILTOT_min[s] )
  	 data_sd[[s]][i_YIELD]  <- max( abs(data_value[[s]][i_YIELD])  * cv_YIELD[s] , sd_YIELD_min[s]  )
     data_sd[[s]][i_LT50]   <-                                                     sd_LT50[s]
  
     data_mm_name [[s]] <- database_mm[[s]][,1]
     data_mm_year [[s]] <- database_mm[[s]][,2]
     data_mm_doy  [[s]] <- database_mm[[s]][,3]
     data_mm_value[[s]] <- database_mm[[s]][,4]
     data_mm_min  [[s]] <- rep( 0, ndata_mm[s] )
     data_mm_max  [[s]] <- rep( 1, ndata_mm[s] )
     data_mm_cv   [[s]] <- cv_mm_default[s]
	 i_FRTILG           <- which(data_mm_name[[s]]=='FRTILG')
     data_mm_cv   [[s]][i_FRTILG] <- cv_mm_FRTILG[s]
   }

## LINKING DATA TO MODEL OUTPUTS
 # The list of outputNames, which is defined in the generic initialisation file
 # "initialise_BASGRA_general.R", lists the names of all model outputs.
 # Note that the names used in the files with calibration data must be the
 # same as the names in the list of outputNames.
 # The data_index gives the model output number for each data point:
   data_index <- sitelist ; data_mm_index <- sitelist
   for (s in 1:nSites) {
     nextdata_index     <- sapply( 1:ndata   [s], function(i)
                                   which(as.character(outputNames)==data_name[[s]][i]) )
     nextdata_mm_index  <- sapply( 1:ndata_mm[s], function(i)
                                   which(as.character(outputNames)==data_mm_name[[s]][i]) )
     data_index   [[s]] <- nextdata_index
     data_mm_index[[s]] <- nextdata_mm_index
   }

## PRIOR DISTRIBUTION FOR THE PARAMETERS ##
   df_params_BC <- read.table( file_prior, header=F, sep="" )
   parname_BC   <-               df_params_BC[,1]
   parmin_BC    <-               df_params_BC[,2]
   parmod_BC    <-               df_params_BC[,3]
   parmax_BC    <-               df_params_BC[,4]
   parsites_BC  <- as.character( df_params_BC[,5] )
   ip_BC        <- match( parname_BC, row.names(df_params) )
   np_BC        <- length(ip_BC)
   
   ip_BC_site   <- sitelist ; icol_pChain_site <- sitelist
   for (p in 1:np_BC) {
     for ( s in 1:nSites ) {
       if( s %in% eval( parse( text = parsites_BC[p] ) ) ) {
         ip_BC_site[[s]]       <- cbind( ip_BC_site[[s]]      , ip_BC[p] )
         icol_pChain_site[[s]] <- cbind( icol_pChain_site[[s]], p        )
       }
     }
   }
 # We scale all parameters by dividing by the mean of the absolute extremes
   sc            <- rowMeans( abs( cbind(parmin_BC,parmax_BC) ) )
   scparmin_BC   <- parmin_BC / sc
   scparmax_BC   <- parmax_BC / sc
   scparmod_BC   <- parmod_BC / sc
 # We use the beta distribution with parameters aa and bb estimated as follows
   aa            <- 1. + 4 * ((scparmod_BC[1:np_BC]-scparmin_BC[1:np_BC]) / 
                              (scparmax_BC[1:np_BC]-scparmin_BC[1:np_BC]))
   bb            <- 6. - aa 

## INITIALISING THE CHAIN ##
   nBurnin       <- as.integer(nChain/10)
   pChain        <- matrix( 0, nrow=nChain, ncol=np_BC )
 # We start the chain at the mode of the prior parameter distribution
   scpValues_BC  <- scparmod_BC ; pChain[1,] <- scpValues_BC
 # Value of the prior at the start of the chain
   pBetaValues   <- (scpValues_BC[1:np_BC] - scparmin_BC[1:np_BC]) / 
                    (scparmax_BC [1:np_BC] - scparmin_BC[1:np_BC])
   logPrior0Beta <- sum( dbeta(pBetaValues,aa,bb,log=T) )
   logPrior0     <- logPrior0Beta

## PROPOSAL DISTRIBUTION ##
 # Load library MASS, which has a routine for multivariate normal random number generation
   library(MASS)
   vcovProp      <- matrix( 0, np_BC, np_BC )
   stddev_beta   <- sqrt((aa*bb)/((1+aa+bb)*(aa+bb)**2.))
   stddev_beta   <- stddev_beta * (scparmax_BC[1:np_BC]-scparmin_BC[1:np_BC])
   fPropGelman   <- 2.38^2 / np_BC # Proposal scaling factor suggested by Gelman et al. (1996)
   vcovProp      <- diag(stddev_beta^2) * fPropGelman * fPropTuning

## FIRST RUN OF THE MODEL FOR EACH SITE, WITH CALCULATION OF LIKELIHOOD ##
   list_output             <- sitelist
   list_output_calibr_rows <- sitelist ; list_output_mm_calibr_rows <- sitelist   
   pValues_BC              <- scpValues_BC * sc
   
   for (s in 1:nSites) {
   # Site-specific model initialisation
     params       <- list_params      [[s]] ; matrix_weather <- list_matrix_weather[[s]]
     days_harvest <- list_days_harvest[[s]] ; NDAYS          <- list_NDAYS         [[s]]
   # Values of calibration parameters at the start of the chain
     params[ ip_BC_site[[s]] ]    <- pValues_BC[ icol_pChain_site[[s]] ]
     output                       <- run_model(params,matrix_weather,days_harvest,NDAYS)
	   list_output[[s]]             <- output
     list_output_calibr_rows[[s]] <- sapply (1:ndata[s], function(i) 
         which( output[,2]==data_year[[s]][i] & output[,3]==data_doy[[s]][i] )
       )
     if(dim(database_mm[[s]])[1]>0) {
       list_output_mm_calibr_rows[[s]] <- sapply (1:ndata_mm[s], function(i) 
         which( output[,2]==data_mm_year[[s]][i] & output[,3]==data_mm_doy[[s]][i] ) )
     }
   }
   
   calc_logL_s <- function( s=1, output=output ) {
     output_calibr <- if(ndata[s]==1) {
                      output[list_output_calibr_rows[[s]], data_index[[s]]]
       } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
     if(dim(database_mm[[s]])[1]>0) {
         output_mm_calibr <- if(ndata_mm[s]==1) {
                          output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
           } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
     }
     logLs      <- flogL(output_calibr,data_value[[s]],data_sd[[s]])
     logLs_mm   <- 0
     if(dim(database_mm[[s]])[1]>0) {
       logLs_mm <- flogL_mm( output_mm_calibr, data_mm_value[[s]],
                             data_mm_min[[s]], data_mm_max[[s]], data_mm_cv[[s]] )
     }
     logL_s <- logLs + logLs_mm
	 return( logL_s )
   }

   calc_sum_logL <- function( list_output = list_output ) {
     sum_logL <- 0 ; for (s in 1:nSites) {
       sum_logL <- sum_logL + calc_logL_s( s, list_output[[s]] )
     }
   return( sum_logL )
   }
   
   logL0 <- calc_sum_logL( list_output )

## The first values for MaxL and MAP parameter vectors
   scparMaxL_BC <- scpValues_BC
   logMaxL      <- logL0
   scparMAP_BC  <- scpValues_BC
   logMAP       <- logPrior0 + logL0
   
################################################################################
## FUNCTION FOR LIKELIHOOD CALCULATION for each variable separately
   
  calc_statsi_s <- function( s=1, output=output ) {
     output_calibr <- if(ndata[s]==1) {
                      output[list_output_calibr_rows[[s]], data_index[[s]]]
       } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
     if(dim(database_mm[[s]])[1]>0) {
         output_mm_calibr <- if(ndata_mm[s]==1) {
                          output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
           } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
     }
     datais  <- cbind(data_index[[s]],data_year[[s]],data_doy[[s]],data_value[[s]])
	 outis   <- output_calibr
	 SSEis   <- sapply( 1:ndata[s], function(i) { (output_calibr[i]-data_value[[s]][i])^2 } )
     logLis  <- sapply( 1:ndata[s], function(i) {
       flogL(output_calibr[i],data_value[[s]][i],data_sd[[s]][i]) } )
	 datai_s <- datais
     outi_s  <- matrix( c(data_index[[s]],outis ), ncol=2 )
     SSEi_s  <- matrix( c(data_index[[s]],SSEis ), ncol=2 )
     logLi_s <- matrix( c(data_index[[s]],logLis), ncol=2 )
     if(dim(database_mm[[s]])[1]>0) {
       datais_mm <- cbind(data_mm_index[[s]],data_mm_year[[s]],data_mm_doy[[s]],data_mm_value[[s]])
	   outis_mm  <- output_mm_calibr
       SSEis_mm  <- sapply( 1:ndata_mm[s], function(i) { (output_mm_calibr[i]-data_mm_value[[s]][i])^2 } )
       logLis_mm <- sapply( 1:ndata_mm[s], function(i) {
         flogL_mm(output_mm_calibr[i],data_mm_value[[s]][i],
                  data_mm_min[[s]][i],data_mm_max[[s]][i],data_mm_cv[[s]][i]) } )
       datai_s   <- rbind( datais ,  datais_mm )
       outi_s    <- rbind( outi_s ,
                           matrix( c(data_mm_index[[s]],outis_mm ), ncol=2 ) )
       SSEi_s    <- rbind( SSEi_s ,
                           matrix( c(data_mm_index[[s]],SSEis_mm ), ncol=2 ) )
       logLi_s   <- rbind( logLi_s,
                           matrix( c(data_mm_index[[s]],logLis_mm), ncol=2 ) )
     }
     list_statsi_s <- list( datai_s=datai_s, outi_s=outi_s, SSEi_s=SSEi_s, logLi_s=logLi_s )
     return( list_statsi_s )
   }

#   calc_logLi_s <- function( s=1, output=output ) {
#      output_calibr <- if(ndata[s]==1) {
#                       output[list_output_calibr_rows[[s]], data_index[[s]]]
#        } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
#      if(dim(database_mm[[s]])[1]>0) {
#          output_mm_calibr <- if(ndata_mm[s]==1) {
#                           output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
#            } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
#      }
#      logLis      <- sapply( 1:ndata[s], function(i) {
#        flogL(output_calibr[i],data_value[[s]][i],data_sd[[s]][i]) } )
#      logLi_s     <- matrix( c(data_index[[s]],logLis), ncol=2 )
#      if(dim(database_mm[[s]])[1]>0) {
#        logLis_mm <- sapply( 1:ndata_mm[s], function(i) {
#          flogL_mm(output_mm_calibr[i],data_mm_value[[s]][i],
#                   data_mm_min[[s]][i],data_mm_max[[s]][i],data_mm_cv[[s]][i]) } )
#        logLi_s   <- rbind( logLi_s,
#                            matrix( c(data_mm_index[[s]],logLis_mm), ncol=2 ) )
#      }
#    return( logLi_s )
#    }
# 
# # Note: the following function definition is preliminary.
# # It should be replaced with simpler code and/or merged with the
# # calc_logLi_s function (but first check in which places that is used).
#   calc_datai_s <- function( s=1 ) {
#      datais    <- sapply( 1:ndata[s], function(i) {
#        (data_value[[s]][i]) } )
#      datai_s   <- matrix( c(data_index[[s]],datais), ncol=2 )
#      if(dim(database_mm[[s]])[1]>0) {
#        datais_mm <- sapply( 1:ndata_mm[s], function(i) {
#          (data_mm_value[[s]][i]) } )
#        datai_s   <- rbind( datai_s,
#                             matrix( c(data_mm_index[[s]],datais_mm), ncol=2 ) )
#      }
#    return( datai_s )
#    }
# 
# # Note: the following function definition is preliminary.
# # It should be replaced with simpler code and/or merged with the
# # calc_logLi_s function (but first check in which places that is used).
#   calc_SSEi_s <- function( s=1, output=output ) {
#      output_calibr <- if(ndata[s]==1) {
#                       output[list_output_calibr_rows[[s]], data_index[[s]]]
#        } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
#      if(dim(database_mm[[s]])[1]>0) {
#          output_mm_calibr <- if(ndata_mm[s]==1) {
#                           output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
#            } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
#      }
#      SSEis      <- sapply( 1:ndata[s], function(i) {
#        (output_calibr[i]-data_value[[s]][i])^2 } )
#      SSEi_s     <- matrix( c(data_index[[s]],SSEis), ncol=2 )
#      if(dim(database_mm[[s]])[1]>0) {
#        SSEis_mm <- sapply( 1:ndata_mm[s], function(i) {
#          (output_mm_calibr[i]-data_mm_value[[s]][i])^2 } )
#        SSEi_s   <- rbind( SSEi_s,
#                           matrix( c(data_mm_index[[s]],SSEis_mm), ncol=2 ) )
#      }
#    return( SSEi_s )
#    }

################################################################################
## FUNCTIONS FOR PLOTTING
   
  plot_outputs_data_s <- function(
    isite        = 1,
    list_runs    = list_runs,
    nruns        = length(list_runs),
  	leg_title    = "LEGEND",
	  leg          = as.character(1:nruns),
    cols         = 1:nruns,
    lwds	       = rep(3,nruns),
	  ltys         = rep(1,nruns) ) {
   
  s <- isite  
    
  outputsMeasured      <- unique(data_index[[s]])   
  if(dim(database_mm[[s]])[1]>0) {
    outputsMeasured_mm <- unique(data_mm_index[[s]])
    noutputsMeasured   <- length(outputsMeasured) + length(outputsMeasured_mm)
  } else {
    noutputsMeasured   <- length(outputsMeasured)
  }
  nrowsPlots           <- ceiling(sqrt(noutputsMeasured+1))
  ncolsPlots           <- ceiling((noutputsMeasured+1)/nrowsPlots)
  par(mfrow=c(nrowsPlots,ncolsPlots),omi=c(0,0,0.5,0), mar=c(2, 2, 2, 1) )

  for (p in outputsMeasured) {
    datap     <- which( data_name[[s]] == as.character(outputNames[p]) )
    lcl       <- data_value[[s]][datap] - data_sd[[s]][datap]
    ucl       <- data_value[[s]][datap] + data_sd[[s]][datap]	
  	g_range_p <- range( sapply( 1:nruns, function(i){range(list_runs[[i]][,p])} ) )
    g_range   <- range( g_range_p, lcl, ucl )	
    plot( list_runs[[1]][,1], list_runs[[1]][,p], type='l',
          xlab="", ylab="", ylim=g_range, cex.main=1,
		      main=paste(outputNames[p]," ",outputUnits[p],sep=""),
          col=cols[1], lwd=lwds[1], lty=ltys[1] )
    if (nruns>=2) {
	  for (i in 2:nruns) {
	    points( list_runs[[i]][,1], list_runs[[i]][,p], type='l',
	            col=cols[i], lwd=lwds[i], lty=ltys[i] )
	  }
	}
    points( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, data_value[[s]][datap],
            col='blue', lwd=1, cex=1 )
    arrows( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, ucl,
            data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, lcl,
            col='blue', lwd=1, angle=90, code=3, length=0.05 )
  }
   
  if(dim(database_mm[[s]])[1]>0) {
    for (p in outputsMeasured_mm) {
      datap     <- which( data_mm_name[[s]] == as.character(outputNames[p]) )
      ucl       <- data_mm_max[[s]][datap]
      lcl       <- data_mm_min[[s]][datap]
      g_range_p <- range( sapply( 1:nruns, function(i){range(list_runs[[i]][,p])} ) )
      g_range   <- range( g_range_p, lcl, ucl )	
      plot( list_runs[[1]][,1], list_runs[[1]][,p], type='l',
            xlab="", ylab="", ylim=g_range, cex.main=1,
		        main=paste(outputNames[p]," ",outputUnits[p],sep=""),
            col=cols[1], lwd=lwds[1], lty=ltys[1] )
    if (nruns>=2) {
	  for (i in 2:nruns) {
	    points( list_runs[[i]][,1], list_runs[[i]][,p], type='l',
	            col=cols[i], lwd=lwds[i], lty=ltys[i] )
	  }
	}

      points( data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, data_mm_value[[s]][datap],
              col='blue', lwd=1, cex=1 )
      arrows( data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, ucl,
              data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, lcl,
              col='blue', lwd=1, angle=90, code=3, length=0.05 )
    }
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title = leg_title, legend=leg,
          col=cols, lty=ltys, lwd=lwds )
  sitenames <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
  mtext( paste("SITE ",s," (",sitenames[s],")",sep=""),
         side=3, line=1, outer=TRUE, cex=1, font=2)   
  }
   