### initialise_BASGRA_general.R ##

################################################################################
days_harvest <- matrix( as.integer(-1), nrow=100, ncol=2 )

################################################################################
### 1. MODEL LIBRARY FILE & FUNCTION FOR RUNNING THE MODEL
run_model <- function(p = params,
                      w = matrix_weather,
                      h = days_harvest,
                      n = NDAYS) {
  .Fortran('BASGRA', p,w,h,n, NOUT,matrix(0,n,NOUT))[[6]]
}

################################################################################
### 2. FUNCTIONS FOR READING WEATHER DATA
read_weather_Bioforsk <- function(y = year_start,
                                  d = doy_start,
                                  n = NDAYS,
                                  f = file_weather) {
  df_weather            <- read.table( f, header=TRUE )
  row_start             <- 1
  while( df_weather[row_start,]$YR  < y ) { row_start <- row_start+1 }
  while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
  df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
  NMAXDAYS              <- as.integer(10000)
  NWEATHER              <- as.integer(8)
  matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
  matrix_weather[1:n,1] <- df_weather_sim$YR
  matrix_weather[1:n,2] <- df_weather_sim$doy
  matrix_weather[1:n,3] <- df_weather_sim$GR
  matrix_weather[1:n,4] <- df_weather_sim$T
  matrix_weather[1:n,5] <- df_weather_sim$T
  matrix_weather[1:n,6] <- exp(17.27*df_weather_sim$T/(df_weather_sim$T+239)) *
                               0.6108 * df_weather_sim$RH / 100
  matrix_weather[1:n,7] <- df_weather_sim$RAINI
  matrix_weather[1:n,8] <- df_weather_sim$WNI   
  return(matrix_weather)
}
   
read_weather_WG <- function(y = year_start,
                            d = doy_start,
                            n = NDAYS,
                            f = file_weather) {
  df_weather            <- read.table( f, header=TRUE, sep="\t" )
  colnames(df_weather)  <- c("ST","YR","doy","TMMNI","TMMXI","RAINI","GR","PETI")
  row_start             <- 1
  while( df_weather[row_start,]$YR  < y ) { row_start <- row_start+1 }
  while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
  df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
  NMAXDAYS              <- as.integer(10000)
  NWEATHER              <- as.integer(7)
  matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
  matrix_weather[1:n,1] <- df_weather_sim$YR
  matrix_weather[1:n,2] <- df_weather_sim$doy
  matrix_weather[1:n,3] <- df_weather_sim$GR
  matrix_weather[1:n,4] <- df_weather_sim$TMMNI
  matrix_weather[1:n,5] <- df_weather_sim$TMMXI
  matrix_weather[1:n,6] <- df_weather_sim$RAINI
  matrix_weather[1:n,7] <- df_weather_sim$PETI
  return(matrix_weather)
}
   
################################################################################
### 3. OUTPUT VARIABLES
outputNames <- c(
  "Time"      , "year"     , "doy"      , "DAVTMP"    , "CLV"      , "CLVD"     ,
  "YIELD"     , "CRES"     , "CRT"      , "CST"       , "CSTUB"    , "DRYSTOR"  ,
  "Fdepth"    , "LAI"      , "LT50"     , "O2"        , "PHEN"     , "ROOTD"    ,
  "Sdepth"    , "TANAER"   , "TILG"     , "TILV"      , "WAL"      , "WAPL"     ,
  "WAPS"      , "WAS"      , "WETSTOR"  , "DM"        , "RES"      , "LERG"     , 
  "NELLVG"    , "RLEAF"    , "SLA"      , "TILTOT"    , "FRTILG"   , "FRTILG1"  ,
  "FRTILG2"   , "RDRT"     , "VERN"                                              )
outputUnits <- c(
  "(y)"       , "(y)"      , "(d)"      , "(degC)"    , "(g C m-2)", "(g C m-2)",
  "(g DM m-2)", "(g C m-2)", "(g C m-2)", "(g C m-2)" , "(g C m-2)", "(mm)"     ,
  "(m)"       , "(m2 m-2)" , "(degC)"   , "(mol m-2)" , "(-)"      , "(m)"      ,
  "(m)"       , "(d)"      , "(m-2)"    , "(m-2)"     , "(mm)"     , "(mm)"     ,
  "(mm)"      , "(mm)"     , "(mm)"     , "(g DM m-2)", "(g g-1)"  , "(m d-1)"  ,
  "(tiller-1)", "(d-1)"    , "(m2 g-1)" , "(m-2)"     , "(-)"      , "(-)"      ,
  "(-)"       , "(d-1)"    , "(-)"                                               )
NOUT <- as.integer( length(outputNames) )
   
################################################################################
### 4. FUNCTIONS FOR EXPORTING THE RESULTS TO FILE (pdf with plots, txt with table)

plot_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:3)],
  leg         = paste( "Run", 1:length(list_output) ),
  leg_title   = "LEGEND",
  nrow_plot   = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot   = ceiling( (length(vars)+1)/nrow_plot ),
  lty         = rep(1,length(list_output)),
  lwd         = rep(3,length(list_output))
) {
  par( mfrow=c(nrow_plot,ncol_plot), mar=c(2, 2, 2, 1) )
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  for (iv in 1:nvars) {
    c       <- col_vars[iv]
    g_range <- range( sapply( 1:nlist, function(il){range(list_output[[il]][,c])} ) )
    plot( list_output[[1]][,1], list_output[[1]][,c],
          xlab="", ylab="", cex.main=1,
          main=paste(outputNames[c]," ",outputUnits[c],sep=""),
          type='l', col=1, lty=lty[1], lwd=lwd[1], ylim=g_range )
    if (nlist >= 2) {
      for (il in 2:nlist) {
      points( list_output[[il]][,1], list_output[[il]][,c],
              col=il, type='l', lty=lty[il], lwd=lwd[il] )            
      }
    }
    if ( (iv%%(nrow_plot*ncol_plot-1)==0) || (iv==nvars) ) {
      plot(1,type='n', axes=FALSE, xlab="", ylab="")
      legend("bottomright", leg, lty=lty, lwd=lwd, col=1:nlist, title = leg_title)
    }
  }
}

table_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:3)],
  file_table  = paste( "output_", format(Sys.time(),"%H_%M.txt"), sep="" ),
  leg         = paste( "Run", 1:length(list_output) )
) {
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  table_output         <- rbind( c("Time (y)","Year","DOY"), list_output[[1]][,1:3] )
  for (il in 1:nlist) {
    table_il     <- if (nvars==1) c    (vars, list_output[[il]][,col_vars]) else
                                  rbind(vars, list_output[[il]][,col_vars])
    table_output <- cbind( table_output, table_il ) 
  }
  colnames(table_output) <- c( "","","",rep(leg,each=nvars) )
  write.table( table_output, file_table, sep="\t", row.names=F )
}

export_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:3)],
  file_table  = paste( "output_", format(Sys.time(),"%H_%M.txt"), sep="" ),
  leg         = paste( "Run", 1:length(list_output) ),
  leg_title   = "LEGEND",
  nrow_plot   = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot   = ceiling( (length(vars)+1)/nrow_plot ),
  lty         = rep(1,length(list_output)),
  lwd         = rep(3,length(list_output))
) {
#   pdf( file_plot, paper="a4r", width=11, height=8 )
  plot_output(  list_output=list_output, vars=vars,
                leg=leg, leg_title=leg_title,
                nrow_plot=nrow_plot, ncol_plot=ncol_plot, lty=lty, lwd=lwd )
  table_output( list_output=list_output, vars=vars, file_table=file_table, leg=leg )
#   dev.off()
}
   
################################################################################
### 5. FUNCTIONS FOR ANALYSIS

#######################
### 5.1 Function 'SA()'
#######################
SA <- function( parname_SA = "TILTOTI",
                pmult      = 2^(-1:1),
                vars       = outputNames[-(1:3)],
                leg_title  = parname_SA,
                nrow_plot  = ceiling( sqrt((length(vars)+1) * 8/11) ),
                ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
                lty        = rep(1,length(pmult)),
                lwd        = rep(3,length(pmult)),
                file_init  = "initialisation/initialise_BASGRA_Saerheim_00_early_Gri.R",
                file_plot  = paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.pdf"),sep="")
) {
  source(file_init)
  cat( "SA initialised for:", substr(basename(file_init),1,nchar(basename(file_init))-2), "\n")
  ip_SA          <- match( parname_SA, row.names(df_params) )
  par_SA_default <- params[ip_SA]
  nmult          <- length(pmult)
  list_output    <- vector( "list", nmult )
  for (im in 1:nmult) {
    params[ip_SA]     <- par_SA_default * pmult[im]
    list_output[[im]] <- run_model(params)
  }
  pdf( file_plot, paper="a4r", width=11, height=8 )
  plot_output( list_output, vars=vars,
               leg=as.character(pmult*par_SA_default), leg_title=parname_SA,
               nrow_plot=nrow_plot, ncol_plot=ncol_plot, lty=lty, lwd=lwd )
  dev.off()
  table_output(list_output, vars=vars,
               file_table=paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.txt"),sep=""),
               leg=paste(parname_SA,"=",pmult*par_SA_default,sep=""))
}

##############################
### 5.2 Function 'SA_BC()'
##############################
SA_BC <- function(
  parname_SA    = "TILTOTI",
  pmult         = 2^(-1:1),
  vars          = outputNames[-(1:3)],
  leg_title     = parname_SA,
  nrow_plot     = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot     = ceiling( (length(vars)+1)/nrow_plot ),
  lty           = rep(1,length(pmult)),
  lwd           = rep(3,length(pmult)),
  file_init_BC  = "BC/BC_BASGRA_MCMC_init_Gri.R",
  file_par      = "BASGRA_parModes.txt",
  partype       = "MAP",
  file_plot_outputs      = paste("SA_BC_outputs"     ,format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_outputs_data = paste("SA_BC_outputs_data",format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_likelihoods  = paste("SA_BC_likelihoods" ,format(Sys.time(),"_%H_%M.pdf"),sep="") )
{ source( file_init_BC )
  nmult <- length(pmult)
  cat( "SA initialised for:", substr(basename(file_init_BC),1,nchar(basename(file_init_BC))-2), "\n")
  parheaders  <- paste( partype, "_", as.character(1:nSites), sep="" )
  df_parModes <- read.table( file_par, header=TRUE, sep="\t" )
  # SITE CONDITIONS
  source('BC/BC_BASGRA_MCMC_init_general.R')
  for (s in 1:nSites) {
    params           <- as.matrix( df_parModes[ parheaders[s] ] )
    list_params[[s]] <- params
  } 
  ip_SA <- match( parname_SA, row.names(df_params) )
  
  ivar             <- unique( c( unlist(data_index), unlist(data_mm_index) ) )
  names_var        <- outputNames[ivar]
  nvar             <- length(ivar)
  df_logL          <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  names(df_logL)   <- names_var
  list_df_logL     <- vector("list",nmult)
  for(i in 1:nmult) list_df_logL[[i]] <- df_logL
  
  pdf( file_plot_outputs_data, paper="a4r", width=11, height=8 )
  dev_outputs_data <- dev.cur()    
  pdf( file_plot_outputs     , paper="a4r", width=11, height=8 )
  dev_outputs      <- dev.cur()  
  pdf( file_plot_likelihoods , paper="a4r", width=11, height=8 )
  dev_likelihoods  <- dev.cur()  
  
  for (s in 1:nSites) {
    matrix_weather <- list_matrix_weather[[s]]
    days_harvest   <- list_days_harvest  [[s]]
    NDAYS          <- list_NDAYS         [[s]]
    params         <- list_params        [[s]]
    par_SA_default <- params[ip_SA]
    list_output    <- vector( "list", nmult )
    for (g in 1:nmult) {
      params[ip_SA]    <- par_SA_default * pmult[g]
      output           <- run_model(params,matrix_weather,days_harvest,NDAYS)
      list_output[[g]] <- output        
      for(v in 1:nvar) {
#         logLi_s                   <- calc_logLi_s( s, output=output )
        logLi_s                   <- calc_statsi_s( s, output=output )$logLi_s
        rows_var_s                <- which( logLi_s[,1]==ivar[v] )
        list_df_logL[[g]][[v]][s] <- sum( logLi_s[rows_var_s,2] )
      }
    }        
    dev.set(dev_outputs_data)
    leg_title_outputs_data <- parname_SA
    leg                    <- as.character(pmult*par_SA_default)
    plot_outputs_data_s( isite        = s,
                         list_runs    = list_output,
                         leg_title    = leg_title_outputs_data,
                         leg          = leg,
                         cols         = 1:nmult,
                         lwds         = lwd,
                         ltys         = lty ) 
    dev.set(dev_outputs)
    leg_title_outputs <- paste( "Site: ", s, ", par: ", parname_SA, sep="")
    par( omi=c(0,0,0.3,0), mar=c(2,2,2,1) )
    plot_output( list_output = list_output,
                 vars        = vars,
                 leg_title   = leg_title_outputs,
                 leg         = leg,
                 nrow_plot   = nrow_plot, ncol_plot = ncol_plot,
                 lty         = lty      , lwd       = lwd ) 
    sitenames <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
    mtext( paste("SITE ",s," (",sitenames[s],"), ","par: ",parname_SA, sep=""),
           side=3, line=1, outer=TRUE, cex=1, font=2)   
  }
  dev.off(dev_outputs_data)
  dev.off(dev_outputs     )
  
  dev.set(dev_likelihoods )
  nrow_plot_L <- ceiling( sqrt((nSites+3) * 8/11) )
  ncol_plot_L <- ceiling( (nSites+3)/nrow_plot_L )
  par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
  
  vector_logLtot <- rep( NA, nmult )
  for(g in 1:nmult) vector_logLtot[g] <- sum( list_df_logL[[g]], na.rm=TRUE )
  barplot( vector_logLtot-min(vector_logLtot),
           main=paste("SUM ALL SITES"), col=1:nmult )
  for(s in 1:nSites) {
    vector_logL <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logL[g] <- rowSums(list_df_logL[[g]]) [s]
    }
    barplot( vector_logL-min(vector_logL), main=paste("site",s),
             ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
             col=1:nmult )
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )

  for(v in 1:nvar) {
    par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
    vector_logLtot <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logLtot[g] <- sum( list_df_logL[[g]][[v]], na.rm=TRUE )
    }
    barplot( vector_logLtot-min(vector_logLtot),
             main=paste(names_var[v],":","SUM ALL SITES"), col=1:nmult )
    vector_logL    <- rep(NA,nmult)
    for (s in 1:nSites) {
      for(g in 1:nmult) {
        vector_logL[g] <- list_df_logL[[g]][[v]] [s]
      }
      barplot( vector_logL-min(vector_logL),
               main=paste(names_var[v],":","site",s),
               ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
               col=1:nmult )
    }
    plot(1,type='n', axes=FALSE, xlab="", ylab="")
    legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )
  }
  dev.off(dev_likelihoods )
} # End of function 'SA_BC()'

################################################################################
### 6. FUNCTIONS FOR ANALYSING & CORRECTING MULTI-YEAR WEATHER FILES
analyse_weather_Bioforsk <- function( file_weather        ,
                                      plot_average = NULL ,
                                      correction   = NULL ,
                                      flag_missing = "NULL") {
# File names
file_weather_corrected      <- gsub(".txt","_corrected.txt",file_weather)
file_weather_plot           <- gsub(".txt",".pdf"          ,file_weather)
file_weather_plot_corrected <- gsub(".txt","_corrected.pdf",file_weather)
# Reading weather data
df_weather    <- read.table( file_weather, header=TRUE, na.strings=flag_missing )
weather_names <- colnames(df_weather)[4:10]
# Summary report
str(df_weather)
ndays_weather <- dim(df_weather)[1]
nNA           <- colSums(is.na(df_weather))
cat("Number of NA's per variable =\n"); print(nNA)
cat("Maxima of",weather_names,":\n")
print( sapply( 4:10, function(v)max(df_weather[,v],na.rm=TRUE) ) )
cat("Minima of",weather_names,":\n")
print( sapply( 4:10, function(v)min(df_weather[,v],na.rm=TRUE) ) )
cat("Number of negative values for\n",weather_names,":\n")
print( sapply( 4:10, function(v)sum(df_weather[,v]<0,na.rm=TRUE) ) )
# Interannual mean, min and max weather
matrix_weather <- matrix( NA, nrow=366, ncol=7 )
weather_mean   <- matrix_weather  
weather_min    <- matrix_weather ; weather_max <- matrix_weather  
for (doy in 1:366) {
  weather_doy        <- as.matrix( df_weather[df_weather$doy==doy,] )
  weather_mean[doy,] <- colMeans( weather_doy[,4:10], na.rm=TRUE )
  weather_max [doy,] <- sapply( 4:10, function(v) max(weather_doy[,v],na.rm=TRUE) )
  weather_min [doy,] <- sapply( 4:10, function(v) min(weather_doy[,v],na.rm=TRUE) )
}
if ( !is.null(correction) ) {
# Prepare a dataframe which will hold corrected values
df_weather_corrected <- df_weather
limitdev_T     <- 30 ; limitlow_T     <- -40   ; limithigh_T     <-  40
limitdev_RH    <- 50 ; limitlow_RH    <-  10   ; limithigh_RH    <- 100
limitdev_RAINI <- 80 ; limitlow_RAINI <-   0   ; limithigh_RAINI <- 100
limitdev_WNI   <- 20 ; limitlow_WNI   <-   0   ; limithigh_WNI   <-  30
limitdev_GR    <- 30 ; limitlow_GR    <-   0.1 ; limithigh_GR    <-  40
# Set outliers (deviating from the mean by more than limitdev_x) to NA
n_removed <- rep(0,7) ; n_RAINImade0 <- 0; n_GRmade0 <- 0
for (d in 1:ndays_weather) {
  w <- df_weather[d,]
  if ( !is.na(w$T)     && ( (abs(w$T    -weather_mean[w$doy,1])>limitdev_T) ||
                            (w$T     < limitlow_T      ) ||
                            (w$T     > limithigh_T     )    )     ) {
    df_weather_corrected$T    [d] <- NA
    n_removed[1] <- n_removed[1] + 1
  }
  if ( !is.na(w$TMMXI) && ( (abs(w$TMMXI-weather_mean[w$doy,2])>limitdev_T) ||
                            (w$TMMXI < limitlow_T      ) ||
                            (w$TMMXI > limithigh_T     )    )     ) {
    df_weather_corrected$TMMXI[d] <- NA
    n_removed[2] <- n_removed[2] + 1
  }
  if ( !is.na(w$TMMNI) && ( (abs(w$TMMNI-weather_mean[w$doy,3])>limitdev_T) ||
                            (w$TMMNI < limitlow_T      ) ||
                            (w$TMMNI > limithigh_T     )    )     ) {
    df_weather_corrected$TMMNI[d] <- NA
    n_removed[3] <- n_removed[3] + 1
  }
  if ( !is.na(w$RH)    && ( (abs(w$RH   -weather_mean[w$doy,4])>limitdev_RH) ||
                            (w$RH    < limitlow_RH     ) ||
                            (w$RH    > limithigh_RH    )    )     ) {
    df_weather_corrected$RH   [d] <- NA
    n_removed[4] <- n_removed[4] + 1
  }
  if ( !is.na(w$RAINI) && ( (abs(w$RAINI-weather_mean[w$doy,5])>limitdev_RAINI) ||
                            (w$RAINI > limithigh_RAINI )    )     ) {
    df_weather_corrected$RAINI[d] <- NA
    n_removed[5] <- n_removed[5] + 1
  } else if ( !is.na(w$RAINI) &&  (w$RAINI < limitlow_RAINI )     ) {
    df_weather_corrected$RAINI[d] <- 0
    n_RAINImade0 <- n_RAINImade0[1] + 1
  }
  if ( !is.na(w$WNI)   && ( (abs(w$WNI  -weather_mean[w$doy,6])>limitdev_WNI) ||
                            (w$WNI   < limitlow_WNI    ) ||
                            (w$WNI   > limithigh_WNI   )    )     ) {
    df_weather_corrected$WNI  [d] <- NA
    n_removed[6] <- n_removed[6] + 1
  }
  if ( !is.na(w$GR) && ( (abs(w$GR-weather_mean[w$doy,7])>limitdev_GR) ||
                            (w$GR > limithigh_GR )    )     ) {
    df_weather_corrected$GR[d] <- NA
    n_removed[7] <- n_removed[7] + 1
  } else if ( !is.na(w$GR) &&  (w$GR < limitlow_GR ) && (weather_max[w$doy,7]> 0.5) ) {
    df_weather_corrected$GR[d] <- NA
    n_removed[7] <- n_removed[7] + 1
  } else if ( !is.na(w$GR) &&  (w$GR < limitlow_GR ) && (weather_max[w$doy,7]<=0.5) ) {
    df_weather_corrected$GR[d] <- 0
    n_GRmade0 <- n_GRmade0[1] + 1
  }
}
cat("Number of outliers removed for \n",weather_names,":\n")
print( n_removed )
cat("Number of negative rain values set to 0:\n")
print( n_RAINImade0 )
cat("Number of negative or zero GR values set to 0:\n")
print( n_GRmade0 )
# Recalculate interannual means (which were biased by outliers before)
weather_corrected_mean <- matrix_weather
weather_corrected_min  <- matrix_weather ; weather_corrected_max <- matrix_weather  
for (doy in 1:366) {
  weather_corrected_doy        <- as.matrix( df_weather_corrected[df_weather_corrected$doy==doy,] )
  weather_corrected_mean[doy,] <- signif( colMeans( weather_corrected_doy[,4:10], na.rm=TRUE ), 3 )
  weather_corrected_max [doy,] <- sapply( 4:10, function(v) max(weather_corrected_doy[,v],na.rm=TRUE) )
  weather_corrected_min [doy,] <- sapply( 4:10, function(v) min(weather_corrected_doy[,v],na.rm=TRUE) )
}
# Replace NA (original as well as from outliers) with interannual means
n_NAmademean <- rep(0,7)
for (d in 1:ndays_weather) {
  doy                           <- df_weather_corrected[d,]$doy
  weather_corrected_day         <- as.numeric( df_weather_corrected[d,4:10] )
  iNA                           <- which( is.na(weather_corrected_day) )
  df_weather_corrected[d,iNA+3] <- weather_corrected_mean[doy,iNA]
  n_NAmademean[iNA]             <- n_NAmademean[iNA] + 1
}
cat("Number of missing values set to multi-year averages for\n",weather_names,":\n")
print( n_NAmademean )
# Export the final corrected weather
write.table( df_weather_corrected, file_weather_corrected,
         		 sep="\t", row.names=F )
}  
# Plot
if ( !is.null(plot_average) ) {
  pdf( file_weather_plot )
  for (v in 1:7) {
    g_range <- range( weather_mean[,v], weather_min[,v], weather_max[,v] ) 
    plot  (1:366,weather_mean[,v],ylab=weather_names[v],ylim=g_range)
    points(1:366,weather_min [,v],ylab=weather_names[v],col='red')
    points(1:366,weather_max [,v],ylab=weather_names[v],col='red')
  }
  dev.off()
  if ( !is.null(correction) ) {
    pdf( file_weather_plot_corrected )
    for (v in 1:7) {
      g_range <- range( weather_corrected_mean[,v], weather_corrected_min[,v], weather_corrected_max[,v] ) 
      plot  (1:366,weather_corrected_mean[,v],ylab=weather_names[v],ylim=g_range)
      points(1:366,weather_corrected_min [,v],ylab=weather_names[v],col='red')
      points(1:366,weather_corrected_max [,v],ylab=weather_names[v],col='red')
    }
  dev.off()
  }
}
}

################################################################################
### 7. FUNCTIONS FOR COMPARING THE RESULTS OF DIFFERENT BCs

##############################
### 7.1 Function 'make_Setups'
##############################
make_Setups <- function( files_init_BC  = c(file_init_BC1 ,file_init_BC2 ),
                         files_parModes = c(file_parModes1,file_parModes2) ) {
# General settings for the collection of simulation setups
  nfiles_init <- length(files_init_BC) ; nfiles_par <- length(files_parModes)
  nSetups     <- max( nfiles_init, nfiles_par )
  if(nfiles_init == 1) files_init_BC  <- rep(files_init_BC ,nSetups)
  if(nfiles_par  == 1) files_parModes <- rep(files_parModes,nSetups)
  Setups      <- vector( "list", nSetups )
  Setups$nSetups        <- nSetups
  Setups$file_init_BC   <- files_init_BC[1] # First init_BC file is used to define sites and data sets
  source( Setups$file_init_BC )
  Setups$nSites         <- nSites
  Setups$sitenames      <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
  Setups$matrix_weather <- list_matrix_weather
  Setups$days_harvest   <- list_days_harvest
  Setups$NDAYS          <- list_NDAYS
  # Defining the individual setups
  for(i in 1:nSetups) {
    source( files_init_BC[i] )
    sitenames_Setup_i          <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
    Setups[[i]]$iSites         <- match( sitenames_Setup_i, Setups$sitenames )
    Setups[[i]]$parname_BC     <- parname_BC
    Setups[[i]]$parsites_BC    <- parsites_BC
    Setups[[i]]$list_ModePrior <- vector( "list", nSites )
    Setups[[i]]$list_MAP       <- vector( "list", nSites )
    Setups[[i]]$list_MaxL      <- vector( "list", nSites )
    parModes                   <- read.table( files_parModes[i], header=TRUE, sep="\t" )
    for (s in 1:nSites) {
      col_ModePrior_s                 <- paste( "ModePrior_", as.character(s), sep="" )
      col_MAP_s                       <- paste( "MAP_"      , as.character(s), sep="" )
      col_MaxL_s                      <- paste( "MaxL_"     , as.character(s), sep="" )
      par_ModePrior_s                 <- as.matrix( parModes[col_ModePrior_s] )
      par_MAP_s                       <- as.matrix( parModes[col_MAP_s      ] )
      par_MaxL_s                      <- as.matrix( parModes[col_MaxL_s     ] )
      Setups[[i]]$list_ModePrior[[s]] <- par_ModePrior_s
      Setups[[i]]$list_MAP[[s]]       <- par_MAP_s
      Setups[[i]]$list_MaxL[[s]]      <- par_MaxL_s
    }
  }
  Setups$siteSetups <- vector( "list", Setups$nSites )
  for (s in 1:Setups$nSites) {
    for(j in 1:nSetups) {
      if(s %in% Setups[[j]]$iSites) Setups$siteSetups[[s]] <- c( Setups$siteSetups[[s]], j )
    }    
  }
  return(Setups)
} # End of function 'make_Setups'

##################################
### 7.2 Function 'plot_outputs_Setups'
##################################
plot_outputs_Setups <- function(
  Setups                 = Setups1,
  partype                = "MAP",
  file_plot_outputs_data = paste("outputs_data_Setups",format(Sys.time(),"_%H_%M.pdf")),
  file_table_outputs_data = NULL,
  file_plot_outputs      = paste("outputs_Setups"     ,format(Sys.time(),"_%H_%M.pdf")),
  file_plot_likelihoods  = paste("likelihoods_Setups" ,format(Sys.time(),"_%H_%M.pdf")),
  file_table_likelihoods = NULL,
  leg_title              = "LEGEND" )
{ source( Setups$file_init_BC )
  nSetups  <- Setups$nSetups ; nSites <- Setups$nSites
  npartype <- length(partype)
  nrepeats_Setup    <- if(nSetups  == 1) npartype else 1
  nrepeats_partype  <- if(npartype == 1) nSetups  else 1
  partype           <- rep( partype, nrepeats_partype )
  nCategories       <- max(nSetups,npartype)    
  
  ivar              <- unique( c( unlist(data_index), unlist(data_mm_index) ) )
  names_var         <- outputNames[ivar]
  nvar              <- length(ivar)
  df_ndata          <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  df_sumdata        <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  df_SSE            <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  df_logL           <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  list_data_output  <- vector("list",nSites)
  names(df_ndata)   <- names_var
  names(df_sumdata) <- names_var
  names(df_SSE)     <- names_var
  names(df_logL)    <- names_var
  list_df_ndata         <- vector("list",nCategories)
  list_df_sumdata       <- vector("list",nCategories)
  list_df_SSE           <- vector("list",nCategories)
  list_df_logL          <- vector("list",nCategories)
  list_list_data_output <- vector("list",nCategories)
  for(i in 1:nCategories) {
    list_df_ndata  [[i]] <- df_ndata
    list_df_sumdata[[i]] <- df_sumdata
    list_df_SSE    [[i]] <- df_SSE
    list_df_logL   [[i]] <- df_logL
  }
  pdf( file_plot_outputs_data, paper="a4r", width=11, height=8 )
  dev_outputs_data <- dev.cur()    
  pdf( file_plot_outputs     , paper="a4r", width=11, height=8 )
  dev_outputs      <- dev.cur()  
  pdf( file_plot_likelihoods , paper="a4r", width=11, height=8 )
  dev_likelihoods  <- dev.cur()  
  
  for (s in 1:nSites) {
    matrix_weather <- Setups$matrix_weather[[s]]
    days_harvest   <- Setups$days_harvest[[s]]
    NDAYS          <- Setups$NDAYS[[s]]
    isiteSetups_s  <- rep( Setups$siteSetups[[s]], nrepeats_Setup )
    nSetup_s       <- length( isiteSetups_s )
    list_output    <- vector( "list", nSetup_s )
    for (g in 1:nSetup_s) {
      iSetup           <- isiteSetups_s[g]
      setup            <- Setups[[iSetup]]
      list_params      <- switch( partype[[g]], "ModePrior"=setup$list_ModePrior,
                                                "MAP"      =setup$list_MAP,
                                                "MaxL"     =setup$list_MaxL )
      params           <- list_params[[ which(setup$iSites==s) ]]
      output           <- run_model(params,matrix_weather,days_harvest,NDAYS)
      list_output[[g]] <- output	  
      datai_s          <- calc_statsi_s( s, output=output )$datai_s
	    outi_s           <- calc_statsi_s( s, output=output )$outi_s
      SSEi_s           <- calc_statsi_s( s, output=output )$SSEi_s
      logLi_s          <- calc_statsi_s( s, output=output )$logLi_s
      list_list_data_output[[g]][[s]] <- cbind(datai_s,outi_s[,2]) 
      for(v in 1:nvar) {
        rows_var_s                   <- which( datai_s[,1]==ivar[v] )
        ndatai_s                     <- length(      rows_var_s )
        list_df_ndata  [[g]][[v]][s] <- ndatai_s
        list_df_sumdata[[g]][[v]][s] <- sum( datai_s[rows_var_s,2] )
        list_df_SSE    [[g]][[v]][s] <- sum( SSEi_s [rows_var_s,2] )
        list_df_logL   [[g]][[v]][s] <- sum( logLi_s[rows_var_s,2] )
      }
    }    
    
    leg  <- if(npartype>nSetups) partype      else paste("Setup",isiteSetups_s)
    cols <- if(npartype>nSetups) (1:npartype) else isiteSetups_s

    dev.set(dev_outputs_data)
    plot_outputs_data_s( isite        = s,
                         list_runs    = list_output,
                         leg_title    = NULL,
                         leg          = leg,
                         cols         = cols,
                         lwds         = rep(3,nSetup_s),
                         ltys         = rep(1,nSetup_s) )  
    
    dev.set(dev_outputs)
    par( omi=c(0,0,0.3,0), mar=c(2,2,2,1) )
    plot_output( list_output = list_output,
                 leg_title   = NULL,
                 leg         = leg ) 
    sitenames <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
    mtext( paste("SITE ",s," (",sitenames[s],")",sep=""),
           side=3, line=1, outer=TRUE, cex=1, font=2)   
  }
  dev.off(dev_outputs_data)
  dev.off(dev_outputs     )
  
  dev.set(dev_likelihoods )
  nrow_plot_L <- ceiling( sqrt((nSites+3) * 8/11) )
  ncol_plot_L <- ceiling( (nSites+3)/nrow_plot_L )
  par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
  leg <- if(npartype>nSetups) partype else as.character(1:nSetups)
  
  vector_logLtot <- rep( NA, nCategories )
  for(g in 1:nCategories) vector_logLtot[g] <- sum( list_df_logL[[g]], na.rm=TRUE )
  barplot( vector_logLtot-min(vector_logLtot),
           main=paste("SUM ALL SITES"), col=1:nCategories )
  for(s in 1:nSites) {
    vector_logL <- rep(NA,nCategories)
    for(g in 1:nCategories) {
      vector_logL[g] <- rowSums(list_df_logL[[g]]) [s]
    }
    barplot( vector_logL-min(vector_logL), main=paste("site",s),
             ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
             col=1:nCategories )
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title=leg_title, leg=leg, fill=1:nCategories, horiz=TRUE )

  for(v in 1:nvar) {
    par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
    vector_logLtot <- rep(NA,nCategories)
    for(g in 1:nCategories) {
      vector_logLtot[g] <- sum( list_df_logL[[g]][[v]], na.rm=TRUE )
    }
    barplot( vector_logLtot-min(vector_logLtot),
             main=paste(names_var[v],":","SUM ALL SITES"), col=1:nCategories )
    vector_logL    <- rep(NA,nCategories)
    for (s in 1:nSites) {
      for(g in 1:nCategories) {
        vector_logL[g] <- list_df_logL[[g]][[v]] [s]
      }
      barplot( vector_logL-min(vector_logL),
               main=paste(names_var[v],":","site",s),
               ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
               col=1:nCategories )
    }
    plot(1,type='n', axes=FALSE, xlab="", ylab="")
    legend( "bottomright", title=leg_title, leg=leg, fill=1:nCategories, horiz=TRUE)
  }
  dev.off(dev_likelihoods )
  
  if(!is.null(file_table_likelihoods)) {
    for(g in 1: nCategories) {
      write.table( rbind(1:nSites,t(as.matrix(list_df_logL   [[g]]))),
                   file_table_likelihoods,
                   col.names=FALSE,
                   sep=",", append=T )
      write.table( rbind(1:nSites,t(as.matrix(list_df_ndata  [[g]]))),
                   file_table_likelihoods,
                   col.names=FALSE,
                   sep=",", append=T )
      write.table( rbind(1:nSites,t(as.matrix(list_df_sumdata[[g]]))),
                   file_table_likelihoods,
                   col.names=FALSE,
                   sep=",", append=T )
      write.table( rbind(1:nSites,t(as.matrix(list_df_SSE    [[g]]))),
                   file_table_likelihoods,
                   col.names=FALSE,
                   sep=",", append=T )
    }
  }
  
  if(!is.null(file_table_outputs_data)) {
    ncolumns <- 6
    for(g in 1: nCategories) {
      write.table( rbind( c("Category: ",g,rep("",ncolumns-2)),
                          rep("",ncolumns),
                          c("Site","Variable","Year","DOY","Data","Output"),
                          rep("",ncolumns)
                   ),
                   file_table_outputs_data,
                   row.names=FALSE,
                   col.names=FALSE,
                   sep=",", append=T )
      for (s in 1:nSites) {
  	    write.table( rbind( cbind( rep( s, dim(list_list_data_output[[g]][[s]])[1] ),
                                   list_list_data_output[[g]][[s]]
                            ),
                            rep("",ncolumns) ),
                     file_table_outputs_data,
  	                 row.names=FALSE,
  	                 col.names=FALSE,
  	                 sep=",", append=T )
	  }
    }
  }

  
} # End of function 'plot_outputs_Setups'

#####################################
### 7.3 Function 'plot_params_Setups'
#####################################
plot_params_Setups <- function(
  Setups           = Setups1,
  partype          = "MAP",
  file_plot_params = paste("params_Setups",format(Sys.time(),"_%H_%M.pdf")) )
{ source( Setups$file_init_BC )

  nSetups  <- Setups$nSetups ; nSites <- Setups$nSites
  npartype <- length(partype)
  nrepeats_Setup   <- if(nSetups  == 1) npartype else 1
  nrepeats_partype <- if(npartype == 1) nSetups  else 1
  partype          <- rep( partype, nrepeats_partype )
  
  # Complete list of parameter names (not only those used in BC)
  parname            <- row.names(df_params)
  # Lists of parameter names used in BC: (all, generic BC, site-specific BC)
  parname_BC_1       <- Setups[[1]]$parname_BC
  parsites_BC_1      <- Setups[[1]]$parsites_BC
  parname_BC_all     <- unique( parname_BC_1 )
  parname_BC_generic <- parname_BC_1[ sapply( 1:length(Setups[[1]]$parsites_BC),
    function(i){identical( eval(parse(text=Setups[[1]]$parsites_BC[i])),1:nSites)} ) ]
  parname_BC_site    <- parname_BC_all[!(parname_BC_all %in% parname_BC_generic)]
  np_BC_all          <- length(parname_BC_all)
  np_BC_generic      <- length(parname_BC_generic)
  np_BC_site         <- length(parname_BC_site)
  # Indices of BC-parameters in the complete parameter list
  ip_BC_generic      <- match( parname_BC_generic, parname )
  ip_BC_site         <- match( parname_BC_site   , parname )
  
  # Initiate barplots for the BC-parameters
  pageh <- 8 ; pagew <- 11
  pdf( file_plot_params, paper="A4r", height=pageh, width=pagew )
  nrowsPlots <- ceiling( sqrt((np_BC_all+1)*pageh/pagew) )
  ncolsPlots <- ceiling( (np_BC_all+1)/nrowsPlots )
  par( mfrow = c(nrowsPlots,ncolsPlots), mar=c(2,2,2,1) )
  
  # Barplots for the generic BC-parameters
  titles <- parname_BC_generic
  for(p in 1:np_BC_generic) {
    iSetups                 <- rep( 1:nSetups, nrepeats_Setup )
    nCategories             <- max(nSetups,npartype)    
    delta_parMAP_BC_generic <- rep( NA, nCategories )
    for(g in 1:nCategories) {
      # We can retrieve generic values from any site (we choose each Setup's site 1)
      list_params_1 <- switch( partype[[1]], "ModePrior"=Setups[[1         ]]$list_ModePrior[[1]],
                                             "MAP"      =Setups[[1         ]]$list_MAP      [[1]],
                                             "MaxL"     =Setups[[1         ]]$list_MaxL     [[1]] )
      list_params_g <- switch( partype[[g]], "ModePrior"=Setups[[iSetups[g]]]$list_ModePrior[[1]],
                                             "MAP"      =Setups[[iSetups[g]]]$list_MAP      [[1]],
                                             "MaxL"     =Setups[[iSetups[g]]]$list_MaxL     [[1]] )
      delta_parMAP_BC_generic[g] <- ( list_params_g[ip_BC_generic[p]] /
                                      list_params_1[ip_BC_generic[p]]   ) - 1
    }
    barplot( delta_parMAP_BC_generic, main=titles[p], cex.main=1,
             col=1:nCategories, ylim=c(-1,1) )
  }
  
  if(np_BC_site>0) {
    # Barplots for the site-specific BC-parameters
    titles <- parname_BC_site
    for(p in 1:np_BC_site) {
      iSetups              <- rep( 1:nSetups, nrepeats_Setup )
      nCategories          <- max(nSetups,npartype)    
      delta_parMAP_BC_site <- vector("list", nCategories )
      for(g in 1:nCategories) {
        delta_parMAP_BC_site[[g]] <- rep( 0, nSites )
        # We retrieve site-specific values for each site separately, even though
        # some of the parameters may actually have the same value for multiple sites:
        for(s in 1:length(Setups[[iSetups[[g]]]]$iSites)) {
          iSite         <- Setups[[iSetups[[g]]]]$iSites[s]
          list_params_1 <- switch( partype[[1]], "ModePrior"=Setups[[1         ]]$list_ModePrior[[iSite]],
                                                 "MAP"      =Setups[[1         ]]$list_MAP      [[iSite]],
                                                 "MaxL"     =Setups[[1         ]]$list_MaxL     [[iSite]] )
          list_params_g <- switch( partype[[g]], "ModePrior"=Setups[[iSetups[g]]]$list_ModePrior[[s    ]],
                                                 "MAP"      =Setups[[iSetups[g]]]$list_MAP      [[s    ]],
                                                 "MaxL"     =Setups[[iSetups[g]]]$list_MaxL     [[s    ]] )   
          if(list_params_1[ip_BC_site[p]] > 0) {
            delta_parMAP_BC_site[[g]][iSite] <- ( list_params_g[ip_BC_site[p]] /
                                                  list_params_1[ip_BC_site[p]]   ) - 1
          } else {
            delta_parMAP_BC_site[[g]][iSite] <- 0
          }
        }
      }
      for(g in 1:nCategories) {
        barplot( delta_parMAP_BC_site[[g]], main=titles[p], cex.main=1,
                 ylim=range(delta_parMAP_BC_site), col=g, add=(g>1), beside=T )
      }
    }
  }
  
  leg  <- if(npartype>nSetups) partype else as.character(1:nSetups)
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", leg=leg, fill=1:nCategories )
  
  dev.off()
}

################################################################################