## run_batch_BASGRA.R ##

################################################################################
## FUNCTIONS USED IN THE FOLLOWING EXAMPLES
################################################################################

## write_output_csv
   write_output_csv <- function(prefix,var) {
     write.table(output,
                 file=paste(prefix,as.character(var),".csv",sep=""),
                 row.names=F,
                 col.names=sapply(1:length(outputNameList),
                                  function(j) paste(outputNameList[j],
                                                    outputUnitList[j]) ),
                 sep="," )
     return()
  }

## print_final_value
   print_final_value <- function(output_onscreen) {
     cat("RUN",as.character(i),
         "\n",
         "Final value of",output_onscreen,"=",
         as.character(output[NDAYS,which(as.character(outputNameList)==output_onscreen)]),
         "\n")
     return()     
   }

################################################################################
## EXAMPLE 1. BATCH JOB: VARYING HARVEST DATES ##
################################################################################
   source('initialise_BASGRA_Saerheim_00_early_Gri.R')
   list_harvestdays   <- list( as.integer(c(150,216,253)),
                               as.integer(c(180)),
                               as.integer(c(200,235)) )

   nH                 <- length(list_harvestdays)

   list_output        <- list()
   for (i in 1:nH) {
     .Fortran('set_params', list_harvestdays[[i]], parameters_default)
     output           <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]
     list_output[[i]] <- output
     write_output_csv("resH_",i)
     print_final_value("TILTOT")
   }
   
  colourseries <- c("red","black","blue","green") ; nc <- length(colourseries)
  par(mfrow=c(4,4))
  par(mar=c(2, 2, 2, 1))
  for (p in 4:NOUT) {
    g_range <- range(sapply(1:length(list_output),function(i) list_output[[i]][,p]))
    plot( list_output[[1]][,1], list_output[[1]][,p],
          xlab="",main=outputNameList[p],type='l',
          col=colourseries[1],lwd=3,ylim=g_range )
    for (i in 2:nH) {
      points( list_output[[i]][,1], list_output[[i]][,p],
              type='l', col=colourseries[1+(i-1)%%nc],lwd=3 ) }
  }

################################################################################
## EXAMPLE 2: Varying the value of a single parameter
################################################################################
source('initialise_BASGRA_Saerheim_00_early_Gri.R')
pSA         <- "K"
#pMult       <- c(0.5,1,2)
pMult       <- seq(0.5,1.5,0.1)

nM          <- length(pMult)
pNameList   <- row.names(dataframe_parameters)
i_pSA       <- which(as.character(pNameList)==pSA)

list_output <- list()
for (i in 1:nM) {
  parameters_modified        <- parameters_default
  parameters_modified[i_pSA] <- parameters_default[i_pSA] * pMult[i]
  .Fortran('set_params', harvestdays, parameters_modified)
  output           <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]
  list_output[[i]] <- output
  write_output_csv("resP_",i)
  print_final_value("TILTOT")
}

colourseries <- c("red","black","blue","green") ; nc <- length(colourseries)
par(mfrow=c(4,4))
par(mar=c(2, 2, 2, 1))
for (p in 4:NOUT) {
  g_range <- range(sapply(1:length(list_output),function(i) list_output[[i]][,p]))
  plot( list_output[[1]][,1], list_output[[1]][,p],
        xlab="",main=outputNameList[p],type='l',
        col=colourseries[1],lwd=3,ylim=g_range )
  for (i in 2:nM) {
    points( list_output[[i]][,1], list_output[[i]][,p],
            type='l', col=colourseries[1+(i-1)%%nc],lwd=3 ) }
}

################################################################################
## EXAMPLE 3: Varying the site
################################################################################
sitesettings_filenames <- c('initialise_BASGRA_Saerheim_00_early_Gri.R',
                            'initialise_BASGRA_Saerheim_00_late_Gri.R')

nS          <- length(sitesettings_filenames)

list_output <- list()
for (i in 1:nS) {
  source(sitesettings_filenames[i])
  output           <- .Fortran('BASGRA', NDAYS,NOUT, y)[[3]]
  list_output[[i]] <- output
  write_output_csv("resS_",i)
  print_final_value("TILTOT")
}

colourseries <- c("red","black","blue","green") ; nc <- length(colourseries)
par(mfrow=c(4,4))
par(mar=c(2, 2, 2, 1))
for (p in 4:NOUT) {
  t_range <- range(sapply(1:length(list_output),function(i) list_output[[i]][,1]))
  g_range <- range(sapply(1:length(list_output),function(i) list_output[[i]][,p]))
  plot( list_output[[1]][,1], list_output[[1]][,p],
        xlab="",main=outputNameList[p],type='l',
        col=colourseries[1],lwd=3,
        xlim=t_range,ylim=g_range )
  for (i in 2:nS) {
    points( list_output[[i]][,1], list_output[[i]][,p],
            type='l', col=colourseries[1+(i-1)%%nc],lwd=3 ) }
}
