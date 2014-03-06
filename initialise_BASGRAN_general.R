## initialise_BASGRA_general.R ##

## 1. MODEL LIBRARY FILE ##
   dyn.load("BASGRAN.DLL")

## 2. OUTPUT VARIABLES ##
   NOUT <- as.integer(38)
   outputNameList <- vector("list",NOUT)  ; outputUnitList <- vector("list",NOUT)
   outputNameList[ 1] = "Time"            ; outputUnitList[ 1] = "(y)"
   outputNameList[ 2] = "year"            ; outputUnitList[ 2] = "(y)"
   outputNameList[ 3] = "doy"             ; outputUnitList[ 3] = "(d)"
   outputNameList[ 4] = "DAVTMP"          ; outputUnitList[ 4] = "(degC)"

   outputNameList[ 5] = "CLV"             ; outputUnitList[ 5] = "(g C m-2)"
   outputNameList[ 6] = "CLVD"            ; outputUnitList[ 6] = "(g C m-2)"
   outputNameList[ 7] = "CLVH"            ; outputUnitList[ 7] = "(g C m-2)"
   outputNameList[ 8] = "CRES"            ; outputUnitList[ 8] = "(g C m-2)"
   outputNameList[ 9] = "CRT"             ; outputUnitList[ 9] = "(g C m-2)"
   outputNameList[10] = "CSTUB"           ; outputUnitList[10] = "(g C m-2)"
   outputNameList[11] = "LAI"             ; outputUnitList[11] = "(m2 m-2)"
   outputNameList[12] = "ROOTD"           ; outputUnitList[12] = "(m)"
   outputNameList[13] = "TILG"            ; outputUnitList[13] = "(m-2)"
   outputNameList[14] = "TILV"            ; outputUnitList[14] = "(m-2)"
   outputNameList[15] = "YIELD"           ; outputUnitList[15] = "(kg C m-2)"
   outputNameList[16] = "YIELDN"          ; outputUnitList[16] = "(kg N m-2)"
   outputNameList[17] = "ET"              ; outputUnitList[17] = "(mm d-1)"
   outputNameList[18] = "WAL"             ; outputUnitList[18] = "(mm)"
   outputNameList[19] = "N2O"             ; outputUnitList[19] = "(kg N ha-1 yr-1)"
   outputNameList[19] = "Nemission"       ; outputUnitList[20] = "(kg N m-2 d-1)" 
   outputNameList[21] = "Ndep"            ; outputUnitList[21] = "(kg N m-2 d-1)"  
   outputNameList[22] = "Nfixation"       ; outputUnitList[22] = "(kg N m-2 d-1)" 
   outputNameList[23] = "FMIN"            ; outputUnitList[23] = "-"
   outputNameList[24] = "FORG"            ; outputUnitList[24] = "-"
   outputNameList[25] = "Nupt"            ; outputUnitList[25] = "(kg N m-2 d-1)"
   outputNameList[26] = "Nleaching"       ; outputUnitList[26] = "(kg N m-2 d-1)"
   outputNameList[27] = "(HARVLV+HARVST-(HARVST*HAGERE))*NC" ; outputUnitList[27] = "(g N m-2)"
   outputNameList[28] = "GLV"             ; outputUnitList[28] = "(g C m-2 d-1)"
   outputNameList[29] = "(CLV+CST+CRT+CSTUB)*NC"             ; outputUnitList[29] = "(g N m-2)"
   outputNameList[30] = "NLITT+NSOMS+NSOMF+NMIN"             ; outputUnitList[30] = "(kg N m-2)"
   outputNameList[31] = "GRT"             ; outputUnitList[31] = "(g C m-2 d-1)"   
   outputNameList[32] = "(HARVLV+HARVST-(HARVST*HAGERE))"    ; outputUnitList[32] = "(g C m-2)"
   outputNameList[33] = "GRES"            ; outputUnitList[33] = "(g C m-2 d-1)"
   outputNameList[34] = "Rsoil"           ; outputUnitList[34] = "(g C m-2 d-1)" 
   outputNameList[35] = "CLV+CST+CRT+CSTUB"                  ; outputUnitList[35] = "(g C m-2)"
   outputNameList[36] = "(CLITT+CSOMS+CSOMF)"                ; outputUnitList[36] = "(kg C m-2 d-1)"
   outputNameList[37] = "NSup"            ; outputUnitList[37] = "(g N m-2)"
   outputNameList[38] = "NMIN"            ; outputUnitList[38] = "(kg N m-2)"  

   
## 3. PREPARE PLOTTING ##
   plotOutputs <- function(nPlotRows,nPlotCols,plotNameList) {
     outputs_i <- sapply( 1:length(plotNameList),
                          function(i) which(as.character(outputNameList)==plotNameList[i]) )
     par(mfrow=c(nPlotRows,nPlotCols))
     for (i in outputs_i) {
     plot( output[,1],output[,i],
           xlab = paste(outputNameList[1],outputUnitList[1]),
           ylab = paste(outputNameList[i],outputUnitList[i])  )
     }
     return(cat("PLOTTING COMPLETED"))
   }
      
