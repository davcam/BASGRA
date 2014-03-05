## initialise_BASGRA_general.R ##

## 1. MODEL LIBRARY FILE ##
   dyn.load("BASGRA.DLL")

## 2. OUTPUT VARIABLES ##
   NOUT <- as.integer(35)
   outputNameList <- vector("list",NOUT) ; outputUnitList <- vector("list",NOUT)
   outputNameList[ 1] = "Time"           ; outputUnitList[ 1] = "(y)"
   outputNameList[ 2] = "year"           ; outputUnitList[ 2] = "(y)"
   outputNameList[ 3] = "doy"            ; outputUnitList[ 3] = "(d)"
   outputNameList[ 4] = "DAVTMP"         ; outputUnitList[ 4] = "(degC)"

   outputNameList[ 5] = "CLV"            ; outputUnitList[ 5] = "(g C m-2)"
   outputNameList[ 6] = "CLVD"           ; outputUnitList[ 6] = "(g C m-2)"
   outputNameList[ 7] = "CLVH"           ; outputUnitList[ 7] = "(g C m-2)"
   outputNameList[ 8] = "CRES"           ; outputUnitList[ 8] = "(g C m-2)"
   outputNameList[ 9] = "CRT"            ; outputUnitList[ 9] = "(g C m-2)"
   outputNameList[10] = "CST"            ; outputUnitList[10] = "(g C m-2)"
   outputNameList[11] = "CSTUB"          ; outputUnitList[11] = "(g C m-2)"
   outputNameList[12] = "DRYSTOR"        ; outputUnitList[12] = "(mm)"
   outputNameList[13] = "Fdepth"         ; outputUnitList[13] = "(m)"
   outputNameList[14] = "LAI"            ; outputUnitList[14] = "(m2 m-2)"
   outputNameList[15] = "LT50"           ; outputUnitList[15] = "(degC)"
   outputNameList[16] = "O2"             ; outputUnitList[16] = "(mol m-2)"
   outputNameList[17] = "PHEN"           ; outputUnitList[17] = "(-)"
   outputNameList[18] = "ROOTD"          ; outputUnitList[18] = "(m)"
   outputNameList[19] = "Sdepth"         ; outputUnitList[19] = "(m)"
   outputNameList[20] = "TANAER"         ; outputUnitList[20] = "(d)"
   outputNameList[21] = "TILG"           ; outputUnitList[21] = "(m-2)"
   outputNameList[22] = "TILV"           ; outputUnitList[22] = "(m-2)"
   outputNameList[23] = "WAL"            ; outputUnitList[23] = "(mm)"
   outputNameList[24] = "WAPL"           ; outputUnitList[24] = "(mm)"
   outputNameList[25] = "WAPS"           ; outputUnitList[25] = "(mm)"
   outputNameList[26] = "WAS"            ; outputUnitList[26] = "(mm)"
   outputNameList[27] = "WETSTOR"        ; outputUnitList[27] = "(mm)"
   outputNameList[28] = "DM"             ; outputUnitList[28] = "(g m-2)"
   outputNameList[29] = "RES"            ; outputUnitList[29] = "(g g-1)"
   outputNameList[30] = "LERG"           ; outputUnitList[30] = "(m d-1)"
   outputNameList[31] = "NELLVG"         ; outputUnitList[31] = "(tiller-1)"
   outputNameList[32] = "RLEAF"          ; outputUnitList[32] = "(d-1)"
   outputNameList[33] = "SLA"            ; outputUnitList[33] = "(m2 g-1)"
   outputNameList[34] = "TILTOT"         ; outputUnitList[34] = "(m-2)"
   
   outputNameList[35] = "FRTILG"         ; outputUnitList[35] = "(-)"
   
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
      