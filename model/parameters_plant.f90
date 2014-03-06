module parameters_plant

implicit none

! Initial constants
!  real  :: CLVI, CRESI, CRTI, CSTI, LAII, PHENI, TILTOTI, FRTILGI
  real  :: CLVI, CRESI, CRTI, CSTI, LAII, PHENI, TILGI, TILVI

! Initial soil constants
  real  :: CLITT0, CSOM0, CNLITT0, CNSOMF0, CNSOMS0, FCSOMF0, NMIN0	
  
! Initial constants, continued
  real, parameter :: CLVDI  =  0.
  real, parameter :: CLVHI  =  0.
  real, parameter :: CSTUBI =  0.
  real            :: LT50I

! Process parameters
  real  :: CLAIV , COCRESMX, CSTAVM , DAYLB , DAYLP , DLMXGE, FSLAMIN, FSMAX
  real  :: HAGERE, K       ,KLUETILG, LAICR  , LAIEFT, LAITIL, LFWIDG, LFWIDV,  NELLVM
  real  :: PHENCR, PHY     , RDRSCO , RDRSMX, RDRTEM, RGENMX, ROOTDM,  RRDMAX
  real  :: RUBISC, SHAPE   , SIMAX1T, SLAMAX, SLAMIN, TBASE , TCRES ,  TOPTGE
  real  :: TRANCO, YG

! Soil Nitrogen paramters
  real :: KNUPT, KNMIN, NC, TCLITT, TCSOMF, TCSOMS, FLITTSOMF, FSOMFSOMS, KNEMIT
  real :: RNLEACH, RFN2O, WFPS50N2O, TMAXF, TSIGMAF
  real :: KNFIXMX,KNFIXK,KFERTEMIT

! Process parameters, continued
  real            :: Dparam, Hparam, KRDRANAER, KRESPHARD, KRSR3H
  real            :: LDT50A, LDT50B, LT50MN, LT50MX, RATEDMX
  real            :: RDRROOT      =  0.
  real, parameter :: RDRSTUB      =  0.2
  real            :: reHardRedDay
  real, parameter :: reHardRedEnd = 91.
  real            :: THARDMX, TsurfDiff
  
end module parameters_plant
