module parameters_plant

implicit none

! Initial constants
  real :: LOG10CLVI, LOG10CRESI, LOG10CRTI, CSTI, LOG10LAII
  real ::      CLVI,      CRESI,      CRTI,            LAII
  real :: PHENI, TILTOTI, FRTILGI, FRTILGG1I
  
! Initial constants, continued
  real, parameter :: CLVDI  = 0.
  real, parameter :: YIELDI = 0.
  real, parameter :: CSTUBI = 0.
  real            :: LT50I

! Process parameters
  real :: CLAIV   , COCRESMX, CSTAVM, DAYLB   , DAYLG1G2, DAYLP  , DLMXGE, FSLAMIN
  real :: FSMAX   , HAGERE  , K     , KLUETILG, LAICR   , LAIEFT , LAITIL, LFWIDG
  real :: LFWIDV  , NELLVM  , PHENCR, PHY     , RDRSCO  , RDRSMX , RDRTEM, RGENMX
  real :: RGRTG1G2, ROOTDM  , RRDMAX, RUBISC  , SHAPE   , SIMAX1T, SLAMAX, SLAMIN
  real :: TBASE   , TCRES   , TOPTGE, TRANCO  , YG
  real :: RDRTMIN , TVERN
  
! Process parameters, continued
  real            :: Dparam, Hparam, KRDRANAER, KRESPHARD, KRSR3H
  real            :: LDT50A, LDT50B, LT50MN, LT50MX, RATEDMX
  real, parameter :: RDRROOT      =  0.001
  real, parameter :: RDRSTUB      =  0.2
  real            :: reHardRedDay
  real, parameter :: reHardRedEnd = 91.
  real            :: THARDMX, TsurfDiff
  
end module parameters_plant
