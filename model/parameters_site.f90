module parameters_site

!%%%%% Settings for Saerheim 2000

! Simulation period and time step
  real, parameter       :: DELT   =   1.0

! Geography
  real                  :: LAT

! Atmospheric conditions
  real, parameter       :: CO2A   = 350   

! Soil
  real, parameter       :: DRATE  =  50
  real                  :: WCI
  real                  :: WCAD, WCFC, WCST, WCWET, WCWP


! Soil - WINTER PARAMETERS
  real                  :: FGAS, FO2MX, gamma, KRTOTAER, KSNOW
  real, parameter       :: LAMBDAice      = 1.9354e+005
  real                  :: LAMBDAsoil
  real, parameter       :: LatentHeat     = 335000.
  real, parameter       :: poolInfilLimit =      0.2
  real                  :: RHOnewSnow, RHOpack
  real, parameter       :: RHOwater       =   1000.
  real                  :: SWret, SWrf, TmeltFreeze, TrainSnow
  real                  :: WpoolMax
  
! Soil initial constants
  real, parameter       :: DRYSTORI = 0.
  real, parameter       :: FdepthI  = 0.
  real, parameter       :: SDEPTHI  = 0.
  real, parameter       :: TANAERI  = 0.
  real, parameter       :: WAPLI    = 0.
  real, parameter       :: WAPSI    = 0.
  real, parameter       :: WASI     = 0.
  real, parameter       :: WETSTORI = 0.
  
! Management: harvest dates and irrigation
  integer, dimension(3) :: doyHA
!  integer, dimension(3) :: doyHA  = (/ 150, 216, 253 /)
  real, parameter       :: IRRIGF = 0.

! Mathematical constants
  real, parameter       :: pi   = 3.141592653589793
  real, parameter       :: Freq = 2.*pi / 365.
  real, parameter       :: Kmin = 4.
  real, parameter       :: Ampl = 0.625
  real, parameter       :: Bias = Kmin + Ampl

end module parameters_site

