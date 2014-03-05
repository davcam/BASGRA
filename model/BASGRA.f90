subroutine BASGRA(NDAYS,NOUT,y)
!-------------------------------------------------------------------------------
! This is the BASic GRAss model originally written in MATLAB/Simulink by Marcel
! van Oijen, Mats Hoglind, Stig Morten Thorsen and Ad Schapendonk.
! 2011-07-13: This translation to FORTRAN by David Cameron and Marcel van Oijen.
!-------------------------------------------------------------------------------

use parameters_site
use parameters_plant
use environment
use resources
use soil
use plant
implicit none

integer :: day, doy, i, NDAYS, NOUT, year
real    :: y(NDAYS,NOUT)

! State variables
real :: CLV, CLVD, CLVH, CRES, CRT, CST, CSTUB, DRYSTOR, Fdepth, LAI, LT50, O2, PHEN
real :: ROOTD, Sdepth, TILG, TILV, TANAER, WAL, WAPL, WAPS, WAS, WETSTOR

! Composite state variables
real :: FRTILG

! Rate variables
real :: DeHardRate, DLAI, DLV, DPHEN, DRAIN, DRT, DSTUB, dTANAER, DTILV, EVAP, EXPLOR
real :: Frate, FREEZEL, FREEZEPL, GLAI, GLV, GPHEN, GRES, GRT, GST, GSTUB, HardRate
real :: HARVLA, HARVLV, HARVPH, HARVRE, HARVST, HARVTG, INFIL, IRRIG, O2IN
real :: O2OUT, PackMelt, poolDrain, poolInfil, Psnow, reFreeze, RESMOB, RGRTV
real :: RGRTVG, RROOTD, RUNOFF, SnowMelt, THAWPS, THAWS, TRAN, Wremain

! Initial constants
CLV     = CLVI
CLVD    = CLVDI
CLVH    = CLVHI
CRES    = CRESI
CRT     = CRTI
CST     = CSTI
CSTUB   = CSTUBI
DRYSTOR = DRYSTORI
Fdepth  = FdepthI
LAI     = LAII
LT50    = LT50I
O2      = FGAS * ROOTDM * FO2MX * 1000./22.4
PHEN    = PHENI
ROOTD   = ROOTDM
Sdepth  = SDEPTHI
TANAER  = TANAERI
TILG    = TILTOTI *       FRTILGI
TILV    = TILTOTI * (1. - FRTILGI)
WAL     = 1000. * ROOTDM * WCI
WAPL    = WAPLI
WAPS    = WAPSI
WAS     = WASI
WETSTOR = WETSTORI

do day = 1, NDAYS

  ! Environment
  call set_weather_day(day,DRYSTOR,                    year,doy)
  call SoilWaterContent(Fdepth,ROOTD,WAL)
  call Physics        (DAVTMP,Fdepth,ROOTD,Sdepth,WAS, Frate)
  call MicroClimate   (doy,DRYSTOR,Fdepth,Frate,LAI,Sdepth,Tsurf,WAPL,WAPS,WETSTOR, &
                                                       FREEZEPL,INFIL,PackMelt,poolDrain,poolInfil, &
                                                       pSnow,reFreeze,SnowMelt,THAWPS,wRemain)
  call DDAYL          (doy)
#ifdef weathergen  
  call PEVAPINPUT     (LAI)
#else
  call PENMAN         (LAI)
#endif
  ! Resources
  call Light          (DAYL,DTR,LAI,PAR)
  call EVAPTRTRF      (Fdepth,PEVAP,PTRAN,ROOTD,WAL,   EVAP,TRAN)
  call ROOTDG         (Fdepth,ROOTD,WAL,               EXPLOR,RROOTD)
  ! Soil
  call FRDRUNIR       (EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS, &
                                                       DRAIN,FREEZEL,IRRIG,RUNOFF,THAWS)
  call O2status       (O2,ROOTD)
  ! Plant
  call Harvest        (CLV,CRES,CST,doy,LAI,PHEN,TILG,TILV, &
                                                       GSTUB,HARVLA,HARVLV,HARVPH,HARVRE,HARVST,HARVTG)
  call Biomass        (CLV,CRES,CST)
  call Phenology      (DAYL,PHEN,                      DPHEN,GPHEN)
  call Foliage1
  call LUECO2TM       (PARAV)
  call HardeningSink  (CLV,DAYL,doy,LT50,Tsurf)
  call Growth         (CLV,CRES,CST,PARINT,TILG,TILV,TRANRF, &
                                                       GLV,GRES,GRT,GST,RESMOB)
  call PlantRespiration(FO2,RESPHARD)
  call Senescence     (CLV,CRT,CSTUB,doy,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                                                       DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
  call Foliage2       (GLV,LAI,TRANRF,Tsurf,           GLAI,RGRTV,RGRTVG)
  ! Soil 2
  call O2fluxes       (O2,PERMgas,ROOTD,RplantAer,     O2IN,O2OUT)

  !================
  ! Outputs
  !================
  y(day, 1) = year + (doy-0.5)/366 ! "Time" = Decimal year (approximation)
  y(day, 2) = year
  y(day, 3) = doy
  y(day, 4) = DAVTMP
  
  y(day, 5) = CLV
  y(day, 6) = CLVD
  y(day, 7) = CLVH
  y(day, 8) = CRES
  y(day, 9) = CRT
  y(day,10) = CST
  y(day,11) = CSTUB
  y(day,12) = DRYSTOR
  y(day,13) = Fdepth
  y(day,14) = LAI
  y(day,15) = LT50
  y(day,16) = O2
  y(day,17) = PHEN
  y(day,18) = ROOTD
  y(day,19) = Sdepth
  y(day,20) = TANAER
  y(day,21) = TILG
  y(day,22) = TILV
  y(day,23) = WAL
  y(day,24) = WAPL
  y(day,25) = WAPS
  y(day,26) = WAS
  y(day,27) = WETSTOR
  
  ! Extra derived variables for calibration
  y(day,28) = (CLV+CST+CSTUB)/0.45 + CRES/0.40 ! "DM"     = Aboveground dry matter in g m-2
  y(day,29) = (CRES/0.40) / y(day,28)          ! "RES"    = Reserves in g g-1 aboveground dry matter
  y(day,30) = LERG                             !
  y(day,31) = NELLVG                           !
  y(day,32) = RLEAF                            !
  y(day,33) = LAI / (CLV/0.45)                 ! "SLA"    = m2 leaf area g-1 dry matter of vegetative tillers
  y(day,34) = TILG + TILV                      ! "TILTOT" = Total tiller number in # m-2
  y(day,35) = TILG / (TILG + TILV)             ! "FRTILG" = Fraction of tillers that is elongating

  CLV     = CLV     + GLV   - DLV    - HARVLV
  CLVD    = CLVD            + DLV
  CLVH    = CLVH                     + HARVLV
  CRES    = CRES    + GRES  - RESMOB - HARVRE
  CRT     = CRT     + GRT   - DRT
  CST     = CST     + GST           - HARVST
  CSTUB   = CSTUB   + GSTUB - DSTUB
  DRYSTOR = DRYSTOR + reFreeze + Psnow - SnowMelt
  Fdepth  = Fdepth  + Frate
  LAI     = LAI     + GLAI - DLAI   - HARVLA
  LT50    = LT50    + DeHardRate - HardRate
  O2      = O2      + O2IN - O2OUT
  PHEN    = min(1., PHEN + GPHEN - DPHEN - HARVPH)
  ROOTD   = ROOTD   + RROOTD
  Sdepth  = Sdepth  + Psnow/RHOnewSnow - PackMelt
  TANAER  = TANAER  + dTANAER
  TILG    = TILG    + TILV*RGRTVG         - HARVTG 
  TILV    = TILV    + TILV*(RGRTV-RGRTVG) - DTILV           
  WAL     = WAL  + THAWS  - FREEZEL  + poolDrain + INFIL +EXPLOR+IRRIG-DRAIN-RUNOFF-EVAP-TRAN
  WAPL    = WAPL + THAWPS - FREEZEPL + poolInfil - poolDrain
  WAPS    = WAPS - THAWPS + FREEZEPL
  WAS     = WAS  - THAWS  + FREEZEL
  WETSTOR = WETSTOR + Wremain - WETSTOR
  
enddo

end