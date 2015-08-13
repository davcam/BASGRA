subroutine BASGRA(PARAMS,MATRIX_WEATHER,DAYS_HARVEST,NDAYS,NOUT,y)
!-------------------------------------------------------------------------------
! This is the BASic GRAss model originally written in MATLAB/Simulink by Marcel
! van Oijen, Mats Hoglind, Stig Morten Thorsen and Ad Schapendonk.
! 2011-07-13: Translation to FORTRAN by David Cameron and Marcel van Oijen.
! 2014-03-17: Extra category of tillers added
! 2014-04-03: Vernalization added
! 2014-04-03: Lower limit of temperature-driven leaf senescence no longer zero
!-------------------------------------------------------------------------------

use parameters_site
use parameters_plant
use environment
use resources
use soil
use plant
implicit none

integer               :: day, doy, i, NDAYS, NOUT, year
integer, dimension(100,2) :: DAYS_HARVEST
integer, parameter    :: NPAR     = 79
#ifdef weathergen  
  integer, parameter  :: NWEATHER =  7
#else
  integer, parameter  :: NWEATHER =  8
#endif
real                  :: PARAMS(NPAR)
real                  :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)
real                  :: y(NDAYS,NOUT)

! State variables
real :: CLV, CLVD, YIELD, CRES, CRT, CST, CSTUB, DRYSTOR, Fdepth, LAI, LT50, O2, PHEN
real ::            YIELD_LAST
real :: ROOTD, Sdepth, TILG1, TILG2, TILV, TANAER, WAL, WAPL, WAPS, WAS, WETSTOR
integer :: VERN

! Intermediate and rate variables
real :: DeHardRate, DLAI, DLV, DPHEN, DRAIN, DRT, DSTUB, dTANAER, DTILV, EVAP, EXPLOR
real :: Frate, FREEZEL, FREEZEPL, GLAI, GLV, GPHEN, GRES, GRT, GST, GSTUB, GTILV, HardRate
real :: HARVLA, HARVLV, HARVPH, HARVRE, HARVST, HARVTILG2, INFIL, IRRIG, O2IN
real :: O2OUT, PackMelt, poolDrain, poolInfil, Psnow, reFreeze, RESMOB, RGRTV
real :: RGRTVG1, RROOTD, RUNOFF, SnowMelt, THAWPS, THAWS, TILVG1, TILG1G2, TRAN, Wremain

! Parameters
call set_params(PARAMS)

! Calendar & weather
YEARI  = MATRIX_WEATHER(:,1)
DOYI   = MATRIX_WEATHER(:,2)
GRI    = MATRIX_WEATHER(:,3)
TMMNI  = MATRIX_WEATHER(:,4)
TMMXI  = MATRIX_WEATHER(:,5)
#ifdef weathergen  
  RAINI = MATRIX_WEATHER(:,6)
  PETI  = MATRIX_WEATHER(:,7)
#else
  VPI   = MATRIX_WEATHER(:,6)
  RAINI = MATRIX_WEATHER(:,7)
  WNI   = MATRIX_WEATHER(:,8)
#endif

! Initial constants
CLV     = CLVI
CLVD    = CLVDI
YIELD   = YIELDI
YIELD_LAST = YIELDI
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
TILG1   = TILTOTI *       FRTILGI *    FRTILGG1I
TILG2   = TILTOTI *       FRTILGI * (1-FRTILGG1I)
TILV    = TILTOTI * (1. - FRTILGI)
VERN    = 0
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
  call Harvest        (CLV,CRES,CST,year,doy,DAYS_HARVEST,LAI,PHEN,TILG2,TILV, &
                                                       GSTUB,HARVLA,HARVLV,HARVPH,HARVRE,HARVST,HARVTILG2)
  call Biomass        (CLV,CRES,CST)
  call Phenology      (DAYL,PHEN,                      DPHEN,GPHEN)
  call Foliage1
  call LUECO2TM       (PARAV)
  call HardeningSink  (CLV,DAYL,doy,LT50,Tsurf)
  call Growth         (CLV,CRES,CST,PARINT,TILG2,TILV,TRANRF, &
                                                       GLV,GRES,GRT,GST,RESMOB)
  call PlantRespiration(FO2,RESPHARD)
  call Senescence     (CLV,CRT,CSTUB,doy,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                                                       DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
!  call Foliage2       (GLV,LAI,TRANRF,Tsurf,           GLAI,RGRTV,RGRTVG1)
  call Foliage2       (DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf,VERN, &
                                                       GLAI,GTILV,TILVG1,TILG1G2)
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
  y(day, 7) = YIELD_LAST
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
  y(day,21) = TILG1 + TILG2
  y(day,22) = TILV
  y(day,23) = WAL
  y(day,24) = WAPL
  y(day,25) = WAPS
  y(day,26) = WAS
  y(day,27) = WETSTOR
  
  ! Extra derived variables for calibration
  y(day,28) = (CLV+CST+CSTUB)/0.45 + CRES/0.40   ! "DM"      = Aboveground dry matter in g m-2
  y(day,29) = (CRES/0.40) / y(day,28)            ! "RES"     = Reserves in g g-1 aboveground dry matter
  y(day,30) = LERG                               !
  y(day,31) = NELLVG                             !
  y(day,32) = RLEAF                              !
  y(day,33) = LAI / (CLV/0.45)                   ! "SLA"     = m2 leaf area g-1 dry matter vegetative tillers
  y(day,34) = TILG1 + TILG2 + TILV               ! "TILTOT"  = Total tiller number in # m-2
  y(day,35) = (TILG1+TILG2) / (TILG1+TILG2+TILV) ! "FRTILG"  = Fraction of tillers that is generative
  y(day,36) =  TILG1        / (TILG1+TILG2+TILV) ! "FRTILG1" = Fraction of tillers that is in TILG1
  y(day,37) =        TILG2  / (TILG1+TILG2+TILV) ! "FRTILG2" = Fraction of tillers that is in TILG2
  y(day,38) = RDRT
  y(day,39) = VERN

  CLV     = CLV     + GLV   - DLV    - HARVLV
  CLVD    = CLVD            + DLV
  YIELD   = (HARVLV + HARVST*HAGERE) / 0.45 + HARVRE/0.40
  if (YIELD>0) YIELD_LAST = YIELD
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
  TILG1   = TILG1           + TILVG1 - TILG1G2
  TILG2   = TILG2                    + TILG1G2 - HARVTILG2
  TILV    = TILV    + GTILV - TILVG1           - DTILV   
  if (DAVTMP<TVERN) VERN = 1
  WAL     = WAL  + THAWS  - FREEZEL  + poolDrain + INFIL +EXPLOR+IRRIG-DRAIN-RUNOFF-EVAP-TRAN
  WAPL    = WAPL + THAWPS - FREEZEPL + poolInfil - poolDrain
  WAPS    = WAPS - THAWPS + FREEZEPL
  WAS     = WAS  - THAWS  + FREEZEL
  WETSTOR = WETSTOR + Wremain - WETSTOR

enddo

end