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

integer :: day, doy, i, NDAYS, NOUT, year, ift, ihv
real    :: y(NDAYS,NOUT)

! State variables
real :: CLV, CLVD, CLVH, CRES, CRT, CST, CSTUB, DRYSTOR, Fdepth, LAI, LT50, O2, PHEN
real :: ROOTD, Sdepth, TILG, TILV, TANAER, WAL, WAPL, WAPS, WAS, WETSTOR

real :: CLITT, CSOMF, CSOMS, NLITT, NSOMF, NSOMS, NMIN 

! Composite state variables
real :: FRTILG

! Rate variables
real :: DeHardRate, DLAI, DLV, DPHEN, DRAIN, DRT, DSTUB, dTANAER, DTILV, EVAP, EXPLOR
real :: Frate, FREEZEL, FREEZEPL, GLAI, GLV, GPHEN, GRES, GRT, GST, GSTUB, HardRate
real :: HARVLA, HARVLV, HARVPH, HARVRE, HARVST, HARVTG, INFIL, IRRIG, O2IN
real :: O2OUT, PackMelt, poolDrain, poolInfil, Psnow, reFreeze, RESMOB, RGRTV
real :: RGRTVG, RROOTD, RUNOFF, SnowMelt, THAWPS, THAWS, TRAN, Wremain

real :: RWA,WFPS

real :: Ndep,Nsup,Nupt,Ndemand

real :: FMIN,FORG

Ndep = 25./(10000*365)

! Initial constants
CLITT    = CLITT0
CSOMF    = CSOM0 * FCSOMF0
CSOMS    = CSOM0 * (1-FCSOMF0)
NLITT    = CLITT0 / CNLITT0
NSOMF    = (CSOM0 *    FCSOMF0)  / CNSOMF0
NSOMS    = (CSOM0 * (1-FCSOMF0)) / CNSOMS0
NMIN     = NMIN0

CLV     = CLVI
CLVD    = CLVDI
CLVH    = CLVHI
CRES    = CRESI
CRT     = CRTI
CST     = CSTI
CSTUB   = CSTUBI
LAI     = LAII
LT50    = LT50I
O2      = FGAS * ROOTDM * FO2MX * 1000./22.4
PHEN    = PHENI
ROOTD   = ROOTDM
!TILG    = TILTOTI *       FRTILGI
!TILV    = TILTOTI * (1. - FRTILGI)
TILG    = TILGI
TILV    = TILVI
WAL     = 1000. * ROOTDM * WCI

#ifdef winter

DRYSTOR = DRYSTORI
Fdepth  = Fdepth
Sdepth  = SDEPTHI
TANAER  = TANAERI
WAPL    = WAPLI
WAPS    = WAPSI
WAS     = WASI
WETSTOR = WETSTORI

#else

DRYSTOR = 0.0
Fdepth  = 0.0
Sdepth  = 0.0
TANAER  = 0.0
WAPL    = 0.0
WAPS    = 0.0
WAS     = 0.0
WETSTOR = 0.0

KRESPHARD = 0.0

#endif


ihv = 1
ift = 1

do day = 1, NDAYS

  ! Environment
  call set_weather_day(day,DRYSTOR,                    year,doy)
  call SoilWaterContent(Fdepth,ROOTD,WAL)
#ifdef winter 
  call Physics        (DAVTMP,Fdepth,ROOTD,Sdepth,WAS, Frate)
  call MicroClimate   (doy,DRYSTOR,Fdepth,Frate,LAI,Sdepth,Tsurf,WAPL,WAPS,WETSTOR, &
                                                       FREEZEPL,INFIL,PackMelt,poolDrain,poolInfil, &
                                                       pSnow,reFreeze,SnowMelt,THAWPS,wRemain)
#else
  Tsurf = DAVTMP
  call MicroClimate   (LAI)
#endif

  call DDAYL          (doy)
#ifdef weathergen  
  call PEVAPINPUT     (LAI)
#else
  call PENMAN         (LAI)
#endif
  ! Resources
  call Light          (DAYL,DTR,LAI,PAR)
  call EVAPTRTRF      (Fdepth,PEVAP,PTRAN,ROOTD,WAL,   EVAP,TRAN,RWA,WFPS)
  call ROOTDG         (Fdepth,ROOTD,WAL,               EXPLOR,RROOTD)
  ! Soil
  call FRDRUNIR       (EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS, &
                                                       DRAIN,FREEZEL,IRRIG,RUNOFF,THAWS)
#ifdef winter 
  call O2status       (O2,ROOTD)
#endif
  ! Plant
  call Harvest        (CLV,CRES,CST,doy,LAI,PHEN,TILG,TILV, &
                                                       GSTUB,HARVLA,HARVLV,HARVPH,HARVRE,HARVST,HARVTG,ihv)
  call Biomass        (CLV,CRES,CST)
  call Phenology      (DAYL,PHEN,                      DPHEN,GPHEN)
  call Foliage1
  call LUECO2TM       (PARAV)
#ifdef winter 
  call HardeningSink  (CLV,DAYL,doy,LT50,Tsurf)
#endif
  call Growth         (CLV,CRES,CST,PARINT,TILG,TILV,TRANRF, &
                                                       GLV,GRES,GRT,GST,RESMOB)
  call Nsupply(CRT,NMIN,Nsup)
  call Nplant(NSup,Nupt,Ndemand,GLV,GRT,GST)
#ifdef winter 
  call PlantRespiration(FO2,RESPHARD)
#endif
  call Senescence     (CLV,CRT,CSTUB,doy,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                                                       DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
  call Foliage2       (GLV,LAI,TRANRF,Tsurf,           GLAI,RGRTV,RGRTVG)
  ! Soil 2
#ifdef winter 
  call O2fluxes       (O2,PERMgas,ROOTD,RplantAer,     O2IN,O2OUT)
#endif
  call Tsoil_calc

  if (doy==doyFERT(ift)) then
    FMIN = FERTMIN(ift)/10000.
    FORG = FERTORG(ift)/10000.
    ift = ift + 1
  else
    FMIN = 0.0
    FORG = 0.0
  endif
  call CNsoil(RWA,WFPS,WAL,CLV,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS,ROOTD,DRAIN,RUNOFF,FMIN) 

  !================
  ! Outputs
  !================
  y(day, 1) = year + (doy-0.5)/366 ! "Time" = Decimal year (approximation)
  y(day, 2) = year
  y(day, 3) = doy
  y(day, 4) = DAVTMP

#ifdef winter 
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
#else
  y(day, 5) = CLV
  y(day, 6) = CLVD
  y(day, 7) = CLVH
  y(day, 8) = CRES
  y(day, 9) = CRT
  y(day,10) = CSTUB
  y(day,11) = LAI
  y(day,12) = ROOTD
  y(day,13) = TILG
  y(day,14) = TILV
  y(day,15) = HARVLV/1000.
  y(day,16) = (HARVLV*NC)/1000.
  y(day,17) = EVAP+TRAN
  y(day,18) = WAL
  y(day,19) = NemissionN2O*(10000.*365.)
  y(day,20) = Nemission
  y(day,21) = Ndep
  y(day,22) = Nfixation
  y(day,23) = FMIN
  y(day,24) = FORG
  y(day,25) = Nupt/1000.
  y(day,26) = Nleaching
  y(day,27) = (HARVLV+HARVST-(HARVST*HAGERE))*NC/1000.
  y(day,28) = GLV
  y(day,29) = (CLV+CST+CRT+CSTUB)*NC/1000.
  y(day,30) = NLITT+NSOMS+NSOMF+NMIN
  y(day,31) = GRT
  y(day,32) = (HARVLV+HARVST-(HARVST*HAGERE))
  y(day,33) = GRES
  y(day,34) = Rsoil*1000.
  y(day,35) = CLV+CST+CRT+CSTUB
  y(day,36) = (CLITT+CSOMS+CSOMF)*1000.
  y(day,37) = NSup
  y(day,38) = NMIN
#endif



  CLV     = CLV     + GLV   - DLV    - HARVLV
  CLVD    = CLVD            + DLV
  CLVH    = CLVH                     + HARVLV
  CRES    = CRES    + GRES  - RESMOB - HARVRE
  CRT     = CRT     + GRT   - DRT
  CST     = CST     + GST           - HARVST
  CSTUB   = CSTUB   + GSTUB - DSTUB
  LAI     = LAI     + GLAI - DLAI   - HARVLA
  PHEN    = min(1., PHEN + GPHEN - DPHEN - HARVPH)
  ROOTD   = ROOTD   + RROOTD
  TILG    = TILG    + TILV*RGRTVG         - HARVTG 
  TILV    = TILV    + TILV*(RGRTV-RGRTVG) - DTILV           
#ifdef winter
  DRYSTOR = DRYSTOR + reFreeze + Psnow - SnowMelt
  Fdepth  = Fdepth  + Frate
  LT50    = LT50    + DeHardRate - HardRate
  O2      = O2      + O2IN - O2OUT
  Sdepth  = Sdepth  + Psnow/RHOnewSnow - PackMelt
  TANAER  = TANAER  + dTANAER
  WAL     = WAL  + THAWS  - FREEZEL  + poolDrain + INFIL +EXPLOR+IRRIG-DRAIN-RUNOFF-EVAP-TRAN
  WAPL    = WAPL + THAWPS - FREEZEPL + poolInfil - poolDrain
  WAPS    = WAPS - THAWPS + FREEZEPL
  WAS     = WAS  - THAWS  + FREEZEL
  WETSTOR = WETSTOR + Wremain - WETSTOR
#else
  WAL = WAL + RAIN + EXPLOR + IRRIG - RNINTC - DRAIN - RUNOFF - EVAP - TRAN
#endif
  
  CLITT = CLITT + (DLV + DSTUB)/1000. - rCLITT - dCLITT
  CSOMF = CSOMF + dCLITTsomf + DRT/1000. - rCSOMF - dCSOMF
  CSOMS = CSOMS + dCSOMFsoms - dCSOMS
  NLITT = NLITT + (DLV*NC)/1000. + (DSTUB*NC)/1000. - rNLITT - dNLITT
  NSOMF = NSOMF + (DRT*NC)/1000. + FORG + NLITTsomf - rNSOMF - dNSOMF
  NSOMS = NSOMS + NSOMFsoms - dNSOMS
  NMIN  = NMIN  + Ndep + Nmineralisation + Nfixation + (FMIN-(FMIN*KFERTEMIT)) - Nupt/1000. - Nleaching - Nemission

enddo

end
