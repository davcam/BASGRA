module plant

use parameters_site
use parameters_plant
use environment
implicit none

integer :: NOHARV
real :: CRESMX,DAYLGE,FRACTV,GLVSI,GSTSI,LERG,LERV,LUEMXQ,NELLVG,PHENRF,PHOT
real :: RDRFROST,RDRT,RDRTOX,RESPGRT,RESPGSH,RESPHARD,RESPHARDSI,RESNOR,RLEAF,RplantAer,SLANEW
real :: RATEH,reHardPeriod,TV2TIL

Contains

!Subroutine Harvest(CLV,CRES,CST,doy,LAI,PHEN,TILG,TILV, &
Subroutine Harvest(CLV,CRES,CST,year,doy,DAYS_HARVEST,LAI,PHEN,TILG2,TILV, &
                             GSTUB,HARVLA,HARVLV,HARVPH,HARVRE,HARVST,HARVTILG2)
  integer :: doy,year
  integer,dimension(100,2) :: DAYS_HARVEST
  real    :: CLV, CRES, CST, LAI, PHEN, TILG2, TILV
  real    :: GSTUB, HARVLV, HARVLA, HARVRE, HARVTILG2, HARVST, HARVPH
  real    :: CLAI, HARVFR, TV1
  integer :: HARV,i
 
  HARV   = 0
  NOHARV = 1
  do i=1,100    
    if ( (year==DAYS_HARVEST(i,1)) .and. (doy==DAYS_HARVEST(i,2)) ) then
      HARV   = 1
      NOHARV = 0	
	end if
  end do
  FRACTV = TILV/(TILG2 + TILV)
  CLAI   = FRACTV * CLAIV
  if (LAI <= CLAI) then
    HARVFR = 0.0
  else
    HARVFR = 1.0 - CLAI/LAI
  end if
  HARVLA    = (HARV   * LAI * HARVFR) / DELT
  HARVLV    = (HARV   * CLV * HARVFR) / DELT
  HARVPH    = (HARV   * PHEN        ) / DELT
  TV1       = (HARVFR * FRACTV) + (1-FRACTV)*HAGERE
  HARVRE    = (HARV   * TV1 * CRES  ) / DELT
  HARVST    = (HARV   * CST         ) / DELT
  GSTUB     =  HARVST * (1-HAGERE)
  HARVTILG2 = (HARV   * TILG2       ) / DELT
end Subroutine Harvest

Subroutine Biomass(CLV,CRES,CST)
  real :: CLV, CRES, CST
  CRESMX = COCRESMX*(CLV + CRES + CST)
  RESNOR = max(0.,min(1., CRES/CRESMX ))
end Subroutine Biomass

Subroutine Phenology(DAYL,PHEN, DPHEN,GPHEN)
  real :: DAYL,PHEN
  real :: DPHEN,GPHEN
  GPHEN = max(0., (DAVTMP-0.01)*0.000144*24. * (min(DAYLP,DAYL)-0.24) )
  DPHEN = 0.
  if (DAYL < DAYLB) DPHEN = PHEN / DELT
  PHENRF = (1 - PHEN)/(1 - PHENCR)
  if (PHENRF > 1.0) PHENRF = 1.0
  if (PHENRF < 0.0) PHENRF = 0.0
  DAYLGE = max(0.,min(1., (DAYL - DAYLB)/(DLMXGE - DAYLB) ))
end Subroutine Phenology

Subroutine Foliage1
  real :: EFFTMP, SLAMIN
  EFFTMP = max(TBASE, DAVTMP)
  LERV   =          max(0., (-0.76 + 0.52*EFFTMP)/1000. )
  LERG   = DAYLGE * max(0., (-5.46 + 2.80*EFFTMP)/1000. )
  SLAMIN = SLAMAX * FSLAMIN
  SLANEW = SLAMAX - RESNOR*(SLAMAX-SLAMIN)
end Subroutine Foliage1

Subroutine LUECO2TM(PARAV)
!=============================================================================
! Calculate LUEMXQ (mol CO2 mol-1 PAR quanta)
! Inputs : PARAV (micromol PAR quanta m-2 s-)
!=============================================================================
  real :: PARAV
  real :: CO2I, EA, EAKMC, EAKMO, EAVCMX, EFF, GAMMAX, KC25, KMC, KMC25
  real :: KMO, KMO25, KOKC, O2, PMAX, R, RUBISCN, T, TMPFAC, VCMAX
  T      = DAVTMP                                            !(degC)
  RUBISCN = RUBISC * (1.E6/550000.)
  EAVCMX =  68000                                            !(J mol-1)
  EAKMC  =  65800                                            !(J mol-1)
  EAKMO  =   1400                                            !(J mol-1)
  KC25   =     20                                            !(mol CO2 mol-1 Rubisco s-1)
  KMC25  =    460                                            !(ppm CO2)
  KMO25  =     33                                            !(% O2)
  KOKC   =      0.21                                         !(-)
  O2     =     21                                            !(% O2)
  R      =      8.314                                        !(J K-1 mol-1)
  CO2I   = 0.7 * CO2A                                        !(ppm CO2)
  VCMAX  = RUBISCN * KC25 * exp((1/298.-1/(T+273))*EAVCMX/R) !(micromol CO2 m-2 s-1)
  KMC    =         KMC25 * exp((1/298.-1/(T+273))*EAKMC /R)  !(ppm CO2)
  KMO    =         KMO25 * exp((1/298.-1/(T+273))*EAKMO /R)  !(% O2)
  GAMMAX = 0.5 * KOKC * KMC * O2 / KMO                       !(ppm CO2)
  PMAX   = VCMAX * (CO2I-GAMMAX) / (CO2I + KMC * (1+O2/KMO)) !(micromol CO2 m-2 s-1)
  TMPFAC = max( 0., min( 1., (T+4.)/5. ) )                   !(-)
  EFF    = TMPFAC * (1/2.1) * (CO2I-GAMMAX) / (4.5*CO2I+10.5*GAMMAX) !(mol CO2 mol-1 PAR quanta)
  LUEMXQ = EFF*PMAX*(1+KLUETILG*(1-FRACTV)) / (EFF*K*PARAV + PMAX) !(mol CO2 mol-1 PAR quanta)Aug 8    
end Subroutine LUECO2TM
  
Subroutine HardeningSink(CLV,DAYL,doy,LT50,Tsurf)
  integer :: doy
  real :: CLV,DAYL,LT50,Tsurf
  real :: doySinceStart, reHardRedStart
  reHardRedStart = modulo( reHardRedEnd-reHardRedDay, 365. )
  doySinceStart  = modulo( doy-reHardRedStart       , 365. )
  if ( doySinceStart < (reHardRedDay+0.5*(365.-reHardRedDay)) ) then
    reHardPeriod = max( 0., 1.-doySinceStart/reHardRedDay )
  else
    reHardPeriod = 1.
  end if
  if ( (Tsurf>THARDMX) .or. (LT50<LT50MN) ) then
    RATEH = 0.
  else
    RATEH = reHardPeriod * Hparam * (THARDMX-Tsurf) * (LT50-LT50MN)
  end if
  RESPHARDSI = RATEH * CLV * KRESPHARD * max(0.,min(1., RESNOR*5. ))
end Subroutine HardeningSink

Subroutine Growth(CLV,CRES,CST,PARINT,TILG2,TILV,TRANRF, GLV,GRES,GRT,GST,RESMOB)
  real :: CLV,CRES,CST,PARINT,TILG2,TILV,TRANRF
  real :: GLV,GRES,GRT,GST,RESMOB
  real :: ALLOTOT,CSTAV,GLAISI,GRESSI,SOURCE,SINK1T
  PHOT     = PARINT * TRANRF * 12. * LUEMXQ * NOHARV
  RESMOB   = (CRES * NOHARV / TCRES) * max(0.,min( 1.,DAVTMP/5. ))
  SOURCE   = RESMOB + PHOT
  RESPHARD = min(SOURCE,RESPHARDSI)
  ALLOTOT  = SOURCE - RESPHARD
  GRESSI   = 0.5 * (RESMOB + max(0.,(CRESMX-CRES)/DELT))
  if (TILG2 /= 0.0) then 
    CSTAV  = CST/TILG2 
  else 
    CSTAV  = 0.
  end if
  SINK1T   = max(0., 1 - (CSTAV/CSTAVM)) * SIMAX1T
  NELLVG   = PHENRF * NELLVM 
  GLAISI   = ((LERV*TILV*NELLVM*LFWIDV) + (LERG*TILG2*NELLVG*LFWIDG)) * SHAPE * TRANRF
  GLVSI    = (GLAISI * NOHARV / SLANEW) / YG
  GSTSI    = (SINK1T * TILG2 * TRANRF * NOHARV) / YG
  call Allocation(ALLOTOT,GRESSI, GRES,GRT,GLV,GST)
end Subroutine Growth

   Subroutine Allocation(ALLOTOT,GRESSI, GRES,GRT,GLV,GST)
     real :: ALLOTOT, GRESSI
     real :: GRES, GRT, GLV, GST
     real :: GSHSI, ALLOSH, ALLORT, ALLOLV, ALLOST
     GSHSI = GLVSI + GSTSI
     if (DAYLGE >= 0.1) then
     ! Situation 1: Growth has priority over storage (spring and growth period)
       ! Calculate amount of assimilates allocated to shoot
       ALLOSH = min( ALLOTOT, GSHSI )
       ! Calculate amount of assimilates allocated to reserves    
       GRES   = min( ALLOTOT - ALLOSH, GRESSI)
     else
     ! Situation 2: Storage has priority over shoot (autumn)
       ! Calculate amount of assimilates allocated to reserves
       GRES   = min( ALLOTOT, GRESSI )
       ! Calculate amount of assimilates allocated to shoot
       ALLOSH = min( ALLOTOT - GRES, GSHSI )
     end if
     ! All surplus carbohydrate goes to roots
     ALLORT  = ALLOTOT - ALLOSH - GRES
       if (GSHSI == 0.) GSHSI = 1
     ALLOLV  = GLVSI * (ALLOSH / GSHSI)
     ALLOST  = GSTSI * (ALLOSH / GSHSI)
     GLV     = ALLOLV * YG
     GST     = ALLOST * YG
     GRT     = ALLORT * YG
     RESPGSH = (ALLOLV + ALLOST) * (1-YG)
     RESPGRT =  ALLORT           * (1-YG)
   end Subroutine Allocation
    
Subroutine PlantRespiration(FO2,RESPHARD)
  real :: FO2,RESPHARD
  real :: fAer
  fAer      = max(0.,min(1., FO2/FO2MX ))
  RplantAer = fAer * ( RESPGRT + RESPGSH + RESPHARD )
end Subroutine PlantRespiration

Subroutine Senescence(CLV,CRT,CSTUB,doy,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                                 DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
  integer :: doy
  real :: CLV,CRT,CSTUB,DAYL,LAI,LT50,PERMgas,TANAER,TILV,Tsurf
  real :: DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate
  real :: RDRS, TV1, TV2
  call AnaerobicDamage(LT50,PERMgas,TANAER, dTANAER)
  call Hardening(CLV,LT50,Tsurf, DeHardRate,HardRate)
  if (LAI < LAICR) then
    TV1 = 0.0 
  else 
    TV1 = RDRSCO*(LAI-LAICR)/LAICR
  end if
  RDRS   = min(TV1, RDRSMX)
  RDRT   = max(RDRTMIN, RDRTEM * Tsurf)
  TV2    = NOHARV * max(RDRS,RDRT,RDRFROST,RDRTOX)
  TV2TIL = NOHARV * max(RDRS,     RDRFROST,RDRTOX)
  DLAI   = LAI    * TV2
  DLV    = CLV    * TV2
  DSTUB  = CSTUB  * RDRSTUB
  DTILV  = TILV   * TV2TIL
  DRT    = CRT    * RDRROOT

end Subroutine Senescence

   Subroutine AnaerobicDamage(LT50,PERMgas,TANAER, dTANAER)
     real :: LT50,PERMgas,TANAER
     real :: dTANAER,LD50
     if (PERMgas==0.) then
       dTANAER = 1.
     else
       dTANAER = -TANAER / DELT
     end if
     LD50 = LDT50A + LDT50B * LT50
     if (TANAER > 0.) then
       RDRTOX = KRDRANAER / (1.+exp(-KRDRANAER*(TANAER-LD50)))
     else
       RDRTOX = 0.
     end if
     end Subroutine AnaerobicDamage

   Subroutine Hardening(CLV,LT50,Tsurf, DeHardRate,HardRate)
     real :: CLV,LT50,Tsurf
     real :: DeHardRate,HardRate
     real :: RATED,RSR3H,RSRDAY
     RSR3H      = 1. / (1.+exp(-KRSR3H*(Tsurf-LT50)))
     ! RDRFROST should be less than 1 to avoid numerical problems
     ! (loss of all biomass but keeping positive reserves). We cap it at 0.5.
     RSRDAY     = RSR3H ! In previous versions we had RSRDAY = RSR3H^8 which understimated survival
     RDRFROST   = min( 0.5, 1. - RSRDAY )
     RATED      = min( Dparam*(LT50MX-LT50)*(Tsurf+TsurfDiff), (LT50MX-LT50)/DELT )
     DeHardRate = max(0.,min( RATEDMX, RATED ))
     HardRate   = RESPHARD / (CLV * KRESPHARD)
   end Subroutine Hardening

!Subroutine Foliage2(GLV,LAI,TRANRF,Tsurf, GLAI,RGRTV,RGRTVG)
!Subroutine Foliage2(GLV,LAI,TRANRF,Tsurf, GLAI,RGRTV,RGRTVG)
Subroutine Foliage2(DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf,VERN, GLAI,GTILV,TILVG1,TILG1G2)
  real    :: DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf
  integer :: VERN
  real    :: GLAI,GTILV,TILVG1,TILG1G2
  real    :: RGRTV,RGRTVG1,TGE,TV1,TV2
  GLAI    = SLANEW * GLV
  if (Tsurf < TBASE) then 
    TV1   = 0. 
  else 
    TV1   = Tsurf/PHY
  end if
  RLEAF   = TV1 * NOHARV * TRANRF * DAYLGE * ( FRACTV + PHENRF*(1-FRACTV) )
  TV2     = LAITIL - LAIEFT*LAI
    if (TV2 > FSMAX) TV2 = FSMAX
    if (TV2 < 0.)   TV2 = 0.
  RGRTV   = max( 0.       , TV2 * RESNOR * RLEAF )
  GTILV   = TILV  * RGRTV
  TGE     = max( 0.       , 1 - (abs(DAVTMP - TOPTGE))/(TOPTGE-TBASE))
  RGRTVG1 = min( 1.-TV2TIL, NOHARV * DAYLGE * TGE * RGENMX ) * VERN
  TILVG1  = TILV  * RGRTVG1
  if (DAYL > DAYLG1G2) then
    TILG1G2 = TILG1 * RGRTG1G2
  else
    TILG1G2 = 0.
  end if
end Subroutine Foliage2

end module plant
