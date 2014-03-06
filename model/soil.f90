module soil

use parameters_site
use parameters_plant
use environment
implicit none

real :: FO2, fPerm, Tsurf, WCL

real :: dCLITT, rCLITT, rCSOMF, Rsoil
real :: dCLITTrsoil, dCLITTsomf, dCSOMF, dCSOMFrsoil, dCSOMFsoms, dCSOMS
real :: Nemission, NemissionN2O, NemissionNO, Nfixation, Nleaching
real :: NLITTnmin, NLITTsomf, Nmineralisation
real :: dNLITT, dNSOMF, dNSOMS, NSOMFnmin, NSOMFsoms, rNLITT, rNSOMF
real :: Tsoil, fTsoil, Thist(10)

Contains

Subroutine SoilWaterContent(Fdepth,ROOTD,WAL)
  real :: Fdepth,ROOTD,WAL
  if (Fdepth < ROOTD) then
    WCL = WAL*0.001 / (ROOTD-Fdepth)
  else
    WCL = 0
  end if
end Subroutine SoilWaterContent

Subroutine Physics(DAVTMP,Fdepth,ROOTD,Sdepth,WAS, Frate)
  real :: DAVTMP,Fdepth,ROOTD,Sdepth,WAS
  real :: Frate
!  if (Fdepth == 0.) then
!    Tsurf = DAVTMP * exp(-gamma*Sdepth)
!    fPerm = 1.
!  else
!    Tsurf = DAVTMP / (1. + 10. * (Sdepth / Fdepth) )
!    fPerm = 0.
!  end if
  if (Fdepth > 0.) then
    Tsurf = DAVTMP / (1. + 10. * (Sdepth / Fdepth) )
    fPerm = 0.
  else
    Tsurf = DAVTMP * exp(-gamma*Sdepth)
    fPerm = 1.
  end if  
  call Frozensoil(Fdepth,ROOTD,WAS, Frate)
end Subroutine Physics
  
   Subroutine FrozenSoil(Fdepth,ROOTD,WAS, Frate)
     real :: Fdepth,ROOTD,WAS
     real :: Frate
     real :: alpha, PFrate, WCeff
     ! Determining the amount of solid water that contributes in transportation of heat to surface 'WCeff'
     if (Fdepth > ROOTD) then
       WCeff = WCFC
     else if (Fdepth > 0.) then
       WCeff = (0.001*WAS) / Fdepth
     else
       WCeff = WCL
     end if
     ! Calculating potential frost rate 'PFrate'
     if ((Fdepth == 0.).and.(Tsurf>0.)) then ! No soil frost present AND no frost starting
       PFrate = 0.
     else
       alpha  = LAMBDAsoil / ( RHOwater * WCeff * LatentHeat )
       PFrate = Sqrt( max(0.,Fdepth**2 - 2.*alpha*Tsurf) ) - Fdepth
     end if
     if ((PFrate >= 0.).and.(Fdepth > 0.).and.(Fdepth < ROOTD)) then
       Frate = PFrate * (0.001*WAS/Fdepth) / WCFC ! Soil frost increasing
     else if ((PFrate+Fdepth/DELT) < 0.) then
       Frate = -Fdepth / DELT                     ! Remaining soil frost thaws away
     else
       Frate = PFrate
     end if
   end Subroutine FrozenSoil

Subroutine FRDRUNIR(EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS, &
                                               DRAIN,FREEZEL,IRRIG,RUNOFF,THAWS)
  real :: EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS
  real :: DRAIN,FREEZEL,IRRIG,RUNOFF,THAWS
  real :: INFILTOT,WAFC,WAST
#ifdef winter  
  WAFC   = 1000. * WCFC * max(0.,(ROOTD-Fdepth))                      ! (mm)
  WAST   = 1000. * WCST * max(0.,(ROOTD-Fdepth))                      ! (mm)
  INFILTOT = INFIL + poolDrain
  if (Fdepth < ROOTD) then
    FREEZEL = max(0., min( WAL/DELT + (INFILTOT - EVAP - TRAN), &
                         (Frate/(ROOTD-Fdepth))*WAL))                 ! (mm d-1)
  else
    FREEZEL = 0.                                                      ! (mm d-1)
  end if
  if ((Fdepth > 0.) .and. (Fdepth <= ROOTD)) then
    THAWS   = max(0.,min( WAS/DELT, -Frate*WAS/Fdepth ))              ! (mm d-1)
  else
    THAWS   = 0.                                                      ! (mm d-1)
  end if
  DRAIN  = max(0.,min( DRATE, (WAL-WAFC)/DELT + &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS) ))                ! (mm d-1)
  RUNOFF = max(0.,            (WAL-WAST)/DELT + &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS - DRAIN) )         ! (mm d-1)
  IRRIG  = IRRIGF *  (        (WAFC-WAL)/DELT - &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS - DRAIN - RUNOFF)) ! (mm d-1)
#else
  WAFC = 1000. * WCFC * ROOTD                                    !(mm)
  WAST = 1000. * WCST * ROOTD                                    !(mm)
  DRAIN  = max(0., min(DRATE, (WAL-WAFC)/DELT + (RAIN-RNINTC-EVAP-TRAN)))     !(mm d-1)
  RUNOFF = max(0., (WAL-WAST)/DELT + (RAIN-RNINTC-EVAP-TRAN-DRAIN))           !(mm d-1)
  IRRIG  = IRRIGF * ((WAFC-WAL)/DELT - (RAIN-RNINTC-EVAP-TRAN-DRAIN-RUNOFF)) !(mm d-1)
#endif
end Subroutine FRDRUNIR

Subroutine O2status(O2,ROOTD)
  real :: O2,ROOTD
  FO2 = O2 / (ROOTD * FGAS * 1000./22.4)
end Subroutine O2status
  
Subroutine O2fluxes(O2,PERMgas,ROOTD,RplantAer, O2IN,O2OUT)
  real :: O2,PERMgas,ROOTD,RplantAer
  real :: O2IN,O2OUT
  real :: O2MX
  O2OUT = RplantAer * KRTOTAER * 1./12. * 1.
  O2MX  = FO2MX * ROOTD * FGAS * 1000./22.4
  O2IN  = PERMgas * ( (O2MX-O2) + O2OUT*DELT )  
end Subroutine O2fluxes

  Subroutine CNsoil(RWA,WFPS,WAL,CLV,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS,ROOTD,DRAIN,RUNOFF,FMIN)
  real :: CLITT, CSOMF, CSOMS, fN2O, CLV, NLITT, NMIN, NSOMF, NSOMS
  real :: RWA, WAL, WFPS, ROOTD, DRAIN, RUNOFF
  real :: FMIN
  
  ! C Litter
  rCLITT      = (((CLITT/1000.)*Runoff) / ROOTD) * RRUNBULK * 0.001
  dCLITT      =  ((CLITT/1000.)*fTsoil) / TCLITT
  dCLITTsomf  = FLITTSOMF * dCLITT
  dCLITTrsoil = dCLITT - dCLITTsomf

  ! C SOM fast
  rCSOMF      = ((CSOMF*Runoff) / ROOTD) * RRUNBULK * 0.001
  dCSOMF      =  (CSOMF*fTsoil) / TCSOMF
  dCSOMFsoms  = FSOMFSOMS * dCSOMF
  dCSOMFrsoil = dCSOMF - dCSOMFSOMS
  
  ! C SOM slow
  dCSOMS      = (CSOMS*fTsoil) / TCSOMS

  ! Respiration
  Rsoil       = dCLITTrsoil + dCSOMFrsoil + dCSOMS

  ! N Litter
  rNLITT      = ((NLITT*Runoff) / ROOTD) * RRUNBULK * 0.001
  dNLITT      =  (NLITT*dCLITT) / CLITT
  NLITTsomf   = dNLITT * FLITTSOMF
  NLITTnmin   = dNLITT - NLITTsomf

  ! N SOM fast
  rNSOMF      = ((NSOMF*Runoff) / ROOTD) * RRUNBULK * 0.001
  dNSOMF      =  (NSOMF*dCSOMF) / CSOMF
  NSOMFsoms   = dNSOMF * FSOMFSOMS
  NSOMFnmin   = dNSOMF - NSOMFsoms

  ! N SOM slow
  dNSOMS      = (NSOMS*dCSOMS) / CSOMS

  ! N mineralisation, fixation, leaching, emission
  Nmineralisation = NLITTnmin + NSOMFnmin + dNSOMS
  !Nfixation       = GRT * KNFIX 
  !Nfixation       = CLV * KNFIX 
  Nfixation       = (KNFIXMX/(10000.*365.))*EXP(-1.0*KNFIXK*NMIN)
  !Nfixation = 0.0
  Nleaching       = (NMIN*RNLEACH*DRAIN) / WAL
  Nemission       =  NMIN * KNEMIT * RWA + FMIN*KFERTEMIT
  fN2O            = 1. / (1. + exp(-RFN2O*(WFPS-WFPS50N2O)))
  NemissionN2O    = Nemission *     fN2O
  NemissionNO     = Nemission * (1.-fN2O) 

  end Subroutine CNsoil

!  Subroutine Tsoil_calc(iday)
  Subroutine Tsoil_calc
  integer iday,i
  Tsoil=0.
  do i=1,10
    Tsoil=Tsoil+Thist(i)/10.
  enddo
  do i=10,2,-1
    Thist(i)=Thist(i-1)
  enddo
!  Thist(1)=T(iday) 
  Thist(1)= DAVTMP
  fTsoil = exp((Tsoil-10.)*(2.*TMAXF-Tsoil-10.)/(2.*TSIGMAF**2.)) 
  end Subroutine Tsoil_calc 

end module soil
