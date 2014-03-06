Subroutine read_weather_oensingen(weatherfile,doy_start,NDAYS)
!=============================================================================
! Reads weather data from a file with the following entries at EVERY line
! YR   : year
! doy  : day
! TMMXI: maximum temperature (degrees Kelvin)								
! TMMNI: minimum temperature (degrees Kelvin)								
! RAINI: precipitation       (mm d-1)	
! GR   : irradiation         (MJ m-2 d-1)
! WNI  : mean wind speed     (m s-1)								
! VPI  : Vapour Pressure     (kPa)
! Two unit conversions are made to create the following outputs:
! RDDI : kJ m-2 d-1
!=============================================================================
  use environment
  integer :: i, doy, doy_start, NDAYS
  integer :: extradays
  real    :: T, RH, GR, dummy
  character(132) :: weatherfile
  open(unit=1,file=weatherfile,status='old')
  ! read file header
  do i=1,3
    read(1,*)
  enddo 
  read(1,*) YEARI(1),DOYI(1),TMMNI(1),TMMXI(1),RAINI(1),GR,WNI(1),VPI(1),dummy
  do while (DOYI(1)<doy_start)
    read(1,*) YR1,DOYI(1),TMMNI(1),TMMXI(1),RAINI(1),GR,WNI(1),VPI(1),dummy
  end do
  ! In the following two lines the maximum and minimum temperature are set
  ! equal to the average because the model later uses subroutine set_weather_day
  ! to recalculate daily mean temperature as the average of maximum and minimum.
  TMMNI(1) = TMMNI(1) - 273. 
  TMMXI(1) = TMMXI(1) - 273. 
  RDDI(1)   = GR * 1000
  !extradays = 0
  do i=2,NDAYS
    read(1,*) YEARI(i),DOYI(i),TMMNI(i),TMMXI(i),RAINI(i),GR,WNI(i),VPI(i),dummy
    !if ( (YR>YR1) .and. (extradays==0) ) extradays = DOYI(i-1)
    !DOYI(i)  = DOYI(i) + extradays
    TMMNI(i) = TMMNI(i) - 273. 
    TMMXI(i) = TMMXI(i) - 273. 
    RDDI(i)  = GR * 1000
  end do
  close(1)
end Subroutine read_weather_oensingen
