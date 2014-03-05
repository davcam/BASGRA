#ifdef weathergen
  Subroutine read_weather(weatherfile,year_start,doy_start,NDAYS)
  !=============================================================================
  ! Reads weather data from a file with the following entries at EVERY line
  ! ST   : station number
  ! year : year
  ! doy  : day
  ! TMMNI: minimum temperature          (degrees Celsius)
  ! TMMXI: maximum temperature          (degrees Celsius)
  ! PREC : precipitation                (mm d-1)
  ! GR   : irradiation                  (MJ m-2 d-1)
  ! PETI : potential evapotranspiration (mm d-1)
  ! One unit conversion is made to create the following output:
  ! RDDI : kJ m-2 d-1
  !=============================================================================
    use environment
    integer :: i, ST, year, doy, doy_start, NDAYS, year_start
    real    :: PREC, GR
    character(132) :: weatherfile
    open(unit=1,file=weatherfile,status='old')
    read(1,*) ! skip header
    read(1,*) ST,YEARI(1),DOYI(1),TMMNI(1),TMMXI(1),PREC,GR,PETI(1)
    do while ((YEARI(1)<year_start).or.(DOYI(1)<doy_start))
      read(1,*) ST,YEARI(1),DOYI(1),TMMNI(1),TMMXI(1),PREC,GR,PETI(1)
    end do
    RAINI(1)  = max(0.,PREC)
    RDDI(1)   = GR * 1000
    do i=2,NDAYS
      read(1,*) ST,YEARI(i),DOYI(i),TMMNI(i),TMMXI(i),PREC,GR,PETI(i)
      RAINI(i) = max(0.,PREC)
      RDDI(i)  = GR * 1000
    end do
    close(1)
  end Subroutine read_weather
#else
  Subroutine read_weather(weatherfile,year_start,doy_start,NDAYS)
  !=============================================================================
  ! Reads weather data from a file with the following entries at EVERY line
  ! ST   : station number
  ! year : year
  ! doy  : day
  ! T    : mean    temperature (degrees Celsius)								
  ! TMMXI: maximum temperature (degrees Celsius)								
  ! TMMNI: minimum temperature (degrees Celsius)								
  ! RH   : relative humidity   (%)
  ! PREC : precipitation       (mm d-1)	
  ! WNI  : mean wind speed     (m s-1)								
  ! GR   : irradiation         (MJ m-2 d-1)
  ! Two unit conversions are made to create the following outputs:
  ! RDDI : kJ m-2 d-1
  ! VPI  : kPa
  !=============================================================================
    use environment
    integer :: i, ST, year, doy, doy_start, NDAYS, year_start
    real    :: T, PREC, RH, GR
    character(132) :: weatherfile
    open(unit=1,file=weatherfile,status='old')
    read(1,*) ! skip header
    read(1,*) ST,YEARI(1),DOYI(1),T,TMMXI(1),TMMNI(1),RH,PREC,WNI(1),GR
    do while ((YEARI(1)<year_start).or.(DOYI(1)<doy_start))
      read(1,*) ST,YEARI(1),DOYI(1),T,TMMXI(1),TMMNI(1),RH,PREC,WNI(1),GR
    end do
    ! In the following two lines the maximum and minimum temperature are set
    ! equal to the average because the model later uses subroutine set_weather_day
    ! to recalculate daily mean temperature as the average of maximum and minimum.
    TMMXI(1)  = T
    TMMNI(1)  = T
    VPI(1)    = 0.6108 * exp(17.27*T/(T+239)) * RH / 100
    RAINI(1)  = max(0.,PREC)
    RDDI(1)   = GR * 1000
    do i=2,NDAYS
      read(1,*) ST,YEARI(i),DOYI(i),T,TMMXI(i),TMMNI(i),RH,PREC,WNI(i),GR
      TMMXI(i) = T
      TMMNI(i) = T
      VPI(i)   = 0.6108 * exp(17.27*T/(T+239)) * RH / 100
      RAINI(i) = max(0.,PREC)
      RDDI(i)  = GR * 1000
    end do
    close(1)
  end Subroutine read_weather
#endif