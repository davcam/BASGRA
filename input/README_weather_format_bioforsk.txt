The weather files were produced by Bioforsk Norway. 
Weather files using the Bioforsk format have the following columns:

! ST   : station number
! YR   : year
! doy  : day
! T    : mean    temperature (degrees Celsius)								
! TMMXI: maximum temperature (degrees Celsius)								
! TMMNI: minimum temperature (degrees Celsius)								
! RH   : relative humidity   (%)
! RAINI: precipitation       (mm d-1)	
! WNI  : mean wind speed     (m s-1)								
! GR   : irradiation         (MJ m-2 d-1)

The fortran subroutine written for BASGRA that can read these files is called:
  'read_weather.f90'
ATTENTION that subroutine has two branches, and depending on the way BASFOR is
compiled, one or the other is selected. The Bioforsk format requires
compilation WITHOUT the 'Dweathergen' switch.