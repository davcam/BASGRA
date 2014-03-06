Subroutine set_params(hd,fd,fm,fo,pa)

use parameters_site
use parameters_plant
implicit none
integer, dimension(24) :: hd
integer, dimension(24) :: fd
real,    dimension(24) :: fm,fo
real                  :: pa(76)

! Management
doyHA     = hd
doyFERT   = fd
FERTMIN   = fm
FERTORG   = fo

! Initial constants
CLVI	  = pa(1)
CRESI     = pa(2)
CRTI	  = pa(3)
LAII      = pa(4)	   
TILVI	  = pa(5)

! Process parameters 
CLAIV     = pa(6)	   
COCRESMX  = pa(7)
FSLAMIN   = pa(8)	 
FSMAX     = pa(9)  
HAGERE    = pa(10)	  
K         = pa(11)  
LAICR	  = pa(12)	 
LAIEFT    = pa(13)	   
LAITIL	  = pa(14) 
LFWIDV	  = pa(15) 
PHY	  = pa(16)	  
RDRSCO	  = pa(17)  
RDRSMX	  = pa(18)	 
RDRTEM    = pa(19) 
RUBISC    = pa(20)	   
SHAPE	  = pa(21)	  
SLAMAX    = pa(22)
TBASE     = pa(23)	   
TCRES     = pa(24)	  
TRANCO	  = pa(25)	 
YG        = pa(26)

LAT       = pa(27)
WCI       = pa(28)
WCAD      = pa(29)
WCWP      = pa(30)
WCFC      = pa(31)
WCWET     = pa(32)
WCST      = pa(33)

! Non ryegrass parameters
CSTI	  = pa(34)
PHENI	  = pa(35) 
TILGI	  = pa(36) 
LT50I     = pa(37)
CSTAVM	  = pa(38)
DAYLB	  = pa(39)  
DAYLP	  = pa(40)
DLMXGE	  = pa(41)
LFWIDG	  = pa(42)
NELLVM	  = pa(43)	 
PHENCR    = pa(44)	  
RGENMX	  = pa(45)  
ROOTDM	  = pa(46)	  
RRDMAX	  = pa(47)
SIMAX1T	  = pa(48)
TOPTGE	  = pa(49)	  


WpoolMax  = pa(50)

Dparam	     = pa(51)			
FGAS	     = pa(52)			
FO2MX	     = pa(53)			
gamma	     = pa(54)			
Hparam	     = pa(55)			
KRDRANAER    = pa(56)			
KRESPHARD    = pa(57)			
KRSR3H	     = pa(58)			
KRTOTAER     = pa(59)			
KSNOW	     = pa(60)			
LAMBDAsoil   = pa(61)			
LDT50A	     = pa(62)			
LDT50B	     = pa(63)			
LT50MN	     = pa(64)			
LT50MX	     = pa(65)			
RATEDMX	     = pa(66)			
reHardRedDay = pa(67)			
RHOnewSnow   = pa(68)			
RHOpack	     = pa(69)			
SWret	     = pa(70)			
SWrf	     = pa(71)			
THARDMX	     = pa(72)			
TmeltFreeze  = pa(73)			
TrainSnow    = pa(74)			
TsurfDiff    = pa(75)
KLUETILG     = pa(76)

return
end
