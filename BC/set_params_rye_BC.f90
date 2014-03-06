Subroutine set_params_BC(pa)

use parameters_site
use parameters_plant
implicit none
! real :: pa(25)
real :: pa(51)

! initial values
CLVI      = pa(1)
CRESI     = pa(2)
CRTI      = pa(3)
LAII      = pa(4)
TILVI     = pa(5) 
! Process parameters 
CLAIV     = pa(6)	   
COCRESMX  =	pa(7)
FSLAMIN   = pa(8)	 
K         =	pa(9)  
LAICR	  = pa(10)	 
LAIEFT    = pa(11)	   
LAITIL	  =	pa(12) 
LFWIDV	  = pa(13) 
PHY	      =	pa(14)	  
RDRSCO	  =	pa(15)
RDRSMX	  = pa(16)	 
RDRTEM    = pa(17) 
RUBISC    = pa(18)	   
SHAPE	  =	pa(19)	  
SLAMAX    = pa(20)
TBASE     = pa(21)	   
TCRES     = pa(22)	  
TRANCO	  = pa(23)
YG        = pa(24)
DAYLB     = pa(25)

CLITT0	  = pa(26)
CSOM0	  = pa(27)
CNLITT0   = pa(28)
CNSOMF0   = pa(29)
CNSOMS0   = pa(30)
FCSOMF0	  = pa(31)
NMIN0	  = pa(32)
NC        = pa(33)
KNMIN	  = pa(34)
KNUPT	  = pa(35)
FLITTSOMF  = pa(36)
FSOMFSOMS  = pa(37)
RNLEACH	  = pa(38)
KNEMIT    = pa(39)
TCLITT    = pa(40)
TCSOMF    = pa(41)
TCSOMS    = pa(42)
TMAXF     = pa(43)
TSIGMAF   = pa(44)
RFN2O     = pa(45)
WFPS50N2O  = pa(46)
KNFIXMX     = pa(47)
KNFIXK     = pa(48)
KFERTEMIT     = pa(49)
RDRROOT	= pa(50)
DLMXGE	= pa(51)

return
end