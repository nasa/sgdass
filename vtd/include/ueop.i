!
! >>>>> Include block for using EOP
! >>>>> 2004.01.25  (c)  L. Petrov  v 1.1  2018.04.03_11:59:01
!
      INTEGER*4    UEOP__MP
      PARAMETER  ( UEOP__MP = 32768 )
      INTEGER*4    UEOP__XPL, UEOP__YPL, UEOP__UT1_M_TAI
      PARAMETER  ( UEOP__XPL = 1       )
      PARAMETER  ( UEOP__YPL = 2       )
      PARAMETER  ( UEOP__UT1_M_TAI = 3 )
      TYPE       UEOP__TYPE
            REAL*8      TIM(UEOP__MP)
            REAL*8      VAL(UEOP__MP,3)
            REAL*8      SPL(UEOP__MP,3)
	    INTEGER*4   NP
!		    
	    INTEGER*4   MJD_BEG
	    REAL*8      TAI_BEG
	    INTEGER*4   MJD_END
	    REAL*8      TAI_END
!
            INTEGER*4   STATUS
      END TYPE   UEOP__TYPE
      INTEGER*4  UEOP__LOADED
      PARAMETER  ( UEOP__LOADED = 1823634073 )
!
! >>>>> End of include block for using EOP
!
