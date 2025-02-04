!
! >> include-block for harmonic eop variations
! >> L. Petrov  2003.09.30   v 3.0     2007.08.23_17:04:46
!
        INTEGER*4  M__HEO
        PARAMETER  ( M__HEO  =  4096 )
	INTEGER*4    HEO__PMC, HEO__PMS, HEO__E3C, HEO__E3S, HEO__ANG, HEO__VEL
	PARAMETER  ( HEO__PMC = 1 )
	PARAMETER  ( HEO__PMS = 2 )
	PARAMETER  ( HEO__E3C = 3 )
	PARAMETER  ( HEO__E3S = 4 )
	PARAMETER  ( HEO__ANG = 1 )
	PARAMETER  ( HEO__VEL = 2 )
	TYPE  HEO__STRUC
	      REAL*8     PHAS
	      REAL*8     FREQ
	      REAL*8     ACCL
	      REAL*8     ROTANG(4,2)
	      REAL*8     ROTANG_ERR(4,2)
	      CHARACTER  WAVE*8
	      LOGICAL*4  USE_VEL
	      LOGICAL*4  USE_ERR
!
	      INTEGER*4  LAST_FIELD ! Because of a bug in Sun compiler we need this field
        END TYPE HEO__STRUC
!
	CHARACTER  HEO__LABEL_NOERR*34
	CHARACTER  HEO__LABEL*34
        PARAMETER  ( HEO__LABEL_NOERR = 'HEO  Format version of 2004.03.12 ' )
        PARAMETER  ( HEO__LABEL = 'HEO  Format version of 2007.08.23 ' )
!
! ----- Commands for harmonic Earth orientation
!
        INTEGER*4    HEO__UNDF, HEO__REQUE, HEO__ALLOC, HEO__READ
        PARAMETER  ( HEO__UNDF  = 0    ) ! Harmonic EOP were not defined
        PARAMETER  ( HEO__REQUE = 4001 ) ! Harmonic EOP were requested
        PARAMETER  ( HEO__ALLOC = 4002 ) ! Arrays for harmonic EOP were allocated
        PARAMETER  ( HEO__READ  = 4003 ) ! Arrays for harmonic EOP were read
!
! --- Constants for nutation models
!
        INTEGER*4    NUT__UNDF, NUT__WAHR1980, NUT__IERS1996, NUT__REN2000, &
     &               NUT__MHB2000, NUT__MHB2000_TRANSF, NUT__MHB2000_ADDON, &
     &               NUT__PETA, NUT__PETB, NUT__PETC
        PARAMETER  ( NUT__UNDF     = 0    )
        PARAMETER  ( NUT__WAHR1980 = 5001 )
        PARAMETER  ( NUT__IERS1996 = 5002 )
        PARAMETER  ( NUT__REN2000  = 5003 )
        PARAMETER  ( NUT__MHB2000  = 5004 )
        PARAMETER  ( NUT__MHB2000_TRANSF = 5005 )
        PARAMETER  ( NUT__MHB2000_ADDON  = 5006 )
        PARAMETER  ( NUT__PETA     = 5007 )
        PARAMETER  ( NUT__PETB     = 5008 )
        PARAMETER  ( NUT__PETC     = 5009 )
!
! << end of include block heo.i  for harmonic eop variations
!
