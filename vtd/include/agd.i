!
! >>>>> Include block with antenna gravity deformation infromation.
! >>>>> Used for computing position variations caused by atnenna gravity deformations
! >>>>> 2008.04.25  (c)  L. Petrov  v 1.0  2008.04.25_09:12:59
!
      CHARACTER  AGD__LABEL*64
      PARAMETER  ( AGD__LABEL = '$ Antenna Gravity Deformation File  Format Version of 2008.04.25' )
!
      TYPE AGD_INFO__TYPE
	   CHARACTER  NAME*8
	   INTEGER*4  N_POI
	   INTEGER*4  FOCUS_TYPE
	   REAL*8,    POINTER :: ELEV(:)
	   REAL*8,    POINTER :: FOC_LEN(:)
	   REAL*8,    POINTER :: FOC_SPL(:)
      END TYPE AGD_INFO__TYPE
!
      TYPE AGD_DATA__TYPE
           INTEGER*4  N_ANT
           TYPE ( AGD_INFO__TYPE ), POINTER :: INFO(:)
	   CHARACTER, POINTER ::  STA_NAM(:)*8
!
           CHARACTER  FILENAME*128
           INTEGER*4  STATUS
      END  TYPE AGD_DATA__TYPE
!
      INTEGER*4    AGD__M_STA, AGD__M_POI
      PARAMETER  ( AGD__M_STA =   256 )
      PARAMETER  ( AGD__M_POI = 16384 )
      INTEGER*4    AGD__FO_PRIM, AGD__FO_SECN
      CHARACTER    AGD__FO_PRIM_CH*7, AGD__FO_SECN_CH*7
      PARAMETER  ( AGD__FO_PRIM = 34001 )
      PARAMETER  ( AGD__FO_SECN = 34002 )
      PARAMETER  ( AGD__FO_PRIM_CH = 'FO_PRIM' )
      PARAMETER  ( AGD__FO_SECN_CH = 'FO_SECN' )
!
      INTEGER*4    AGD__UNDF, AGD__LOAD
      PARAMETER  ( AGD__UNDF =          0 ) ! Undefined
      PARAMETER  ( AGD__LOAD = 2029810237 ) ! Loaded
!
! >>>>> End of include block with antenna infromation
!
