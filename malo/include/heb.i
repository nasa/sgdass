!
! >>>>> heb.i  2013.01.28  version 1.4   of 2016.01.04
!
        INTEGER*4  HEB__MC
        PARAMETER  ( HEB__MC = 4 )
        TYPE     HEB__TYPE
            CHARACTER  SDS_NAME*1024
            CHARACTER  UNITS*16
            CHARACTER  PROD_NAME*128
            CHARACTER  FILE_NAME*128
            CHARACTER  HISTORY*128
            CHARACTER  SOURCE*128
            CHARACTER  TITLE*128
            CHARACTER  INSTITUTION*128
            CHARACTER  REFERENCES*128
            CHARACTER  PROD_DATE_TIME*128
            CHARACTER  VERSION_ID*32
            CHARACTER  COMMENT(HEB__MC)*128
            INTEGER*4  MJD
            REAL*8     UTC
            REAL*8     TAI
            INTEGER*8  DIMS(4)
            INTEGER*8  DATA_OFFSET
            INTEGER*8  DATA_LENGTH
            CHARACTER  DATA_FORMAT*4
            CHARACTER  DATA_TRANSFORM*4
            CHARACTER  ENDIAN*4
            REAL*4     MIN_VALUE
            REAL*4     MAX_VALUE
            REAL*4     VALID_RANGE(2)
            REAL*4     FILL_VALUE
            REAL*4     OFFSET
            REAL*4     SCALE_FACTOR
            CHARACTER  DATA_COMPRESSION*4
            INTEGER*1, POINTER :: VAL1(:,:,:,:) => NULL()
            INTEGER*2, POINTER :: VAL2(:,:,:,:) => NULL()
            REAL*4,    POINTER :: VAL(:,:,:,:)  => NULL()
            REAL*8,    POINTER :: VAL8(:,:,:,:) => NULL()
            REAL*4,    POINTER :: ARR_LEV(:)    => NULL()
            REAL*4,    POINTER :: ARR_LON(:)    => NULL()
            REAL*4,    POINTER :: ARR_LAT(:)    => NULL()
            REAL*4,    POINTER :: ARR_TIM(:)    => NULL()
	    INTEGER*4  STATUS
        END TYPE HEB__TYPE
!
        INTEGER*4   HEB__UNDF, HEB__ALLO, HEB__LOAD, HEB__HDLO, HEB__HDON
        CHARACTER   HEB__NONE*4, HEB__GZ*4, HEB__I1*2, &
     &              HEB__I2*2, HEB__I4*2, HEB__I8*2, &
     &              HEB__R4*2, HEB__R8*2, HEB__LE*2, &
     &              HEB__BE*2, HEB__SCOF*4, HEB__LOG*4
        PARAMETER  ( HEB__UNDF = 0 )
	PARAMETER  ( HEB__ALLO = 120381231 )
	PARAMETER  ( HEB__LOAD = 192839012 )
	PARAMETER  ( HEB__HDLO = 158920924 )
	PARAMETER  ( HEB__HDON = 192784892 )
        PARAMETER  ( HEB__NONE = 'none' )
        PARAMETER  ( HEB__SCOF = 'scof' )
        PARAMETER  ( HEB__LOG  = 'log ' )
        PARAMETER  ( HEB__GZ   = 'gz  ' )
        PARAMETER  ( HEB__I1   = 'I1  ' )
        PARAMETER  ( HEB__I2   = 'I2  ' )
        PARAMETER  ( HEB__I4   = 'I4  ' )
        PARAMETER  ( HEB__I8   = 'I8  ' )
	PARAMETER  ( HEB__R4   = 'R4  ' )
	PARAMETER  ( HEB__R8   = 'R8  ' )
	PARAMETER  ( HEB__LE   = 'LE  ' )
	PARAMETER  ( HEB__BE   = 'BE  ' )
	INTEGER*8    HEB__DATA_CHUNK
        PARAMETER  ( HEB__DATA_CHUNK = 128*1024*1024 )
!
	INTEGER*4    HEB__HDS, HEB__HDS_V1
        PARAMETER  ( HEB__HDS_V1 = 2048 )
        PARAMETER  ( HEB__HDS = 2048 )
	CHARACTER    HEB__LABEL_V1*32, HEB__LABEL*32
        PARAMETER  ( HEB__LABEL_V1 = 'HEB Format version of 2013.01.30' )
        PARAMETER  ( HEB__LABEL = 'HEB Format version of 2013.01.30' )
!
! <<<<  end of eflux.i
