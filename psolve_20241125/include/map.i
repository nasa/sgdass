!
      INTEGER*4    MAP__UNDF, MAP__LOADED
      PARAMETER  ( MAP__UNDF   =     0 )
      PARAMETER  ( MAP__LOADED = 13001 )
      TYPE       MAP__CC__TYPE
           CHARACTER  SOU_NAME*16
           CHARACTER  IVS_NAME*8
           CHARACTER  B1950_NAME*8
           CHARACTER  J2000_NAME*10
           CHARACTER  EXP_NAME*8
           REAL*8     FREQ
           REAL*4     BMIN
           REAL*4     BMAJ
           REAL*4     BPOS_ANG
           INTEGER*4  MJD_OBS
           REAL*8     TAI_OBS
           INTEGER*4  NUM_CC
           REAL*8     OBS_RA
           REAL*8     OBS_DEC
           REAL*4,    POINTER :: POS_CC(:,:)
           REAL*4,    POINTER :: FLUX_CC(:)
	   INTEGER*4  STATUS
      END TYPE   MAP__CC__TYPE
