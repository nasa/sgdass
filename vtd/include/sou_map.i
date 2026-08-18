!
!  >>>> Include block sou_map.i
!  >>>> 2004.02.26  L. Petrov   2016.02.25_15:39:38
!
        INTEGER*4  MHDR, MKEY, MSTA, MSUB
        PARAMETER  ( MHDR =       32 )
        PARAMETER  ( MKEY = 256*1024 )
        PARAMETER  ( MSTA =      256 )
        PARAMETER  ( MSUB =      256 )
!
	INTEGER*4    SMP__MAX, SMP__M2
	PARAMETER  ( SMP__MAX = 1024 ) 
	PARAMETER  ( SMP__M2  = SMP__MAX*SMP__MAX ) 
!
        TYPE      SOUMAP__TYPE !
            INTEGER*4  DIM1
            INTEGER*4  DIM2
	    INTEGER*4  NUM_CC
	    INTEGER*4  NUM_SEG
	    INTEGER*4  NSTK
	    INTEGER*4  MJD
	    REAL*8     TAI
	    REAL*8     ALPHA
	    REAL*8     DELTA
	    REAL*8     STEP_RA
	    REAL*8     STEP_DL
	    REAL*8     FREQ
	    REAL*8     FLUX_MAX
	    REAL*8     FLUX_INT
	    REAL*8     FLUX_SHR
	    REAL*8     FLUX_UNR
	    REAL*8     BEAM_MAJ
	    REAL*8     BEAM_MIN
	    REAL*8     BEAM_POS_ANG
	    REAL*8     NOISE
	    REAL*4,    POINTER :: IMAGE(:,:)   => NULL()
	    REAL*4,    POINTER :: FLUX_CC(:)   => NULL()
	    REAL*4,    POINTER :: COOR_CC(:,:) => NULL()
	    INTEGER*4  STATUS_MAP
	    INTEGER*4  STATUS_CC
	    CHARACTER  SOU_NAME*10
	    CHARACTER  EXP_NAME*16
	    CHARACTER  DATE_OBS*10
	    CHARACTER  FINAM*256
        END TYPE  SOUMAP__TYPE ! SOUMAP__TYPE !
!
        TYPE      SOUMAP_GACO__TYPE
	     REAL*4,    POINTER :: GAIN_CORR(:)
	     REAL*4,    POINTER :: GACO_ERR(:)
	     INTEGER*4, POINTER :: NVIS(:)
        END  TYPE SOUMAP_GACO__TYPE
!
        TYPE      VIS__TYPE !
            INTEGER*4  NP
	    REAL*8     ALPHA
	    REAL*8     DELTA
	    REAL*8     FRQ_LO
	    CHARACTER  SOU_NAME*10
	    CHARACTER  EXP_NAME*16
	    CHARACTER  DATE_OBS*10
	    CHARACTER  FINAM*256
	    INTEGER*4  STATUS
	    INTEGER*4  STATUS_GACO
	    INTEGER*4  MJD_REF
	    REAL*8     TAI_REF
	    INTEGER*4  NUM_SEG
	    INTEGER*4  NFRQ
	    INTEGER*4  NSTA
	    INTEGER*4  NSUB
	    INTEGER*4  NSTK
	    INTEGER*4, POINTER :: L_STA(:)     => NULL()
            INTEGER*4, POINTER :: LIS_STA(:,:) => NULL()
  	    REAL*8,    POINTER :: SKY_FRQ(:)   => NULL()
	    INTEGER*4, POINTER :: MJD(:)       => NULL()
	    REAL*8,    POINTER :: TAI(:)       => NULL()
	    COMPLEX*8, POINTER :: VIS(:,:)     => NULL()
	    REAL*4,    POINTER :: UV(:,:,:)    => NULL()
	    REAL*4,    POINTER :: WEI(:,:)     => NULL()
	    REAL*4,    POINTER :: IND_BAS(:)   => NULL()
	    REAL*4,    POINTER :: INT_TIM(:)   => NULL()
	    CHARACTER, POINTER :: C_STA(:)*8   => NULL()
	    TYPE ( SOUMAP_GACO__TYPE ), POINTER :: GACO(:) => NULL()
        END TYPE  VIS__TYPE ! SOUMAP__TYPE !
	INTEGER*4  SMP__UNDF, SMP__ALLC, SMP__LOAD
	PARAMETER  ( SMP__UNDF =     0 )
	PARAMETER  ( SMP__ALLC = 16001 )
	PARAMETER  ( SMP__LOAD = 16002 )
!
        CHARACTER    SOU_MAP__LABEL*36
        PARAMETER  ( SOU_MAP__LABEL = 'SOU_MAP Format version of 2016.02.29' )
!
! >>>>> end of block sou_map.i
