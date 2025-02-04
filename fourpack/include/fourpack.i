!
! >>>>> fourpack.i  2012.08.10 v 1.3  --  2015.10.19_10:40:12
!
       TYPE      SPHE_TYPE
            INTEGER*4  FIRST
            INTEGER*4  NUM_THR
            INTEGER*4  DEG
            INTEGER*4  NORM
            REAL*8,    POINTER :: AJ(:)     => NULL()
            REAL*8,    POINTER :: F1(:,:)   => NULL()
            REAL*8,    POINTER :: F2(:,:)   => NULL()
            REAL*8,    POINTER :: F3(:,:)   => NULL()
	    REAL*8,    POINTER :: PL(:,:)   => NULL()
	    REAL*8,    POINTER :: PLT(:,:)  => NULL()
	    REAL*8,    POINTER :: DPLT(:,:) => NULL()
            REAL*8,    POINTER :: MSIN(:)   => NULL()
            REAL*8,    POINTER :: MCOS(:)   => NULL()
!
	    INTEGER*4  FF_DEG
	    INTEGER*4  FF_NORM
	    INTEGER*4  FF_MODE
	    INTEGER*4  FF_FORCE_F
	    INTEGER*4  FF_FORCE_X
	    INTEGER*4  FF_STATUS
!
	    INTEGER*4  AJ_DIM
	    INTEGER*4  AJ_NORM
	    INTEGER*4  AJ_STATUS
!
	    INTEGER*4  PL_DEG
	    INTEGER*4  PL_NORM
	    INTEGER*4  PL_STATUS
!
	    INTEGER*4  MS_DEG
	    INTEGER*4  MS_STATUS
!
	    REAL*8     LAT
	    REAL*8     LON
!
	    INTEGER*4  STATUS
       END  TYPE SPHE_TYPE
!
       TYPE      X__TYPE
           REAL*8     F
           INTEGER*4  E
       END TYPE  X__TYPE
       INTEGER*4   RAD_FF
       REAL*8      BIGO_FF, BIGU_FF, BIGHO_FF, BIGHU_FF
       PARAMETER ( RAD_FF = 960 )
       PARAMETER ( BIGO_FF  = 2.D0**RAD_FF,     BIGU_FF  = 2.D0**(-RAD_FF)   )
       PARAMETER ( BIGHO_FF = 2.D0**(RAD_FF/2), BIGHU_FF = 2.D0**(-RAD_FF/2) )
!
       INTEGER*4    FSH__MAX_SCL, FSH__MAX_DEG, FSH__INIT, FSH__ALLO, FSH__COMP
       INTEGER*4    FSH__UNDF,    FSH__ENFORCE
       PARAMETER  ( FSH__MAX_SCL =       2711 )
       PARAMETER  ( FSH__MAX_DEG =      65535 )
       PARAMETER  ( FSH__INIT    =  923801392 )
       PARAMETER  ( FSH__ALLO    = 1923823127 )
       PARAMETER  ( FSH__COMP    = 2040289752 )
       PARAMETER  ( FSH__UNDF    = 0 )
       PARAMETER  ( FSH__ENFORCE = 1397230854 )
       REAL*8       FSH__ANG_EPS
       PARAMETER  ( FSH__ANG_EPS = 1.D-7 ) ! radians
!
! <<<<  end of fourpack.i  
