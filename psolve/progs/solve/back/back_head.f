      PROGRAM BACK_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BACK PROGRAM SPECIFICATION
!
! 1.1 Do a back solution.
!
! 1.2 REFERENCES:
!
! 2.  BACK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'prfil.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES:  back_do
!
! 3.  LOCAL VARIABLES
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
!
      LOGICAL*2  KBIT
      INTEGER*2  INT2_ARG
      INTEGER*4  M3, MODE, IUER
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   pet  970407  Added reading socom { USE_COMMON ( 'ORC') }
!   pet  980710  Added support of the situation when some sources/stations/
!                baselines were excluded from estimation or solution on the
!                flight by PROC. Changed the place where arc file is read
!                in fast mode
!   pet  990305  Extracted as a separate module from the previous back.f
!
! 5.  BACK PROGRAM STRUCTURE
!
      CALL PRE_PROG()
      INCLUDE 'back_version.i' ! Set revision date of the current version
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      CORLN = KBIT ( PRE_IBATCH, INT2(9))
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL START_MN
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Restore prfil -- it may be needed since it keeps psitd for piece
! --- wise model of station positions
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- Attempt to override FAST_MODE from environment variable and make test
! --- of eligibility FAST_MODE for this session
!
      IUER = -1
      CALL FAST_BYPASS ( IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8501, IUER, 'BACK_HEAD', 'Error in trying '// &
     &         'to bypass fast_mode' )
           STOP 'BACK: Abnormal termination'
      END IF
!
! --- Call the routine which will actually do the work
!
      IUER = -1
      MODE = 1
      CALL BACK_DO ( MODE, B3DOBJ, B1B3DOBJ, 0, -1,0, -1, M3, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, * ) ' iuer =',iuer
           CALL ERR_LOG ( 8502, -1, 'BACK_HEAD', 'Error in '//'backward run' )
           STOP 'BACK: Abnormal termination'
      END IF
!
      CALL END_PROG()
      END  !#!  BACK_HEAD  #!#
