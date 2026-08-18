      SUBROUTINE DO_ATM ( FAST_MODE, FAST_DBG, WHO_STA, LPARM, IPARMS, A, &
     &                    B3DOBJ, B1B3DOBJ )
      IMPLICIT NONE
!
! 1.  DO_ATM PROGRAM SPECIFICATION
!
! 1.1 Apply atmosphere constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_ATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4     IPARMS
      CHARACTER     LPARM(IPARMS)*(*)
      REAL*8        A(*)
      CHARACTER*8   WHO_STA(*)
!
! A - The normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_atm
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4    END_APARM, I, APARM(M_GPA)
      LOGICAL*2    LDUM
      CHARACTER*3  PARMTYP
      CHARACTER*20 DUMRA
      REAL*8       CONSTRAINT, SIGMA
      LOGICAL*4,   EXTERNAL :: CHECK_STABIT
!C
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  FAST_MODE, FAST_DBG
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   MWH  910524  Include interval size in calculation of constraint
!   jmg  960610 Remove holleriths.
!   pet  970117 Added support of B3D parametrization
!   pet  970226 Added support of B1B3D parametrization
!   pet  971203  Added logic for bypassing deselected station
!
! 5.  DO_ATM PROGRAM STRUCTURE
!
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( CHECK_STABIT ( I ) ) THEN
              SIGMA = DBLE(SACNST(I))
              SIGMA = (SIGMA*1.D-12)
!
! ----------- At this point we must include the factor of the time interval
! ----------- per epoch;  in the case of 60 minutes (1 hour), this is
! ----------- simply unity, and has no additional effect
!
              ATMOS_INTERVAL = ( TATM( IATSTR(I)+2 ) -TATM( IATSTR(I)+1 ) )*24.D0
              SIGMA = SIGMA * ATMOS_INTERVAL
              IF ( SIGMA .EQ. 0.D0 ) THEN
                   CONSTRAINT = 0.D0
                ELSE
                   CONSTRAINT=1.D0/(SIGMA**2)
              ENDIF
              PARMTYP = 'AT1'
              LDUM = .FALSE.
              CALL CINDEX_PARM ( PARMTYP, APARM, LPARM, IPARMS, END_APARM, &
     &                         DUMRA, LDUM, WHO_STA(I) )
!
! --------- Add the constraint
!
              CALL ADD_ATM ( FAST_MODE, CONSTRAINT, APARM, END_APARM, A, &
     &                       B3DOBJ, B1B3DOBJ )
         END IF
      ENDDO
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' do_atm        fast_mode = ', fast_mode
      END IF
!
      RETURN
      END  !#!  DO_ATM  #!#
