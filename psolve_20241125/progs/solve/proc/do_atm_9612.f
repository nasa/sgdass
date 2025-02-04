      SUBROUTINE DO_ATM_9612 ( WHO_STA, LPARM, IPARMS, A, CNSTROBJ )
      IMPLICIT NONE
!
! ************************************************************************
! *                                                                      *
! *     Archaic version of SOLVE at 01-DEC-96. Used inly in the old      *
! *   SOLVE compatibility mode.                                          *
! *                                                                      *
! ************************************************************************
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
      INCLUDE 'glbc4.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IPARMS
      CHARACTER*(*) LPARM(IPARMS)
      REAL*8 A(*  )
      CHARACTER*8 WHO_STA(*)
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
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_atm
!
! 3.  LOCAL VARIABLES
!
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*2  IERR
      INTEGER*4  END_APARM, I, J, K, APARM(M_GPA)
      LOGICAL*2 LDUM
      CHARACTER*3 PARMTYP
      CHARACTER*20 ATM_PARM*13, DUMRA
      REAL*8       CONSTRAINT, SIGMA, TIME_INTERVAL, TIME_DAY, ANTRVL8
      LOGICAL*4,   EXTERNAL :: CHECK_STABIT
!
! 4.  HISTORY
!   MWH  910524  Include interval size in calculation of constraint
!   jmg  960610  Remove holleriths.
!   pet  971203  Added logic for bypassing deselected station
!   pet  980119  Added support data structure CNSTR
!   pet  1999.05.05  Added calll of ADD_TYCNS
!
! 5.  DO_ATM PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
      DO I=1,NUMSTA
!
! ------ Check: was the I-th station in solution?
!
         IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 810
         SIGMA=DBLE(SACNST(I))
         SIGMA=(SIGMA*1.d-12)
!
!*****************************************************************
! At this point we must include the factor of the time interval
!   per epoch;  in the case of 60 minutes (1 hour), this is
!   simply unity, and has no additional effect
!******************************************************************
        atmos_interval = &
     &    (tatm(iatstr(i)+2)-tatm(iatstr(i)+1))*24.d0
        sigma = sigma * atmos_interval
        if (sigma.eq.0.d0) then
          constraint=0
        else
          CONSTRAINT=1/(SIGMA**2)
        endif
        parmtyp = 'AT1'
        ldum = .FALSE.
        CALL CINDEX_PARM(parmtyp,APARM,LPARM,IPARMS,END_APARM, &
     &                 DUMRA,ldum,WHO_STA(I))
!
        IF ( DABS(SIGMA) .GT. 1.D-30 ) THEN
             CALL ADD_TYCNS ( 'ATT_RATE', 'Atmosphere rate between '// &
     &           'segments', 'psec/hour', SIGMA, .FALSE., CNSTROBJ )
        END IF
!
! Add the constraint
!
        CALL ADD_ATM ( FAST_MODE, CONSTRAINT, APARM, END_APARM, A, 0, 0, &
     &                 CNSTROBJ )
 810    CONTINUE
      ENDDO
!
! --- Add information about the type of the constraint applied
!
!
      RETURN
      END  !#!  DO_ATM_9612  #!#
