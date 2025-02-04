      SUBROUTINE AUTOC(ISTA,INTRVL,max_gclock_deg,FAIL)
      IMPLICIT NONE
!
! 1.  AUTOC PROGRAM SPECIFICATION
!
! 1.1 Implement automatic parameterization of clocks.
!
! 1.2 REFERENCES:
!
! 2.  AUTOC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTA,INTRVL
      integer*4 max_gclock_deg
!
! INTRVL - Time interval between successive clock epochs
! ISTA - Site number of station to be parameterized
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - True if number of clock parameters exceeds maximum
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: obstm,epocs,ofst_sta,tstcl_sta,setcl_sta
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NEPOC,I,IERR
      REAL*8 INDAY,FJDOBS,LJDOBS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  AUTOC PROGRAM STRUCTURE
!
! Convert interval from minutes to days
!
      INDAY=DBLE(INTRVL)/1440.0D0
!
! Get start and stop times of observation
!
      CALL OBSTM(FJDOBS,LJDOBS)
!
! determine number of clock epochs in observation
!
      CALL EPOCS(FJDOBS,LJDOBS,INDAY,NEPOC)
!
! Get clock offsets for this station
!
      CALL OFST_STA(ISTA,FJDOBS,max_gclock_deg)
!
! Test for excessive number of clock parameters
!
      CALL TSTCL_STA(ISTA,NEPOC,FAIL)
!
! Set up clock parameters
!
      IF(.NOT.FAIL) CALL SETCL_STA(ISTA,NEPOC,INDAY)
      RETURN
      END
