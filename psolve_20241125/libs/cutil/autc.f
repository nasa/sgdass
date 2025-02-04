      SUBROUTINE AUTC(MODE,REFSTA,INTRVL,FAIL)
      IMPLICIT NONE
!
! 1.  AUTC PROGRAM SPECIFICATION
!
! 1.1 Put into effect automatic parameterization of clocks
!
! 1.2 REFERENCES:
!
! 2.  AUTC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 INTRVL,REFSTA
      CHARACTER*(*) MODE
!
! INTRVL - Interval between continued rate epochs, in minutes
! MODE - 'F'= forced mode; `A`= auto mode
! REFSTA - the number of the reference station
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - False if we were successful in setting the clock arrays
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: obstm,epocs,ofsts_stflg,tstcl,setcl
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NEPOC,NOFST
      REAL*8 INDAY,FJDOBS,LJDOBS
!
! FJDOBS - Julian date (time) at start of observation
! INDAY - epoch interval, in days
! LJDOBS - Julian date (time) at end of observation
! NEPOC - Number of epochs defined
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  AUTC PROGRAM STRUCTURE
!
! Determine size of epoch interval in days
!
      INDAY=DBLE(INTRVL)/1440.0D0 !interval(minutes) converted to day
!
! Get start and stop times for this observation
!
      CALL OBSTM(FJDOBS,LJDOBS)
!
! Determine number of clock epochs
!
      CALL EPOCS(FJDOBS,LJDOBS,INDAY,NEPOC)
      intrvl = inday * 1440.0d0
      CALL OFSTS_STFLG(NOFST,REFSTA,FJDOBS,MODE)
!
! Make sure we don't have too many epochs
!
      CALL TSTCL(NEPOC,NOFST,FAIL)
!
! Go ahead and set the clock epochs
!
      IF(.NOT.FAIL) &
     &    CALL SETCL(NEPOC,REFSTA,NOFST,INDAY)
      RETURN
      END
