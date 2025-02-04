      SUBROUTINE AUTA(INTRVL,FAIL)
      IMPLICIT    NONE
!
! 1.  AUTA PROGRAM SPECIFICATION
!
! 1.1 Puts into effect automatic parameterization of the atmosphere
!
! 1.2 REFERENCES:
!
! 2.  INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 INTRVL
!
! INTRVL - Interval to use to set the continued rate epochs, in minutes
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - False if we were successful in setting the atmosphere epochs
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: obstm,epocs,testf,setat
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   NTAGS
      REAL*8  FJDOBS,LJDOBS,INDAY
!
! FJDOBS - Julian date at start of observation
! INDAY - specified interval, in units of days
! LJDOBS - Julian date at end of observation
! NTAGS - Number of epochs spanned by the observation
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  AUTA PROGRAM STRUCTURE
!
! Determine length of interval in days
!
      INDAY=DBLE(INTRVL)/1440.0D0
!
! Get the start and stop times for this observation
!
      CALL OBSTM(FJDOBS,LJDOBS)
!
! Determine how many epochs there will be
!
      CALL EPOCS(FJDOBS,LJDOBS,INDAY,NTAGS)
!
! Make sure we don't have too many epochs
!
      CALL TESTF(NTAGS,FAIL)
!
! Set the atmosphere epochs
!
      IF(.NOT.FAIL) CALL SETAT(FJDOBS,NTAGS,INDAY)
      RETURN
      END
