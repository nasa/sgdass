      SUBROUTINE AUTAT(INTRVL,FAIL)
      IMPLICIT    NONE
!
! 1.  AUTAT PROGRAM SPECIFICATION
!
! 1.1 Set up automatic parameterization of atmospheres.
!
! 1.2 REFERENCES:
!
! 2.  AUTAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   INTRVL
!
! INTRVL - Interval to use for clock parameterization
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2   FAIL
!
! FAIL - True if number of atmosphere epochs exceeds maximum
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: obstm,epocs,testf,setat
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   NTAGS
      REAL*8  FJDOBS,LJDOBS,INDAY
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Set size of interval for atmosphere epochs
!
! 5.  AUTAT PROGRAM STRUCTURE
!
! Calculate epoch interval in days
!
      atmos_interval = intrvl/60.
      INDAY=DBLE(INTRVL)/1440.0D0
!
! Get start and stop julian dates for the observation
!
      CALL OBSTM(FJDOBS,LJDOBS)
!
! Determine number of epochs of requested length
!
      CALL EPOCS(FJDOBS,LJDOBS,INDAY,NTAGS)
!
! Check whether number of epochs exceeds maximum
!
      CALL TESTF(NTAGS,FAIL)
!
! If not, then go ahead and set epochs
!
      IF(.NOT.FAIL) CALL SETAT(FJDOBS,NTAGS,INDAY)
      RETURN
      END
