      SUBROUTINE EPOCS ( FJDOBS, LJDOBS, INDAY, NEPOCS )
      IMPLICIT   NONE
!
! 1.  EPOCS PROGRAM SPECIFICATION
!
! 1.1 Determines number of epochs of length INDAY (rounded to the
!     nearest integer) contained in the period between two given
!     Julian dates.
!
! 1.2 REFERENCES:
!
! 2.  EPOCS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 FJDOBS,LJDOBS,INDAY
!
! FJDOBS - Julian date at start of period
! INDAY - Length of one epoch
! LJDOBS - Julian date at end of period
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NEPOCS
!
! NEPOCS - Number of epochs in specified period
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      REAL*8  SPAN
      INTEGER*2  IDNINT
!
! SPAN - Length of time in specified period
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Add one to number of epochs for new parameterization scheme
!
! 5.  EPOCS PROGRAM STRUCTURE
!
      SPAN=LJDOBS-FJDOBS
      IF ( INDAY .GT. SPAN ) INDAY = SPAN+0.0001D0
      NEPOCS=IDNINT(SPAN/INDAY) + 1
      RETURN
      END  !#!  EPOCS  #!#
