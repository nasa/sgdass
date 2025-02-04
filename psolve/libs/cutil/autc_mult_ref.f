      SUBROUTINE AUTC_MULT_REF ( MODE, BM_REF_BITS, INTRVL, FAIL )
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
      INTEGER*2 INTRVL, REFSTA, I, BM_REF_BITS(*)
      CHARACTER MODE*(*)
!
! INTRVL - Interval between continued rate epochs, in minutes
! MODE - 'F'= forced mode; `A`= auto mode
! mb_ref_bits - the bits array of reference clock
!               bit 1 is on if station 1 is a reference clock, ect.
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL, QUIT, KBIT
!
! FAIL - False if we were successful in setting the clock arrays
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
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
!   :97.10.05:jwr: Autc, which supports one clock reference site, used to make
!                  autc_mult_ref, which supports multiple reference clocks
!   pet  2001.05.02  Prevented dividing by zero
!
! --- Determine size of epoch interval in days
!
      INDAY=DBLE(INTRVL)/1440.0D0 ! Interval(minutes) converted to day
      IF ( INTRVL .LT. 0.1 ) THEN
           FAIL = .FALSE.
           RETURN
      END IF
!
! --- Get start and stop times for this observation
!
      CALL OBSTM ( FJDOBS, LJDOBS )
!
! --- Determine number of clock epochs
!
      CALL EPOCS ( FJDOBS, LJDOBS, INDAY, NEPOC )
      INTRVL = INDAY *1440.0D0
      CALL OFSTS_STFLG_MULT_REF ( NOFST, BM_REF_BITS, FJDOBS, MODE )
!
! --- Make sure we don't have too many epochs
!
      CALL TSTCL ( NEPOC, NOFST, FAIL )
!
! --- Go ahead and set the clock epochs
!
      IF ( .NOT. FAIL ) CALL SETC_MULT_REF ( NEPOC, BM_REF_BITS, NOFST, INDAY )
!
      RETURN
      END  !#!  AUTC_MULT_REF  #!#
