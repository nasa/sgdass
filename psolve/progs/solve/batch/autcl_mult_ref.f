      SUBROUTINE AUTCL_MULT_REF ( CLKPOL_FLG, CLKPOL_DEG, MODE, INTRVL, FAIL, &
     &                            ISTAD )
      IMPLICIT NONE
!
! 1.  AUTCL PROGRAM SPECIFICATION
!
! 1.1 Set up automatic parameterization of clocks.
!
! 1.2 REFERENCES:
!
! 2.  AUTCL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2     CLKPOL_DEG, INTRVL, ISTAD(*)
      CHARACTER     CLKPOL_FLG*(*), MODE*(*) 
!
! INTRVL - Interval to use for clock parameterization
! ISTAD - station data flag
! MODE - A = auto; F = force
!
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - True if number of clock epochs exceeds maximum
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: obstm,epocs,ofsts,tstcl,setcl
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NEPOC, NOFST
      INTEGER*2  ISTA, JCLOCK, K, IORD
      REAL*8    INDAY, FJDOBS, LJDOBS
                      LOGICAL*2 kbit
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Set size of interval for automatic clock epochs
!   pet  2000.11.28  Fixed the logic for setting clock parameters without
!                    linear spline
!   pet  2001.05.02  Prevented dividing by zero
!   pet  2004.03.22  Added a logic for setting clock parameters if &
!                    option NO INTERVALS has been specified
!
! 5.  AUTCL PROGRAM STRUCTURE
!
      CLOCK_INTERVAL = INTRVL/60.
      INDAY=DBLE(INTRVL)/1440.0D0 ! Interval(minutes) converted to day
!
      CALL OFSTS_MULT_REF ( CLKPOL_FLG, CLKPOL_DEG, NOFST, MODE, ISTAD )
      IF ( INTRVL .LE. 0 ) THEN
           DO ISTA=1,NUMSTA
              K = 1 + ICLSTR(ISTA)
              CALL SBIT ( LCLK(K), INT2(14), INT2(0) ) 
              CALL SBIT ( LCLK(K), INT2(15), INT2(0) ) 
              CALL SBIT ( LCLK(K), INT2(16), INT2(0) ) 
           END DO
!
           FAIL = .FALSE.
           RETURN
      END IF
!
! --- Get start and stop Julian dates for this observation
!
      CALL OBSTM ( FJDOBS, LJDOBS )
!
! --- Determine number of epochs of requested length, and offsets
!
      CALL EPOCS ( FJDOBS, LJDOBS, INDAY, NEPOC )
!
! --- Check whether the number of epochs exceeds maximum
!
      CALL TSTCL ( NEPOC, NOFST, FAIL )
!
! --- If not, go ahead and set up epochs
!
      IF ( .NOT. FAIL ) CALL SETCL_MULT_REF ( MODE, NEPOC, NOFST, INDAY )
!
      RETURN
      END  !#!  AUTCL_MULT_REF  #!#
