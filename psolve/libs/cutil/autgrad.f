      SUBROUTINE AUTGRAD ( INTRVL, FAIL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  AUTGRAD PROGRAM SPECIFICATION
!
! 1.1 Set up automatic parameterization of gradients.
!
! 1.2 REFERENCES:
!
! 2.  AUTGRAD INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      real*8   INTRVL
!
! INTRVL - Interval to use for gradient parameterization
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2   FAIL
!
! FAIL - True if number of gradient epochs exceeds maximum
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: obstm,epocs,testg,setat
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   NTAGS
      REAL*8  FJDOBS,LJDOBS,INDAY
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   MWH  941219      Create (based on autat)
!   pet  2001.05.02  Prevented dividing on zero
!
! 5.  AUTGRAD PROGRAM STRUCTURE
!
! --- Calculate epoch interval in days
! --- INTRVL        is in hours
! --- GRAD_INTERVAL is in minutes
! --- INDAY         is in days
!
      GRAD_INTERVAL = INTRVL/60.0D0
      INDAY=DBLE(INTRVL)/1440.0D0
      IF ( GRAD_INTERVAL .LT. 0.1 ) THEN
           FAIL = .TRUE.
           RETURN
      END IF
!
! --- Get start and stop julian dates for the observation
!
      CALL OBSTM ( FJDOBS, LJDOBS )
!
! --- Determine number of epochs of requested length
!
      CALL EPOCS ( FJDOBS, LJDOBS, INDAY, NTAGS )
!
! --- Check whether number of epochs exceeds maximum
!
      CALL TESTG ( NTAGS, FAIL )
!
! --- If not, then go ahead and set epochs
!
      IF ( .NOT. FAIL ) then
           CALL SETGRAD ( FJDOBS, NTAGS, INDAY )
           GRAD_INTERVAL = INDAY*24.D0
         ELSE
           CALL FERR ( NTAGS, "More than 22 gradient intervals", INT2(0), &
     &                         INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  AUTGRAD  #!#
