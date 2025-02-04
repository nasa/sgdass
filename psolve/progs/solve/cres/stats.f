      SUBROUTINE STATS ( PT, WT, WRMS, FACT, IUNW, SUPMET, FL_USED, NC )
      IMPLICIT   NONE
      INCLUDE   'solve.i'
!
! 1.  STATS PROGRAM SPECIFICATION
!
! 1.1 Calculate the residual statistics.
!
! 1.2 REFERENCES:
!
! 2.  STATS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IUNW, SUPMET
      LOGICAL*4  FL_USED
      INTEGER*4 NC
      REAL*8 PT,WT,WRMS,FACT
!
! FACT - Total of Square errors for observations. See equ. 15 of Ed Himwich's
!        Estimation Accordding to SOLVE
! IUNW - Unweight flag
! NC - Observation flag, NC < 0 means excluded baseline observation
! PT - Residual delay
! WRMS - Weighted rms
! WT - Error on this observation
!
! 2.3 OUTPUT Variables:
!
! WRMS - Incremented weighted rms
! FACT - Total of Square errors for observations. See equ. 15 of Ed Himwich's
!        Estimation Accordding to SOLVE
! NC - Observation flag, NC < 0 means excluded baseline observation
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: secnd
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      REAL*8 W,D
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   IRS  840810  Accumulate statistics for excluded baselines.  IUNW=16
!                means observation is for an excluded baseline.
!                Accumulation is negative for these observations in
!                order to flag CRES for printout.
!   KDB  951213  Integer*4 number of observations
!   pet  2007.06.08  Added support of SUPMET__META suppression strategy
!   pet  2007.06.26  Added check for the deselected source (IUNW=17)
!
! 5.  STATS PROGRAM STRUCTURE
!
      IF ( SUPMET == SUPMET__META ) THEN
           IF ( FL_USED ) THEN
                IF ( IUNW .EQ. 16 ) THEN
                     NC = NC - 1
                   ELSE IF ( IUNW .EQ. 17 ) THEN
                     NC = NC - 1
                   ELSE 
                     NC = NC + 1
                END IF
              ELSE
                RETURN 
           END IF 
         ELSE
           IF ( IUNW .EQ. 0 ) THEN
                NC = NC + 1
              ELSE IF ( IUNW .EQ. 16 ) THEN
                NC = NC - 1
              ELSE IF ( IUNW .EQ. 17 ) THEN
                NC = NC - 1
              ELSE
                RETURN
           ENDIF
      ENDIF
!
      W = WT*WT
      D = PT*PT
      IF ( DABS(W) .GT. 1.D-30 ) THEN
           WRMS=WRMS + D/W
           FACT=FACT + 1.0D0/W
         ELSE
           WRMS = 0.0D0
           FACT = 0.0D0
      END IF
!
      RETURN
      END  !#!  STATS  #!#
