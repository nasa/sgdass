      SUBROUTINE AUTO_INTRVL ( REFSTA, TJD, KLOC_IN, KLOC_OUT, NEPOCS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  AUTO_INTRVL PROGRAM SPECIFICATION
!
! 1.1 Insert clock polynomial epochs in the SOLVE clock
!     solution list:  these are the rate intervals developed for the
!     automatic parameterization of the clocks.  NOTE that if the
!     interval is within 10 minutes of a real clock break, then the
!     interval WON'T be inserted, and KLOC_OUT will be returned with a -1.
!
! 1.2 REFERENCES:
!
! 2.  AUTO_INTRVL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 REFSTA,KLOC_IN,NEPOCS
      REAL*8 TJD
!
! KLOC_IN - Number of epoch at which to start search for proper range
! NEPOCS - Total number of auto epoch intervals
! REFSTA - Site number of reference station
! TJD - Julian date of epoch to be inserted
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 KLOC_OUT
!
! KLOC_OUT - Number of epoch inserted
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: date_in_bds,wthn_10_min
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K
      LOGICAL*2 STOP_LOOP,KBIT,DATE_IN_BDS,WTHN_10_MIN,OFST
      REAL*8 DATE1,DATE2,centimin
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Correct "within-10-minutes" test for new parameteriztion scheme
!
! 5.  AUTO_INTRVL PROGRAM STRUCTURE
!
! calculate .01 minute as fraction of a day
!
      CENTIMIN = 1.0D0/( 100.0*60.0*24.0)
!
! --- Determine the position of the new epoch.
!
      I=KLOC_IN
      STOP_LOOP=.FALSE.
      DO WHILE ( I .LE. MAX_CLK-1 .AND. .NOT. STOP_LOOP )
         IF ( I+1 .LE. NEPOCS ) THEN
!
! ----------- Add .01 minute to make test equivalent to previous version
! ----------- (i.e. before overhaul of clock parameterization MWH 910426)
!
              DATE1=FJDCL(I)   + CENTIMIN
              DATE2=FJDCL(I+1) + CENTIMIN
              IF ( DATE_IN_BDS(DATE1,DATE2,TJD) ) THEN
                   IF ( WTHN_10_MIN(DATE1,DATE2,TJD,I) ) THEN
                        KLOC_OUT  = -1
                        STOP_LOOP = .TRUE. !kick out of loop
                     ELSE
                       DO J=MAX_CLK-1,I+1,-1
                          FJDCL(J+1)=FJDCL(J)
                          LCLK(J+1)=LCLK(J)
                          DO K=1,2
                             ICLSTA(K,J+1)=ICLSTA(K,J)
                          ENDDO
                       ENDDO
                       FJDCL(I+1) = TJD
                       LCLK(I+1)  = 0
                       KLOC_OUT   = I+1
                       STOP_LOOP  = .TRUE.
                   ENDIF
                 ELSE
                   I=I+1
                   STOP_LOOP=.FALSE.
              ENDIF
           ELSE
              IF ( I .GT. 1 ) THEN
                   OFST = KBIT( LCLK(I-1), INT2(1) )
                 ELSE
                   OFST = KBIT( LCLK(I), INT2(1) )
              ENDIF
              IF ( OFST .AND. DABS(TJD-FJDCL(I)) .LT. 10.0/1440.0 ) THEN
                   KLOC_OUT = -1
                ELSE
                   FJDCL(I+1) = TJD
                   LCLK(I+1)  = 0
                   KLOC_OUT   = I+1
              ENDIF
              STOP_LOOP = .TRUE.
         END IF
      ENDDO
!
      RETURN
      END  SUBROUTINE  AUTO_INTRVL  !#!#
