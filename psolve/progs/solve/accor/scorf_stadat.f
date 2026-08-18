      SUBROUTINE SCORF_STADAT (IFIRSTSTATION, ISTATIONNO, ITPCAL, &
     &                            ICALPT, &
     &                            QSITN,  JCAFFL, JPRINT, &
     &                            IHIGHYIND)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCORF_STADAT PROGRAM SPECIFICATION
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.1
!     THIS SUBROUTINE LISTS STATIONS AND PRINTS DATA INDICATING
!     WHETHER A CALIBRATION IS APPLIED OR JUST AVAILABLE
!     FOR EVERY STATION.
!
! 1.2 REFERENCES:
!
! 2.  SCORF_STADAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      integer*2  JCAFFL(7,MAX_ARC_STA)
      integer*2 ifirststation,istationno,ITPCAL,ihighyind,icalpt
      CHARACTER*8 QSITN(MAX_ARC_STA)
!
! ITPCAL - Number of calibration types
! IFIRSTSTATION - Index of first site to be displayed on this page
! IHIGHYIND - Y coordinate of bottom valid field for changing
! ISTATIONNO - Number of stations being used
! JCAFFL - List of calibrations applied to each station
! QSITN - List of station names
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*112 JPRINT(MAX_ARC_STA)
!
! JPRINT - Stores calibration status to be printed
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcorf
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*61 BUFR
      INTEGER*2 ILIM1,ILIM2
      LOGICAL*2 KBIT
      INTEGER*2 I,I1,I2,IFIRSTSTATLINE,IFLDLENIND,IFRSTCHRXIND, &
     &   IFRSTINDBOUND,ISTATNO,J,K,J_REAL
      integer*4 ix,iy
      integer*2 num_per_screen
      DATA IFIRSTSTATLINE/10/
      DATA IFLDLENIND/5/
      DATA IFRSTCHRXIND/13/
      DATA IFRSTINDBOUND/11/
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   KDB  07/18/91 CREATED
!   KDB   8/10/95 FINISH CHANGING FOR 32 SITES.
!
! 5.  SCORF_STADAT PROGRAM STRUCTURE
!
!     THERE MAY BE TOO MANY STATIONS TO PRINT ON ONE SCREEN.  IF SO,
!     SELCOR USES TWO SCREENS, PRINTING HALF THE STATIONS ON EACH SCREEN.
!     IFIRSTSTATION ALREADY GIVES THE INDEX OF
!     THE FIRST SITE TO BE DISPLAYED.  NOW DETERMINE THE INDEX OF THE
!     LAST SITE TO BE DISPLAYED.
!
      NUM_PER_SCREEN = 10 !number of sites per screen
      IF ( IFIRSTSTATION .EQ. 1 .AND. ISTATIONNO .GE. NUM_PER_SCREEN+1 ) THEN
           ISTATNO = NUM_PER_SCREEN
        ELSE
           ISTATNO = ISTATIONNO
      END IF
!
      IHIGHYIND = IFIRSTSTATLINE + (ISTATNO - IFIRSTSTATION)
!
!     LOOK AT BIT ARRAY JCAFFL (WHICH CALIBRATIONS ARE APPLIED) TO
!     DETERMINE WHETHER A GIVEN CALIBRATION IS APPLIED FOR A GIVEN
!     STATION, OR JUST AVAILABLE.  PLACE THIS
!     DATA IN JPRINT.  V = AVAILABLE, AND  P = APPLIED.
!
      DO 30 I = IFIRSTSTATION, ISTATNO
        DO 40 J = ICALPT, ICALPT + ITPCAL - 1
          IF (KBIT(JCAFFL(1,I), J)) THEN
            JPRINT(I)(J:J) = 'P'
          ELSE
            JPRINT(I)(J:J) = 'V'
          END IF
 40     CONTINUE
 30   CONTINUE
!
!     PRINT STATIONS, STATION/CALIBRATION DATA
!
      IX = 0
      DO 50 I1 = IFIRSTSTATION, ISTATNO
!       Set the index to the site for this screen.
!       For example, if printing sites num_screen+1, 2*num_screen,
!       i2 will run from 1 to num_screen
        IF (IFIRSTSTATION .EQ. 1) THEN
          I2 = I1
        ELSE
          I2 = I1 - NUM_PER_SCREEN
        END IF
        DO K = 1, 61
          BUFR(K:K) = ' '
        END DO
        WRITE (BUFR(1:10),1030) QSITN(I1)
 1030   FORMAT (A8, 2X)
        DO 55 J = 1, ITPCAL
          J_REAL = ICALPT - 1 + J
          ILIM1 = IFRSTINDBOUND + (J - 1) * IFLDLENIND
          ILIM2 = ILIM1 + IFLDLENIND - 1
          WRITE (BUFR(ILIM1:ILIM2), 1033) JPRINT(I1)(J_REAL:J_REAL)
 1033     FORMAT ("|", 2X, A1, 1X)
 55     CONTINUE
        ILIM1 = IFRSTINDBOUND + ITPCAL * IFLDLENIND
        ILIM2 = ILIM1
        WRITE (BUFR(ILIM1:ILIM2),1035)
 1035   FORMAT ("|")
        IY = IFIRSTSTATLINE + (I2 - 1)
        CALL setcr_mn (IX, IY )
        call addstr_f(bufr )
        call nl_mn()
 50   CONTINUE
!
! --- So far, statdat has overwritten only the old data lines for
! --- Which a replacement data line exists.  now erase any remaining
! --- old data lines plus status line plus bottom line
!
      IF ( I2 .LT. NUM_PER_SCREEN ) THEN
        I2 = I2 + 1
        DO 57 I = I2, NUM_PER_SCREEN+2
           IX = 0
           IY = IY + 1
           CALL SETCR_MN ( IX, IY )
           CALL CLRTOEOL_MN()
 57     CONTINUE
      END IF
!
!     POSITION CURSOR SO THAT THE USER CAN CONVENIENTLY INPUT CHANGES
!     FOR INDIVIDUAL STATIONS, THE ONLY COMMANDS WHICH MUST BE ENTERED
!     BY CURSOR.
!
      IX = IFRSTCHRXIND + (ITPCAL/2) * IFLDLENIND
      IY = IHIGHYIND + 1
      CALL setcr_mn (IX, IY )
!
      RETURN
      END
