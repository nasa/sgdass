      SUBROUTINE SCOR_STATDAT ( IFIRSTSTATION, ISTATIONNO, ICALIBNO, &
     &                          QSITN, JCAVAL, JCAPPL, JPRINT, &
     &                          IHIGHYIND, PHC_STR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCOR_STATDAT PROGRAM SPECIFICATION
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.1
!     THIS SUBROUTINE LISTS STATIONS AND PRINTS DATA INDICATING
!     WHETHER A CALIBRATION IS UNAVAILABLE, AVAILABLE BUT NOT
!     APPLIED OR APPLIED, FOR EVERY STATION.
!
! 1.2 REFERENCES:
!
! 2.  SCOR_STATDAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      integer*2 JCAVAL(MAX_ARC_STA), JCAPPL(MAX_ARC_STA)
      integer*2 ifirststation,istationno,icalibno,ihighyind
      CHARACTER QSITN(MAX_ARC_STA)*8, PHC_STR(MAX_ARC_STA)*2
!
! ICALIBNO - Number of calibration types
! IFIRSTSTATION - Indicates index of first site to be displayed on the
!                 current page.
! IHIGHYIND - Y coordinate of bottom valid field for changing
! ISTATIONNO - Number of stations being used
! JCAPPL - List of calibrations applied to each station
! JCAVL - List of calibrations available for each station
! QSITN - List of station names
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*15 JPRINT(MAX_ARC_STA)
!
! JPRINT - Stores calibration status to be printed
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcor
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*79 BUFR
      INTEGER*2 ILIM1,ILIM2,ifirststatline,ifldlenind,ifrstchrxind
      integer*2 ifrstindbound,istatno,i,j,i1,i2,k
      integer*4 ix,iy
      integer*2 num_per_screen
      LOGICAL*2 KBIT
      DATA IFIRSTSTATLINE/10/
      DATA IFLDLENIND/5/
      DATA IFRSTCHRXIND/13/
      DATA IFRSTINDBOUND/11/
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!
!   kdb   8/10/95  Finish changing to 32 sites.
!
! 5.  SCOR_STATDAT PROGRAM STRUCTURE
!
!     THERE MAY BE TOO MANY STATIONS TO PRINT ON ONE SCREEN.  IF SO,
!     SELCOR PRINTS HALF THE SITES AT A TIME.  IFIRSTSTATION
!     ALREADY INDICATES THE INDEX OF THE FIRST SITE IN THE CURRENT HALF
!     TO BE DISPLAYED.  NOW FIND THE LAST SITE TO BE DISPLAYED IN THIS HALF.
!
      NUM_PER_SCREEN = 10 !number of sites per screen
      IF ( IFIRSTSTATION .EQ. 1 .AND. ISTATIONNO .GE. NUM_PER_SCREEN+1 ) THEN
           ISTATNO   = NUM_PER_SCREEN
         ELSE
           ISTATNO   = ISTATIONNO
      END IF
      IHIGHYIND = IFIRSTSTATLINE + ISTATNO-IFIRSTSTATION
!
!     COMPARE BIT ARRAYS JCAVAL (WHICH CALIBRATIONS ARE AVAILABLE FOR
!     A STATION) AND JCAPPL (WHICH CALIBRATIONS ARE APPLIED) TO
!     DETERMINE WHETHER A GIVEN CALIBRATION IS UNAVAILABLE, AVAILABLE
!     BUT NOT APPLIED, OR APPLIED FOR A GIVEN STATION.  PLACE THIS
!     DATA IN JPRINT.  - WILL REPRESENT UNAVAILABLE; V, AVAILABLE, AND
!     P, APPLIED.
!
      DO 30 I = IFIRSTSTATION, ISTATNO
        DO 40 J = 1, ICALIBNO
          IF (KBIT(JCAPPL(I), J)) THEN
            JPRINT(I)(J:J) = 'P'
          ELSE IF (KBIT(JCAVAL(I), J)) THEN
            JPRINT(I)(J:J) = 'V'
          ELSE
            JPRINT(I)(J:J) = '-'
          END IF
 40     CONTINUE
 30   CONTINUE
!
!     PRINT STATIONS, STATION/CALIBRATION DATA
!
      IX = 0
      DO 50 I1 = IFIRSTSTATION, ISTATNO
!
! ----- Set the index to the site for this screen.
! ----- For example, if printing sites num_screen+1, 2*num_screen,
! ----- i2 will run from 1 to num_screen
!
        IF ( IFIRSTSTATION .EQ. 1 ) THEN
             I2 = I1
          ELSE
             I2 = I1 - NUM_PER_SCREEN
        END IF
        DO K = 1, 79
           BUFR(K:K) = ' '
        END DO
        BUFR(1:10) = QSITN(I1)//'  '
        DO 55 J = 1, ICALIBNO
          ILIM1 = IFRSTINDBOUND + (J - 1) * IFLDLENIND
          ILIM2 = ILIM1 + IFLDLENIND - 1
          BUFR(ILIM1:ILIM2) = '|  '//JPRINT(I1)(J:J)//' '
 55     CONTINUE
        ILIM1 = IFRSTINDBOUND + ICALIBNO * IFLDLENIND
        ILIM2 = ILIM1+5
        BUFR(ILIM1:ILIM2) = '| '//PHC_STR(I1)//' |'
        IY = IFIRSTSTATLINE + (I2 - 1)
        CALL SETCR_MN ( IX, IY )
        CALL ADDSTR_F ( BUFR )
        CALL NL_MN()
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
      IX = IFRSTCHRXIND + (ICALIBNO/2) * IFLDLENIND
      IY = IHIGHYIND + 1
      CALL SETCR_MN ( IX, IY )
      CALL REFRESH_MN()
!
      RETURN
      END  !#!  SCOR_STATDAT   #!#
