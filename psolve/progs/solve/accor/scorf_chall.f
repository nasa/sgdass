      SUBROUTINE SCORF_CHALL (IALLCAL, IALL_SCR,IFIRSTSTATION, &
     &                              JCAFFL, JPRINT, &
     &                             ISTATIONNO, CHANGEALLGOAL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCORF_CHALL PROGRAM SPECIFICATION
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.1
!     THIS SUBROUTINE AFFECTS THE STATUS OF A CALIBRATION AT ALL STATIONS.
!     DEPENDING ON THE VALUE OF A SWITCH
!     (CHANGEALLGOAL), THE SUBROUTINE EITHER APPLIES THE CALIBRATION
!     TO THE APPROPRIATE STATIONS, OR TERMINATES THE APPLICATION,
!     REGARDLESS OF THE CALIBRATION'S PREVIOUS STATUSES.
!     THE SUBROUTINE ALSO CHANGES THE SCREEN TO REFLECT
!     THESE CHANGES.
!     THE SUBROUTINE WILL NOT "TURN OFF"  CALIBRATIONS WHICH CONFLICT
!     WITH A NEWLY APPLIED CALIBRATION, OR APPLY A NEW CALIBRATION IF
!     TURNING OFF THE OLD ONE LEFT A STATION WITHOUT ANY CALIBRATIONS
!     APPLIED TO IT.
!
! 1.2 REFERENCES:
!
! 2.  SCORF_CHALL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 iallcal,ifirststation,istationno,IALL_SCR
      INTEGER*4 I4P1
      CHARACTER*1 CHANGEALLGOAL
!
! CHANGEALLGOAL - Specifies whether data will be changed to available (V)
!                 or applied (P)
! IALLCAL - Calibration to be changed for all stations:
!                   position within array that tracks which calibs are applied
!                   and array used for  printing this info
! IALL_SCR - calib to be changed for all stations: position on the screen
!                 (1-10)
! IFIRSTSTATION - Index of first site to be displayed on the current page
! ISTATIONNO - Actual number of stations being used (up to 32)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 JCAFFL(7,MAX_ARC_STA)
      CHARACTER*112 JPRINT(MAX_ARC_STA)
!
! JCAFFL - bit array telling whether a given calib is applied or not at a
!          given station.   on = applied    off = not applied
! JPRINT - corresponds to jcaffl,  P = applied V = not applied
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
      CHARACTER*9 BLANKVAR
      character*79 bufstr
      LOGICAL*2 KBIT
      INTEGER*2 IFIRSTSTATLINE,IFLDLENIND,IFRSTCHRXIND
      INTEGER*2 I,I2,ISTATL
      INTEGER*4 IPRINTCOORDX,IPRINTCOORDY,IYPLACE
      INTEGER*2 IALLSTAT
      INTEGER*2 NUM_PER_SCREEN
      DATA I4P1 / 1 /
      DATA BLANKVAR /'         '/
      DATA IFIRSTSTATLINE/10/
      DATA IFRSTCHRXIND/13/
      DATA IFLDLENIND/5/
!
! 4. HISTORY
!   WHO     WHEN     WHAT
!   KDB    7/19/91   CREATED
!   JLR   921215   added variable I4P1 to replace 1J
!   KDB   950809   Fix error in which changing all sites to applied or
!                  not applied only affects sites on the current display page.
!                  Finish changing for 32 sites.
!
! 5.  SCORF_CHALL PROGRAM STRUCTURE
!
!
      NUM_PER_SCREEN = 10 !number of sites per screen
      IF (IFIRSTSTATION .EQ. 1 .AND. ISTATIONNO .GE. NUM_PER_SCREEN+ &
     &        1)THEN
        ISTATL  = NUM_PER_SCREEN
      ELSE
        ISTATL  = ISTATIONNO
      END IF
!
      IPRINTCOORDX = IFRSTCHRXIND + (IALL_SCR - 1) * IFLDLENIND
!
      I2 = 1
      DO 140 I = 1, ISTATIONNO
        IPRINTCOORDY = IFIRSTSTATLINE + (I2 - 1)
        IALLSTAT = I
!
!     SET ARRAY WHICH TRACKS CALIBRATION STATUSES TO KNOW WHETHER THE
!     GIVEN CALIBRATION WILL NOW BE APPLIED OR NOT AT THE CURRENT
!     STATION.  THEN GET READY TO SET THE SYMBOL OF THE CALIBRATION
!     ON THE SCREEN TO REFLECT ITS CURRENT STATUS.
!
        IF (CHANGEALLGOAL .EQ. 'P') THEN
          CALL SBIT ( JCAFFL(1,IALLSTAT), IALLCAL, INT2(1) )
          JPRINT(IALLSTAT)(IALLCAL:IALLCAL) &
     &           = 'P'
        ELSE
          CALL SBIT ( JCAFFL(1,IALLSTAT), IALLCAL, INT2(0) )
            JPRINT(IALLSTAT)(IALLCAL:IALLCAL) &
     &           = 'V'
        END IF
!
        IF (I.GE.IFIRSTSTATION.AND.I.LE.ISTATL) THEN
          CALL setcr_mn (IPRINTCOORDX, IPRINTCOORDY )
          WRITE (bufstr, &
     &       1111)JPRINT(IALLSTAT)(IALLCAL:IALLCAL)
          call addstr_f(bufstr(1:1) )
 1111     FORMAT (A1)
          I2 = I2 + 1
          IYPLACE = IPRINTCOORDY
        END IF
 140  CONTINUE
!
      CALL SETCR_MN ( IPRINTCOORDX + I4P1, IYPLACE )
!
      RETURN
      END
