      SUBROUTINE SCOR_CHALL (IALLCHANGECALIB, IFIRSTSTATION, &
     &                             JCAVAL, JCAPPL, JPRINT, &
     &                             ISTATIONNO, ISTAND, &
     &                             CHANGEALLGOAL,JCSPCL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCOR_CHALL PROGRAM SPECIFICATION
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.1
!     THIS SUBROUTINE AFFECTS ALL STATIONS FOR WHICH THE GIVEN
!     CALIBRATION IS AVAILABLE, OR ALL STATIONS, IF THE CALIBRATION
!     IS A SPECIAL CASE.  DEPENDING ON THE VALUE OF A SWITCH
!     (CHANGEALLGOAL), THE SUBROUTINE EITHER APPLIES THE CALIBRATION
!     TO THE APPROPRIATE STATIONS, OR TERMINATES THE APPLICATION,
!     REGARDLESS OF THE CALIBRATION'S PREVIOUS STATUSES.
!     THE SUBROUTINE ALSO CHANGES THE SCREEN TO REFLECT
!     THESE CHANGES.  IF THE CALIBRATION IS NOT AVAILABLE FOR A
!     STATION, AN APPROPRIATE MESSAGE WILL BE PRINTED ON THE SCREEN.
!     THE SUBROUTINE WILL NOT "TURN OFF"  CALIBRATIONS WHICH CONFLICT
!     WITH A NEWLY APPLIED CALIBRATION, OR APPLY A NEW CALIBRATION IF
!     TURNING OFF THE OLD ONE LEFT A STATION WITHOUT ANY CALIBRATIONS
!     APPLIED TO IT.
!
! 1.2 REFERENCES:
!
! 2.  SCOR_CHALL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 JCAVAL(MAX_ARC_STA)
      INTEGER*2 JCSPCL,iallchangecalib,ifirststation,istationno,istand
      CHARACTER*1 CHANGEALLGOAL
!
! CHANGEALLGOAL - Specifies whether data will be changed to available (V)
!                 or applied (P) or else toggled from current state (T)
! IALLCHANGECALIB - Calibration to be changed for all stations
! IFIRSTSTATION - Index of first site to be displayed on this page
! ISTATIONNO - Actual number of stations being used (up to 32)
! ISTAND - Not used (used only by program SDBH)
! JCAVAL - List of available calibrations for each station
! JCSPL - Flag for calibrations which are unavailable, but which
!         can be applied anyway
!
! 2.3 OUTPUT Variables:
!
      integer*2 JCAPPL(MAX_ARC_STA)
      CHARACTER*15 JPRINT(MAX_ARC_STA)
!
! JCAPPL - List of calibrations originally applied for each station
! JPRINT - Stores and prints information about whether a calibration
!          is unavailable, available but not applied, or applied to
!          a station
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
      integer*4 iprintcoordx,iprintcoordy,imsgx,imsglin
      INTEGER*4 I4M1
      integer*2 n,ifirststatline,ifrstchrxind,ifldlenind
      integer*2 istatno,i2,i,iallchangestat,kbitn,istat1,istatl
      character*79 bufstr
      CHARACTER*9 BLANKVAR
      CHARACTER*1 UNAFFECT
      INTEGER*2 NUM_PER_SCREEN
      LOGICAL*2 KBIT
      DATA I4M1 / -1 /
      DATA BLANKVAR /'         '/
      DATA IFIRSTSTATLINE/10/
      DATA IFRSTCHRXIND/13/
      DATA IFLDLENIND/5/
      DATA IMSGX/0/, IMSGLIN/23/
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replaced -1J with I4M1
!   kdb   950809   Fix error in which changing all sites to applied or
!                  not applied only affects sites on the current display page.
!                  Finish bringing up to 32 sites
!
! 5.  SCOR_CHALL PROGRAM STRUCTURE
!
      UNAFFECT = 'N'
      NUM_PER_SCREEN = 10 !number of sites per screen
!
!     IFIRSTSTATION = FIRST STATION ON DISPLAY PAGE
!     ISTATL = LAST STATION ON DISPLAY PAGE
!     ISTAT1 = FIRST STATION TO AFFECT
!     ISTATNO = LAST STATION TO AFFECT
!
      IF (IFIRSTSTATION .EQ. 1 .AND. ISTATIONNO .GE. NUM_PER_SCREEN+ &
     &        1)THEN
        ISTATL = NUM_PER_SCREEN
      ELSE
        ISTATL = ISTATIONNO
      END IF
      istat1 = 1
      istatno = istationno
!
      IPRINTCOORDX = IFRSTCHRXIND + (IALLCHANGECALIB - 1) * IFLDLENIND
!
      I2 = 1
      DO 140 I = istat1, ISTATNO
!       Set y coordinate which will be used to print the new status
!       IF this site is on the display page (which will be determined
!       a little later).
        IPRINTCOORDY = IFIRSTSTATLINE + (I2 - 1)
        IALLCHANGESTAT = I
!
!     IF THE CALIBRATION IS AVAILABLE FOR THE STATION OR IS A SPECIAL
!     CASE AND CAN BE CHANGED FOR ALL STATIONS, MAKE
!     SURE IT IS NOW AT WHATEVER STATUS (APPLIED OR JUST AVAILABLE)
!     THAT THE USER SELECTED.
!
        IF (KBIT (JCAVAL(IALLCHANGESTAT), IALLCHANGECALIB) .OR. &
     &      KBIT (JCSPCL, IALLCHANGECALIB)) THEN
          IF (CHANGEALLGOAL .EQ. 'P') THEN
            CALL SBIT ( JCAPPL(IALLCHANGESTAT), IALLCHANGECALIB, INT2(1) )
            JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = 'P'
          ELSE if (changeallgoal.eq.'V') then
            CALL SBIT ( JCAPPL(IALLCHANGESTAT), IALLCHANGECALIB, INT2(0) )
            IF (KBIT(JCSPCL,IALLCHANGECALIB)) THEN
              JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = '-'
            ELSE
              JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = 'V'
            endif
          else if (changeallgoal.eq.'T') then
              n=kbitn(jcappl(iallchangestat),iallchangecalib)
              if (n.eq.0) then
                CALL SBIT ( JCAPPL(IALLCHANGESTAT), IALLCHANGECALIB, INT2(1) )
                JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = 'P'
              else
                CALL SBIT ( JCAPPL(IALLCHANGESTAT), IALLCHANGECALIB, INT2(0) )
                JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = 'V'
              endif
          END IF
          if (i.lt.ifirststation.or.i.gt.istatl) iprintcoordy=0
!
!     CHANGE THE SYMBOL OF THE CALIBRATION ON THE SCREEN TO
!     TO ACKNOWLEDGE THE CALIBRATION'S NEW STATUS.  SINCE THE USER
!     HAS CHANGED A CALIBRATION, THE DEFAULT CALIBRATIONS ORIGINALLY
!     PASSED TO SELCOR NO LONGER NECESSARILY EXIST.  CHANGE ISTAND
!     TO 0, SO THAT IF SELCOR WAS CALLED FROM SDBH, SDBH WILL NOT
!     PRINT A MESSAGE SAYING THAT THE STANDARD, DEFAULT CALIBRATIONS
!     ARE STILL APPLIED, WHEN SELCOR RETURNS.
!
        if (iprintcoordy.gt.0) then
          CALL setcr_mn (IPRINTCOORDX, IPRINTCOORDY )
          WRITE (bufstr, &
     &       1111)JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB)
 1111     FORMAT (A1)
          call addstr_f(bufstr(1:1) )
          CALL setcr_mn (IPRINTCOORDX - I4M1, IPRINTCOORDY )
          IF (ISTAND .NE. 0) ISTAND = 0
          I2 = I2 + 1
        endif
        ELSE
!
!         The calibration is not available for this station, nor can it be
!         applied on the fly
!
          IF (CHANGEALLGOAL .EQ. 'V') THEN
!
!           Permit backwards compatibility; some "special", on the fly
!           calibrations may be discontinued, but old data bases may be
!           brought in with these calibrations turned on.  This will let
!           the user turn them off.
!           Note: no longer applicable under namfil/corfil scheme of 7/91,
!           but not a problem.
!
            CALL SBIT ( JCAPPL(IALLCHANGESTAT), IALLCHANGECALIB, INT2(0) )
            JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB) &
     &             = '-'
            if (i.ge.ifirststation.and.i.le.istatl) then
              CALL setcr_mn (IPRINTCOORDX, IPRINTCOORDY )
              WRITE (bufstr, &
     &          1111)JPRINT(IALLCHANGESTAT)(IALLCHANGECALIB:IALLCHANGECALIB)
              call addstr_f(bufstr(1:1) )
              CALL setcr_mn (IPRINTCOORDX - I4M1, IPRINTCOORDY )
            end if
            IF (ISTAND .NE. 0) ISTAND = 0
          ELSE
            UNAFFECT = 'Y'
          END IF
          if (i.ge.ifirststation.and.i.le.istatl) then
            I2 = I2 + 1
          endif
        END IF
 140  CONTINUE
!
!     IF THE CALIBRATION WAS UNAVAILABLE FOR SOME OF THE STATIONS,
!     NOTIFY THE USER.
!
      IF (UNAFFECT .EQ. 'Y') THEN
        CALL setcr_mn (IMSGX, IMSGLIN )
        call addstr_f("SOME STATIONS UNAFFECTED" )
      END IF
!
      RETURN
      END
