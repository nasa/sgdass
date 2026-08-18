      SUBROUTINE SCORF_CHIND (INDSTAT, INDCAL, &
     &                             JCAFFL, JPRINT, &
     &                             IPRINTCOORDX, IPRINTCOORDY)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCORF_CHIND PROGRAM SPECIFICATION
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.1
!     THE SUBROUTINE APPLIES THE CALIBRATION IF NOT YET APPLIED, OR
!     TERMINATES ITS APPLICATION IF ALREADY APPLIED.
!     THE SUBROUTINE ALSO CHANGES THE
!     SCREEN TO REFLECT THE CHANGE.  AT THE PRESENT
!     TIME, THE SUBROUTINE DOES NOT "TURN OFF" CALIBRATIONS WHICH
!     CONFLICT WITH THE NEW CALIBRATION, OR APPLY A NEW CALIBRATION IF
!     TURNING OFF THE OLD ONE LEFT THE STATION WITHOUT ANY
!     CALIBRATIONS APPLIED TO IT.
!
!
! 1.2 REFERENCES:
!
! 2.  SCORF_CHIND INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4  iprintcoordx,iprintcoordy
      INTEGER*2 INDSTAT,INDCAL
!
! INDCAL - Calibration to be changed for the specified station
! INDSTAT - Station for which calibration is to be changed
! IPRINTCOORDX/Y - X, Y coordinates of field which gives the status of
!                  the given calibration for the given station
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 JCAFFL(7,MAX_ARC_STA)
      CHARACTER*112 JPRINT(MAX_ARC_STA)
!
! JCAFFL - bit array showing whether or not a given cal is applied at a
!          given station.  bit on = applied, off = not applied
! JPRINT - corresponds to jcaffl.  P = applied V = not applied
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'ioncm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
      character*79 bufstr
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  7/19/91 CREATED
!
! 5.  SCORF_CHIND PROGRAM STRUCTURE
!
!     IF THE CALIBRATION IS ALREADY APPLIED, TURN IT OFF AND CHANGE
!     ITS SYMBOL ON THE SCREEN TO INDICATE IT'S NO LONGER APPLIED
!
      IF (KBIT (JCAFFL(1,INDSTAT), INDCAL)) THEN
        CALL SBIT ( JCAFFL(1,INDSTAT), INDCAL, INT2(0) )
        JPRINT(INDSTAT)(INDCAL:INDCAL) = 'V'
!
!     IF THE CALIBRATION IS NOT YET APPLIED, APPLY IT AND CHANGE ITS
!     SYMBOL ON THE SCREEN TO INDICATE IT'S APPLIED
!
      ELSE
        CALL SBIT ( JCAFFL(1,INDSTAT), INDCAL, INT2(1) )
        JPRINT(INDSTAT)(INDCAL:INDCAL) = 'P'
      END IF
!
!     CHANGE THE SYMBOL OF THE CALIBRATION ON THE SCREEN TO
!     ACKNOWLEDGE THE CALIBRATION'S NEW STATUS.
!
      CALL setcr_mn (IPRINTCOORDX, IPRINTCOORDY )
      WRITE (bufstr, &
     &     1110)JPRINT(INDSTAT)(INDCAL:INDCAL)
      call addstr_f(bufstr(1:1) )
 1110 FORMAT (A1)
      CALL setcr_mn (IPRINTCOORDX - 1, IPRINTCOORDY )
!
      RETURN
      END
