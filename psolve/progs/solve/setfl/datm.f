      SUBROUTINE DATM(ISITE,IATLN,ipagea,listlen)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  DATM PROGRAM SPECIFICATION
!
! 1.1 Delete atmosphere parameter epochs.
!
! 1.2 REFERENCES:
!
! 2.  DATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITE,ipagea,listlen
      INTEGER*4 IATLN
!
! IATLN - Line of screen at which cursor will be placed for selection
! ISITE - Site number of station being processed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: posn
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 IQUIT
      INTEGER*2 IDSP(16),KSTA,J,KBITN,K,ICHR
      character*2 ich
      equivalence (ich,ichr)
      INTEGER*4  I4P2, I4P50
      DATA  I4P2, I4P50 / 2, 50 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  851112  Structures inserted, mask logic removed
!
! 5.  DATM PROGRAM STRUCTURE
!
!     Determine whether this is the last atmosphere at this site. Don't
!     delete it.
!
      IF(NUMATM(ISITE).LE.1) RETURN
!
      CALL setcr_mn(I4P50,I4P2 )
      call reverse_on_mn()
      call blink_on_mn()
      call addstr_f("Delete atmospheres" )
      call blink_off_mn()
      call reverse_off_mn()
!
!     Within the list of atmosphere epochs for this site, determine the
!     number of the epoch to be deleted. POSN checks that the number
!     is legal. If not legal or user wants to get out return.
!
      IQUIT = .TRUE.
!      DO WHILE (IQUIT .AND. NUMATM(ISITE).GT.1)
!       DO BEGIN looking
          CALL POSN(IATLN,IATSTR(ISITE),NUMATM(ISITE),K,ICHR )
          k = k + (ipagea-1)*listlen
          IF(ICH(1:1).eq.' '.and.k.gt.0)  THEN
!           THEN BEGIN delete an epoch
!
!             Delete the line the epoch is on and move up the screen
!
!             K is the location of the atmosphere epoch to be deleted
!
!             Move the epochs and the flags for the atms succeeding
!
              DO J=K,IATSTR(NUMSTA)+NUMATM(NUMSTA)-1
!               DO BEGIN moving
                  TATM(J) = TATM(J+1)
                  CALL SBIT(LATM(1,1),J,KBITN( LATM(1,1), INT2(J+1)) )
                  CALL SBIT(LATM(1,2),J,KBITN( LATM(1,2), INT2(J+1)) )
                  CALL SBIT(LATM(1,3),J,KBITN( LATM(1,3), INT2(J+1)) )
!                 ENDF moving
              END DO
!
!             Decrement the counters for the succeeding stations
!
              IF (ISITE.LE.NUMSTA)  THEN
!               THEN BEGIN this is not the last site
                  DO KSTA = ISITE+1,NUMSTA
                    IATSTR(KSTA) = IATSTR(KSTA) - 1
                  END DO
!                 ENDT this is not the last site
!
!             Decrement the atmosphere counter for this station
!
              END IF
              NUMATM(ISITE) = NUMATM(ISITE) - 1
!             ENDT delete an epoch
          ELSE
            IQUIT = .FALSE.
          END IF
!         ENDW looking
!      END DO
!
      CALL setcr_mn(I4P50,I4P2 )
      call addstr_f("                    " )
!
32767 RETURN
      END
