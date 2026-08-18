      SUBROUTINE SCOR_ALLGOAL (CHANGEALLGOAL, ICALIBNO)
      implicit none
!
! 1.  SCOR_ALLGOAL PROGRAM SPECIFICATION
!
! 1.1
!     THIS SUBROUTINE PRINTS THE FIELD WHICH ALLOWS THE USER TO SAY
!     WHETHER DATA WILL BE CHANGED TO "APPLIED" OR TO "AVAILABLE",
!     WHEN THE USER CHOOSES TO CHANGE ALL STATIONS FOR A CALIBRATION.
!
! 1.2 REFERENCES:
!
! 2.  SCOR_ALLGOAL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 CHANGEALLGOAL
      integer*2 icalibno
!
! CHANGEALLGOAL - Flag to indicate changeing to 'applied' (P) or
!                 'available' (V)
! ICALIBNO - Calibration number
!
! 2.3 OUTPUT Variables: None
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
      integer*2 ifldlenall,ifrstallline,ifrstloxall
      integer*4 ix,iy
      DATA IFLDLENALL/13/
      DATA IFRSTALLLINE/27/
      DATA IFRSTLOXALL/16/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SCOR_ALLGOAL PROGRAM STRUCTURE
!
! Position cursor
!
!      IX = 0
!      IY = IFRSTALLLINE
!      CALL setcr_mn (IX, IY)
!C
!C Display appropriate message on the screen
!C
!      IF (CHANGEALLGOAL .EQ. 'P') THEN
!        call addstr_f("(/) Apply       ")
!      ELSE
!        call addstr_f("(/) Don't apply ")
!      END IF
!C
!      IY = IFRSTALLLINE + 1
!      CALL setcr_mn (IX, IY)
!C
!      IF (CHANGEALLGOAL .EQ. 'P') THEN
!        call addstr_f("to all stations ")
!      ELSE
!        call addstr_f("to any station  ")
!      END IF
      IF ( CHANGEALLGOAL .EQ. 'P' ) THEN
           CALL ADDSTR_F ( "(/) Apply to all stations " )
        ELSE
           CALL ADDSTR_F ( "(/) Don't apply to any station" )
      END IF
!C
!C     THE USER WILL PROBABLY CHANGE ALL STATIONS FOR A CALIBRATION,
!     NEXT.  POSITION THE CURSOR SO THAT THE USER CAN CONVENIENTLY
!     ENTER SUCH A COMMAND USING A BLANK.
!
      IF (ICALIBNO .LE. 5) THEN
        IX = IFRSTLOXALL + (IFLDLENALL/2) + (ICALIBNO/2) * IFLDLENALL
      ELSE
        IX = IFRSTLOXALL + (IFLDLENALL/2) + 2 * IFLDLENALL
      END IF
!
      IY = IFRSTALLLINE - 1
      CALL setcr_mn (IX, IY)
      RETURN
      END
