      SUBROUTINE POSN(ILINE,ISTART,NUMBER,K,ICHAR)
      IMPLICIT NONE
!
! 1.  POSN PROGRAM SPECIFICATION
!
! 1.1 Select atmospheres and clocks for deletion.  The cursor will be
!     set to (0,ILINE+2), and the user will select the line to be
!     deleted.  Then the sequence number appropriate for this line
!     will be returned in K.  K will be set to -1 if an illegal position
!     is sensed.
!
! 1.2 REFERENCES:
!
! 2.  POSN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTART,NUMBER
      INTEGER*4 ILINE
!
! ILINE - Screen line at beginning of the section we're interested in
! ISTART - Number of position in bit array immediately before the position
!           where this station's flags begin
! NUMBER - Total number of epochs for this station
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 K,ICHAR
!
! ICHAR - Character sensed at the cursor's position
! K - Number of the epoch to be deleted
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: datm,dclk
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      integer*2 ich
      character*2 cch
      INTEGER*4 IY,IX,IT,ich4
      INTEGER*4 I4P0
      DATA  I4P0 / 0 /
      character*4 cch4
      equivalence (ich4,cch4)
      equivalence (ich,cch)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  POSN PROGRAM STRUCTURE
!
!     Set the curson position
!
      K  = -1
      IT = ILINE + 2
      CALL SETCR_MN(I4P0,IT)
!
!     Sense the curson position. If not a blank, get out.
!
100   CALL SENKR_MN(IX,IY,ICH4)
      cch(1:1) = cch4(4:4)
      ichar = ich
       if (cch(1:1).ne.' ') return
!
!     Check for legal curson position
!
      K = IY - ILINE
      IF (K.GT.NUMBER .OR. K.LT.0)  THEN
!       THEN BEGIN error exit
          K = -1
          RETURN
!         ENDT error exit
!
!     Calculate the desired postion
!
      END IF
      K = K + ISTART
!
32767 RETURN
      END
