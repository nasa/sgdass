      SUBROUTINE NEWSEC(CNTCAL)
      IMPLICIT NONE
!
! 1.  NEWSEC PROGRAM SPECIFICATION
!
! 1.1 Advance the calibration array one frame ending a section.
!
! 1.2 REFERENCES:
!
! 2.  NEWSEC INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 CNTCAL
!
! CNTCAL - Array position of current frame
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcalib
!       CALLED SUBROUTINES: newfrm
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 SECTIN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  NEWSEC PROGRAM STRUCTURE
!
      SECTIN=CNTCAL
      CALL NEWFRM(CNTCAL)
      IACALI(SECTIN)=-ABS(IACALI(SECTIN))
!
      RETURN
      END
