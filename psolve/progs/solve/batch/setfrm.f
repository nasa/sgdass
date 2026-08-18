      SUBROUTINE SETFRM(SEC,FRAME)
      IMPLICIT NONE
!
! 1.  SETFRM PROGRAM SPECIFICATION
!
! 1.1 Set to next frame in DCALI.
!
! 1.2 REFERENCES:
!
! 2.  SETFRM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 SEC,FRAME
!
! FRAME - Current frame index (go to first frame this section if zero)
! SEC - current section index
!
! 2.3 OUTPUT Variables:
!
! FRAME - The index of the next frame (negative if no frames remain in
!         this section)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: fndcls,fndlst
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETFRM PROGRAM STRUCTURE
!
! FRAME < 0 IS THE IMPLICIT ELSE OF THIS BLOCK
!
      IF(SEC.LE.0) THEN
        FRAME=-1
      ELSE IF(FRAME.EQ.0) THEN
        FRAME=SEC
      ELSE IF(FRAME.GT.0) THEN
        IF(IACALI(FRAME).GT.0) THEN
          FRAME=FRAME+IACALI(FRAME)*4+1
        ELSE IF(IACALI(FRAME).LT.0) THEN
          FRAME=-FRAME
        ENDIF
      ENDIF
!
      RETURN
      END
