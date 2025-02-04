      SUBROUTINE FNDLST(SEC,LSTFRM)
      IMPLICIT NONE
!
! 1.  FNDLST PROGRAM SPECIFICATION
!
! 1.1 Find the last frame in this section.
!
! 1.2 REFERENCES:
!
! 2.  FNDLST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 SEC
!
! SEC - Current section index
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSTFRM
!
! LSTFRM - The index of the last frame; negative if there are
!           no frames or empty section
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: fndcls
!       CALLED SUBROUTINES: setfrm
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 FRAME
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FNDLST PROGRAM STRUCTURE
!
      IF(SEC.LE.0) THEN
        LSTFRM=-1
        RETURN
      ENDIF
!
      FRAME=0
      LSTFRM=-1
      CALL SETFRM(SEC,FRAME)
      DO WHILE(FRAME.GT.0)
         LSTFRM=FRAME
         CALL SETFRM(SEC,FRAME)
      ENDDO
!
      RETURN
      END
