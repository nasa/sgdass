      SUBROUTINE WHERE_ARC(ARCNAME,FNAME)
      IMPLICIT NONE
!
! 1.  WHERE_ARC PROGRAM SPECIFICATION
!
! 1.1 Form complete arcfile name including path
!
! 1.2 REFERENCES:
!
! 2.  WHERE_ARC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) ARCNAME
!
! ARCNAME - String form of number of saved arcfile
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) FNAME
!
! FNAME - Complete arc file name including path
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arc_i
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*64 APPEND
      INTEGER*2 I
      LOGICAL*4 LXT
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  WHERE_ARC PROGRAM STRUCTURE
!
      DO I=1,3
        IF(ARCDIR(I).EQ.' ') THEN
          FNAME=' '
          RETURN
        ENDIF
        FNAME = APPEND(ARCDIR(I),ARCNAME)
        INQUIRE(FILE=FNAME,EXIST=LXT)
        IF(LXT) RETURN
      ENDDO
!
      FNAME=' '
      RETURN
      END
