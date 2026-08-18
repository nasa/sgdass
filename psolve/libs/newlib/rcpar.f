      SUBROUTINE RCPAR(INUM,STRING)
      IMPLICIT   NONE
      INTEGER*2  INUM
      CHARACTER  STRING*(*)
      INTEGER*4  IARGC
!
! RCPAR: return run string parameter INUM as STRING
!        truncate if longer than STRING, blank fill if shorter
!
! example: prog arg1 arg2 arg3
!
!          prog is argument 0
!
!          if there is no argument INUM, this routine is a no-op.
!
      INTEGER*4 J,NUM
!
      STRING = ' '
      NUM=IARGC()
      J=INUM
      IF ( NUM .LT. J  .OR.  INUM .LT. 0 ) RETURN
!
      CALL GETARG ( J, STRING )
      RETURN
      END
