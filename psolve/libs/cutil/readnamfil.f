      SUBROUTINE READNAMFIL(IERR)
      Implicit NONE
!
! 1.  READNAMFIL PROGRAM SPECIFICATION
!
! 1.1 NAMFIL access routine.  This is the lowest level card reading routine.
!
! 1.2 REFERENCES:
!
! 2.  READNAMFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR - Error return (0 = no error; -2 = error in READ
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'namfl.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  I
      INTEGER*4  IOS
!
! I - Loop index
! IOS - IOSTAT return from READ
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  READNAMFIL PROGRAM STRUCTURE
!
      LHOLD = '    '
      READ(UNITNAM,IOSTAT=IOS,REC=IREC) (LBUF(I),I=1,JNAMREC_WORDS)
      IERR=0
      IF(IOS.EQ.0)  LHOLD = KBUF(1:4)
      IF(IOS.ne.0) IERR = -2
      RETURN
      END
