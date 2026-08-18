      SUBROUTINE WRITENAMFIL ( IERR )
      IMPLICIT NONE
!
! 1.  WRITENAMFIL PROGRAM SPECIFICATION
!
! 1.1 Low level write to NAMFIL.
!
! 1.2 REFERENCES:
!
! 2.  WRITENAMFIL INTERFACE
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
! IERR - Error return to calling routine (-2 = error condition)
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
      INTEGER*4 IOS, I
!
! I - Loop index
! IOS - IOSTAT return from WRITE
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  WRITENAMFIL PROGRAM STRUCTURE
!
      LHOLD = '    '
      IERR=0
      WRITE(UNITNAM,IOSTAT=IOS,REC=IREC) (LBUF(I),I=1,JNAMREC_WORDS)
      IF(IOS.EQ.0)  LHOLD = KBUF(1:4)
      IF(IOS.ne.0) IERR = -2
!
      RETURN
      END  !#!  WRITENAMFIL   #!#
