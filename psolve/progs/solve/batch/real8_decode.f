      REAL*8 FUNCTION REAL8_DECODE(STRING,IERR)
      IMPLICIT NONE
!
! 1.  REAL8_DECODE PROGRAM SPECIFICATION
!
! 1.1 Convert an ASCII string to a real*8 value.
!
! 1.2 REFERENCES:
!
! 2.  REAL8_DECODE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Input string to be converted
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR - Error return (0 = okay; -1 = read error)
! REAL8_DECODE - Real*8 version of number represented by STRING
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gtelev
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  REAL8_DECODE PROGRAM STRUCTURE
!
! Simply do a read into the real*8 variable
!
      READ(STRING,*,ERR=900) REAL8_DECODE
      IERR=0
      RETURN
!
! Error return if read has a problem
!
900   CONTINUE
      IERR=-1
!
      RETURN
      END
