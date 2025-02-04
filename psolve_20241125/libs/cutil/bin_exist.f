      SUBROUTINE BIN_EXIST(FNAME,KEXIST)
      IMPLICIT NONE
!
! 1.  BIN_EXIST PROGRAM SPECIFICATION
!
! 1.1 Check on the existence of a specified file
!
! 1.2 REFERENCES:
!
! 2.  BIN_EXIST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME
!
! FNAME - Name of file to be checked
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 KEXIST
!
! KEXIST - TRUE if file exists, FALSE if not
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*4 K4
!
! K4 - Value returned by the EXIST specifier of the INQUIRE statement
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  BIN_EXIST PROGRAM STRUCTURE
!
      INQUIRE(FILE=FNAME,EXIST=K4)
      KEXIST=K4
!
      END
