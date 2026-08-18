      FUNCTION DATE_IN_BDS(DATE1,DATE2,DATET)
      IMPLICIT NONE
!
! 1.  DATE_IN_BDS PROGRAM SPECIFICATION
!
! 1.1 Check whether a given date falls within the boundaries of a
!     specified range.
!
! 1.2 REFERENCES:
!
! 2.  DATE_IN_BDS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 DATE1,DATE2,DATET
!
! DATE1 - Beginning of the specified range
! DATE2 - End of the specified range
! DATET - Date being checked
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 DATE_IN_BDS
!
! DATE_IN_BDS - True if DATET falls within the specified range
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
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DATE_IN_BDS PROGRAM STRUCTURE
!
      DATE_IN_BDS = (DATE1.LE.DATET).AND.(DATET.LT.DATE2)
      RETURN
      END
