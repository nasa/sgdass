      SUBROUTINE DUMPS(STANAM,MASPOS,TSTPOS,PARTYP,STAXRF,NSTA,IPAR)
      IMPLICIT NONE
!
! 1.  DUMPS PROGRAM SPECIFICATION
!
! 1.1 Dump station and parameter information to the screen.
!
! 1.2 REFERENCES:
!
! 2.  DUMPS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*8  STANAM(*)
      CHARACTER*(*)          PARTYP(*)
      INTEGER*2 STAXRF(*),NSTA,IPAR
      REAL*8 MASPOS(3,*),TSTPOS(3,*)
!
! IPAR - Number of parameters
! MASPOS - Station positions in master CGM
! NSTA - Number of stations
! PARTYP - Parameter types
! STANAM - Station names
! STAXRF - Cross-reference from parameter number to station number
! TSTPOS - Station positions in test CGM
!
! 2.3 OUTPUT Variables: None
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
      INTEGER*2 I,IEJH
      character*80 bufstr
!
! I,IEJH - Loop indices
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DUMPS PROGRAM STRUCTURE
!
!
      WRITE(*,'("1STATION INFORMATION, NSTA= ",I5)')NSTA
      WRITE(*,1966) (STANAM(IEJH),(MASPOS(I,IEJH),I=1,3), &
     &               (TSTPOS(I,IEJH),I=1,3),IEJH=1,NSTA)
!
      IF(IPAR.EQ.0) RETURN
      WRITE(*,'("1PARAMETER INFORMATION, IPAR= ",I5)')IPAR
      WRITE(*,1956) (PARTYP(IEJH),STAXRF(IEJH),IEJH=1,IPAR)
!
 1956 FORMAT(" ",10("   '",A,"' ",I4))
 1966 FORMAT(" '",A,"' ",6F20.5)
      RETURN
      END
