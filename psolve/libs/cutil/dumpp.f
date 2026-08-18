      SUBROUTINE DUMPP(NAME, NPARM, IPARM)
      IMPLICIT NONE
!
! 1.  DUMPP PROGRAM SPECIFICATION
!
! 1.1 Dump list of parameter names to printer (LU 7)
!
! 1.2 REFERENCES:
!
! 2.  DUMPP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NPARM,IPARM(10,NPARM)
      CHARACTER*(*) NAME
!
! IPARM - The array of parametr names
! NAME - Name to be printed as header
! NPARM - Number of parameter names to be dumped
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
      INTEGER*2 IEJH,JEJH
!
! IEJH,JEJH - Loop indices
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DUMPP PROGRAM STRUCTURE
!
      WRITE (7,1111) NAME,NPARM
      WRITE (7,1956)   ((IPARM(IEJH,JEJH),IEJH=1,10),JEJH=1,NPARM)
 1111 FORMAT ("1 '",A,"'  NPARM = ",I7)
 1956 FORMAT(25(" ",10A2/))
      RETURN
      END
