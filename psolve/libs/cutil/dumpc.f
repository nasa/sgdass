      SUBROUTINE DUMPC(NAME, NPARM, IXREF)
      IMPLICIT NONE
!
! 1.  DUMPC PROGRAM SPECIFICATION
!
! 1.1 Dump data from integer array to the printer (LU 7)
!
! 1.2 REFERENCES:
!
! 2.  DUMPC INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NPARM,IXREF(NPARM)
      CHARACTER*(*) NAME
!
! IXREF - The array to be dumped
! NAME - Name to be printed as header
! NPARM - Number of values to be dumped
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
      INTEGER*2 JEJH
!
! JEJH - Loop index
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DUMPC PROGRAM STRUCTURE
!
      WRITE (7,1111) NAME
      WRITE (7,1956) (IXREF(JEJH),JEJH=1,NPARM)
 1111 FORMAT ("1",2X,A)
 1956 FORMAT(20(" ",10I8/))
      RETURN
      END
