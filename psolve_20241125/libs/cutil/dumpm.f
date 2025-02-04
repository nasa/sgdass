      SUBROUTINE DUMPM (NAME, XMAT, NPARM, MPAR)
      implicit none
!
! 1.  DUMPM PROGRAM SPECIFICATION
!
! 1.1 Dump a SOLVE format matrix to the printer (LU 7)
!
! 1.2 REFERENCES:
!
! 2.  DUMPM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NPARM,MPAR
      REAL*8 XMAT(*)
      CHARACTER*(*) NAME
!
! NAME - Name to be printed as header
! MPAR - Maximum number of parameters
! NPARM - Number of parameters
! XMAT - Matrix
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
      INTEGER*2 I,ISIG,IB,IA,JEJH
      INTEGER*4 INDX4
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DUMPM PROGRAM STRUCTURE
!
         WRITE (7,1550) NAME
 1550    FORMAT("1",A)
         DO I = 1, NPARM
            ISIG = I + MPAR    !step over SCAL to SIG!
            IB = I + MPAR*2    !step over SCAL and SIG to B!
            IA = MPAR*3        !step over SCAL, SIG, & B to A!
            WRITE (7,1549) XMAT(I),XMAT(ISIG),XMAT(IB),I,(XMAT(IA+INDX4(I, &
     &      JEJH)),JEJH=1,I)
         ENDDO
 1549    FORMAT("0",3D25.17,I10/25(" ",5D25.17/))
      RETURN
      END
