      SUBROUTINE SETUP_PRELUDE(LETRX)
      IMPLICIT NONE
!
! 1.  SETUP_PRELUDE PROGRAM SPECIFICATION
!
! 1.1 Set up prelude common (precm).
!
! 1.2 REFERENCES:
!
! 2.  SETUP_PRELUDE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      integer*2 letrx
!
! LETRX - User initials
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: unpack_rmpar,fc_umask
!
! 3.  LOCAL VARIABLES
!
      integer*4 int4,iret
      INTEGER*2 IP(5),I
      LOGICAL*2 KBIT
!
! I - Loop index
! IP - RMPAR variables
! IRET - Return from FC_UMASK
! INT4 - Argument sent to FC_UMASK (0)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  SETUP_PRELUDE PROGRAM STRUCTURE
!
!  SET UP PRELUDE COMMON BY HAND FOR NOW
!
      DO I=1,5
        IP(I)=0
      ENDDO
      IP(5)=LETRX
      CALL UNPACK_RMPAR(IP)
!
!  also set umask so that permissions are set explicitly at creation
!
      int4=0
      iret=fc_umask(int4)
!
      RETURN
      END
