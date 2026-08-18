      SUBROUTINE END_PROG()
      IMPLICIT NONE
!
! 1.  END_PROG PROGRAM SPECIFICATION
!
! 1.1 Prepare to exit program by filing away return parameters
!
! 1.2 REFERENCES:
!
! 2.  END_PROG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_write,fatal_file,fc_exit
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,IBUF(8)
      INTEGER*4 ierr, nbytes,JZERO
!
      DATA IBUF/2H  ,2H  ,2H  ,5*0/,nbytes/16/,JZERO/0/
!
! I - Loop index
! IBUF - Buffer to hold return parameters
! IERR - Return value from fc_write
! JZERO - Constant 0J
! NBYTES - Number of bytes in IBUF
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  END_PROG PROGRAM STRUCTURE
!
      DO I=1,5
         IBUF(I+3)=PRE_IP(I)
      ENDDO
!
      ierr = fc_write(PIPE_IDS(2),ptr_nc(ibuf),nbytes)
      call fatal_file(ierr,'writing','return params','end_porg')
      CALL FC_EXIT(JZERO)
      END
