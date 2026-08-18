      SUBROUTINE PRE_PROG()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PRE_PROG PROGRAM SPECIFICATION
!
! 1.1 Standard prelude for all SOLVE programs except ENTER and
!     SOLVE.  Pick up the standard RMPAR parameters and set up
!     the PRELUDE_COM common block.
!     NOTE: A call to PRE_PROG should be the first executable
!     statement in a program.
!
! 1.2 REFERENCES:
!
! 2.  PRE_PROG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IP(5)
!
! IP - Input parameters picked up by RMPAR
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'precm.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'curlib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: rmpar,unpack_rmpar
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KOPEN
      COMMON/BUFFCM/KOPEN
!
      LOGICAL*2 KBIT
      character*63 bufstr
!
! KOPEN - .FALSE. indicates that buffer is not open
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet   12-APR-99  Added initialization of curses flag
!
! 5. PRE_PROG PROGRAM STRUCTURE
!
!  GET RMPAR AND UNPACK EACH WORD
!
      CALL RMPAR(IP )
      IF ( KBIT ( IP(2), INT2(5)) ) THEN
           CALL START_MN()
           CALL ADDSTR_F ( "xdb pause" )
           CALL GETSTR_F ( BUFSTR )
           READ ( BUFSTR, '(A)' )
           CALL END_MN()
      ENDIF
!
      CALL UNPACK_RMPAR ( IP )
!
      KOPEN=.FALSE.
      PIPE_IDS(1)=3
      PIPE_IDS(2)=4
      PIPE_IDS(3)=5
      PIPE_IDS(4)=6
      SOCOM_PLUS_FIRST = SPL__UNDF  !  Initialization of socom_plus
      CURLIB_FLAG      = CRS__UND   !  Initiaze curses flag
!
      RETURN
      END  !#!  PRE_PROG  #!#
