      SUBROUTINE fatal(str)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FATAL PROGRAM SPECIFICATION
!
! 1.1 Report a fatal error and exit.
!
! 1.2 REFERENCES:
!
! 2.  FATAL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      character*(*) str
!
! STR - String with message to be displayed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_exit,rcpar
!
! 3.  LOCAL VARIABLES
!
      character*128 name
      integer*2 trimlen, it, is
      INTEGER*4 IARR(2), IP
!
! I4 - Argument for call to FC_EXIT (-1)
! NAME - Name of currently executing program
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FATAL PROGRAM STRUCTURE
!
! Get the program name, display the message and exit
!
      CALL RCPAR( INT2(0), NAME )
      IT = TRIMLEN(NAME)
      IS = TRIMLEN(STR)
      IF ( IT .LE. 0 ) IT = 1
      IF ( IS .LE. 0 ) IS = 1
      WRITE ( *, * ) ' '
      WRITE ( *, * ) ' '
      WRITE ( *,'(A,": ",A)' ) NAME(1:IT), STR(1:IS)
      WRITE ( *, * ) 'Stack unwind: '
      WRITE ( *, * ) ' '
      WRITE ( *, * ) ' '
!
! --- This line is for deliberate crashing and unwinding the stack
!
      IP = -3
      IARR(IP) = IP
      CALL FC_EXIT( -1 )
      RETURN
      END  SUBROUTINE  !#!# FATAL  #!##
