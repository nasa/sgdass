      SUBROUTINE FATAL_W ( STR, WHO )
      IMPLICIT NONE
!
! 1.  FATAL_W PROGRAM SPECIFICATION
!
! 1.1 Format fatal error message.
!
! 1.2 REFERENCES:
!
! 2.  FATAL_W INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      character*(*) str,who
!
! STR - Main body of message
! WHO - Routine in which error was detected
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fatal
!
! 3.  LOCAL VARIABLES
!
!
      integer*2 trimlen
      character*1024 output
      INTEGER*4  IT1, IT2
!
! OUTPUT - Formatted message to be sent along to FATAL
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FATAL_W PROGRAM STRUCTURE
!
! Format message and send off to FATAL
!
      IT1 = TRIMLEN(STR)
      IT2 = TRIMLEN(WHO)
      IF ( IT1 == 0 ) IT1 = 1
      IF ( IT2 == 0 ) IT2 = 1
      OUTPUT= 'error '//STR(1:IT1)//' in '//WHO(1:IT2)
      CALL FATAL(OUTPUT)
      RETURN
      END   SUBROUTINE   FATAL_W  !#!#
