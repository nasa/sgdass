      SUBROUTINE MAKE_PIPES()
      IMPLICIT NONE
!
! 1.  MAKE_PIPES PROGRAM SPECIFICATION
!
! 1.1 Set up parameter pipes.
!
! 1.2 REFERENCES:
!
! 2.  MAKE_PIPES INTERFACE
!
! 2.1 Parameter File
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
!       CALLED SUBROUTINES: fc_pipe,fatal_file,fc_fcntl,fc_const_g,fc_pipe,
!                           fatal_w
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4    IERR4, CMD, FLAGS
      CHARACTER*10 ME
      DATA ME   / 'make_pipes' /
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   AEE   920204      Removed hard coded path for fclib.i
!   pet   2001.02.26  Fixed a bug: there was function ptr_ch in one place which
!                     requires ptr_nc. Improved comments.
!   pet   2007.06.21  It tuned out that at some systems the pipes may have
!                     different numbers than 3,4,5,6. Added support of this 
!                     case
!
! MAKE_PIPES PROGRAM STRUCTURE
!
!
!CCCC
!
!
! --- Allocate parameter pipes to (1) and (2)
!
      IERR4=FC_PIPE ( PTR_NC(PIPE_IDS) )
      CALL FATAL_FILE ( IERR4, 'creating', 'parameter pipes', ME )
!
! --- get current flags for pipe 1
!
      IERR4=FC_CONST_G ( PTR_CH('F_GETFL'), CMD )
      CALL FATAL_file ( IERR4, 'getting F_GETFL', 'parameter pipes', ME )
      IERR4 = FC_FCNTL ( PIPE_IDS(1), CMD, CMD )
      CALL FATAL_FILE ( IERR4, 'reading flags', 'parameter pipes', ME )
!
! --- Set O_NDELAY
!
      IERR4 = FC_CONST_G ( PTR_CH ('O_NDELAY'), CMD )
      CALL FATAL_FILE ( IERR4, 'getting O_NDELAY', 'parameter pipes', ME )
      FLAGS = IOR ( IERR4, CMD )
!
! --- Update flag values
!
      IERR4 = FC_CONST_G ( PTR_CH('F_SETFL'), CMD )
      CALL FATAL_FILE  ( IERR4, 'getting F_SETFL', 'parameter pipes', ME )
      IERR4 = FC_FCNTL ( PIPE_IDS(1), CMD, FLAGS )
      CALL FATAL_FILE  ( IERR4, 'setting flags', 'parameter pipes', ME )
!
! --- Get buffer pipes into (3) and (4)
!
      IERR4=FC_PIPE ( PTR_NC(PIPE_IDS(3)) )
      CALL FATAL_FILE ( IERR4, 'creating', 'buffer pipes', ME )
!
! --- Get current flags for pipe 3
!
      IERR4 = FC_CONST_G ( PTR_CH('F_GETFL'), CMD )
      CALL FATAL_FILE ( IERR4, 'getting F_GETFL', 'buffer pipes', ME )
      IERR4 = FC_FCNTL ( PIPE_IDS(3), CMD, CMD )
      CALL FATAL_file ( IERR4, 'reading flags', 'buffer pipes', ME )
!
! --- Set O_NDELAY
!
      IERR4 = FC_CONST_G ( PTR_CH ('O_NDELAY'), CMD )
      CALL FATAL_FILE ( IERR4, 'getting O_NDELAY', 'buffer pipes', ME )
      FLAGS = IOR ( IERR4, CMD )
!
! --- Update flag values
!
      IERR4 = FC_CONST_G ( PTR_CH('F_SETFL'), CMD )
      CALL FATAL_FILE ( IERR4, 'getting F_SETFL', 'buffer pipes', ME )
      IERR4 = FC_FCNTL ( PIPE_IDS(3), CMD, FLAGS )
      CALL FATAL_FILE ( IERR4, 'setting flags', 'buffer pipes', ME )
! 
      IF ( PIPE_IDS(1) .LE. 0  .OR.  &
     &     PIPE_IDS(2) .LE. 0  .OR.  & 
     &     PIPE_IDS(3) .LE. 0  .OR.  & 
     &     PIPE_IDS(4) .LE. 0        ) THEN
!
           WRITE ( 6, * )  ' PIPE_IDS = ', PIPE_IDS
           CALL FATAL_W ( 'incorrect ids', ME )
      ENDIF
!
      RETURN
      END  !#!  MAKE_PIPES  #!#
