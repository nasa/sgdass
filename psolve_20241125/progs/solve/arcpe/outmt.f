      SUBROUTINE OUTMT ( SCALE, NPARM, STACM )
      IMPLICIT NONE
!
! 1.  OUTMT PROGRAM SPECIFICATION
!
! 1.1 Write out the provided matrix to the arc file.
!
! 1.2 REFERENCES:
!
! 2.  OUTMT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NPARM
      LOGICAL*2 STACM
      REAL*8 SCALE(*)
!
! NPARM - Number of parameters
! SCALE - Matrix to be written to the arc file
! STACM - True if this arc's commons are to be written to the arc file
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO  WHEN   WHAT
!
! 5.  OUTMT PROGRAM STRUCTURE
!
!!      IF ( .NOT. STACM ) CALL ACS_ARCFIL ( SAVAF, STACM, 'O' )
      CALL ACS_ARCFIL   ( SAVAF, STACM, 'O' )
      CALL USE_ARCF_MAT ( SCALE, NPARM, 'W' )
      CALL ACS_ARCFIL   ( SAVAF, STACM, 'C' )
!
      RETURN
      END  !#!  OUTMT  #!#
