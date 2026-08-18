      SUBROUTINE FATAL_FILE ( IERR4, STR, FNAME, WHO )
      IMPLICIT NONE
!
! 1.  FATAL_FILE PROGRAM SPECIFICATION
!
! 1.1 Display the provided message in case of a file access error.
!
! 1.2 REFERENCES:
!
! 2.  FATAL_FILE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      character*(*) str,fname,who
      integer*4 ierr4
!
! FNAME - File name
! IERR4 - Error type (>= 0 means no error)
! STR - Message to be displayed
! WHO - Name of routine in which error is detected
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_perror,fatal_w
!
! 3.  LOCAL VARIABLES
!
      integer*2 trimlen
      character*1024 output
      character*1 chr
      INTEGER*4   CHRP, IERRIN, IT1, IT2
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  FATAL_FILE PROGRAM STRUCTURE
!
! First check whether we have an error
!
      IF ( IERR4 .GE. 0 ) RETURN
      IERRIN = NULL_TERM ( CHR, ' ' )
      IF ( IERRIN .EQ. 0 ) THEN
           CALL FC_PERROR( PTR_CH(CHR) )
        ELSE
           WRITE ( *, '("null_term failed in fatal_file")' )
      ENDIF
      IT1 = TRIMLEN(STR)
      IT2 = TRIMLEN(FNAME)
      IF ( IT1 == 0 ) IT1 = 1
      IF ( IT2 == 0 ) IT2 = 1
      OUTPUT = STR(1:IT1)//' '//FNAME(1:IT2)
      CALL FATAL_W ( OUTPUT, WHO )
!
      RETURN
      END  !#!  FATAL_FILE  #!#
