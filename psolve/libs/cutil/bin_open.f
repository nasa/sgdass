      SUBROUTINE BIN_OPEN ( FNAME, FILDES, STRING )
      IMPLICIT NONE
!
! 1.  BIN_OPEN PROGRAM SPECIFICATION
!
! 1.1 Open a binary file by calling the C routine fc_open
!
! 1.2 REFERENCES:
!
! 2.  BIN_OPEN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,STRING
!
! FNAME - Name of file to be opened
! STRING - File access option (only 'O' is supported)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 FILDES
!
! FILDES - File descriptor returned from fc_open
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fatal_w,fc_open,fatal_file
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 OPEN_FLAG, MODE_FLAG, LN, IERR
      CHARACTER ME*8 
      INTEGER*2 TRIMLEN
      DATA ME  / 'BIN_OPEN' /
!
! ME - Name of this routine
! MODE - Mode specified in call to fc_open (0)
! OFLAG - Flag sent to fc_open (2)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!   ALF   12/19/92   Call fc_const_g to set OFLAG
!   KDB   951102 Call new version of fc_open (new subroutine) that does
!                not abend, providing time for solve to print more
!                diagnostics before stopping.
!   pet   2004.11.15  Replaced hard-conded constant with call to &
!                     GET_SYSTEM_CONSTANT
!
! 5.  BIN_OPEN PROGRAM STRUCTURE
!
!CCCC
!
! --- Check for legal string option (only 'O' is supported)
!
      IF ( STRING .NE. 'O' ) THEN
           CALL FATAL_W ( 'Illegal string: '//STRING, ME )
      ENDIF
!
! --- Open file, and report problem, if any
!
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR', OPEN_FLAG, LN )
      MODE_FLAG = 0 
!
! --- Check to prevent indexing error
!
      IF ( TRIMLEN(FNAME) .EQ. 0 ) THEN
           CALL FATAL_W ( 'Empty file name', ME )
      END IF
      FILDES = FCIV_OPEN ( PTR_CH( FNAME(:TRIMLEN(FNAME))//CHAR(0) ), &
     &                     OPEN_FLAG, MODE_FLAG )
      CALL FATAL_FILE ( FILDES, 'Opening', FNAME, ME )
!
      RETURN
      END  SUBROUTINE  BIN_OPEN
