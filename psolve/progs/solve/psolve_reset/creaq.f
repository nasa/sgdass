      SUBROUTINE CREAQ ( NAM, IERR, SIZE_I8 )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CREAQ PROGRAM SPECIFICATION
!
! 1.1 This routine creates the specified SOLVE scratch file.
!
! 1.2 REFERENCES:
!
! 2.  CREAQ INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) NAM
      INTEGER*8 SIZE_I8
!
! SIZE_I8 - Requested size for the file, in blocks
! NAM - Name of the file to be created
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR - Error return; -1 if file already exists
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: solve
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*(NAME_SIZE) FNAME
      SAVE FNAME
      INTEGER*2 POSN,TRIMLEN,IL
      LOGICAL*4 ex
      Integer*4 fildes, ios
      character*120 bufstr
      character*1 cdum
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  960326  Fix error: change pause without accompanying error message
!                to ferr call.
!
! 5.  CREAQ PROGRAM STRUCTURE
!
! Construct file path/name and check whether file already exists
!
      POSN  = TRIMLEN(NAM)
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//NAM(1:POSN)//PRE_LETRS
      INQUIRE ( FILE=FNAME, IOSTAT=IOS, EXIST=EX )
      IF ( IOS .NE. 0 ) CALL PAUSE ( 'error on inquire in creaq' )
!
! --- If file exists, set error flag and return
!
      IF ( EX ) THEN
           IERR = -1
!
! -------- Otherwise go ahead and create the file
!
         ELSE
           CALL BIN_CREATE8 ( FNAME, FILDES, SIZE_I8 )
           IF ( FILDES .LT. 0 ) THEN
                WRITE ( BUFSTR, '("Check disk space: couldn''t create: ",A)') &
     &                  FNAME
                CALL FERR ( INT2(260), BUFSTR, INT2(0), INT2(0) )
           ENDIF
           CALL BIN_CLOSE ( FNAME, FILDES )
           IERR = 0
      ENDIF
!
      RETURN
      END  !#!  CREAQ  #!#
