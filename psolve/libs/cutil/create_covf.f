      SUBROUTINE CREATE_COVF ( STRING, NP )
      IMPLICIT NONE
!
! 1.  CREATE_COVF PROGRAM SPECIFICATION
!
! 1.1 Create a COVFIL.
!
! 1.2 REFERENCES:
!
! 2.  CREATE_COVF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NP
      CHARACTER*(*) STRING
!
! NP - Number of parameters
! STRING - Must be 'U'
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      COMMON/SAVCOV/FILDES,IDIRECT
      SAVE /SAVCOV/
      COMMON/SAVCOVCH/FNAME
      SAVE /SAVCOVCH/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,cgm_create
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*13 ME
!
      CHARACTER*(NAME_SIZE) FNAME
      integer*2 ierr
!
      DATA ME/'CREATE_COVF'/
!
! FNAME - Name of file to create
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CREATE_COVF PROGRAM STRUCTURE
!
! First construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'COVF'//PRE_LETRS
!
1     CONTINUE
!
! Check for validity of STRING
!
      IF ( STRING .NE. 'U' ) THEN
           CALL FILE_REPORT ( FNAME, ME, 'ILLEGAL STRING' )
           GOTO 1
      ENDIF
!
! Create the file
!
      CALL CGM_CREATE8 ( IDIRECT, FNAME, FILDES, NP, STRING, IERR )
      IF ( FILDES .LT. 0 ) THEN
           WRITE ( 6, * ) ' NP = ',NP
           CALL FILE_MESS ( fildes, FNAME, ME, 'creat' )
           RETURN
      ENDIF
!
      RETURN
      END
