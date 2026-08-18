      FUNCTION CREATE_CGMF ( FNAME, SOLUID, NP, STRING, OUTCGM )
      IMPLICIT NONE
!
! 1.  CREATE_CGMF PROGRAM SPECIFICATION
!
! 1.1 Create a CGM file.
!
! 1.2 REFERENCES:
!
! 2.  CREATE_CGMF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,STRING,SOLUID, outcgm
      INTEGER*4 NP
      INTEGER*2 CREATE_CGMF
!
! FNAME - Indicates naming convention to use: 'SAVE'=CYYMMDD.HHMMSS
!                                             'SCRATCH'=CGMFxx
!                                             'SCRATCHB'=CGMBxx
! NP - Number of parameters
! SOLUID - Solution ID
! STRING - 'M'=make new file; 'U'=update existing file
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      INTEGER*2 TRIMLEN
      COMMON/SAVCGM/FILDES,IDIRECT
      COMMON/NAMCGM/SAVNAM
      SAVE /SAVCGM/,/NAMCGM/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,numerictime,upcat,cgm_create,fatal_file,
!                           fc_sleep,use_sema,fc_close
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*11 ME
      INTEGER*4  ONE, IRET, FDES, USE_SEMA, TIMENOW
      INTEGER*2  IERR
      CHARACTER  CNAMCH*128, DIR*128, TEST*5, SOLUID2*60, ERRSTR(3)*17
      CHARACTER  CGM_DIR__ACTUAL*80
      INTEGER*4  J1, IP, ILC
      INTEGER*4  LINDEX, I_LEN
!
      DATA ME     / 'CREATE_CGMF' /
      DATA ERRSTR / 'ILLEGAL STRING', 'ALREADY EXISTS', &
     &              'ERROR IN CREATION' /
!
! CNAMCH - Name of file to be accessed
! DIR - Directory in which file is located
! FDES - File descriptor
! IRET - Return value from FC_SLEEP,FC_CLOSE
! ME - Name of this routine
! ONE - I*4 copy of value '1'
! SOLUID2 - Second copy of solution ID
! TEST - First field of solution ID, 'TEST'=test solution
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  911204  replaced CGMLCK checking for READY or LOCKED with the
!                call to use_sema because if two jobs get there at the
!                same time, CGMLCK sometimes gets set to empty (file
!                length of zero).
!   AEE  920204  Removed hard coded path for fclib.i
!   PET  970418  Added check of environment variable CGM_DIR which override
!                directory name for CGM specified in gsfcb
!   PET  990407  Changed a bit logic in setting full nale for the ouytput CGM:
!                if FNAME contains "/" in the first symbol then it is used
!                as is without adding directory name of CGM.
!   PET  990607  Corrected an error in error message. Made it function.
!                It returns a status code: 0 -- OK, not 0 -- error occurred
!   R. Lanotte   2000.04.18  Increase dimension of ERRSTR from 2 to 3.
!   pet 2000.09.29  Fixed a bug: the previous version set output CGM name
!                   incorrectly when the full filename with path was specified
!                   for the output output CGM name and solution ID specified
!                   that it was a test run.
!   pet 2001.05.10  Changed the logic: subroutine does nothing end returns
!                   promptly if FNAME .eq. 'NONE'
!
! 5.  CREATE_CGMF PROGRAM STRUCTURE
!
!
! --- Form actual name of CGM_DIR  . We check environment variable CGM_DIR
!
      IF ( TRIMLEN(FNAME) .EQ. 4  .AND.  FNAME(1:4) .EQ. 'NONE' ) THEN
!
! -------- Nothing to do
!
           RETURN
      END IF
!
      DO 410 J1=1,LEN(CGM_DIR__ACTUAL)
         CGM_DIR__ACTUAL(J1:J1) = ' '
 410  CONTINUE
!
! --- reading environment variable
!
      CALL GETENVAR ( 'PSOLVE_CGM_DIR', CGM_DIR__ACTUAL )
!
! --- ILC -- the length of actual directory name
!
      ILC = INDEX ( CGM_DIR__ACTUAL, ' ' ) - 1
      IF ( ILC .EQ. 0 ) THEN
!
! -------- Environment variable was not set up. Use default from solve.i
!
           CGM_DIR__ACTUAL = CGM_DIR
           ILC = INDEX ( CGM_DIR__ACTUAL, ' ' ) - 1
         ELSE
!
! -------- Test: didn't we forget to put / at the end of directory name
!
           IF ( CGM_DIR__ACTUAL(ILC:ILC) .NE. '/' ) THEN
                ILC = ILC + 1
                CGM_DIR__ACTUAL(ILC:ILC) = '/'
           END IF
      END IF
 910  CONTINUE
!
! --- Check for illegal STRING
!
      IF ( INDEX ( 'MU', STRING ) .EQ. 0   .OR.   LEN(STRING).NE.1 ) THEN
           CALL FILE_REPORT ( 'CREATE_CGMF', ME, 'ILLEGAL STRING' )
           GOTO 910
        ELSE IF ( STRING.EQ.'M' .AND. FNAME.EQ.'SAVE' ) THEN
!
! -------- Construct CGM file name and put it in the catalog
!
!@           FDES = USE_SEMA ( CGM_DIR__ACTUAL(1:ILC)//'CGMLCK', 'W' )
!@           ONE  = 1
!@           IRET = FC_SLEEP ( ONE )
           CALL CLRCH ( CNAMCH )
           CNAMCH(1:1) = 'C'
           CALL NUMERICTIME ( TIMENOW(), CNAMCH(2:14) )
           SOLUID2=SOLUID
           CALL SPLITSTRING ( SOLUID2, TEST, SOLUID2 )
           IF ( OUTCGM .NE. ' '  .AND. OUTCGM .NE. 'SAVE' ) CNAMCH = OUTCGM
!
           IF ( CNAMCH(1:1) .EQ. '/' ) THEN
!
! ------------- We don't add prefix with directory if the first symbol is /
!
                SAVNAM=CNAMCH
                IP = LINDEX ( SAVNAM, '/' )
                DIR = SAVNAM(1:IP)
              ELSE
!
! ------------- Otherwise we add a directory either as SCRATCH or as
! ------------- CGM_DIR__ACTUAL
!
                IF ( TEST(1:4) .EQ. 'TEST'  .OR.  TEST(1:4) .EQ. 'test' ) THEN
                     SAVNAM=SCRATCH_DIR//CNAMCH(1:14)
                     DIR=SCRATCH_DIR
                  ELSE
                     SAVNAM=CGM_DIR__ACTUAL(1:ILC)//CNAMCH(1:14)
                     DIR=CGM_DIR__ACTUAL(1:ILC)
                ENDIF
           ENDIF
!@           CALL UPCAT ( CNAMCH(1:I_LEN(CNAMCH)), DIR, PRE_LETRS, SOLUID, &
!@     &                  TEST(1:4) )
        ELSE IF ( FNAME .EQ. 'SCRATCH' ) THEN
!
! -------- Construct path/name for CGMF scratach file
!
           SAVNAM=PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
        ELSE IF ( FNAME .EQ. 'SCRATCHB' ) THEN
!
! -------- Construct path/name for CGMB scratch file
!
           SAVNAM=PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS
        ELSE
           CALL FILE_REPORT('CGMFIL',ME,'ILLEGAL OPTIONS')
           GOTO 910
      ENDIF
!
! --- Create the appropriate file
!
      CALL CGM_CREATE8 ( IDIRECT, SAVNAM, FILDES, NP, STRING, IERR )
!@!
!@! --- Close the catalog lock if it was opened
!@!
!@      IF ( FNAME .EQ. 'SAVE' ) THEN
!@           IRET=FC_CLOSE(FDES)
!@           CALL FATAL_FILE ( IRET, 'closing', &
!@     &                       CGM_DIR__ACTUAL(1:ILC)//'CATLCK', ME )
!@      ENDIF
!
      FNAME=SAVNAM
      IF ( IERR .NE. 0 ) THEN
           WRITE ( 6, * ) ' IERR=',IERR
           CALL FILE_REPORT ( FNAME, ME, ERRSTR(IERR) )
      ENDIF
      CREATE_CGMF = IERR
!
      RETURN
      END  !#!  CREATE_CGMF  #!#
