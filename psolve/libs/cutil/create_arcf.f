      SUBROUTINE CREATE_ARCF ( FNAME, KPURGABLE, ARCNUM, ARCDIR, NP, KCOM, KOLD, &
     &                         STRING, REALLY_CREATE, FS_FULL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CREATE_ARCF PROGRAM SPECIFICATION
!
! 1.1 Create an ARCFIL.
!
! 1.2 REFERENCES:
!
! 2.  CREATE_ARCF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING,ARCDIR(*)
      INTEGER*2 ARCNUM
      INTEGER*4 NP
      LOGICAL*2 KCOM, REALLY_CREATE
!
!  ARCDIR - Directory in which to put arc file
!  ARCNUM - Arc number of this arc
!  KCOM   - True if common to be saved in arc file
!  NP     - Number of parameters
!  STRING - 'N' means delete old saved arc file first
!           'U' means use the first one you encounter
!           'P' means new arc file is permanent (not to be purged)
!  REALLY_CREATE -- TRUE is we are really going to create file. FALSE if
!                   we are going to imitate creating file. Logic os SOLVE
!                   is so tangled that sometimee we have to do foolish things.
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) FNAME
      LOGICAL*2 KOLD,KPURGABLE,FS_FULL(3)
!
!  FNAME   - Name of arc file created
!  FS_FULL - TRUE if arc file directory is full
!  KOLD    - TRUE if old saved arc file exists
!  KPURGABLE - TRUE if arc file can be purged later
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      COMMON / FREESP_BLK / OLD_BLOCKS_FREE, FILE_LENGTH, SAME_DIRECTORY
!
!  freesp_blk is to keep free space parameters used by BIN_CREATE.F
!
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*4 FILDES
      INTEGER*8 SIZE_I8
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      LOGICAL*2 KSCOM
      COMMON / SAVARC / FILDES, IDIRECT, KSCOM
      COMMON / NAMARC / SAVNAM
      SAVE   / SAVARC /, / NAMARC /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,bin_exist,bin_unlink,arc_access,
!                           arc_create
!
! 3.  LOCAL VARIABLES
!
      CHARACTER ASCIIN*6, INTTODECIMAL*5, ZEROS*6, ME*11
      INTEGER*2 IL, TRIMLEN, LENGTH, I, DELETED_FROM, IERR
      INTEGER*4 JRND_BLOCKS, FILE_LENGTH, OLD_BLOCKS_FREE, FLEN, IS
      INTEGER*4 FILE_INFO, FC_FSTAT, FC_STATFS, UNIX_DATE
      LOGICAL*2 SAME_DIRECTORY, KEXIST, LOOP
      CHARACTER ERRSTR*100, BUFSTR*80, DEBUG_ARCF*8
      DATA ZEROS/'[00000'/,ME/'CREATE_ARCF'/
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!  AEE  010891  Added code to keep track of the free space parameters
!               in order to correct DRACO hangup when changing directory.
!  AEE  910515  Enhanced error messages written to the error file.
!  BA   950731  arcdir variably dimensioned.  Do while fixed to
!               handle case where size (currently "4" from glbcm.i)
!               exceeded.
!  kdb  951006  Fix error in loop that checks for old arc directories
!               (was skipping loop entirely)
!  pet  97????  Added an actual parameter REALLY_CREATE
!  pet  990302  Improved commnets
!  jwr  2004.10.19  USed modern call FILE_INFO for getting file size
!  pet  2017.10.10  DELETED_FROM should be integeer*2, not logical*2
!  pet  2023.12.31  Extended string line to support a case more than 10000 arcs
!  pet  2024.05.28  Fixed a big in processing arcfiles with arcnum > 9999
!
! 5.  CREATE_ARCF PROGRAM STRUCTURE
!
!
! --- Check for invalid input STRING
!
 910  CONTINUE
      IF ( LEN(STRING) .NE. 1  .OR.  INDEX ( 'NUP', STRING ) .EQ. 0 ) THEN
           CALL FILE_REPORT ( SAVNAM, ME, 'ILLEGAL STRING' )
           GO TO 910
      ENDIF
      CALL CLRCH    (  DEBUG_ARCF )  
      CALL GETENVAR ( 'DEBUG_ARCF', DEBUG_ARCF )
!
! --- Check to see if we want saved arcfiles
!
      KOLD=.FALSE.
      KSCOM=KCOM
      KPURGABLE = STRING.NE.'P'
      IF ( ARCDIR(1) .EQ. ' ' ) GOTO 810
!
! --- Check for old saved arc files, delete them if 'N' or
! --- use the first one you encounter if 'U', delete others
!
      ASCIIN=INTTODECIMAL(ARCNUM)
      IL=MAX(TRIMLEN(ASCIIN),INT2(1))
     I=1
      DELETED_FROM = 0
      SAVNAM=' '
      LOOP = .TRUE.
      DO WHILE ( LOOP )
         LENGTH=TRIMLEN(ARCDIR(I))
         FNAME = ARCDIR(I)(1:LENGTH)//ZEROS(1:6-IL)//ASCIIN(1:IL)//PRE_LETRS
         CALL BIN_EXIST ( FNAME, KEXIST )
         IF ( DEBUG_ARCF(1:3) == 'YES' ) write ( 6, * ) 'CREATE_ARCF-123 ARCNUM = ', ARCNUM, ' FNAME= ', TRIM(FNAME)
         IF ( KEXIST .AND. INDEX('UP',STRING) .NE. 0  .AND. SAVNAM.EQ.' ' ) THEN
!
! ----------- The only thing which we wanted was to check: whether the arcfile
! ----------- exist and save its name
!
              SAVNAM=FNAME
           ELSE IF ( KEXIST ) THEN
!
! ----------- Keep track of what directory arc file is deleted from and
! ----------- record the file size and free space before purging the file.
! ----------- The free space is checked again before creating a new file
! ----------- and if there has been no changes(free space not updated by
! ----------- the system, yet), the deleted file size is taken into account.
!
              DELETED_FROM = I
              FLEN = TRIMLEN ( FNAME )
              IS = FILE_INFO ( FNAME(:FLEN)//CHAR(0), UNIX_DATE, SIZE_I8 )
              FILE_LENGTH = SIZE_I8
              OLD_BLOCKS_FREE=FC_STATFS(PTR_CH(FNAME(:FLEN)//CHAR(0)),FLEN)
!
              CALL BIN_UNLINK ( FNAME, IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, '("CREATE_ARCF: Unlinking: ",A)' ) FNAME
                   CALL FERR ( INT2(153), ERRSTR, INT2(0), INT2(0) )
              ENDIF
         ENDIF
         I=I+1
         IF ( I .GE. 4 ) THEN
              LOOP = .FALSE.
            ELSE
              IF ( ARCDIR(I) .EQ. ' ' ) LOOP = .FALSE.
         ENDIF
      ENDDO
!
      IF ( SAVNAM .NE. ' ' ) THEN
           KOLD  = .TRUE.
           FNAME = SAVNAM
           CALL ARC_ACCESS ( IDIRECT, SAVNAM, FILDES, KSCOM, 'O' )
           RETURN
      ENDIF
!
! --- If we just want use, then use the scratch file
!
      IF ( STRING .EQ. 'U' ) GOTO 810
!
! --- Okay we must create one
!
      I=1
      FILDES=-1
      IF ( .NOT. REALLY_CREATE ) RETURN
      DO WHILE(ARCDIR(I).NE.' '.AND.I.LT.4.AND.FILDES.LT.0)
         IF ( .NOT.FS_FULL(I) ) THEN
!
! --------- Get name
!
            SAME_DIRECTORY = I .EQ. DELETED_FROM
            LENGTH= TRIMLEN(ARCDIR(I))
            FNAME = ARCDIR(I)(1:LENGTH)//ZEROS(1:6-IL)//ASCIIN(1:IL)//PRE_LETRS
!
! --------- ... and create a file
!
            CALL ARC_CREATE8 ( IDIRECT, FNAME, FILDES, NP, KSCOM )
            FS_FULL(I) = FILDES.LT.0
         ENDIF
         I=I+1
      ENDDO
!
! --- Success
!
      IF ( FILDES.GE.0 ) THEN
           SAVNAM=FNAME
           RETURN
      ENDIF
!
! --- We could make or we aren't using saved arcfiles so use NRMFIL
!
810   CONTINUE
      SAVNAM = PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
      CALL ARC_ACCESS ( IDIRECT, SAVNAM, FILDES, KSCOM, 'O' )
      FNAME     = SAVNAM
      KPURGABLE = .FALSE.
!
      RETURN
      END  !#!  CREATE_ARCF  #!#
