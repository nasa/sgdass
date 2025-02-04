      SUBROUTINE GET_MASTER ( WGET_EXE, URL_IVSCONTROL, MASTER_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_MASTER  downloads the full set of master-files        *
! *   (Multi-agency VLBI schedule) from the IVS Data Center by using     *
! *   wget program. Master fiels are stored in the MASTER_DIR directory. *
! *   In addition to master fiels, ns-codes.txt fiel with station codes, *
! *   station names etc is retrieved.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       WGET_EXE ( CHARACTER ) -- filename with path of the wget       *
! *                                 executable.                          *
! * URL_IVSCONTROL ( CHARACTER ) -- URL of the directory where master    *
! *                                 files are located. It should include *
! *                                 IVS Data Center host name and should *
! *                                 end by character "/"                 *
! *     MASTER_DIR ( CHARACTER ) -- Directory name where master files    *
! *                                 will be put at local file system.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *  History:                                                            *
! *  pet  2001.01.05  changed the logic in order to prevent conflicts    *
! *                   when more thatn one process executes GET_MASTER    *
! *                   and removal of all master_files when there is      *
! *                   no connection to the IVS data center. Side-effect: *
! *                   garbige subdirectories with nema temp_xxxxx will   *
! *                   remain in MASTER_DIR directory in the case of      *
! *                   forcible abnormal termination (f.e. is user killed *
! *                   the process).                                      *
! *  pet  2003.08.28  Changed the pattern for browsing master file       *
! *                   names: made it more restrictive.                   *
! *  pet  2004.05.20  Suddenly noticed that somebody in IVS changed      *
! *                   master file names. Now all they have extension     *
! *                   .txt . Upgraded GET_MASTER corresondingly.         *
! *                                                                      *
! *  ### 26-OCT-2000   GET_MASTER  v2.3 (c)  L. Petrov  20-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  WGET_EXE*(*), URL_IVSCONTROL*(*), MASTER_DIR*(*)
      CHARACTER  REMOTE_COMSTR*256, STR*80, PID_STR*5, DIR_NAME*256, &
     &           DATE_FIL*256
      CHARACTER  GET_CDATE*19
      INTEGER*4  IUER
      INTEGER*2  MASK
      DATA MASK / O'775' / ! Protection mask: read-write-execute for owner and
!                          ! group, read execute for others
      INTEGER*4  IS, IO, ICOD, ISIG, PID, LUN, IER
      INTEGER*4  SYSTEM, I_LEN, GETPID, MKDIR, GET_UNIT
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Create a temporary directory
!
      CALL CLRCH ( DATE_FIL )
      CALL CLRCH ( DIR_NAME )
      IF ( MASTER_DIR(I_LEN(MASTER_DIR):I_LEN(MASTER_DIR)) .EQ. '/' ) THEN
           DIR_NAME = MASTER_DIR(1:I_LEN(MASTER_DIR))//'temp_'//PID_STR//'/'
           DATE_FIL = MASTER_DIR(1:I_LEN(MASTER_DIR))//'last_update.date'
         ELSE
           DIR_NAME = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/temp_'//PID_STR//'/'
           DATE_FIL = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/last_update.date'
      END IF
!
      IS = MKDIR ( DIR_NAME(1:I_LEN(DIR_NAME))//CHAR(0), %VAL(MASK) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5801, IUER, 'GET_MASTER', 'Failure to create '// &
     &                   'directory '//DIR_NAME(1:I_LEN(DIR_NAME))//'  '// &
     &                    STR )
           RETURN
      END IF
!
      CALL CLRCH ( REMOTE_COMSTR )
      REMOTE_COMSTR = 'cd '//DIR_NAME(1:I_LEN(DIR_NAME))//' ; '// &
     &       WGET_EXE(1:I_LEN(WGET_EXE))//' -q -c '// &
     &       URL_IVSCONTROL(1:I_LEN(URL_IVSCONTROL))//'master\?\?.txt '// &
     &       URL_IVSCONTROL(1:I_LEN(URL_IVSCONTROL))//'master\?\?-int.txt '// &
     &       URL_IVSCONTROL(1:I_LEN(URL_IVSCONTROL))//'ns-codes.txt'
!
! --- Launch wget for file retrieving and wait
!
      IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 5802, IUER, 'GET_MASTER', 'Error in '// &
     &            'attempt to execute a command >'// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            '< for downloading master files ICOD='//STR )
!
! -------- Remove direcotry tree
!
           IER = 0
           CALL REMOVE_TREE ( DIR_NAME, IER )
           RETURN
      END IF
!
! --- Now move master-files from the temporary directory to the destination
! --- directory and lifting any protection
!
      CALL CLRCH ( REMOTE_COMSTR )
      REMOTE_COMSTR = 'mv '// &
     &                 DIR_NAME(1:I_LEN(DIR_NAME))//'* '// &
     &                 MASTER_DIR(1:I_LEN(MASTER_DIR))//' ; '// &
     &                 'chmod 777 '//MASTER_DIR(1:I_LEN(MASTER_DIR))//'*'
      IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 5803, IUER, 'GET_MASTER', 'Error in '// &
     &            'attempt to execute a command '// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            '" for downloading master files ICOD='//STR )
!
! -------- Remove direcotry tree
!
           IER = 0
           CALL REMOVE_TREE ( DIR_NAME, IER )
           RETURN
      END IF
!
! --- Remove remporary directory tree
!
      CALL ERR_PASS ( IUER, IER )
      CALL REMOVE_TREE ( DIR_NAME, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5804, IUER, 'GET_MASTER', 'Error on attempt '// &
     &         'to remove the temporary directory tree '//DIR_NAME )
           RETURN
      END IF
!
      LUN = GET_UNIT ()
      OPEN  ( UNIT=LUN, FILE=DATE_FIL, STATUS='UNKNOWN', IOSTAT=IO )
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IO ) GET_CDATE()
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_MASTER  #!#
