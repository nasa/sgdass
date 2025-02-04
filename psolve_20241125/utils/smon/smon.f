#include <mk5_preprocessor_directives.inc>
      PROGRAM SMON
! ************************************************************************
! *                                                                      *
! *   Program is for monitoring execution SOLVE running in  batch mode.  *
! *   It displays status information in the screen. It executes          *
! *   infinite loop and update status information every second.          *
! *                                                                      *
! *   Usage:  SMON <solve_initials> <work_dir> [<interval_update>]       *
! *                                                                      *
! *      where                                                           *
! *      <solve_initials> is a line with two character of the initials   *
! *      <work_dir> -- full path to the directory where solve scratch    *
! *                    file are located.                                 *
! *      [<interval_update>] -- optional argument. Specifies time        *
! *                             interval in seconds for updating screen  *
! *                             output. Default is 1 second.             *
! *                                                                      *
! *  Example of the output:                                              *
! *                                                                      *
! *  picasso    /disk4/vlbi/petrov/tests/job59                           *
! *  PT SOLVE:  FORW      2(4)  $88NOV09X  <14> started  (F)     0:02:50 *
! *                                                                      *
! *  Here:                                                               *
! *    picasso -- hostname where SOVLE is running.                       *
! *  /disk4/.. -- full name of the control file to be executed.          *
! *         PT -- Solve initials.                                        *
! *       FORW -- program name which now is being executed.              *
! *          2 -- index of the session in the arc-list which is being    *
! *               analyzed.                                              *
! *        (4) -- total number of the sessions to be analyzed.           *
! *  $88NOV09X -- database name which is now being processed.            *
! *       <14> -- version number of the database which is now being      *
! *               processed.                                             *
! *    started -- status of the processing: one of                       *
! *               started                                                *
! *               done                                                   *
! *        (F) -- mode:                                                  *
! *               (F) -- forward run in fast mode;                       *
! *               (B) -- backward run in fast mode;                      *
! *               (I) -- independent run in fast mode;                   *
! *               (f) -- forward run in non-fast mode;                   *
! *               (b) -- backward run in non-fast mode;                  *
! *               (i) -- independent run in non-fast mode;               *
! *    0:02:50 -- amount of time elapsed since the last status update.   *
! *               large amount of time may indicate that SOLVE is not    *
! *               active now because it died.                            *
! *  (Comment: the first line may not been shown in the beginning of the *
! *      batch run before parsing control file)                          *
! *                                                                      *
! *  ###  22-APR-1999     SMON     v1.8  (c)  L. Petrov 05-JUL-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SOLVE_INIT*2, WORK_DIR*160, STAT_FILE*160, STR*160, DAT*9, &
     &           CNTR_FILE*160
      CHARACTER  STRT*10, OUT*80
      LOGICAL*4  LEX
      INTEGER*4  I11, J1, IP, IDT, IER, NUMARG, IS, IT, ISEC, IMIN, IHOU, &
     &           IDELTIM, INT_SLEEP, MIND, LIND, IND(2,64), IK, PID
      PARAMETER  ( MIND = 64 )
      INTEGER*4  ISTAT(12)
      ADDRESS__TYPE :: DIR_DESC
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4,     EXTERNAL :: I_LEN, ILEN, FOR_STAT, KILL, TIME 
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
!
! --- Get parameters
!
      NUMARG = IARGC()
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 1,  SOLVE_INIT )
           CALL TRAN   ( 11, SOLVE_INIT, SOLVE_INIT  )
           CALL GETARG ( 2,  WORK_DIR   )
        ELSE
           WRITE ( 6, '(A)' ) 'Usage: SMON <solve_initials> <work_dir> '// &
     &                        '[<interval_update]>'
           STOP 'SMON'
      END IF
      IF ( NUMARG .GE. 3 ) THEN
           CALL GETARG ( 3, STR )
           READ ( UNIT=STR, FMT='(I6)' ) INT_SLEEP
           IF ( INT_SLEEP .LE. 0 ) INT_SLEEP = 1
         ELSE
           INT_SLEEP = 1
      END IF
!
      CALL SET_SIGNAL_CTRLC ( 1 )
!
! --- Check: does work directory exist
!
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', STR )
      IF ( ILEN(STR) > 0 ) WORK_DIR = STR
      DIR_DESC = OPENDIR ( WORK_DIR(1:I_LEN(WORK_DIR))//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 401, -1, 'SMON', 'Error in access to directory '// &
     &          WORK_DIR(1:I_LEN(WORK_DIR))//' -- '//STR )
           CALL EXIT ( 1 )
         ELSE 
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Form the name of the file where control file is saved
!
      IP = I_LEN(WORK_DIR)
      IF ( WORK_DIR(IP:IP) .NE. '/' ) WORK_DIR(IP+1:IP+1) = '/'
      CNTR_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'CNTR'//SOLVE_INIT
!
! --- Learn does this file exist?
!
      INQUIRE ( FILE=CNTR_FILE, EXIST=LEX )
      IF ( LEX ) THEN
!
! ------ Yes, it exists
!
         OPEN ( UNIT=11, FILE=CNTR_FILE, STATUS='OLD', IOSTAT=I11 )
         IF ( I11 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( I11, STR )
              CALL ERR_LOG ( 402, -1, 'SMON', 'Error openning file '// &
     &             STAT_FILE(1:I_LEN(STAT_FILE))//' IOSTAT='//STR )
              STOP 'SMON'
         END IF
!
! ------ Read a line from there
!
         READ ( UNIT=11, IOSTAT=I11, FMT='(A)' ) STR
         IF ( I11 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( I11, STR )
              CALL ERR_LOG ( 403, -1, 'SMON', 'Error in reading file '// &
     &             STAT_FILE(1:I_LEN(STAT_FILE))//' IOSTAT='//STR )
              STOP 'SMON'
         END IF
         CLOSE ( UNIT=11 )
!
! ------ Parse the line onto words
!
         CALL EXWORD ( STR, MIND, LIND, IND, ' ', -3 )
         CALL CLRCH ( OUT )
         IF ( IND(1,2) .LE. 14   .AND.  IND(2,2) .LE. 79  ) THEN
!
! ----------- Shift the second word right if it is possible
!
              OUT = STR(IND(1,1):IND(2,1))
              OUT(14:) = STR(IND(1,2):IND(2,2))
            ELSE
              OUT = STR
         END IF
!
         IF ( LIND .EQ. 2 ) THEN
!
! ----------- Write down the line from there on screen if the file is not
! ----------- stale. File is stale when there is a third word.
!
              WRITE ( 6, FMT='(A)' ) OUT(1:I_LEN(OUT))
         END IF
      END IF
!
! --- Form status file name
!
      STAT_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'STAT'//SOLVE_INIT
!
! --- Check: does status file exist
!
      INQUIRE ( FILE=STAT_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 404, -1, 'SMON', 'Status file '// &
     &          STAT_FILE(1:I_LEN(STAT_FILE))//' not found' )
           STOP 'SMON'
      END IF
!
! --- Infintie loop
!
      DO 410 J1=1,1024*1024*1024 ! 30 years
!
! ------ Get information about status file
!
         IS = FOR_STAT ( STAT_FILE, ISTAT )
!
! ------ Get current time
!
         IT = TIME ( %VAL(0) )
!
! ------ Compute amount of time elapsed since modification of status file
!
         IDELTIM = IT - ISTAT(10)
!
! ------ DELTIM may appear less than zero due to lack of synchronization system
! ------ clocks runnung on two different machines
!
         IF ( IDELTIM .LT. 0 ) IDELTIM = 0
!
! ------ Open status file
!
         OPEN ( UNIT=11, FILE=STAT_FILE, STATUS='OLD', IOSTAT=I11 )
         IF ( I11 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( I11, STR )
              CALL ERR_LOG ( 405, -1, 'SMON', 'Error openning status file '// &
     &             STAT_FILE(1:I_LEN(STAT_FILE))//' IOSTAT='//STR )
              STOP 'SMON'
         END IF
!
! ------ Read a line from there
!
         READ ( UNIT=11, IOSTAT=I11, FMT='(A)' ) STR
!
         IF ( STR(67:71) .NE. '     ' ) THEN
              CALL CHIN ( STR(67:74), PID )
!
! ----------- Send a signal with number 0 to the subprocess
!
#ifdef  HPUX
              IK = KILL ( PID, 0 )
#else
              IK = KILL ( %VAL(PID), %VAL(0) )
#endif
              IF ( STR(13:33) .NE. 'successful completion' .AND. &
     &             IK .NE. 0 ) THEN
                   STR = STR(1:42)//' TERMINATED '//STR(51:64)  
              END IF
         END IF
         IF ( I11 .EQ. 0 ) THEN
!
! ----------- Formatting line with elapsed time
!
              CALL CLRCH ( STRT )
              IHOU = IDELTIM/3600
              IMIN = (IDELTIM - IHOU*3600 )/60
              ISEC = (IDELTIM - IHOU*3600 - IMIN*60 )
              CALL INCH   ( ISEC, STRT(9:10) )
              CALL CHASHR (       STRT(9:10) )
              STRT(8:8) = ':'
              CALL INCH    ( IMIN, STRT(6:7) )
              CALL CHASHR  (       STRT(6:7) )
              STRT(5:5) = ':'
              CALL INCH    ( IHOU, STRT(1:4) )
              CALL CHASHR  (       STRT(1:4) )
              CALL BLANK_TO_ZERO ( STRT(5:10))
!
! ----------- Print line on the screen
!
              CALL CLRCH ( OUT )
              OUT = '  '//SOLVE_INIT//'  '//STR(1:60)//STRT
              WRITE ( 6, FMT='(A,$)' ) OUT(1:78)//CHAR(13)
              CALL FLUSH ( 6 )
         END IF
!
! ------ Close status file
!
         CLOSE ( UNIT=11 )
!
! ------ Wait for one second and then resume execution
!
#ifdef INTEL
         CALL FUNC_SLEEP ( INT_SLEEP )
#else
         CALL SLEEP ( INT_SLEEP )
#endif
 410  CONTINUE
!
      END  !#!  SMON  #!#
