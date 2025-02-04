#include <mk5_preprocessor_directives.inc>
       PROGRAM    LOG_TO_ANTAB_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*4, EXTERNAL :: ILEN
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL GETENVAR ( 'GOMP_STACKSIZE', STR )
       IF ( ILEN(STR) == 0 ) THEN
            CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       END IF
!
       CALL LOG_TO_ANTAB()
       END  PROGRAM  LOG_TO_ANTAB_MAIN
!
! ------------------------------------------------------------------------
!
      SUBROUTINE     LOG_TO_ANTAB
! ************************************************************************
! *                                                                      *
! *   Program LOG_TO_ANTAB
! *                                                                      *
! *  ### 10-FEB-2008  LOG_TO_ANTAB  v3.2 (c)  L. Petrov 16-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INTEGER*4    M_TIM, M_CHN
      PARAMETER  ( M_TIM =     8*1024 )
      PARAMETER  ( M_CHN =        512 )
      CHARACTER, ALLOCATABLE :: BUF(:)*256, SCAN_NAME(:)*10, SOURCE_NAME(:)*10, SOURCE_ONS(:)*10 
      CHARACTER  FILIN*128, FILOUT*128, STR*128, STA_NAM*8, POL_FRQ(M_CHN)*3, FS_VERS*8
      CHARACTER  PROG__LABEL*50
      PARAMETER  ( PROG__LABEL = '# Generator:  log_to_antab  Version of 2016.09.05' )
      REAL*8     IF_FRQ(M_CHN), LO_FRQ(M_CHN), UTC_TSYS(M_TIM), &
     &           UTC_ONS(2,M_TIM), UTC_CAB(M_TIM), UTC_ATM(M_TIM), &
     &           TSYS(M_CHN,M_TIM), CAB(M_TIM), PRES(M_TIM), &
     &           TEMP(M_TIM), HUMID(M_TIM), IS, UNIX_DATE
      INTEGER*4  MJD_TSYS(M_TIM), MJD_ONS(2,M_TIM), MJD_CAB(M_TIM), &
     &           MJD_ATM(M_TIM), MBUF, N_TSYS, N_FRQ, N_ONS, &
     &           N_CAB, N_ATM, YEAR, IB1, IB2, IB3, IVERS(3), FS_IVERS
      INTEGER*8  SIZE_I8
      INTEGER*4  DEF_LIN_LEN, DEF_EXTRA_LINES
      PARAMETER  ( DEF_LIN_LEN     =      64 ) ! Default line length
      PARAMETER  ( DEF_EXTRA_LINES = 64*1024 ) ! The number of additional lines
      INTEGER*4  NBUF, MODE, IUER
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS8
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT, FILE_INFO 
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
!  MODE =  1  -- for IVS log-files after 2008
!  MODE =  2  -- for IVS log-files after in or before 2002
!  MODE =  3  -- for IVS log-files after at 1995
!  MODE =  4  -- for IVS log-files after at 1996
!  MODE =  5  -- DBBC log file with USB/LSB pairs of BBC
!  MODE =  6  -- VLBA DBBC logs
!  MODE =  7  -- DBBC2 logs
!  MODE = 11  -- for KVN log-files
!  MODE = 25  -- for KASHIM34 log-files: extract Tsys for user-device u5
!
!
! ---- Set stacksize
!
      IS8 = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL GETENVAR ( 'GOMP_STACKSIZE', STR )
      IF ( ILEN(STR) == 0 ) THEN
           CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       END IF
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: log_to_antab {mode} {log_file} {antab_file} [year]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, STR    )
           CALL CHIN   ( STR, MODE )
           CALL GETARG ( 2, FILIN  )
           CALL GETARG ( 3, FILOUT )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR    )
                CALL CHIN   ( STR, YEAR )
           END IF
      END IF
!
      IUER = -1
      IS = FILE_INFO ( TRIM(FILIN)//CHAR(0), UNIX_DATE, SIZE_I8, IUER )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           IUER = -1
           CALL ERR_LOG ( 1401, IUER, 'LOG_TO_ANTAB', 'Error in accessing '// &
     &         'to the input log file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- MBUF -- is the predicted maximum number of lines in the log file
!
      MBUF = DEF_EXTRA_LINES + SIZE_I8/DEF_LIN_LEN
      ALLOCATE ( BUF(MBUF),         STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(MBUF)*LEN(BUF(1)), STR )
           IUER = -1
           CALL ERR_LOG ( 1402, IUER, 'LOG_TO_ANTAB', 'Error in allocation '// &
     &         'of '//TRIM(STR)//' bytes for array BUF' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SCAN_NAME(MBUF),   STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(MBUF)*LEN(SCAN_NAME(1)), STR )
           IUER = -1
           CALL ERR_LOG ( 1403, IUER, 'LOG_TO_ANTAB', 'Error in allocation '// &
     &         'of '//TRIM(STR)//' bytes for array SCAN_NAME' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SOURCE_NAME(MBUF), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(MBUF)*LEN(SOURCE_NAME(1)), STR )
           IUER = -1
           CALL ERR_LOG ( 1404, IUER, 'LOG_TO_ANTAB', 'Error in allocation '// &
     &         'of '//TRIM(STR)//' bytes for array SOURCE_NAME' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SOURCE_ONS(MBUF),  STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(MBUF)*LEN(SOURCE_ONS(1)), STR )
           IUER = -1
           CALL ERR_LOG ( 1405, IUER, 'LOG_TO_ANTAB', 'Error in allocation '// &
     &         'of '//TRIM(STR)//' bytes for array SOURCE_ONS' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILIN, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1406, IUER, 'LOG_TO_ANTAB', 'Error in reading '// &
     &         'input log file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MODE .NE. 6 .AND. MODE .NE. 11 ) THEN
           IUER = -1
           CALL CHECK_FS_VERSION ( NBUF, BUF, FS_VERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1407, IUER, 'LOG_TO_ANTAB', 'Failure in '// &
     &              'parsing input log file '//FILIN )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           IB1 = INDEX ( FS_VERS, '.' )
           IB2 = INDEX ( FS_VERS(IB1+1:), '.' ) + IB1
           IB3 = INDEX ( FS_VERS(IB2+1:), '.' ) + IB2
           READ ( UNIT=FS_VERS(1:IB1-1),     FMT='(I4)' ) IVERS(1)
           READ ( UNIT=FS_VERS(IB1+1:IB2-1), FMT='(I4)' ) IVERS(2)
           READ ( UNIT=FS_VERS(IB2+1:),      FMT='(I4)' ) IVERS(3)
           FS_IVERS = 10000*IVERS(1) + 100*IVERS(2) + IVERS(3)
      END IF
      IF ( FS_IVERS .GE. 91105 .AND. ( MODE == 1 .OR. MODE == 7 ) ) THEN
            CALL PIMA_PARSE_LOG ( MODE, NBUF, BUF, M_TIM, M_CHN, N_TSYS, N_FRQ, &
     &                 MJD_TSYS, UTC_TSYS, N_ONS, MJD_ONS, UTC_ONS, &
     &                 SOURCE_ONS, IF_FRQ, LO_FRQ, POL_FRQ, SCAN_NAME, &
     &                 SOURCE_NAME, TSYS, N_CAB, MJD_CAB, UTC_CAB, CAB, &
     &                 N_ATM, MJD_ATM, UTC_ATM, PRES, TEMP, &
     &                 HUMID, STA_NAM, YEAR, IUER )
         ELSE IF ( MODE == 6 ) THEN
           N_ONS = 0
           N_CAB = 0
           N_ATM = 0
           CALL PIMA_PARSE_VLBA_TSYS ( FILIN, NBUF, BUF, M_TIM, M_CHN, N_TSYS, N_FRQ, &
     &                 MJD_TSYS, UTC_TSYS, IF_FRQ, LO_FRQ, POL_FRQ, SCAN_NAME, &
     &                 SOURCE_NAME, TSYS, STA_NAM, YEAR, IUER )
         ELSE 
           CALL PARSE_LOG_PRE911 ( MODE, NBUF, BUF, M_TIM, M_CHN, N_TSYS, N_FRQ, &
     &                 MJD_TSYS, UTC_TSYS, N_ONS, MJD_ONS, UTC_ONS, &
     &                 SOURCE_ONS, IF_FRQ, LO_FRQ, SCAN_NAME, &
     &                 SOURCE_NAME, TSYS, N_CAB, MJD_CAB, UTC_CAB, &
     &                 CAB, N_ATM, MJD_ATM, UTC_ATM, &
     &                 PRES, TEMP, HUMID, STA_NAM, YEAR, IUER )
           POL_FRQ = 'R'
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1408, IUER, 'LOG_TO_ANTAB', 'Failure in parsing '// &
     &         'log file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL WRI_ANTAB ( M_CHN, N_TSYS, N_FRQ, MJD_TSYS, UTC_TSYS, &
     &                 IF_FRQ, LO_FRQ, POL_FRQ, SCAN_NAME, SOURCE_NAME, &
     &                 TSYS, N_ONS, MJD_ONS, UTC_ONS, SOURCE_ONS, &
     &                 N_CAB, MJD_CAB, UTC_CAB, CAB, N_ATM, MJD_ATM, UTC_ATM, &
     &                 PRES, TEMP, HUMID, STA_NAM, PROG__LABEL, &
     &                 FILIN, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1409, IUER, 'LOG_TO_ANTAB', 'Failure in an attempt'// &
     &         ' to write into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  SUBROUTINE   LOG_TO_ANTAB !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_FS_VERSION ( NBUF, BUF, VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CHECK_FS_VERSION
! *                                                                      *
! * ### 05-SEP-2016  CHECK_FS_VERSION v1.0 (c) L. Petrov 05-SEP-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NBUF, IUER 
      CHARACTER  BUF(NBUF)*(*), VERS*(*)
      INTEGER*4  IP, J1
!
      DO 410 J1=1,NBUF
         IP = INDEX ( BUF(J1), 'Field System Version' )
         IF ( IP > 0 ) THEN
              VERS = BUF(J1)(IP+21:)
              CALL CHASHL ( VERS )
              CALL ERR_LOG ( 0, IUER )
              RETURN 
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 1409, IUER, 'CHECK_FS_VERSION', 'Trap of internal control: '// &
     &    'cannot find Fields System version' )
      RETURN
      END  SUBROUTINE  CHECK_FS_VERSION   !#!#
