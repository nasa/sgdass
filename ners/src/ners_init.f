      SUBROUTINE NERS_INIT ( CONFIG_FILE, NERS, &
     &                       TIME_TAI_START, TIME_TAI_STOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_INIT initializes internal NERS data structure, parses *
! *   the control file reads the leap second file. It also specifues     *
! *   the interval of time for the Earth orientation parameters that     *
! *   will be computed at the next call to NERS. This interval should    *
! *   not exceed 10 days.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * CONFIG_FILE    ( CHARACTER ) -- The name of the configuration file.  *
! *                                 In most cases the configuration file *
! *                                 NERS__CONFIG specified in            *
! *                                 ners_local.i include block can be    *
! *                                 used.                                *
! * TIME_TAI_START ( REAL*8    ) -- Start time of the time range for     *
! *                                 consequent cals of NERS_GET_EOP.     *
! *                                 Units: seconds since                 *
! *                                 2000.01.01_00:00:00.0 TAI. Value     *
! *                                 -1.0D0 means the current moment.     *
! * TIME_TAI_STOP  ( REAL*8    ) -- Stop time of the time range for      *
! *                                 consequent calls of NERS_GET_EOP.    *
! *                                 Units: seconds since                 *
! *                                 2000.01.01_00:00:00.0 TAI. Value     *
! *                                 -1.0D0 means the current moment.     *
! *                                 If TIME_TAI_START and TIME_TAI_STOP  *
! *                                 are both -1.0D0, then Earth          *
! *                                 orientation parameters will be       *
! *                                 computed byu NERS_GET_EOP without    *
! *                                 interpolation. This will be slower.  *
! *                                 The purpose of specifing the interval*
! *                                 is to speed up consequent            *
! *                                 computations.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 15-JUN-2016   NERS_INIT  v2.10  (c)  L. Petrov  21-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIME_TAI_START, TIME_TAI_STOP
      LOGICAL*1  LEX
      INTEGER*4  IUER
      INTEGER*4  MP
      PARAMETER  ( MP = 128 )
      CHARACTER  CONFIG_FILE_USE*128, BUF(MP)*128, CONN_TIMEOUT_STR*128, &
     &           READ_TIMEOUT_STR*128, LOCK_TIMEOUT_STR*128, AGE_FCS_STR*128, &
     &           AGE_SPL_STR*128, NTRIES_STR*128, LTP_USE_STR*128, &
     &           ON_FAIL_TO_READ_STR*128, ANS*16, STR*128, STR1*21, STR2*21
      CHARACTER  NERS_IO_LOCK_FILE*128, NERS_READ_LOCK_FILE*128, NERS_WRITE_LOCK_FILE*128
      REAL*8     UTC_CUR, UTC_M_TAI
      INTEGER*4  NP, J1, ID, IER
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, LINDEX, TIME
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      CALL NERS_VERSION ( 'NERS__VERSION', ANS ) 
      IF ( ANS .NE. NERS__VERSION ) THEN
           CALL ERR_LOG ( 4221, IUER, 'NERS_INIT', 'Trap of internal '// &
     &         'control: you program was compiled against '//NERS__VERSION// &
     &         ', but linked against '//TRIM(ANS)//' . Please re-compile and '// &
     &         're-link your application' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(TIME_TAI_START) ) THEN
           CALL ERR_LOG ( 4222, IUER, 'NERS_GET_EOP', 'Argument TIME_TAI_START '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(TIME_TAI_STOP) ) THEN
           CALL ERR_LOG ( 4223, IUER, 'NERS_GET_EOP', 'Argument TIME_TAI_STOP '// &
     &         'is not a number' )
           RETURN 
      END IF
!
! --- Check validity of TIME_TAI_START and TIME_TAI_STOP
!
      IF ( TIME_TAI_STOP == -1.0D0 .AND. TIME_TAI_START == -1.0D0 ) THEN
           CONTINUE 
         ELSE IF ( TIME_TAI_STOP - TIME_TAI_START > NERS__INTR_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR(1:10), FMT='(F10.4)' ) NERS__INTR_MAX/86400.0D0
           STR1 = TIM_TO_DATE ( TIME_TAI_START, IER )
           STR2 = TIM_TO_DATE ( TIME_TAI_STOP, IER )
           CALL ERR_LOG ( 4224, IUER, 'NERS_INIT', 'Trap of '// &
      &        'internal control: the interval for EOP spline '// &
      &        'expansion is too large: ['//STR1//', '//STR2//'] -- '// &
      &        'longer than '//TRIM(STR)//' days' )
            RETURN 
      END IF
!
! --- Clear NERS internal data strucuture
!
      CALL NERS_QUIT ( NERS__FCS, NERS )
!
! --- Check NERS config file.
!
      CONFIG_FILE_USE = CONFIG_FILE
!
! --- If configuration file is NERS_CONFIG or $NERS_CONFIG, then
! --- check environment variable NERS_CONFIG
!
      IF ( CONFIG_FILE_USE == 'NERS_CONFIG' .OR. CONFIG_FILE_USE == '$NERS_CONFIG' ) THEN
           CALL GETENVAR ( 'NERS_CONFIG', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CONFIG_FILE_USE = STR
              ELSE
                CALL CLRCH ( CONFIG_FILE_USE )
           END IF
      END IF
      IF ( ILEN(CONFIG_FILE_USE) == 0 ) THEN
!
! -------- If it is not defined, then check $HOME/.ners_config
!
           CALL GETENVAR ( 'HOME', CONFIG_FILE_USE )
           CONFIG_FILE_USE = TRIM(CONFIG_FILE_USE)//'.ners_config'
           INQUIRE ( FILE=CONFIG_FILE_USE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
!
! ------------- If $HOME/.ners_config is not defined, then use system-wide configuration file
!
                CONFIG_FILE_USE = NERS__CONFIG
           END IF
      END IF
!
! --- Check whether the input configuration file exists
!
      INQUIRE ( FILE=CONFIG_FILE_USE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
            CALL ERR_LOG ( 4225, IUER, 'NERS_INIT', 'Cannot find '// &
      &         'configuration file '//CONFIG_FILE_USE )
            RETURN 
      END IF
!
! --- Read the NERS configuration file
!
      IER = IUER
      CALL RD_TEXT ( CONFIG_FILE_USE, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4226, IUER, 'NERS_INIT', 'Error in reading '// &
     &         'configuration file '//CONFIG_FILE_USE )
           RETURN 
      END IF
!
! --- Check whether it has correct label
!
      IF ( BUF(1)(1:LEN(NERS__CONF)) .NE. NERS__CONF ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4227, IUER, 'NERS_INIT', 'Error in parsing '// &
     &         'configuration file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &         ' -- the first line is '//STR(1:LEN(NERS__CONF))// &
     &         ' while '//NERS__CONF//' was expected' )
           RETURN 
      END IF
!
! --- Initialization
!
      NERS%CNF%FCS_FILE = ' '
      CONN_TIMEOUT_STR  = ' '
      AGE_FCS_STR       = ' '
      AGE_SPL_STR       = ' '
      NTRIES_STR        = ' '
      LTP_USE_STR       = ' '
      NERS%CNF%N_URL    = 0
      NERS%CNF%ON_FAIL_TO_READ = ' '
!
! --- Parse configuration file
!
      DO 410 J1=2,NP
         IF ( INDEX ( BUF(J1), 'URL:' ) == 1 ) THEN
!
! ----------- Parse the URL with the NERS message
!
              NERS%CNF%N_URL = NERS%CNF%N_URL + 1
              IF ( NERS%CNF%N_URL > NERS__M_URL ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( NERS__M_URL, STR )
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( J1, STR1 )
                   CALL ERR_LOG ( 4228, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'configuration file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' line '//STR1(1:I_LEN(STR1))//' -- too many URLs: '// &
     &                  'more than '//STR )
                   RETURN 
              END IF
              ID = INDEX ( BUF(J1), ' ' ) 
              NERS%CNF%URL(NERS%CNF%N_URL) = ADJUSTL( BUF(J1)(ID:) )
              IF ( NERS%CNF%URL(NERS%CNF%N_URL)(1:1) == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4229, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword URL should have a value' )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'FCS_FILE:' ) == 1 ) THEN
!
! ----------- Parse the local forecast file name
!
              ID = INDEX ( BUF(J1), ' ' ) 
              NERS%CNF%FCS_FILE = ADJUSTL( BUF(J1)(ID:) )
              IF ( NERS%CNF%FCS_FILE == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4230, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword FCS_FILE: should have a value' )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'LEAPSEC_FILE:' ) == 1 ) THEN
!
! ----------- Parse the local leap second file name
!
              ID = INDEX ( BUF(J1), ' ' ) 
              NERS%CNF%LEAPSEC_FILE = ADJUSTL( BUF(J1)(ID:) )
              IF ( NERS%CNF%LEAPSEC_FILE == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4231, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword LEAPSEC_FILE: should have a value' )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'CONN_TIMEOUT:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              CONN_TIMEOUT_STR = ADJUSTL( BUF(J1)(ID:) )
              IF ( INDEX ( CONN_TIMEOUT_STR, '.' ) == 0 ) CONN_TIMEOUT_STR = TRIM(CONN_TIMEOUT_STR)//'.0'
              IF ( CONN_TIMEOUT_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4232, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword CONN_TIMEOUT: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=CONN_TIMEOUT_STR, FMT='(F8.4)', IOSTAT=IER ) NERS%CNF%CONN_TIMEOUT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4233, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of CONN_TIMEOUT should be a real number, '// &
     &                  'but got '//CONN_TIMEOUT_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'READ_TIMEOUT:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              READ_TIMEOUT_STR = ADJUSTL( BUF(J1)(ID:) )
              IF ( INDEX ( READ_TIMEOUT_STR, '.' ) == 0 ) READ_TIMEOUT_STR = TRIM(READ_TIMEOUT_STR)//'.0'
              IF ( READ_TIMEOUT_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4234, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword READ_TIMEOUT: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=READ_TIMEOUT_STR, FMT='(F8.4)', IOSTAT=IER ) NERS%CNF%READ_TIMEOUT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4235, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of READ_TIMEOUT should be a real number, '// &
     &                  'but got '//READ_TIMEOUT_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'LOCK_TIMEOUT:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              LOCK_TIMEOUT_STR = ADJUSTL( BUF(J1)(ID:) )
              IF ( INDEX ( LOCK_TIMEOUT_STR, '.' ) == 0 ) LOCK_TIMEOUT_STR = TRIM(LOCK_TIMEOUT_STR)//'.0'
              IF ( LOCK_TIMEOUT_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4236, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword LOCK_TIMEOUT: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=LOCK_TIMEOUT_STR, FMT='(F8.4)', IOSTAT=IER ) NERS%CNF%LOCK_TIMEOUT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4237, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of LOCK_TIMEOUT should be a real number, '// &
     &                  'but got '//LOCK_TIMEOUT_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'N_TRIES:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              NTRIES_STR = ADJUSTL ( BUF(J1)(ID:) )
              IF ( NTRIES_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4238, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword N_TRIES: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=NTRIES_STR, FMT='(I12)' ) NERS%CNF%N_TRIES
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4239, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of N_TRIES should be a real number, '// &
     &                  'but got '//NTRIES_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'AGE_FCS:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              AGE_FCS_STR = ADJUSTL ( BUF(J1)(ID:) )
              IF ( INDEX ( AGE_FCS_STR, '.' ) == 0 ) AGE_FCS_STR = TRIM(AGE_FCS_STR)//'.0'
              IF ( AGE_FCS_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4240, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword AGE_FCS: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=AGE_FCS_STR, FMT='(F8.4)' ) NERS%CNF%AGE_FCS
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4241, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of AGE_FCS_STR should be a real number, '// &
     &                  'but got '//AGE_FCS_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'AGE_SPL:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              AGE_SPL_STR = ADJUSTL ( BUF(J1)(ID:) )
              IF ( INDEX ( AGE_SPL_STR, '.' ) == 0 ) AGE_SPL_STR = TRIM(AGE_SPL_STR)//'.0'
              IF ( AGE_SPL_STR == ' ' ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4242, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- the keyword AGE_SPL: should have a value' )
                   RETURN 
              END IF
              READ ( UNIT=AGE_SPL_STR, FMT='(F8.4)' ) NERS%CNF%AGE_SPL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4243, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- value of AGE_SPL should be a real number, '// &
     &                  'but got '//AGE_SPL_STR )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'LTP_USAGE:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              LTP_USE_STR = ADJUSTL ( BUF(J1)(ID:) )
              IF ( LTP_USE_STR == NERS__WARNING ) THEN
                   NERS%CNF%LTP_USAGE =  NERS__WARNING
                ELSE IF ( LTP_USE_STR == NERS__SILENT ) THEN
                   NERS%CNF%LTP_USAGE =  NERS__SILENT
                ELSE IF ( LTP_USE_STR == NERS__STOP   ) THEN
                   NERS%CNF%LTP_USAGE =  NERS__STOP
                ELSE
                   CALL ERR_LOG ( 4244, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- unsupported value of the keyword LTP_USAGE: '// &
     &                  TRIM(LTP_USE_STR)//'. Acceptable values: silent warning stop' )
                   RETURN 
              END IF
           ELSE IF ( INDEX ( BUF(J1), 'ON_FAIL_TO_READ:' ) == 1 ) THEN
              ID = INDEX ( BUF(J1), ' ' ) 
              IF ( ID < 1 ) ID = LEN(BUF(J1))
              ON_FAIL_TO_READ_STR = ADJUSTL ( BUF(J1)(ID:) )
              CALL TRAN ( 13, ON_FAIL_TO_READ_STR, ON_FAIL_TO_READ_STR )
              CALL TRAN ( 12, ON_FAIL_TO_READ_STR, ON_FAIL_TO_READ_STR )
              IF ( ON_FAIL_TO_READ_STR == NERS__IGNORE ) THEN
                   NERS%CNF%ON_FAIL_TO_READ = NERS__IGNORE
                 ELSE IF ( ON_FAIL_TO_READ_STR == NERS__WARNING  ) THEN
                   NERS%CNF%ON_FAIL_TO_READ = NERS__WARNING
                 ELSE IF ( ON_FAIL_TO_READ_STR == NERS__STOP ) THEN
                   NERS%CNF%ON_FAIL_TO_READ = NERS__STOP
                 ELSE 
                   CALL ERR_LOG ( 4245, IUER, 'NERS_INIT', 'Error in parsing '// &
     &                  'the '//STR(1:I_LEN(STR))//'th line of configuration '// &
     &                  'file '//CONFIG_FILE_USE(1:I_LEN(CONFIG_FILE_USE))// &
     &                  ' -- unsupported value of the keyword ON_FAIL_TO_READ: '// &
     &                  TRIM(ON_FAIL_TO_READ_STR)//'. Acceptable values: '// &
     &                  'ignore warning stop' )
                   RETURN 
              END IF
         END IF
 410  CONTINUE 
!
! --- Check whether all parameters have been specified in the configuration file
!
      IF ( NERS%CNF%N_URL == 0 ) THEN
           CALL ERR_LOG ( 4246, IUER, 'NERS_INIT', 'Keyword URL was not defined '// &
     &         'in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(NERS%CNF%FCS_FILE) == 0 ) THEN
           CALL ERR_LOG ( 4247, IUER, 'NERS_INIT', 'Keyword FCS_FILE was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(NERS%CNF%LEAPSEC_FILE) == 0 ) THEN
           CALL ERR_LOG ( 4248, IUER, 'NERS_INIT', 'Keyword LEAPSEC_FILE was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(CONN_TIMEOUT_STR) == 0 ) THEN
           CALL ERR_LOG ( 4249, IUER, 'NERS_INIT', 'Keyword CONN_TIMEOUT was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(READ_TIMEOUT_STR) == 0 ) THEN
           CALL ERR_LOG ( 4250, IUER, 'NERS_INIT', 'Keyword READ_TIMEOUT was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(NTRIES_STR) == 0 ) THEN
           CALL ERR_LOG ( 4251, IUER, 'NERS_INIT', 'Keyword N_TRIES was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(AGE_FCS_STR) == 0 ) THEN
           CALL ERR_LOG ( 4252, IUER, 'NERS_INIT', 'Keyword AGE_FCS was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(AGE_SPL_STR) == 0 ) THEN
           CALL ERR_LOG ( 4253, IUER, 'NERS_INIT', 'Keyword AGE_SPL was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( ILEN(LTP_USE_STR) == 0 ) THEN
           CALL ERR_LOG ( 4254, IUER, 'NERS_INIT', 'Keyword LTP_USAGE was not '// &
     &         'defined in the control file '//CONFIG_FILE_USE ) 
           RETURN 
      END IF
!
      IF ( NERS%CNF%CONN_TIMEOUT < NERS__TIMEOUT_MIN ) THEN
           WRITE ( UNIT=STR, FMT='(F6.1)' ) NERS__TIMEOUT_MIN
           CALL ERR_LOG ( 4255, IUER, 'NERS_INIT', 'CONN_TIMEOUT '// &
     &          TRIM(CONN_TIMEOUT_STR)//' defined in the control file '// &
     &          TRIM(CONFIG_FILE_USE)//' is too short. The minimum timeout is '// &
     &          TRIM(STR) )
           RETURN 
      END IF
!
      IF ( NERS%CNF%READ_TIMEOUT < NERS__TIMEOUT_MIN ) THEN
           WRITE ( UNIT=STR, FMT='(F6.1)' ) NERS__TIMEOUT_MIN
           CALL ERR_LOG ( 4256, IUER, 'NERS_INIT', 'READ_TIMEOUT '// &
     &         TRIM(READ_TIMEOUT_STR)//' defined in the control file '// &
     &         TRIM(CONFIG_FILE_USE)//' is too short. The minimum timeout is '// &
     &         TRIM(STR) )
           RETURN 
      END IF
!
      IF ( NERS%CNF%LOCK_TIMEOUT < NERS__TIMEOUT_MIN ) THEN
           WRITE ( UNIT=STR, FMT='(F6.1)' ) NERS__TIMEOUT_MIN
           CALL ERR_LOG ( 4257, IUER, 'NERS_INIT', 'LOCK_TIMEOUT '// &
     &         TRIM(LOCK_TIMEOUT_STR)//' defined in the control file '// &
     &         TRIM(CONFIG_FILE_USE)//' is too short. The minimum timeout is '// &
     &         TRIM(STR) )
           RETURN 
      END IF
!
      IF ( NERS%CNF%AGE_FCS < NERS__AGE_MIN ) THEN
           WRITE ( UNIT=STR, FMT='(F6.1)' ) NERS__AGE_MIN
           CALL ERR_LOG ( 4258, IUER, 'NERS_INIT', 'AGE '//TRIM(AGE_FCS_STR)// &
     &         ' defined in the control file '//TRIM(CONFIG_FILE_USE)// &
     &         ' is too short. The minimum age is '//TRIM(STR) )
           RETURN 
      END IF
!
      IF ( NERS%CNF%AGE_SPL > 0 .AND. NERS%CNF%AGE_SPL < NERS%CNF%AGE_FCS ) THEN
           CALL ERR_LOG ( 4259, IUER, 'NERS_INIT', 'Parameter '// &
     &         'AGE_SPL defined in the control file '// &
     &          TRIM(CONFIG_FILE_USE)//' should be either negative or '// &
     &         'be equal or greater than AGE_FCS' )
           RETURN 
      END IF
!
      IF ( ILEN(NERS%CNF%ON_FAIL_TO_READ) == 0 ) THEN
           CALL ERR_LOG ( 4260, IUER, 'NERS_INIT', 'Parameter ON_FAIL_TO_READ: '// &
     &         'was not defined in the control file '//CONFIG_FILE_USE )
           RETURN 
      END IF
!
      NERS%WARN_LTP   = .FALSE.
      NERS%FCS_STATUS = NERS__INIT
!
      ID = LINDEX ( NERS%CNF%FCS_FILE, '/' )
      IF ( ID == 0 ) ID = ILEN(NERS%CNF%FCS_FILE)
      NERS_IO_LOCK_FILE    = NERS%CNF%FCS_FILE(1:ID)//NERS__IO_LOCK
      NERS_READ_LOCK_FILE  = NERS%CNF%FCS_FILE(1:ID)//NERS__READ_LOCK
      NERS_WRITE_LOCK_FILE = NERS%CNF%FCS_FILE(1:ID)//NERS__WRITE_LOCK
!
! --- Set write lock for the leap second file
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_WRITE_LOCK ( NERS_IO_LOCK_FILE, NERS_READ_LOCK_FILE, &
     &                      NERS_WRITE_LOCK_FILE, NERS%CNF%LOCK_TIMEOUT, &
     &                      NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4261, IUER, 'NERS_INIT', 'Error in setting '// &
     &         'write lock for the ners leap second file' )
           RETURN 
      END IF
!
! --- Check whether the local leap second file exists
!
      WRITE ( UNIT=STR(1:8), FMT='(I8.8)' ) GETPID()
      INQUIRE ( FILE=NERS%CNF%LEAPSEC_FILE, EXIST=LEX ) 
      IF ( LEX ) THEN
           IER = IUER
!
! -------- Read and parse local leap second file.
!
           CALL NERS_READ_LEAPSEC ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4262, IUER, 'NERS_INIT', 'Error in reading '// &
          &         'and parsing local leap second file '//NERS%CNF%LEAPSEC_FILE )
                RETURN 
           END IF
      END IF
!
! --- Lift write lock
!
      CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
      NERS%CONFIG_FILE = CONFIG_FILE_USE
      IF ( LEX .AND. TIME_TAI_STOP == -1.0D0 .AND. TIME_TAI_START == -1.0D0 ) THEN
           UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
!
! -------- Read the leap second file
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4263, IUER, 'NERS_INIT', 'Trap of internal '// &
     &              'control: cannot determine UTC minus TAI on the current epoch' )
                RETURN 
           END IF
           NERS%TIM_START  = UTC_CUR - UTC_M_TAI
           NERS%TIM_STOP   = UTC_CUR - UTC_M_TAI
         ELSE
           NERS%TIM_START  = TIME_TAI_START
           NERS%TIM_STOP   = TIME_TAI_STOP
      END IF
      NERS%TIM_MATROT_LAST = 1.D30
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_INIT  !#!#
