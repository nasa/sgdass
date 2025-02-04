      PROGRAM    UTCMTAI
! ************************************************************************
! *                                                                      *
! *   Routine  UTCMTAI
! *                                                                      *
! *  ### 22-JUN-2016    UTCMTAI    v2.1 (c)  L. Petrov  31-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  NERS_CONFIG*128, CPARM*16, DATE_STR*32
      REAL*8     UTC_TAG, UTC, UTC_M_TAI
      LOGICAL*1  FL_CUR, LEX
      INTEGER*4  J1, J2, J3, IS, UNIX_DATE, NS, IVRB, MJD, NOPT, L_PAR, IUER
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME, FILE_INFO
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      IF ( IARGC() > 0 ) THEN
           CALL GETARG ( 1, DATE_STR )
           IF ( DATE_STR(1:2) == '-h' .OR. DATE_STR(1:3) == '--h' ) THEN
                WRITE ( 6, * ) 'Usage: utcmtai [date]'
                WRITE ( 6, * ) ' '
                WRITE ( 6, * ) 'Program utcmtai returns the value of function UTC minus TAI'
                WRITE ( 6, * ) 'If the optional date argument is present, UTC minus TAI for that epoch is returned'
                WRITE ( 6, * ) 'Otherwise, the value on the current epoch is returned.'
                CALL EXIT ( 0 )
           END IF
         ELSE 
           CALL CLRCH  ( DATE_STR )
      END IF
!
! --- Get NERS_CONFIG file
! --- First, check environment variable NERS_CONFIG
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
!
! -------- Second, check $HOME/.ners_config file
!
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
!
! ------------- Third, check for the system-wide ners configuration file 
!
                NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
! --- Innitialization of NERS structures, reading andparsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4001, IUER, 'UT1CTAI', 'Error in initializing '// &
     &         'NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check the age of the NERS date update
!
      IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )
      IF ( IS .NE. 0       .OR.  &
     &     SIZE_I8 .EQ. 0  .OR.  &
     &     (TIME(%VAL(0)) - UNIX_DATE) > NERS%CNF%AGE_FCS ) THEN
!
! -------- If NERS object is too old, fetch it.
!
           IUER = -1
           CALL NERS_FETCH ( NERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4002, IUER, 'UT1CTAI', 'Error in '// &
     &               'an attempt to retrieve NERS forecast parameters '// &
     &               'form the remote server' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Parse NERS message and load it
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4003, IUER, 'UT1CTAI', 'Error in '// &
     &         'an attempt to retrieve NERS forecast parameters '// &
     &         'form the remote server' )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse the date
!
      IF ( ILEN(DATE_STR) == 0 ) THEN
!
! -------- Get the current date
!
           UTC_TAG = TIME ( %VAL(0) ) - UNIX__J2000_UTC
         ELSE
           CALL DATE_TO_TIME ( DATE_STR, MJD, UTC, IUER )
           UTC_TAG = (MJD - J2000__MJD)*86400.0D0 + UTC
      END IF
!
! --- Get UTC minus TAU function
!
      IUER = -1
      CALL NERS_GET_UTCMTAI ( NERS, UTC_TAG, UTC_M_TAI, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4004, IUER, 'UT1CTAI', 'Error in getting '// &
     &         'UTC minus TAI' )
           CALL EXIT ( 1 )
      END IF
!
! --- Print UTC minus TAI function
!
      WRITE ( 6, 110 ) UTC_M_TAI
 110  FORMAT ( F5.1 )
      END  PROGRAM    UTCMTAI  !#!  
