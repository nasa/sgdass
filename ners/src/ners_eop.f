      PROGRAM    NERS_EOP
! ************************************************************************
! *                                                                      *
! *   Program NERS_EOP
! *                                                                      *
! *  ### 16-JUN-2016    NERS_EOP   v2.4 (c)  L. Petrov  31-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  M_OPT
      PARAMETER  ( M_OPT = 64 )
      INTEGER*4  M_PAR, M_SER
      PARAMETER  ( M_PAR = 6, M_SER = 8192 )
      CHARACTER  OPTS(M_OPT)*128, NERS_CONFIG*128, HOME_DIR*128, NERS_DOC_FILE*128, &
     &           NERS_DOC(M_SER)*256, CPARM*16, STR*128, STR1*128, STR2*128, &
     &           FILE_AGE_STR*10, FCS_AGE_STR*7, NERS_MAT_FORMAT*128
      REAL*8     PARS(NERS__MPAR), TAI_EOP, UTC_EOP, EPS, TIM, UTC_CUR, UTC_M_TAI
      LOGICAL*1  FL_CUR, FL_RAN, FL_INFO, LEX
      INTEGER*4  J1, J2, J3, NS, IVRB, MJD, NOPT, L_PAR, IND, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      NOPT = IARGC()
      NERS_CONFIG = ' '
      CPARM       = ' '
      IVRB        = 0
      FL_RAN      = .FALSE.
      FL_INFO     = .FALSE.
      FL_CUR      = .TRUE.
      TAI_EOP     = -1.0D14
      EPS         = 200.0D0
      DO 410 J1=1,NOPT
         CALL GETARG ( J1, OPTS(J1) )
 410  CONTINUE 
!
      IND = 0
      DO 420 J2=1,NOPT
         IND = IND + 1
         IF ( IND > NOPT ) GOTO 420
         IF ( OPTS(IND) == '-h' ) THEN
              NERS_DOC_FILE = NERS__PREFIX//'/doc/ners_eop.txt'
              INQUIRE ( FILE=NERS_DOC_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4001, IUER, 'NERS_EOPS', 'Trap of internal '// &
     &                 'error: cannot fund file '//NERS_DOC_FILE )
                   CALL EXIT ( 1 )
              END IF
              IUER = -1
              CALL RD_TEXT ( NERS_DOC_FILE, M_SER, NERS_DOC, NS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4002, IUER, 'NERS_EOPS', 'Trap of internal '// &
     &                 'error: cannot fund file '//NERS_DOC_FILE )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( 6, '(A)' ) ' '
              DO 430 J3=1,NS
                 WRITE ( 6, '(A)' ) TRIM(NERS_DOC(J3))
 430          CONTINUE 
              WRITE ( 6, '(A)' ) ' '
              CALL EXIT ( 0 )
            ELSE IF ( OPTS(IND) == '-c' ) THEN
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed configuration file name' 
                   CALL EXIT ( 1 )
              END IF
              NERS_CONFIG = OPTS(IND+1)
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-p' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed EOP parameter name' 
                   CALL EXIT ( 1 )
              END IF
              CPARM= OPTS(IND+1)
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-v' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed verbosity level'
                   CALL EXIT ( 1 )
              END IF
              READ ( UNIT=OPTS(IND+1), FMT='(I4)', IOSTAT=IUER ) IVRB
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, '(A)' ) 'Verbosity parameter should be an interger'
                   CALL EXIT ( 1 )
              END IF
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-t' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed time epoch'
                   CALL EXIT ( 1 )
              END IF
              CALL DATE_TO_TIME ( OPTS(IND+1), MJD, TIM, IUER )
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, '(A)' ) 'Error in parsing time epoch'
                   CALL EXIT ( 1 )
              END IF
              TAI_EOP = (MJD - J2000__MJD)*86400.0D0 +  TIM
              FL_CUR = .FALSE.
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-r' ) THEN 
              FL_RAN = .TRUE.
            ELSE IF ( OPTS(IND) == '-i' ) THEN 
              FL_INFO = .TRUE.
            ELSE
              WRITE ( 6, '(A)' ) 'Unrecognized option '//TRIM(OPTS(IND))
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
!
! --- Get NERS configuration file
!
      IF ( NERS_CONFIG == ' ' ) THEN
!
! -------- First, check environment variable NERS_CONFIG
!
           CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
           IF ( NERS_CONFIG == ' ' ) THEN
!
! ------------- Second, check $HOME/.ners_config file
!
                CALL GETENVAR ( 'HOME', HOME_DIR )
                NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
                INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
                IF ( .NOT. LEX ) THEN
!
! ------------------ Third, check for the system-wide ners configuration file 
!
                     NERS_CONFIG = NERS__CONFIG
                END IF
           END IF
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'NERS_CONFIG= '//TRIM(NERS_CONFIG)
      END IF
!
      IF ( FL_RAN .OR. FL_INFO ) THEN
           IUER = -1
           CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4003, IUER, 'NERS_EOPS', 'Error in initializing '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
! 
           IUER = -1
           CALL NERS_INQ ( NERS, 'range', 3, L_PAR, PARS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4004, IUER, 'NERS_EOP', 'Error in inquring '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( FL_INFO ) THEN
           UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
           WRITE ( UNIT=FILE_AGE_STR, FMT='(F10.0)' ) UTC_CUR - NERS%UTC_FILE
           CALL CHASHL ( FILE_AGE_STR )
           FILE_AGE_STR(I_LEN(FILE_AGE_STR):I_LEN(FILE_AGE_STR)) = ' '
           WRITE ( UNIT=FCS_AGE_STR, FMT='(F7.1)' ) MAX ( 0.0, (UTC_CUR - NERS%FCS%TAI_GEN)/3600.0)
           CALL CHASHL ( FCS_AGE_STR )
!
           WRITE ( 6, '(A)' ) 'NERS message format:                  '//TRIM(NERS%FCS%NERS_FMT)
           WRITE ( 6, '(A)' ) 'NERS server version:                  '//TRIM(NERS%FCS%EOP_FCS_VERS)
           WRITE ( 6, '(A)' ) 'NERS client version:                  '//NERS__LABEL
           WRITE ( 6, '(A)' ) 'NERS local forecast file name:        '//TRIM(NERS%CNF%FCS_FILE)
           WRITE ( 6, '(A)' ) 'NERS local forecast file age:         '//TRIM(FILE_AGE_STR)//' s'
           WRITE ( 6, '(A)' ) 'NERS forecast age:                    '//TRIM(FCS_AGE_STR)//' h'
           WRITE ( 6, '(A)' ) 'Used NERS URL:                        '//TRIM(NERS%FCS%NERS_URL)
           WRITE ( 6, '(A)' ) 'Nutation   apriori modei:             '//TRIM(NERS%FCS%NUT_APR_MOD)
           WRITE ( 6, '(A)' ) 'Precession apriori modei:             '//TRIM(NERS%FCS%PRC_APR_MOD)
           WRITE ( 6, '(A)' ) 'UT1 variations zonal tide model:      '//TRIM(NERS%FCS%E3Z_APR_MOD)
           WRITE ( 6, '(A)' ) 'HEO apriori model:                    '//TRIM(NERS%FCS%HEO_MOD)
           WRITE ( 6, '(A)' ) 'HEO model id:                         '//TRIM(NERS%FCS%HEO_ID)
           WRITE ( 6, '(A)' ) 'EOP forecast model version:           '//TRIM(NERS%FCS%EANG_MOD)
           WRITE ( 6, '(A)' ) 'EOP long-term predict. model version: '//TRIM(NERS%FCS%LTP_MOD)
           WRITE ( 6, '(A)' ) 'Forecast generation time:             '//TIM_TO_DATE ( NERS%FCS%TAI_GEN, IUER )//' TAI'

           WRITE ( 6, '(A)' ) 'URL of used AAM                       '//TRIM( NERS%FCS%URL_A )
           WRITE ( 6, '(A)' ) 'URL of used IAA Intensives:           '//TRIM( NERS%FCS%URL_J )
           WRITE ( 6, '(A)' ) 'URL of used IVS Intensives:           '//TRIM( NERS%FCS%URL_I )
           WRITE ( 6, '(A)' ) 'URL of used IVS 24-hours:             '//TRIM( NERS%FCS%URL_S )
           WRITE ( 6, '(A)' ) 'URL of used UltraRapid IGS:           '//TRIM( NERS%FCS%URL_U )
           WRITE ( 6, '(A)' ) 'URL of used Rapid IGS:                '//TRIM( NERS%FCS%URL_R )
           WRITE ( 6, '(A)' ) 'URL of used Final CODE:               '//TRIM( NERS%FCS%URL_F )
           WRITE ( 6, '(A)' ) 'URL of used C04:                      '//TRIM( NERS%FCS%URL_C )
           WRITE ( 6, '(A)' ) 'URL of used Long-term pred.:          '//TRIM( NERS%FCS%URL_L )
!
           WRITE ( 6, '(A)' ) 'Last used epoch of AAM forecast:      '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_A, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of AAM assimilation:  '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_A_ASS, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of IAA Intensives:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_J, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of IVS Intensives:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_I, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of IVS 24-hours:      '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_S, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of UltraRapid IGS:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_U, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of Rapid IGS:         '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_R, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of Final CODE:        '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_F, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of C04:               '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_C, IUER )//' TAI'
           WRITE ( 6, '(A)' ) 'Last used epoch of Long-term pred.:   '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_L, IUER )//' TAI'
!!           WRITE ( 6, '(A)' ) 'Last used epoch for HEO observations: '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_HEO, IUER )
           IF ( .NOT. FL_RAN .AND. ILEN(CPARM) == 0 ) CALL EXIT ( 0 ) 
      END IF
      IF ( FL_RAN ) THEN
           STR  = TIM_TO_DATE ( PARS(1), IUER )
           STR1 = TIM_TO_DATE ( PARS(2), IUER )
           STR2 = TIM_TO_DATE ( PARS(3), IUER )
           WRITE ( 6, '(A, 1X, A, 1X, A)' ) STR(1:21), STR1(1:21), STR2(1:21)
           IF ( ILEN(CPARM) == 0 ) CALL EXIT ( 0 )
      END IF
      IF ( CPARM == ' ' ) CPARM = 'ut1mtai'
!
      IF ( FL_CUR ) THEN
           IUER = -1
           CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4005, IUER, 'NERS_EOP', 'Error in initializing '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
           TAI_EOP = NERS__FIL_TIM
         ELSE 
           IUER = -1
           CALL NERS_INIT ( NERS_CONFIG, NERS, TAI_EOP - EPS, TAI_EOP + EPS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4006, IUER, 'NERS_EOP', 'Error in initializing '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL NERS_GET_EOP ( NERS, TAI_EOP, CPARM, NERS__MPAR, L_PAR, PARS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4007, IUER, 'NERS_EOP', 'Error evaluating the '// &
     &         'Earth orientation parameter' )
           CALL EXIT ( 1 )
      END IF
      IF ( FL_CUR ) THEN
           IF ( IVRB == 0 ) THEN
                WRITE ( 6, * ) PARS(1:L_PAR)
             ELSE
                UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
                CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IUER )
                WRITE ( 6, 120 ) TIM_TO_DATE ( UTC_CUR - UTC_M_TAI, IUER ), PARS(1:L_PAR)
           END IF
         ELSE
           WRITE ( 6, 120 ) TIM_TO_DATE ( TAI_EOP, IUER ), PARS(1:L_PAR)
 120       FORMAT ( 'TAI: ', A23, ' EOP: ', 48(1PD19.12,1X) )
           CALL GETENVAR ( 'NERS_MAT_FORMAT', NERS_MAT_FORMAT )
           IF ( NERS_MAT_FORMAT == 'yes' ) THEN
                WRITE ( 6, '(A)' ) '-----------------------------------------'
                WRITE ( 6, '(A,1X,F15.3)' ) 'TAI_EOP = ', TAI_EOP
                WRITE ( 6, '(A)' ) '-----------------------------------------'
                WRITE ( 6, 210 ) 'ROT_MAT(1,1:3): ', PARS(1), PARS(4), PARS(7)
                WRITE ( 6, 210 ) 'ROT_MAT(2,1:3): ', PARS(2), PARS(5), PARS(8)
                WRITE ( 6, 210 ) 'ROT_MAT(3,1:3): ', PARS(3), PARS(6), PARS(9)
!
                WRITE ( 6, '(A)' ) '-----------------------------------------'
!
                WRITE ( 6, 210 ) 'ROT_DR1(1,1:3): ', PARS(10), PARS(13), PARS(16)
                WRITE ( 6, 210 ) 'ROT_DR1(2,1:3): ', PARS(11), PARS(14), PARS(17)
                WRITE ( 6, 210 ) 'ROT_DR1(3,1:3): ', PARS(12), PARS(15), PARS(18)
                WRITE ( 6, '(A)' ) '-----------------------------------------'
!
                WRITE ( 6, 210 ) 'ROT_DR2(1,1:3): ', PARS(19), PARS(22), PARS(25)
                WRITE ( 6, 210 ) 'ROT_DR2(2,1:3): ', PARS(20), PARS(23), PARS(26)
                WRITE ( 6, 210 ) 'ROT_DR2(3,1:3): ', PARS(21), PARS(24), PARS(27)
                WRITE ( 6, '(A)' ) '-----------------------------------------'
!
 210           FORMAT ( A, 3(2X,1F15.12 ) )
           END IF 
      END IF
!
      CALL NERS_QUIT ( NERS__ALL, NERS )
!      
      END  PROGRAM  NERS_EOP  !#!#
