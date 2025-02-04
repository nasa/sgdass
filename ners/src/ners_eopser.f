      PROGRAM    NERS_EOPSER
! ************************************************************************
! *                                                                      *
! *   Program  NERS_EOPSER
! *                                                                      *
! *  ### 16-JUN-2016  NERS_EOPSER  v1.6 (c)  L. Petrov  17-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  M_OPT
      PARAMETER  ( M_OPT = 20 )
      INTEGER*4  M_PAR, M_DOC, M_SER
      PARAMETER  ( M_PAR = 8 )
      PARAMETER  ( M_DOC = 1024 )
      PARAMETER  ( M_SER = 16*1024*1024 )
      CHARACTER  OPTS(M_OPT)*128, NERS_CONFIG*128, NERS_DOC_FILE*128, &
     &           NERS_DOC(M_DOC)*256, CPARM*16, STR*128, STR1*128, &
     &           COMSTR*256, FILE_AGE_STR*10, FCS_AGE_STR*7
      REAL*8     PARS(NERS__MPAR), EPS, TAI_BEG, TAI_END, &
     &           TIM_STEP, UTC_M_TAI, UTC_CUR
      REAL*8,    ALLOCATABLE :: TIM(:), SER(:,:)
      LOGICAL*1  FL_PAR, FL_BEG, FL_END, FL_STP, FL_RAN, LEX
      INTEGER*4  J1, J2, J3, J4, NS, IVRB, MJD, NOPT, L_PAR, IND, IDAY, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23, GET_CDATE*19
!
      NOPT = IARGC()
      NERS_CONFIG = ' '
      IVRB        = 0
      EPS         =  2.0D0
      FL_PAR      = .FALSE.
      FL_BEG      = .FALSE.
      FL_END      = .FALSE.
      FL_STP      = .FALSE.
      FL_RAN      = .FALSE.
      IF ( NOPT == 0 ) THEN
           WRITE ( 6, '(A)' ) 'ners_eopser -c config_file -p param_name '// &
     &                        '-b start date -e stop date -s time_step [-v verbosity] [-r ]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 0, COMSTR )
           DO 410 J1=1,NOPT
              CALL GETARG ( J1, OPTS(J1) )
              COMSTR = TRIM(COMSTR)//' '//TRIM(OPTS(J1))
 410       CONTINUE 
      END IF
!
! --- Allocate dymanic memory
!
      ALLOCATE ( TIM(M_SER), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH8 ( INT8(8)*INT8(M_SER), STR )
           IUER = -1
           CALL ERR_LOG ( 4101, IUER, 'NERS_EOPSER', 'Failure in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory for array TIM' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SER(M_SER,M_PAR), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH8 ( INT8(8)*INT8(M_SER)*INT8(M_PAR), STR )
           IUER = -1
           CALL ERR_LOG ( 4102, IUER, 'NERS_EOPSER', 'Failure in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory for arrya SER' )
           CALL EXIT ( 1 )
      END IF
!
! --- Get options
!
      IND = 0
      DO 420 J2=1,NOPT
         IND = IND + 1
         IF ( IND > NOPT ) GOTO 420
         IF ( OPTS(IND) == '-h' ) THEN
              NERS_DOC_FILE = NERS__PREFIX//'/doc/ners_eopser.txt'
              INQUIRE ( FILE=NERS_DOC_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4103, IUER, 'NERS_EOPSER', 'Trap of internal '// &
     &                 'error: cannot fund file '//NERS_DOC_FILE )
                   CALL EXIT ( 1 )
              END IF
              IUER = -1
              CALL RD_TEXT ( NERS_DOC_FILE, M_DOC, NERS_DOC, NS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4104, IUER, 'NERS_EOPSER', 'Trap of internal '// &
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
              FL_PAR = .TRUE.
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
            ELSE IF ( OPTS(IND) == '-b' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed start time epoch'
                   CALL EXIT ( 1 )
              END IF
              CALL DATE_TO_TIME ( OPTS(IND+1), MJD, TIM(1), IUER )
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, '(A)' ) 'Error in parsing start epoch'
                   CALL EXIT ( 1 )
              END IF
              TAI_BEG = (MJD - J2000__MJD)*86400.0D0 +  TIM(1)
              FL_BEG = .TRUE.
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-e' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed stop time epoch'
                   CALL EXIT ( 1 )
              END IF
              IF ( OPTS(IND+1) == '-1'        .OR. &
     &             OPTS(IND+1)(1:3) == '-1.'  .OR. &
     &             OPTS(IND+1)(1:3) == 'END'  .OR. &
     &             OPTS(IND+1)(1:3) == 'End'  .OR. &
     &             OPTS(IND+1)(1:3) == 'end'  .OR. &
     &             OPTS(IND+1)(1:4) == '-1.0'      ) THEN
!
                   TAI_END = -1.0D0
                 ELSE IF ( OPTS(IND+1)(1:3) == 'NOW' .OR. &
     &                     OPTS(IND+1)(1:3) == 'Now' .OR. &
     &                     OPTS(IND+1)(1:3) == 'now'      ) THEN
                   TAI_END = TIME ( %VAL(0) ) - UNIX__J2000_UTC
                 ELSE IF ( OPTS(IND+1)(1:6) == 'future' .AND. &
     &                     I_LEN(OPTS(IND+1)) > 7       .AND. &
     &                     OPTS(IND+1)(I_LEN(OPTS(IND+1)):I_LEN(OPTS(IND+1))) == 'd' ) THEN
                   CALL CHIN ( OPTS(IND+1)(7:I_LEN(OPTS(IND+1))-1), IDAY )
                   IF ( IDAY < 0 .OR. IDAY > 90 ) THEN
                        WRITE ( 6, '(A)' ) 'Wrong value option -e: and integer .GE. 0 '// &
     &                                     'and .LE. 90 was expected within futureXXXd '// &
     &                                     'field '//OPTS(IND+1)(1:I_LEN(OPTS(IND+1)))
                        CALL EXIT ( 1 )
                   END IF
                   TAI_END = TIME ( %VAL(0) ) - UNIX__J2000_UTC + IDAY*86400.0
                 ELSE
                   CALL DATE_TO_TIME ( OPTS(IND+1), MJD, TIM(1), IUER )
                   IF ( IUER .NE. 0 ) THEN
                        WRITE ( 6, '(A)' ) 'Error in parsing stop epoch'
                        CALL EXIT ( 1 )
                   END IF
                   TAI_END = (MJD - J2000__MJD)*86400.0D0 +  TIM(1)
              END IF
              FL_END = .TRUE.
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-s' ) THEN 
              IF ( J2 == NOPT ) THEN
                   WRITE ( 6, '(A)' ) 'Missed time step'
                   CALL EXIT ( 1 )
              END IF
              IF ( INDEX( OPTS(IND+1), '.' ) > -1 ) THEN
                   READ ( UNIT=OPTS(IND+1), FMT='(F10.1)', IOSTAT=IUER ) TIM_STEP
                 ELSE 
                   STR = TRIM(OPTS(IND+1))//'.0'
                   READ ( UNIT=STR, FMT='(F10.1)', IOSTAT=IUER ) TIM_STEP
              END IF
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, '(A)' ) 'Error in parsing time step'
                   CALL EXIT ( 1 )
              END IF
              FL_STP = .TRUE.
              IND = IND + 1
            ELSE IF ( OPTS(IND) == '-r' ) THEN 
              FL_RAN = .TRUE.
            ELSE
              WRITE ( 6, '(A)' ) 'Unrecognized option '//TRIM(OPTS(IND))
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
      IF ( CPARM == ' ' ) CPARM = 'polu'
!
! --- Buiild NERS configuration file, if it was not explicitly specitied
!
      IF ( NERS_CONFIG == ' ' ) THEN
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4105, IUER, 'NERS_EOPSER', 'Error in initializing '// &
     &         'NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_RAN ) THEN
           IUER = -1
           CALL NERS_INQ ( NERS, 'range', 2, L_PAR, PARS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4106, IUER, 'NERS_EOPSER', 'Error in inquring '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
           STR  = TIM_TO_DATE ( PARS(1), IUER )
           STR1 = TIM_TO_DATE ( PARS(2), IUER )
           WRITE ( 6, '(A, 1X, A)' ) STR(1:21), STR1(1:21)
           CALL EXIT ( 0 )
      END IF
!
      IF ( .NOT. FL_PAR ) THEN
           WRITE ( 6, '(A)' ) 'Please specify parameter name. One of euler, '// &
     &                        'euler_r, polu, polu_r, heo, heo_r, nut'
           CALL EXIT ( 1 )
      END IF 
      IF ( .NOT. FL_BEG ) THEN
           WRITE ( 6, '(A)' ) 'Please specify start date in format YYYY.MM.DD_hh:mm:ss'
           CALL EXIT ( 1 )
      END IF 
      IF ( .NOT. FL_END ) THEN
           WRITE ( 6, '(A)' ) 'Please specify stop  date in format YYYY.MM.DD_hh:mm:ss'
           CALL EXIT ( 1 )
      END IF 
      IF ( .NOT. FL_STP ) THEN
           WRITE ( 6, '(A)' ) 'Please specify time step of the series'
           CALL EXIT ( 1 )
      END IF 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4107, IUER, 'NERS_EOPSER', 'Error in initializing '// &
     &         'NERS data structure' )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL NERS_GET_SERIES ( NERS, TAI_BEG, TAI_END, TIM_STEP, &
     &                       CPARM, M_PAR, M_SER, NS, TIM, SER, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4108, IUER, 'NERS_EOPSER', 'Error in computing '// &
     &         'EOP time series' )
           CALL EXIT ( 1 )
      END IF 
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
      WRITE ( UNIT=FILE_AGE_STR, FMT='(F10.0)' ) UTC_CUR - NERS%UTC_FILE
      CALL CHASHL ( FILE_AGE_STR )
      FILE_AGE_STR(I_LEN(FILE_AGE_STR):I_LEN(FILE_AGE_STR)) = ' '
      WRITE ( UNIT=FCS_AGE_STR, FMT='(F7.1)' ) MAX ( 0.0, (UTC_CUR - NERS%FCS%TAI_GEN)/3600.0)
      CALL CHASHL ( FCS_AGE_STR )
!
      WRITE ( 6, '(A)' ) NERS__SER_FMT
      WRITE ( 6, '(A)' ) '#'
      WRITE ( 6, '(A)' ) '# Output type: '//TRIM(CPARM)
      WRITE ( 6, '(A)' ) '#'
      WRITE ( 6, '(A)' ) '# See http://earthrotation.net'
      WRITE ( 6, '(A)' ) '#'
      WRITE ( 6, '(A)' ) '# Generated by '//NERS__LABEL//' on '//GET_CDATE()
      WRITE ( 6, '(A)' ) '# NERS server version:                  '//TRIM(NERS%FCS%EOP_FCS_VERS)
      WRITE ( 6, '(A)' ) '# NERS client version:                  '//NERS__LABEL
      WRITE ( 6, '(A)' ) '# NERS local forecast file name:        '//TRIM(NERS%CNF%FCS_FILE)
      WRITE ( 6, '(A)' ) '# NERS local forecast file age:         '//TRIM(FILE_AGE_STR)//' s'
      WRITE ( 6, '(A)' ) '# NERS forecast age:                    '//TRIM(FCS_AGE_STR)//' h'
      WRITE ( 6, '(A)' ) '# Used NERS URL:                        '//TRIM(NERS%FCS%NERS_URL)
      WRITE ( 6, '(A)' ) '# Nutation   apriori modei:             '//TRIM(NERS%FCS%NUT_APR_MOD)
      WRITE ( 6, '(A)' ) '# Precession apriori modei:             '//TRIM(NERS%FCS%PRC_APR_MOD)
      WRITE ( 6, '(A)' ) '# UT1 variations zonal tide model:      '//TRIM(NERS%FCS%E3Z_APR_MOD)
      WRITE ( 6, '(A)' ) '# HEO apriori model:                    '//TRIM(NERS%FCS%HEO_MOD)
      WRITE ( 6, '(A)' ) '# HEO model id:                         '//TRIM(NERS%FCS%HEO_ID)
      WRITE ( 6, '(A)' ) '# EOP forecast model version:           '//TRIM(NERS%FCS%EANG_MOD)
      WRITE ( 6, '(A)' ) '# EOP long-term predict. model version: '//TRIM(NERS%FCS%LTP_MOD)
      WRITE ( 6, '(A)' ) '# Forecast generation time:             '//TIM_TO_DATE ( NERS%FCS%TAI_GEN, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# URL of used AAM                       '//TRIM( NERS%FCS%URL_A )
      WRITE ( 6, '(A)' ) '# URL of used IAA Intensives:           '//TRIM( NERS%FCS%URL_J )
      WRITE ( 6, '(A)' ) '# URL of used IVS Intensives:           '//TRIM( NERS%FCS%URL_I )
      WRITE ( 6, '(A)' ) '# URL of used IVS 24-hours:             '//TRIM( NERS%FCS%URL_S )
      WRITE ( 6, '(A)' ) '# URL of used UltraRapid IGS:           '//TRIM( NERS%FCS%URL_U )
      WRITE ( 6, '(A)' ) '# URL of used Rapid IGS:                '//TRIM( NERS%FCS%URL_R )
      WRITE ( 6, '(A)' ) '# URL of used Final CODE:               '//TRIM( NERS%FCS%URL_F )
      WRITE ( 6, '(A)' ) '# URL of used C04:                      '//TRIM( NERS%FCS%URL_C )
      WRITE ( 6, '(A)' ) '# URL of used Long-term pred.:          '//TRIM( NERS%FCS%URL_L )
      WRITE ( 6, '(A)' ) '# Last used epoch of AAM forecast:      '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_A, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of AAM assimilation:  '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_A_ASS, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of IAA Intensives:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_J, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of IVS Intensives:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_I, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of IVS 24-hours:      '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_S, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of UltraRapid IGS:    '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_U, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of Rapid IGS:         '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_R, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of Final CODE:        '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_F, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of C04:               '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_C, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '# Last used epoch of Long-term pred.:   '//TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_L, IUER )//' TAI'
      WRITE ( 6, '(A)' ) '#'
      WRITE ( 6, '(A)' ) '# Command line: '//TRIM(COMSTR)
      WRITE ( 6, '(A)' ) '#'
      WRITE ( 6, '(A)' ) '#     Format description:'
      WRITE ( 6, '(A)' ) '#'
      IF ( CPARM == 'euler' ) THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---  row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  E1    rad  Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  E2    rad  Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  E3    rad  Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 E1-angle (rad)  E2-angle(rad)   E3-angle (rad)'
         ELSE IF ( CPARM == 'euler_r' ) THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---    row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s      Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---    TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  E1    rad    Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  E2    rad    Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  E3    rad    Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#  110:124  1PD15.8  XPR   rad/s  Rate of change of the Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#  126:140  1PD15.8  YPR   rad/s  Rate of change of the Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#  142:156  1PD15.8  U1R   s/s    Rate of change of the Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 E1-angle (rad)  E2-angle(rad)   E3-angle (rad)             E1-rate (rad/s) E2-rate (rad/s) E3-rate (rad/s)'
         ELSE IF ( CPARM == 'polu'    )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---  row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  XPol  rad  X pole coordinate'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  YPol  rad  Y pole coordinate'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  UT1   s    UT1 minus TAI'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (arcsec) Y-pole (arcsec) UT1-TAI (sec)'
         ELSE IF ( CPARM == 'poluz'    )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---  row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  XPol  rad  X pole coordinate'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  YPol  rad  Y pole coordinate'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  UT1Z  s    UT1 minus TAI with the contribution of zonal tide removed'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (arcsec) Y-pole (arcsec) UT1-TAI (sec)'
         ELSE IF ( CPARM == 'polu_r'  )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---    row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s      Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---    TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  XPol  rad    X pole coordinate'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  YPol  rad    Y pole coordinate'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  UT1   s      UT1 minus TAI'
           WRITE ( 6, '(A)' ) '#  110:124  1PD15.8  E1R   rad/s  Rate of change of the X pole coordinate'
           WRITE ( 6, '(A)' ) '#  126:140  1PD15.8  E2R   rad/s  Rate of change of the Y pole coordinate'
           WRITE ( 6, '(A)' ) '#  142:156  1PD15.8  E3R   rad/s  Rate of change of the UT1'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (rad)    Y-pole (rad)    UT1-TAI (sec)              X-pole (rad/s)  Y-pole (rad/s)  UT1-TAI (sec/s)'
         ELSE IF ( CPARM == 'poluz_r'  )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---    row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s      Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---    TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  XPol  rad    X pole coordinate'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  YPol  rad    Y pole coordinate'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  UT1   s      UT1 minus TAI with the contribution of zonal tide removed'
           WRITE ( 6, '(A)' ) '#  110:124  1PD15.8  E1R   rad/s  Rate of change of the X pole coordinate'
           WRITE ( 6, '(A)' ) '#  126:140  1PD15.8  E2R   rad/s  Rate of change of the Y pole coordinate'
           WRITE ( 6, '(A)' ) '#  142:156  1PD15.8  E3R   rad/s  Rate of change of the UT1 with the contribution of zonal tide removed'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (rad)    Y-pole (rad)    UT1-TAI (sec)              X-pole (rad/s)  Y-pole (rad/s)  UT1-TAI (sec/s)'
         ELSE IF ( CPARM == 'heo'    )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---  row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  H1    rad  The contribution to the Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  H2    rad  The contribution to the Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  H3    rad  The contribution to the Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 H1-angle (rad)  H2-angle(rad)   H3-angle (rad)'
         ELSE IF ( CPARM == 'heo_r'  )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---    row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s      Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---    TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  H1    rad    The contribution to the Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  H2    rad    The contribution to the Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#   83:97   1PD15.8  H3    rad    The contribution to the Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#  110:124  1PD15.8  H1R   rad/s  The rate of change of the contribution to the Euler rotation angle with respect to axis 1'
           WRITE ( 6, '(A)' ) '#  126:140  1PD15.8  H2R   rad/s  The rate of change of the contribution to the Euler rotation angle with respect to axis 2'
           WRITE ( 6, '(A)' ) '#  142:156  1PD15.8  H3R   rad/s  The rate of change of the contribution to the Euler rotation angle with respect to axis 3'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 H1-angle (rad)  H2-angle(rad)   H3-angle (rad)             H1-rate (rad/s) H2-rate (rad/s) H3-rate (rad/s)'
         ELSE IF ( CPARM == 'nut'    )  THEN
           WRITE ( 6, '(A)' ) '#    1:7    I7       Ind   ---  row index'
           WRITE ( 6, '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
           WRITE ( 6, '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
           WRITE ( 6, '(A)' ) '#   51:65   1PD15.8  Dpsi  rad  Nutation in longitude'
           WRITE ( 6, '(A)' ) '#   67:81   1PD15.8  Deps  rad  Nutation in obliquity'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Ind  TAI time    Calendar time                 Dpsi nut (rad)  Deps nut (rad)'
      END IF
      WRITE ( 6, '(A)' ) '#'
      DO 440 J4=1,NS
         IF ( CPARM == 'euler' .OR. CPARM == 'polu' .OR. CPARM == 'poluz' .OR. CPARM == 'heo' ) THEN
              WRITE ( 6, 110 ) J4, TAI_BEG + TIM(J4), TIM_TO_DATE ( TAI_BEG + TIM(J4), IUER ), SER(J4,1:3)
            ELSE IF ( CPARM == 'euler_r' .OR. CPARM == 'polu_r' .OR. CPARM == 'poluz_r' .OR. CPARM == 'heo_r' ) THEN
              WRITE ( 6, 120 ) J4, TAI_BEG + TIM(J4), TIM_TO_DATE ( TAI_BEG + TIM(J4), IUER ), SER(J4,1:6)
            ELSE IF ( CPARM == 'nut' ) THEN
              WRITE ( 6, 130 ) J4, TAI_BEG + TIM(J4), TIM_TO_DATE ( TAI_BEG + TIM(J4), IUER ), SER(J4,1:2)
         END IF
 110     FORMAT ( I7, 1X, F12.1, 1X, A, ' EOP= ', 3(1PD15.8,1X) )
 120     FORMAT ( I7, 1X, F12.1, 1X, A, ' EOP= ', 3(1PD15.8,1X), &
     &            ' EOP_Rate= ', 3(1PD15.8,1X) )
 130     FORMAT ( I7, 1X, F12.1, 1X, A, ' NUT= ', 2(1PD15.8,1X) )
 440  CONTINUE 
!
      CALL NERS_QUIT ( NERS__ALL, NERS )
      DEALLOCATE ( SER )
!      
      END  PROGRAM    NERS_EOPSER  !#!  
