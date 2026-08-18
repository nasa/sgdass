      PROGRAM    FRESEL_HEO_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  FRESEL_HEO  creates
! *                                                                      *
! *  ### 16-OCT-2003   FRESEL_HEO  v1.1 (c)  L. Petrov  26-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'nut.i'
      INCLUDE   'vtd.i'
      INCLUDE   'fresel.i'
      TYPE     ( FRESEL__STRU ) :: FRESEL
      CHARACTER  CONF_FILE*128, STR*32, LOG_FILE*128, BATCH_FILE*128 
      INTEGER*4  IVRB, LUN_LOG, LUN_BATCH, IUER
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      IF ( IARGC() .LT. 1 ) THEN
           WRITE ( 6, * ) 'Usage: fresel_heo <control_file> [verbosity_level]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, CONF_FILE ) 
           IF ( IARGC() .GE. 2 ) THEN
                CALL GETARG ( 2, STR    ) 
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB .LT. 0 ) IVRB = 0
                IF ( IVRB .GT. 4 ) IVRB = 4
              ELSE
                IVRB = 3
           END IF
      END IF
!
      IUER = -1
      CALL INIT_FFTW ( ' ', 0, 1.0D0, 1, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
!
! --- Parse configuration file
!
      IUER = -1
      CALL FRESEL_CONF ( FRESEL, CONF_FILE, IVRB, LOG_FILE, LUN_LOG, &
     &                   BATCH_FILE, LUN_BATCH, IUER ) 
      IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
!
      FRESEL%N_FRQ = 0
      FRESEL%N_PRC = 0
!
      IF ( FRESEL%ALG == 'CLOSE' ) THEN
           IUER = -1
           CALL NUT_FRESEL_HEO ( FRESEL, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 3 )
!
           IUER = -1
           CALL TID_FRESEL_HEO ( FRESEL, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 4 )
!
           IUER = -1
           CALL ADH_FRESEL_HEO ( FRESEL, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 5 )
         ELSE IF ( FRESEL%ALG == 'HAR' ) THEN
           IUER = -1
           CALL FRESEL_HAR ( FRESEL, LUN_LOG, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 6 )
!
           IUER = -1
           CALL FRESEL_TID ( FRESEL, LUN_LOG, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 6 )
!
           IUER = -1
           CALL ADH_FRESEL_HEO ( FRESEL, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 5 )
         ELSE IF ( FRESEL%ALG == 'DRF' ) THEN
           IUER = -1
           CALL FRESEL_DRF ( FRESEL, LUN_LOG, IVRB, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 6 )
      END IF
!
      IUER = -1
      CALL WRI_FRESEL_HEO ( FRESEL, IVRB, LUN_LOG, LUN_BATCH, IUER ) 
      IF ( IUER .NE. 0 ) CALL EXIT ( 7 )
      CLOSE ( UNIT=LUN_LOG )
      CLOSE ( UNIT=LUN_BATCH )
!
      WRITE( 6, * ) 'Output file: '//FRESEL%FILOUT(1:I_LEN(FRESEL%FILOUT))
      WRITE( 6, * ) 'Log    file: '//LOG_FILE(1:I_LEN(LOG_FILE))
      WRITE( 6, * ) 'Batch  file: '//BATCH_FILE(1:I_LEN(BATCH_FILE))
!
      END  !#!  FRESEL_HEO__MAIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FRESEL_CONF ( FRESEL, CONF_FILE, IVRB, LOG_FILE, LUN_LOG, &
     &                         BATCH_FILE, LUN_BATCH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_CONF
! *                                                                      *
! *  ### 16-OCT-2003  FRESEL_CONF  v1.1 (c)  L. Petrov  07-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'nut.i'
      INCLUDE   'heo.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      CHARACTER  CONF_FILE*(*), LOG_FILE*(*), BATCH_FILE*(*)
      INTEGER*4  IVRB, LUN_LOG, LUN_BATCH, IUER
      INTEGER*4  MBUF, MIND
      PARAMETER   ( MBUF = 32*1024, MIND = 32 ) 
      CHARACTER  BUF(MBUF)*128, STR*80, STR1*80, REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//':' )
      LOGICAL*4  LEX, FL_DATE_BEG, FL_DATE_END, FL_DATE_REF, FL_NUTEXP, &
     &           FL_NUTAMP, FL_FILOUT, FL_RFCN_FREQ_MIN, FL_RFCN_FREQ_MAX, &
     &           FL_PFICN_FREQ_MIN, FL_PFICN_FREQ_MAX, FL_TIDAMP,       &
     &           FL_UT1_EST_NUT,    FL_UT1_EST_TID,    FL_UT1_EST_ADH,  &
     &           FL_PM_EST_NUT,     FL_PM_EST_TID,     FL_PM_EST_ADH,   &
     &           FL_PRC_EST,        FL_ADH_FILE,       FL_AMD_DIR,      &
     &           FL_TIDES,          FL_APR_NUT,        FL_EOP_SEG,      &
     &           FL_SEG_LEN,        FL_SIGMA_TIE,      FL_SIGMA_TRN,    &
     &           FL_SIGMA_RAT,      FL_SIGMA_HRM,      FL_IGNORE_SDL,   & 
     &           FL_TIDZON,         FL_ALG,            FL_VTD_CONF,     &
     &           FL_NF_SCALE
      INTEGER*4  NBUF, LIND, IND(2,MIND), IP, IOS, MJD, N_FSL, J1, IER
      REAL*8     SEC, TIDAL_AMPL_M2, TIDAL_AMPL_MF
      REAL*8       M2_AMPL, MF_AMPL, M3_AMPL
      PARAMETER  ( M2_AMPL = 6.1945540D0 )
      PARAMETER  ( MF_AMPL = 0.6526320D0 )
      PARAMETER  ( M3_AMPL = 0.0750421D0 )
      CHARACTER  FSL__CNF*27, FSL__CNF_V2*27
      PARAMETER  ( FSL__CNF_V2 = '# FRESEL  v 5.0  2021.06.26' )
      PARAMETER  ( FSL__CNF    = '# FRESEL  v 6.0  2025.12.03' )
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LINDEX
!
      FL_DATE_BEG       = .FALSE.
      FL_DATE_REF       = .FALSE.
      FL_DATE_END       = .FALSE.
      FL_NUTAMP         = .FALSE.
      FL_NUTEXP         = .FALSE.
      FL_ALG            = .FALSE. 
      FL_FILOUT         = .FALSE.
      FL_RFCN_FREQ_MIN  = .FALSE.
      FL_RFCN_FREQ_MAX  = .FALSE.
      FL_PFICN_FREQ_MIN = .FALSE.
      FL_PFICN_FREQ_MAX = .FALSE.
      FL_TIDAMP         = .FALSE.
      FL_TIDZON         = .FALSE.
      FL_PM_EST_NUT     = .FALSE.
      FL_PM_EST_TID     = .FALSE.
      FL_PM_EST_ADH     = .FALSE.
      FL_UT1_EST_NUT    = .FALSE.
      FL_UT1_EST_TID    = .FALSE.
      FL_UT1_EST_ADH    = .FALSE.
      FL_PRC_EST        = .FALSE.
      FL_ADH_FILE       = .FALSE.
      FL_AMD_DIR        = .FALSE.
      FL_TIDES          = .FALSE.
      FL_APR_NUT        = .FALSE.
      FL_EOP_SEG        = .FALSE.
      FL_SEG_LEN        = .FALSE.
      FL_SIGMA_TIE      = .FALSE.
      FL_SIGMA_TRN      = .FALSE.
      FL_SIGMA_RAT      = .FALSE.
      FL_SIGMA_HRM      = .FALSE.
      FL_IGNORE_SDL     = .FALSE.
      FL_VTD_CONF       = .FALSE.
      FL_NF_SCALE       = .FALSE.
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 3301, IUER, 'FRESEL_CONF', 'Error in an attempt '// &
     &         'to read a control file for nutation estimation '//CONF_FILE )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(FSL__CNF)) .NE.  FSL__CNF ) THEN
           CALL ERR_LOG  ( 3302, IUER, 'FRESEL_CONF', 'Wrong format of '// &
     &         'the configuration file '//TRIM(CONF_FILE)// &
     &         ' lavel '//FSL__CNF//' was expected' )
           RETURN
      END IF
!
! --- Initlialization
!
      CALL NOUT ( SIZEOF(FRESEL), FRESEL )
!
! --- Scan a buffer with contents of user nutation control file
!
      N_FSL = 0
      DO 410 J1=1,NBUF
         IF ( ILEN(BUF(J1)) .EQ. 0  ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '*' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IP = INDEX  ( BUF(J1), '!' )
         IF ( IP .LE. 0 ) IP = ILEN(BUF(J1))
!
! ------ Parse the line onto words
!
         CALL EXWORD ( BUF(J1)(1:IP), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 2 ) GOTO 410
!
! ------ ... and convert the first word into letters to the upper registr
!
         CALL TRAN ( 11, BUF(J1)(IND(1,1):IND(2,1)), BUF(J1)(IND(1,1):IND(2,1)))
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DATE_BEG' ) THEN
              FRESEL%DATE_BEG = BUF(J1)(IND(1,2):IND(2,2)) 
              FL_DATE_BEG = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DATE_REF' ) THEN
              FRESEL%DATE_REF = BUF(J1)(IND(1,2):IND(2,2))
              FL_DATE_REF = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DATE_END' ) THEN
              FRESEL%DATE_END = BUF(J1)(IND(1,2):IND(2,2))
              FL_DATE_END = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'FILOUT' ) THEN
              FRESEL%FILOUT = BUF(J1)(IND(1,2):IND(2,2))
              FL_FILOUT = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'NUTEXP' ) THEN
              FRESEL%NUTEXP = BUF(J1)(IND(1,2):IND(2,2))
              FL_NUTEXP = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'FRESEL_ALGORITHM' ) THEN
              FRESEL%ALG = BUF(J1)(IND(1,2):IND(2,2))
              IF ( FRESEL%ALG == 'CLOSE' ) THEN
                   CONTINUE 
                 ELSE IF ( FRESEL%ALG == 'HAR' ) THEN
                   CONTINUE 
                 ELSE IF ( FRESEL%ALG == 'DRF' ) THEN
                   CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 3302, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "FRESEL_ALGORITHM" value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_ALG = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'NUTAMP_MIN' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', &
     &                IOSTAT=IOS ) FRESEL%NUTAMP_MIN
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3302, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "NUTAMP_MIN" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_NUTAMP = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'RFCN_FREQ_MIN' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.8)', &
     &                IOSTAT=IOS ) FRESEL%RFCN_FREQ_MIN
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3303, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "RFCN_FREQ_MIN" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_RFCN_FREQ_MIN = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'RFCN_FREQ_MAX' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.8)', &
     &                IOSTAT=IOS ) FRESEL%RFCN_FREQ_MAX
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3304, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "RFCN_FREQ_MAX" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_RFCN_FREQ_MAX = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PFICN_FREQ_MIN' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.8)', &
     &                IOSTAT=IOS ) FRESEL%PFICN_FREQ_MIN
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3305, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "PFICN_FREQ_MIN" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_PFICN_FREQ_MIN = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PFICN_FREQ_MAX' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.8)', &
     &                IOSTAT=IOS ) FRESEL%PFICN_FREQ_MAX
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3306, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "PFICN_FREQ_MAX" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FL_PFICN_FREQ_MAX = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IGNORE_SIDELOBES' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%IGNORE_SIDELOBES = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%IGNORE_SIDELOBES = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3307, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword IGNORE_SIDELOBES: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_IGNORE_SDL = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'TIDAL_AMPL_M2' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)', &
     &                IOSTAT=IOS ) TIDAL_AMPL_M2 
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3308, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "TIDAL_AMPL_M2" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FRESEL%TIDAMP_MIN = TIDAL_AMPL_M2*M2_AMPL 
              FL_TIDAMP = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'TIDAL_AMPL_MF' ) THEN
              READ  ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)', &
     &                IOSTAT=IOS ) TIDAL_AMPL_MF 
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3308, IUER, 'FRESEL_CONF', 'Error in '// &
     &                 'parsing "TIDAL_AMPL_MF" value '//STR(1:I_LEN(STR))// &
     &                 ' of the control file '//CONF_FILE )
                   RETURN 
              END IF
              FRESEL%TIDZON_MIN = TIDAL_AMPL_MF*MF_AMPL 
              FL_TIDZON = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PM_EST_NUT' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%PM_EST_NUT = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%PM_EST_NUT = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3309, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword PM_EST_NUT: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_PM_EST_NUT = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PM_EST_TID' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%PM_EST_TID = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%PM_EST_TID = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3310, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword PM_EST_TID: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_PM_EST_TID = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PM_EST_ADH' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%PM_EST_ADH = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%PM_EST_ADH = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3311, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword PM_EST_ADH: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_PM_EST_ADH = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PRC_EST' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%PRC_EST = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%PRC_EST = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3312, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword PRC_EST: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_PRC_EST = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UT1_EST_NUT' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%UT1_EST_NUT = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%UT1_EST_NUT = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3313, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword UT1_EST_NUT: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_UT1_EST_NUT = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UT1_EST_TID' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%UT1_EST_TID = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%UT1_EST_TID = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3314, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword UT1_EST_TID: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_UT1_EST_TID = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UT1_EST_ADH' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%UT1_EST_ADH = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%UT1_EST_ADH = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3315, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword UT1_EST_ADH: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_UT1_EST_ADH = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ADH_FILE' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NO'   .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'no'   .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'none' .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'      ) THEN
                   FRESEL%ADH_FILE = 'NONE'
                 ELSE
                   FRESEL%ADH_FILE = BUF(J1)(IND(1,2):IND(2,2)) 
              END IF
              FL_ADH_FILE = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'VTD_CONF' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NO'   .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. '0'         ) THEN
                   CALL CLRCH ( FRESEL%VTD_CONF )
                 ELSE
                   FRESEL%VTD_CONF = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              FL_VTD_CONF = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'AMD_DIR' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NO'   .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' .OR. &
     &             BUF(J1)(IND(1,2):IND(2,2)) .EQ. '0'         ) THEN
                   CALL CLRCH ( FRESEL%AMD_DIR )
                 ELSE
                   FRESEL%AMD_DIR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              FL_AMD_DIR = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOLID_EARTH_TIDES' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CALC' ) THEN
                   FRESEL%APRIORI_SOLID_TIDES = TID__CALC 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MATHEWS_2001' ) THEN
                   FRESEL%APRIORI_SOLID_TIDES = TID__MTH
                 ELSE 
                   FRESEL%APRIORI_SOLID_TIDES = TID__UNDF
              END IF
              FL_TIDES = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'APRIORI_NUTATION' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CALC' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__CALC 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WAHR1980' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__WAHR1980
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IERS1996' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__IERS1996 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'REN2000' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__REN2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__MHB2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000_ADDON'  ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__MHB2000_ADDON
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000_TRANSF' ) THEN
                   FRESEL%APRIORI_NUTATION = NUT__MHB2000_TRANSF
                 ELSE 
                   FRESEL%APRIORI_NUTATION = NUT__UNDF
              END IF
              FL_APR_NUT = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ESTIMATION_SEGEOP' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'YES' ) THEN
                   FRESEL%SEG_EOP = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+3) .EQ. 'NO' ) THEN
                   FRESEL%SEG_EOP = .FALSE.
                 ELSE 
                   CALL ERR_LOG ( 3316, IUER, 'FRESEL_CONF', 'Wrong value '// &
     &                 'of the keyword SEG_EOP: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- one of YES or NO were was expected' )
                   RETURN 
              END IF
              FL_EOP_SEG = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SEGMENT_LENGTH' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%EOP_SEG
              FRESEL%EOP_SEG = DBLE ( NINT ( FRESEL%EOP_SEG + 0.001) )
              FL_SEG_LEN = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SIGMA_TIE' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%SIGMA_TIE
              FL_SIGMA_TIE = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SIGMA_TRN' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%SIGMA_TRN
              FL_SIGMA_TRN = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SIGMA_RAT' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%SIGMA_RAT
              FL_SIGMA_RAT = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SIGMA_HRM' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%SIGMA_HRM
              FL_SIGMA_HRM = .TRUE.
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'NF_SCALE' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,2):IND(2,2))
              IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.'
              READ ( UNIT=STR, FMT='(F12.5)' ) FRESEL%NF_SCALE
              FL_NF_SCALE = .TRUE.
         END IF
!
         N_FSL = N_FSL + 1
 410  CONTINUE 
!
      IF ( N_FSL .NE. M_FSL ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( N_FSL, STR ) 
           CALL INCH  ( M_FSL, STR1 ) 
           CALL ERR_LOG ( 3317, IUER, 'FRESEL_CONF', 'Worng number of '// &
     &         'keywords in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' -- '//STR(1:I_LEN(STR))// &
     &         ' while '//STR1(1:I_LEN(STR1))//' were expected' ) 
           RETURN 
      END IF
!
      IF ( .NOT. FL_DATE_BEG ) THEN
           CALL ERR_LOG ( 3318, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"DATE_BEG:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_DATE_END ) THEN
           CALL ERR_LOG ( 3319, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"DATE_END:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_FILOUT ) THEN
           CALL ERR_LOG ( 3320, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"FILOUT:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_NUTEXP ) THEN
           CALL ERR_LOG ( 3321, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"NUTEXP:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_NUTAMP ) THEN
           CALL ERR_LOG ( 3322, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"NUTAMP_MIN:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_RFCN_FREQ_MIN ) THEN
           CALL ERR_LOG ( 3323, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"RFCN_FREQ_MIN:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_RFCN_FREQ_MAX ) THEN
           CALL ERR_LOG ( 3324, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"RFCN_FREQ_MAX:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PFICN_FREQ_MIN ) THEN
           CALL ERR_LOG ( 3325, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PFICN_FREQ_MIN:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PFICN_FREQ_MAX ) THEN
           CALL ERR_LOG ( 3326, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PFICN_FREQ_MAX:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
! --- Parse DATE_BEG
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL DATE_TO_TIME ( FRESEL%DATE_BEG, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3327, IUER, 'FRESEL_CONF', 'Error in parsing '// &
     &         'value "DATE_BEG": '//FRESEL%DATE_BEG )
           RETURN 
      END IF
      FRESEL%TIME_BEG = (MJD - J2000__MJD)*86400.0D0 + (SEC - 43200.0D0)
!
! --- Parse DATE_REF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL DATE_TO_TIME ( FRESEL%DATE_REF, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3328, IUER, 'FRESEL_CONF', 'Error in parsing '// &
     &         'value "DATE_BEG": '//FRESEL%DATE_REF )
           RETURN 
      END IF
      FRESEL%TIME_REF = (MJD - J2000__MJD)*86400.0D0 + (SEC - 43200.0D0)
!
! --- Parse DATE_END
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL DATE_TO_TIME ( FRESEL%DATE_END, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3329, IUER, 'FRESEL_CONF', 'Error in parsing '// &
     &         'value "DATE_END": '//FRESEL%DATE_END )
           RETURN 
      END IF
      FRESEL%TIME_END = (MJD - J2000__MJD)*86400.0D0 + (SEC - 43200.0D0)
!
      IF ( FRESEL%NUTEXP .EQ. REN2000__REF ) THEN
           CONTINUE 
         ELSE IF ( FRESEL%NUTEXP .EQ. IERS1996__REF ) THEN
           CONTINUE 
         ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REF ) THEN
           CONTINUE 
         ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REN2000__REF ) THEN
           CONTINUE 
         ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000_TRANSF__REF ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 3330, IUER, 'FRESEL_CONF', 'Wrong value of the '// &
     &         'keyword "NUTEXP": '//FRESEL%NUTEXP//' one of '// &
     &          IERS1996__REF//' or '// &
     &          REN2000__REF //' or '// &
     &          MHB2000__REF //' or '// &
     &          MHB2000_TRANSF__REF //' or '// &
     &          MHB2000__REN2000__REF   )
           RETURN 
      END IF
!
      IF ( .NOT. FL_IGNORE_SDL ) THEN
           CALL ERR_LOG ( 3331, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"IGNORE_SIDELOBES:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_TIDAMP ) THEN
           CALL ERR_LOG ( 3332, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"TIDAMP:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PM_EST_NUT ) THEN
           CALL ERR_LOG ( 3333, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PM_EST_NUT:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PM_EST_TID ) THEN
           CALL ERR_LOG ( 3334, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PM_EST_TID:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PM_EST_ADH ) THEN
           CALL ERR_LOG ( 33365, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PM_EST_ADH:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_UT1_EST_NUT ) THEN
           CALL ERR_LOG ( 3336, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"UT1_EST_NUT:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_UT1_EST_TID ) THEN
           CALL ERR_LOG ( 3337, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"UT1_EST_TID:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_UT1_EST_ADH ) THEN
           CALL ERR_LOG ( 3338, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"UT1_EST_ADH:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_PRC_EST ) THEN
           CALL ERR_LOG ( 3339, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"PRC_EST:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_ADH_FILE ) THEN
           CALL ERR_LOG ( 3340, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"ADH_FILE:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( FRESEL%ADH_FILE .NE. 'NONE' ) THEN
           INQUIRE ( FILE=FRESEL%ADH_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 3341, IUER, 'FRESEL_CONF', 'ADH_FILE '// &
     &               FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &               ' was not found ' )
                RETURN
           END IF
      END IF
!
      IF ( .NOT. FL_EOP_SEG ) THEN
           CALL ERR_LOG ( 3342, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"Estimation_SegEOP:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_SEG_LEN ) THEN
           CALL ERR_LOG ( 3343, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"Segment_length:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_SIGMA_TIE ) THEN
           CALL ERR_LOG ( 3344, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"SIGMA_TRN:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_SIGMA_TRN ) THEN
           CALL ERR_LOG ( 3345, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"SIGMA_TRN:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_SIGMA_RAT ) THEN
           CALL ERR_LOG ( 3346, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"SIGMA_RAT:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_SIGMA_HRM ) THEN
           CALL ERR_LOG ( 3347, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"SIGMA_HRM:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_AMD_DIR ) THEN
           CALL ERR_LOG ( 3348, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"AMD_DIR:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
      IF ( .NOT. FL_TIDES ) THEN
           CALL ERR_LOG ( 3349, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"Solid_Earth_tides:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
!     IF ( FRESEL%APRIORI_SOLID_TIDES .EQ. TID__UNDF ) THEN
!           CALL ERR_LOG ( 3350, IUER, 'FRESEL_CONF', 'A keyword '// &
!     &          '"Solid_Earth_tides:" in file '// &
!     &           CONF_FILE(1:I_LEN(CONF_FILE))// &
!     &          ' has a wrong value. Only Calc or Mathews_2001 are allowed' )
!           RETURN
!      END IF
!
      IF ( .NOT. FL_APR_NUT ) THEN
           CALL ERR_LOG ( 3351, IUER, 'FRESEL_CONF', 'A keyword '// &
     &          '"Apriori_Nutation:" is missed in the control file '// &
     &           CONF_FILE )
           RETURN
      END IF
!
!      IF ( FRESEL%APRIORI_NUTATION .EQ. NUT__UNDF ) THEN
!           CALL ERR_LOG ( 3352, IUER, 'FRESEL_CONF', 'A keyword '// &
!     &          '"Apriori_Nutation:" in file '// &
!     &           CONF_FILE(1:I_LEN(CONF_FILE))// &
!     &          ' has a wrong value. Only Calc or WAHR1980 or IERS1996,'//&
!     &          ' REN2000 or MHB2000 are allowed' )
!           RETURN
!      END IF
!
      FRESEL%NF = FRESEL%NF_SCALE * PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
      FRESEL%CONF_FILE = CONF_FILE
!
      IP = LINDEX ( FRESEL%FILOUT, '.' ) - 1
!
      IF ( IP .LE. 0 ) IP = ILEN(FRESEL%FILOUT)
      BATCH_FILE = FRESEL%FILOUT(1:IP)//'.bat'
      LUN_BATCH = GET_UNIT()
      OPEN ( UNIT=LUN_BATCH, FILE=BATCH_FILE, STATUS='UNKNOWN' ) 
!
      LUN_LOG = GET_UNIT()
      LOG_FILE = FRESEL%FILOUT(1:IP)//'.log'
      OPEN ( UNIT=LUN_LOG, FILE=LOG_FILE, STATUS='UNKNOWN' ) 
      WRITE ( LUN_LOG, '(A)' ) 'FRESEL log. Created at '//GET_CDATE()
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, 110 ) ' FRESEL%CONF_FILE           = ', &
     &                        FRESEL%CONF_FILE(1:I_LEN(FRESEL%CONF_FILE)),   ' '
           WRITE ( 6, 110 ) ' FRESEL%FILOUT              = ', &
     &                        FRESEL%FILOUT(1:I_LEN(FRESEL%FILOUT)),    ' '
           WRITE ( 6, 110 ) ' FRESEL%DATE_BEG            = ', FRESEL%DATE_BEG,   ' '
           WRITE ( 6, 110 ) ' FRESEL%DATE_REF            = ', FRESEL%DATE_REF,   ' '
           WRITE ( 6, 110 ) ' FRESEL%DATE_END            = ', FRESEL%DATE_END,   ' '
           WRITE ( 6, 110 ) ' FRESEL%NUTEXP              = ', FRESEL%NUTEXP,     ' '
           WRITE ( 6, 120 ) ' FRESEL%NUTAMP_MIN          = ', FRESEL%NUTAMP_MIN, 'rad'
           WRITE ( 6, 120 ) ' FRESEL%TIDAMP_MIN          = ', FRESEL%TIDAMP_MIN, 'm^2/s^2'
           WRITE ( 6, 120 ) ' FRESEL%TIDZON_MIN          = ', FRESEL%TIDZON_MIN, 'm^2/s^2'
           WRITE ( 6, 130 ) ' FRESEL%IGNORE_SIDELOBES    = ', FRESEL%IGNORE_SIDELOBES
           WRITE ( 6, 130 ) ' FRESEL%PM_EST_NUT          = ', FRESEL%PM_EST_NUT
           WRITE ( 6, 130 ) ' FRESEL%PM_EST_TID          = ', FRESEL%PM_EST_TID
           WRITE ( 6, 130 ) ' FRESEL%PM_EST_ADH          = ', FRESEL%PM_EST_ADH
           WRITE ( 6, 130 ) ' FRESEL%UT1_EST_NUT         = ', FRESEL%UT1_EST_NUT
           WRITE ( 6, 130 ) ' FRESEL%UT1_EST_TID         = ', FRESEL%UT1_EST_TID
           WRITE ( 6, 130 ) ' FRESEL%UT1_EST_ADH         = ', FRESEL%UT1_EST_ADH
           WRITE ( 6, 130 ) ' FRESEL%PRC_EST             = ', FRESEL%PRC_EST
           WRITE ( 6, 130 ) ' FRESEL%SEG_EOP             = ', FRESEL%SEG_EOP
!
           WRITE ( 6, 125 ) ' FRESEL%RFCN_FREQ_MIN       = ', FRESEL%RFCN_FREQ_MIN,  'rad/sec', &
     &                        PI2/(OM__EAR + FRESEL%RFCN_FREQ_MIN)/86400.0D0, 'days'
           WRITE ( 6, 125 ) ' FRESEL%RFCN_FREQ_MAX       = ', FRESEL%RFCN_FREQ_MAX,  'rad/sec', &
     &                        PI2/(OM__EAR + FRESEL%RFCN_FREQ_MAX)/86400.0D0, 'days'
           WRITE ( 6, 125 ) ' FRESEL%PFICN_FREQ_MIN      = ', FRESEL%PFICN_FREQ_MIN, 'rad/sec', &
     &                        PI2/(OM__EAR + FRESEL%PFICN_FREQ_MIN)/86400.0D0, 'days'
           WRITE ( 6, 125 ) ' FRESEL%PFICN_FREQ_MAX      = ', FRESEL%PFICN_FREQ_MAX, 'rad/sec', &
     &                        PI2/(OM__EAR + FRESEL%PFICN_FREQ_MAX)/86400.0D0, 'days'
           WRITE ( 6, 110 ) ' FRESEL%ADH_FILE            = ', FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE)),    ' '
           WRITE ( 6, 110 ) ' FRESEL%AMD_DIR             = ', FRESEL%AMD_DIR(1:I_LEN(FRESEL%AMD_DIR))
           WRITE ( 6, 140 ) ' FRESEL%APROPRI_SOLID_TIDES = ', FRESEL%APRIORI_SOLID_TIDES
           WRITE ( 6, 140 ) ' FRESEL%APROPRI_NUTATION    = ', FRESEL%APRIORI_NUTATION
           WRITE ( 6, 120 ) ' FRESEL%EOP_SEG             = ', FRESEL%EOP_SEG,     ' sec'
           WRITE ( 6, 120 ) ' FRESEL%SIGMA_TIE           = ', FRESEL%SIGMA_TIE,   ' rad'
           WRITE ( 6, 120 ) ' FRESEL%SIGMA_TRN           = ', FRESEL%SIGMA_TRN,   ' rad'
           WRITE ( 6, 120 ) ' FRESEL%SIGMA_RAT           = ', FRESEL%SIGMA_RAT,   ' rad/sec'
           WRITE ( 6, 120 ) ' FRESEL%SIGMA_HRM           = ', FRESEL%SIGMA_HRM,   ' rad'
!
 110       FORMAT ( 'fresel_heo:  ',A, A, A        )
 120       FORMAT ( 'fresel_heo:  ',A,1PD19.12,1X,A )
 122       FORMAT ( 'fresel_heo:  ',A,F6.3,1X,A )
 125       FORMAT ( 'fresel_heo:  ',A,1PD19.12,1X,A, ' Period: ', 0PF8.2, 1X, A )
 130       FORMAT ( 'fresel_heo:  ',A, L6 )
 140       FORMAT ( 'fresel_heo:  ',A, I6 )
      END IF
!
      WRITE ( LUN_LOG, 110 ) ' FRESEL%CONF_FILE           = ', &
     &                        FRESEL%CONF_FILE(1:I_LEN(FRESEL%CONF_FILE)),   ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%FILOUT              = ', &
     &                        FRESEL%FILOUT(1:I_LEN(FRESEL%FILOUT)),    ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%DATE_BEG            = ', FRESEL%DATE_BEG,   ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%DATE_REF            = ', FRESEL%DATE_REF,   ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%DATE_END            = ', FRESEL%DATE_END,   ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%NUTEXP              = ', FRESEL%NUTEXP,     ' '
      WRITE ( LUN_LOG, 120 ) ' FRESEL%NUTAMP_MIN          = ', FRESEL%NUTAMP_MIN, 'rad'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%TIDAMP_MIN          = ', FRESEL%TIDAMP_MIN, 'm^2/s^2'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%TIDZON_MIN          = ', FRESEL%TIDZON_MIN, 'm^2/s^2'
      WRITE ( LUN_LOG, 130 ) ' FRESEL%IGNORE_SIDELOBES    = ', FRESEL%IGNORE_SIDELOBES
      WRITE ( LUN_LOG, 130 ) ' FRESEL%PM_EST_NUT          = ', FRESEL%PM_EST_NUT
      WRITE ( LUN_LOG, 130 ) ' FRESEL%PM_EST_TID          = ', FRESEL%PM_EST_TID
      WRITE ( LUN_LOG, 130 ) ' FRESEL%PM_EST_ADH          = ', FRESEL%PM_EST_ADH
      WRITE ( LUN_LOG, 130 ) ' FRESEL%UT1_EST_NUT         = ', FRESEL%UT1_EST_NUT
      WRITE ( LUN_LOG, 130 ) ' FRESEL%UT1_EST_TID         = ', FRESEL%UT1_EST_TID
      WRITE ( LUN_LOG, 130 ) ' FRESEL%UT1_EST_ADH         = ', FRESEL%UT1_EST_ADH
      WRITE ( LUN_LOG, 130 ) ' FRESEL%PRC_EST             = ', FRESEL%PRC_EST
      WRITE ( LUN_LOG, 130 ) ' FRESEL%SEG_EOP             = ', FRESEL%SEG_EOP
!
      WRITE ( LUN_LOG, 120 ) ' FRESEL%RFCN_FREQ_MIN       = ', FRESEL%RFCN_FREQ_MIN,  'rad/sec'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%RFCN_FREQ_MAX       = ', FRESEL%RFCN_FREQ_MAX,  'rad/sec'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%PFICN_FREQ_MIN      = ', FRESEL%PFICN_FREQ_MIN, 'rad/sec'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%PFICN_FREQ_MAX      = ', FRESEL%PFICN_FREQ_MAX, 'rad/sec'
      WRITE ( LUN_LOG, 110 ) ' FRESEL%ADH_FILE            = ', FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE)),    ' '
      WRITE ( LUN_LOG, 110 ) ' FRESEL%AMD_DIR             = ', FRESEL%AMD_DIR(1:I_LEN(FRESEL%AMD_DIR))
      WRITE ( LUN_LOG, 140 ) ' FRESEL%APROPRI_SOLID_TIDES = ', FRESEL%APRIORI_SOLID_TIDES
      WRITE ( LUN_LOG, 140 ) ' FRESEL%APROPRI_NUTATION    = ', FRESEL%APRIORI_NUTATION
      WRITE ( LUN_LOG, 120 ) ' FRESEL%EOP_SEG             = ', FRESEL%EOP_SEG,     ' sec'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%SIGMA_TIE           = ', FRESEL%SIGMA_TIE,   ' rad'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%SIGMA_TRN           = ', FRESEL%SIGMA_TRN,   ' rad'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%SIGMA_RAT           = ', FRESEL%SIGMA_RAT,   ' rad/sec'
      WRITE ( LUN_LOG, 120 ) ' FRESEL%SIGMA_HRM           = ', FRESEL%SIGMA_HRM,   ' rad'
!    
      WRITE ( LUN_LOG, 122 ) ' NQ_SCALE: ', FRESEL%NF_SCALE, ' '
      WRITE ( LUN_LOG, 120 ) ' FREQ_MIN: ', PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG), ' rad/sec'
      WRITE ( LUN_LOG, 120 ) ' NQ_FREQ:  ', FRESEL%NF, ' rad/sec'
      WRITE ( LUN_LOG, 120 ) ' RFCN_per_min:  ', PI2/(FRESEL%RFCN_FREQ_MIN + OM__EAR)/86400.0D0, ' days'
      WRITE ( LUN_LOG, 120 ) ' RFCN_per_max:  ', PI2/(FRESEL%RFCN_FREQ_MAX + OM__EAR)/86400.0D0, ' days'
      WRITE ( LUN_LOG, 120 ) ' PRFCN_per_min: ', PI2/(FRESEL%PFICN_FREQ_MIN + OM__EAR)/86400.0D0, ' days'
      WRITE ( LUN_LOG, 120 ) ' PRFCN_per_max: ', PI2/(FRESEL%PFICN_FREQ_MAX + OM__EAR)/86400.0D0, ' days'
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  !#!  FRESEL_CONF  #!#
