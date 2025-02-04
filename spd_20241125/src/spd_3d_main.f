#include <mk5_preprocessor_directives.inc>
      PROGRAM    SPD_3D_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = SPD__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL SPD_3D_MAIN()
      END  PROGRAM  SPD_3D_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  SPD_3D_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program  SPD_3D_MAIN  computes slant path delay through the        *
! *   atmosphere for a set of stations using the file with               *
! *   meterorological data defined on a global 3D grid.                  *
! *                                                                      *
! * ###  25-NOV-2008  SPD_3D_MAIN  v3.0 (c) L. Petrov  15-SEP-2014  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL
      CHARACTER  FIL_CONF*128, DIR_MET*128, FIL_MET*128, FIL_STA*128, OUT_PREF*128
      INTEGER*4     M_MOD, M_INP
      PARAMETER  (  M_MOD = 128 ) 
      PARAMETER  (  M_INP = 128 ) 
      REAL*8     EDGE_SEC
      PARAMETER  ( EDGE_SEC = 0.0D0 ) 
      CHARACTER  MOD_TEXT(M_MOD)*128, INP_TEXT(M_INP)*128, C_FIL(SPD__M_FIL)*128, &
     &           DATE_BEG*32, DATE_END*32, STR*128, FILNAM*128, EXT*4, DATE_FIL*21
      PARAMETER  ( EXT = '.heb' )
      LOGICAL*1  LEX, FL_DIR
      INTEGER*4  J1, J2, J3, TYP_MET, TYP_DAT, IVRB, N_MOD, N_INP, IS, &
     &           L_FIL, LEV, MJD_BEG, MJD_END, MJD_FIL, IL, IE, IUER
      REAL*8     TIM_BEG, TIM_END, TIM_FIL, TIM_EPS
      PARAMETER  ( TIM_EPS = 100.0D0 )
      INTEGER*8  DIR_DESC(16), IP
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, GET_FILE_FROM_DIR 
      INTEGER*8, EXTERNAL ::   FUNC_OPENDIR, CLOSEDIR 
      CALL SET_SIGNAL_CTRLC ( 1 )
!
      DATE_BEG = '1979.01.01_00:00:00.0'
      DATE_END = '2049.12.31_00:00:00.0'
      IF ( IARGC() < 4 ) THEN
           IF ( IARGC() .GE. 1 ) THEN
                CALL GETARG ( 1, FIL_CONF )
                IF ( FIL_CONF == 'version'   .OR.  &
     &               FIL_CONF == '--version' .OR.  &
     &               FIL_CONF == '-v' ) THEN
!
                     WRITE ( 6, '(A)' ) SPD__VERSION
                     CALL EXIT ( 0 )
                 END IF
           END IF
           WRITE ( 6, '(A)' ) 'Usage: spd_3d {fil_conf} {fil_met} {out_pref} '// &
     &                        'verbosity [date_beg] [date_end]'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Parse arguments
!
           CALL GETARG ( 1, FIL_CONF )
           INQUIRE ( FILE=FIL_CONF, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5201, -2, 'SPD_3D_MAIN', 'Wrong '// &
     &              'first argument: configuration file '// &
     &              FIL_CONF(1:I_LEN(FIL_CONF))//' is not found' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 2, FIL_MET  )
           DIR_DESC(1) = FUNC_OPENDIR ( FIL_MET(1:I_LEN(FIL_MET))//CHAR(0) )
           IF ( DIR_DESC(1) .EQ. 0 ) THEN
                INQUIRE ( FILE=FIL_MET, EXIST=LEX ) 
                IF ( .NOT. LEX ) THEN
                      CALL ERR_LOG ( 5202, -2, 'SPD_3D_MAIN', 'Wrong '// &
     &                    'second argument: meteorological file '// &
     &                FIL_MET(1:I_LEN(FIL_MET))//' is not found' )
                      CALL EXIT ( 1 )
                END IF
                FL_DIR = .FALSE.
              ELSE 
                FL_DIR = .TRUE.
                IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
                DIR_MET = FIL_MET
           END IF
!
           CALL GETARG ( 3, OUT_PREF )
           CALL GETARG ( 4, STR    )
           CALL CHIN   ( STR, IVRB )
           IF ( IARGC() .GE. 5 ) CALL GETARG ( 5, DATE_BEG )
           IF ( IARGC() .GE. 6 ) CALL GETARG ( 6, DATE_END )
      END IF
!
      IUER = -1
      CALL SPD_INIT ( SPD, IUER )
!
      IF ( FL_DIR ) THEN
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5203, -2, 'SPD_3D_MAIN', 'Wrong begin date '//DATE_BEG )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5204, -2, 'SPD_3D_MAIN', 'Wrong end date '//DATE_END )
                CALL EXIT ( 1 )
           END IF
!
           L_FIL = 0
           LEV   = 0
           DO 410 J1=1,SPD__M_FIL
              IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_MET, FILNAM )
              IF ( IS .NE. 0 ) THEN
                   CALL ERR_LOG ( 5205, -2, 'SPD_3D_MAIN', 'Error in '// &
     &                 'reading input directory '//DIR_MET(1:I_LEN(DIR_MET))// &
     &                 '  '//FILNAM )
                   CALL EXIT ( 1 )
              END IF
              IF ( LEV == 0 ) GOTO 810 ! End of work
              IF ( INDEX ( FILNAM, EXT )     .LE. 0 ) GOTO 410
              IF ( INDEX ( FILNAM, '#' )     .GT. 0 ) GOTO 410
              IF ( INDEX ( FILNAM, '/d/d_' ) .LE. 0 ) GOTO 410
!
              IL = ILEN(FILNAM)
              IF ( IL < 20 ) GOTO 410
              IE = INDEX ( FILNAM, EXT ) 
!
              DATE_FIL = FILNAM(IE-13:IE-10)//'_'//FILNAM(IE-9:IE-8)//'_'// &
     &                   FILNAM(IE-7:IE-3)//':'//FILNAM(IE-2:IE-1)//':00.0'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL  ERR_LOG ( 5206, -2, 'SPD_3D_MAIN', 'Unexpected format of '// &
     &                  'file name '//FILNAM )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG - TIM_EPS ) GOTO 410
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END + TIM_EPS ) GOTO 410
!
              L_FIL = L_FIL + 1
              IF ( L_FIL > SPD__M_FIL )  THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( SPD__M_FIL, STR )
                   IUER = -1
                   CALL ERR_LOG ( 5207, -2, 'SPD_3D_MAIN', 'Too many files '// &
     &                 'in directory '//DIR_MET(1:I_LEN(DIR_MET))// &
     &                 ' -- more than '//STR )
                   CALL EXIT ( 1 )
              END IF
              C_FIL(L_FIL) = FILNAM 
 410       CONTINUE 
 810       CONTINUE 
           IF ( L_FIL == 0 ) THEN
                CALL ERR_LOG ( 5208, -2, 'SPD_3D_MAIN', 'No files with extension '// &
     &               EXT(1:I_LEN(EXT))//' were found in the input directory '// &
     &               FIL_MET )
                CALL EXIT ( 1 )
           END IF
           CALL SORT_FAST_CH ( L_FIL, C_FIL )
         ELSE 
           L_FIL = 1
           C_FIL(L_FIL) = FIL_MET
      END IF
!
! --- Read and parse configuration file
!
      IUER = -1
      CALL SPD_3D_CONF ( FIL_CONF, SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5209, -2, 'SPD_3D_MAIN', 'Failure in parsing '// &
     &         'input configuration file '//FIL_CONF )
           CALL EXIT ( 1 )
      END IF
!!  SPD%CONF%TEST_STR = 'timer' ! %%%%%%%%%%%%%%%%%%%%
!
! --- Read and parse station file
!
      IUER = -1
      CALL SPD_3D_INP_STA ( SPD, EDGE_SEC, HEB_GEOID_BSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5210, -2, 'SPD_3D_MAIN', 'Failure in loading '// &
     &         'the list of stations' )
           CALL EXIT ( 1 )
      END IF
!
      DO 420 J2=1,L_FIL
         IF ( IVRB .GE. 5 ) THEN
              WRITE ( 6, * ) '  SPD_3D_MAIN: Load    file '//TRIM(C_FIL(J2)) ; CALL FLUSH ( 6 )
         END IF
!
! ------ Read and parse meteorological data, extract geopotentical height,
! ------ air temperature and the specific humidity. Compute the 3D refractivity
! ------ field, surface atmospheric pressure and surface temperature
!
         IUER = -1
         CALL SPD_3D_LOAD ( C_FIL(J2), HEB_GEOID_BSPL, SPD, IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 5211, -2, 'SPD_3D_MAIN', 'Failure in '// &
     &            'an attempt to read meteorological file '//C_FIL(J2) )
              CALL EXIT ( 1 )
         END IF
         IF ( IVRB .GE. 5 ) THEN
              WRITE ( 6, * ) '  SPD_3D_MAIN  203' ; CALL FLUSH ( 6 )
         END IF
!
! ------ Compute slant path delay at the elevation/azimuth grid for each site
!
         IF ( IVRB .GE. 5 ) THEN
              WRITE ( 6, * ) '  SPD_3D_MAIN: Process file '//TRIM(C_FIL(J2)) ; CALL FLUSH ( 6 )
         END IF
         IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD ) THEN
              IUER = -1
              CALL SPD_2D_ZPD_WRI ( SPD, OUT_PREF, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5212, -2, 'SPD_3D_MAIN', 'Error in an attempt '// &
     &                 'to write the zenith path delay' )
                   CALL EXIT ( 1 )
              END IF
              CALL SPD_FREE ( SPD, 1 ) 
              CALL SPD_FREE ( SPD, 2 ) 
              CALL EXIT ( 0 )
            ELSE IF ( SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
              IUER = -1
              CALL SPD_2D_MZPD_WRI ( SPD, OUT_PREF, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5213, -2, 'SPD_3D_MAIN', 'Error in an attempt '// &
     &                 'to write the zenith path delay' )
                   CALL EXIT ( 1 )
              END IF
              CALL SPD_FREE ( SPD, 1 ) 
              CALL SPD_FREE ( SPD, 2 ) 
              CALL EXIT ( 0 )
         END IF
         IUER = -1
         CALL SPD_3D_COMP ( SPD, IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 5214, -2, 'SPD_3D_MAIN', 'Failure in '// &
      &           'an attempt to compute slant path delay' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Prepare explanation 
!
         N_MOD = 0
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'Slant path delays were computed using the algorithm of'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'L. Petrov (2015, in preparation) by intergration of differential'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'equations of wave propagation in the 3D continuous, heterogeneous '
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'atmosphere.'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = ' '
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'Atmosphere refractivity was computed using atmospheric'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'pressure, air temperature, and specific humidity provided'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &           'by numeric models of the amtosphere. '
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = ' '
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Parameters of computation:'
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = ' '
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Expression for refractivity:'// &
     &                     '       '//SPD%CONF%REFR_EXPR
         N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Variant of equation '// &
     &                     'integraion:    '//SPD%CONF%SPD_ALG
         IF ( SPD%CONF%N_FRQ == 0 ) THEN
              N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Atmospheric optical thickness '// &
     &                'and brightness temperature were not computed'
            ELSE 
              N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Atmospheric optical thickness '// &
     &                'and brightness temperature were computed'
              N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'using the atmospheric attenuation model '// &
     &                                             'according to recommendation ITU-R P.676-13'
              N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = '  https://www.itu.int/rec/R-REC-P.676/en'
              N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = 'Model for atmospheric optical thickness and '// &
     &                                             'brightness temperature: '// &
     &                                             SPD%CONF%SOB_ALG
         END IF
!
         N_INP = 0
         N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Name of the input atmospheric '// &
     &          'data: '
         N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//SPD%CONF%TITLE
         N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Institution that provided '// &
     &                    'atmospheric data: '
         N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//SPD%CONF%INSTITUTION
         N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Reference to the source of '// &
     &                    'atmospheric data: '
         N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//SPD%CONF%REFERENCE
!
! ------ Write down results
!
         IUER = -1
         CALL SPD_3D_WRITE ( SPD__WRITE_ASC, 1, SPD, OUT_PREF, &
     &                       N_MOD, MOD_TEXT, N_INP, INP_TEXT, IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 5215, -2, 'SPD_3D_MAIN', 'Failure in writing '// &
     &            'the output file with slant path delays' )
              CALL EXIT ( 1 )
         END IF
         CALL SPD_FREE ( SPD, 1 ) 
 420  CONTINUE 
      CALL SPD_FREE ( SPD, 2 ) 
      CALL EXIT ( 0 )
!
      END  SUBROUTINE  SPD_3D_MAIN !#!#
