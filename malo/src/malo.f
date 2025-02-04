      PROGRAM    MALO_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL MALO()
      END  PROGRAM  MALO_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  MALO()
! ************************************************************************
! *                                                                      *
! *   Program MALO is the main program of software package MALO          *
! *   (MAss LOading). It computes displacements caused by mass loading   *
! *   and their contribution to the geopotential.                        *
! *                                                                      *
! *  ### 17-OCT-2012      MALO    v2.18 (c)  L. Petrov  23-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'malo.i'
      INCLUDE   'agra.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE     ( AGRA__TYPE )  :: AGRA 
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LS, HEB_PRES
      CHARACTER  MALO_TASK*128, CONFIG_FILE*128, MALO_INPUT*128, &
     &           MALO_OUTPUT*128, DATE_BEG*32, DATE_END*32, DATE_FIL*32, &
     &           STR*128, FILNAM*128, C_FIL(MALO__FIL)*128, &
     &           MALO_INPUT_TYPE*9, FILOUT*128, PREF*128, OUT_EXT*128, &
     &           SGN_LAT*1, SHC_FMT*10, C_TXT(MALO__SHC_LTXT)*128, EXT*6, &
     &           DATE_STR*128, SUBDIR*128, FILHEB*128
      CHARACTER  COMPR*16, COMPR_COM*64, COM_STR*256, OMCT_VERS*2, &
     &           NUM_THR_STR*128, WISDOM_FILE*128
      LOGICAL*1  LEX
      REAL*8     TIM_BEG, TIM_END, TIM_FIL, TAI_DATA
      INTEGER*8  DIR_DESC(16), IP8
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'775' /
      INTEGER*4  IVRB, MJD_BEG, MJD_END, MJD_FIL, MJD_DATA, L_FIL, LEV, &
     &           IS, IP, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, NP, NS, IL, IDEV, &
     &           ISCL, IR, DEG_SPHE, DEG_LOAD, NUM_SETS, ID, L_TXT, DATA_OFFS, &
     &           IND_WC, IND_FRQ, NUM_THR, IUER
      INTEGER*8  FSH
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX, MALO_LOADING
      INTEGER*8, EXTERNAL :: MKDIR, OPENDIR, CLOSEDIR, SPHE_INIT_PLAN
!
      IF ( IARGC() < 6 ) THEN
           IF ( IARGC() .GE. 1 ) THEN
                CALL GETARG ( 1, MALO_TASK   )
                IF ( MALO_TASK == 'version'   .OR.  &
     &               MALO_TASK == '--version' .OR.  &
     &               MALO_TASK == '-v' ) THEN
!
                     WRITE ( 6, '(A)' ) MALO_VERSION
                     CALL EXIT ( 0 )
                 END IF
           END IF
           WRITE ( 6, '(A)' ) 'Usage: malo task config_file input_file/dir '// &
     &                        'date_beg date_end output_file/dir [verbosity_level] '// &
     &                        '[compr]' 
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, MALO_TASK   )
           CALL GETARG ( 2, CONFIG_FILE )
           CALL GETARG ( 3, MALO_INPUT  )
           CALL GETARG ( 4, DATE_BEG    )
           CALL GETARG ( 5, DATE_END    )
           CALL GETARG ( 6, MALO_OUTPUT )
!
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB < 0 .OR. IVRB > 99  ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6001, -2, 'MALO', 'Wrong verbosity '// &
     &                   'level argument: '//STR(1:I_LEN(STR))// &
     &                   ' an integer in range [0, 99] was expected' )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                IVRB = 1
           END IF
!
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, COMPR )
              ELSE 
                CALL CLRCH ( COMPR )
           END IF
      END IF
      IF ( MALO_TASK == 'sphe_create' ) THEN
           CONTINUE 
         ELSE IF ( MALO_TASK == 'sphe_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'sphe_love_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'sphe_love_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'grav_sphe_love_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'grav_sphe_love_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'grav_sphe_pres_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'grav_sphe_pres_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'vgep_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'vgep_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_d1_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_d1_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_cf_create' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_cf_update' ) THEN
           CONTINUE 
         ELSE  IF ( MALO_TASK == 'load_harpos' ) THEN
           CONTINUE 
         ELSE  
           CALL ERR_LOG ( 6002, -2, 'MALO', 'Task '//MALO_TASK(1:I_LEN(MALO_TASK))// &
     &         ' is not supported. List of supported tasks: sphe_create, '// &
     &         'sphe_update, sphe_love_create, sphe_love_update, '//   &
     &         'load_create, load_update, load_d1_create, load_d1_update, '// &
     &         'load_cf_create, load_cf_upload, load_harpos, grav_sphe_love_create, '// &
     &         'grav_sphe_love_update, grav_sphe_pres_create, grav_sphe_pres_update '// &
     &         'vgep_create vgep_update' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check compression method
!
      IF ( ILEN(COMPR) == 0 ) THEN
           CONTINUE 
         ELSE IF ( COMPR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR == 'lbzip2' ) THEN
           COMPR_COM= 'lbzip2 -9 -f '
         ELSE IF ( COMPR == 'lbzip2_p1' ) THEN
           COMPR_COM= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_1' ) THEN
           COMPR_COM= 'lbzip2 -1 -f '
         ELSE IF ( COMPR == 'lbzip2_1p1' ) THEN
           COMPR_COM= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_2p1' ) THEN
           COMPR_COM = 'lbzip2 -2 -n1 -f '
         ELSE 
           CALL ERR_LOG ( 6003, -2, 'MALO', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
! --- Check input name: is it an exisitig file or existing directory
!
      DIR_DESC(1) = OPENDIR ( MALO_INPUT(1:I_LEN(MALO_INPUT))//CHAR(0) )
      IF ( DIR_DESC(1) > 0 ) THEN
           MALO_INPUT_TYPE = 'directory' 
           IP8 = CLOSEDIR ( %VAL(DIR_DESC(1)) )
         ELSE 
           INQUIRE ( FILE=MALO_INPUT, EXIST=LEX ) 
           IF ( LEX ) THEN
                MALO_INPUT_TYPE = 'file'
              ELSE 
                CALL ERR_LOG ( 6004, -2, 'MALO', 'Wrong input argument: '// &
     &               MALO_INPUT(1:I_LEN(MALO_INPUT))// &
     &               ' -- it is neither file, nor directory' )
                CALL EXIT ( 1 )
           END IF 
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6005, -2, 'MALO', 'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6006, -2, 'MALO', 'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( MAL(2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6007, -2, 'MALO', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6008, -2, 'MALO', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CONFIG ( CONFIG_FILE, MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6009, -2, 'MALO', 'Failure in parsing MALO '// &
     &         'configuration file '//CONFIG_FILE )
           CALL EXIT ( 1 )
      END IF
      IF ( MALO_TASK == 'sphe_create' .OR. &
     &     MALO_TASK == 'sphe_update' .OR. &
     &     MALO_TASK == 'sphe_love_create' .OR. &
     &     MALO_TASK == 'sphe_love_update'      ) THEN
           EXT = '.heb'
         ELSE IF ( MALO_TASK =='load_create'    .OR. &
     &             MALO_TASK =='load_update'    .OR. &
     &             MALO_TASK =='load_d1_create' .OR. &
     &             MALO_TASK =='load_d1_update' .OR. &
     &             MALO_TASK =='load_cf_create' .OR. &
     &             MALO_TASK =='load_cf_update' .OR. &
     &             MALO_TASK =='load_harpos'         ) THEN
           EXT = '.shc'
         ELSE IF ( MALO_TASK == 'grav_sphe_love_create' .OR. &
     &             MALO_TASK == 'grav_sphe_love_update'      ) THEN
           EXT = '.asc'
         ELSE IF ( MALO_TASK == 'vgep_create' .OR. &
     &             MALO_TASK == 'vgep_update'      ) THEN
           EXT = '.shc'
      END IF
!      
      NP = 0
      NS = 0
      IF ( MALO_INPUT_TYPE == 'directory' ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'MALO:  Scan directory '//MALO_INPUT(1:I_LEN(MALO_INPUT))
           END IF 
           L_FIL = 0
           LEV   = 0
           DO 420 J2=1,16*MALO__FIL
              IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, MALO_INPUT, FILNAM )
              IF ( IS .NE. 0 ) THEN
                   CALL ERR_LOG ( 6010, -2, 'MALO', 'Error in '// &
     &                 'reading input directory '//MALO_INPUT(1:I_LEN(MALO_INPUT))// &
     &                 '  '//FILNAM )
                   CALL EXIT ( 1 )
              END IF
              IF ( LEV == 0 ) GOTO 820 ! End of work
              IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 420
              IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 420
!
              IF ( MALO_TASK == 'load_create'    .OR. &
     &             MALO_TASK == 'load_update'    .OR. &
     &             MALO_TASK == 'vgep_create'    .OR. &
     &             MALO_TASK == 'vgep_update'    .OR. &
     &             MALO_TASK == 'load_d1_create' .OR. &
     &             MALO_TASK == 'load_d1_update'       ) THEN
!
                   IL = ILEN(FILNAM)
                   IF ( IL < 20 ) GOTO 420
                   IF ( FILNAM(IL-3:IL) .NE. '.shc' ) GOTO 420
                   DATE_FIL = FILNAM(IL-16:IL-13)//'_'//FILNAM(IL-12:IL-11)//'_'// &
     &                        FILNAM(IL-10:IL-6)//':'//FILNAM(IL-5:IL-4)//':00.0'
                   IUER = -1
                   CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL  ERR_LOG ( 6011, -2, 'MALO', 'Unexpected format of '// &
     &                       'file name '//FILNAM )
                        CALL EXIT ( 1 )
                   END IF
!
                   IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) GOTO 420
                   IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) GOTO 420
                 ELSE IF ( MALO_TASK == 'grav_sphe_love_create' .OR. &
     &                     MALO_TASK == 'grav_sphe_love_update'      ) THEN
!
                   IL = ILEN(FILNAM)
                   IF ( IL < 25 ) GOTO 420
                   IF ( FILNAM(IL-3:IL) .NE. '.asc' ) GOTO 420
                   DATE_FIL = FILNAM(IL-18:IL-15)//'_'//FILNAM(IL-13:IL-12)//'_'// &
     &                        FILNAM(IL-10:IL-9)//'_00:00'
                   IUER = -1
                   CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL  ERR_LOG ( 6012, -2, 'MALO', 'Unexpected format of '// &
     &                       'file name '//FILNAM )
                        CALL EXIT ( 1 )
                   END IF
!
                   IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) GOTO 420
                   IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) GOTO 420
              END IF
!
              L_FIL = L_FIL + 1
              IF ( L_FIL > MALO__FIL )  THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( MALO__FIL, STR )
                   IUER = -1
                   CALL ERR_LOG ( 6013, -2, 'MALO', 'Too many files '// &
     &                 'in directory '//MALO_INPUT(1:I_LEN(MALO_INPUT))// &
     &                 ' -- more than '//STR )
                   CALL EXIT ( 1 )
              END IF
              C_FIL(L_FIL) = FILNAM 
 420       CONTINUE 
 820       CONTINUE 
           IF ( L_FIL == 0 ) THEN
                CALL ERR_LOG ( 6014, -2, 'MALO', 'No files with extension '// &
     &               EXT(1:I_LEN(EXT))//' were found in the input directory '// &
     &               MALO_INPUT )
                CALL EXIT ( 1 )
           END IF
           CALL SORT_CH ( L_FIL, C_FIL )
         ELSE 
           L_FIL = 1
           C_FIL(L_FIL) = MALO_INPUT
      END IF
!
      IF ( L_FIL == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6015, IUER, 'MALO', 'No appropriate data files '// &
     &         'were found in the input directory '//MALO_INPUT )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) L_FIL
 110       FORMAT ( I6, ' data input files have been found' )
      END IF 
!
! --- Check how manuy threads shoujld we use
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
      IF ( ILEN(NUM_THR_STR) > 0 ) THEN
!
! --------- Well, OMP_NUM_THREADS was setup. Read it.
!
            CALL CHIN ( NUM_THR_STR, NUM_THR )
            IF ( NUM_THR < 1 ) NUM_THR = 1
         ELSE 
            NUM_THR = 1
      END IF
!
! --- Build wisdom file name
!
      CALL CLRCH ( WISDOM_FILE )
      CALL INCH ( NUM_THR, WISDOM_FILE )
      WISDOM_FILE = MALO_SHARE//'/malo_fftw_plan_'// &
     &              WISDOM_FILE(1:I_LEN(WISDOM_FILE))//'thr.wis'
      INQUIRE ( FILE=WISDOM_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, '(A)' ) 'WARNING: wisdom file '// &
     &                         WISDOM_FILE(1:I_LEN(WISDOM_FILE))// &
     &                         ' was not found. Nevertheless, continue.'
           CALL CLRCH ( WISDOM_FILE )
      END IF
!
! --- Initialization for spherical harmonics package
!
      IUER = -1
      FSH = SPHE_INIT_PLAN ( WISDOM_FILE, 1, 2.0D0, NUM_THR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6016, -2, 'MALO', 'Error in an attempt to '// &
     &         'initialize FSH object for spherical harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
      NP = 0
      NS = 0
      IR = 0
!
      IF ( MALO_TASK == 'sphe_love_create'      .OR. &
     &     MALO_TASK == 'sphe_love_update'      .OR. & 
     &     MALO_TASK == 'load_d1_create'        .OR. &
     &     MALO_TASK == 'load_d1_update'        .OR. & 
     &     MALO_TASK == 'load_cf_create'        .OR. &
     &     MALO_TASK == 'load_cf_update'        .OR. & 
     &     MALO_TASK == 'vgep_create'           .OR. &
     &     MALO_TASK == 'vgep_update'           .OR. & 
     &     MALO_TASK == 'grav_sphe_pres_create' .OR. & 
     &     MALO_TASK == 'grav_sphe_pres_update' .OR. &
     &     MALO_TASK == 'grav_sphe_love_create' .OR. & 
     &     MALO_TASK == 'grav_sphe_love_update'      ) THEN
!
           IUER = -1
           CALL READ_LOVE ( MAL(1), IUER ) 
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6017, -2, 'MALO', 'Error in '// &
     &              'an attempt to read a file with Love numbers '// &
     &               MAL(1)%CONF%LOVE_FILE )
                CALL EXIT ( 1 )
           END IF   
      END IF   
!
      IF ( MALO_TASK == 'sphe_create'      .OR. &
     &     MALO_TASK == 'sphe_update'      .OR. &
     &     MALO_TASK == 'sphe_love_create' .OR. &
     &     MALO_TASK == 'sphe_love_update'      ) THEN
!
           IL = ILEN(MAL(1)%CONF%FINAM_LS_MASK)
           IF ( IL < 4 ) IL = 4
           IF ( MAL(1)%CONF%FINAM_LS_MASK .EQ. 'NONE' ) THEN
                HEB_LS%DIMS = 0
              ELSE IF ( MAL(1)%CONF%FINAM_LS_MASK(IL-3:IL) == '.heb' ) THEN
                IUER = -1
                CALL READ_HEB ( MAL(1)%CONF%FINAM_LS_MASK, HEB_LS, IUER ) 
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6018, -2, 'MALO', 'Error in '// &
          &              'an attempt to read nc-file with land-sea '// &
          &              'mask '//MAL(1)%CONF%FINAM_LS_MASK )
                     CALL EXIT ( 1 )
                END IF   
           END IF
!
! -------- Read the surface pressure model
!
           IF ( MAL(1)%CONF%FINAM_MODEL .NE. 'NONE' ) THEN
!
! ------------- NB: since the model is very big, we read only header
!
                IUER = -1
                CALL READ_HEB_HEADER ( MAL(1)%CONF%FINAM_MODEL, HEB_MOD, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6019, -2, 'MALO', 'Error in '// &
     &                   'an attempt to read heb-file with the model of surface '// &
     &                   'atmospheric pressure '//MAL(1)%CONF%FINAM_MODEL )
                     CALL EXIT ( 1 )
                END IF   
                MAL(2)%NLON = HEB_MOD%DIMS(1)
                MAL(2)%NLAT = HEB_MOD%DIMS(2)
              ELSE 
                MAL(2)%NLON = HEB_LS%DIMS(1)
                MAL(2)%NLAT = HEB_LS%DIMS(2)
           END IF   
           IF ( DABS ( DLOG(MAL(2)%NLAT-1.D0)/DLOG(2.0D0) - IDINT(DLOG(MAL(2)%NLAT-1.D0)/DLOG(2.0D0)) ) < 1.0D-5 ) THEN
                MAL(2)%NLAT = MAL(2)%NLAT - 1
           END IF
!
           IUER = -1
           DO 430 J3=1,L_FIL
              IP = INDEX ( C_FIL(J3), '.heb' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6020, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J3) )
                   CALL EXIT ( 1 )
              END IF
!
              DATE_FIL = C_FIL(J3)(IP-13:IP-10)//'.'//C_FIL(J3)(IP-9:IP-8)//'.'// &
     &                   C_FIL(J3)(IP-7:IP-3)//':'//C_FIL(J3)(IP-2:IP-1)//'00.0'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER  )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6021, -2, 'MALO', 'Unexpected format of '// &
     &                           'file name '//C_FIL(J3) )
                   CALL EXIT ( 1 )
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 430
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 430
              END IF
              IF ( MALO_TASK == 'sphe_update'      .OR. &
     &             MALO_TASK == 'sphe_love_update'      ) THEN
!
! ---------------- Check whether the name has asterisk
!
                   IND_WC = INDEX ( MALO_OUTPUT, '*' ) 
                   IF ( IND_WC > 1 ) THEN
                        PREF    = MALO_OUTPUT(1:IND_WC-1)
                        OUT_EXT =  MALO_OUTPUT(IND_WC+1:)
                      ELSE 
                        CALL CLRCH ( PREF )
                        CALL CLRCH ( OUT_EXT  )
                        IP = LINDEX ( MALO_OUTPUT, '.' ) 
                        IF ( IP > 1 ) THEN
                             OUT_EXT = MALO_OUTPUT(IP:)
                        END IF
                    END IF
                    FILOUT = PREF(1:I_LEN(PREF))//C_FIL(J3)(IP-13:IP-1)// &
     &                       OUT_EXT
                    INQUIRE ( FILE=FILOUT, EXIST=LEX )
                    IF ( LEX ) THEN
                         IF ( IVRB .GE. 4 ) WRITE ( 6, * ) 'File '// &
     &                        FILOUT(1:I_LEN(FILOUT))//' already exists'
                         NS = NS + 1
                         GOTO 430
                    END IF
              END IF
!
              IUER = -1
              CALL MALO_SPHE ( C_FIL(J3), MAL(1), HEB_LS, HEB_MOD, MALO_TASK, &
     &                         %VAL(FSH), MALO_OUTPUT, IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6022, -2, 'MALO', 'Error during '// &
     &                 'computation of spherical harmonics of the '// &
     &                 'surface pressure field extracted from file '//C_FIL(J3) )
                   CALL EXIT ( 1 )
              END IF
!
              DEG_SPHE = MAL(1)%NLAT/2 - 1
              NP = NP + 1
 430       CONTINUE 
        ELSE IF ( MALO_TASK == 'load_create'    .OR. &
     &            MALO_TASK == 'load_update'    .OR. &
     &            MALO_TASK == 'load_d1_create' .OR. &
     &            MALO_TASK == 'load_d1_update' .OR. &
     &            MALO_TASK == 'load_cf_create' .OR. &
     &            MALO_TASK == 'load_cf_update'      ) THEN
           IF ( MAL(1)%CONF%STATION_FINAM .NE. 'NONE' ) THEN
                IUER = -1
                CALL MALO_INP_STA ( MAL(1), MAL(1)%CONF%STATION_FINAM, &
     &                              0.0D0, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL  ERR_LOG ( 6023, -2, 'MALO', 'Failure in '// &
     &                    'an attempt to load station file '// &
     &                     MAL(1)%CONF%STATION_FINAM )
                      CALL EXIT ( 1 )
                END IF 
           END IF
!
           DO 440 J4=1,L_FIL
              IP = INDEX ( C_FIL(J4), '.shc' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6024, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
!
              DATE_FIL = C_FIL(J4)(IP-13:IP-10)//'.'//C_FIL(J4)(IP-9:IP-8)//'.'// &
     &                   C_FIL(J4)(IP-7:IP-3)//':'//C_FIL(J4)(IP-2:IP-1)//'00.0'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER  )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6025, -2, 'MALO', 'Unexpected format of '// &
     &                           'file name '//C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 440
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 440
              END IF
!
              IUER = -1
              CALL SHC_INQ ( C_FIL(J4), DEG_SPHE, SHC_FMT, MALO__SHC_LTXT, &
     &                       L_TXT, C_TXT, NUM_SETS, MJD_DATA, TAI_DATA, &
     &                       DATA_OFFS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6026, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read the header of the spherical haramonic file '// &
     &                  C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( IVRB .GE. 2 ) THEN
                   STR = MJDSEC_TO_DATE ( MJD_DATA, TAI_DATA, IUER ) 
                   IF ( IVRB == 2 ) THEN
                        STR(32:32) = CHAR(13)
                      ELSE 
                        STR(32:32) = ' '
                   END IF 
                   WRITE ( 6, 120 ) J4, L_FIL, STR(1:21), STR(32:32)
 120               FORMAT ( '  Processing data file ', I6, &
     &                      ' ( ', I6, ' ) for epoch ', A,A$ )
                   IF ( IVRB > 2 ) WRITE ( 6, '(A)' ) ""
                   CALL FLUSH ( 6 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH ) ) THEN
                   DEALLOCATE ( MAL(1)%SPH )
              END IF
!
              ALLOCATE ( MAL(1)%SPH(2,0:DEG_SPHE,0:DEG_SPHE,2,1), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*2*(DEG_SPHE+1)**2*2, STR )
                   CALL ERR_LOG ( 6027, -2, 'MALO', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for spherical harmonic '// &
     &                 'coefficients' )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__ALLO
!
! ----------- Read spherical harmonics from the input file
!
              IUER = -1
              CALL SHC_READ ( C_FIL(J4), DEG_SPHE, SHC_FMT, NUM_SETS, DATA_OFFS, &
     &                        MALO__TRA, MAL(1)%SPH, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6028, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read spherical haramonics from file '//C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__LOAD
!
              MAL(1)%MJD_BEG = MJD_DATA
              MAL(1)%TAI_BEG = TAI_DATA
              MAL(1)%NTIM    = 1
              ALLOCATE ( MAL(1)%MJD_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6029, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array MJD_ARR' )
                   CALL EXIT ( 1 )
              END IF
              ALLOCATE ( MAL(1)%TAI_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6030, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array TAI_ARR' )
                   CALL EXIT ( 1 )
              END IF
!
              MAL(1)%MJD_ARR(1) = MJD_DATA
              MAL(1)%TAI_ARR(1) = TAI_DATA
!
              IUER = -1
              IR = MALO_LOADING ( MALO_TASK, MAL(1), %VAL(FSH), DEG_SPHE, &
     &                            MALO_OUTPUT, COMPR_COM, IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6031, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to compute loading using spherical harmonics from '// &
     &                 ' input file '//C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH     ) ) DEALLOCATE ( MAL(1)%SPH     )
              IF ( ASSOCIATED ( MAL(1)%MJD_ARR ) ) DEALLOCATE ( MAL(1)%MJD_ARR )
              IF ( ASSOCIATED ( MAL(1)%TAI_ARR ) ) DEALLOCATE ( MAL(1)%TAI_ARR )
              IF ( IR == 0 ) NS = NS + 1
              NP = NP + IR
 440       CONTINUE 
           CALL MALO_FREE ( MAL(1), IUER )
        ELSE IF ( MALO_TASK == 'load_harpos' ) THEN
           IUER = -1
           IF ( MAL(1)%CONF%STATION_FINAM .NE. 'NONE' ) THEN
                IUER = -1
                CALL MALO_INP_STA ( MAL(1), MAL(1)%CONF%STATION_FINAM, &
     &                              0.0D0, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL  ERR_LOG ( 6032, -2, 'MALO', 'Failure in '// &
     &                    'an attempt to load station file '// &
     &                     MAL(1)%CONF%STATION_FINAM )
                     CALL EXIT ( 1 )
                END IF 
           END IF 
!
           DO 450 J5=1,L_FIL
              IP = INDEX ( C_FIL(J5), '.shc' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6033, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J5) )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL SHC_INQ ( C_FIL(J5), DEG_SPHE, SHC_FMT, MALO__SHC_LTXT, &
     &                       L_TXT, C_TXT, NUM_SETS, MJD_DATA, TAI_DATA, &
     &                       DATA_OFFS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6034, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read the header of the spherical haramonic file '// &
     &                  C_FIL(J5) )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( IVRB .GE. 2 ) THEN
                   WRITE ( 6, 130 ) J5, L_FIL, CHAR(13)
 130               FORMAT ( '  Processing data file ', I6, &
     &                      ' ( ', I6, ' ) ',A$ ) 
                   IF ( IVRB > 2 ) WRITE ( 6, '(A)' ) ""
                   CALL FLUSH ( 6 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH ) ) THEN
                   DEALLOCATE ( MAL(1)%SPH )
              END IF
!
              ALLOCATE ( MAL(1)%SPH(2,0:DEG_SPHE,0:DEG_SPHE,2,1), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*2*(DEG_SPHE+1)**2*2, STR )
                   CALL ERR_LOG ( 6035, -2, 'MALO', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for spherical harmonic '// &
     &                 'coefficients' )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__ALLO
!
! ----------- Read spherical harmonics from the input file
!
              IUER = -1
              CALL SHC_READ ( C_FIL(J5), DEG_SPHE, SHC_FMT, NUM_SETS, DATA_OFFS, &
     &                        MALO__TRA, MAL(1)%SPH, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6036, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read spherical haramonics from file '//C_FIL(J4) )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__LOAD
!
              MAL(1)%MJD_BEG = MJD_DATA
              MAL(1)%TAI_BEG = TAI_DATA
              MAL(1)%NTIM    = 1
              ALLOCATE ( MAL(1)%MJD_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6037, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array MJD_ARR' )
                   CALL EXIT ( 1 )
              END IF
              ALLOCATE ( MAL(1)%TAI_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6038, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array TAI_ARR' )
                   CALL EXIT ( 1 )
              END IF
!
              MAL(1)%MJD_ARR(1) = J2000__MJD
!
! ----------- A dirty trick: we use TAI_ARR(1) in order to pass the index of 
! ----------- the harmomic component to MALO_LOADING.
!
              IL = ILEN(C_FIL(J5))
              CALL CHIN ( C_FIL(J5)(IL-5:IL-4), IND_FRQ )
              MAL(1)%TAI_ARR(1) = IND_FRQ
!
              IUER = -1
              IR = MALO_LOADING ( MALO_TASK, MAL(1), %VAL(FSH), DEG_SPHE, &
     &                            MALO_OUTPUT, COMPR_COM, IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6039, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to compute loading using spherical harmonics from '// &
     &                 ' input file '//C_FIL(J5) )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH     ) ) DEALLOCATE ( MAL(1)%SPH     )
              IF ( ASSOCIATED ( MAL(1)%MJD_ARR ) ) DEALLOCATE ( MAL(1)%MJD_ARR )
              IF ( ASSOCIATED ( MAL(1)%TAI_ARR ) ) DEALLOCATE ( MAL(1)%TAI_ARR )
              IF ( IR == 0 ) NS = NS + 1
              NP = NP + IR
 450       CONTINUE 
        ELSE IF ( MALO_TASK == 'grav_sphe_pres_create' .OR. & 
     &            MALO_TASK == 'grav_sphe_pres_update'      ) THEN
           DO 460 J6=1,L_FIL
              IP = INDEX ( C_FIL(J6), '.asc' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6040, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J6) )
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Extract the date from the file name
!
              IL = ILEN(C_FIL(J6))
              DATE_FIL = C_FIL(J6)(IL-18:IL-15)//'_'//C_FIL(J6)(IL-13:IL-12)//'_'// &
     &                   C_FIL(J6)(IL-10:IL-9)//'_00:00'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
!
! ----------- Check the dates, whether we have to process this file
!
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 460
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 460
              END IF
!
              IUER = -1
              CALL MALO_READ_AOD1B ( 1, C_FIL(J6), MAL(1), IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6041, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read the header of the spherical haramonic file '// &
     &                  C_FIL(J6) )
                   CALL EXIT ( 1 )
              END IF
              IL = ILEN(C_FIL(J6))
              OMCT_VERS = C_FIL(J6)(IL-5:IL-4)
!
              IUER = -1
              CALL MALO_AOD1B_SPHE ( MAL(1), IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6042, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to compute raw bottom pressure field from its '// &
     &                 'spherical harmonics transform' )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              IF ( ILEN(MAL(1)%CONF%UPGRID_LS_MASK) > 0 ) THEN
                   CALL READ_HEB ( MAL(1)%CONF%UPGRID_LS_MASK, HEB_LS, IUER ) 
                 ELSE
                   CALL READ_HEB ( MAL(1)%CONF%FINAM_LS_MASK, HEB_LS, IUER ) 
              END IF
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6043, -2, 'MALO', 'Error in '// &
          &             'an attempt to read nc-file with land-sea '// &
          &             'mask '//MAL(1)%CONF%FINAM_LS_MASK )
                   CALL EXIT ( 1 )
              END IF   
!
              MAL(1)%NLON = HEB_LS%DIMS(1)
              MAL(1)%NLAT = HEB_LS%DIMS(2)
              ALLOCATE ( MAL(1)%SPR(MAL(1)%NLON,MAL(1)%NLAT,MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6044, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory for array MAL(1)%SPR' )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL OMCT_TO_SPR ( MAL(1), HEB_LS, IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6045, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to refine bottom pressure field extracted from its '// &
     &                 'spherical harnonics transform' )
                   CALL EXIT ( 1 )
              END IF
!
              DO 470 J7=1,MAL(1)%NTIM 
                 DATE_STR = MJDSEC_TO_DATE ( MAL(1)%MJD_ARR(J7), MAL(1)%TAI_ARR(J7), -2 )
                 IF (  OMCT_VERS == '05' ) THEN
                       IF ( MALO_OUTPUT(1:I_LEN(MALO_OUTPUT)) == '/imls/oper_temp' ) THEN
                            FILHEB = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'// &
     &                               'omct'//OMCT_VERS//'/'//'omct'//OMCT_VERS//'_'// &
     &                               DATE_STR(1:4)//DATE_STR(6:7)//DATE_STR(9:10)//'_'// &
     &                               DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
                          ELSE
                            FILHEB = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'// &
     &                               DATE_STR(1:4)//'/'//'omct'//OMCT_VERS//'/'//'omct'//OMCT_VERS//'_'// &
     &                               DATE_STR(1:4)//DATE_STR(6:7)//DATE_STR(9:10)//'_'// &
     &                               DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
                       END IF
                     ELSE 
                       IF ( MALO_OUTPUT(1:I_LEN(MALO_OUTPUT)) == '/imls/oper_temp' ) THEN
                            FILHEB = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'// &
     &                               'mpiom'//OMCT_VERS//'_'// &
     &                               DATE_STR(1:4)//DATE_STR(6:7)//DATE_STR(9:10)//'_'// &
     &                               DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
                           ELSE
                            FILHEB = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'// &
     &                               DATE_STR(1:4)//'/mpiom'//OMCT_VERS//'_'// &
     &                               DATE_STR(1:4)//DATE_STR(6:7)//DATE_STR(9:10)//'_'// &
     &                               DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
                       END IF
                 END IF
!
                 HEB_PRES%DIMS(1) = MAL(1)%NLON
                 HEB_PRES%DIMS(2) = MAL(1)%NLAT
                 HEB_PRES%DIMS(3) = 1
                 HEB_PRES%DIMS(4) = 1
                 HEB_PRES%SDS_NAME  = 'Bottom pressure from AODR1B oba'
                 HEB_PRES%PROD_NAME = 'Bottom pressure from AODR1B oba'
                 HEB_PRES%TITLE     = 'Bottom pressure from AODR1B oba'
                 HEB_PRES%UNITS     = 'Pa'
                 HEB_PRES%HISTORY   = ' '
                 HEB_PRES%INSTITUTION = 'GFZ Potstam'
                 HEB_PRES%SOURCE    = ' '
                 HEB_PRES%REFERENCES  = ' '
                 HEB_PRES%PROD_DATE_TIME  = ' '
                 HEB_PRES%VERSION_ID  = ' '
                 HEB_PRES%VALID_RANGE(1) = -50000.0D0
                 HEB_PRES%VALID_RANGE(2) =  50000.0D0
                 HEB_PRES%FILL_VALUE   = 0.0
                 HEB_PRES%OFFSET       = 0.0
                 HEB_PRES%SCALE_FACTOR = 1.0
                 HEB_PRES%DATA_FORMAT  = HEB__R4
                 HEB_PRES%DATA_COMPRESSION = HEB__NONE
                 HEB_PRES%DATA_TRANSFORM = HEB__NONE
!
                 HEB_PRES%MJD = MAL(1)%MJD_ARR(J7)
                 HEB_PRES%UTC = MAL(1)%TAI_ARR(J7)
                 HEB_PRES%TAI = MAL(1)%TAI_ARR(J7)
!
                 HEB_PRES%FILL_VALUE = 1.0E14
                 HEB_PRES%MIN_VALUE = MINVAL(MAL(1)%SPR(1:MAL(1)%NLON,1:MAL(1)%NLAT,J7))
                 HEB_PRES%MAX_VALUE = MAXVAL(MAL(1)%SPR(1:MAL(1)%NLON,1:MAL(1)%NLAT,J7))
!
                 IF ( MALO_OUTPUT(1:I_LEN(MALO_OUTPUT)) .NE. '/imls/oper_temp' ) THEN
                      SUBDIR = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'// &
     &                         DATE_STR(1:4)
                      DIR_DESC(1) = OPENDIR ( SUBDIR(1:I_LEN(SUBDIR))//CHAR(0) )
                      IF ( DIR_DESC(1) == 0 )    THEN
                           IS = MKDIR ( SUBDIR(1:I_LEN(SUBDIR))//CHAR(0), %VAL(MODE_I2) )
                           IF ( IS .NE. 0 ) THEN
                                CALL CLRCH  ( STR )
                                CALL GERROR ( STR )
                                IUER = -1
                                CALL ERR_LOG ( 6046, -2, 'MALO', &
     &                              'Failure to create directory '// &
     &                               SUBDIR(1:I_LEN(SUBDIR))//' -- '//STR )
                                CALL EXIT ( 1 )
                           END IF
!
                           IF (  OMCT_VERS == '05' ) THEN
                                 SUBDIR = SUBDIR(1:I_LEN(SUBDIR))//'/omct'//OMCT_VERS
                                 IS = MKDIR ( SUBDIR(1:I_LEN(SUBDIR))//CHAR(0), %VAL(MODE_I2) )
                                 IF ( IS .NE. 0 ) THEN
                                      CALL CLRCH  ( STR )
                                      CALL GERROR ( STR )
                                      IUER = -1
                                      CALL ERR_LOG ( 6047, -2, 'MALO', 'Failure to '// &
     &                                    'create directory '//SUBDIR(1:I_LEN(SUBDIR))// &
     &                                    ' -- '//STR )
                                      CALL EXIT ( 1 )
                                 END IF
                           END IF
                        ELSE 
                           IP8 = CLOSEDIR ( %VAL(DIR_DESC(1)) )
                      END IF
                 END IF
!
                 IUER = -1
                 CALL WRITE_HEB ( HEB_PRES, MAL(1)%SPR(1,1,J7), FILHEB, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6048, -2, 'MALO', 'Failure '// &
     &                    'in an attempt to write into output file '//FILHEB )
                      CALL EXIT ( 1 )
                 END IF
!
                 IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ------------------- Now compress the output file 
!
                      COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                         FILHEB(1:I_LEN(FILHEB))
                      CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
                 END IF
                 IF ( IVRB .GE. 1 ) THEN
                      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
                           WRITE ( 6, '(A)' ) 'Wrote file '//FILHEB(1:I_LEN(FILHEB))//'.bz2'
                         ELSE 
                           WRITE ( 6, '(A)' ) 'Wrote file '//FILHEB(1:I_LEN(FILHEB))
                      END IF
                 END IF
                 NP = NP + 1
 470          CONTINUE 
 460       CONTINUE 
        ELSE IF ( MALO_TASK == 'grav_sphe_love_create' .OR. & 
     &            MALO_TASK == 'grav_sphe_love_update'      ) THEN
           DO 480 J8=1,L_FIL
              IP = INDEX ( C_FIL(J8), '.asc' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6049, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J8) )
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Extract the date from the file name
!
              IL = ILEN(C_FIL(J8))
              DATE_FIL = C_FIL(J8)(IL-18:IL-15)//'_'//C_FIL(J8)(IL-13:IL-12)//'_'// &
     &                   C_FIL(J8)(IL-10:IL-9)//'_00:00'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
!
! ----------- Check the dates, whether we have to process this file
!
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 480
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 480
              END IF
!
              IUER = -1
              CALL MALO_READ_AOD1B ( 1, C_FIL(J8), MAL(1), IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6050, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read the header of the spherical haramonic file '// &
     &                  C_FIL(J8) )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL MALO_AOD1B_TO_LOVE ( MAL(1), IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6051, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to extract process harmonics from file '//C_FIL(J8) )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL MALO_WRITE_AOD1B_LOVE ( MAL(1), MALO_OUTPUT, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6052, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to write harmonics with Love numbers extracted from '// &
     &                 'file '//C_FIL(J8) )
                   CALL EXIT ( 1 )
              END IF
              NP = NP + 1
 480       CONTINUE 
        ELSE IF ( MALO_TASK == 'vgep_create' .OR. &
     &            MALO_TASK == 'vgep_update'      ) THEN
!
           DO 490 J9=1,L_FIL
              IP = INDEX ( C_FIL(J9), '.shc' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6053, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J9) )
                   CALL EXIT ( 1 )
              END IF
!
              DATE_FIL = C_FIL(J9)(IP-13:IP-10)//'.'//C_FIL(J9)(IP-9:IP-8)//'.'// &
     &                   C_FIL(J9)(IP-7:IP-3)//':'//C_FIL(J9)(IP-2:IP-1)//'00.0'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER  )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6054, -2, 'MALO', 'Unexpected format of '// &
     &                           'file name '//C_FIL(J9) )
                   CALL EXIT ( 1 )
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 490
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 490
              END IF
!
! ----------- Check whether the name has asterisk
!
              IND_WC = INDEX ( MALO_OUTPUT, '*' ) 
              IF ( IND_WC > 1 ) THEN
                   PREF    = MALO_OUTPUT(1:IND_WC-1)
                   OUT_EXT =  MALO_OUTPUT(IND_WC+1:)
                 ELSE 
                   CALL CLRCH ( PREF )
                   CALL CLRCH ( OUT_EXT  )
                   IP = LINDEX ( MALO_OUTPUT, '.' ) 
                   IF ( IP > 1 ) THEN
                        OUT_EXT = MALO_OUTPUT(IP:)
                   END IF
              END IF
!
              FILOUT = PREF(1:I_LEN(PREF))//C_FIL(J9)(IP-13:IP-1)// &
     &                 OUT_EXT
              INQUIRE ( FILE=FILOUT, EXIST=LEX )
              IF ( LEX ) THEN
                   IF ( MALO_TASK == 'vgep_update' ) GOTO 490
              END IF
!
              IUER = -1
              CALL SHC_INQ ( C_FIL(J9), DEG_SPHE, SHC_FMT, MALO__SHC_LTXT, &
     &                       L_TXT, C_TXT, NUM_SETS, MJD_DATA, TAI_DATA, &
     &                       DATA_OFFS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6055, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read the header of the spherical haramonic file '// &
     &                  C_FIL(J9) )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( IVRB .GE. 2 ) THEN
                   STR = MJDSEC_TO_DATE ( MJD_DATA, TAI_DATA, IUER ) 
                   IF ( IVRB == 2 ) THEN
                        STR(32:32) = CHAR(13)
                      ELSE 
                        STR(32:32) = ' '
                   END IF 
                   WRITE ( 6, 120 ) J9, L_FIL, STR(1:21), STR(32:32)
                   IF ( IVRB > 2 ) WRITE ( 6, '(A)' ) ""
                   CALL FLUSH ( 6 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH ) ) THEN
                   DEALLOCATE ( MAL(1)%SPH )
              END IF
!
              ALLOCATE ( MAL(1)%SPH(2,0:DEG_SPHE,0:DEG_SPHE,2,1), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*2*(DEG_SPHE+1)**2*2, STR )
                   CALL ERR_LOG ( 6056, -2, 'MALO', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for spherical harmonic '// &
     &                 'coefficients' )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__ALLO
!
! ----------- Read spherical harmonics from the input file
!
              IUER = -1
              CALL SHC_READ ( C_FIL(J9), DEG_SPHE, SHC_FMT, NUM_SETS, DATA_OFFS, &
     &                        MALO__TRA, MAL(1)%SPH, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6057, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to read spherical haramonics from file '//C_FIL(J9) )
                   CALL EXIT ( 1 )
              END IF
              MAL(1)%SPH_STATUS = MALO__LOAD
!
              MAL(1)%MJD_BEG = MJD_DATA
              MAL(1)%TAI_BEG = TAI_DATA
              MAL(1)%NTIM    = 1
              ALLOCATE ( MAL(1)%MJD_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6058, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array MJD_ARR' )
                   CALL EXIT ( 1 )
              END IF
              ALLOCATE ( MAL(1)%TAI_ARR(MAL(1)%NTIM), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6059, -2, 'MALO', 'Failure to allocate '// &
     &                 'dynamic memory for array TAI_ARR' )
                   CALL EXIT ( 1 )
              END IF
!
              MAL(1)%MJD_ARR(1) = MJD_DATA
              MAL(1)%TAI_ARR(1) = TAI_DATA
!
              IUER = -1
              CALL MALO_SHC_VGEP ( MAL(1), AGRA, DEG_SPHE, MALO_OUTPUT, &
     &                             IVRB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6060, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to compute unscaled spherical harminocs from the '// &
     &                 'input file '//C_FIL(J9) )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL AGRA_WRITE ( MALO__LABEL, MAL(1)%CONF%AGRA_FINAM_FMT, &
     &                          MAL(1)%CONF%AGRA_FINAM_DESC, &
     &                          MAL(1)%CONF%LOA_FINAM_DESCR, &
     &                          AGRA, FILOUT, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6061, -2, 'MALO', 'Failure in an attempt '// &
     &                 'to write the output file '//FILOUT )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( MAL(1)%SPH     ) ) DEALLOCATE ( MAL(1)%SPH     )
              IF ( ASSOCIATED ( MAL(1)%MJD_ARR ) ) DEALLOCATE ( MAL(1)%MJD_ARR )
              IF ( ASSOCIATED ( MAL(1)%TAI_ARR ) ) DEALLOCATE ( MAL(1)%TAI_ARR )
              IF ( ASSOCIATED ( AGRA%STOKES    ) ) DEALLOCATE ( AGRA%STOKES    )
              IF ( IR == 0 ) NS = NS + 1
              NP = NP + IR
 490       CONTINUE 
           CALL MALO_FREE ( MAL(1), IUER )
        ELSE IF ( MALO_TASK == 'sc_apply' ) THEN
           DO 4100 J10=1,L_FIL
              IP = INDEX ( C_FIL(J10), '.heb' )
              IF ( IP < 10 ) THEN
                   CALL  ERR_LOG ( 6062, -2, 'MALO', 'Trap of internal control '// &
     &                  ' -- input malformed file name '//C_FIL(J10) )
                   CALL EXIT ( 1 )
              END IF
!
              DATE_FIL = C_FIL(J10)(IP-13:IP-10)//'.'//C_FIL(J10)(IP-9:IP-8)//'.'// &
     &                   C_FIL(J10)(IP-7:IP-3)//':'//C_FIL(J10)(IP-2:IP-1)//'00.0'
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER  )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6054, -2, 'MALO', 'Unexpected format of '// &
     &                           'file name '//C_FIL(J10) )
                   CALL EXIT ( 1 )
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) THEN
                   NS = NS + 1
                   GOTO 4100
              END IF
              IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) THEN
                   NS = NS + 1
                   GOTO 4100
              END IF
              ID = LINDEX ( C_FIL(J10), '/' ) + 1
              FILOUT = MALO_OUTPUT(1:I_LEN(MALO_OUTPUT))//'/'//C_FIL(J10)(IP-13:)
 4100      CONTINUE 
      END IF
      IF ( MALO_INPUT_TYPE == 'directory' .AND. IVRB .GE. 2 ) THEN
           WRITE ( 6, 140 ) NP, NS
 140       FORMAT ( I6, ' data files have been be processed and ', I6, ' skipped' )
      END IF 
 910  CONTINUE 
      IF ( NP > 0 .AND. MAL(1)%CONF%MONTHLY_EPH ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, 150 ) NP
 150            FORMAT ( 'MALO: consolidating ', I6, ' output time series ', &
     &                   'into monthly files' )
           END IF 
           IUER = -1
           CALL MALO_EPHE_MONTHLY ( MAL(1), MALO_OUTPUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6062, -2, 'MALO', 'Failure in an attempt '// &
     &              'to re-organize output mass loading files in EPHEDISP '// &
     &              'format to monthly files' )
!!                CALL EXIT ( 1 )
                CONTINUE
           END IF
         ELSE 
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'MALO: finished '//MALO_TASK(1:I_LEN(MALO_TASK))// &
     &                        ' at '//GET_CDATE()
      END IF
      END  SUBROUTINE  MALO  !#!#
