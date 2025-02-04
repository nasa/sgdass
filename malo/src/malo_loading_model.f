      PROGRAM    MALO_LOADING_MODEL_LAUNCH
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
      CALL MALO_LOADING_MODEL()
      END  PROGRAM  MALO_LOADING_MODEL_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_LOADING_MODEL()
! ************************************************************************
! *                                                                      *
! *   Program  MALO_LOADING_MODEL  computes mass loading for the         *
! *   coefficients of the harmonic model of surface pressure field.      *
! *                                                                      *
! * ## 12-MAR-2013  MALO_LOADING_MODEL v6.1 (c) L. Petrov 18-FEB-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  CONFIG_FILE*128, FILIN_HEB*128, FILCOM*128, &
     &           FILDSC*128, FILOUT*128, FILNC*128, FILHEB*128, FILHPS*128, &
     &           FOLLOWUP_COM*1024, WISDOM_FILE*128, LOAD_TYPE*8, &
     &           NUM_THR_STR*128, EXT*8
      CHARACTER  STR*128, STR1*128 
      INTEGER*4  IVRB, IP, IS, NUM_THR, IUER
      LOGICAL*1  LEX
      INTEGER*8  FSH
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, SYSTEM
      INTEGER*8, EXTERNAL :: SPHE_INIT, SPHE_INIT_PLAN
!
      IVRB = 1
      CALL CLRCH ( FILDSC )
      CALL CLRCH ( FILCOM )
      CALL CLRCH ( FOLLOWUP_COM )
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_loading_model load|load_d1 malo_config input_heb '//&
     &                        ' output_file [fildsc] [filcom] [ivrb] [command]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, LOAD_TYPE   )
           IF ( LOAD_TYPE == 'load' ) THEN
                CONTINUE
              ELSE  IF ( LOAD_TYPE == 'load_d1' ) THEN
                CONTINUE
              ELSE
                IUER = -1
                CALL ERR_LOG ( 6301, IUER, 'MALO_LOADING_MODEL', 'Wrong first '// &
     &              'argument '//LOAD_TYPE//' one of load or load_d1 were '// &
     &              'expected' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 2, CONFIG_FILE )
!
           CALL GETARG ( 3, FILIN_HEB )
           IUER = 0
           CALL MALO_CHECK_SHARE_FILE ( FILIN_HEB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = 0
                CALL MALO_CHECK_MODEL_FILE ( FILIN_HEB, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6303, IUER, 'MALO_LOADING_MODEL', &
     &                   'Cannot find infput model file '//FILIN_HEB )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           CALL GETARG ( 4, FILOUT )
           CALL CLRCH ( EXT  )
           IP = LINDEX ( FILOUT, '.' ) 
           IF ( IP > 1 ) THEN
                EXT = FILOUT(IP:)
           END IF
           IF ( EXT == '.nc' ) THEN
                FILNC  = FILOUT
                FILHEB = FILOUT(1:I_LEN(FILOUT)-3)//'.heb'
              ELSE IF ( EXT == '.heb' ) THEN
                FILHEB = FILOUT
                FILNC  = FILOUT(1:I_LEN(FILOUT)-4)//'.nc'
                CONTINUE 
              ELSE IF ( EXT == '.hps' ) THEN
                FILHPS = FILOUT
              ELSE IF ( EXT == '.all' ) THEN
                FILHEB = FILOUT(1:I_LEN(FILOUT)-4)//'.heb'
                FILNC  = FILOUT(1:I_LEN(FILOUT)-4)//'.nc'
                FILHPS = FILOUT(1:I_LEN(FILOUT)-4)//'.hps'
                CONTINUE 
              ELSE 
                IUER = -1
                CALL ERR_LOG ( 6304, IUER, 'MALO_LOADING_MODEL', 'Unsupported '// &
     &              'output file name extension '//EXT(1:I_LEN(EXT))// &
     &              ' . Supported extensions: .nc, .heb, .hps, .all' )
                CALL EXIT ( 1 )
           END IF
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, FILDSC )
                CALL MALO_CHECK_SHARE_FILE ( FILDSC, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6305, IUER, 'MALO_LOADING_MODEL', &
               &         'Cannot find data source description file '//FILDSC )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, FILCOM )
                CALL MALO_CHECK_SHARE_FILE ( FILCOM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6306, IUER, 'MALO_LOADING_MODEL', &
               &         'Cannot find loading description file '//FILCOM )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                CALL CHIN   ( STR, IVRB )
           END IF
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, FOLLOWUP_COM )
           END IF
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( CONFIG_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6307, IUER, 'MALO_LOADING_MODEL', &
     &         'Cannot find configuration file '//CONFIG_FILE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CONFIG ( CONFIG_FILE, MALO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6308, IUER, 'MALO_LOADING_MODEL', &
     &         'Failure in an attempt to read and parse input file '// &
     &          FILIN_HEB(1:I_LEN(FILIN_HEB))//' in heb format' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(FILDSC) > 0 ) MALO%CONF%LOA_FINAM_DESCR = FILDSC
      IF ( ILEN(FILCOM) > 0 ) MALO%CONF%LOA_FINAM_COMM  = FILCOM
!
      IUER = -1
      CALL READ_LOVE ( MALO, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6309, IUER, 'MALO_LOADING_MODEL', 'Error in '// &
     &         'an attempt to read a file with Love numbers '// &
     &          MALO%CONF%LOVE_FILE )
           CALL EXIT ( 1 )
      END IF   
!
! --- Check how many threads should we use
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
      CALL INCH  ( NUM_THR, WISDOM_FILE )
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
           CALL ERR_LOG ( 6016, -2, 'MALO_LOADING_MODEL', 'Error in an '// &
     &         'attempt to initialize FSH object for spherical harmonics '// &
     &         'transform'  )
           CALL EXIT ( 1 )
      END IF   
!
      IF ( IVRB .EQ. -3 ) THEN
!
! -------- Compute loading displacements for large dimensions
!
           IUER = -1
           CALL MALO_LOD_MOD_LARGE ( MALO, FSH, LOAD_TYPE, EXT, FILIN_HEB, FILOUT, &
     &                               FILNC, FILHEB, FILHPS, FILDSC, FILCOM, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6018, -2, 'MALO_LOADING_MODEL', 'Error in an '// &
     &              'attempt to compute loading displacements in large mode' )
                CALL EXIT ( 1 )
           END IF   
        ELSE IF ( IVRB .NE. -3 ) THEN 
!!           write ( 6, * ) 'MALO_LOADING_MODEL: small variant' 
           IUER = -1
           CALL MALO_LOD_MOD_SMALL ( MALO, FSH, LOAD_TYPE, EXT, FILIN_HEB, FILOUT, &
     &                               FILNC, FILHEB, FILHPS, FILDSC, FILCOM, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6017, -2, 'MALO_LOADING_MODEL', 'Error in an '// &
     &              'attempt to compute loading displacements in small mode' )
                CALL EXIT ( 1 )
           END IF   
      END IF   
!
      IF ( ILEN(FOLLOWUP_COM) > 0 ) THEN
           WRITE ( 6, '(A)' ) 'MALO_LOADING_MODEL: Running the followup command'
           CALL FLUSH ( 6 )
           IS = SYSTEM ( FOLLOWUP_COM(1:I_LEN(FOLLOWUP_COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6019, IUER, 'MALO_LOADING_MODEL', 'Error in '// &
     &              'running the followup command '//FOLLOWUP_COM )
                CALL EXIT(  1 )
           ENDIF
      END IF
      WRITE ( 6, '(A)' ) 'MALO_LOADING_MODEL: finished'
      CALL FLUSH ( 6 )
!
      END  SUBROUTINE  MALO_LOADING_MODEL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_LOD_MOD_SMALL ( MALO, FSH, LOAD_TYPE, &
     &           EXT, FILIN_HEB, FILOUT, FILNC, FILHEB, FILHPS, FILDSC, &
     &           FILCOM, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_LOD_MOD_SMALL implements a variant of computation of  *
! *   mass loading for the coefficients of the harmonic model of surface *
! *   pressure field when the results is short, say less than 10Gb.      *
! *                                                                      *
! * ### 29-APR-2016 MALO_LOD_MOD_SMALL v2.0 (c) L. Petrov 03-JUN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*8  FSH
      CHARACTER  LOAD_TYPE*(*), EXT*(*), FILIN_HEB*(*), &
     &           FILCOM*(*), FILDSC*(*), FILOUT*(*), FILNC*(*), &
     &           FILHEB*(*), FILHPS*(*)
      INTEGER*4  IVRB, IUER
!
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LS, HEB_SC, HEB_DSPL
      CHARACTER  FRQ_STR*1024
      CHARACTER  STR*1024, STR1*128
      REAL*8,    ALLOCATABLE :: SPR_R8(:,:), DSP_ARR(:,:,:), DSPL_ARR(:,:,:,:), &
     &           DSP_ARR3(:,:,:), SPH_MD(:,:,:,:)
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:,:,:,:,:), SPR_R4(:,:), &
     &                          ARR_TMP_R4(:,:,:,:)
      REAL*8     LAT_VAL, LON_VAL, RES(3), LAT_GDT, GE, MALO__MAX_VAL 
      PARAMETER  ( MALO__MAX_VAL = 1.D6 )
      REAL*4     MOD_VAL
      INTEGER*4  M_FRQ
      PARAMETER  ( M_FRQ = (MALO__MFRQ-1)*2 + 1 )
      INTEGER*4  DEG, NORM, IPHS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, &
     &           J21, J22, L_LON, L_LAT, IP, ID, IS, L_FRQ, MD, MR, MLON, MLAT, &
     &           IND_LON, IND_LAT, IND_4, I_WAV, IND_HEB(MALO__MWAV), &
     &           IND_FRQ(MALO__MWAV), NUM_CMP(MALO__MWAV), L_CMP, IND_MOD, &
     &           IX_FRQ(MALO__MWAV), IX_CMP(MALO__MWAV), IER
      REAL*4     VAL_MIN, VAL_MAX
      INTEGER*8  NEL_I8
      CHARACTER  WAVE_FRQ(MALO__MWAV)*4, C_CMP(2)*1
      DATA       C_CMP / 'c', 's' /
      COMPLEX*16 CMPL_AMP_FRQ(MALO__MWAV)
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, SYSTEM
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FILIN_HEB, HEB_MOD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6031, IUER, 'MALO_LOADING_MODEL', &
     &         'Failure in an attempt to read and parse input file '// &
     &          FILIN_HEB(1:I_LEN(FILIN_HEB))//' in heb format' )
           RETURN
      END IF
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, * ) 'Allocated ', INT2(INT8(4)*INT8(HEB_MOD%DIMS(1))*INT8(HEB_MOD%DIMS(2))*INT8(HEB_MOD%DIMS(3))*INT8(HEB_MOD%DIMS(4))/INT8(1024)**3), ' Gb for heb_mod%val'
      END IF
      IF ( HEB_MOD%DIMS(4) == 1 ) THEN
!
! -------- Tranform linear array cos/sin into a two dimensional array with
! -------- the 3rd dimension running over frequency and 4th dimension running
! -------- over component (1: cos; 2: sin)
!
           NEL_I8 = HEB_MOD%DIMS(1)*HEB_MOD%DIMS(2)*HEB_MOD%DIMS(3)*HEB_MOD%DIMS(4)
           HEB_MOD%DIMS(4) = (HEB_MOD%DIMS(3) - 1)/2 + 1
           HEB_MOD%DIMS(3) = 2
           ALLOCATE ( ARR_TMP_R4(HEB_MOD%DIMS(1),HEB_MOD%DIMS(2),HEB_MOD%DIMS(3),HEB_MOD%DIMS(4)), &
     &                STAT=IER  )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( HEB_MOD%DIMS(1)*HEB_MOD%DIMS(2)*HEB_MOD%DIMS(3)*HEB_MOD%DIMS(4)*INT8(4), STR )
                CALL ERR_LOG ( 6032, IUER, 'MALO_LOAD_MOD_SHORT',  &
     &              'Failure in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for array ARR_TMP_R4' )
                RETURN
           END IF
!
           ARR_TMP_R4 = 0.0
           CALL LIB$MOVC8 ( NEL_I8*INT8(4), HEB_MOD%VAL, ARR_TMP_R4 )
           DEALLOCATE ( HEB_MOD%VAL )
!
           ALLOCATE   ( HEB_MOD%VAL(HEB_MOD%DIMS(1),HEB_MOD%DIMS(2),HEB_MOD%DIMS(3),HEB_MOD%DIMS(4)), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( HEB_MOD%DIMS(1)*HEB_MOD%DIMS(2)*HEB_MOD%DIMS(3)*HEB_MOD%DIMS(4)*INT8(4), STR )
                CALL ERR_LOG ( 6033, IUER, 'MALO_LOAD_MOD_SHORT',  &
     &              'Failure in an attempt tp allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for array HEB_MOD%VAL' )
                RETURN
           END IF
           CALL LIB$MOVC8 ( HEB_MOD%DIMS(1)*HEB_MOD%DIMS(2)*HEB_MOD%DIMS(3)*HEB_MOD%DIMS(4)*INT8(4), &
     &                      ARR_TMP_R4, HEB_MOD%VAL )
           DEALLOCATE ( ARR_TMP_R4 )
      END IF
      IF ( IVRB .GE. 7 ) THEN
           WRITE ( 6, * ) 'Shape(HEB_MOD%VAL) = ', SHAPE(HEB_MOD%VAL)
      END IF
!
      IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND  .OR. &
     &     MALO%CONF%SURFACE_TYPE == MALO__OCEAN      ) THEN
!
           IER = -1
           CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%FINAM_LS_MASK, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEB ( MALO%CONF%FINAM_LS_MASK, HEB_LS, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6034, IUER, 'MALO_LOAD_MOD_SHORT',  'Error '// &
     &              'in an attempt to read nc-file with land-sea '// &
     &              'mask '//MALO%CONF%FINAM_LS_MASK )
                RETURN
           END IF   
      END IF
      IF ( MALO%CONF%SC_FILE .NE. 'NONE' ) THEN
           IER = -1
           CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%SC_FILE, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEB ( MALO%CONF%SC_FILE, HEB_SC, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6035, IUER, 'MALO_LOAD_MOD_SHORT',  'Error '// &
     &              'in an attempt to read samling correction file '// &
     &               MALO%CONF%SC_FILE )
                RETURN
           END IF   
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'MALO_LOADING_MODEL: '// &
     &             'Started computation of loading harmonics coefficients'
           CALL FLUSH ( 6 )
      END IF
!
      MALO%NLON = 4*(MALO%CONF%OUTPUT_GRID_DEG+1)
      MALO%NLAT = 2*(MALO%CONF%OUTPUT_GRID_DEG+1)+1
      L_LON     = HEB_MOD%DIMS(1)
      L_LAT     = HEB_MOD%DIMS(2)
      IF ( 2*(L_LAT-1) == L_LON ) THEN
           L_LAT = L_LAT - 1
      END IF
      DEG  = L_LAT/2 - 1
      NORM = 1
      IPHS = 1
!
      ALLOCATE ( SPR_R8(L_LON,L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_LON*L_LAT, STR )
           CALL ERR_LOG ( 6035, IUER, 'MALO_LOAD_MOD_SHORT',  'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPR_R8' )
           RETURN
      END IF
      IF ( IVRB .GE. 6 ) THEN
           ALLOCATE ( SPR_R4(L_LON,L_LAT) )
      END IF
!
      ALLOCATE ( MALO%SPH(2,0:DEG,0:DEG,3,1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(2)*INT8(3)*INT8(DEG+1)**2, STR )
           CALL ERR_LOG ( 6036, IUER, 'MALO_LOAD_MOD_SHORT',  'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%SPH' )
           RETURN
      END IF
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, * ) 'Allocated ', INT2(INT8(8)*INT8(2)*INT8(3)*INT8(DEG+1)**2/INT8(1024)**3), ' Gb for malo%sph'
      END IF
!
      IND_MOD = 0
      IF ( HEB_MOD%COMMENT(2)(1:22) == 'Regression model code:' ) THEN
           CALL CHIN ( HEB_MOD%COMMENT(2)(24:), IND_MOD )
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_WAVE_TABLE ( MALO, HEB_MOD%SDS_NAME, L_FRQ, IND_FRQ, &
     &                       IND_MOD, IND_HEB, NUM_CMP, L_CMP,       &
     &                       IX_CMP, IX_FRQ, CMPL_AMP_FRQ, FRQ_STR,  &
     &                       IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6037, IUER, 'MALO_LOAD_MOD_SHORT',  'Error in '// &
     &         'an attempt to assigne cross-reference table to tidal '// &
     &         'waves' )
           RETURN
      END IF
!
      IF ( MALO%CONF%OUTPUT_GRID_DEG > 0 ) THEN
           IF ( IVRB .EQ. 8 ) THEN
                WRITE ( 6, * ) 'H7 ', MALO%NLON, MALO%NLAT, L_FRQ
                CALL FLUSH ( 6 )
           END IF
           ALLOCATE ( DSP_ARR(3,MALO%NLON,MALO%NLAT), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*INT8(3)*INT8(MALO%NLON)*INT8(MALO%NLAT), STR )
                CALL ERR_LOG ( 6038, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
           IF ( IVRB .GE. 5 ) THEN
                WRITE ( 6, * ) 'Allocated ', INT2(INT8(8)*INT8(3)*INT8(MALO%NLON)*INT8(MALO%NLAT)/INT8(1024)**3), ' Gb for dsp_arr'
           END IF
           DSP_ARR    = 0.0D0
           ALLOCATE ( DSP_ARR_R4(MALO%NLON,MALO%NLAT,3,2,L_FRQ), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*INT8(3)*INT8(MALO%NLON)*INT8(MALO%NLAT)*INT8(L_FRQ)*INT8(2), STR )
                CALL ERR_LOG ( 6050, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
           IF ( IVRB .GE. 5 ) THEN
                WRITE ( 6, * ) 'Allocated ', INT2(INT8(4)*INT8(3)*INT8(MALO%NLON)*INT8(MALO%NLAT)*INT8(L_FRQ)*INT8(2)/INT8(1024)**3), ' Gb for dsp_arr_r4'
           END IF
           DSP_ARR_R4 = 0.0
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL MALO_INP_STA ( MALO, MALO%CONF%STATION_FINAM, 0.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6039, IUER, 'MALO_LOAD_MOD_SHORT', 'Failure in '// &
     &              'an attempt to load station file '// &
     &               MALO%CONF%STATION_FINAM )
                RETURN 
           END IF 
!
           ALLOCATE ( DSPL_ARR(3,MALO%NSTA,L_FRQ,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*MALO%NSTA*L_FRQ, STR )
                CALL ERR_LOG ( 6040, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                RETURN 
           END IF
      END IF
!
      IND_4 = 0
      DO 410 J1=1,L_FRQ
         DO 420 J2=1,NUM_CMP(J1)
            IND_4 = IND_4 + 1
            IF ( IVRB .GE. 2 ) THEN
                 WRITE ( 6, 110 ) J1, L_FRQ, J2, NUM_CMP(J1), IND_HEB(J1), IND_4
 110             FORMAT ( 'Wave ', I3, ' ( ', I3, ' )   Component ', I1, ' ( ', I2, ' )  Ind_Heb: ', I2, ' Ind4: ', I2 ) 
                 CALL FLUSH ( 6 )
            END IF
!
            DO 430 J3=1,L_LAT
               IND_LAT = J3
               IF ( IND_LAT > HEB_LS%DIMS(2) ) IND_LAT = HEB_LS%DIMS(2) 
               LAT_GDT = -P2I + (J3-1)*PI__NUM/(L_LAT-1)
               GE = MALO__ACC_EQU_WGS84*(1.D0 + MALO__GRV_LAT_WGS84*DSIN(LAT_GDT)**2)/ &
     &              DSQRT(1.D0 - (2.D0*MALO__FLAT_WGS84 - MALO__FLAT_WGS84**2)*DSIN(LAT_GDT)**2 )
               DO 440 J4=1,L_LON
                  IND_LON = J4
                  IF ( IND_LON > HEB_LS%DIMS(1) ) IND_LON = IND_LON - HEB_LS%DIMS(1) 
                  IF ( HEB_MOD%VAL(J4,J3,1,IND_HEB(J1)) > MALO__MAX_VAL ) THEN
                       SPR_R8(J4,J3) = 0.0D0
                       GOTO 440
                  END IF
                  IF ( CMPL_AMP_FRQ(J1) == (1.0D0, 0.0D0) ) THEN
!
! -------------------- Main wave
!
                       MOD_VAL = HEB_MOD%VAL(J4,J3,J2,IND_HEB(J1))
                     ELSE
!
! -------------------- Sidelobe wave that is constructed on the fly from 
! -------------------- the main component by multiplying by a complex admitance
!
                       IF ( J2 == 1 ) THEN
                            MOD_VAL = HEB_MOD%VAL(J4,J3,1,IND_HEB(J1))*REAL(CMPL_AMP_FRQ(J1)) - &
     &                                HEB_MOD%VAL(J4,J3,2,IND_HEB(J1))*IMAG(CMPL_AMP_FRQ(J1)) 
                          ELSE IF ( J2 == 2 ) THEN
                            MOD_VAL = HEB_MOD%VAL(J4,J3,1,IND_HEB(J1))*IMAG(CMPL_AMP_FRQ(J1)) + &
     &                                HEB_MOD%VAL(J4,J3,2,IND_HEB(J1))*REAL(CMPL_AMP_FRQ(J1)) 
                       END IF
                  END IF
                  IF ( HEB_MOD%UNITS == "meter" ) THEN
!
! -------------------- Convert sea height (m) to bottom pressure (Pa)
!
                       MOD_VAL = GE*MALO__SW_DENS*MOD_VAL 
                  END IF
                  IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
                       SPR_R8(J4,J3) = MOD_VAL*HEB_LS%VAL(IND_LON,IND_LAT,1,1)
                    ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
                       SPR_R8(J4,J3) = MOD_VAL*(1.D0 - HEB_LS%VAL(IND_LON,IND_LAT,1,1))* &
     &                                 MALO__SW_DENS/1000.0D0
                    ELSE 
                       SPR_R8(J4,J3) = MOD_VAL
                  END IF
 440           CONTINUE 
 430        CONTINUE 
            IF  ( IVRB == 6 .OR. IVRB == 7 ) THEN
                  SPR_R4 = SPR_R8
                  IER = -1
                  STR = 'Pressure: Wave '//OTID_WAV(IND_FRQ(J1))//' Comp: '//C_CMP(J2)
                  VAL_MIN =  1.0
                  VAL_MAX = -1.0
                  CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, SPR_R4, &
     &                               STR(1:I_LEN(STR)), 'Pa', VAL_MIN, VAL_MAX, &
     &                               '/tmp/boo', IER )
            END IF
!
            IF ( IVRB .EQ. 2 .OR. IVRB .EQ. 8 ) THEN
                 WRITE ( 6, * ) 'MLM Before SPHE-transform J1= ', INT2(J1), ' L_LAT= ', L_LAT
                 CALL FLUSH ( 6 )
                 CALL WALL_TIMER ( %VAL(0) ) 
            END IF
            CALL ERR_PASS ( IUER, IER )
            CALL SPHE_DIR_2NN ( %VAL(FSH), L_LAT, SPR_R8, DEG, DEG, NORM, &
     &                          IPHS, MALO%SPH(1,0,0,3,1), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6041, IUER, 'MALO_LOAD_MOD_SHORT',  'Error in '// &
     &               'an attempt to perform spherical transform of the '// &
     &               'pressure field' )
                 RETURN
            END IF
            IF ( IVRB == 8 ) THEN
                 CALL WALL_TIMER ( STR ) 
                 WRITE ( 6, * ) 'Direct SHT: '//STR(1:25)
                 CALL WALL_TIMER ( %VAL(0) ) 
            END IF
!
! --------- Multiply spherical harmonics by Love numbers
!
            DO 450 J5=0,DEG
               DO 460 J6=J5,DEG
                  DO 470 J7=1,2
!
! ------------------ NB: We transpose the 2nd and 3rd index since 
! ------------------ SPHE_COMP_VEC is expects to find coefficients of the 
! ------------------ spherical harmonic transform in this order:
! ------------------ real/image, order, degree, radial/transverse
!
                     MALO%SPH(J7,J5,J6,1,1) = MALO%SPH(J7,J6,J5,3,1)* &
     &                                      3.0D0*MALO%LOVE(J6,MALO__H)/(2*J6+1)/ &
     &                                      (MALO__DENS*MALO__GRAV)
                     MALO%SPH(J7,J5,J6,2,1) = MALO%SPH(J7,J6,J5,3,1)* &
     &                                      3.0D0*MALO%LOVE(J6,MALO__L)/(2*J6+1)/ &
     &                                      (MALO__DENS*MALO__GRAV)
 470              CONTINUE 
 460           CONTINUE 
 450        CONTINUE 
!
            IF ( LOAD_TYPE == 'load_d1' ) THEN
!
! ------------- In a case if compute correction for D1-term, we 
! ------------- unscale spherical harmonic of degree one.
!
                MALO%SPH(1:2,0:1,1,MALO__H,1) = 1.0D0/MALO%LOVE(1,MALO__H)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__H,1)
                MALO%SPH(1:2,0:1,1,MALO__L,1) = 1.0D0/MALO%LOVE(1,MALO__L)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__L,1)
                MALO%SPH(1:2,0:2,2,MALO__H,1) = 0.0D0
                MALO%SPH(1:2,0:2,2,MALO__L,1) = 0.0D0
!
                MALO%SPH(1:2,1,0,MALO__H,1)  = MALO%SPH(1:2,0,1,MALO__H,1)  
                MALO%SPH(1:2,1,0,MALO__L,1)  = MALO%SPH(1:2,0,1,MALO__L,1)  
            END IF
            IF ( IVRB .EQ. 2 ) THEN
                 WRITE ( 6, * ) 'MLM After  SPHE-transform J1= ', INT2(J1), ' DEG= ', DEG
                 CALL FLUSH ( 6 )
            END IF
!
            IF ( MALO%CONF%OUTPUT_GRID_DEG > 0 ) THEN
#ifdef MALO_GRID_DIRECT  ! Pre Feb-2015 code. It is very slow
!  ############################################################################################################
                 DO 480 J8=1,MALO%NLAT
                    LAT_VAL = -P2I + (J8-1)*PI__NUM/(MALO%NLAT-1)
                    DO 490 J9=1,MALO%NLON 
                       LON_VAL = (J9-1)*PI2/MALO%NLON
                       CALL ERR_PASS ( IUER, IER )
                       IF ( LOAD_TYPE == 'load' ) THEN
                            CALL SPHE_COMP_VEC ( %VAL(FSH), DEG, DEG, LAT_VAL, &
     &                                           LON_VAL, NORM, IPHS, &
     &                                           MALO%SPH, DSP_ARR(1,J9,J8,IND_4), &
     &                                           IER )
                          ELSE IF ( LOAD_TYPE == 'load_d1' ) THEN
                            CALL SPHE_COMP_VEC ( %VAL(FSH), DEG, 2, LAT_VAL, &
     &                                           LON_VAL, NORM, IPHS, &
     &                                           MALO%SPH, DSP_ARR(1,J9,J8), &
     &                                           IER )
                       END IF
                       IF ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 6042, IUER, 'MALO_LOAD_MOD_SHORT',  &
     &                          'Error in an attempt to compute loading '// &
     &                          'for a given point using spherical harmonics' )
                            RETURN
                       END IF
 490                CONTINUE 
                    IF ( IVRB .GE. 3 ) THEN
                         WRITE ( 6, 120 ) J1, L_FRQ, J2, J8, MALO%NLAT, CHAR(13)
 120                     FORMAT ( '  MLM Computing for freq ', I3, ' ( ', I3, ' ) ', &
     &                            ' Cmp: ', I1, ' Lat ', I4, ' ( ', I4, ' ) ', A$ )
                         CALL FLUSH ( 6 ) 
                    END IF
 480             CONTINUE 
!  ############################################################################################################
#else
!
! -------------- Mordern code
!
                 MR = 4*(DEG+1)/MALO%NLON
                 IF ( MR*MALO%NLON < 4*(DEG+1) ) MR = MR + 1
                 IF ( MR < 1 ) THEN
                      WRITE ( 6, * ) 'DEG= ', DEG, ' MALO%NLON= ', MALO%NLON
                      CALL ERR_LOG ( 6043, IUER, 'MALO_LOAD_MOD_SHORT',  'Trap of internal '// &
     &                    'control: multiresolution factor is less than 2. '// &
     &                    'Please check dimnesions' )
                      RETURN
                 END IF
                 MLON =  MALO%NLON*MR
                 MLAT = (MALO%NLAT-1)*MR + 1
                 MD   = MLON/4 - 1
                 IF ( IVRB .GE. 4 ) THEN
                      WRITE ( 6, * ) 'MALO%NLON = ', MALO%NLON, ' MLON= ', MLON
                      WRITE ( 6, * ) 'MALO%NLAT = ', MALO%NLAT, ' MLAT= ', MLAT
                      WRITE ( 6, * ) 'DEG       = ', DEG,       ' MD=   ', MD
                 END IF
!
! -------------- Create the extended copy of scaled spherical harmonics
!
                 ALLOCATE ( SPH_MD(2,0:MD,0:MD,2), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL IINCH ( 8*2*2*(MD+1)*(MD+1), STR )
                      CALL ERR_LOG ( 6044, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure to '// &
     &                    'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &                    'memory' )
                      RETURN 
                 END IF
                 SPH_MD = 0.0D0
                 DO 480 J8=0,DEG
                    DO 490 J9=0,J8
                       SPH_MD(1:2,J9,J8,1) = MALO%SPH(1:2,J9,J8,1,1)
                       SPH_MD(1:2,J9,J8,2) = MALO%SPH(1:2,J9,J8,2,1)
                       SPH_MD(1:2,J8,J9,1) = MALO%SPH(1:2,J9,J8,1,1)
                       SPH_MD(1:2,J8,J9,2) = MALO%SPH(1:2,J9,J8,2,1)
 490                CONTINUE 
 480             CONTINUE 
!
                 ALLOCATE ( DSP_ARR3(MLON,MLAT,3), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL IINCH ( 8*3*MLON*(MLAT-1), STR )
                      CALL ERR_LOG ( 6045, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure to '// &
     &                    'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &                    'memory' )
                      RETURN 
                 END IF
                 IF ( IVRB == 8 ) THEN
                      CALL WALL_TIMER ( STR ) 
                      WRITE ( 6, * ) 'Love numbers scaling: '//STR(1:25)
                      CALL WALL_TIMER ( %VAL(0) ) 
                 END IF
                 IF ( LOAD_TYPE == 'load' ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, MD, SPH_MD, &
     &                                        MLAT, DSP_ARR3, IER )
                      IF ( IER .NE. 0 ) THEN
                           write ( 6, * ) 'DEG= ', DEG, ' NALO%NLAT = ', MLAT 
                           CALL ERR_LOG ( 6046, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &                         'in an attempt to perform inverse vector spherical '// &
     &                         'harmonic transform' )
                           RETURN 
                      END IF
                      IF ( IVRB == 8 ) THEN
                           CALL WALL_TIMER ( STR ) 
                           WRITE ( 6, * ) 'Inverse SHT: '//STR(1:25)
                           CALL WALL_TIMER ( %VAL(0) ) 
                      END IF
                    ELSE IF ( LOAD_TYPE == 'load_d1' ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, 2, SPH_MD, &
     &                                        MLAT, DSP_ARR3, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 6047, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &                         'in an attempt to perform inverse vector spherical '// &
     &                         'harmonic transform' )
                           RETURN 
                      END IF
                 END IF
!
                 IF ( MALO%CONF%SC_FILE .EQ. 'NONE' ) THEN
!
! ------------------- No sampling correction was requested
!
                      DO 4100 J10=1,MALO%NLAT
                         DO 4110 J11=1,MALO%NLON 
                            DSP_ARR(1,J11,J10) = DSP_ARR3(1+MR*(J11-1),1+MR*(J10-1),1)
                            DSP_ARR(2,J11,J10) = DSP_ARR3(1+MR*(J11-1),1+MR*(J10-1),2)
                            DSP_ARR(3,J11,J10) = DSP_ARR3(1+MR*(J11-1),1+MR*(J10-1),3)
 4110                    CONTINUE 
 4100                 CONTINUE 
                    ELSE
!
! ------------------- Sampling correction was requested
!
                      DO 4120 J12=1,MALO%NLAT-1
                         DO 4130 J13=1,MALO%NLON 
                            DSP_ARR(1,J13,J12) = DSP_ARR3(1+MR*(J13-1),1+MR*(J12-1),1) &
     &                             + SPR_R8(J13,J12)*HEB_SC%VAL(J13,J12,1,1) 
                            DSP_ARR(2,J13,J12) = DSP_ARR3(1+MR*(J13-1),1+MR*(J12-1),2) &
     &                             + SPR_R8(J13,J12)*HEB_SC%VAL(J13,J12,2,1) 
                            DSP_ARR(3,J13,J12) = DSP_ARR3(1+MR*(J13-1),1+MR*(J12-1),3) &
     &                             + SPR_R8(J13,J12)*HEB_SC%VAL(J13,J12,3,1) 
 4130                    CONTINUE 
 4120                 CONTINUE 
!
! ------------------- Keep in mind that the 2nd dimension SPR_R8 is defined from
! ------------------- 1 to L_LAT = MALO%NLAT - 1. Just skip sampling correction
! ------------------- for the north pole
!
                      DO 4140 J14=1,MALO%NLON 
                         DSP_ARR(1,J14,MALO%NLAT) =DSP_ARR3(1+MR*(J14-1),1+MR*(MALO%NLAT-1),1)
                         DSP_ARR(2,J14,MALO%NLAT) =DSP_ARR3(1+MR*(J14-1),1+MR*(MALO%NLAT-1),2)
                         DSP_ARR(3,J14,MALO%NLAT) =DSP_ARR3(1+MR*(J14-1),1+MR*(MALO%NLAT-1),3)
 4140                 CONTINUE 
                 END IF
!
! -------------- Deallocate temporary arrays
!
                 DEALLOCATE ( DSP_ARR3 ) 
                 DEALLOCATE ( SPH_MD   )
#endif
                 IF ( IVRB == 7 ) THEN
                      IF ( .NOT. ALLOCATED ( DSP_ARR_R4 ) ) THEN
                           ALLOCATE ( DSP_ARR_R4(MALO%NLON,MALO%NLAT,3,2,L_FRQ), STAT=IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL IINCH ( 4*3*MALO%NLON*MALO%NLAT*L_FRQ*2, STR )
                                CALL ERR_LOG ( 6048, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &                              'to allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &                              'memory' )
                                RETURN 
                            END IF
                      END IF
!
                      DO 4150 J15=1,MALO%NLAT
                         DO 4160 J16=1,MALO%NLON
                            DSP_ARR_R4(J16,J15,1,1,1) = 1.D3*DSP_ARR(1,J16,J15)
 4160                    CONTINUE 
 4150                 CONTINUE 
                      STR = 'Loading Up: '//OTID_WAV(IND_FRQ(J1))//' Comp: '//C_CMP(J2)
                      IER = -1
                      VAL_MIN =  1.0
                      VAL_MAX = -1.0
                      CALL PLOT_GRID_R4 ( 1, 7, 0, 1, MALO%NLON, MALO%NLAT, DSP_ARR_R4, &
     &                                   STR(1:I_LEN(STR)), 'mm', VAL_MIN, VAL_MAX, &
     &                                   '/tmp/boo', IER )
                 END IF
!
! -------------- Put the loading results in the output array
!
                 DO 4170 J17=1,3 
                    DO 4180 J18=1,MALO%NLAT
                       DO 4190 J19=1,MALO%NLON 
                          DSP_ARR_R4(J19,J18,J17,IX_CMP(IND_4),IX_FRQ(IND_4)) = DSP_ARR(J17,J19,J18)
 4190                  CONTINUE 
 4180               CONTINUE 
 4170            CONTINUE 
               ELSE 
                 DO 4200 J20=1,MALO%NSTA
                    CALL ERR_PASS ( IUER, IER )
                    CALL SPHE_COMP_VEC ( %VAL(FSH), DEG, DEG, MALO%STA(J20)%LAT_GDT, &
     &                                   MALO%STA(J20)%LON, NORM, IPHS, &
     &                                   MALO%SPH, DSPL_ARR(1,J20,J1,J2), &
     &                                   IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6049, IUER, 'MALO_LOAD_MOD_SHORT',  &
     &                       'Error in an attempt to compute loading '// &
     &                       'for a given station using spherical harmonics' )
                         RETURN 
                    END IF
                    IF ( IVRB .GE. 3 ) THEN
                         WRITE ( 6, 130 ) J1, L_FRQ, J2, NUM_CMP(J2), &
     &                                    J20, MALO%NSTA, CHAR(13)
 130                     FORMAT ( '  MLM Computing for freq ', I3, ' ( ', I3, ' ) ', &
     &                            ' Cmp: ', I1, ' ( ', I1, ' )  ', &
     &                            'Station ', I8, ' ( ', I8, ' ) ', A$ )
                         CALL FLUSH ( 6 ) 
                    END IF
 4200            CONTINUE 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!     
      IF ( IVRB == 8 ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, * ) 'After SHT: '//STR(1:25)
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
      IF ( EXT == '.heb' .OR. EXT == '.nc' .OR. EXT == '.all' ) THEN
           IF ( ALLOCATED  ( SPR_R8      ) ) DEALLOCATE ( SPR_R8      )
           IF ( ASSOCIATED ( HEB_MOD%VAL ) ) DEALLOCATE ( HEB_MOD%VAL )
           IF ( ASSOCIATED ( MALO%SPH    ) ) DEALLOCATE ( MALO%SPH    )
           IF ( ASSOCIATED ( HEB_MOD%VAL ) ) DEALLOCATE ( HEB_MOD%VAL )
!
! sha(DSP_ARR) =            3       10800        5401          74
! sha(DSP_ARR) =        10800        5401           3           2          37
!
           IF ( MALO%CONF%OUTPUT_GRID_DEG == 0 ) THEN
                CALL ERR_LOG ( 6051, IUER, 'MALO_LOAD_MOD_SHORT',  &
     &              'Output in HEB-format is supported only for a case '// &
     &              'of gridded output. Please use HARPOS-format '// &
     &              'for station output' )
                RETURN 
           END IF  
!
           ID = LINDEX ( FILOUT, '/' ) 
!
           HEB_DSPL%MJD     = HEB_MOD%MJD
           HEB_DSPL%UTC     = HEB_MOD%UTC
           HEB_DSPL%TAI     = HEB_MOD%TAI 
           HEB_DSPL%DIMS(1) = MALO%NLON
           HEB_DSPL%DIMS(2) = MALO%NLAT 
           HEB_DSPL%DIMS(3) = 3
           HEB_DSPL%DIMS(4) = L_CMP
           HEB_DSPL%DATA_OFFSET = HEB__HDS
           HEB_DSPL%ENDIAN      = HEB__LE
           HEB_DSPL%DATA_TRANSFORM = HEB__NONE
           HEB_DSPL%FILL_VALUE     = 1.0E15
           HEB_DSPL%OFFSET         = 0.0
           HEB_DSPL%SCALE_FACTOR   = 1.0 ! 3.D-6
           HEB_DSPL%DATA_COMPRESSION = HEB__NONE
           STR = 'Site displacement '//FRQ_STR(1:I_LEN(FRQ_STR))  
           IF ( ILEN(STR) .LE. LEN(HEB_DSPL%SDS_NAME) ) THEN
                HEB_DSPL%SDS_NAME  = STR
              ELSE
                HEB_DSPL%SDS_NAME  = STR(1:LEN(HEB_DSPL%SDS_NAME))
                HEB_DSPL%COMMENT(4) = 'SDS_NAME continuation '//STR(LEN(HEB_DSPL%SDS_NAME)+1:)
           END IF
           HEB_DSPL%UNITS          = 'meter'
           HEB_DSPL%DATA_FORMAT    = HEB__R4
           HEB_DSPL%MIN_VALUE      = MINVAL(DSP_ARR_R4)
           HEB_DSPL%MAX_VALUE      = MAXVAL(DSP_ARR_R4)
           HEB_DSPL%VALID_RANGE(1) =  -0.096D0
           HEB_DSPL%VALID_RANGE(2) =   0.096D0
           HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
!
           HEB_DSPL%FILE_NAME      = FILHEB(ID+1:)
           HEB_DSPL%HISTORY        = 'Processed using input numerical weather '// &
     &                               'model and input digital elevation map'
           HEB_DSPL%SOURCE         = 'Output of the regression model over surface pressure'
           HEB_DSPL%TITLE          = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%PROD_NAME      = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%INSTITUTION    = 'Astrogeo Center'
           HEB_DSPL%REFERENCES     = 'http://astrogeo.org/malo/'
           HEB_DSPL%VERSION_ID     = MALO__LABEL
           HEB_DSPL%COMMENT(1)     = HEB_MOD%COMMENT(1)
           HEB_DSPL%COMMENT(2)     = HEB_MOD%COMMENT(2)
           HEB_DSPL%COMMENT(3)     = HEB_MOD%COMMENT(3)
!
           IF ( IVRB .GE. 3 ) THEN
                WRITE ( 6, * ) 'L_FRQ= ', L_FRQ
                WRITE ( 6, * ) 'Writing in the output file '//FILHEB(1:I_LEN(FILHEB))
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL WRITE_HEB ( HEB_DSPL, DSP_ARR_R4, FILHEB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6052, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILHEB )
                RETURN 
           END IF
           IF ( IVRB == 8 ) THEN
                CALL WALL_TIMER ( STR ) 
                WRITE ( 6, * ) 'Write HEB: '//STR(1:25)
                CALL WALL_TIMER ( %VAL(0) ) 
           END IF
!
           IF ( IVRB .GE. 3 ) THEN
                WRITE ( 6, * ) 'Writing in the output file '//FILNC(1:I_LEN(FILNC))
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL WRITE_LOADING_NC ( MALO%NLON, MALO%NLAT, L_FRQ, IND_FRQ, &
     &                             HEB_MOD%MJD, HEB_MOD%TAI, %VAL(0), DSP_ARR_R4, &
     &                             MALO__LABEL, 'NO', FILDSC, FILCOM, FILNC, &
     &                             IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6053, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILNC )
                RETURN 
           END IF
           IF ( IVRB == 8 ) THEN
                CALL WALL_TIMER ( STR ) 
                WRITE ( 6, * ) 'Write LOADING_NC: '//STR(1:25)
                CALL WALL_TIMER ( %VAL(0) ) 
           END IF
        ELSE IF ( EXT == '.hps' ) THEN
           IF ( IVRB .GE. 3 ) THEN
               WRITE ( 6, * ) 'Writing in the output file '//FILHPS(1:I_LEN(FILHPS))
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL MALO_HARPOS_WRITE ( MALO%NSTA, L_FRQ, IND_FRQ, NUM_CMP, &
     &                              DSPL_ARR, MALO, MALO__LABEL, FILHPS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6054, IUER, 'MALO_LOAD_MOD_SHORT',  'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILHPS )
                RETURN 
           END IF
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_LOD_MOD_SMALL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_LOD_MOD_LARGE ( MALO, FSH, LOAD_TYPE, EXT, &
     &           FILIN_HEB, FILOUT, FILNC, FILHEB, FILHPS, FILDSC, &
     &           FILCOM, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_LOD_MOD_LARGE implements a variant of computation of  *
! *   mass loading for the coefficients of the harmonic model of surface *
! *   pressure field when the results is large, say more than 10Gb.      *
! *                                                                      *
! * ### 13-MAY-2016 MALO_LOD_MOD_LARGE v1.1 (c) L. Petrov 01-JUN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*8  FSH
      CHARACTER  LOAD_TYPE*(*), EXT*(*), FILIN_HEB*(*), &
     &           FILCOM*(*), FILDSC*(*), FILOUT*(*), FILNC*(*), &
     &           FILHEB*(*), FILHPS*(*)
      INTEGER*4  IVRB, IUER
!
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LS, HEB_SC, HEB_DSPL
      CHARACTER  FRQ_STR*1024
      CHARACTER  STR*128, STR1*128
      REAL*8,    ALLOCATABLE :: SPR_R8(:,:), DSP_ARR3_R8(:,:,:)
      REAL*4,    ALLOCATABLE :: DSP_ARR3_R4(:,:,:), SPR_R4(:,:)
      REAL*8     LAT_VAL, LON_VAL, RES(3), LAT_GDT, GE, SPH_VAL, MALO__MAX_VAL 
      PARAMETER  ( MALO__MAX_VAL = 1.D6 )
      REAL*4     MOD_VAL, LOAD_MIN, LOAD_MAX
      INTEGER*4  M_FRQ
      PARAMETER  ( M_FRQ = (MALO__MFRQ-1)*2 + 1 )
      INTEGER*4  DEG, NORM, IPHS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, &
     &           J21, J22, L_LON, L_LAT, IP, ID, IS, L_FRQ, MD, MR, MLON, MLAT, &
     &           IND_LON, IND_LAT, IND_4, I_WAV, IND_HEB(MALO__MWAV), &
     &           IND_FRQ(MALO__MWAV), NUM_CMP(MALO__MWAV), L_CMP, LUN, &
     &           SEEK_SET, IND3_SECT(2), IND4_SECT(2), ARG_LN, IND_MOD, &
     &           IX_FRQ(MALO__MWAV), IX_CMP(MALO__MWAV), IER
      INTEGER*8  LEN_I8, OFFS_I8, TOT_LEN_I8, OFFSET_RET 
      LOGICAL*1  LEX
      CHARACTER  WAVE_FRQ(MALO__MWAV)*4, C_CMP(2)*1
      DATA       C_CMP / 'c', 's' /
      REAL*4     VAL_MIN, VAL_MAX
      COMPLEX*16 CMPL_AMP_FRQ(MALO__MWAV)
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, SYSTEM
      INTEGER*8, EXTERNAL :: LSEEK
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
!
! --- Read input file with harmonic model
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FILIN_HEB, HEB_MOD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6211, IUER, 'MALO_LOD_MOD_LARGE', &
     &         'Failure in an attempt to read and parse the headeer of '// &
     &         'input file '//FILIN_HEB(1:I_LEN(FILIN_HEB))// &
     &         ' in heb format' )
           RETURN
      END IF
      IF ( HEB_MOD%DIMS(4) == 1 ) THEN
           CALL ERR_LOG ( 6212, IUER, 'MALO_LOD_MOD_LARGE', &
     &         'Trap of internal control: the last dimension is 1.'// &
     &         ' Please use MALO_LOD_MOD_SMALL for such a case' )
           RETURN
      END IF
      IF ( MALO%CONF%OUTPUT_GRID_DEG < 2 ) THEN
           CALL ERR_LOG ( 6213, IUER, 'MALO_LOD_MOD_LARGE', &
     &         'Trap of internal control: this routine can only '// &
     &         'computed gridded loading' )
           RETURN
      END IF
!
      IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND  .OR. &
     &     MALO%CONF%SURFACE_TYPE == MALO__OCEAN      ) THEN
!
! -------- Read land-sea mask
!
           IER = -1
           CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%FINAM_LS_MASK, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEB ( MALO%CONF%FINAM_LS_MASK, HEB_LS, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6214, IUER, 'MALO_LOD_MOD_LARGE',  'Error '// &
     &              'in an attempt to read nc-file with land-sea '// &
     &              'mask '//MALO%CONF%FINAM_LS_MASK )
                RETURN
           END IF   
      END IF
      IF ( MALO%CONF%SC_FILE .NE. 'NONE' ) THEN
!
! -------- Read the samling correction file
!
           IER = -1
           CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%SC_FILE, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEB ( MALO%CONF%SC_FILE, HEB_SC, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6215, IUER, 'MALO_LOD_MOD_LARGE',  'Error '// &
     &              'in an attempt to read samling correction file '// &
     &               MALO%CONF%SC_FILE )
                RETURN
           END IF   
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A,A,A)' ) 'MALO_LOADING_MODEL: ', GET_CDATE(),  &
     &             ' Started  loading compution'
           CALL FLUSH ( 6 )
      END IF
!
! --- Define the output dimension
!
      MALO%NLON = 4*(MALO%CONF%OUTPUT_GRID_DEG+1)
      MALO%NLAT = 2*(MALO%CONF%OUTPUT_GRID_DEG+1)+1
      L_LON     = HEB_MOD%DIMS(1)
      L_LAT     = HEB_MOD%DIMS(2)
      IF ( 2*(L_LAT-1) == L_LON ) THEN
           L_LAT = L_LAT - 1
      END IF
      DEG  = L_LAT/2 - 1
      NORM = 1
      IPHS = 1
!
! --- NB: support of reduced output grid
!
      MR = 4*(DEG+1)/MALO%NLON
      IF ( MR*MALO%NLON < 4*(DEG+1) ) MR = MR + 1
      IF ( MR < 1 ) THEN
           WRITE ( 6, * ) 'DEG= ', DEG, ' MALO%NLON= ', MALO%NLON
           CALL ERR_LOG ( 6216, IUER, 'MALO_LOD_MOD_LARGE', 'Trap '// &
     &         'of internal control: multiresolution factor is less '// &
     &         'than 2. Please check dimnesions' )
           RETURN
      END IF
      MLON =  MALO%NLON*MR
      MLAT = (MALO%NLAT-1)*MR + 1
      MD   = MLON/4 - 1
!
! --- Allocate memory
!
      IF ( IVRB .EQ. 6 .OR. IVRB .EQ. 7 ) THEN
           ALLOCATE ( SPR_R4(L_LON,L_LAT) )
      END IF
!
! --- Get the frequency table
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_WAVE_TABLE ( MALO, HEB_MOD%SDS_NAME, L_FRQ, IND_FRQ, IND_MOD, &
     &                       IND_HEB, NUM_CMP,  L_CMP, IX_CMP, IX_FRQ, &
     &                       CMPL_AMP_FRQ, FRQ_STR, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6217, IUER, 'MALO_LOD_MOD_LARGE', 'Error in '// &
     &         'an attempt to assign cross-reference table to tidal '// &
     &         'waves' )
           RETURN
      END IF
!
      FRQ_STR = '|'
      IND_4 = 0
      DO 410 J1=1,L_FRQ
         DO 420 J2=1,NUM_CMP(J1)
            IND_4 = IND_4 + 1
            FRQ_STR = FRQ_STR(1:I_LEN(FRQ_STR))//OTID_WAV(IND_FRQ(J1))// &
     &                C_CMP(J2)//'|'
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( EXT == '.heb' .OR. EXT == '.nc' .OR. EXT == '.all' ) THEN
!
! -------- Define HEB description
!
           ID = LINDEX ( FILOUT, '/' ) 
           HEB_DSPL%MJD     = HEB_MOD%MJD
           HEB_DSPL%UTC     = HEB_MOD%UTC
           HEB_DSPL%TAI     = HEB_MOD%TAI 
           HEB_DSPL%DIMS(1) = MALO%NLON
           HEB_DSPL%DIMS(2) = MALO%NLAT 
           HEB_DSPL%DIMS(3) = 3
           HEB_DSPL%DIMS(4) = L_CMP
           HEB_DSPL%DATA_OFFSET = HEB__HDS
           HEB_DSPL%ENDIAN      = HEB__LE
           HEB_DSPL%DATA_TRANSFORM = HEB__NONE
           HEB_DSPL%FILL_VALUE     = 1.0E15
           HEB_DSPL%OFFSET         = 0.0
           HEB_DSPL%SCALE_FACTOR   = 1.0 ! 3.D-6
           HEB_DSPL%DATA_COMPRESSION = HEB__NONE
           HEB_DSPL%SDS_NAME       = 'Site displacement '//FRQ_STR(1:I_LEN(FRQ_STR))
           HEB_DSPL%UNITS          = 'meter'
           HEB_DSPL%DATA_FORMAT    = HEB__R4
           HEB_DSPL%VALID_RANGE(1) =  -0.096D0
           HEB_DSPL%VALID_RANGE(2) =   0.096D0
           HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
!
           HEB_DSPL%FILE_NAME      = FILHEB(ID+1:)
           HEB_DSPL%HISTORY        = 'Processed using input numerical weather '// &
     &                               'model and input digital elevation map'
           HEB_DSPL%SOURCE         = 'Output of the regression model over surface pressure'
           HEB_DSPL%TITLE          = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%PROD_NAME      = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%INSTITUTION    = 'Astrogeo Center'
           HEB_DSPL%REFERENCES     = 'http://astrogeo.org/malo/'
           HEB_DSPL%VERSION_ID     = MALO__LABEL
      END IF
      IF ( IVRB == 8 ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
      IND_4 = 0
      TOT_LEN_I8 = 0
      DO 430 J3=1,L_FRQ
         IND3_SECT(1) =  1
         IND3_SECT(2) =  2
         IND4_SECT(1) = IND_HEB(J3)
         IND4_SECT(2) = IND_HEB(J3)
!
         CALL ERR_PASS ( IUER, IER )
         CALL READ_HEB_SECT ( FILIN_HEB, HEB_MOD, IND3_SECT, IND4_SECT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6218, IUER, 'MALO_LOD_MOD_LARGE', 'Error in '// &
     &            'reading the section of the input file '//FILIN_HEB )
              RETURN
         END IF
!
         DO 440 J4=1,NUM_CMP(J3)
            IND_4 = IND_4 + 1
            IF ( IVRB .GE. 2 ) THEN
                 WRITE ( 6, 110 ) J3, L_FRQ, J4, NUM_CMP(J3), IND_HEB(J3)
 110             FORMAT ( 'Wave ', I3, ' ( ', I3, ' )   Component ', I1, &
     &                    ' ( ', I2, ' )  Ind_Heb: ', I2, '  ',$ ) 
                 CALL FLUSH ( 6 )
            END IF
!
            ALLOCATE ( SPR_R8(L_LON,L_LAT), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*L_LON*L_LAT, STR )
                 CALL ERR_LOG ( 6219, IUER, 'MALO_LOD_MOD_LARGE',  'Error in '// &
     &                'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                ' bytes of dynamic memory for array SPR_R8' )
                 RETURN
            END IF
!
            DO 450 J5=1,L_LAT
               IND_LAT = J5
               IF ( IND_LAT > HEB_LS%DIMS(2) ) IND_LAT = HEB_LS%DIMS(2) 
               LAT_GDT = -P2I + (J5-1)*PI__NUM/(L_LAT-1)
               GE = MALO__ACC_EQU_WGS84*(1.D0 + MALO__GRV_LAT_WGS84*DSIN(LAT_GDT)**2)/ &
     &              DSQRT(1.D0 - (2.D0*MALO__FLAT_WGS84 - MALO__FLAT_WGS84**2)*DSIN(LAT_GDT)**2 )
               DO 460 J6=1,L_LON
                  IND_LON = J6
                  IF ( IND_LON > HEB_LS%DIMS(1) ) IND_LON = IND_LON - HEB_LS%DIMS(1) 
                  IF ( HEB_MOD%VAL(J6,J5,J4,1) > MALO__MAX_VAL ) THEN
                       SPR_R8(J6,J5) = 0.0D0
                       GOTO 460
                  END IF
                  IF ( CMPL_AMP_FRQ(J3) == (1.0D0, 0.0D0) ) THEN
!
! -------------------- Main wave
!
                       MOD_VAL = HEB_MOD%VAL(J6,J5,J4,1)
                     ELSE
!
! -------------------- Sidelobe wave that is constructed on the fly from 
! -------------------- the main component by multiplying by a complex admitance
!
                       IF ( J4 == 1 ) THEN
                            MOD_VAL = HEB_MOD%VAL(J6,J5,1,1)*REAL(CMPL_AMP_FRQ(J3)) - &
     &                                HEB_MOD%VAL(J6,J5,2,1)*IMAG(CMPL_AMP_FRQ(J3)) 
                          ELSE IF ( J4 == 2 ) THEN
                            MOD_VAL = HEB_MOD%VAL(J6,J5,1,1)*IMAG(CMPL_AMP_FRQ(J3)) + &
     &                                HEB_MOD%VAL(J6,J5,2,1)*REAL(CMPL_AMP_FRQ(J3)) 
                       END IF
                  END IF
                  IF ( HEB_MOD%UNITS == "meter" ) THEN
!
! -------------------- Convert sea height (m) to bottom pressure (Pa)
!
                       MOD_VAL = GE*MALO__SW_DENS*MOD_VAL 
                  END IF
                  IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
!
! -------------------- Surface pressure
!
                       SPR_R8(J6,J5) = MOD_VAL*HEB_LS%VAL(IND_LON,IND_LAT,1,1)
                    ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__LAKE ) THEN
!
! -------------------- Lake pressure
!
                       SPR_R8(J6,J5) = MOD_VAL*(1.D0 - HEB_LS%VAL(IND_LON,IND_LAT,1,1))
                    ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
!
! -------------------- Correct bottom pressure for ocean salinity
!
                       SPR_R8(J6,J5) = MOD_VAL*(1.D0 - HEB_LS%VAL(IND_LON,IND_LAT,1,1))* &
     &                                 MALO__SW_DENS/1000.0D0
                    ELSE 
                       SPR_R8(J6,J5) = MOD_VAL
                  END IF
 460           CONTINUE 
 450        CONTINUE 
            IF  ( IVRB == 6 .OR. IVRB == 7 ) THEN
                  SPR_R4 = SPR_R8
                  IER = -1
                  STR = 'Pressure: Wave '//OTID_WAV(IND_FRQ(J3))//' Comp: '//C_CMP(J4)
                  VAL_MIN =  1.0
                  VAL_MAX = -1.0
                  CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, SPR_R4, &
     &                               STR(1:I_LEN(STR)), 'Pa', VAL_MIN, VAL_MAX, &
     &                               '/tmp/boo', IER )
            END IF
!
            IF ( IVRB .EQ. 2 ) THEN
                 WRITE ( 6, * ) 'MLM Before SPHE-transform J3= ', INT2(J3), ' L_LAT= ', L_LAT
                 CALL FLUSH ( 6 )
                 CALL WALL_TIMER ( %VAL(0) ) 
            END IF
!
            ALLOCATE ( MALO%SPH(2,0:DEG,0:DEG,2,1), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*2*3*(DEG+1)**2, STR )
                 CALL ERR_LOG ( 6220, IUER, 'MALO_LOD_MOD_LARGE',  'Error in '// &
     &               'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for array MALO%SPH' )
                 RETURN
            END IF
!
! --------- Direct spherical harmonic trasform
!
            CALL ERR_PASS ( IUER, IER )
            CALL SPHE_DIR_2NN ( %VAL(FSH), L_LAT, SPR_R8, DEG, DEG, NORM, &
     &                          IPHS, MALO%SPH(1,0,0,1,1), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6221, IUER, 'MALO_LOD_MOD_LARGE', 'Error in '// &
     &               'an attempt to perform spherical transform of the '// &
     &               'pressure field' )
                 RETURN
            END IF
!
! --------- Multiply spherical harmonics by Love numbers
!
            DO 470 J7=0,DEG
               DO 480 J8=J7,DEG
                  DO 490 J9=1,2
!
! ------------------ NB: We transpose the 2nd and 3rd index since 
! ------------------ SPHE_INV_2NN_VEC expects (?) to find coefficients 
! ------------------ of the spherical harmonic transform in this order:
! ------------------ real/image, order, degree, radial/transverse
!
                     SPH_VAL = MALO%SPH(J9,J8,J7,1,1)
                     MALO%SPH(J9,J7,J8,MALO__H,1) = &
     &                        SPH_VAL* 3.0D0*MALO%LOVE(J8,MALO__H)/(2*J8+1)/ &
     &                                 (MALO__DENS*MALO__GRAV)
                     MALO%SPH(J9,J7,J8,MALO__L,1) = &
     &                        SPH_VAL* 3.0D0*MALO%LOVE(J8,MALO__L)/(2*J8+1)/ &
     &                                 (MALO__DENS*MALO__GRAV)
!
! ------------------ Not sure... Let us define spherical harminics both ways
!
                     MALO%SPH(J9,J8,J7,MALO__H,1) = MALO%SPH(J9,J7,J8,MALO__H,1) 
                     MALO%SPH(J9,J8,J7,MALO__L,1) = MALO%SPH(J9,J7,J8,MALO__L,1) 
 490              CONTINUE 
 480           CONTINUE 
 470        CONTINUE 
!
            IF ( LOAD_TYPE == 'load_d1' ) THEN
!
! ------------- In a case if compute correction for D1-term, we 
! ------------- unscale spherical harmonic of degree one.
!
                MALO%SPH(1:2,0:1,1,MALO__H,1) = 1.0D0/MALO%LOVE(1,MALO__H)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__H,1)
                MALO%SPH(1:2,0:1,1,MALO__L,1) = 1.0D0/MALO%LOVE(1,MALO__L)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__L,1)
                MALO%SPH(1:2,0:2,2,MALO__H,1) = 0.0D0
                MALO%SPH(1:2,0:2,2,MALO__L,1) = 0.0D0
!
                MALO%SPH(1:2,1,0,MALO__H,1)  = MALO%SPH(1:2,0,1,MALO__H,1)  
                MALO%SPH(1:2,1,0,MALO__L,1)  = MALO%SPH(1:2,0,1,MALO__L,1)  
            END IF
            IF ( IVRB .EQ. 2 ) THEN
                 WRITE ( 6, * ) 'MLM After  SPHE-transform J3= ', INT2(J3), ' DEG= ', DEG
                 CALL FLUSH ( 6 )
            END IF
            IF ( IVRB .EQ. 3 ) THEN
                 WRITE ( 6, * ) 'MALO%NLON = ', MALO%NLON, ' MLON= ', MLON
                 WRITE ( 6, * ) 'MALO%NLAT = ', MALO%NLAT, ' MLAT= ', MLAT
                 WRITE ( 6, * ) 'DEG       = ', DEG,       ' MD=   ', MD
            END IF
!
            ALLOCATE ( DSP_ARR3_R8(MLON,MLAT,3), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*3*MLON*(MLAT-1), STR )
                 CALL ERR_LOG ( 6222, IUER, 'MALO_LOD_MOD_LARGE', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &               'memory for array DSP_ARR3_R8' )
                 RETURN 
            END IF
!
            IF ( LOAD_TYPE == 'load' ) THEN
!
! -------------- Inverse vector harmonic transform
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, MD, MALO%SPH, &
     &                                   MLAT, DSP_ARR3_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      write ( 6, * ) 'DEG= ', DEG, ' NALO%NLAT = ', MLAT 
                      CALL ERR_LOG ( 6223, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &                    'in an attempt to perform inverse vector spherical '// &
     &                    'harmonic transform' )
                      RETURN 
                 END IF
               ELSE IF ( LOAD_TYPE == 'load_d1' ) THEN
!
! -------------- Inverse vector harmonic transform
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, 2, MALO%SPH, &
     &                                   MLAT, DSP_ARR3_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6224, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &                    'in an attempt to perform inverse vector spherical '// &
     &                    'harmonic transform' )
                      RETURN 
                 END IF
            END IF
!
            DEALLOCATE ( MALO%SPH )
            ALLOCATE ( DSP_ARR3_R4(MALO%NLON,MALO%NLAT,3), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*3*MALO%NLON*(MALO%NLAT-1), STR )
                 CALL ERR_LOG ( 6225, IUER, 'MALO_LOD_MOD_LARGE', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &               'memory' )
                 RETURN 
            END IF
!
            IF ( MALO%CONF%SC_FILE .EQ. 'NONE' ) THEN
!
! -------------- No sampling correction was requested
!
                 DO 4100 J10=1,MALO%NLAT
                    DO 4110 J11=1,MALO%NLON 
                       DSP_ARR3_R4(J11,J10,1:3) = DSP_ARR3_R8(1+MR*(J11-1),1+MR*(J10-1),1:3)
 4110               CONTINUE 
 4100            CONTINUE 
              ELSE
!
! -------------- Sampling correction was requested
!
                 DO 4120 J12=1,MALO%NLAT-1
                    DO 4130 J13=1,MALO%NLON 
                       DSP_ARR3_R4(J13,J12,1:3) = DSP_ARR3_R8(1+MR*(J13-1),1+MR*(J12-1),1:3) &
     &                             + SPR_R8(J13,J12)*HEB_SC%VAL(J13,J12,1:3,1) 
 4130               CONTINUE 
 4120            CONTINUE 
!
! -------------- Keep in mind that the 2nd dimension SPR_R8 is defined from
! -------------- 1 to L_LAT = MALO%NLAT - 1. Just skip sampling correction
! -------------- for the north pole
!
                 DO 4140 J14=1,MALO%NLON 
                    DSP_ARR3_R4(J14,MALO%NLAT,1:3) = DSP_ARR3_R8(1+MR*(J14-1),1+MR*(MALO%NLAT-1),1:3)
 4140            CONTINUE 
            END IF
! 
            IF ( IVRB == 7 ) THEN
                 STR = 'Loading Up: '//OTID_WAV(IND_FRQ(J3))//' Comp: '//C_CMP(J4)
                 IER = -1
                 DSP_ARR3_R4 = 1.D3*DSP_ARR3_R4
                 VAL_MIN =  1.0
                 VAL_MAX = -1.0
                 CALL PLOT_GRID_R4 ( 1, 7, 0, 1, MALO%NLON, MALO%NLAT, DSP_ARR3_R4, &
     &                               STR(1:I_LEN(STR)), 'mm', VAL_MIN, VAL_MAX, &
     &                               '/tmp/boo', IER )
                 DSP_ARR3_R4 = 1.D-3*DSP_ARR3_R4
            END IF
            IF ( IND_4 == 1 ) THEN
!
! -------------- If the output file exists, remove it
!
                 INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
                 IF ( LEX ) CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
!
! -------------- Write down the header of the output displacement file
!
                 CALL ERR_PASS ( IUER, IER )
                 HEB_DSPL%STATUS  = HEB__HDON
                 CALL WRITE_HEB ( HEB_DSPL, %VAL(0), FILOUT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6226, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &                    'in an attempt to write displacement into the output '// &
     &                    'file '//FILOUT )
                      RETURN 
                 END IF
                 LOAD_MIN = MINVAL(DSP_ARR3_R4)
                 LOAD_MAX = MAXVAL(DSP_ARR3_R4)
                 TOT_LEN_I8 = HEB__HDS_V1
               ELSE 
!
! -------------- Update min and max values
!
                 LOAD_MIN = MIN ( LOAD_MIN, MINVAL(DSP_ARR3_R4) )
                 LOAD_MAX = MAX ( LOAD_MAX, MAXVAL(DSP_ARR3_R4) )
            END IF
!
! --------- Open the output file in the append mode
!
            CALL ERR_PASS ( IUER, IER )
            CALL BINF_OPEN ( FILOUT, 'APPEND', LUN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6227, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &               'in an attempt to re-open the output '// &
     &               'file '//FILOUT )
                 RETURN 
            END IF
!
! --------- Write the slice of the displacement field for this component,
! --------- this wave into the output file
!
            OFFS_I8 = TOT_LEN_I8
            LEN_I8  = SIZEOF(DSP_ARR3_R4)
            TOT_LEN_I8 = TOT_LEN_I8 + LEN_I8
            CALL ERR_PASS ( IUER, IER )
            CALL BIG_WRITE ( LUN, OFFS_I8, LEN_I8, DSP_ARR3_R4, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IND_4, STR )
                 CALL ERR_LOG ( 6228, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &               'in an attempt to append displacement for the '// &
     &                STR(1:I_LEN(STR))//' th wave to the end of output '// &
     &               'file '//FILOUT )
                 RETURN 
            END IF
!
! --------- Close the output file
!
            CALL ERR_PASS ( IUER, IER )
            CALL BINF_CLOSE ( LUN, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6229, IUER, 'MALO_LOD_MOD_LARGE', &
     &               'Failure in an attempt to close the output '// &
     &               'file '//FILOUT )
                 RETURN 
            END IF
            IF ( IVRB == 8 ) THEN
                 CALL WALL_TIMER ( STR ) 
                 WRITE ( 6, '(A,A)' ) 'Wall time: ', STR(11:20)
                 CALL WALL_TIMER ( %VAL(0) ) 
            END IF
!
            DEALLOCATE ( SPR_R8      )
            DEALLOCATE ( DSP_ARR3_R8 )
            DEALLOCATE ( DSP_ARR3_R4 )
 440     CONTINUE 
         DEALLOCATE ( HEB_MOD%VAL )
 430  CONTINUE 
!
! --- Update the header section. Open the output file once again
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'APPEND', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6230, IUER, 'MALO_LOD_MOD_LARGE', 'Failure '// &
     &         'in an attempt to re-open the output '// &
     &         'file '//FILOUT )
           RETURN 
      END IF
!
! --- Position to the beginning of the output file
!
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. 0 ) THEN
           write ( 6, * ) 'offset_ret= ', offset_ret ! %%%
           CALL ERR_LOG ( 6231, IUER, 'MALO_LOD_MOD_LARGE', &
     &         'Failure in an attempt to position to the beginning '// &
     &         'the output file '//FILOUT )
           RETURN 
      END IF
!
! --- Re-write the header
!
      HEB_DSPL%MIN_VALUE = LOAD_MIN
      HEB_DSPL%MAX_VALUE = LOAD_MAX
      HEB_DSPL%STATUS  = HEB__HDON
      CALL WRITE_HEB ( HEB_DSPL, %VAL(0), FILOUT, IER )
!
! --- Position to the end of the output file
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(TOT_LEN_I8), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. TOT_LEN_I8 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6232, IUER, 'BIG_WRITE', 'Failure in '// &
     &         'position the file into the end of the output file' )
           RETURN 
      END IF
!
! --- Finally, close the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6233, IUER, 'MALO_LOD_MOD_LARGE', &
     &         'Failure in an attempt to close the output '// &
     &         'file '//FILOUT )
           RETURN 
      END IF
!
! --- Deallocate memory
!
      IF ( IVRB .EQ. 6 .OR. IVRB .EQ. 7 ) DEALLOCATE ( SPR_R4 )
      CALL FREE_HEB ( HEB_SC )
      CALL FREE_HEB ( HEB_LS )
      CALL FREE_HEB ( HEB_MOD )
!
      IF ( IVRB .GE. 2 ) THEN
!
! -------- Write down the good bye message
!
           WRITE ( 6, '(A,A,A)' ) 'MALO_LOADING_MODEL: ', GET_CDATE(),  &
     &             ' Finished loading compution'
           WRITE ( 6, '(A)' ) 'MALO_LOADING_MODEL output file name: '// &
     &                        FILOUT(1:I_LEN(FILOUT))
           CALL CLRCH  ( STR )
           CALL IINCH8 ( TOT_LEN_I8, STR )
           WRITE ( 6, '(A,A,A)' ) 'MALO_LOADING_MODEL output file size: ', STR(1:I_LEN(STR)), &
     &                            ' bytes'
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_LOD_MOD_LARGE  !#!#
