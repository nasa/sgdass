      PROGRAM    LOADING_SPL_HEB_TO_STA_LAUNCH
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
      CALL LOADING_SPL_HEB_TO_STA()
      END  PROGRAM  LOADING_SPL_HEB_TO_STA_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LOADING_SPL_HEB_TO_STA()
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_SPL_HEB_TO_EPH interpolates the displacements     *
! *   caused by mass loading for the list of stations. The input for     *
! *   the computation are the coefficients of expansion of the global    *
! *   displacement field into 2D B-spline basis performed with program   *
! *   loading_nc_to_spl_heb. The results are written in EPHEDISP format. *
! *                                                                      *
! *   This program assumes the input displacement file name obeys        *
! *   the following convention: ttt_ooooooo_YYYYMMDD_hhmm.heb      or    *
! *                             ttt_ooooooo_YYYYMMDD_hhmm.heb.bz2, where *
! *                                                                      *
! *   ttt     -- model type: atm, lws, or nto                            *
! *   ooooooo -- model name: merra2, geosfp or geosfpit                  *
! *   YYYY    -- year   as an integer number                             *
! *   MM      -- month  as an integer number                             *
! *   DD      -- day    as an integer number                             *
! *   hh      -- hour   as an integer number                             *
! *   mm      -- minute as an integer number                             *
! *                                                                      *
! *   Usage: loading_spl_heb_to_eph splheb_file [splheb_file2]           *
! *                                 station_file eph_file [ivrb]         *
! *                                                                      *
! *          splheb-file  -- file B-spline coefficients of the 2D        *
! *                          displacements field in HEB-format;          *
! *                                                                      *
! *          splheb-file  -- file B-spline coefficients of the 2D        *
! *                          displacements field in HEB-format;          *
! *                                                                      *
! *          station_file -- list of stations with names and Cartesian   *
! *                          coordinates in SITLIST format.              *
! *                                                                      *
! *          eph_file     -- name of the output file with station        *
! *                          displacements in EPHEDISP (ascii) format.   *
! *                                                                      *
! *          ivrb         -- verbosity level:                            *
! *                          0 -- silent;                                *
! *                          1 -- name of the output file is printed;    *
! *                          2 -- timing information is collected and    *
! *                               printed.                               *
! *                                                                      *
! * # 06-JUN-2016 LOADING_SPL_HEB_TO_EPH v3.2 (c) L.Petrov 11-JUN-2017 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo_local.i'
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE     ( HEB__TYPE  ) :: HEB
      CHARACTER  FILIN(2)*128, FILSTA*128, FILOUT*128, WAV_NAM*4, STR*128
      CHARACTER  FILDSC*128, FILCOM*128, &
     &           FILFMT*128, TMPDIR*128, SYSNAME*128, NODENAME*128, &
     &           HARDWARE*128, PID_STR*5, STR_TIME_INTR*28, &
     &           STR_TIME_READ*28, STR_TIME_WRIT*28, LOAD_TYPE*16, &
     &           LOAD_MODEL*64
      REAL*4,    ALLOCATABLE :: DSPL_2D_R4(:,:,:)
      REAL*4,    ALLOCATABLE :: LON(:), LAT(:), BSPL_2D(:,:,:)
      REAL*8,    ALLOCATABLE :: DSPL_STA(:,:,:,:)
      REAL*8     TAI
      LOGICAL*1  LEX, FL_NC, FL_HEB, FL_TIMER
      CHARACTER  PRG_NAME*48, C_FRQ(MALO__MWAV)*4
      INTEGER*4  NLON, NLAT, NOVL, IVEC, ICMP, IFRQ, MJD, LC, LS, IUER 
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IL, LFIL, IND(2,MIND), &
     &           LIND, NCMP, IND_LON, IND_LAT, IND3_SECT(2), IND4_SECT(2), &
     &           IND1, IND2, IVRB, L_FRQ, IND_FRQ(MALO__MWAV), NUM_CMP(MALO__MWAV), &
     &           IX_FRQ(MALO__MWAV), IX_CMP(MALO__MWAV), I_FRQ
      INTEGER*4, EXTERNAL :: ADD_CLIST, GETPID, ILEN, I_LEN, IXMN8, IXMN4, &
     &                       LTM_DIF, MULTI_INDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4
!
      IVRB = 2
      PRG_NAME = 'loading_spl_heb_to_eph version 3.0 of 2017.06.01' 
      FL_TIMER = .FALSE.
      CALL CLRCH ( FILIN(1) )
      CALL CLRCH ( FILIN(2) )
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: loading_spl_heb_to_eph splheb_file [splheb_file2] '// &
     &                    'station_file eph_file|har_file [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN(1)  )
           CALL GETARG ( 2, FILSTA  )
           LFIL = 1
           IL = ILEN(FILSTA)
           IF ( IL > 8 ) THEN
                IF ( FILSTA(IL-3:IL) == '.heb' .OR. FILSTA(IL-7:IL) == '.heb.bz2' ) THEN
!
! ------------------ The second argument is the secondary heb file
!
                     CALL GETARG ( 2, FILIN(2) )
                     CALL GETARG ( 3, FILSTA   )
                     LFIL = 2
                END IF
           END IF
!
           IF ( ILEN(FILIN(2)) == 0 ) THEN
                CALL GETARG ( 3, FILOUT )
                IF ( IARGC() .GE. 4 ) THEN
                     CALL GETARG ( 4, STR )
                     CALL CHIN   ( STR, IVRB ) 
                   ELSE 
                     IVRB = 0
                END IF
              ELSE
                CALL GETARG ( 4, FILOUT )
                IF ( IARGC() .GE. 5 ) THEN
                     CALL GETARG ( 5, STR )
                     CALL CHIN   ( STR, IVRB ) 
                   ELSE 
                     IVRB = 0
                END IF
           END IF
      END IF
      IF ( IVRB .EQ. 3 ) THEN
           FL_TIMER = .TRUE.
      END IF
#ifdef LINUX
       TMPDIR = '/dev/shm'
#else
       TMPDIR = '/tmp'
#endif
!
! --- File with EPHEDISP format description
!
      IF ( ILEN(FILOUT) < 6 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6701, IUER, 'LOADING_SPL_HEB_TO_STA', 'Too short name '// &
     &         'the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IL = ILEN(FILOUT)
      IF ( FILOUT(IL-3:IL) == '.eph' ) THEN
           FILFMT = MALO_SHARE//'/ephedisp_format.txt'
        ELSE IF ( FILOUT(IL-3:IL) == '.hps' ) THEN
           FILFMT = MALO_SHARE//'/harpos_format.txt'
        ELSE
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'LOADING_SPL_HEB_TO_STA', 'Unsupported extension '// &
     &         'of the output file '//TRIM(FILOUT)//', while .hps or .eph were expected' )
           CALL EXIT ( 1 )
      END IF
!
! --- Get PID
!
      CALL INCH     ( GETPID(), PID_STR )
      CALL CHASHR   (           PID_STR )
      CALL BLANK_TO_ZERO (      PID_STR )
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6703, IUER, 'LOADING_SPL_HEB_TO_STA', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize MALO object
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'LOADING_SPL_HEB_TO_STA', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read and parse input file with station names
!
      IUER = -1
      CALL MALO_INP_STA ( MAL(1), FILSTA, 0.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6705, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &         'an attempt to read the header of the input file '//FILIN(1) )
           CALL EXIT ( 1 )
      END IF
!
!@      CALL WALL_TIMER ( %VAL(0) ) 
      DO 410 J1=1,LFIL
         INQUIRE ( FILE=FILIN(J1), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              IF ( J1 == 1 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6706, IUER, 'LOADING_SPL_HEB_TO_STA', 'The first '// &
     &                 'input file with spline coefficients '//TRIM(FILIN(J1))// &
     &                 ' was not found' )
                   CALL EXIT ( 1 )
                 ELSE
                   IUER = -1
                   CALL ERR_LOG ( 6707, IUER, 'LOADING_SPL_HEB_TO_STA', 'The second '// &
     &                 'input file with spline coefficients '//TRIM(FILIN(J1))// &
     &                 ' was not found' )
                   CALL EXIT ( 1 )
              END IF
         END IF
!
! ------ Read input HEB file with B-spline expansion coefficients
!
         IUER = -1
         CALL READ_HEB_HEADER ( FILIN(J1), HEB, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6708, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &           'an attempt to read the header of the input file '//FILIN(J1) )
              CALL EXIT ( 1 )
         END IF
         NCMP = HEB%DIMS(4)
         HEB%TAI = HEB%UTC
!
         IF ( NCMP > 1 ) THEN
              L_FRQ   = 0
              IND_FRQ = 0
              NUM_CMP = 0
              DO 420 J2=1,NCMP
                 IND1 = MULTI_INDEX ( J2,   HEB%SDS_NAME, '|' )
                 IND2 = MULTI_INDEX ( J2+1, HEB%SDS_NAME, '|' )
                 IF ( IND1 < 1 .OR. IND2 < 1 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J2, STR )
                      WRITE ( 6, * ) 'HEB%SDS_NAME = ', TRIM(HEB%SDS_NAME)
                      IUER = -1
                      CALL ERR_LOG ( 6709, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &                    'parsing SDS_NAME string in the input file '//TRIM(FILIN(J1))// &
     &                    ' no code for the '//TRIM(STR)//' th component was found' )
                      CALL EXIT ( 1 )
                 END IF
                 I_FRQ = LTM_DIF ( 1, MOT, OTID_WAV, HEB%SDS_NAME(IND1+1:IND1+4) )
                 IF ( I_FRQ < 1 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J2, STR )
                      IUER = -1
                      CALL ERR_LOG ( 6710, IUER, 'LOADING_SPL_HEB_TO_STA', 'Error in '// &
     &                    'parsing SDS_NAME stringe in the input file '//TRIM(FILIN(J1))// &
     &                    ' unknown tidal wave code '//HEB%SDS_NAME(IND1+1:IND1+4)// &
     &                    ' for the '//TRIM(STR)//' th component was found. Supported '// &
     &                    'codes are listed in OTID_WAV file in malo.i' )
                      CALL EXIT ( 1 )
                 END IF
!
                 IUER = -1
                 IX_FRQ(J2) = ADD_CLIST ( MOT, L_FRQ, C_FRQ, HEB%SDS_NAME(IND1+1:IND1+4), IUER )
                 IND_FRQ(L_FRQ) = LTM_DIF ( 1, MOT, OTID_WAV, HEB%SDS_NAME(IND1+1:IND1+4) )
!
                 IF ( HEB%SDS_NAME(IND2-1:IND2-1) == 'c' ) THEN
                      NUM_CMP(L_FRQ) = 1
                      IX_CMP(J2)  = 1
                   ELSE IF ( HEB%SDS_NAME(IND2-1:IND2-1) == 's' ) THEN
                      NUM_CMP(L_FRQ) = 2
                      IX_CMP(J2)  = 2
                   ELSE
                      CALL CLRCH ( STR )
                      CALL INCH  ( J2, STR )
                      IUER = -1
                      CALL ERR_LOG ( 6711, IUER, 'LOADING_SPL_HEB_TO_STA', 'Error in '// &
     &                    'parsing SDS_NAME stringe in the input file '//TRIM(FILIN(J1))// &
     &                    ' unknown comonent code '//HEB%SDS_NAME(IND2-1:IND2-1)// &
     &                    ' for the '//TRIM(STR)//' th component was found. Supported '// &
     &                    'codes are c or s' )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( IVRB .GE. 3 ) THEN
                      WRITE ( 6, 210 ) J2, HEB%SDS_NAME(IND1+1:IND1+4), IX_FRQ(J2), &
     &                                 IND_FRQ(L_FRQ), IX_CMP(J2)
 210                  FORMAT ( I2, 1X, 'Wave ', A, ' IX_frq: ', I2, ' Ind_frq: ', I3, &
     &                        ' IX_cmp: ', I1 )
                 END IF
 420          CONTINUE 
            ELSE
              L_FRQ = 1
              NUM_CMP(1) = 1
              IX_FRQ(1)  = 1
              IX_CMP(1)  = 1
         END IF
!
! ------ Parse the first comment and extract there dimensions and longitude overlap
!
         CALL EXWORD ( HEB%COMMENT(1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( LIND < 6 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6712, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &            'internal control: The first comment has less than 6 words: '// &
     &             HEB%COMMENT(1) )
             CALL EXIT ( 1 )
         END IF
!
         CALL CHIN   ( HEB%COMMENT(1)(IND(1,2):IND(2,2)), NLON )
         CALL CHIN   ( HEB%COMMENT(1)(IND(1,4):IND(2,4)), NLAT )
         CALL CHIN   ( HEB%COMMENT(1)(IND(1,6):IND(2,6)), NOVL )
         IF ( NOVL .NE. 2 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6713, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &           'internal control: parameter NOVL should be 2, but got '// &
     &            HEB%COMMENT(1) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Extract model type and model name
!
         CALL EXWORD ( HEB%COMMENT(2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( LIND < 3 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6714, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &            'internal control: The second comment has less than 3 words: '// &
     &             HEB%COMMENT(2) )
              CALL EXIT ( 1 )
         END IF
         LOAD_TYPE  = HEB%COMMENT(2)(IND(1,3):IND(2,3))
!
         CALL EXWORD ( HEB%COMMENT(3), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( LIND < 3 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6715, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &            'internal control: The third comment has less than 3 words: '// &
     &             HEB%COMMENT(2) )
              CALL EXIT ( 1 )
         END IF
         LOAD_MODEL = HEB%COMMENT(3)(IND(1,3):IND(2,3))
         IF ( LOAD_MODEL == 'omct'  ) LOAD_MODEL = 'omct05'
         IF ( LOAD_MODEL == 'mpiom' ) LOAD_MODEL = 'mpiom06'
!
! ------ Build the name of data source descrption and the loading computation 
! ------ description depending on model name and model type
!
         IF ( LOAD_TYPE == 'atm' ) THEN
              IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) == 0 ) THEN
                   FILCOM = MALO_SHARE//'/atm_description.txt'
                 ELSE IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) > 0 ) THEN
                   FILCOM = MALO_SHARE//'/d1_atm_description.txt'
                 ELSE
                   FILCOM = MALO_SHARE//'/cf_atm_description.txt'
              END IF
!            
              IF ( LOAD_MODEL == 'geosfpit' ) THEN
                   FILDSC = MALO_SHARE//'/atm_geosfpit_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'geosfp' ) THEN
                   FILDSC = MALO_SHARE//'/atm_geosfp_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'merra2' ) THEN
                   FILDSC = MALO_SHARE//'/atm_merra2_data_source.txt'
                 ELSE 
                   IUER = -1
                   CALL ERR_LOG ( 6716, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &                 'internal control: unknown model '//TRIM(LOAD_MODEL)// &
     &                 'for data type '//LOAD_TYPE )
                   CALL EXIT ( 1 )
              END IF
           ELSE IF ( LOAD_TYPE == 'lws' .OR. LOAD_TYPE == 'gra' ) THEN
              IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) == 0 ) THEN
                   FILCOM = MALO_SHARE//'/lws_description.txt'
                 ELSE IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) > 0 ) THEN
                   FILCOM = MALO_SHARE//'/d1_lws_description.txt'
                 ELSE
                   FILCOM = MALO_SHARE//'/cf_lws_description.txt'
              END IF
!
              IF ( LOAD_MODEL == 'geosfpit' ) THEN
                   FILDSC = MALO_SHARE//'/lws_geosfpit_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'geosfp' ) THEN
                   FILDSC = MALO_SHARE//'/atm_geosfp_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'merra2' ) THEN
                   FILDSC = MALO_SHARE//'/lws_merra2_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'ward' ) THEN
                   FILDSC = MALO_SHARE//'/lws_ward_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'grace' .OR. LOAD_MODEL == 'GRACE' ) THEN
                   FILDSC = MALO_SHARE//'/grace_mascon_data_source.txt'
                 ELSE 
                   IUER = -1
                   CALL ERR_LOG ( 6717, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &                 'internal control: unknown model '//TRIM(LOAD_MODEL)// &
     &                 ' for data type '//LOAD_TYPE )
                   CALL EXIT ( 1 )
              END IF
           ELSE IF ( LOAD_TYPE == 'nto' ) THEN
              FILCOM = MALO_SHARE//'/nto_description.txt'
              IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) == 0 ) THEN
                   FILCOM = MALO_SHARE//'/nto_description.txt'
                 ELSE IF ( LFIL == 1 .AND. INDEX ( FILIN(J1), '_d1_' ) > 0 ) THEN
                   FILCOM = MALO_SHARE//'/d1_nto_description.txt'
                 ELSE
                   FILCOM = MALO_SHARE//'/cf_nto_description.txt'
              END IF
!
              IF ( LOAD_MODEL == 'omct05' ) THEN
                   FILDSC = MALO_SHARE//'/nto_omct05_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'mpiom06' ) THEN
                   FILDSC = MALO_SHARE//'/nto_mpiom06_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'mpiom07' ) THEN
                   FILDSC = MALO_SHARE//'/nto_mpiom07_data_source.txt'
                 ELSE 
                   IUER = -1
                   CALL ERR_LOG ( 6718, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &                 'internal control: unknown model '//TRIM(LOAD_MODEL)// &
     &                 ' for data type '//LOAD_TYPE )
                   CALL EXIT ( 1 )
              END IF
           ELSE IF ( LOAD_TYPE == 'toc' ) THEN
              FILCOM = MALO_SHARE//'/toc_description.txt'
              IF ( LOAD_MODEL == 'got48' ) THEN
                   FILDSC = MALO_SHARE//'/toc_got48_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'fes2012' ) THEN
                   FILDSC = MALO_SHARE//'/toc_fes2012_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'got410c' ) THEN
                   FILDSC = MALO_SHARE//'/toc_got410c_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'fes2014b' ) THEN
                   FILDSC = MALO_SHARE//'/toc_fes2014b_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'equil01' ) THEN
                   FILDSC = MALO_SHARE//'/toc_equil01_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'equil02' ) THEN
                   FILDSC = MALO_SHARE//'/toc_equil02_data_source.txt'
                 ELSE IF ( LOAD_MODEL == 'psi1' ) THEN
                   FILDSC = MALO_SHARE//'/toc_ray_psi1_data_source.txt'
                 ELSE 
                   IUER = -1
                   CALL ERR_LOG ( 6719, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &                 'internal control: unknown model '//TRIM(LOAD_MODEL)// &
     &                 ' for data type '//LOAD_TYPE )
                   CALL EXIT ( 1 )
              END IF
           ELSE 
              IUER = -1
              CALL ERR_LOG ( 6720, IUER, 'LOADING_SPL_HEB_TO_STA', 'Trap of '// &
     &            'internal control: unsuppoted LOAD_TYPE: '//LOAD_TYPE )
              CALL EXIT ( 1 )
        END IF

        IF ( J1 == 1 ) THEN
!
! ---------- Allocate memory for arrays with longitude and longitude, 
! ---------- and station displacements
!
             ALLOCATE ( LON(NLON+NOVL), STAT=IUER )
             IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH  ( STR   )
                  CALL IINCH8 ( INT8(NLON+NOVL), STR )
                  IUER = -1
                  CALL ERR_LOG ( 6721, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure to '// &
     &                'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &                'for array LON' )
                  CALL EXIT ( 1 )
            END IF
!
            ALLOCATE ( LAT(NLAT), STAT=IUER )
            IF ( IUER .NE. 0 ) THEN
                 CALL CLRCH  ( STR   )
                 CALL IINCH8 ( INT8(NLAT), STR )
                 IUER = -1
                 CALL ERR_LOG ( 6722, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &               'for array LON' )
                 CALL EXIT ( 1 )
            END IF
!
            ALLOCATE ( DSPL_STA(3,MAL(1)%NSTA,L_FRQ,2), STAT=IUER )
            IF ( IUER .NE. 0 ) THEN
                 CALL CLRCH  ( STR   )
                 CALL IINCH8 ( INT8(3)*INT8(8)*INT8(MAL(1)%NSTA)*INT8(NCMP), STR )
                 IUER = -1
                 CALL ERR_LOG ( 6723, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &               'for array LON' )
                 CALL EXIT ( 1 )
            END IF
            DSPL_STA = 0.0D0
!
!@            CALL WALL_TIMER ( STR_TIME_READ ) 
!@            CALL WALL_TIMER ( %VAL(0) ) 
!
! --------- Generate arrys of longitued nad latitude in knots of the grid
!
            DO 430 J3=1,NLON+NOVL
               LON(J3) = 0.0 + (J3-1)*PI2/NLON
 430        CONTINUE
!
            DO 440 J4=1,NLAT
               LAT(J4) = -P2I + (J4-1)*PI__NUM/(NLAT-1)
 440        CONTINUE
         END IF
         DO 450 J5=1,NCMP
            IF ( IVRB .GE. 2 ) THEN
                 WRITE ( 6, 120 ) J5, NCMP, J1, LFIL
 120             FORMAT ( 'Processing the ', I3, ' ( ', I3, ' ) -th harmonic of the ', &
     &                    I1, ' ( ', I1, ' ) run' )
                 CALL FLUSH ( 6 )
            END IF
            IF ( J5 == 1 ) THEN
                 IUER = -1
                 CALL READ_HEB ( FILIN(J1), HEB, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6724, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &                    'an attempt to read the input file '//FILIN(J1) )
                      CALL EXIT ( 1 )
                 END IF
               ELSE
                 IND3_SECT(1) =  1
                 IND3_SECT(2) =  3
                 IND4_SECT(1) = J5
                 IND4_SECT(2) = J5
!
                 IUER = -1
                 CALL READ_HEB_SECT ( FILIN(J1), HEB, IND3_SECT, IND4_SECT, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J5, STR )
                      IUER = -1
                        CALL ERR_LOG ( 6725, IUER, 'LOADING_SPL_HEB_TO_STA', &
     &                    'Failure to read the '//STR(1:I_LEN(STR))// &
     &                    ' th section of the input heb file '//FILIN(J1) )
                      CALL EXIT ( 1 )
                 END IF
            END IF
!
! --------- Compute displacements by interpolation
!
            DO 460 J6=1,MAL(1)%NSTA
               IND_LON = IXMN4 ( NLON+NOVL, LON, SNGL(MAL(1)%STA(J6)%LON)     )
               IND_LAT = IXMN4 ( NLAT,      LAT, SNGL(MAL(1)%STA(J6)%LAT_GDT) )
               DO 470 J7=1,3
                  DSPL_STA(J7,J6,IX_FRQ(J5),IX_CMP(J5)) = DSPL_STA(J7,J6,IX_FRQ(J5),IX_CMP(J5)) + &
     &                              VAL_2D_BSPL4 ( SNGL(MAL(1)%STA(J6)%LON), SNGL(MAL(1)%STA(J6)%LAT_GDT), &
     &                                             NLON+NOVL, NLAT, MALO__MDEG, IND_LON, IND_LAT, &
     &                                             LON, LAT, HEB%VAL(1,1,J7,1) )
 470           CONTINUE 
 460        CONTINUE 
 450     CONTINUE 
!
         MAL(1)%NTIM = 1
         MAL(1)%MJD_BEG = HEB%MJD
         MAL(1)%TAI_BEG = HEB%TAI
         MAL(1)%MJD_END = HEB%MJD
         MAL(1)%TAI_END = HEB%TAI
!@         CALL WALL_TIMER ( STR_TIME_INTR ) 
 410  CONTINUE 
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'IND_FRQ= ', INT2(IND_FRQ(1:L_FRQ))
           WRITE ( 6, * ) 'NUM_CMP= ', INT2(NUM_CMP(1:L_FRQ))
      END IF
!
!@      CALL WALL_TIMER ( %VAL(0) ) 
      IF ( FILOUT(IL-3:IL) == '.eph' ) THEN
!
! -------- Write displacements in EPHEDISP format
!
           IUER = -1
           CALL MALO_EPHEDISP_WRITE ( MAL(1), DSPL_STA, MALO__LABEL, FILOUT, &
     &                                FILDSC, FILCOM, FILFMT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6726, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &              'writing the output file in EPHEDISP format' )
                CALL EXIT ( 1 )
           END IF
        ELSE IF ( FILOUT(IL-3:IL) == '.hps' ) THEN
           MAL(1)%CONF%LOA_FINAM_DESCR  = FILDSC
           MAL(1)%CONF%LOA_FINAM_COMM   = FILCOM
           MAL(1)%CONF%HARPOS_FINAM_FMT = FILFMT
           IUER = -1
           CALL MALO_HARPOS_WRITE ( MAL(1)%NSTA, L_FRQ, IND_FRQ, NUM_CMP, &
     &                              DSPL_STA, MAL(1), PRG_NAME, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6727, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &              'writing the output file in HARPOS format' )
                CALL EXIT ( 1 )
           END IF
      END IF
      CALL WALL_TIMER ( STR_TIME_WRIT ) 
!
      IF ( FL_TIMER ) THEN
           WRITE ( 6, '(A)' ) 'Wall time reading:       '//TRIM(STR_TIME_READ)
           WRITE ( 6, '(A)' ) 'Wall time interpolating: '//TRIM(STR_TIME_INTR)
           WRITE ( 6, '(A)' ) 'Wall time writing:       '//TRIM(STR_TIME_WRIT)
      END IF
      DEALLOCATE ( HEB%VAL )
      DEALLOCATE ( LAT     )
      DEALLOCATE ( LON     )
!
      IL = ILEN(FILOUT)
      IF ( FILOUT(IL-6:IL) == '_s1.hps' ) THEN
           IUER =  -1
           CALL HARPOS_KEEP_S1_ONLY ( FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6728, IUER, 'LOADING_SPL_HEB_TO_STA', 'Failure in '// &
     &              'removing all harmonics , except S1, in the output file '// &
     &               FILOUT )
                CALL EXIT ( 1 )
           END IF
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'LOADING_SPL_HEB_TO_STA: retained only S1 harmonics'
           END IF
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'LOADING_SPL_HEB_TO_STA: finished. Written output file: '//TRIM(FILOUT)
      END IF
!
      CALL EXIT  ( 0 )
      END  SUBROUTINE  LOADING_SPL_HEB_TO_STA  !#!#
      
