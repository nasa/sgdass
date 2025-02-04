      PROGRAM    GEN_TOC_EQUIL
! ************************************************************************
! *                                                                      *
! *   Program GEN_TOC_EQUIL  merges displacements caaused by ocean tides *
! *   from three sources: gravity ocean tide, polar motion ocean tide    *
! *   and equalibroum tide in one output.                                *
! *                                                                      *
! *   Modes:                                                             *
! *           1 -- Paw, Pch1, Pch2, Sa, Nod                              *
! *           2 -- Paw, Pch1, Pch2, Mf, Mf+, Msm, Msm, Mm, Ssa, Sa, Nod  *
! *                                                                      *
! *
! *  $MALO_DIR/bin_static/gen_toc_equil $MALO_DIR/share/toc_equil01_load_d2699.cnf /imsl/load_har_grid/toc/equil01/toc_equil01_harmod.all 2
! *
! *  ### 04-JUN-2014  GEN_TOC_EQUIL  v2.0 (c)  L. Petrov  07-JUN-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'harpos.i'
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE     ( HEB__TYPE ) :: HEBC, HEBS, HEBE, HEBO
      TYPE     ( MALO_STA__TYPE ) :: STA(MALO__MSTA)
      TYPE     ( HARPOS__S_RECORD ) :: SREC
      TYPE     ( HARPOS__D_RECORD ) :: DREC
      INTEGER*4  MP
      REAL*8     EXC_SQ
      PARAMETER  ( EXC_SQ = 2.0D0*MALO__FLAT_WGS84 - MALO__FLAT_WGS84**2 )
      PARAMETER  ( MP = 1024*1024 )
      INTEGER*4  M__COS, M__SIN
      PARAMETER  ( M__COS = 1 )
      PARAMETER  ( M__SIN = 2 )
!
      CHARACTER  FILCNF*128, FILG*128, FILC*128, FILS*128, FILE*128, FILO*128, &
     &           FILOUT*128, FILDSC*128, FILCOM*128, FILNC*128, &
     &           STR*128, EXT*4, WAVE_NAME*4, FRQ_STR*1024, PRG__LABEL*36
      PARAMETER  ( PRG__LABEL = 'gen_toc_equil v  1.0  of 2017.06.08 ' )
      INTEGER*4  J1, J2, J3, J4, IND_FRQ(MALO__MWAV), IVRB, IW, IC, &
     &           LW, L_FRQ, ID, IL, NLON, NLAT, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN4, LINDEX, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      IVRB = 1
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: gen_to_equil filcnf filout [ivrb]'
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, FILCNF )
           CALL GETARG ( 2, FILO )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF 
      IL = ILEN(FILO)
      IF ( IL < 7 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5301, IUER, 'GEN_TOC_EQUIL', 'Output file '// &
     &          FILO(1:I_LEN(FILO))//' is too short' )
           CALL EXIT ( 1 )
      END IF
      EXT = FILO(IL-3:IL)
      CALL CHASHL ( EXT )
      IF ( EXT == '.heb' .OR. EXT == '.nc' .OR. EXT == '.all' ) THEN
           CONTINUE 
         ELSE
           IUER = -1
           CALL ERR_LOG ( 5302, IUER, 'GEN_TOC_EQUIL', 'Unsupported '// &
     &         'extension of the output file '//FILO(1:I_LEN(FILO))// &
     &         ', while one of .heb, .nc, or .all were expected' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize MALO object
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5303, IUER, 'GEN_TOC_EQUIL', 'Error in an '// &
     &         'attempt to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5304, IUER, 'GEN_TOC_EQUIL', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read MALO configuration file
!
      IUER = -1
      CALL MALO_CONFIG ( FILCNF, MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5305, IUER, 'GEN_TOC_EQUIL', 'Failure in parsing MALO '// &
     &         'configuration file '//FILCNF )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MAL(1)%CONF%MODEL_CODE == 0 )THEN
           IUER = -1
           CALL ERR_LOG ( 5306, IUER, 'GEN_TOC_EQUIL', 'Trap of internal control: '// &
     &         'the value of the MAL(1)%CONF%MODEL_CODE file specified in the '// &
     &         'configuration file '//TRIM(FILCNF)//' is zero' )
           CALL EXIT ( 1 )
      END IF
!
      FILE = MAL(1)%CONF%FINAM_MODEL
      IF ( ILEN(FILE) < 12 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5307, IUER, 'GEN_TOC_EQUIL', 'Name of the MAL(1)%CONF%FINAM_MODEL'// &
     &         'file specified in the configuration file '//TRIM(FILCNF)// &
     &         ' is too short: shorter tan 12 characters' )
           CALL EXIT ( 1 )
      END IF
      ID = LINDEX ( FILE, '_d' )
      IF ( ID < 2 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5308, IUER, 'GEN_TOC_EQUIL', 'Trap of internal control: '// &
     &         'the name of the MAL(1)%CONF%FINAM_MODEL file specified in the '// &
     &         'configuration file '//TRIM(FILCNF)//' does not have pateern _d' )
           CALL EXIT ( 1 )
      END IF
      FILC = FILE(1:ID-1)//'_coslam'//FILE(ID:)
      FILS = FILE(1:ID-1)//'_sinlam'//FILE(ID:)
!
      IUER = -1
      CALL MALO_CHECK_MODEL_FILE ( FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5309, IUER, 'GEN_TOC_EQUIL', 'Cannot find '// &
     &         'input file with site displacements due to unit '// &
     &         'equilibrium part' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILE, HEBE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5310, IUER, 'GEN_TOC_EQUIL', 'Failure in '// &
     &         'reading input file with site displacements due to '// &
     &         'unit equilibrium tide' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_MODEL_FILE ( FILC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5311, IUER, 'GEN_TOC_EQUIL', 'Cannot find '// &
     &         'input file with site displacements due to unit cosine '// &
     &         'lambda part of ocean polar tide' )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL READ_HEB ( FILC, HEBC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5312, IUER, 'GEN_TOC_EQUIL', 'Failure in reading '// &
     &         'input file with site displacements due to unit cosine '// &
     &         'lambda part of ocean polar tide' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_MODEL_FILE ( FILS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5313, IUER, 'GEN_TOC_EQUIL', 'Cannot find '// &
     &         'input file with site displacements due to unit sine '// &
     &         'lambda part of ocean polar tide' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILS, HEBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5314, IUER, 'GEN_TOC_EQUIL', 'Failure in reading '// &
     &         'input file with site displacements due to unit sine lambda '// &
     &         'part of ocean polar tide' )
           CALL EXIT ( 1 )
      END IF
!
      HEBO = HEBE
      HEBO%DIMS(3) = 3
      HEBO%DIMS(4) = MALO_NFS(MAL(1)%CONF%MODEL_CODE)
      NLON = HEBO%DIMS(1)
      NLAT = HEBO%DIMS(2)
!
      ALLOCATE ( HEBO%VAL(HEBO%DIMS(1),HEBO%DIMS(2),HEBO%DIMS(3),HEBO%DIMS(4)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(HEBO%DIMS(1))*INT8(HEBO%DIMS(2))*INT8(HEBO%DIMS(3))*INT8(HEBO%DIMS(4)), STR )
           IUER = -1
           CALL ERR_LOG ( 5315, IUER, 'GEN_TOC_EQUIL', 'Failure in an attempt to'// &
     &         'allocate '//TRIM(STR)//' bytes of dynamic memory for array '// &
     &         'HEBO%VAL' )
           CALL EXIT ( 1 )
      END IF
!
      FRQ_STR = '|'
      LW = 0
      L_FRQ = 0
      DO 410 J1=1,MALO_NFS(MAL(1)%CONF%MODEL_CODE)
         LW = LW + 1
         WAVE_NAME = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)
         CALL TRAN ( 11, WAVE_NAME, WAVE_NAME )
         CALL UNDERSCORE_TO_BLANK ( WAVE_NAME )
         IW = LTM_DIF ( 0, MOT, OTID_WAV, WAVE_NAME )
         IF ( IW < 1 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5316, IUER, 'GEN_TOC_EQUIL', 'Trap of internal '// &
          &       'control: cannot find wave '//WAVE_NAME//' in OTID_WAV' )
              CALL EXIT ( 1 )
         END IF
         IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Pc01_cos' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_C_CH1*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         - POL_S_CH1*HEBS%VAL(1:NLON,1:NLAT,1:3,1)
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'c|'
              IC = M__COS
           ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Pc01_sin' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_S_CH1*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         + POL_C_CH1*HEBS%VAL(1:NLON,1:NLAT,1:3,1) 
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'s|'
              IC = M__SIN
           ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Pc02_cos' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_C_CH2*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         - POL_S_CH2*HEBS%VAL(1:NLON,1:NLAT,1:3,1) 
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'c|'
              IC = M__COS
           ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Pc02_sin' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_S_CH2*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         + POL_C_CH2*HEBS%VAL(1:NLON,1:NLAT,1:3,1) 
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'s|'
              IC = M__SIN
           ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Paw__cos' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_C_PAW*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         - POL_S_PAW*HEBS%VAL(1:NLON,1:NLAT,1:3,1) 
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'c|'
              IC = M__COS
           ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE) == 'Paw__sin' ) THEN
              HEBO%VAL(1:NLON,1:NLAT,1:3,LW) =   POL_S_PAW*HEBC%VAL(1:NLON,1:NLAT,1:3,1) &
     &                                         + POL_C_PAW*HEBS%VAL(1:NLON,1:NLAT,1:3,1) 
              STR = MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(1:4)//'s|'
              IC = M__SIN
           ELSE
              IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(6:8) == 'cos' ) THEN
                   IC = M__COS
                 ELSE IF ( MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)(6:8) == 'sin' ) THEN
                   IC = M__SIN
                 ELSE
                   IUER = -1
                   CALL ERR_LOG ( 5317, IUER, 'GEN_TOC_EQUIL', 'Trap of internal '// &
          &            'control: suffix of wave '//MALO_WFS(J1,MAL(1)%CONF%MODEL_CODE)// &
     &                 ' is niether cos, nor sin' )
                   CALL EXIT ( 1 )
              END IF
              IF ( IC == M__COS ) THEN
                   HEBO%VAL(1:NLON,1:NLAT,1:3,LW) = OTID_AMP(IW)*HEBE%VAL(1:NLON,1:NLAT,1:3,1)
                 ELSE IF ( IC == M__SIN ) THEN
                   HEBO%VAL(1:NLON,1:NLAT,1:3,LW) = 0.0
              END IF
         END IF
         IF ( IC == M__COS ) THEN
              FRQ_STR = TRIM(FRQ_STR)//WAVE_NAME//'c|'
              L_FRQ = L_FRQ + 1
              IND_FRQ(L_FRQ) = IW
            ELSE IF ( IC == M__SIN ) THEN
              FRQ_STR = TRIM(FRQ_STR)//WAVE_NAME//'s|'
         END IF
 410  CONTINUE 
      CALL CLRCH ( STR )
      CALL INCH  ( MAL(1)%CONF%MODEL_CODE, STR )
      IF ( EXT == '.heb' .OR. EXT == '.all' ) THEN
           ID = LINDEX ( FILO, '.' )
           FILOUT = FILO(1:ID-1)//'.heb'
           HEBO%TITLE          = 'Equilibrium tidal ocean loading'
           HEBO%SDS_NAME       = 'Site displacements '//TRIM(FRQ_STR)
           HEBO%PROD_NAME      = 'Equilibrium tidal ocean loading'
           HEBO%COMMENT(1)     = 'Model code: '//STR(1:I_LEN(STR))
           HEBO%INSTITUTION    = 'Astrogeo Center'
           HEBO%REFERENCES     = 'http://astrogeo.org/malo/'
           HEBO%HISTORY        = 'Generated by Astrogeo Center'
           HEBO%UNITS          = 'm'
           HEBO%PROD_DATE_TIME = GET_CDATE()
           HEBO%DATA_OFFSET    = HEB__HDS
           HEBO%ENDIAN         = HEB__LE
           HEBO%FILL_VALUE     = 1.0E15
           HEBO%OFFSET         = 0.0
           HEBO%SCALE_FACTOR   = 1.0
           HEBO%MIN_VALUE      = MINVAL(HEBO%VAL)
           HEBO%MAX_VALUE      = MAXVAL(HEBO%VAL)
           HEBO%SOURCE         = 'Astrogeo Center'
           HEBO%VERSION_ID     = PRG__LABEL
           HEBO%DATA_FORMAT    = HEB__R4
!
           IUER = -1
           CALL WRITE_HEB ( HEBO, HEBO%VAL, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5318, IUER, 'GEN_TOC_EQUIL', 'Failure in an '// &
     &              'attempt to write loading displacements into the output '// &
     &              'file '//FILOUT )
                CALL EXIT ( 1 )
          END IF
          WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
      IF ( EXT == '.nc' .OR. EXT == '.all' ) THEN
           ID = LINDEX ( FILO, '.' )
           FILOUT = FILO(1:ID-1)//'.nc'
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, * ) 'NLON,NLAT= ', NLON, NLAT
                WRITE ( 6, * ) 'L_FRQ= ', L_FRQ, ' IND_FRQ= ', INT2(IND_FRQ(1:L_FRQ))
           END IF
           IUER = -1
           CALL WRITE_LOADING_NC ( NLON, NLAT, L_FRQ, IND_FRQ, &
     &                             J2000__MJD, 0.0D0, %VAL(0), HEBO%VAL, &
     &                             PRG__LABEL, 'NO', MAL(1)%CONF%LOA_FINAM_DESCR, &
     &                             MAL(1)%CONF%LOA_FINAM_COMM, FILOUT, &
     &                             IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5319, IUER, 'GEN_TOC_EQUIL', 'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILOUT )
                RETURN 
           END IF
           WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      END  PROGRAM  GEN_TOC_EQUIL  !#!  
