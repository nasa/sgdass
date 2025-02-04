      PROGRAM    LOAD_MAP_VIEW
! ************************************************************************
! *                                                                      *
! *   Program LOAD_MAP_VIEW                                              *
! *                                                                      *
! *   ICMP = 1 -- cos                                                    *
! *   ICMP = 2 -- sin                                                    *
! *   ICMP = 3 -- amp                                                    *
! *   ICMP = 4 -- phase                                                  *
! *                                                                      *
! *   Up-East-North = 1 -- Up                                            *
! *   Up-East-North = 2 -- East                                          *
! *   Up-East-North = 3 -- North                                         *
! *                                                                      *
! *  ### 11-MAR-2013  LOAD_MAP_VIEW  v4.0 (c) L. Petrov 16-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB, HEB_COA
      CHARACTER  FILIN*128, FILIN_2ND*128, FILCOA*128, FILOUT*128
      CHARACTER  STR*128, STR1*128, EXT_OUT*4
      INTEGER*4  M_TXT
      PARAMETER  ( M_TXT = 1024 )
      INTEGER*4  L_TXT, NLON, NLAT, MJD, DATA_OFFS
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:,:,:), TMP_3D_R4(:,:,:)
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4_2ND(:,:,:)
      LOGICAL*1  FL_VIEW_LOOP
      CHARACTER  C_TXT(M_TXT)*128, TITLE*128, DATE_STR*32, UNIT*2, &
     &           WAV_NAM*4, FRQ_STR*256, &
     &           WORD1*16, WORD2*16, WORD3*16, WORD4*16, WORD5*16, WORD6*16, REG*4
!
      CHARACTER  VEC_NAM(3)*5
      DATA       VEC_NAM / &
     &                    'Up   ', &
     &                    'East ', &
     &                    'North'  &
     &                   /
!
      CHARACTER  CMP_NAM(4)*5
      DATA       CMP_NAM / &
     &                     'Cos',   &
     &                     'Sin',   &
     &                     'Ampl',  &
     &                     'Phase'  &
     &                   /
!
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  ID, IDEV, IPAL, ISCL, IPRJ, J1, J2, J3, J4, J5, J6, ICOAST, &
     &           ICMP, IFRQ, IVEC, IND(2,MIND), NCMP, NFRQ, IND3_SECT(2), &
     &           IND4_SECT(2), LIND, IP, I_PAR, KLON, KLAT, DEG, ILAND, &
     &           LN, IUER
      LOGICAL*1  FL_LAT_NP_POLE, FL_LAT_LON_VAL 
      INTEGER*8  I8_SWAP
      REAL*4     LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, VAL_MIN, VAL_MAX, &
     &           LAT_VAL, LON_VAL, DSPL_MIN, DSPL_MAX
      REAL*8     TAI
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, MULTI_INDEX, READ_LINE
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
!     $MALO_DIR/bin/load_map_view /s0/temp/load/toc_fes2012_grid_d21599_out.heb 1 3 1 1 -1 1
!
      DSPL_MIN =  1.D0
      DSPL_MAX = -1.0D0
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: load_map_view input_file Up-East-North cos-sin frq [scale] [projection] [land] [output_file]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, FILIN )
           CALL GETARG ( 2, STR   )
           CALL CHIN ( STR, IVEC  ) 
           IF ( IVEC < 1 .OR. IVEC > 3 ) THEN
                CALL ERR_LOG ( 6201, IUER, 'LOAD_MAP_VIEW', 'Wrong Up-East-North '// &
     &              'index: '//STR(1:I_LEN(STR))//' -- an integer in range '// &
     &              '[1,3] was expected' ) 
                CALL EXIT ( 1 )
           END IF 
!
           CALL GETARG ( 3,   STR  )
           CALL CHIN   ( STR, ICMP ) 
           IF ( ICMP < 1 .OR. ICMP > 4 ) THEN
                CALL ERR_LOG ( 6202, IUER, 'LOAD_MAP_VIEW', 'Wrong cos-sin-ampl-phas '// &
     &              'index: '//STR(1:I_LEN(STR))//' -- an integer in range '// &
     &              '[1,2] was expected' ) 
                CALL EXIT ( 1 )
           END IF 
!
           CALL GETARG ( 4,   STR  )
           CALL CHIN   ( STR, IFRQ ) 
           IF ( IFRQ < 1 ) THEN
                CALL ERR_LOG ( 6203, IUER, 'LOAD_MAP_VIEW', 'Wrong IFRQ '// &
     &              'index: '//STR(1:I_LEN(STR))//' -- an positive integer '// &
     &              'was expected' ) 
                CALL EXIT ( 1 )
           END IF 
!
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR  )
                CALL CHIN ( STR, ISCL )
                IF ( ISCL < 0 .OR. ISCL > 43 ) THEN
                     CALL ERR_LOG ( 6204, IUER, 'LOAD_MAP_VIEW', 'Wrong sacle '// &
     &                   'code: '//STR(1:I_LEN(STR))//' -- an integer in range '// &
     &                   '[0,43] was expected' ) 
                     CALL EXIT ( 1 )
                END IF 
              ELSE 
                ISCL = 0
           END IF 
!
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR  )
                CALL CHIN ( STR, IPRJ )
                IF ( IPRJ .NE. 1 .AND. IPRJ .NE. 2 .AND. IPRJ .NE. -1 .AND. IPRJ .NE. -2 ) THEN
                     CALL ERR_LOG ( 6205, IUER, 'LOAD_MAP_VIEW', 'Wrong '// &
     &                   'projection code: '//STR(1:I_LEN(STR))//' -- '// &
     &                   'an integer 1, 2, -1, or -2 was expected' ) 
                     CALL EXIT ( 1 )
                END IF
                IF ( IPRJ < 0 ) THEN
                     ICOAST = 1
                     IPRJ = -IPRJ
                   ELSE 
                     ICOAST = 1
                END IF
             ELSE 
                IPRJ = 1
                ICOAST = 0
           END IF 
!
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                CALL CHIN   ( STR, ILAND )
              ELSE 
                ILAND = 0
           END IF
!
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, FILOUT )
              ELSE 
                FILOUT = '/tmp/1.xw'
           END IF
      END IF
!
      ID = LINDEX ( FILOUT, '.' ) 
      IF ( FILOUT == '/XW' .OR. FILOUT == 'XW' .OR. &
     &     FILOUT == '/XS' .OR. FILOUT == 'XS'      ) THEN
           EXT_OUT = '.xw'
           IDEV = 1
         ELSE IF ( ID .LE. 0 ) THEN
           CALL ERR_LOG ( 6206, IUER, 'LOAD_MAP_VIEW', 'Output file should '// &
     &         'have extension' )
           CALL EXIT ( 1 )
         ELSE IF ( FILOUT(ID:) == '.xw' ) THEN
           EXT_OUT = '.xw'
           IDEV = 1
         ELSE IF ( FILOUT(ID:) == '.ps' ) THEN
           CALL CLRCH( FILOUT(ID:) )
           EXT_OUT = '.ps'
           IDEV = 4
         ELSE IF ( FILOUT(ID:) == '.gif' ) THEN
           CALL CLRCH( FILOUT(ID:) )
           EXT_OUT = '.gif'
           IDEV = 3
         ELSE IF ( FILOUT(ID:) == '.heb' ) THEN
           CALL CLRCH( FILOUT(ID:) )
           EXT_OUT = '.heb'
           IDEV = 5
         ELSE 
           CALL ERR_LOG ( 6207, IUER, 'LOAD_MAP_VIEW', 'Unsupported extension '// &
     &         'of the output file '//FILOUT(1:I_LEN(FILOUT))//' -- supported '// &
     &         'extensions are .ps, .gif, .xw, .heb or filename XW' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILAND == 0  .OR. ILAND == 1 ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 6208, IUER, 'LOAD_MAP_VIEW', 'Unsupported 7th '// &
     &         'parameter iland: 0 (loading for ocean and land) or 1 '// &
     &         '(loading for land only) are supported' )
           CALL EXIT ( 1 )
      END IF
!
      CALL GETENVAR ( 'HEB_VIEW_LOOP', STR )
      IF ( STR == 'YES' ) THEN
           FL_VIEW_LOOP = .TRUE.
         ELSE 
           FL_VIEW_LOOP = .FALSE.
      END IF 
!
      CALL GETENVAR ( 'LOAD_MAP_LAT_LON_VAL', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL EXWORD ( STR, MIND, LIND, IND, ', '//CHAR(9), IUER )
           IF ( LIND .GE. 2 ) THEN
                READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT='(F10.5)') LAT_VAL
                READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT='(F10.5)') LON_VAL
                FL_LAT_LON_VAL = .TRUE.
              ELSE 
                FL_LAT_LON_VAL = .FALSE.
           END IF
         ELSE 
           FL_LAT_LON_VAL = .FALSE.
      END IF
!
      ID = LINDEX ( FILIN, '.' ) 
      IF ( ID > 1 .AND. ID .LE. ILEN(FILIN) - 3 ) THEN
           IF ( FILIN(ID:ID+3) == '.bz2' ) THEN
                ID = LINDEX ( FILIN(1:ID-1), '.' ) 
           END IF
      END IF
      IF ( ID == 0 ) THEN
           CALL ERR_LOG ( 6209, IUER, 'LOAD_MAP_VIEW', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' shoud have an extension' )
           CALL EXIT ( 1 )
      END IF
      IF ( ID < 1 ) ID = 1
      IF ( FILIN(ID:) == '.loa' ) THEN
           IUER = -1
           CALL LOA_INQ ( FILIN, NLON, NLAT, MJD, TAI, M_TXT, L_TXT, C_TXT, &
     &                    DATA_OFFS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6210, IUER, 'LOAD_MAP_VIEW', 'Failure in an attempt '// &
     &              'to read the header of the input file '//FILIN )
                CALL EXIT ( 1 )
           END IF
!
           ALLOCATE ( TMP_3D_R4(3,NLON,NLAT), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR   )
                CALL INCH  ( 4*3*NLON*NLAT, STR )
                CALL ERR_LOG ( 6211, IUER, 'LOAD_MAP_VIEW', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array TMP_3D_R4' )
                CALL EXIT ( 1 )
           END IF
!
           ALLOCATE ( DSP_ARR_R4(NLON,NLAT,3), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR   )
                CALL INCH  ( 4*NLON*NLAT, STR )
                CALL ERR_LOG ( 6212, IUER, 'LOAD_MAP_VIEW', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array DSP_ARR_R4' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL LOA_READ ( FILIN, NLON, NLAT, DATA_OFFS, TMP_3D_R4, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6213, IUER, 'LOAD_MAP_VIEW', 'Failure in an attempt '// &
     &              'to read the data from the input file '//FILIN )
                CALL EXIT ( 1 )
           END IF
           IUER = -1
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
!           DO 410 J1=1,3
!              DO 420 J2=1,NLAT
!                 DO 430 J3=1,NLON
!                    DSP_ARR_R4(J3,J2,J1) = 1000.0D0*TMP_3D_R4(J1,J3,J2)
!!!                    DSP_ARR_R4(J3,J2,J1) = 1.0D6*TMP_3D_R4(J1,J3,J2)
! 430             CONTINUE 
! 420          CONTINUE 
! 410       CONTINUE 
         ELSE IF ( FILIN(ID:) == '.nc' .OR. FILIN(ID:) == '.nc.bz2' ) THEN
           IUER = -1
           CALL INQ_LOADING_NC ( FILIN, NLON, NLAT, MJD, TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6214, IUER, 'LOAD_MAP_VIEW', 'Failure in an attempt '// &
     &              'to read the header of the input file '//FILIN )
                CALL EXIT ( 1 )
           END IF
!
           ALLOCATE ( DSP_ARR_R4(NLON,NLAT,3), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR   )
                CALL IINCH8 ( INT8(4)*INT8(3)*INT8(NLON)*INT8(NLAT), STR )
                CALL ERR_LOG ( 6215, IUER, 'LOAD_MAP_VIEW', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array DSP_ARR_R4' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL READ_LOADING_NC ( FILIN, INT8(3)*INT8(NLON)*INT8(NLAT), NLON, NLAT, &
     &                            IVEC, ICMP, IFRQ, MJD, TAI, DSP_ARR_R4, WAV_NAM, &
     &                            IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6216, IUER, 'LOAD_MAP_VIEW', 'Failure in an attempt '// &
     &              'to read the data from the input file '//FILIN )
                CALL EXIT ( 1 )
           END IF
           DEG = NLON/4 - 1
           CALL INCH  ( DEG, STR )
           IF ( ILAND == 0 ) THEN
                CALL GETENVAR ( 'MALO_SHARE', STR1 )
                IF ( ILEN(STR) > 0 ) THEN
                     FILCOA = TRIM(STR1)//'/coast_d'//STR(1:I_LEN(STR))//'.heb'
                   ELSE
                     FILCOA = MALO_SHARE//'/coast_d'//STR(1:I_LEN(STR))//'.heb'
                END IF
              ELSE
                FILCOA = '/s0/mod44w/mod44w_ls_blackman_d'//STR(1:I_LEN(STR))//'.heb'
           END IF
           WRITE ( 6, * ) 'Read filcoa= '//FILCOA(1:I_LEN(FILCOA))
           IF ( ICOAST == 1 ) THEN
                IUER = -1
                CALL READ_HEB ( FILCOA, HEB_COA, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6217, IUER, 'LOAD_MAP_VIEW', 'Failure '// &
     &                   'in an attempt to read the coast line file '//FILCOA )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                ALLOCATE ( HEB_COA%VAL(NLON,NLAT,1,1) )
                HEB_COA%VAL = 0.0
           END IF
!
           CALL GETENVAR ( 'LOAD_MAP_SECOND_NC', FILIN_2ND )
           IF ( ILEN(FILIN_2ND) > 0 ) THEN
                ALLOCATE ( DSP_ARR_R4_2ND(NLON,NLAT,3), STAT=IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL CLRCH ( STR   )
                     CALL INCH  ( 4*NLON*NLAT, STR )
                     CALL ERR_LOG ( 6218, IUER, 'LOAD_MAP_VIEW', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &                   'for array DSP_ARR_R4_2ND' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL READ_LOADING_NC ( FILIN_2ND, INT8(3)*INT8(NLON)*INT8(NLAT), &
     &                                 NLON, NLAT, IVEC, ICMP, IFRQ, MJD, TAI, &
     &                                 DSP_ARR_R4_2ND, WAV_NAM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6219, IUER, 'LOAD_MAP_VIEW', 'Failure in an attempt '// &
     &                   'to read the data from the input file '//FILIN_2ND )
                     CALL EXIT ( 1 )
                END IF
!
                DSP_ARR_R4 = DSP_ARR_R4 - DSP_ARR_R4_2ND
           END IF
!
           IUER = -1
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
           IF ( FL_LAT_LON_VAL ) THEN
                KLON = 1 + NINT(LON_VAL/360.*NLON) + 1
                KLAT = NINT((LAT_VAL+90.0)/180.*(NLAT-1))  + 1
           END IF
           DO 450 J5=1,NLAT
               DO 460 J6=1,NLON
                  IF ( IDEV == 5 ) THEN
                       DSP_ARR_R4(J6,J5,1:3) = 1000.0*DSP_ARR_R4(J6,J5,1:3) 
                     ELSE
                       DSP_ARR_R4(J6,J5,1) = 1000.0*DSP_ARR_R4(J6,J5,IVEC) 
                  END IF
                  IF ( FL_LAT_LON_VAL ) THEN
                       IF ( J6 == KLON .AND. J5 == KLAT ) THEN
                            WRITE ( 6, * ) 'LOA_val: ', 1000.0*DSP_ARR_R4(J6,J5,1:3) , &
     &                                     ' Lat= ', -90.0 + (180.0*(KLAT-1))/(NLAT-1), &
     &                                     ' Lon= ', (360.*(KLON-1))/NLON
                       END IF
                  END IF
 460           CONTINUE 
 450       CONTINUE 
 440       CONTINUE 
         ELSE IF ( FILIN(ID:) == '.heb' .OR. FILIN(ID:) == '.heb.bz2' ) THEN
           IF ( FILIN(ID:) == '.heb.bz2' ) THEN
                IUER = -1
                CALL READ_HEB ( FILIN, HEB, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6220, IUER, 'LOAD_MAP_VIEW', 'Failure '// &
     &                   'in an attempt to read the input file '//FILIN )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                IUER = -1
                CALL READ_HEB_HEADER ( FILIN, HEB, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -2
                     CALL ERR_LOG ( 6711, IUER, 'LOAD_MAP_VIEW', 'Error in '// &
     &                   'reading input heb-file '//FILIN )
                     CALL EXIT ( 1 )
                END IF 
           END IF
           IP = MULTI_INDEX ( (IFRQ-1)*2+1, HEB%SDS_NAME, '|' )
           IF ( IP > 0 ) THEN
                FRQ_STR = HEB%SDS_NAME(IP+1:)
                IP = INDEX ( FRQ_STR, '|' )
                IF ( IP > 0 ) CALL CLRCH ( FRQ_STR(IP:) )
                I_PAR   = (IFRQ - 1)*2 + 1
                WAV_NAM = FRQ_STR
                IF ( HEB%DIMS(4) > 1 ) THEN
                     TITLE = CMP_NAM(ICMP)//' '//VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))// &
     &                      ' mass loading at frequency '//WAV_NAM
                   ELSE 
                     TITLE = VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))// &
     &                      ' mass loading at frequency '//WAV_NAM
                END IF
              ELSE 
                IF ( HEB%DIMS(4) > 1 ) THEN
                     TITLE = CMP_NAM(ICMP)//' '//VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))// &
     &                       ' mass loading'
                   ELSE 
                     TITLE = VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))// &
     &                       ' mass loading'
                END IF
                I_PAR = -1
           END IF
!
           IF ( FILIN(ID:) == '.heb' ) THEN
                IND3_SECT(1) = 1
                IND3_SECT(2) = 3
                IF ( I_PAR == -1 ) THEN
                     IND4_SECT(1) = 1
                     IND4_SECT(2) = 1
                   ELSE
                     IND4_SECT(1) = I_PAR
                     IND4_SECT(2) = I_PAR+1
                END IF
           END IF
!
           NLON = HEB%DIMS(1)
           NLAT = HEB%DIMS(2)
!
           ALLOCATE ( DSP_ARR_R4(NLON,NLAT,1), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR   )
                CALL INCH  ( INT8(4)*INT8(NLON)*INT8(NLAT), STR )
                CALL ERR_LOG ( 6221, IUER, 'LOAD_MAP_VIEW', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array DSP_ARR_R4' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL READ_HEB_SECT ( FILIN, HEB, IND3_SECT, IND4_SECT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                WRITE ( 6, * ) 'IND3_SECT= ', IND3_SECT, ' IND4_SECT= ', IND4_SECT
                CALL ERR_LOG ( 6715, IUER, 'LOAD_MAP_VIEW', 'Error in reading '// &
     &              'input heb-file '//FILIN )
                CALL EXIT ( 1 )
           END IF 
!
           IUER = -1
           CALL GET_LOAD_FIELD ( IVEC, ICMP, 1, HEB, DSP_ARR_R4, &
     &                           DSPL_MIN, DSPL_MAX, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6222, IUER, 'LOAD_MAP_VIEW', 'Failure '// &
     &              'in an attempt to extract the data field from '// &
     &              'the input file with the loading model in HEB-format '// &
     &              FILIN )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           CALL ERR_LOG ( 6223, IUER, 'LOAD_MAP_VIEW', 'Unsupported '// &
     &         'extension in the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- supported extensions are .loa, .nc, .heb, .nc.bz2, heb.bz2' )
           CALL EXIT ( 1 )
      END IF
      IF ( DABS ( DLOG(NLAT-1.D0)/DLOG(2.0D0) - IDINT(DLOG(NLAT-1.D0)/DLOG(2.0D0)) ) < 1.0D-5 ) THEN
           FL_LAT_NP_POLE = .TRUE.
         ELSE 
           FL_LAT_NP_POLE = .FALSE.
      END IF
!
      IUER  = -1
      IPAL  = 7
      IF ( FILIN(ID:) == '.loa' .OR. FILIN(ID:) == '.nc' .OR. FILIN(ID:) == '.nc.bz2' ) THEN
           IF ( ILEN(WAV_NAM) == 0 ) THEN
                TITLE = VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))//' mass loading on '//DATE_STR(1:16)
              ELSE 
                TITLE = CMP_NAM(ICMP)(1:I_LEN(CMP_NAM(ICMP)))//' of '// &
     &                  VEC_NAM(IVEC)(1:I_LEN(VEC_NAM(IVEC)))// &
     &                  ' mass loading diplacment wave '//WAV_NAM
           END IF
           UNIT  = 'mm'
           CALL GETENVAR ( 'LOAD_MAP_LAT_LON_BOX', STR )
           IF ( ILEN(STR) == 0 ) THEN
                IF ( IDEV == 5 ) THEN
                     FILOUT = FILOUT(1:I_LEN(FILOUT))//'.heb'
                     HEB%DIMS(1) = NLON
                     HEB%DIMS(2) = NLAT
                     HEB%DIMS(3) = 1 
                     HEB%DIMS(4) = 1
!
                     HEB%MJD     = MJD
                     HEB%TAI     = TAI
                     HEB%UTC     = TAI
!
                     HEB%DATA_OFFSET     = HEB__HDS
                     HEB%ENDIAN          = HEB__LE
                     HEB%DATA_TRANSFORM  = HEB__NONE
                     HEB%FILL_VALUE      = 1.0E15
                     HEB%OFFSET          = 0.0
                     HEB%SCALE_FACTOR    = 1.0
                     HEB%DATA_COMPRESSION = HEB__NONE
                     HEB%SDS_NAME        = VEC_NAM(IVEC)//' Loading diplacement'
                     HEB%UNITS           = 'mm'
                     HEB%DATA_FORMAT     = HEB__R4
                     HEB%MIN_VALUE       = MINVAL(DSP_ARR_R4(1:NLON,1:NLAT,IVEC))
                     HEB%MAX_VALUE       = MAXVAL(DSP_ARR_R4(1:NLON,1:NLAT,IVEC))
                     HEB%VALID_RANGE(1)  = HEB%MIN_VALUE
                     HEB%VALID_RANGE(2)  = HEB%MAX_VALUE
                     HEB%PROD_DATE_TIME  = GET_CDATE()
                     HEB%HISTORY         = 'n/a'
                     HEB%SOURCE          = 'n/a'
                     HEB%TITLE           = HEB%SDS_NAME       
                     HEB%PROD_NAME       = HEB%SDS_NAME
                     HEB%INSTITUTION     = 'Astrogeo Center'
                     HEB%REFERENCES      = 'http://astrogeo.org/malo/'
                     HEB%VERSION_ID      = '01'
!
                     IUER = -1
                     CALL WRITE_HEB ( HEB, DSP_ARR_R4(1,1,IVEC), FILOUT, IUER )
                     IF ( IUER .NE. 0 ) THEN
                          CALL ERR_LOG ( 6224, IUER, 'LOAD_MAP_VIEW', 'Error in an attempt '// &
     &                        'to write the loading component into the output '// &
     &                        'file '//FILOUT )
                          CALL EXIT ( 1 )
                     END IF
                   ELSE
                     IF ( IPRJ == 2 ) THEN 
                          CALL GRID_2D_SHIFT_180_R4 ( NLON, NLAT, DSP_ARR_R4  )
                          CALL GRID_2D_SHIFT_180_R4 ( NLON, NLAT, HEB_COA%VAL )
                     END IF
                     CALL PLOT_GRID_COA_R4 ( IDEV, IPAL, ISCL, IPRJ, NLON, NLAT, &
     &                                       DSP_ARR_R4, HEB_COA%VAL, TITLE, UNIT, &
     &                                       FILOUT, IUER )
                END IF
              ELSE
                CALL EXWORD ( STR, MIND, LIND, IND, ',', IUER )
                IF ( LIND < 4 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6225, IUER, 'LOAD_MAP_VIEW', 'Wrong '// &
     &                   'value of LOAD_MAP_LAT_LON_BOX: there should be '// &
     &                   'four comma separated words' )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=* ) LAT_MIN
                READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=* ) LAT_MAX
                READ ( UNIT=STR(IND(1,3):IND(2,3)), FMT=* ) LON_MIN
                READ ( UNIT=STR(IND(1,4):IND(2,4)), FMT=* ) LON_MAX
                CALL GETENVAR ( 'LOAD_MAP_MIN_MAX', STR )
                IF ( ILEN(STR) == 0 ) THEN
                     IF ( DSPL_MAX .LE. DSPL_MIN ) THEN
                          VAL_MIN =  1.0
                          VAL_MAX = -1.0
                        ELSE
                          VAL_MIN = DSPL_MIN
                          VAL_MAX = DSPL_MAX
                     END IF
                   ELSE 
                     CALL EXWORD ( STR, MIND, LIND, IND, ',', IUER )
                     IF ( LIND < 2 ) THEN
                          CALL ERR_LOG ( 6226, IUER, 'LOAD_MAP_VIEW', 'Wrong '// &
     &                        'value of LOAD_MAP_MIN_MAX: there should be '// &
     &                        'two comma separated words' )
                         CALL EXIT ( 1 )
                     END IF
                     READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=* ) VAL_MIN
                     READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=* ) VAL_MAX
                END IF
!
                IUER = -1
                CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT, &
     &                    DSP_ARR_R4(1,1,1), HEB_COA%VAL, %VAL(0), TITLE, UNIT, FILOUT, &
     &                    LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                    VAL_MIN, VAL_MAX, IUER )
                IF ( FL_VIEW_LOOP ) THEN
 710                 CONTINUE 
 720                 CONTINUE 
                     LN = READ_LINE ( 'Enter new lat_min lat_max  lon_min lon_max >>', STR )
                     REG = CHAR(32)//CHAR(0)//CHAR(9)//','
                     CALL EXWORD ( STR, MIND, LIND, IND, REG, IUER )
                     IF ( LIND < 4 ) THEN
                          WRITE ( 6, * ) 'Too few words: ', LIND
                          GOTO 720
                     END IF
!
                     WORD1 = STR(IND(1,1):IND(2,1))
                     IF ( INDEX ( WORD1, '.' ) == 0 ) WORD1 = WORD1(1:I_LEN(WORD1))//'.'
                     READ ( UNIT=WORD1, FMT='(F15.7)', IOSTAT=IUER ) LAT_MIN
                     IF ( IUER .NE. 0 ) THEN
                          WRITE ( 6, * ) 'Wrong lat_min'
                          GOTO 720
                     END IF
!         
                     WORD2 = STR(IND(1,2):IND(2,2))
                     IF ( INDEX ( WORD2, '.' ) == 0 ) WORD2 = WORD2(1:I_LEN(WORD2))//'.'
                     READ ( UNIT=WORD2, FMT='(F15.7)', IOSTAT=IUER ) LAT_MAX
                     IF ( IUER .NE. 0 ) THEN
                          WRITE ( 6, * ) 'Wrong lat_max'
                          GOTO 720
                     END IF
!
                     WORD3 = STR(IND(1,3):IND(2,3))
                     IF ( INDEX ( WORD3, '.' ) == 0 ) WORD3 = WORD3(1:I_LEN(WORD3))//'.'
                     READ ( UNIT=WORD3, FMT='(F15.7)', IOSTAT=IUER ) LON_MIN
                     IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong lon_min'
                     GOTO 720
                     END IF
!
                     WORD4 = STR(IND(1,4):IND(2,4))
                     IF ( INDEX ( WORD4, '.' ) == 0 ) WORD4 = WORD4(1:I_LEN(WORD4))//'.'
                     READ ( UNIT=WORD4, FMT='(F15.7)', IOSTAT=IUER ) LON_MAX
                     IF ( IUER .NE. 0 ) THEN
                          WRITE ( 6, * ) 'Wrong lon_max'
                          GOTO 720
                     END IF
!
                     CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT, &
     &                         DSP_ARR_R4(1,1,1), HEB_COA%VAL, %VAL(0), TITLE, UNIT, FILOUT, &
     &                         LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                         VAL_MIN, VAL_MAX, IUER )
                     GOTO 710
                END IF
           END IF
        ELSE IF ( FILIN(ID:) == '.heb' .OR. FILIN(ID:) == '.heb.bz2' ) THEN
           DEG = HEB%DIMS(1)/4 - 1
           CALL INCH  ( DEG, STR )
           IF ( ILAND == 0 ) THEN
                FILCOA = MALO_SHARE//'/coast_d'//STR(1:I_LEN(STR))//'.heb'
              ELSE
                FILCOA = '/s0/mod44w/mod44w_ls_blackman_d'//STR(1:I_LEN(STR))//'.heb'
           END IF
           WRITE ( 6, * ) 'Read filcoa= '//FILCOA(1:I_LEN(FILCOA))
           IF ( ICOAST == 1 ) THEN
                IUER = -1
                CALL READ_HEB ( FILCOA, HEB_COA, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6227, IUER, 'LOAD_MAP_VIEW', 'Failure '// &
     &                   'in an attempt to read the coast line file '//FILCOA )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                ALLOCATE ( HEB_COA%VAL(NLON,NLAT,1,1) )
                HEB_COA%VAL = 0.0
           END IF
           IF ( ICMP == 4 ) THEN
                UNIT  = 'deg'
              ELSE 
                UNIT  = 'mm'
           END IF
           CALL GETENVAR ( 'LOAD_MAP_LAT_LON_BOX', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL EXWORD ( STR, MIND, LIND, IND, ',', IUER )
                IF ( LIND < 4 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6228, IUER, 'LOAD_MAP_VIEW', 'Wrong '// &
     &                   'value of LOAD_MAP_LAT_LON_BOX: there should be '// &
     &                   'four comma separated words' )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=* ) LAT_MIN
                READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=* ) LAT_MAX
                READ ( UNIT=STR(IND(1,3):IND(2,3)), FMT=* ) LON_MIN
                READ ( UNIT=STR(IND(1,4):IND(2,4)), FMT=* ) LON_MAX
              ELSE
                LAT_MIN = -90.0
                LAT_MAX =  90.0
                LON_MIN =   0.0
                LON_MAX = 360.0
           END IF 
           CALL GETENVAR ( 'LOAD_MAP_MIN_MAX', STR )
           IF ( ILEN(STR) == 0 ) THEN
                IF ( DSPL_MAX .LE. DSPL_MIN ) THEN
                     VAL_MIN = 1000.0D0*HEB%MIN_VALUE
                     VAL_MAX = 1000.0D0*HEB%MAX_VALUE
                   ELSE
                     VAL_MIN = DSPL_MIN
                     VAL_MAX = DSPL_MAX
                 END IF
              ELSE 
                CALL EXWORD ( STR, MIND, LIND, IND, ',', IUER )
                IF ( LIND < 2 ) THEN
                     CALL ERR_LOG ( 6229, IUER, 'LOAD_MAP_VIEW', 'Wrong '// &
     &                   'value of LOAD_MAP_MIN_MAX: there should be '// &
     &                   'two comma separated words' )
                    CALL EXIT ( 1 )
                END IF
                READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=* ) VAL_MIN
                READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=* ) VAL_MAX
           END IF
 730       CONTINUE 
           IUER = -1
           IF ( FL_LAT_NP_POLE ) THEN
!                CALL PLOT_GRID_R4 ( IDEV, IPAL, ISCL, IPRJ, NLON, NLAT-1, &
!     &                              DSP_ARR_R4, TITLE, UNIT, VAL_MIN, VAL_MAX, FILOUT, IUER )
                 IF ( ASSOCIATED ( HEB_COA%VAL ) ) THEN
                      write ( 6, * ) 'load_map-678:  loc(heb_coa%val1) = ', loc(heb_coa%val1)  ! %%%%
                      CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT-1, &
     &                          DSP_ARR_R4(1,1,1), HEB_COA%VAL, %VAL(0), TITLE, UNIT, FILOUT, &
     &                          LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                          VAL_MIN, VAL_MAX, IUER )
                    ELSE
                      write ( 6, * ) 'load_map-684:  loc(heb_coa%val1) = ', loc(heb_coa%val1)  ! %%%%
                      CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT-1, &
     &                          DSP_ARR_R4(1,1,1), %VAL(0), HEB_COA%VAL1, TITLE, UNIT, FILOUT, &
     &                          LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                          VAL_MIN, VAL_MAX, IUER )
                 END IF
              ELSE 
                 IF ( ASSOCIATED ( HEB_COA%VAL ) ) THEN
!                      write ( 6, * ) 'load_map-695:  loc(heb_coa%val1) = ', loc(heb_coa%val1)  ! %%%%
                      CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT, &
     &                         DSP_ARR_R4(1,1,1), HEB_COA%VAL, %VAL(0), TITLE, UNIT, FILOUT, &
     &                         LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                         VAL_MIN, VAL_MAX, IUER )
                    ELSE
                      write ( 6, * ) 'load_map-701:  loc(heb_coa%val1) = ', loc(heb_coa%val1)  ! %%%%
                      CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, 1, ILAND, NLON, NLAT, &
     &                         DSP_ARR_R4(1,1,1), %VAL(0), HEB_COA%VAL1, TITLE, UNIT, FILOUT, &
     &                         LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                         VAL_MIN, VAL_MAX, IUER )
                 END IF
           END IF
           IF ( FL_VIEW_LOOP ) THEN
 740            CONTINUE 
                LN = READ_LINE ( 'Enter new lat_min lat_max  lon_min lon_max, val_min val_max >>', STR )
                REG = CHAR(32)//CHAR(0)//CHAR(9)//','
                CALL EXWORD ( STR, MIND, LIND, IND, REG, IUER )
                IF ( LIND < 6 ) THEN
                     WRITE ( 6, * ) 'Too few words: ', LIND
                     GOTO 740
                END IF
!
                WORD1 = STR(IND(1,1):IND(2,1))
                IF ( INDEX ( WORD1, '.' ) == 0 ) WORD1 = WORD1(1:I_LEN(WORD1))//'.'
                READ ( UNIT=WORD1, FMT='(F15.7)', IOSTAT=IUER ) LAT_MIN
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong lat_min'
                     GOTO 740
                END IF
!         
                WORD2 = STR(IND(1,2):IND(2,2))
                IF ( INDEX ( WORD2, '.' ) == 0 ) WORD2 = WORD2(1:I_LEN(WORD2))//'.'
                READ ( UNIT=WORD2, FMT='(F15.7)', IOSTAT=IUER ) LAT_MAX
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong lat_max'
                     GOTO 740
                END IF
!
                WORD3 = STR(IND(1,3):IND(2,3))
                IF ( INDEX ( WORD3, '.' ) == 0 ) WORD3 = WORD3(1:I_LEN(WORD3))//'.'
                 READ ( UNIT=WORD3, FMT='(F15.7)', IOSTAT=IUER ) LON_MIN
                 IF ( IUER .NE. 0 ) THEN
                      WRITE ( 6, * ) 'Wrong lon_min'
                      GOTO 740
                END IF
!
                WORD4 = STR(IND(1,4):IND(2,4))
                IF ( INDEX ( WORD4, '.' ) == 0 ) WORD4 = WORD4(1:I_LEN(WORD4))//'.'
                READ ( UNIT=WORD4, FMT='(F15.7)', IOSTAT=IUER ) LON_MAX
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong lon_max'
                     GOTO 740
                END IF
!
                WORD5 = STR(IND(1,5):IND(2,5))
                IF ( INDEX ( WORD5, '.' ) == 0 ) WORD5 = WORD5(1:I_LEN(WORD5))//'.'
                READ ( UNIT=WORD5, FMT='(F15.7)', IOSTAT=IUER ) VAL_MIN
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong val_min'
                     GOTO 740
                END IF
!
                WORD6 = STR(IND(1,6):IND(2,6))
                IF ( INDEX ( WORD6, '.' ) == 0 ) WORD6 = WORD6(1:I_LEN(WORD6))//'.'
                READ ( UNIT=WORD6, FMT='(F15.7)', IOSTAT=IUER ) VAL_MAX
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong val_max'
                     GOTO 740
                END IF
                GOTO 730
           END IF
      END IF
      END  PROGRAM  LOAD_MAP_VIEW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_LOAD_FIELD ( IVEC, ICMP, IFRQ, HEB, DSPL_ARR_R4, &
     &                            VAL_MIN, VAL_MAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_LOAD_FIELD
! *                                                                      *
! * ### 30-MAR-2013  GET_LOAD_FIELD  v1.0 (c)  L. Petrov 30-MAR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IVEC, ICMP, IFRQ, IUER
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  STR*128
      REAL*4     DSPL_ARR_R4(HEB%DIMS(1),HEB%DIMS(2)), VAL_MIN, VAL_MAX
      INTEGER*4  J1, J2, J3
      REAL*4,    EXTERNAL :: ATAN_CS_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IVEC < 1 .OR. IVEC > 3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC, STR )
           CALL ERR_LOG ( 6230, IUER, 'GET_LOAD_FIELD', 'Wrong '// &
     &         'parameter "vector": '//STR(1:I_LEN(STR))// &
     &         ' it should be in range [1,3]' )
           RETURN
      END IF
      IF ( ICMP < 1 .OR. ICMP > 4 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ICMP, STR )
           CALL ERR_LOG ( 6231, IUER, 'GET_LOAD_FIELD', 'Wrong '// &
     &         'parameter "component": '//STR(1:I_LEN(STR))// &
     &         ' it should be in range [1,4]' )
           RETURN
      END IF
      IF ( IFRQ < 1 .OR. IFRQ > HEB%DIMS(4)/2 ) THEN
           IF ( IFRQ == 1 .AND. HEB%DIMS(4) == 1 ) THEN
                CONTINUE 
              ELSE
                CALL CLRCH ( STR )
                CALL INCH8 ( HEB%DIMS(4)/INT8(2), STR )
                CALL ERR_LOG ( 6232, IUER, 'GET_LOAD_FIELD', 'Wrong '// &
     &              'parameter "component": '//STR(1:I_LEN(STR))// &
     &              ' -- frequency sub-field should be in range [1,'// &
     &               STR(1:I_LEN(STR))//']' )
                RETURN
           END IF
      END IF
!
!$OMP DO PRIVATE ( J1, J2 ), SCHEDULE ( DYNAMIC )
      DO 410 J1=1,HEB%DIMS(2)
         DO 420 J2=1,HEB%DIMS(1)
            IF ( ICMP == 1 .OR. ICMP == 2 ) THEN
                 DSPL_ARR_R4(J2,J1) = 1000.0*HEB%VAL(J2,J1,IVEC,(IFRQ-1)*2+ICMP)
               ELSE IF ( ICMP == 3 ) THEN
                 DSPL_ARR_R4(J2,J1) = 1000.0* &
     &                    SQRT ( HEB%VAL(J2,J1,IVEC,(IFRQ-1)*2+1)**2 + &
     &                           HEB%VAL(J2,J1,IVEC,(IFRQ-1)*2+2)**2   )
               ELSE IF ( ICMP == 4 ) THEN
                 DSPL_ARR_R4(J2,J1) = ATAN_CS_R4 ( HEB%VAL(J2,J1,IVEC,(IFRQ-1)*2+1), &
     &                                             HEB%VAL(J2,J1,IVEC,(IFRQ-1)*2+2)  )/ &
     &                                DEG__TO__RAD
            END IF
            IF ( J2 == 1 .AND. J1 == 1 ) THEN
                 VAL_MIN = DSPL_ARR_R4(J2,J1)
                 VAL_MAX = DSPL_ARR_R4(J2,J1)
               ELSE 
                 VAL_MIN = MIN ( DSPL_ARR_R4(J2,J1), VAL_MIN )
                 VAL_MAX = MAX ( DSPL_ARR_R4(J2,J1), VAL_MAX )
            END IF
 420     CONTINUE 
 410  CONTINUE 
!$OMP END DO
!
      DEALLOCATE ( HEB%VAL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_LOAD_FIELD  !#!  
