      PROGRAM    MALO_TEST_MAIN
! ************************************************************************
! *                                                                      *
! *   Program MALO_TEST_MAIN
! *                                                                      *
! * ### 05-NOV-2012 MALO_TEST_MAIN v3.0 (c)  L. Petrov  16-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      TYPE     ( HEB__TYPE  ) :: HEB, HEB2, HEB_OUT
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH
      INTEGER*4    M_TXT, M_FRQ
      PARAMETER  ( M_TXT =   32 )
      PARAMETER  ( M_FRQ =   21 )
      INTEGER*4  TEST1, TEST2, L_TXT, NLON, NLAT, MJD, DATA_OFFS
      REAL*4,    ALLOCATABLE :: DSPL_ARR_R4(:,:,:), DSPL_GRID_R4(:,:,:)
      REAL*4     VAL_MIN, VAL_MAX
      REAL*8     TAI
      INTEGER*8  HEB_RES, HEB_READ, HEB_I8(8192)  ! %%%%
      INTEGER*4  IND_FRQ, IND_CMP, IND_SC, IND_PAR
      CHARACTER  FRQ_NAM(21)*6
      DATA       FRQ_NAM / &
     &           'Unknow', &
     &           'SA    ', &
     &           'SSA   ', &
     &           'PI1   ', &
     &           'P1    ', &
     &           'S1    ', &
     &           'K1    ', &
     &           'PSI1  ', &
     &           '2T2   ', &
     &           'T2    ', &
     &           'S2    ', &
     &           'R2    ', &
     &           'K2    ', &
     &           'S3-SSa', &
     &           'S3-Sa ', &
     &           'S3    ', &
     &           'S3+Sa ', &
     &           'S3+SSa', &
     &           'S4-SSa', &
     &           'S4-Sa ', &
     &           'S4    '  &
     &           /
      CHARACTER  CMP_NAM(3)*9, CS_NAM(2)*3
      DATA       CMP_NAM / &
     &                     'vertical', &
     &                     'east',     &
     &                     'north'     &
     &                   /
!
      DATA       CS_NAM / &
     &                     'cos', &
     &                     'sin'  &
     &                   /
      CHARACTER  C_TXT(M_TXT)*128, CS_NAME*3
      CHARACTER  FIL1*128, FIL2*128, FIL_G*128, FIL_OH*128, STR*128, &
     &           FILTMP*128, COM_STR*512, INTERNET_HOSTNAME*64, WAVE_STR*128, &
     &           FILOUT*128, WAVE_NAME*32
      LOGICAL*1  FL_BZIP2
      INTEGER*4  IVRB, J1, J2, J3, IL, IS, ILON, PID, IW, IP, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID, SYSTEM, MULTI_INDEX, PUTENV, SETENV
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
!
      VAL_MIN =  1.0
      VAL_MAX = -1.0
      FILOUT  = '/tmp/foo.heb'
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: test_par1 test_par2 file1 [file2] [val_min] [val_max]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, STR )
           CALL CHIN   ( STR, TEST1 )
           CALL GETARG ( 2, STR )
           CALL CHIN   ( STR, TEST2 )
           CALL GETARG ( 3, FIL1 )
           CALL GETARG ( 4, FIL2 )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.0'
                READ ( UNIT=STR, FMT='(F10.5)' ) VAL_MIN
                IS = SETENV ( 'MALO_RVAL_MIN'//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
           END IF
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR )
                IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.0'
                READ ( UNIT=STR, FMT='(F10.5)' ) VAL_MAX
                IS = SETENV ( 'MALO_RVAL_MAX'//CHAR(0), STR(1:I_LEN(STR))//CHAR(0), %VAL(1) )
           END IF
      END IF
      IVRB = 2
!
      ALLOCATE ( MAL(2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6101, IUER, 'MALO_TEST', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6102, IUER, 'MALO_TEST', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(2), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6103, IUER, 'MALO_TEST', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( TEST1 == 11 ) THEN
           IUER = -1
           CALL READ_DQT_HEB ( FIL1, FIL2, HEB_DELP, HEB_Q, HEB_T, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6104, IUER, 'MALO_TEST', 'Error in an attempt '// &
     &               'to read numerical weather model for date '//FIL2 )
                CALL EXIT ( 1 )
           END IF
!
           IF ( HEB_DELP%DIMS(1) == 540 ) THEN
                FIL_G = '/progs/malo_20121012/share/merra_ellipsoid_height.heb'
              ELSE 
                FIL_G = '/progs/malo_20121012/share/geos57_ellipsoid_height.heb'
           END IF
!!  FIL_G = '/tmp/geos_ellipsoid_height.heb'
!!           FIL_OH = '/progs/malo_20121012/share/dig_elev_merra.heb'
!!           FIL_OH = '/progs/malo_20121012/share/dig_elev_geos57.heb'
           FIL_OH = '/progs/malo_20121012/share/gtopo30_dig_elev_1023.heb'
!
           IUER = -1
           CALL READ_HEB_HEADER ( FIL_G, HEB_G, IUER )
           IF ( IUER  .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6105, IUER, 'MALO_TEST', 'Error in reading '// &
     &              'heb-file '//FIL_G )
               RETURN 
           END IF
!
           IUER = -1
           CALL READ_HEB_DATA ( FIL_G, HEB_G, IUER )
           IF ( IUER  .NE. 0 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 6106, IUER, 'MALO_TEST', 'Error in reading '// &
     &               'heb-file '//FIL_G )
                 RETURN 
           END IF
!
           IUER = -1
           CALL READ_HEB_HEADER ( FIL_OH, HEB_OH, IUER )
           IF ( IUER  .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6105, IUER, 'MALO_TEST', 'Error in reading '// &
     &              'heb-file '//FIL_OH )
               RETURN 
           END IF
!
           IUER = -1
           CALL READ_HEB_DATA ( FIL_OH, HEB_OH, IUER )
           IF ( IUER  .NE. 0 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 6106, IUER, 'MALO_TEST', 'Error in reading '// &
     &               'heb-file '//FIL_OH )
                 RETURN 
           END IF
!
           IUER = -1
           CALL MALO_COMP_SPR ( HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH, &
     &                          MAL(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6107, IUER, 'MALO_TEST', 'Error in an attempt '// &
     &               'to compute surface atmospheric pressure' )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( TEST1 .GE. 1 .AND. TEST1 .LE. 4 ) THEN
           IUER = -1
           CALL SPR_READ_NC ( FIL1, MAL(1), IVRB, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE IF ( TEST1 .EQ. 5 ) THEN
           IUER = -1
           CALL LOA_INQ  ( FIL1, NLON, NLAT, MJD, TAI, M_TXT, L_TXT, C_TXT, &
     &                     DATA_OFFS, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
           ALLOCATE ( DSPL_ARR_R4(3,NLON,NLAT) )
           ALLOCATE ( DSPL_GRID_R4(NLON,NLAT,3) )
!
           IUER = -1
           CALL LOA_READ ( FIL1, NLON, NLAT, DATA_OFFS, DSPL_ARR_R4, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
           DO 410 J1=1,NLAT
              DO 420 J2=1,NLON
                 DO 430 J3=1,3
                    DSPL_GRID_R4(J2,J1,J3) = 1000.0D0*DSPL_ARR_R4(J3,J2,J1)
 430             CONTINUE 
 420          CONTINUE 
 410       CONTINUE 
         ELSE IF ( TEST1 .EQ. 6  .OR. TEST1 .EQ. 7 ) THEN
           IUER = -1
!
           IL = ILEN(FIL1)
           IF ( IL < 4 ) IL = 4
           IF ( FIL1(IL-3:IL) == '.bz2' ) THEN
                CALL GETINFO_HOST ( INTERNET_HOSTNAME )
                PID = GETPID()
                CALL INCH ( PID, FILTMP(1:8) )
                CALL CHASHR    ( FILTMP(1:8) )
                CALL BLANK_TO_ZERO ( FILTMP(1:8) )
                IF ( INTERNET_HOSTNAME == 'astrogeo' ) THEN
                     FILTMP = '/oper/temp/'//FILTMP(1:8)//'.heb'
                  ELSE IF ( INTERNET_HOSTNAME == 'terra' ) THEN
                     FILTMP = '/f0/temp/'//FILTMP(1:8)//'.heb'
                END IF
!
                FL_BZIP2 = .TRUE.
                COM_STR = 'pbzip2 -m1024 -S4096 -dfc '//FIL1(1:I_LEN(FIL1))// &
     &                ' > '//FILTMP
                IS = SYSTEM ( COM_STR(1:I_LEN(COM_STR)) )
                IF ( IS .NE. 0 ) THEN
                     CALL CLRCH  ( STR )
                     CALL GERROR ( STR )
                     IUER = -1
                     CALL ERR_LOG ( 6241, IUER, 'MALO_TEST_MAIN', 'Failure '// &
     &                   'to parse the header of the input heb-file '//FIL1 )
                     IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                FL_BZIP2 = .FALSE.
                FILTMP = FIL1
           END IF
!
           CALL READ_HEB_HEADER ( FILTMP, HEB, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
           IF ( TEST2 == 99 ) THEN
                WRITE ( 6, '(A,(I18,1X,I18,1X,I18,1X,I18))' ) 'Dims:             ', HEB%DIMS(1), HEB%DIMS(2), HEB%DIMS(3), HEB%DIMS(4)
                WRITE ( 6, '(A,A)' )   'Endian:           ', HEB%ENDIAN
                WRITE ( 6, '(A,A)' )   'Data_Format:      ', HEB%DATA_FORMAT
                WRITE ( 6, '(A,I4)' )  'Data_Offset:      ', HEB%DATA_OFFSET
                WRITE ( 6, '(A,I18)' ) 'Data_Length:      ', HEB%DATA_LENGTH
                WRITE ( 6, '(A,A)' )   'Data_Transform:   ', HEB%DATA_TRANSFORM
                WRITE ( 6, '(A,A)' )   'Data_Compression: ', HEB%DATA_COMPRESSION
                WRITE ( 6, '(A,1PE14.7)' ) 'Fill_Value:       ', HEB%FILL_VALUE
                WRITE ( 6, '(A,1PE14.7)' ) 'Scale_Factor:     ', HEB%SCALE_FACTOR
                WRITE ( 6, '(A,1PE14.7)' ) 'Offset:           ', HEB%OFFSET
                WRITE ( 6, '(A,A)' ) 'SDS_name:         ', HEB%SDS_NAME(1:I_LEN(HEB%SDS_NAME))
                WRITE ( 6, '(A,A)' ) 'Units:            ', HEB%UNITS(1:I_LEN(HEB%UNITS))
                WRITE ( 6, '(A,A)' ) 'Prod_name:        ', HEB%PROD_NAME(1:I_LEN(HEB%PROD_NAME))
                WRITE ( 6, '(A,A)' ) 'File_name:        ', HEB%FILE_NAME(1:I_LEN(HEB%FILE_NAME))
                WRITE ( 6, '(A,A)' ) 'History:          ', HEB%HISTORY(1:I_LEN(HEB%HISTORY))
                WRITE ( 6, '(A,A)' ) 'Source:           ', HEB%SOURCE(1:I_LEN(HEB%SOURCE))
                WRITE ( 6, '(A,A)' ) 'Title:            ', HEB%TITLE(1:I_LEN(HEB%TITLE))
                WRITE ( 6, '(A,A)' ) 'Institution:      ', HEB%INSTITUTION(1:I_LEN(HEB%INSTITUTION))
                WRITE ( 6, '(A,A)' ) 'References:       ', HEB%REFERENCES(1:I_LEN(HEB%REFERENCES))
                WRITE ( 6, '(A,A)' ) 'Prod_date_time:   ', HEB%PROD_DATE_TIME(1:I_LEN(HEB%PROD_DATE_TIME))
                WRITE ( 6, '(A,A)' ) 'Version_ID:       ', HEB%VERSION_ID(1:I_LEN(HEB%VERSION_ID))
                WRITE ( 6, '(A,I5,1X,F7.1)' ) 'MJD_UTC:          ', HEB%MJD, HEB%UTC
                WRITE ( 6, '(A,1PE14.7)' ) 'Min_Value:        ', HEB%MIN_VALUE
                WRITE ( 6, '(A,1PE14.7)' ) 'Max_Value:        ', HEB%MAX_VALUE
                WRITE ( 6, '(A,1PE14.7,1X,1PE14.6)' ) 'Valid_range:    ', HEB%VALID_RANGE
                CALL EXIT ( 0 ) 
           END IF
           HEB%TAI = HEB%UTC
!
           IUER = -1
           CALL READ_HEB_DATA ( FILTMP, HEB, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
!!    heb%val = (heb%val - 101300.0) ! %%%%%%%%%%%%%%%%%%%%%%%%
           IF ( TEST1 .EQ. 7 ) THEN
                IUER = -1
                CALL READ_HEB_HEADER ( FIL2, HEB2, IUER )
                IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
                IUER = -1
                CALL READ_HEB_DATA ( FIL2, HEB2, IUER )
                IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
!!                HEB%VAL = HEB%VAL - HEB2%VAL
                DO 510 J1=1,HEB%DIMS(2)
                   DO 520 J2=1,HEB%DIMS(1)
                      ILON = J2 + HEB%DIMS(1)/2 
                      IF ( ILON > HEB%DIMS(1) ) ILON = ILON - HEB%DIMS(1)
!!                      HEB%VAL(J2,J1,1,1) = HEB%VAL(J2,J1,1,1) - HEB2%VAL(ILON,J1,1,1)
                      HEB%VAL(J2,J1,1,1) = HEB%VAL(J2,J1,1,1) - HEB2%VAL(J2,J1,1,1)
 520               CONTINUE 
 510            CONTINUE 
          END IF
        ELSE IF ( TEST1 .EQ. 8 ) THEN
          CONTINUE 
      END IF
!
      IF ( TEST1 == 3 .OR. TEST1 == 4 ) THEN
!
! -------- Read mean surface pressure
!
           IUER = -1
           CALL SPR_READ_NC ( FIL2, MAL(2), IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6108, -2, 'MALO_TEST', 'Error in '// &
     &              'an attempt to read nc-file with mean surphase '// &
     &              'pressure '//FIL2 )
                CALL EXIT ( 1 )
           END IF   
      END IF
!
      IF ( TEST1 == 6 .OR. TEST1 == 7 ) THEN
           IUER = -2
           MAL(1)%SPR_LONG_NAME = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
           MAL(1)%SPR_LONG_NAME = HEB%SDS_NAME(1:I_LEN(HEB%SDS_NAME))// &
     &                           ' at '//MAL(1)%SPR_LONG_NAME(1:16)
           MAL(1)%SPR_UNITS = HEB%UNITS
           IUER = -1
           IF ( TEST1 == 6  ) THEN
                IF ( TEST2 > 100 .AND. TEST2 < 200   ) THEN
                     IND_FRQ = (TEST2-101)/2 + 1
                     IF ( IND_FRQ > M_FRQ ) IND_FRQ = 1
                     IF ( IND_FRQ <  1    ) IND_FRQ = 1
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of the pressure at freq '// &
     &                                       FRQ_NAM(IND_FRQ)
                     CALL HEB_ABS  ( HEB%DIMS(1)*HEB%DIMS(2), &
     &                               HEB%VAL(1,1,TEST2-100,1), HEB%VAL(1,1,TEST2-101,1)  )
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,TEST2-100,1), IUER )
                  ELSE IF ( TEST2 > 200 .AND. TEST2 < 300 ) THEN
                     IND_FRQ = (TEST2-201)/2 + 1
                     IF ( IND_FRQ > 21 ) IND_FRQ = 1
                     IF ( IND_FRQ <  1 ) IND_FRQ = 1
                     CALL HEB_ATAN ( HEB%DIMS(1)*HEB%DIMS(2), &
     &                               HEB%VAL(1,1,TEST2-200,1), HEB%VAL(1,1,TEST2-201,1)  )
                     MAL(1)%SPR_LONG_NAME = 'Phase of the pressure at freq '// &
     &                                      FRQ_NAM(IND_FRQ)
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,TEST2-200,1), IUER )
                  ELSE IF ( TEST2 > 300 .AND. TEST2 < 400 ) THEN
                     IND_FRQ = TEST2-300
                     WAVE_STR = ','//HEB%SDS_NAME(19:)
                     IP = MULTI_INDEX ( IND_FRQ, WAVE_STR, ',' )
                     IF ( IP < 1 ) THEN
                          WAVE_NAME = 'unknown'
                        ELSE
                          WAVE_NAME = WAVE_STR(IP+1:)
                          IP = INDEX ( WAVE_NAME, ',' )
                          IF ( IP > 0 ) CALL CLRCH ( WAVE_NAME(IP:) )
                     END IF
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of the ocean tide at freq '// &
     &                                       WAVE_NAME
                     CALL HEB_ABS  ( HEB%DIMS(1)*HEB%DIMS(2), &
     &                               HEB%VAL(1,1,1,IND_FRQ), HEB%VAL(1,1,2,IND_FRQ)  )
                     HEB_OUT = HEB
                     HEB_OUT%DIMS(3) = 1
                     HEB_OUT%DIMS(4) = 1
                     CALL HEB_MINMAX ( HEB_OUT, HEB%VAL, 0.99*HEB%FILL_VALUE )
                     HEB_OUT%TITLE  = MAL(1)%SPR_LONG_NAME
!
                     IUER = -1 
                     CALL WRITE_HEB ( HEB_OUT, HEB%VAL, FILOUT, IUER )
                     IF ( IUER .NE. 0 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 6808, IUER, 'MALO_TEST_MAIN', 'Failure '// &
     &                        'to write the output HEB file '//FILOUT )
                          CALL EXIT ( 1 )
                     END IF
                     WRITE ( 6, * ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
!                     
                     IUER = -1 
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,1,IND_FRQ), IUER )
                  ELSE IF ( TEST2 > 400 .AND. TEST2 < 500 ) THEN
                     IND_FRQ = TEST2-400
                     WAVE_STR = ','//HEB%SDS_NAME(19:)
                     IP = MULTI_INDEX ( IND_FRQ, WAVE_STR, ',' )
                     IF ( IP < 1 ) THEN
                          WAVE_NAME = 'unknown'
                        ELSE
                          WAVE_NAME = WAVE_STR(IP+1:)
                          IP = INDEX ( WAVE_NAME, ',' )
                          IF ( IP > 0 ) CALL CLRCH ( WAVE_NAME(IP:) )
                     END IF
                     MAL(1)%SPR_LONG_NAME = 'Phase of the ocean tide at freq '// &
     &                                       WAVE_NAME
                     CALL HEB_ATAN ( HEB%DIMS(1)*HEB%DIMS(2), &
     &                               HEB%VAL(1,1,1,IND_FRQ), HEB%VAL(1,1,2,IND_FRQ)  )
                     CALL GETENVAR ( 'MALO_REVERSE_SIGN', STR )
                     IF ( STR == 'YES'  .OR.  STR == 'yes' ) THEN
                          WRITE ( 6, * ) 'Reverse the sign'
                          HEB%VAL = -HEB%VAL
                     END IF
                     MAL(1)%SPR_UNITS = 'rad'
                     HEB%UNITS= 'rad'
                     HEB_OUT = HEB
                     HEB_OUT%DIMS(3) = 1
                     HEB_OUT%DIMS(4) = 1
                     CALL HEB_MINMAX ( HEB_OUT, HEB%VAL, 0.99*HEB%FILL_VALUE )
                     HEB_OUT%SDS_NAME  = MAL(1)%SPR_LONG_NAME
!
                     IUER = -1 
                     CALL WRITE_HEB ( HEB_OUT, HEB%VAL, FILOUT, IUER )
                     IF ( IUER .NE. 0 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 6809, IUER, 'MALO_TEST_MAIN', 'Failure '// &
     &                        'to write the output HEB file '//FILOUT )
                          CALL EXIT ( 1 )
                     END IF
                     WRITE ( 6, * ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
!
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,1,IND_FRQ), IUER )
                  ELSE IF ( TEST2 > 500 .AND. TEST2 < 600 ) THEN
                     IND_FRQ = TEST2-500
                     WAVE_STR = ','//HEB%SDS_NAME(19:)
                     IP = MULTI_INDEX ( IND_FRQ, WAVE_STR, ',' )
                     IF ( IP < 1 ) THEN
                          WAVE_NAME = 'unknown'
                        ELSE
                          WAVE_NAME = WAVE_STR(IP+1:)
                          IP = INDEX ( WAVE_NAME, ',' )
                          IF ( IP > 0 ) CALL CLRCH ( WAVE_NAME(IP:) )
                     END IF
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of the ocean tide at freq '// &
     &                                       WAVE_NAME
                     CALL HEB_ABS ( HEB%DIMS(1)*HEB%DIMS(2), &
     &                               HEB%VAL(1,1,1,IND_FRQ), HEB%VAL(1,1,2,IND_FRQ)  )
                     MAL(1)%SPR_UNITS = HEB%UNITS
                     HEB_OUT = HEB
                     HEB_OUT%DIMS(3) = 1
                     HEB_OUT%DIMS(4) = 1
                     CALL HEB_MINMAX ( HEB_OUT, HEB%VAL, 0.99*HEB%FILL_VALUE )
                     HEB_OUT%SDS_NAME  = MAL(1)%SPR_LONG_NAME
!
                     IUER = -1 
                     CALL WRITE_HEB ( HEB_OUT, HEB%VAL, FILOUT, IUER )
                     IF ( IUER .NE. 0 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 6809, IUER, 'MALO_TEST_MAIN', 'Failure '// &
     &                        'to write the output HEB file '//FILOUT )
                          CALL EXIT ( 1 )
                     END IF
                     WRITE ( 6, * ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
!
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,1,IND_FRQ), IUER )
                  ELSE IF ( TEST2 > 900 .AND. TEST2 < 942 ) THEN
                     IND_FRQ = (TEST2-901)/2 + 1
                     IF ( IND_FRQ > M_FRQ ) IND_FRQ = 1
                     IF ( IND_FRQ <  1    ) IND_FRQ = 1
                     IF ( MOD(TEST2,2) == 1 ) CS_NAME = 'cos'
                     IF ( MOD(TEST2,2) == 0 ) CS_NAME = 'sin'
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of '//CS_NAME//' component of pressure '// &
     &                                      'at freq '//FRQ_NAM(IND_FRQ)
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,TEST2-900,1), IUER )
                  ELSE IF ( TEST2 .GE. 1101 .AND. TEST2 .LE. 1341   ) THEN
                     IND_CMP = (TEST2-1000)/100
                     IND_FRQ = (TEST2-1001-100*IND_CMP)/2 + 1
                     IND_PAR = (TEST2-1001-100*IND_CMP) + 1 
                     IF ( IND_FRQ > M_FRQ ) IND_FRQ = 1
                     IF ( IND_FRQ <  1    ) IND_FRQ = 1
                     WRITE ( 6, * ) ' IND_CMP = ', INT2(IND_CMP), &
     &                              ' IND_FRQ = ', INT2(IND_FRQ), &
     &                              ' IND_PAR = ', INT2(IND_PAR)
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of the '// &
     &                         CMP_NAM(IND_CMP)(1:I_LEN(CMP_NAM(IND_CMP)))// &
     &                         ' atmospheric tide loading at freq '// &
     &                                       FRQ_NAM(IND_FRQ)
                     MAL(1)%SPR_UNITS = 'mm'
                     ALLOCATE ( MAL(1)%LSM(HEB%DIMS(2),HEB%DIMS(3)) )
                     WRITE  ( 6, * ) ' IND_CMP= ', IND_CMP, ' IND_FRQ= ', IND_FRQ
                     CALL GET_DSPL_AMP ( HEB, IND_CMP, IND_PAR, MAL(1)%LSM )
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), &
     &                                HEB%DIMS(2), HEB%DIMS(3), HEB%MJD, &
     &                                HEB%TAI, MAL(1)%LSM, IUER )
                  ELSE IF ( TEST2 .GE. 2101 .AND. TEST2 .LE. 2341   ) THEN
                     IND_CMP = (TEST2-2000)/100
                     IND_FRQ = (TEST2-2001-100*IND_CMP)/2 + 1
                     IND_PAR = (TEST2-2001-100*IND_CMP) + 1 
                     IF ( MOD(TEST2,2) == 1 ) IND_CMP = 1
                     IF ( MOD(TEST2,2) == 0 ) IND_CMP = 2
                     IF ( IND_FRQ > M_FRQ ) IND_FRQ = 1
                     IF ( IND_FRQ <  1    ) IND_FRQ = 1
                     WRITE ( 6, * ) ' IND_CMP = ', INT2(IND_CMP), &
     &                              ' IND_FRQ = ', INT2(IND_FRQ), &
     &                              ' IND_PAR = ', INT2(IND_PAR)
                     MAL(1)%SPR_LONG_NAME = 'Amplitude of the '// &
     &                         CMP_NAM(IND_CMP)(1:I_LEN(CMP_NAM(IND_CMP)))// &
     &                         ' atmospheric tide loading at freq '// &
     &                                       FRQ_NAM(IND_FRQ)//' '//CS_NAM(IND_CMP)
                     MAL(1)%SPR_UNITS = 'mm'
                     ALLOCATE ( MAL(1)%LSM(HEB%DIMS(2),HEB%DIMS(3)) )
                     WRITE  ( 6, * ) ' IND_CMP= ', IND_CMP, ' IND_FRQ= ', IND_FRQ
                     CALL GET_DSPL ( HEB, IND_CMP, IND_PAR, MAL(1)%LSM )
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), &
     &                                HEB%DIMS(2), HEB%DIMS(3), HEB%MJD, &
     &                                HEB%TAI, MAL(1)%LSM, IUER )
                  ELSE 
                     CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                                HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                                HEB%VAL(1,1,TEST2,1), IUER )
                END IF
              ELSE IF ( TEST1 == 7 ) THEN
                CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), HEB%DIMS(1), &
     &                           HEB%DIMS(2), HEB%MJD, HEB%TAI,  &
     &                           HEB%VAL(1,1,1,1), IUER )
           END IF
         ELSE IF ( TEST1 == 11 ) THEN
           IUER = -1
           CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), &
     &                      MAL(1)%NLON, MAL(1)%NLAT, &
     &                      MAL(1)%MJD_BEG, MAL(1)%TAI_BEG, MAL(1)%SPR, IUER )
         ELSE 
           IUER = -1
           CALL MALO_TEST ( TEST1, TEST2, MAL(1), MAL(2), NLON, NLAT, &
     &                      MJD, TAI,  DSPL_GRID_R4, IUER )
      END IF
!
      END  PROGRAM    MALO_TEST_MAIN  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_ATAN ( NEL, C_ARR, S_ARR )
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*8  NEL
      REAL*4     C_ARR(NEL), S_ARR(NEL)
      REAL*4     VAL_MAX
      PARAMETER  ( VAL_MAX = 0.99D14 )
      INTEGER*8  J1
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
!
      DO 410 J1=1,NEL
         IF ( ABS(C_ARR(J1)) < VAL_MAX .AND. ABS(S_ARR(J1)) < VAL_MAX ) THEN
              C_ARR(J1) = ATAN_CS_R4 ( C_ARR(J1), S_ARR(J1) )
              IF ( C_ARR(J1) < 0.0 ) C_ARR(J1) = C_ARR(J1) + PI2
         END IF
 410  CONTINUE 
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_ABS ( NEL, C_ARR, S_ARR )
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*8  NEL
      REAL*4     C_ARR(NEL), S_ARR(NEL)
      REAL*4     VAL_MAX
      PARAMETER  ( VAL_MAX = 0.99D14 )
      INTEGER*8  J1
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
!
      DO 410 J1=1,NEL
         IF ( ABS(C_ARR(J1)) < VAL_MAX .AND. ABS(S_ARR(J1)) < VAL_MAX ) THEN
              C_ARR(J1) = SQRT ( C_ARR(J1)**2 +  S_ARR(J1)**2 )
         END IF
 410  CONTINUE 
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_DSPL ( HEB, ICMP, IND_PAR, ARR_R4 )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB
      INTEGER*4  ICMP, IND_PAR
      REAL*4     ARR_R4(HEB%DIMS(2),HEB%DIMS(3))
      INTEGER*4  J1, J2
!
      DO 410 J1=1,HEB%DIMS(3)
         DO 420 J2=1,HEB%DIMS(2)
            ARR_R4(J2,J1) = 1.E3*HEB%VAL(ICMP,J2,J1,IND_PAR)
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  GET_DSPL !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_DSPL_AMP ( HEB, ICMP, IND_PAR, ARR_R4 )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB
      INTEGER*4  ICMP, IND_PAR
      REAL*4     ARR_R4(HEB%DIMS(2),HEB%DIMS(3))
      INTEGER*4  J1, J2
!
      DO 410 J1=1,HEB%DIMS(3)
         DO 420 J2=1,HEB%DIMS(2)
            ARR_R4(J2,J1) = 1.0E3*SQRT ( HEB%VAL(ICMP,J2,J1,IND_PAR)**2 + &
     &                                   HEB%VAL(ICMP,J2,J1,IND_PAR+1)**2 )
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  GET_DSPL_AMP  !#!#
