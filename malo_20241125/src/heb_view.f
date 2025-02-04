      PROGRAM    HEB_VIEW
! ************************************************************************
! *                                                                      *
! *   Program  HEB_VIEW  generates lat-lon plot of a variable defined in *
! *   in the input fiole in HEB format.                                  *
! *                                                                      *
! *  ### 23-MAY-2013    HEB_VIEW  v3.0 (c)  L. Petrov   15-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB, HEB_CL
      CHARACTER  FILHEB*128, FIL_COAST_LINE_HEB*128, FILOUT*128, STR*128, &
     &           STR1*128, TITLE*128, WORD1*16, WORD2*16, WORD3*16, WORD4*16, &
     &           WORD5*16, WORD6*16, REG*4
      REAL*4     LAT_MIN, LAT_MAX, LON_MIN, LON_MAX, VAL_MIN, VAL_MAX, SHIFT
      LOGICAL*1  LEX
      REAL*4     SCALE
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      LOGICAL*1  FL_VIEW_LOOP
      INTEGER*8  LEN_MAX_I8
      PARAMETER  ( LEN_MAX_I8 = INT8(4)*INT8(1024)*INT8(1024)*INT8(1024) )
      INTEGER*4  IPAL, IPRC, IDEV, DIM3, IND3, IND4, IND(2,MIND), &
     &           LIND, LN, IND3_SECT(2), IND4_SECT(2), IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ_LINE 
      CHARACTER, EXTERNAL ::   MJDSEC_TO_DATE*30
!
      IND3 = 1
      IND4 = 1
      CALL CLRCH ( FILOUT )
      IF ( IARGC() == 1 ) THEN
           CALL GETARG ( 1, FILHEB )
           LAT_MIN = -90.0
           LAT_MAX =  90.0
           LON_MIN =   0.0
           LON_MAX = 360.0
           VAL_MIN =  0.0
           VAL_MAX = -10.0
           IPRC = 1
           IDEV = 1
           FILOUT = '/tmp/foo'
         ELSE IF ( IARGC() < 8 ) THEN
           WRITE ( 6, '(A)' ) 'heb_view filheb lat_min lat_max lon_min '// &
     &                        'lon_max val_min val_max iprc [filout] [ind3] [ind4]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILHEB )
!
           CALL GETARG ( 2, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) LAT_MIN
!
           CALL GETARG ( 3, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) LAT_MAX
!
           CALL GETARG ( 4, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) LON_MIN
!
           CALL GETARG ( 5, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) LON_MAX
!
           CALL GETARG ( 6, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) VAL_MIN
!
           CALL GETARG ( 7, STR  )
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.5)' ) VAL_MAX
           CALL GETARG ( 8, STR    )
           CALL CHIN   ( STR, IPRC )
!
           IF ( IARGC() .GE. 9 ) THEN
                CALL GETARG ( 9, FILOUT )
                IF ( INDEX ( FILOUT, '.ps'  ) > 0 ) THEN
                     IDEV = 2
                     CALL CLRCH ( FILOUT(ILEN(FILOUT)-2:) )
                  ELSE IF ( INDEX ( FILOUT, '.eps'  ) > 0 ) THEN
                     IDEV = 2
                     CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                  ELSE IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                     IDEV = 3
                     CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                  ELSE IF ( FILOUT == 'XW' .OR. FILOUT == '/XW' .OR. &
     &                      FILOUT == 'xw' .OR. FILOUT == '/xw'      ) THEN
                     IDEV = 1
                     FILOUT = '/tmp/foo'
                  ELSE 
                     IUER = -2
                     CALL ERR_LOG ( 6701, IUER, 'HEB_VIEW', 'Output '// &
     &                   'file should have extension .ps or .gif' )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                IDEV = 1
                FILOUT = '/tmp/foo'
           END IF
!
           IF ( IARGC() .GE. 10 ) THEN
                CALL GETARG ( 10, STR )
                CALL CHIN ( STR, IND3 )
                IF ( IND3 < 1 ) IND3 = 1
           END IF
!
           IF ( IARGC() .GE. 11 ) THEN
                CALL GETARG ( 11, STR )
                CALL CHIN ( STR, IND4 )
                IF ( IND4 < 1 ) IND4 = 1
           END IF
      END IF
!
      INQUIRE ( FILE=FILHEB, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -2
           CALL ERR_LOG ( 6702, IUER, 'HEB_VIEW', 'Input file '// &
     &          FILHEB(1:I_LEN(FILHEB))//' is not found' )
           CALL EXIT ( 1 )
      END IF
      IF ( LAT_MIN < -90.0 ) THEN
           IUER = -2
           CALL GETARG ( 2, STR )
           CALL ERR_LOG ( 6703, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lat_min: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'less than -90.0' )
           CALL EXIT ( 1 )
      END IF 
      IF ( LAT_MAX >  90.0 ) THEN
           IUER = -2
           CALL GETARG ( 3, STR )
           CALL ERR_LOG ( 6704, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lat_max: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'greater than 90.0' )
           CALL EXIT ( 1 )
      END IF 
      IF ( LAT_MIN .GE. LAT_MAX ) THEN
           IUER = -2
           CALL GETARG ( 2, STR )
           CALL GETARG ( 3, STR1 )
           CALL ERR_LOG ( 6705, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lat_min: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'greater than lat_max '//STR1 )
           CALL EXIT ( 1 )
      END IF 
      IF ( LON_MIN < -180.0 ) THEN
           IUER = -2
           CALL GETARG ( 4, STR )
           CALL ERR_LOG ( 6706, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lat_min: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'less than -180.0' )
           CALL EXIT ( 1 )
      END IF 
      IF ( LON_MAX > 360.0 ) THEN
           IUER = -2
           CALL GETARG ( 5, STR )
           CALL ERR_LOG ( 6707, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lat_max: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'greater than 360.0' )
           CALL EXIT ( 1 )
      END IF 
      IF ( LON_MIN .GE. LON_MAX ) THEN
           IUER = -2
           CALL GETARG ( 4, STR )
           CALL GETARG ( 5, STR1 )
           CALL ERR_LOG ( 6708, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lon_min: '//STR(1:I_LEN(STR))//' -- it should not be '// &
     &         'greater than lon_max '//STR1 )
           CALL EXIT ( 1 )
      END IF 
      IF ( LON_MAX - LON_MIN > 360.0 ) THEN
           IUER = -2
           CALL GETARG ( 4, STR )
           CALL GETARG ( 5, STR1 )
           CALL ERR_LOG ( 6709, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'lon_min: '//STR(1:I_LEN(STR))//' -- the difference wrt '// &
     &         'lon_max '//STR1(1:I_LEN(STR1))//' should not exceed '// &
     &         '360.0 degrees' )
           CALL EXIT ( 1 )
      END IF 
      IF ( IPRC .NE. 1  .AND.  IPRC .NE. 2  .AND.  &
     &     IPRC .NE. -2 .AND.  IPRC .NE. 3  .AND.  IPRC .NE. 4 ) THEN
           IUER = -2
           CALL GETARG  ( 8, STR )
           CALL ERR_LOG ( 6710, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'IPRC: '//STR(1:I_LEN(STR))//' -- only values 1 or 2, -2, or 3'// &
     &         ' are supported' )
           CALL EXIT ( 1 )
      END IF 
      CALL GETENVAR ( 'MALO_PLOT_SHIFT', STR )
!
      IUER = -1
      CALL READ_HEB_HEADER ( FILHEB, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6711, IUER, 'HEB_VIEW', 'Error in reading '// &
     &         'input heb-file '//FILHEB )
           CALL EXIT ( 1 )
      END IF 
!
      IF ( IND3 > HEB%DIMS(3) ) THEN
           IUER = -2
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND3, STR1 )
           CALL INCH8 ( HEB%DIMS(3), STR1 )
           CALL ERR_LOG ( 6712, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'IND3: '//STR(1:I_LEN(STR))//' it is greater than '// &
     &         'DIMS(3): '//STR1(1:I_LEN(STR1))//' of the '// &
     &         'input heb-file '//FILHEB )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IND4 > HEB%DIMS(4) ) THEN
           IUER = -2
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND4, STR1 )
           CALL INCH8 ( HEB%DIMS(3), STR1 )
           CALL ERR_LOG ( 6713, IUER, 'HEB_VIEW', 'Wrong parameter '// &
     &         'IND4: '//STR(1:I_LEN(STR))//' it is greater than '// &
     &         'DIMS(4): '//STR1(1:I_LEN(STR1))//' of the '// &
     &         'input heb-file '//FILHEB )
           CALL EXIT ( 1 )
      END IF
!
      IF ( HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4) < LEN_MAX_I8 ) THEN
           IUER = -1
           CALL READ_HEB ( FILHEB, HEB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 6714, IUER, 'HEB_VIEW', 'Error in reading '// &
     &              'input heb-file '//FILHEB )
                CALL EXIT ( 1 )
           END IF 
         ELSE
           IND3_SECT(1) = IND3
           IND3_SECT(2) = IND3
           IND4_SECT(1) = IND4
           IND4_SECT(2) = IND4
           IUER = -1
           CALL READ_HEB_SECT ( FILHEB, HEB, IND3_SECT, IND4_SECT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 6715, IUER, 'HEB_VIEW', 'Error in reading '// &
     &              'input heb-file '//FILHEB )
                CALL EXIT ( 1 )
           END IF 
           CALL GETENVAR ( 'HEB_VIEW_AMPL', STR1 )
           IF ( STR1 == 'YES' .OR. STR1  == 'yes' ) THEN
                CALL READ_HEB_SECT ( FILHEB, HEB_CL, IND3_SECT, IND4_SECT+1, IUER )
                HEB%VAL(1:HEB%DIMS(1),1:HEB%DIMS(2),1,1) = SQRT ( HEB%VAL(1:HEB%DIMS(1),1:HEB%DIMS(2),1,1)**2 + &
     &                                                            HEB_CL%VAL(1:HEB%DIMS(1),1:HEB%DIMS(2),1,1)**2   )
           END IF
           IND3 = 1
           IND4 = 1
      END IF 
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SHIFT
           HEB%VAL = HEB%VAL + SHIFT
           VAL_MIN = MINVAL ( HEB%VAL )
           VAL_MAX = MAXVAL ( HEB%VAL )
      END IF
      IF ( VAL_MIN .GE. VAL_MAX ) THEN
           VAL_MIN = HEB%MIN_VALUE
           VAL_MAX = HEB%MAX_VALUE
      END IF
      CALL GETENVAR ( 'HEB_VIEW_COAST_LINE', FIL_COAST_LINE_HEB )
      IF ( ILEN(FIL_COAST_LINE_HEB) > 0 ) THEN
           CALL READ_HEB ( FIL_COAST_LINE_HEB, HEB_CL, IUER )
      END IF
!
      IPAL = 7
      IUER = -1
      CALL GETENVAR ( 'HEB_VIEW_SCALE', STR )
      IF ( ILEN(STR) > 0 .AND. ASSOCIATED ( HEB%VAL) ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) SCALE
           HEB%VAL = SCALE*HEB%VAL
           VAL_MIN = SCALE*VAL_MIN
           VAL_MAX = SCALE*VAL_MAX
      END IF 
      CALL GETENVAR ( 'HEB_VIEW_LOOP', STR )
      IF ( STR == 'NO' ) THEN
           FL_VIEW_LOOP = .FALSE.
         ELSE 
           FL_VIEW_LOOP = .TRUE.
      END IF 
      STR = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
      TITLE = HEB%TITLE(1:I_LEN(HEB%TITLE))//' at '//STR(1:16)
 710  CONTINUE 
      IUER = -1
      IF ( IPRC .EQ. 1 .OR. IPRC == 2 ) THEN
           IF ( HEB%DATA_FORMAT    == HEB__I1   .AND. &
     &          HEB%DATA_TRANSFORM == HEB__NONE       ) THEN
                CALL PLOT_REGION_R1 ( IDEV, IPAL, IPRC, INT(HEB%DIMS(1),KIND=4),  &
     &                                                  INT(HEB%DIMS(2),KIND=4),  &
     &                                HEB%VAL1(1,1,IND3,IND4), TITLE,  HEB%UNITS, &
     &                                FILOUT, LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                                VAL_MIN, VAL_MAX, IUER )
              ELSE
                IF ( HEB%DATA_FORMAT == HEB__R8 .AND. ASSOCIATED ( HEB%VAL8 ) ) THEN
                     ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &                          STAT=IUER )
                     IF ( IUER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH8 ( INT8(4)*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
                          IUER = -2
                          CALL ERR_LOG ( 6716, IUER, 'HEB_VIEW', 'Failure to allocate '// &
     &                         STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &                         'HEB_OUT%VAL4' )
                          CALL EXIT ( 1 )
                     END IF 
                     CALL SPD_R8_TO_R4 ( HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), HEB%VAL8, HEB%VAL )
                     DEALLOCATE ( HEB%VAL8 )
                END IF 
!
                IF ( ILEN(FIL_COAST_LINE_HEB) == 0 ) THEN
                     CALL PLOT_REGION_R4 ( IDEV, IPAL, IPRC, INT(HEB%DIMS(1),KIND=4),  &
     &                                                       INT(HEB%DIMS(2),KIND=4),  &
     &                                     HEB%VAL(1,1,IND3,IND4), TITLE,  HEB%UNITS,  &
     &                                     FILOUT, LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                                     VAL_MIN, VAL_MAX, IUER )
                   ELSE
                     CALL PLOT_REGION_COA_R4 ( IDEV, IPAL, IPRC, 0, &
     &                                         INT(HEB%DIMS(1),KIND=4),  &
     &                                         INT(HEB%DIMS(2),KIND=4),  &
     &                                         HEB%VAL(1,1,IND3,IND4), HEB_CL%VAL(1,1,IND3,IND4), %VAL(0), &
     &                                         TITLE,  HEB%UNITS, FILOUT, &
     &                                         LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                                         VAL_MIN, VAL_MAX, IUER )
                END IF
           END IF
         ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
!!         TITLE = 'Geoid undulations'
           IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
                ALLOCATE (   HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)) )
                HEB%VAL = HEB%VAL1
                DEALLOCATE ( HEB%VAL1 )
             ELSE IF ( HEB%DATA_FORMAT == HEB__I2 ) THEN
                ALLOCATE (   HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)) )
                HEB%VAL = HEB%VAL2
                DEALLOCATE ( HEB%VAL2 )
           END IF
           CALL PLOT_GRID_R4 ( IDEV, IPAL, 0, IPRC, &
     &                         INT(HEB%DIMS(1),KIND=4), INT(HEB%DIMS(2),KIND=4), &
     &                         HEB%VAL(1,1,IND3,IND4), TITLE, HEB%UNITS, &
     &                         VAL_MIN, VAL_MAX, FILOUT, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6717, IUER, 'HEB_VIEW', 'Error in an attempt '// &
     &         'to generate a plot' )
           CALL EXIT ( 1 )
      END IF 
      IF ( IDEV == 1 .AND. FL_VIEW_LOOP ) THEN
 720       CONTINUE 
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
           IF ( LIND .GE. 5 ) THEN
                WORD5 = STR(IND(1,5):IND(2,5))
                IF ( INDEX ( WORD5, '.' ) == 0 ) WORD5 = WORD5(1:I_LEN(WORD5))//'.'
                READ ( UNIT=WORD5, FMT='(F15.7)', IOSTAT=IUER ) VAL_MIN
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong val_min'
                     GOTO 720
                END IF
           END IF
!
           IF ( LIND .GE. 6 ) THEN
                WORD6 = STR(IND(1,6):IND(2,6))
                IF ( INDEX ( WORD6, '.' ) == 0 ) WORD6 = WORD6(1:I_LEN(WORD6))//'.'
                READ ( UNIT=WORD6, FMT='(F15.7)', IOSTAT=IUER ) VAL_MAX
                IF ( IUER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'Wrong val_max'
                     GOTO 720
                END IF
           END IF
           GOTO 710
      END IF
      END  PROGRAM  HEB_VIEW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOT_GRID_R4 ( IDEV, IPAL, ISCL, IPRC, NLON, NLAT, ARR_R4, &
     &                          TITLE, UNIT, VAL_MIN, VAL_MAX, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLOT_GRID_R4 
! *                                                                      *
! * ### 09-OCT-2012   PLOT_GRID_R4  v1.5 (c)  L. Petrov 03-DEC-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INTEGER*4  IDEV, IPAL, ISCL, IPRC, NLON, NLAT, IUER
      CHARACTER  TITLE*(*), UNIT*(*), FILOUT*(*)
      CHARACTER  UFILOUT*128
      INTEGER*4  MP, ML
      PARAMETER  ( MP = 512 ) 
      PARAMETER  ( ML = 4 ) 
      REAL*4     ARR_R4(NLON,NLAT), VAL_MIN, VAL_MAX
      INTEGER*4  INIT_COL, NCOL, NLAB, IVAL_MIN, IVAL_MAX
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      CHARACTER  STR*128, FINAM*128, CH*4, STR_RVAL_MIN*20, STR_RVAL_MAX*20
      REAL*4     XCOEF_IMA, XCOEF_PAP, LONB_R4, LONE_R4, LATB_R4, LATE_R4, &
     &           PAP_SIZE, WHITE_CLR, XC, YC, XPR, YPR, LON, LAT, &
     &           PHI, LAM, LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &           XMIN, XMAX, YMIN, YMAX, XL, XR, YB, YT, RD, VAL_MAX_ALLOWED
      REAL*4     RVAL_MIN, RVAL_MAX, VAL_R4, ARR_X(8192), ARR_Y(8192), &
     &           LAT_ARR(MP), LON_ARR(MP)
      REAL*4     EPS
      REAL*4     RANGE(2)
      DATA       RANGE / -1.0E10, 1.0E10 /
      PARAMETER  ( EPS = 1.E-6 )
      LOGICAL*4  FL_PLOT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           NUM, IP, CI1, CI2, IX, IY, ID, ILON, UDEV, NR
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN, LINDEX
!
      VAL_MAX_ALLOWED = 0.99E+14
      UDEV = IDEV
      INIT_COL = 20
      NCOL = 200
      IF ( IPRC == 1 ) THEN
           NLAB = 16
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           NLAB =  5
         ELSE IF ( IPRC == 3 ) THEN
!
! -------- Southern pole
!
           LAT_MIN =  -P2I
           LAT_MAX = (-P2I + 30.0D0*DEG__TO__RAD )
           RD = COS(PI__NUM/4.0 - LAT_MAX/2.0)
!!           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.6225 ! Coefficient 0.6225 was found empirically to avoid mosiac
           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.60 ! Coefficient 0.6225 was found empirically to avoid mosiac
!!           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.58 ! Coefficient 0.58 was found empirically to avoid mosiac
           NLAB =  7
         ELSE IF ( IPRC == 4 ) THEN
!
! -------- Northern pole
!
           LAT_MIN = (P2I - 30.0D0*DEG__TO__RAD )
           LAT_MAX =  P2I
           RD = SIN(PI__NUM/4.0 - LAT_MIN/2.0)
           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.6225 ! Coefficient 0.6225 was found empirically to avoid mosiac
           NLAB =  7
      END IF 
!
      LONB_R4 =    0.0
      LONE_R4 =  360.0
      LATB_R4 =  -90.0
      LATE_R4 =   90.0
!
      IVAL_MIN =  1.0E9
      IVAL_MAX = -1.0E9
      RVAL_MIN =  1.0E9
      RVAL_MAX = -1.0E9
      IF ( IPRC == 1 ) THEN
           XCOEF_IMA = 0.75
           XCOEF_PAP = 0.75
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           XCOEF_IMA = 0.5
           XCOEF_PAP = 0.75
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           XCOEF_IMA = 1.0
           XCOEF_PAP = 1.0
      END IF
      IF ( IPRC == 1 .OR. IPRC == 2 .OR. IPRC == 2 ) THEN
           ALLOCATE ( MAP_I4(NLON,NLAT) )
         ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
           ALLOCATE ( MAP_I4(NR,NR) )
      END IF
      MAP_I4 = -9999
      DO 410 J1=1,NLAT
         IP = J1
         DO 420 J2=1,NLON
!
! --------- Find minimal and maximal value
!
            IF ( ABS(ARR_R4(J2,J1)) > VAL_MAX_ALLOWED ) GOTO 420
            IF ( ARR_R4(J2,J1) > RANGE(1) .AND. ARR_R4(J2,J1) < RANGE(2) ) THEN
                 IF ( ARR_R4(J2,J1) < RVAL_MIN ) THEN
                      RVAL_MIN = ARR_R4(J2,J1) 
                 END IF
!
                 IF ( ARR_R4(J2,J1) > RVAL_MAX ) THEN
                      RVAL_MAX = ARR_R4(J2,J1) 
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( ISCL == 0 ) THEN
           WRITE ( 6, * ) 'PGR RVAL_MIN/RVAL_MAX = ', RVAL_MIN, RVAL_MAX
         ELSE IF ( ISCL == 1 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 1.0
         ELSE IF ( ISCL == 2 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 1.5
         ELSE IF ( ISCL == 3 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 2.0
         ELSE IF ( ISCL == 4 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 3.0
         ELSE IF ( ISCL == 5 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 4.0
         ELSE IF ( ISCL == 6 ) THEN
           RVAL_MIN =  0.0
           RVAL_MAX =  1.0
         ELSE IF ( ISCL == 7 ) THEN
           RVAL_MIN =   5.0
           RVAL_MAX = 120.0
         ELSE IF ( ISCL == 8 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =  180.0
         ELSE IF ( ISCL == 10 ) THEN
           RVAL_MIN = -1.0
           RVAL_MAX =  1.0
         ELSE IF ( ISCL == 11 ) THEN
           RVAL_MIN =  0.0
           RVAL_MAX = 10.0
         ELSE IF ( ISCL == 12 ) THEN
           RVAL_MIN = -0.1
           RVAL_MAX =  0.1
         ELSE IF ( ISCL == 13 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX = 100.0
         ELSE IF ( ISCL == 14 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX = 1000.0
         ELSE IF ( ISCL == 15 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX =   0.2
         ELSE IF ( ISCL == 16 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX =   1.D9
         ELSE IF ( ISCL == 17 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =   90.0
         ELSE IF ( ISCL == 18 ) THEN
           RVAL_MIN =  -180.0
           RVAL_MAX =   180.0
         ELSE IF ( ISCL == 19 ) THEN
           RVAL_MIN =     0.0
           RVAL_MAX =     0.5
         ELSE IF ( ISCL == 20 ) THEN
           RVAL_MIN =      480.
           RVAL_MAX =     1080.0
         ELSE IF ( ISCL == 21 ) THEN
           RVAL_MIN =     0.
           RVAL_MAX =    30.
         ELSE IF ( ISCL == 22 ) THEN
           RVAL_MIN =    240.0
           RVAL_MAX =    320.0
        ELSE IF ( ISCL == 23 ) THEN
           RVAL_MIN =    -3.0
           RVAL_MAX =     5.0
        ELSE IF ( ISCL == 24 ) THEN
           RVAL_MIN =     0.0
           RVAL_MAX =     7.0
        ELSE IF ( ISCL == 25 ) THEN
           RVAL_MIN =      0.0
           RVAL_MAX =     30.0
         ELSE IF ( ISCL == 26 ) THEN
           RVAL_MIN =    50000.0
           RVAL_MAX =   110000.0
         ELSE IF ( ISCL == 27 ) THEN
           RVAL_MIN =  -10000.0
           RVAL_MAX =   10000.0
         ELSE IF ( ISCL == 28 ) THEN
           RVAL_MIN =  -5000.0
           RVAL_MAX =   5000.0
         ELSE IF ( ISCL == 29 ) THEN
           RVAL_MIN =  -300.0
           RVAL_MAX =   1000.0
         ELSE IF ( ISCL == 30 ) THEN
           RVAL_MIN =  -30.0
           RVAL_MAX =   30.0
         ELSE IF ( ISCL == 31 ) THEN
           RVAL_MIN =  -10.0
           RVAL_MAX =   10.0
         ELSE IF ( ISCL == 32 ) THEN
           RVAL_MIN =  -0.05
           RVAL_MAX =   0.15
         ELSE IF ( ISCL == 33 ) THEN
           RVAL_MIN =  280.0
           RVAL_MAX =  300.0
         ELSE IF ( ISCL == 34 ) THEN
           RVAL_MIN =    0
           RVAL_MAX =  500.0
         ELSE IF ( ISCL == 35 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =    25000.0
         ELSE IF ( ISCL == 36 ) THEN
           RVAL_MIN =    -15.
           RVAL_MAX =     15.
         ELSE IF ( ISCL == 37 ) THEN
           RVAL_MIN =    -20.
           RVAL_MAX =     20.
         ELSE IF ( ISCL == 38 ) THEN
           RVAL_MIN =    -25.
           RVAL_MAX =     25.
         ELSE IF ( ISCL == 39 ) THEN
           RVAL_MIN =    -200.
           RVAL_MAX =     200.
         ELSE IF ( ISCL == 40 ) THEN
           RVAL_MIN =    -2000.
           RVAL_MAX =     2000.
         ELSE IF ( ISCL == 41 ) THEN
           RVAL_MIN =    -100.
           RVAL_MAX =     100.
         ELSE IF ( ISCL == 42 ) THEN
           RVAL_MIN =    -50.
           RVAL_MAX =     50.
         ELSE IF ( ISCL == 43 ) THEN
           RVAL_MIN =    -4.
           RVAL_MAX =     4.
         ELSE IF ( ISCL == 44 ) THEN
           RVAL_MIN =    -4000.
           RVAL_MAX =     4000.
         ELSE IF ( ISCL == 45 ) THEN
           RVAL_MIN =   -1.d-6
           RVAL_MAX =    1.e-6
         ELSE IF ( ISCL == 81 ) THEN
           RVAL_MIN =   -1.D-3
           RVAL_MAX =    1.D-3
      END IF
!
      IF ( VAL_MAX > VAL_MIN ) THEN
           RVAL_MIN = VAL_MIN
           RVAL_MAX = VAL_MAX
      END IF
      IF ( ( RVAL_MAX - RVAL_MIN) < EPS ) THEN
             RVAL_MAX = RVAL_MIN + MAX ( EPS, EPS*RVAL_MIN )
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MIN', STR_RVAL_MIN )
      IF ( ILEN(STR_RVAL_MIN) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MIN, '.' ) .LE. 0 ) THEN
                STR_RVAL_MIN = STR_RVAL_MIN(1:I_LEN(STR_RVAL_MIN))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MIN, FMT='(F20.10)' ) RVAL_MIN
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MAX', STR_RVAL_MAX )
      IF ( ILEN(STR_RVAL_MAX) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MAX, '.' ) .LE. 0 ) THEN
                STR_RVAL_MAX = STR_RVAL_MAX(1:I_LEN(STR_RVAL_MAX))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MAX, FMT='(F20.10)' ) RVAL_MAX
      END IF
 910  CONTINUE
!
! --- Open plotting device and set up coordinate system.
!
      UFILOUT = FILOUT
      IF ( UDEV .EQ. 1 ) THEN
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 2 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/CPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 3 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//GIF_DIAGI//'/GIF'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 4 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/VCPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
      END IF
!
      IF ( UDEV .EQ. 1 ) THEN
           IF ( XCOEF_PAP < 1 ) THEN
                PAP_SIZE = 400.0 
              ELSE 
                PAP_SIZE = 300.0 
           END IF
         ELSE IF ( UDEV .EQ. 2 ) THEN
           PAP_SIZE = 270.0
         ELSE IF ( UDEV .EQ. 3 ) THEN
           PAP_SIZE = 400.0
         ELSE IF ( UDEV .EQ. 4 ) THEN
           PAP_SIZE = 600.0
         ELSE
           PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
!
! --- Set parameters of the plotting window
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN  ( LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
           CALL PGSVP   ( 0.17, 0.97, 0.10, 0.90  ) ! makes fields for labels
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSVP   (  0.05, 0.95, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.0,  1.0, -1.1,  1.0  )
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSVP   (  0.15, 0.90, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.1,  1.0, -1.1,  1.0  )
      END IF
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2 .LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           CALL ERR_LOG ( 6351, IUER, 'POLOT_GRID_R4', 'This program requires '// &
     &         'a device with at least '//STR(1:I_LEN(STR))//' colors' )
           RETURN
      END IF
!
! --- Set white color
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- ... it is a little bit dimmer for the interactive device
!
           CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
           WHITE_CLR = BCG_CLR(1)
         ELSE
           WHITE_CLR = 255.0
           CALL PGCOL_RGB ( BCG_CLRI, 255, 255, 255 )
      END IF
!
! --- Set up a color palette using NCOL indices from INIT_COL to INIT_COL+NCOL.
!
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
      CALL DEFINE_PALETTE ( IPAL, UDEV, NCOL, INIT_COL )
!
! --- Transform the map from [min, max] range to [1, NCOL] range
!
      MAP_I4 = BCG_CLRI
      NUM = 0
!
      DO 430 J3=1,NLON
         LON = (J3-1)*PI2/(NLON-1)
         IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XPR = -1.0 + (J3-1)*2.0/(NLON-1)
         END IF
         DO 440 J4=1,NLAT
            LAT = -P2I + (J4-1)*PI__NUM/(NLAT-1)
            IF ( IPRC == 1 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                      MAP_I4(J3,J4) = BCG_CLRI
                    ELSE 
                      IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                      IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                      MAP_I4(J3,J4) = (VAL_R4 - RVAL_MIN)/(RVAL_MAX - RVAL_MIN)* &
     &                                (NCOL-1) + INIT_COL
                  END IF
               ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
                 MAP_I4(J3,J4) = BCG_CLRI
                 YPR = -1.0 + J4*2.0/(NLAT-1)
                 CALL INV_HAMMER_TRANS ( XPR, YPR, XCOEF_IMA, PHI, LAM )
                 PHI = PHI*180.0/PI__NUM
                 IF ( IPRC == 2 ) THEN
                      LAM = LAM*180.0/PI__NUM + 180.0
                    ELSE IF ( IPRC == -2 ) THEN
                      LAM = LAM*180.0/PI__NUM
                 END IF 
                 IF ( LAM > 360.0 ) LAM = LAM - 360.0
                 IF ( LAM < 0.0   ) LAM = LAM + 360.0
                 IF ( PHI < -99.9 ) GOTO 440
!
                 IX = INT ( (LAM/360.0)*NLON + 1.E-5 ) + 1
                 IF ( IX == 0  ) IX = NLON
                 IY = INT ( (PHI+90.0)/180.0*(NLAT-1) + 1.E-5 ) + 1
!
                 IF ( IY == 0  ) IY = 1
!
                 VAL_R4 = ARR_R4(IX,IY)
                 IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                      MAP_I4(J3,J4) = BCG_CLRI
                    ELSE 
                      IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                      IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                      MAP_I4(J3,J4) = ( VAL_R4 - RVAL_MIN )/ &
     &                                (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                 END IF
               ELSE IF ( IPRC == 3 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( LAT < LAT_MAX ) THEN
                      XPR = COS(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      YPR = COS(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      IF ( IX < 1 .OR. IX > NR .OR. IY < 1 .OR. IY > NR ) GOTO 440
                      IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                           MAP_I4(IX,IY) = BCG_CLRI
                         ELSE 
                           MAP_I4(IX,IY) = (  VAL_R4 - RVAL_MIN)/ &
     &                                     (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                           IF ( MAP_I4(IX,IY) > NCOL ) MAP_I4(IX,IY) = INIT_COL + NCOL - 1 
                           IF ( MAP_I4(IX,IY) <    1 ) MAP_I4(IX,IY) = INIT_COL 
                      END IF
                 END IF
               ELSE IF ( IPRC == 4 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( LAT > LAT_MIN ) THEN
                      XPR = SIN(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      YPR = SIN(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                           MAP_I4(IX,IY) = BCG_CLRI
                         ELSE 
                           MAP_I4(IX,IY) = (  VAL_R4 - RVAL_MIN )/ &
     &                                     (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                           IF ( MAP_I4(IX,IY) > NCOL ) MAP_I4(IX,IY) = INIT_COL + NCOL - 1
                           IF ( MAP_I4(IX,IY) <    1 ) MAP_I4(IX,IY) = INIT_COL
                      END IF
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( IPRC == 1 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   -1.0, 1.0, -1.0, 1.0 )
!
           DO 450 J5=1,NLAT
              PHI = -P2I + (J5-1)*PI__NUM/(NLAT-1)
              LAM = PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J5), ARR_Y(J5) )
              ARR_Y(J5) = ARR_Y(J5) - 2.0/(NLAT-1)
              IF ( ARR_Y(J5) > 1.0 ) ARR_Y(J5) = 1.0
 450       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
!
           DO 460 J6=1,NLAT
              PHI = -P2I + (J6-1)*PI__NUM/(NLAT-1)
              LAM = -PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J6), ARR_Y(J6) )
              ARR_Y(J6) = ARR_Y(J6) - 2.0/(NLAT-1)
              IF ( ARR_Y(J6) > 1.0 ) ARR_Y(J6) = 1.0
 460       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
         ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
           CALL PGPIXL ( MAP_I4, NR, NR, 1, NR, 1, NR, -1.0, 1.0, -1.0, 1.0 )
           IF ( IPRC == 3 ) THEN
                LAT_ARR(1) = -90.0*PI__NUM/180.0 
                LAT_ARR(2) = -80.0*PI__NUM/180.0 
                LAT_ARR(3) = -70.0*PI__NUM/180.0 
                LAT_ARR(4) = -60.0*PI__NUM/180.0 
              ELSE IF ( IPRC == 4 ) THEN
                LAT_ARR(1) =  60.0*PI__NUM/180.0 
                LAT_ARR(2) =  70.0*PI__NUM/180.0 
                LAT_ARR(3) =  80.0*PI__NUM/180.0 
                LAT_ARR(4) =  90.0*PI__NUM/180.0 
           END IF
!
! ======== Now putting coordinate grid around the picture
!
           DO 470 J7=1,ML
              LON = PI2*(J7-1)/(1.0*ML)
              DO 480 J8=1,MP
                 LAT = LAT_ARR(1) + (J8-1)*(LAT_ARR(ML) - LAT_ARR(1))/(MP-1)
                 IF ( IPRC == 3 ) THEN
                      ARR_X(J8) = COS(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      ARR_Y(J8) = COS(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                    ELSE IF ( IPRC == 4 ) THEN
                      ARR_X(J8) = SIN(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      ARR_Y(J8) = SIN(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                 END IF
 480           CONTINUE 
               CALL PGLINE ( MP, ARR_X, ARR_Y ) 
               CALL CLRCH  ( STR )
               WRITE ( UNIT=STR, FMT='(F4.0)' ) LON/PI2*360.
               STR(4:4) = ' '
               CALL CHASHL ( STR )
               IF ( IPRC == 3 ) THEN 
                    CALL PGPTXT ( ARR_X(MP), ARR_Y(MP), 0.0, 0.0, &
     &                            STR(1:I_LEN(STR))//'\u\(0902)\d' )
                  ELSE IF ( IPRC == 4 ) THEN
                    CALL PGPTXT ( ARR_X(1), ARR_Y(1), 0.0, 0.0, &
     &                            STR(1:I_LEN(STR))//'\u\(0902)\d' )
               END IF
 470       CONTINUE 
!
           DO 490 J9=1,ML
              CALL PGSCI  ( 1   )
              LAT = LAT_ARR(J9)
              DO 4100 J10=1,MP
                 IF ( IPRC == 3 ) THEN
                      LON = PI2*(J10-1)/(MP-1.0)
                    ELSE IF ( IPRC == 4 ) THEN
                      LON = PI2*(J10-1)/(MP-1.0)
                 END IF
                 IF ( IPRC == 3 ) THEN
                      ARR_X(J10) = COS(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      ARR_Y(J10) = COS(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                    ELSE IF ( IPRC == 4 ) THEN
                      ARR_X(J10) = SIN(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      ARR_Y(J10) = SIN(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                 END IF
 4100         CONTINUE 
              CALL PGLINE ( MP, ARR_X, ARR_Y ) 
              IF ( IPRC == 3 .AND. J9 == ML  ) GOTO 490
              IF ( IPRC == 4 .AND. J9 == 1   ) GOTO 490
              CALL CLRCH  ( STR )
              WRITE ( UNIT=STR, FMT='(F4.0)' ) LAT/PI__NUM*180.
              STR(4:4) = ' '
              CALL CHASHL ( STR )
              CALL PGPTXT ( ARR_X(MP/4), ARR_Y(MP/4), 0.0, 0.0, &
     &                      STR(1:I_LEN(STR))//'\u\(0902)\d' )
 490       CONTINUE 
      END IF
!
! --- Print title
!
      IF ( ILEN(TITLE) > 70 ) THEN
           CALL PGSCH  ( 0.9  )
         ELSE 
           CALL PGSCH  ( 1.44 )
      END IF
      CALL PGSCF  ( 2    )
      IF ( UDEV == 2 .OR. UDEV == 4 ) THEN
           CALL PGSLW  ( 2 )
         ELSE 
           CALL PGSLW  ( 5 )
      END IF
!
      CALL PGSCH  ( 1.2 )
      CALL PGMTXT ( 't', 1.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
      CALL PGSCH  ( 1.0 )
      CALL PGSCF  ( 1   )
      CALL PGSLW  ( 1 )
      IF ( IPRC == 1 ) THEN
!
! -------- Plot the box around the image
!
           CALL PGSCI  ( 1 )
           CALL PGBOX  ( 'bicnts', 30.0, 3, 'bicnts', 20.0, 2 )
!
! -------- Print the annotation
!
           CALL PGSLW  ( 4 )
           CALL PGSCH  ( 1.5 )
           CALL PGMTXT ( 'b', 1.9, 1.0, 1.0, 'longitude' )
           CALL PGMTXT ( 'l', 1.5, 1.0, 1.0, 'latitude' )
      END IF
!
! --- Printing the left column with scale
!
      IF ( IPRC == 1 ) THEN
           XMIN = 0.08
           XMAX = 0.11
           YMIN = 0.08
           YMAX = 0.92
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4      ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
      END IF
      CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  ) !
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*NCOL )
         ELSE  IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
         ELSE  IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
      END IF
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
         ELSE
           CALL PGSCH  ( 0.7 )
      END IF
      CALL PGSFS  ( 1   )
      CALL PGSLW  ( 2   )
!
      DO 4110 J11=1,NCOL
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = J11
              XR = XMAX
              YT = (J11-1)
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XL = J11-1 
              YB = YMIN
              XR = J11 
              YT = YMAX
            ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
              XL = J11-1 
              YB = YMIN
              XR = J11 
              YT = YMAX
         END IF
!
! ------ Make a box
!
         CALL PGSCI  ( J11-1+INIT_COL )
         CALL PGSFS  ( 1 )
         CALL PGRECT ( XL, XR, YB, YT )
 4110 CONTINUE
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
      END IF
!
      DO 4120 J12=1,NLAB
         VAL_R4 = RVAL_MIN + (RVAL_MAX-RVAL_MIN)/FLOAT(NLAB-1)*(J12-1)
         IF ( VAL_R4 .GE. 10000.0 .OR. VAL_R4 < -1000.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.0)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 1000.0 .OR. VAL_R4 < -100.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.1)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 100.0 .OR. VAL_R4 < -10.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.2)' ) VAL_R4
            ELSE 
              WRITE ( UNIT=STR(1:7), FMT='(F7.3)' ) VAL_R4
         END IF
         CALL CHASHR ( STR(1:7) )
!
! ------- Write annotation near the box
!
         CALL PGSCI  ( 1 )
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = FLOAT(J12-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:6) )
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 .OR. &
     &                IPRc == 3 .OR. IPRC ==  4      ) THEN
              XL = FLOAT(J12-1) 
              YB = YMAX
              CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:6) )
         END IF
 4120 CONTINUE 
!
! --- Write units of the annotation
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
!
! --- ... and the box around the scale
!
      CALL PGSCI ( 1 )
      CALL PGSFS ( 2 )
      CALL PGSLW ( 2 )
      IF ( UDEV .NE. 3 ) THEN
           CALL PGRECT ( XMIN+0.0001, XMAX-0.0001, 0.0, 1.0*(NLAB-1)-0.001 )
      END IF
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0 )
      CALL PGSLW  ( 4 )
      CALL PGSCH  ( 1.4 )
      CALL PGMTXT ( 'b', 0.9, 1.0, 1.0, UNIT(1:I_LEN(UNIT)) )
!
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
           CALL PGPTXT ( 0.01, -1.4, 0.0, 0.0, 'Hit <CNTRL/P> for printing, '// &
     &         'P - for making ps-file, G - for gif-file, other key to exit' )
      END IF
!
      XC = 0.95
      YC = 0.0
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- Wait for a user to hit a key
!
           CALL PGCURS ( XC, YC, CH )
           CALL TRAN ( 11, CH, CH )
           CALL PGCLOQ()
!
! -------- Check the value of the key
!
           IF ( CH .EQ. CHAR(16) ) THEN
!
! ------------- Make the plot in ps format and then print
!
                UDEV = 2
                FL_PLOT = .TRUE.
                GOTO 910
              ELSE IF ( CH .EQ. 'P' ) THEN
!
! ------------- Make the polt in postscript format
!
                UDEV = 2
                GOTO 910
              ELSE IF ( CH .EQ. 'G' ) THEN
!
! ------------- Make the plot in gif format
!
                UDEV = 3
                GOTO 910
           END IF
         ELSE
           CALL PGCLOQ()  ! quit pgplot
           ID = LINDEX ( UFILOUT, '/' ) - 1
           IF ( ID < 0 ) ID = ILEN(UFILOUT)
           WRITE ( 6, * ) 'Plot is written in file '//UFILOUT(1:ID)
           IF ( FL_PLOT ) THEN
!
! ------------- Aga. A user wants the plot to be printed
!
                CALL GETENVAR ( 'DIAGI_PRICOM', STR )
                IF ( ILEN(STR) .GT. 0 ) THEN
!
! ------------------ Set the command for printing
!
                     STR = STR(1:I_LEN(STR))//' '// &
     &                      UFILOUT(1:I_LEN(UFILOUT)-4)//' &'
                     WRITE ( 6, * ) 'Plot is being sent to printer ...'
                     CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                     WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                     WRITE ( 6, * ) 'Plot is sent to printer'
                END IF
           END IF
      END IF
!
      DEALLOCATE ( MAP_I4 ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_GRID_R4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GRID_2D_SHIFT_180_R4 ( L_LON, L_LAT, ARR_R4 )
      IMPLICIT   NONE 
      INTEGER*4  L_LON, L_LAT
      REAL*4     ARR_R4(L_LON,L_LAT)
      REAL*4     SWAP
      INTEGER*4  J1, J2, ILON
!
! --- Transform the grid rom -180,+180 to 0,360 deg
!
      DO 410 J1=1,L_LAT
         DO 420 J2=1,L_LON/2
            ILON = J2 + L_LON/2
            SWAP = ARR_R4(J2,J1)
            ARR_R4(J2,J1) = ARR_R4(ILON,J1)
            ARR_R4(ILON,J1) = SWAP
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE GRID_2D_SHIFT_180_R4   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GRID_2D_SHIFT_180_R8 ( L_LON, L_LAT, ARR_R8 )
      IMPLICIT   NONE 
      INTEGER*4  L_LON, L_LAT
      REAL*8     ARR_R8(L_LON,L_LAT)
      REAL*8     SWAP
      INTEGER*4  J1, J2, ILON
!
! --- Transform the grid rom -180,+180 to 0,360 deg
!
      DO 410 J1=1,L_LAT
         DO 420 J2=1,L_LON/2
            ILON = J2 + L_LON/2
            SWAP = ARR_R8(J2,J1)
            ARR_R8(J2,J1) = ARR_R8(ILON,J1)
            ARR_R8(ILON,J1) = SWAP
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE GRID_2D_SHIFT_180_R8  !#!#
