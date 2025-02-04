      PROGRAM    AVERAGE_LOADING
! ************************************************************************
! *                                                                      *
! *   Program AVERAGE_LOADING
! *                                                                      *
! * ### 15-APR-2015  AVERAGE_LOADING  v1.0 (c) L. Petrov 15-APR-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*8  MEL
      PARAMETER  ( MEL = 3*(MALO__NDEG+1)**2 )
      CHARACTER  DIRIN*128, C_FIL(MALO__FIL)*128, DATE_BEG*21, DATE_END*21, &
     &           EXT*3, FILNAM*128, DATE_FIL*21, WAV_NAM*4, STR*128, &
     &           DATE_STR*30, FILOUT*128
      INTEGER*8  DIR_DESC(16), IP8
      REAL*8     TIM_BEG, TIM_END, TIM_FIL, TAI_LOA
      REAL*4     DSP_ARR(MEL), DSPL_AVR(MEL), LON_VAL, LAT_VAL
      LOGICAL*1  FL_LAT_LON_VAL 
      INTEGER*4  MJD_BEG, MJD_END, MJD_FIL, MJD_LOA, J1, J2, J3, J4, &
     &           IL, IS, LEV, L_FIL, NLON, NLAT, IVEC, ICMP, IFRQ, IVRB, &
     &           KLON, KLAT, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX, &
     &                       MKDIR, OPENDIR, CLOSEDIR
!
      FL_LAT_LON_VAL = .FALSE.
      EXT = '.nc'
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: dir_in date_beg date_end output_heb_file [ivrb]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIRIN )
           CALL GETARG ( 2, DATE_BEG )
           CALL GETARG ( 3, DATE_END )
           CALL GETARG ( 4, FILOUT )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                CALL CHIN   ( STR, IVRB )
             ELSE
                IVRB = 0
           END IF
      END IF
!
! --- Check input name: is it an exisitig file or existing directory
!
      DIR_DESC(1) = OPENDIR ( DIRIN(1:I_LEN(DIRIN))//CHAR(0) )
      IF ( DIR_DESC(1) > 0 ) THEN
           IP8 = CLOSEDIR ( %VAL(DIR_DESC(1)) )
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6801, IUER, 'AVERAGE_LOADING', 'Cannot find '// &
     &         'directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6802, IUER, 'AVERAGE_LOADING', 'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6803, IUER, 'AVERAGE_LOADING', 'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      L_FIL = 0
      LEV   = 0
      DO 410 J1=1,MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6804, IUER, 'AVERAGE_LOADING', 'Error in '// &
     &            'reading input directory '//DIRIN(1:I_LEN(DIRIN))// &
     &            '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IL = ILEN(FILNAM)
         IF ( FILNAM(IL-ILEN(EXT)+1:IL) .NE. EXT ) GOTO 410
         DATE_FIL = FILNAM(IL-15:IL-12)//'_'//FILNAM(IL-11:IL-10)//'_'// &
     &              FILNAM(IL-9:IL-5)//':'//FILNAM(IL-4:IL-3)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL  ERR_LOG ( 6805, IUER, 'AVERAGE_LOADING', 'Unexpected format of '// &
     &             'file name '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) GOTO 410
         IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) GOTO 410
!
         L_FIL = L_FIL + 1
         C_FIL(L_FIL) = FILNAM 
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6806, IUER, 'AVERAGE_LOADING', 'No appropriate '// &
     &         'data files were found in the input directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
      CALL SORT_CH ( L_FIL, C_FIL )
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, 110 ) L_FIL
 110       FORMAT ( I6, ' data input files have been found' )
      END IF
!
      IVEC = 1
      ICMP = 1
      IFRQ = 1
      DSPL_AVR = 0.0
      DO 420 J2=1,L_FIL
         IUER = -1
         CALL READ_LOADING_NC ( C_FIL(J2), MEL, NLON, NLAT, IVEC, ICMP, &
     &                          IFRQ, MJD_LOA, TAI_LOA, DSP_ARR, WAV_NAM, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 6807, IUER, 'AVERAGE_LOADING', 'Error in '// &
     &            'reading the '//STR(1:I_LEN(STR))//'th file '//C_FIL(J2) )
              CALL EXIT ( 1 )
         END IF
         DSPL_AVR(1:NLON*NLAT) = DSPL_AVR(1:NLON*NLAT) + DSP_ARR(1:NLON*NLAT) 
 430     CONTINUE 
         IF ( IVRB > 1 ) THEN
              WRITE ( 6, 120 ) J2, L_FIL, CHAR(13)
 120          FORMAT ( '  Read file ', I6, ' ( ', I6, ' ) ',A$ )
              CALL FLUSH ( 6 )
         END IF
         IF ( FL_LAT_LON_VAL ) THEN
              LON_VAL = 20.0
              LAT_VAL = 50.0
              KLON = NINT(LON_VAL/360.*NLON) + 1
              KLAT = NINT((LAT_VAL+90.0)/180.*(NLAT-1))  + 1
              DATE_STR = MJDSEC_TO_DATE ( MJD_LOA, TAI_LOA, IUER )
              WRITE ( 6, 210 ) DATE_STR(1:16), -90.0 + (180.0*(KLAT-1))/(NLAT-1), &
     &                         (360.*(KLON-1))/NLON, &
     &                         1000.0*DSP_ARR(KLON+(KLAT-1)*NLON)
 210                           FORMAT ( 'SSS ', A, ' Lat= ', F6.2, ' Lon= ', F7.2, &
     &                                 ' Dspl_up= ', F7.3, ' mm ' )
         END IF
 420  CONTINUE 
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, 130 ) L_FIL
 130       FORMAT ( ' All ', I6, ' files have been read      ' )
      END IF
      DSPL_AVR(1:NLON*NLAT) = 1000.0*DSPL_AVR(1:NLON*NLAT)/L_FIL
!
      CALL NOUT ( SIZEOF(HEB), HEB )
      HEB%MJD     = (MJD_BEG + MJD_END)/2.0D0
      HEB%UTC     = (TIM_BEG + TIM_END)/2.0D0
      HEB%TAI     = (TIM_BEG + TIM_END)/2.0D0
      HEB%DIMS(1) = NLON
      HEB%DIMS(2) = NLAT
      HEB%DIMS(3) = 1
      HEB%DIMS(4) = 1
      HEB%ENDIAN  = HEB__LE
      HEB%DATA_TRANSFORM = HEB__NONE
      HEB%FILL_VALUE     = 1.0E15
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 1.0
      HEB%DATA_COMPRESSION = HEB__NONE
!
      HEB%SDS_NAME       = 'Average vertical loading'
      HEB%PROD_NAME      = HEB%SDS_NAME       
      HEB%TITLE          = HEB%SDS_NAME       
      HEB%UNITS          = 'mm'
      HEB%HISTORY        = ' '
      HEB%INSTITUTION    = 'Astrogeo Center'
      HEB%HISTORY        = ' '
      HEB%REFERENCES     = ' '
      HEB%SOURCE         = ' '
      HEB%PROD_DATE_TIME = GET_CDATE()
      HEB%VERSION_ID     = '1.0'
      HEB%DATA_FORMAT    = HEB__R4
      CALL HEB_SETMIN ( HEB, DSPL_AVR, HEB%MIN_VALUE )
      CALL HEB_SETMAX ( HEB, DSPL_AVR, HEB%MAX_VALUE )
      HEB%VALID_RANGE(1) = HEB%MIN_VALUE 
      HEB%VALID_RANGE(2) = HEB%MAX_VALUE 
!
      IUER = -1
      CALL WRITE_HEB ( HEB, DSPL_AVR, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6807, IUER, 'AVERAGE_LOADING', 'Error in '// &
     &         'writing into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      END  PROGRAM  AVERAGE_LOADING  !#!  
