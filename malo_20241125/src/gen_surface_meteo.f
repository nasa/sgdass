      PROGRAM    GEN_SURFACE_METEO
! ************************************************************************
! *                                                                      *
! *   Routine GEN_SURFACE_METEO reads an input file with station names   *
! *   and station coordinates, HEB files with the output of numerical    *
! *   weather mode and for a given range of dates generates a set of     *
! *   files, one per epoch with                                          *
! *
! bin/gen_surface_meteo /m1/heb/geosfpit/ $MALO_DIR/share/geosfpit_height_above_geoid.heb /g0/ge/ge_sta_20141102.fil /g0/ge/surf_met 15.0 2014.10.01 2014.10.31_22 | tee /tmp/f.2
! *                                                                      *
! * ## 13-MAR-2013  GEN_SURFACE_METEO v2.0 (c) L. Petrov  13-MAR-2013 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, &
     &                           HEB_OH
      TYPE     ( MALO__TYPE ),   POINTER :: MAL(:)
      CHARACTER  DIR_HEB*128, FIL_OH*128, STA_FIL*128, STR*128, &
     &           DIR_OUT*128, FIL_OUT*128, CAL_DATE*19, FILNAM*128, &
     &           C_FIL(MALO__FIL)*128, DATE_BEG*21, DATE_END*21, DATE_FIL*19
      CHARACTER  GEN_SURFACE_METEO__LABEL*32, EXT*8
      PARAMETER  ( GEN_SURFACE_METEO__LABEL = 'GEN_SURFACE_METEO Vers  1.0 of 2014.11.04' )
      PARAMETER  ( EXT = '.heb.bz2' )
      REAL*8     EPS
      PARAMETER  ( EPS = 100.0 )
      INTEGER*8  DIR_DESC(16)
      REAL*8     HEI_ABOVE_SURFACE, TIM_BEG, TIM_END, TIM_FIL
      INTEGER*4  DEG, MJD_BEG, MJD_END, MJD_FIL, L_FIL, LEV, ID, IS, IL, &
     &           IVRB, J1, J2, J3, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX, GET_FILE_FROM_DIR
!
      IVRB = 2
      IF ( IARGC() < 7 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_surface_meteo heb_dir fil_oh '// &
     &                        'station_file output_dir height date_beg date_end'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB  )
           CALL GETARG ( 2, FIL_OH   )
           CALL GETARG ( 3, STA_FIL  )
           CALL GETARG ( 4, DIR_OUT  )
           CALL GETARG ( 5, STR      )
           IF ( INDEX ( STR, "." ) < 1 ) THEN
                STR = STR(1:I_LEN(STR))//'.'
           END IF
           READ ( UNIT=STR, FMT='(F10.2)' ) HEI_ABOVE_SURFACE
           CALL GETARG ( 6, DATE_BEG )
           CALL GETARG ( 7, DATE_END )
      END IF
!
      DIR_DESC(1) = OPENDIR ( DIR_HEB(1:I_LEN(DIR_HEB))//CHAR(0) )
      IF ( DIR_DESC(1) > 0 ) THEN 
           DIR_DESC(1) = CLOSEDIR ( %VAL(DIR_DESC(1)) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 6701, IUER, 'GEN_SURFACE_METEO', 'Wrong 1th '// &
     &         'argument: heb directory '//DIR_HEB(1:I_LEN(DIR_HEB))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      DIR_DESC(1) = OPENDIR ( DIR_OUT(1:I_LEN(DIR_OUT))//CHAR(0) )
      IF ( DIR_DESC(1) > 0 ) THEN 
           DIR_DESC(1) = CLOSEDIR ( %VAL(DIR_DESC(1)) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'GEN_SURFACE_METEO', 'Wrong 5th argument: '// &
     &         'output directory '//DIR_OUT(1:I_LEN(DIR_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6703, IUER, 'GEN_SURFACE_METEO', &
     &                    'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'GEN_SURFACE_METEO', &
     &                   'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_OH, HEB_OH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6705, IUER, 'GEN_SURFACE_METEO', 'Error in reading '// &
     &         'heb-file with nominal surface ortho-height'//FIL_OH )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6706, IUER, 'GEN_SURFACE_METEO', 'Error in '// &
     &         'an attempt to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6707, IUER, 'GEN_SURFACE_METEO', 'Error in '// &
     &         'an attempt to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      LEV = 0
      L_FIL = 0
      DO 410 J1=1,16*MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_HEB, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 6708, IUER, 'GEN_SURFACE_METEO', 'Error in '// &
     &            'reading input directory '//DIR_HEB(1:I_LEN(DIR_HEB))// &
     &            '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT )     < 1    ) GOTO 410
         IF ( INDEX ( FILNAM, '#' )     .GE. 1 ) GOTO 410
         IF ( INDEX ( FILNAM, '/d/d_' ) < 1    ) GOTO 410
!
         IL = ILEN(FILNAM)
         IF ( IL < 22 ) GOTO 410
         DATE_FIL = FILNAM(IL-20:IL-17)//'_'//FILNAM(IL-16:IL-15)//'_'// &
     &              FILNAM(IL-14:IL-10)//':'//FILNAM(IL-9:IL-8)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL  ERR_LOG ( 6709, IUER, 'GEN_SURFACE_METEO', &
     &             'Unexpected format of file name '//FILNAM )
              CALL EXIT ( 1 )
         END IF
!
         IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_BEG*86400.0D0 +  TIM_BEG - EPS .AND. &
     &        MJD_FIL*86400.0D0 + TIM_FIL < MJD_END*86400.0D0 +  TIM_END + EPS       ) THEN
              L_FIL = L_FIL + 1
              IF ( L_FIL > MALO__FIL )  THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( MALO__FIL, STR )
                   IUER = -1
                   CALL ERR_LOG ( 6710, IUER, 'GEN_SURFACE_METEO', &
     &                 'Too many files in directory '// &
     &                 DIR_HEB(1:I_LEN(DIR_HEB))// &
     &                 ' -- more than '//STR )
                   CALL EXIT ( 1 )
              END IF
              C_FIL(L_FIL) = FILNAM 
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6711, IUER, 'GEN_SURFACE_METEO', 'No files with '// &
     &         'extension '//EXT(1:I_LEN(EXT))//' were found in the '// &
     &         'input directory '//DIR_HEB )
           CALL EXIT ( 1 )
         ELSE
           CALL SORT_CH ( L_FIL, C_FIL )
      END IF
!
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6712, IUER, 'GEN_SURFACE_METEO', &
     &         'No appropriate data files were found in the input '// &
     &         'directory '//DIR_HEB )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) L_FIL
 110       FORMAT ( I6, ' data input files have been found' )
      END IF 
!
      DO 420 J2=1,L_FIL
         WRITE ( 6, '(A)' ) C_FIL(J2)(1:I_LEN(C_FIL(J2)))
         IL = ILEN(C_FIL(J2))
         IUER = -1
         CALL READ_DQTUV_HEB ( C_FIL(J2), HEB_D, HEB_Q, HEB_T, HEB_U, HEB_V, &
     &                         IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6713, IUER, 'GEN_SURFACE_METEO', &
     &            'Error in reading data with meteorological parameters '// &
     &            'from directory '//C_FIL(J2)(1:IL-24)//' for date '// &
     &             C_FIL(J2)(IL-20:IL-8) )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL MALO_INTRP_PWTUV ( HEB_OH, HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, &
     &                           MAL(1), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6714, IUER, 'GEN_SURFACE_METEO', 'Error in '// &
     &            'an attempt to compute surface atmospheric pressure' )
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  GEN_SURFACE_METEO  !#!  
