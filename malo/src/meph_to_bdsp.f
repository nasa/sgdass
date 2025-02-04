      PROGRAM    MEPH_TO_BDSP
! ************************************************************************
! *                                                                      *
! *   Program  MEPH_TO_BDSP
! *                                                                      *
! *  ### 21-JUL-2015  MEPH_TO_BDSP v2.0 (c)  L. Petrov  29-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'bindisp.i'
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      INTEGER*4  M__FIL, M__BUF
      PARAMETER  ( M__FIL = 12 )
      PARAMETER  ( M__BUF = 256*1024 )
      TYPE     ( BINDISP_MODEL    ) ::  BDSM(M__FIL)
      TYPE     ( BINDISP_HEADER_2 ) ::  HDR2
      TYPE     ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE     ( BINDISP_HEADER_8 ) ::  HDR8
      TYPE     ( BDSSUM_STAREC    ) ::  BDSUM
      CHARACTER  STR*256, DIR_BDSP*128, DATE_BEG*24, DATE_END*24, &
     &           DIR_EPH(M__FIL)*128, FIL_HPS(M__FIL)*128, EXT*4, &
     &           FILNAM*128, FILS(MALO__FIL,M__FIL)*128, STR1*128, &
     &           C_STA(MALO__MSTA)*8, &
     &           C_STA_HPS(MALO__MSTA,M__FIL)*8, &
     &           C_HAR(MALO__MWAV,M__FIL)*8, FILOUT(MALO__MSTA)*128, &
     &           HEADER(M__HDR)*(LEN__HDR), FILSUM*128
      REAL*8     STEP, STA_COO_HPS(3,MALO__MSTA,M__FIL), &
     &           HARVAL(3,MALO__MWAV,M__FIL), &
     &           HARDSP(3,2,MALO__MWAV,MALO__MSTA,M__FIL), RD_AREA
      REAL*8     TIM__TOL
      PARAMETER  ( TIM__TOL = 90.0D0 )
      INTEGER*8  DIR_DESC(16), IP8
      LOGICAL*1  FL_DIR
      INTEGER*2  KX, KY, KZ
      INTEGER*4  NUM_ARG, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           J12, J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           J23, J24, J25, J26, J27, J28, J29, J30, J31, J32, J33, &
     &           J34, L_MOD, IND_EPC, L_HPS, L_EPH, LUN, &
     &           IX, IL, IVAL, IVRB, LEV, L_FIL(M__FIL), IS, NB, &
     &           IDS(4), N_BUF, IUER
      INTEGER*4  MJD_BEG_ARG, MJD_END_ARG, MJD_BEG, MJD_END, &
     &           MJD_BEG_FIL, MJD_END_FIL, MJD_BEG_LAST, &
     &           MJD_END_LAST, MJD_BEG_EPH(M__FIL), MJD_END_EPH(M__FIL), &
     &           L_EPC_FIL, L_STA_FIL, L_EPC_EPH(M__FIL), L_STA_EPH(M__FIL), &
     &           MJD_BEG_ALL, MJD_END_ALL, L_EPC_ALL, L_STA_ALL, L_HAR(M__FIL), &
     &           L_STA_HPS(M__FIL), MJD_VER, L_HDR
      REAL*8     TAI_BEG_ARG, TAI_END_ARG, TAI_BEG, TAI_END, &
     &           TAI_BEG_FIL, TAI_END_FIL, TAI_BEG_LAST, TAI_END_LAST, &
     &           TAI_BEG_EPH(M__FIL), TAI_END_EPH(M__FIL), TIM_STEP_EPH(M__FIL), &
     &           TIM_STEP_FIL, TIM_STEP_LAST, TAI_BEG_ALL, TAI_END_ALL, GAP, &
     &           TIM_STEP, VEC_UEN(3), VEC_XYZ(3), TIM_ARG, PHS_ARG, SEC_VER
      REAL*8,    ALLOCATABLE :: TIM(:), VAL(:,:,:), SPL(:,:,:), &
     &                          TIM_EPH(:), VAL_EPH(:,:,:), SPL_EPH(:,:,:), &
     &                          DSPL_ARR(:,:,:), TMP(:), UEN_TO_XYZ(:,:,:)
      CHARACTER  ENDIAN_FMT*1, FLOAT_FMT
#ifdef BIG_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'B' ) ! ENDIAN defiend as preprocessor D option
#else
! if LITTLE_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'L' ) ! ENDIAN defiend as preprocessor D option
#endif
      PARAMETER  ( FLOAT_FMT  = 'I' ) ! IEEE 754/854 float format
      TYPE     ( BINDISP_DATA     ), POINTER :: BDS(:,:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, IXMN8, GET_UNIT, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, GET_FILE_FROM_DIR 
      REAL*8,    EXTERNAL :: FSPL8 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: dir_bdsp fil_in [fil_in] time_step '// &
     &                    '[ivrb] [date_beg] [date_end]'
           CALL EXIT ( 1 )
         ELSE   
           CALL GETARG ( 1, DIR_BDSP )
           CALL CLRCH  ( DATE_BEG )
           CALL CLRCH  ( DATE_END )
           NUM_ARG = IARGC()
           L_HPS = 0
           L_EPH = 0
           IVRB = 1
           TIM_STEP = -1.0D0
           DO 410 J1=2,NUM_ARG
              CALL GETARG ( J1, STR )
              IL = ILEN ( STR )
              IF ( IL < 4 ) IL = 4
              DIR_DESC(1) = OPENDIR ( STR(1:I_LEN(STR))//CHAR(0) )
              IF ( DIR_DESC(1) > 0 ) THEN
                   IP8 = CLOSEDIR ( %VAL(DIR_DESC(1)) )
                   FL_DIR = .TRUE.
                 ELSE 
                   FL_DIR = .FALSE. 
              END IF
              IF ( STR(IL-3:IL) == '.hps' ) THEN
                   L_HPS = L_HPS + 1
                   IF ( L_HPS > M__FIL ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M__FIL, STR )
                        IUER = -1
                        CALL ERR_LOG  ( 6801, IUER, 'MEPH_TO_BDSP', 'Too many '// &
     &                      'input files: more than '//STR )
                        CALL EXIT ( 1 )
                   END IF
                   FIL_HPS(L_HPS) = STR
                 ELSE 
                   IF ( FL_DIR ) THEN
                        L_EPH = L_EPH + 1
                        IF ( L_EPH > M__FIL ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( M__FIL, STR )
                             IUER = -1
                             CALL ERR_LOG ( 6802, IUER, 'MEPH_TO_BDSP', 'Too many '// &
     &                           'input files: more than '//STR )
                             CALL EXIT ( 1 )
                        END IF
                        DIR_EPH(L_EPH) = STR
                      ELSE
                        CALL CHIN ( STR, IVAL )
                        IF ( IVAL > 0 ) THEN
                             IVRB = IVAL
                           ELSE 
                             IF ( TIM_STEP == -1.0D0 ) THEN
                                  READ ( UNIT=STR, FMT='(F20.10)', &
     &                                   IOSTAT=IUER ) TIM_STEP
                                  IF ( IUER .NE. 0 ) THEN
                                       IUER = -1
                                       CALL ERR_LOG ( 6803, IUER, 'MEPH_TO_BDSP', &
     &                                     'Failure in parsing argument '// &
     &                                     'time step '//STR )
                                       CALL EXIT ( 1 )
                                  END IF
                                ELSE IF ( ILEN(DATE_BEG) == 0 ) THEN
                                  DATE_BEG = STR
                                ELSE IF ( ILEN(DATE_END) == 0 ) THEN
                                  DATE_END = STR
                             END IF
                        END IF
                   END IF
              END IF
 410       CONTINUE 
      END IF
      IF ( ILEN(DATE_BEG) > 0 ) THEN
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG_ARG, TAI_BEG_ARG, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6804, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'parsing begin date '//DATE_BEG )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( ILEN(DATE_END) > 0 ) THEN
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END_ARG, TAI_END_ARG, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6805, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'parsing end date '//DATE_END )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( TIM_STEP == -1.0D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6806, IUER, 'MEPH_TO_BDSP', 'No time step was '// &
     &         'defined' )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, * ) 'L_EPH = ', L_EPH
      WRITE ( 6, * ) 'L_HPS = ', L_HPS
      WRITE ( 6, * ) 'DIR_EPH(1) = ', DIR_EPH(1)
      WRITE ( 6, * ) 'FIL_HPS(1) = ', FIL_HPS(1)
      WRITE ( 6, * ) 'TIM_STEP   = ', TIM_STEP
      WRITE ( 6, * ) 'DATE_BEG   = ', DATE_BEG
      WRITE ( 6, * ) 'DATE_END   = ', DATE_END
!
      MJD_BEG_ALL = 0
      MJD_END_ALL = 0
      TAI_BEG_ALL = 0.0D0
      TAI_END_ALL = 0.0D0
      L_STA_ALL   = 0
!
      IF ( L_EPH > 0 ) THEN
           EXT = '.eph'
           DO 420 J2=1,L_EPH
              L_FIL(J2) = 0
              LEV = 0
              L_EPC_EPH(J2) = 0
!
              DO 430 J3=1,16*MALO__FIL
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_EPH(J2), FILNAM )
                 IF ( IS .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6807, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                    'reading input directory '// &
     &                     DIR_EPH(J3)(1:I_LEN(DIR_EPH(J3)))//'  '//FILNAM )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( LEV == 0 ) GOTO 830 ! End of work
                 IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 430
                 IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 430
                 IF ( INDEX ( FILNAM, EXT//'~' ) .GT. 0 ) GOTO 430
!
                 L_FIL(J2) = L_FIL(J2) + 1
                 IF ( L_FIL(J2) > MALO__FIL )  THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( MALO__FIL, STR )
                      IUER = -1
                      CALL ERR_LOG ( 6808, IUER, 'MEPH_TO_BDSP', 'Too many '// &
     &                    'files in directory '// &
     &                     DIR_EPH(J3)(1:I_LEN(DIR_EPH(J3)))// &
     &                    ' -- more than '//STR )
                      CALL EXIT ( 1 )
                 END IF
                 FILS(L_FIL(J2),J2) = FILNAM 
 430          CONTINUE 
 830          CONTINUE 
              IF ( L_FIL(J2) == 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6809, IUER, 'MALO', 'No files with extension '// &
     &                  EXT(1:I_LEN(EXT))//' were found in the input directory '// &
     &                  DIR_EPH(J3) )
                   CALL EXIT ( 1 )
              END IF
              CALL SORT_CH ( L_FIL(J2), FILS(1:L_FIL(J2),J2) )
              WRITE ( 6, * ) 'j2= ', j2, ' lf= ', l_fil(j2) ! %%%
!
              MJD_BEG_EPH(J2) = 0
              MJD_END_EPH(J2) = 0
              L_STA_EPH(J2)   = 0
              TAI_BEG_EPH(J2) = 0.0D0
              TAI_END_EPH(J2) = 0.0D0
              MJD_BEG_LAST  = 0
              MJD_END_LAST  = 0
              TAI_BEG_LAST  = 0
              TAI_END_LAST  = 0
              TIM_STEP_LAST = 0.0D0
              DO 440 J4=1,L_FIL(J2)
                 IUER = -1
                 CALL MALO_EPHEDISP_LEARN ( FILS(J4,J2), MJD_BEG_FIL, TAI_BEG_FIL, &
     &                                      MJD_END_FIL, TAI_END_FIL, TIM_STEP_FIL, &
     &                                      L_EPC_FIL, L_STA_FIL, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6810, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                    'reading file with site displacements '//FILS(J4,J2) )
                      CALL EXIT ( 1 )
                 END IF
!
                 IF ( MJD_BEG_EPH(J2) == 0 ) THEN
                      MJD_BEG_EPH(J2)  = MJD_BEG_FIL
                      TAI_BEG_EPH(J2)  = TAI_BEG_FIL
                      TIM_STEP_EPH(J2) = TIM_STEP_FIL
                      L_STA_EPH(J2)    = L_STA_FIL
                   ELSE
                      IF ( L_STA_FIL .NE. L_STA_EPH(J2) ) THEN
                           CALL CLRCH ( STR  )
                           CALL CLRCH ( STR1 )
                           CALL INCH  ( L_STA_FIL, STR )
                           CALL INCH  ( L_STA_EPH(J2), STR1 )
                           IUER = -1
                           CALL ERR_LOG ( 6811, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &                         'internal control: the number of stations in file '// &
     &                          FILS(J4,J2)(1:I_LEN(FILS(J4,J2)))//' '// &
     &                          STR(1:I_LEN(STR))//' is not the same as in '// &
     &                         'previous file(s): '//STR1 )
                           CALL EXIT ( 1 )
                      END IF
                      IF ( DABS(TIM_STEP_FIL - TIM_STEP_EPH(J2)) > TIM__TOL ) THEN
                           CALL CLRCH ( STR  )
                           CALL CLRCH ( STR1 )
                           WRITE ( UNIT=STR(1:12),  FMT='(F12.1)' ) TIM_STEP_FIL
                           WRITE ( UNIT=STR1(1:12), FMT='(F12.1)' ) TIM_STEP_EPH(J2)
                           CALL CHASHL ( STR  )
                           CALL CHASHL ( STR1 )
                           IUER = -1
                           CALL ERR_LOG ( 6812, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &                         'internal control: the time step in file '// &
     &                          FILS(J4,J2)(1:I_LEN(FILS(J4,J2)))//' '// &
     &                          STR(1:I_LEN(STR))//' is not the same as in '// &
     &                         'previous file(s): '//STR1 )
                           CALL EXIT ( 1 )
                      END IF
                      GAP = (MJD_BEG_FIL*86400.0D0 + TAI_BEG_FIL) - &
     &                      (MJD_END_LAST*86400.0D0 + TAI_END_LAST )
                      IF ( DABS ( GAP - TIM_STEP_EPH(J2) ) > TIM__TOL ) THEN
                           CALL CLRCH ( STR  )
                           CALL CLRCH ( STR1 )
                           WRITE ( UNIT=STR(1:12),  FMT='(F12.1)' ) GAP
                           WRITE ( UNIT=STR1(1:12), FMT='(F12.1)' ) TIM_STEP_EPH(J2)
                           CALL CHASHL ( STR  )
                           CALL CHASHL ( STR1 )
                           IUER = -1
                           CALL ERR_LOG ( 6813, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &                         'internal control: the gap between the file '// &
     &                          FILS(J4,J2)(1:I_LEN(FILS(J4,J2)))//' '// &
     &                          ' and the previous file: '//STR(1:I_LEN(STR))// &
     &                         ' is not equal to the time step '//STR1 )
                           CALL EXIT ( 1 )
                      END IF
                 END IF
! 
                 L_EPC_EPH(J2)   = L_EPC_EPH(J2) + L_EPC_FIL
                 MJD_END_EPH(J2) = MJD_END_FIL
                 TAI_END_EPH(J2) = TAI_END_FIL
!
                 MJD_BEG_LAST  = MJD_BEG_FIL
                 MJD_END_LAST  = MJD_END_FIL
                 TAI_BEG_LAST  = TAI_BEG_FIL
                 TAI_END_LAST  = TAI_END_FIL
                 TIM_STEP_LAST = TIM_STEP_FIL
 440          CONTINUE 
              IF ( MJD_BEG_ALL == 0 ) THEN
                   MJD_BEG_ALL = MJD_BEG_EPH(J2)
                   MJD_END_ALL = MJD_END_EPH(J2)
                   TAI_BEG_ALL = TAI_BEG_EPH(J2)
                   TAI_END_ALL = TAI_END_EPH(J2)
                   L_STA_ALL   = L_STA_EPH(J2)
                 ELSE 
                   IF ( (MJD_BEG_EPH(J2)*86400.0D0 + TAI_BEG_EPH(J2)) > &
     &                  (MJD_BEG_ALL*86400.0D0 + TAI_BEG_ALL)           ) THEN
                        MJD_BEG_ALL = MJD_BEG_EPH(J2)
                        TAI_BEG_ALL = TAI_BEG_EPH(J2)
                   END IF
                   IF ( (MJD_END_EPH(J2)*86400.0D0 + TAI_END_EPH(J2)) < &
     &                  (MJD_END_ALL*86400.0D0 + TAI_END_ALL)           ) THEN
                        MJD_END_ALL = MJD_END_EPH(J2)
                        TAI_END_ALL = TAI_END_EPH(J2)
                   END IF
                   IF ( L_STA_EPH(J2) .NE. L_STA_ALL ) THEN
                        CALL CLRCH ( STR  )
                        CALL CLRCH ( STR1 )
                        CALL INCH  ( L_STA_EPH(J2), STR )
                        CALL INCH  ( L_STA_ALL, STR1 )
                        IUER = -1
                        CALL ERR_LOG ( 6814, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &                      'internal control: The number of stations for stations '// &
     &                      ' in directory '//DIR_EPH(J2)(1:I_LEN(DIR_EPH(J2)))// &
     &                      ' -- '//STR(1:I_LEN(STR))//' is not the same as '// &
     &                      ' in the previous direcotry '// &
     &                      DIR_EPH(J2-1)(1:I_LEN(DIR_EPH(J2-1)))//' -- '//STR1 )
                        CALL EXIT ( 1 )
                   END IF
              END IF
 420       CONTINUE 
      END IF
!
      IF ( L_HPS > 0 ) THEN
           DO 460 J6=1,L_HPS
              IUER = -1
              CALL MALO_HARPOS_READ ( FIL_HPS(J6), MALO__MWAV, MALO__MSTA, &
     &                                L_HAR(J6), L_STA_HPS(J6), &
     &                                C_HAR(1,J6), C_STA_HPS(1,J6), RD_AREA, &
     &                                HARVAL(1,1,J6), STA_COO_HPS(1,1,J6), &
     &                                HARDSP(1,1,1,1,J6), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6815, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                 'reading and parsing file with harmonic site '// &
     &                 'position variations '//FIL_HPS(J6) )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( 6, * ) 'J6= ', INT2(J6), ' L_HAR= ', L_HAR(J6), ' L_STA_HPS= ', L_STA_HPS(J6)
              IF ( L_EPH == 0 .AND. J6 == 1 ) THEN
!
! ---------------- Copy the station list if no time series was specified
!
                   L_STA_ALL = L_STA_HPS(J6)
                   C_STA(1:L_STA_ALL) = C_STA_HPS(1:L_STA_ALL,J6)
              END IF
              IF ( J6 > 1 ) THEN
                   IF ( L_STA_HPS(J6) .NE. L_STA_HPS(J6-1) )  THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( L_STA_HPS(J6), STR )
                        CALL CLRCH ( STR1 )
                        CALL INCH  ( L_STA_HPS(J6-1), STR1 )
                        IUER = -1
                        CALL ERR_LOG ( 6816, IUER, 'MEPH_TO_BDSP', 'The number of '// &
     &                      'stations in the harmonic site position variation '// &
     &                      'file '//FIL_HPS(J6)(1:I_LEN(FIL_HPS(J6)))//' is '// &
     &                       STR(1:I_LEN(STR))//', which is different from the '// &
     &                      'number of stations in file '// &
     &                       FIL_HPS(J6-1)(1:I_LEN(FIL_HPS(J6-1)))//' -- '//STR1 )
                        CALL EXIT ( 1 )
                   END IF
!
                   DO 470 J7=1,L_STA_HPS(J6)
                      IF ( LTM_DIF ( 0, L_STA_HPS(J6-1), C_STA_HPS(1:L_STA_HPS(J6-1),J6-1), &
     &                               C_STA_HPS(J7,J6) ) < 1 ) THEN
                           IUER = -1
                           CALL ERR_LOG ( 6817, IUER, 'MEPH_TO_BDSP', &
     &                         'The station list in file '// &
     &                          FIL_HPS(J6)(1:I_LEN(FIL_HPS(J6)))//' is '// &
     &                         'different than in file '// &
     &                          FIL_HPS(J6-1)(1:I_LEN(FIL_HPS(J6-1)))// &
     &                         '. Station '//C_STA_HPS(J7,J6)//' was not '// &
     &                         'found there' )
                           CALL EXIT ( 1 )
                      END IF
 470               CONTINUE 
              END IF
 460       CONTINUE 
      END IF
!
      IF ( L_EPH == 0 .AND. ILEN(DATE_BEG) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6818, IUER, 'MEPH_TO_BDSP', 'No begin date was '// &
     &         'defined, but only harmonic position variation '// &
     &         'file(s) were supplied. Please supply begin date' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( L_EPH == 0 ) THEN
           MJD_BEG = MJD_BEG_ARG
           TAI_BEG = TAI_BEG_ARG
           MJD_END = MJD_END_ARG
           TAI_END = TAI_END_ARG
           MJD_BEG_ALL = MJD_BEG_ARG
           TAI_BEG_ALL = TAI_BEG_ARG
           MJD_END_ALL = MJD_END_ARG
           TAI_END_ALL = TAI_END_ARG
         ELSE 
           IF ( ILEN(DATE_BEG) == 0 ) THEN
                MJD_BEG = MJD_BEG_ALL
                TAI_BEG = TAI_BEG_ALL
              ELSE 
                IF ( MJD_BEG_ALL*86400.0D0 + TAI_BEG_ALL > &
     &               MJD_BEG_ARG*86400.0D0 + TAI_BEG_ARG   ) THEN
                     MJD_BEG = MJD_BEG_ALL
                     TAI_BEG = TAI_BEG_ALL
                   ELSE
                     MJD_BEG = MJD_BEG_ARG
                     TAI_BEG = TAI_BEG_ARG
                END IF
           END IF
!
           IF ( ILEN(DATE_END) == 0 ) THEN
                MJD_END = MJD_END_ALL
                TAI_END = TAI_END_ALL
              ELSE 
                IF ( MJD_END_ALL*86400.0D0 + TAI_END_ALL < &
     &               MJD_END_ARG*86400.0D0 + TAI_END_ARG   ) THEN
                     MJD_END = MJD_END_ALL
                     TAI_END = TAI_END_ALL
                   ELSE
                     MJD_END = MJD_END_ARG
                     TAI_END = TAI_END_ARG
                END IF
           END IF
      END IF
!
      IF ( (MJD_END*86400.0D0 + TAI_END) - (MJD_BEG*86400.0D0 + TAI_BEG) < &
     &     TIM_STEP + TIM__TOL   ) THEN
           DATE_BEG = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
           DATE_END = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
           IUER = -1
           CALL ERR_LOG ( 6820, IUER, 'MEPH_TO_BDSP', 'Wrong begin/end '// &
     &         'dates: '//DATE_BEG//' and '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      L_EPC_ALL = IDNINT ( ( (MJD_END - MJD_BEG)*86400.0D0 + &
     &                       (TAI_END - TAI_BEG) )/ TIM_STEP ) + 1
!
      WRITE ( 6, * ) 'L_STA_ALL = ', L_STA_ALL
      WRITE ( 6, * ) 'L_EPC_ALL = ', L_EPC_ALL
      DATE_BEG = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
      DATE_END = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
      WRITE ( 6, * ) 'DATEs = ', DATE_BEG(1:21), '  ', DATE_END(1:21)
      WRITE ( 6, * ) 'DaTEs = ', MJDSEC_TO_DATE ( MJD_END_ALL, TAI_END_ALL, IUER ) ! %%%%%
!
      ALLOCATE ( MAL(2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6821, IUER, 'MEPH_TO_BDSP', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6822, IUER, 'MEPH_TO_BDSP', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( TIM(L_EPC_ALL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL IINCH ( L_EPC_ALL*8, STR )
           CALL ERR_LOG ( 6823, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &        'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &        'memory for array TIM' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( VAL(L_EPC_ALL,L_STA_ALL,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL IINCH ( L_EPC_ALL*L_STA_ALL*3*8, STR )
           CALL ERR_LOG ( 6824, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &        'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &        'memory for array VAL' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SPL(L_EPC_ALL,L_STA_ALL,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL IINCH ( L_EPC_ALL*L_STA_ALL*3*8, STR )
           CALL ERR_LOG ( 6825, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &        'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &        'memory for array SPL' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( BDS(L_EPC_ALL,L_STA_ALL), STAT=IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL CLRCH ( STR )
           CALL IINCH8 ( SIZEOF(BDS(1,1))*L_STA_ALL*L_EPC_ALL, STR ) 
           IUER = -1
           CALL ERR_LOG ( 6825, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'for array BDS' )
           CALL EXIT ( 1 )
      END IF
!
      DO 480 J8=1,L_EPC_ALL
         TIM(J8) = (J8-1)*TIM_STEP
 480  CONTINUE 
      VAL = 0.0D0
      SPL = 0.0D0
!
      ALLOCATE ( UEN_TO_XYZ(3,3,L_STA_ALL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 3*3*L_STA_ALL*8, STR )
           IUER = -1
           CALL ERR_LOG ( 6829, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &         'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'memory for array UEN_TO_XYZ' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( L_EPH > 0 ) THEN
           ALLOCATE ( VAL_EPH(L_EPC_ALL,L_STA_ALL,3), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( L_EPC_ALL*3*L_STA_ALL*8, STR )
                IUER = -1
                CALL ERR_LOG ( 6826, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'memory for array VAL_EPH' )
                CALL EXIT ( 1 )
           END IF
           ALLOCATE ( SPL_EPH(L_EPC_ALL,L_STA_ALL,3), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( L_EPC_ALL*3*L_STA_ALL*8, STR )
                IUER = -1
                CALL ERR_LOG ( 6827, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'memory for array SPL_EPH' )
                CALL EXIT ( 1 )
           END IF
!
           ALLOCATE ( TIM_EPH(L_EPC_ALL), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( L_EPC_ALL*8, STR )
                IUER = -1
                CALL ERR_LOG ( 6828, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'memory for array TIM_EPH' )
                CALL EXIT ( 1 )
           END IF
!
           ALLOCATE ( TMP(L_EPC_ALL), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( L_EPC_ALL*8, STR )
                IUER = -1
                CALL ERR_LOG ( 6829, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'memory for array TMP' )
                CALL EXIT ( 1 )
           END IF
!           
           DO 490 J9=1,L_EPH
              IND_EPC = 0 
              DO 4100 J10=1,L_FIL(J9)
                 IUER = -1
                 CALL MALO_EPHEDISP_LEARN ( FILS(J10,J9), MJD_BEG_FIL, TAI_BEG_FIL, &  
     &                                      MJD_END_FIL, TAI_END_FIL, TIM_STEP_FIL, &
     &                                      L_EPC_FIL, L_STA_FIL, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6830, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                    'parsing file with site displacements '//FILS(J10,J9) )
                      CALL EXIT ( 1 )
                 END IF
                 ALLOCATE( DSPL_ARR(3,L_STA_FIL,L_EPC_FIL), STAT=IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL CLRCH ( STR ) 
                      CALL IINCH ( 3*8*L_STA_FIL*L_EPC_FIL, STR )
                      IUER = -1
                      CALL ERR_LOG ( 6831, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &                    'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &                    'memory for array DSPL_ARR' )
                      CALL EXIT ( 1 )
                 END IF
!
                 IUER = -1
                 CALL MALO_EPHEDISP_READ ( FILS(J10,J9), MAL(1), DSPL_ARR, MALO__REA, &
     &                                     L_STA_FIL, L_EPC_FIL, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6832, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                    'reading file with site displacements '//FILS(J10,J9) )
                      CALL EXIT ( 1 )
                 END IF
                 DO 4110 J11=1,L_EPC_FIL
                    IND_EPC = IND_EPC + 1
                    TIM_EPH(IND_EPC) = (IND_EPC-1)*TIM_STEP_FIL
                    DO 4120 J12=1,L_STA_FIL
                       VAL_EPH(IND_EPC,J12,1:3) = DSPL_ARR(1:3,J12,J11)
 4120               CONTINUE 
 4110            CONTINUE 
                 DEALLOCATE ( DSPL_ARR )
 4100         CONTINUE 
!
              DO 4130 J13=1,L_STA_EPH(J9)
                 DO 4140 J14=1,3
                    IUER = -1
                    CALL MAKE_SPLINE ( 3, L_EPC_EPH(J9), TIM_EPH, &
     &                                 VAL_EPH(1,J13,J14), 0.0D0, 0.0D0, &
     &                                 SPL_EPH(1,J13,J14), TMP, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         IUER = -1
                         CALL ERR_LOG ( 6833, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &                       'an attempt to compute the coefficients of the '// &
     &                       'B-spline that interopolates the data' )
                         CALL EXIT ( 1 )
                    END IF
 4140            CONTINUE 
                 CALL MAKE_UEN_TO_XYZ  ( MAL(1)%STA(J13)%COO, UEN_TO_XYZ(1,1,J13) )
 4130         CONTINUE 
!
              DO 4150 J15=1,L_EPC_ALL
                 IX = IXMN8 ( L_EPC_EPH(J9), TIM_EPH, TIM(J15) )
                 DO 4160 J16=1,L_STA_EPH(J9)
                    DO 4170 J17=1,3
                       VEC_UEN(J17) = FSPL8 ( TIM(J15), L_EPC_EPH(J9), TIM_EPH, &
     &                                        VAL_EPH(1,J16,J17), IX, SPL_EPH(1,J16,J17) )
 4170               CONTINUE 
                    CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J16), 3, VEC_UEN, &
     &                                    3, VEC_XYZ, IUER )
                    VAL(J15,J16,1:3) = VAL(J15,J16,1:3) + VEC_XYZ(1:3)
 4160            CONTINUE 
 4150         CONTINUE 
 490       CONTINUE 
      END IF
      IF ( L_HPS > 0 ) THEN
           IF ( L_EPH == 0 ) THEN
                DO 4180 J18=1,L_STA_ALL
                   CALL MAKE_UEN_TO_XYZ  ( STA_COO_HPS(1,J18,1), UEN_TO_XYZ(1,1,J18) )
 4180           CONTINUE 
!
                ALLOCATE ( TMP(L_EPC_ALL), STAT=IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL IINCH ( L_EPC_ALL*8, STR )
                     IUER = -1
                     CALL ERR_LOG ( 6829, IUER, 'MEPH_TO_BDSP', 'Failure in '// &
     &                   'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &                   'memory for array TMP' )
                     CALL EXIT ( 1 )
                END IF
           END IF
           DO 4190 J19=1,L_EPC_ALL
              TIM_ARG = TIM(J19) - (MJD_BEG - J2000__MJD)*86400.0D0 + &
     &                             (TAI_BEG - 43200.0D0)
              DO 4200 J20=1,L_HPS
                 DO 4210 J21=1,L_HAR(J20)
                    PHS_ARG = HARVAL(1,J21,J20) + HARVAL(2,J21,J20)*TIM_ARG + &
     &                        HARVAL(3,J21,J20)*TIM_ARG**2/2.D0
                    DO 4220 J22=1,L_STA_ALL
                       DO 4230 J23=1,3
                          VEC_UEN(J23) = HARDSP(J23,1,J21,J22,J20)*DCOS(PHS_ARG) + &
     &                                   HARDSP(J23,2,J21,J22,J20)*DSIN(PHS_ARG) 
 4230                  CONTINUE 
                       CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J22), 3, VEC_UEN, &
     &                                    3, VEC_XYZ, IUER )
                       VAL(J19,J22,1:3) = VAL(J19,J22,1:3) + VEC_XYZ(1:3)
 4220               CONTINUE 
 4210            CONTINUE 
 4200         CONTINUE 
 4190      CONTINUE 
      END IF
!
      DO 4240 J24=1,L_STA_ALL
         DO 4250 J25=1,3
            IUER = -1
            CALL MAKE_SPLINE ( 3, L_EPC_ALL, TIM, &
     &                         VAL(1,J24,J25), 0.0D0, 0.0D0, &
     &                         SPL(1,J24,J25), TMP, IUER )
            IF ( IUER .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6834, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &               'an attempt to compute the coefficients of the '// &
     &               'B-spline that interopolates the data' )
                 CALL EXIT ( 1 )
            END IF
 4250    CONTINUE 
!
         DO 4260 J26=1,L_EPC_ALL
            IX = J26
            IF ( J26 == L_EPC_ALL ) IX = L_EPC_ALL - 1
            DO 4270 J27=1,3
               VEC_UEN(J27) = FSPL8 ( TIM(J26), L_EPC_ALL, TIM, &
     &                                VAL(1,J24,J27), IX, SPL(1,J24,J27) )
 4270       CONTINUE 
!
            KX = VEC_UEN(1)/VTD__BDS_MAX
            KY = VEC_UEN(2)/VTD__BDS_MAX
            KZ = VEC_UEN(3)/VTD__BDS_MAX
            BDS(J26,J24)%EXT_DSP  = 0
            CALL MVBITS (  KX, 15, 1,  BDS(J26,J24)%EXT_DSP,  1 )
            CALL MVBITS (  KY, 15, 1,  BDS(J26,J24)%EXT_DSP,  2 )
            CALL MVBITS (  KZ, 15, 1,  BDS(J26,J24)%EXT_DSP,  3 )
            CALL MVBITS (  KX,  0, 4,  BDS(J26,J24)%EXT_DSP,  4 )
            CALL MVBITS (  KY,  0, 4,  BDS(J26,J24)%EXT_DSP,  8 )
            CALL MVBITS (  KZ,  0, 4,  BDS(J26,J24)%EXT_DSP, 12 )
            BDS(J26,J24)%X_DSP = NINT( (VEC_UEN(1) - KX*VTD__BDS_MAX)*1.D5 )
            BDS(J26,J24)%Y_DSP = NINT( (VEC_UEN(2) - KY*VTD__BDS_MAX)*1.D5 )
            BDS(J26,J24)%Z_DSP = NINT( (VEC_UEN(3) - KZ*VTD__BDS_MAX)*1.D5 )
 4260    CONTINUE 
!
! ------ Build the postfix which will be appended to the temporary file names
! ------ Cycle over all stations
!
! ------ Build the name of the output file
!
         CALL CLRCH ( FILOUT(J24) )
         IF ( L_HPS > 0 ) THEN
              C_STA(J24) = C_STA_HPS(J24,1)
            ELSE 
              C_STA(J24) = MAL(1)%STA(J24)%NAME
         END IF
         FILOUT(J24) = DIR_BDSP(1:I_LEN(DIR_BDSP))//'/'// &
     &                 C_STA(J24)(1:I_LEN(C_STA(J24)))//'.bds'
         CALL NOUT ( M__HDR*LEN__HDR, HEADER )
!
! ------ Create the BINDISP file header. It consists of 8 records
!
! ------ 1-st header record: format label
!
         HEADER(1) = 'BINDISP '
!
! ------ 2-nd header record: format decription
!
         IUER = -1
         CALL DATE_TO_TIME ( BINDISP_VERSION_DATE, MJD_VER, SEC_VER, IUER )
         HDR2%MJD_FMT    = MJD_VER
         HDR2%ENDIAN_FMT = ENDIAN_FMT
         HDR2%FLOAT_FMT  = FLOAT_FMT
         HDR2%N_MOD      = L_HPS + L_EPH
         CALL LIB$MOVC3 ( LEN__HDR, HDR2, %REF(HEADER(2)) )
!
! ------ 3-rd header record: site name
!
         HEADER(3) = C_STA(J24)
!
! ------ 4-th header record: the number of data records and the sampling
! ------                     interval
!
         HDR4%NUM_REC = L_EPC_ALL
         HDR4%SAMPLING_INTERVAL = TIM_STEP
         CALL LIB$MOVC3 ( LEN__HDR, HDR4, %REF(HEADER(4)) )
!
! ------ 5,6,7-th records: X,Y,Z site coordinates in the crust-fixed reference
! ------                   frame
!
         IF ( L_EPH > 0 ) THEN
              CALL LIB$MOVC3 ( LEN__HDR, MAL(1)%STA(J24)%COO(1), %REF(HEADER(5)) )
              CALL LIB$MOVC3 ( LEN__HDR, MAL(1)%STA(J24)%COO(2), %REF(HEADER(6)) )
              CALL LIB$MOVC3 ( LEN__HDR, MAL(1)%STA(J24)%COO(3), %REF(HEADER(7)) )
            ELSE 
              CALL LIB$MOVC3 ( LEN__HDR, STA_COO_HPS(1,J24,1),   %REF(HEADER(5)) )
              CALL LIB$MOVC3 ( LEN__HDR, STA_COO_HPS(2,J24,1),   %REF(HEADER(6)) )
              CALL LIB$MOVC3 ( LEN__HDR, STA_COO_HPS(3,J24,1),   %REF(HEADER(7)) )
         END IF
!
! ------ 8-th header record: first epoch: MJD and SEC
!
         HDR8%MJD_FIRST = MJD_BEG
         HDR8%TAI_FIRST = TAI_BEG
         CALL LIB$MOVC3 ( LEN__HDR, HDR8, %REF(HEADER(8)) )
!
         L_MOD = 0
         IF ( L_EPH > 0 ) THEN
              DO 4280 J28=1,L_EPH
                 IDS(1) = LINDEX ( FILS(1,J28), '_' )
                 IDS(2) = LINDEX ( FILS(1,J28)(1:IDS(1)-1), '_' )
                 IDS(3) = LINDEX ( FILS(1,J28)(1:IDS(2)-1), '_' )
                 IDS(4) = LINDEX ( FILS(1,J28)(1:IDS(3)-1), '_' )
                 L_MOD = L_MOD + 1
                 BDSM(L_MOD)%LOAD_TYPE = FILS(1,J28)(IDS(4)+1:IDS(3)-1)
                 BDSM(L_MOD)%LOAD_NAME = FILS(1,J28)(IDS(3)+1:IDS(2)-1)
                 CALL TRAN ( 12, BDSM(L_MOD)%LOAD_TYPE, BDSM(L_MOD)%LOAD_TYPE )
                 CALL TRAN ( 12, BDSM(L_MOD)%LOAD_NAME, BDSM(L_MOD)%LOAD_NAME )
                 BDSM(L_MOD)%LOAD_VERS = '     eph'
!!                 write ( 6, * ) ' l_mod= ', int2(l_mod), bdsm(l_mod)%load_type, bdsm(l_mod)%load_name ! %%%
!!                 write ( 6, * ) ' ids= ', ids ! %%
!! /s0/test/load_list/atm_geosfpit/test_01_malo_atm_GEOSFPIT_2009_04.eph   
 4280         CONTINUE 
         END IF
         IF ( L_HPS > 0 ) THEN
              DO 4290 J29=1,L_HPS
                 IDS(1) = LINDEX ( FIL_HPS(J29), '_' )
                 IDS(2) = LINDEX ( FIL_HPS(J29)(1:IDS(1)-1), '_' )
                 IDS(3) = LINDEX ( FIL_HPS(J29)(1:IDS(2)-1), '_' )
                 L_MOD = L_MOD + 1
                 BDSM(L_MOD)%LOAD_TYPE = FIL_HPS(J29)(IDS(3)+1:IDS(2)-1)
                 BDSM(L_MOD)%LOAD_NAME = FIL_HPS(J29)(IDS(2)+1:IDS(1)-1)
                 CALL TRAN ( 12, BDSM(L_MOD)%LOAD_TYPE, BDSM(L_MOD)%LOAD_TYPE )
                 CALL TRAN ( 12, BDSM(L_MOD)%LOAD_NAME, BDSM(L_MOD)%LOAD_NAME )
                 BDSM(L_MOD)%LOAD_VERS = '     hps'
!!                 write ( 6, * ) ' l_mod= ', int2(l_mod), bdsm(l_mod)%load_type, bdsm(l_mod)%load_name ! %%%
!/s0/test/load_hps/test_01_malo_atm_GEOSFPIT_loa.hps
 4290         CONTINUE 
         END IF
!
! ------ Open the temporary output file
!
         LUN = GET_UNIT()
         OPEN ( UNIT=LUN, FILE=FILOUT(J24), STATUS='UNKNOWN', ACCESS='DIRECT', &
     &          FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IUER, STR )
              IUER = -1
              CALL ERR_LOG ( 6833, IUER, 'MEPH_TO_BDSP', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to open output file '// &
     &             FILOUT(J24) )
              CALL EXIT ( 1 )
         END IF
         L_HDR = 8
         DO 4300 J30=1,L_MOD
            L_HDR = L_HDR + 1
            CALL LIB$MOVC3 ( 8, BDSM(J30)%LOAD_TYPE, HEADER(L_HDR) )
            L_HDR = L_HDR + 1
            CALL LIB$MOVC3 ( 8, BDSM(J30)%LOAD_NAME, HEADER(L_HDR) )
            L_HDR = L_HDR + 1
            CALL LIB$MOVC3 ( 8, BDSM(J30)%LOAD_VERS, HEADER(L_HDR) )
 4300    CONTINUE 
!
! ------ Write the header
!
         DO 4310 J31=1,M__HDR
            WRITE ( UNIT=LUN, REC=J31, IOSTAT=IUER ) HEADER(J31)
            IF ( IUER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J31=',J31,' IUER=',IUER
                 IUER = -1
                 CALL ERR_LOG ( 6834, IUER, 'MEPH_TO_BDSP', 'Error in '// &
     &               'writing in the header of the output file '//FILOUT(J31) )
                 CALL EXIT ( 1 )
            END IF
 4310     CONTINUE
!
! ------ Write the body
!
!!  write ( 6, * ) 'l_hdr= ', l_hdr, ' m__hdr= ', m__hdr, ' l_epc_all = ', l_epc_all ! %%%
         DO 4320 J32=1,L_EPC_ALL
            WRITE ( UNIT=LUN, REC=J32+M__HDR, IOSTAT=IUER ) BDS(J32,J24)
            IF ( IUER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J32=',J32,' IUER=',IUER
                 IUER = -1
                 CALL ERR_LOG ( 6835, IUER, 'MEPH_TO_BDSP', &
     &               'Error in writing in the body of the output '// &
     &               'file '//FILOUT(J32) )
                 CALL EXIT ( 1 )
            END IF
 4320    CONTINUE
!
! ------ Close output file
!
         CLOSE ( UNIT=LUN  )
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 130 ) J24, L_STA_ALL, C_STA(J24)
 130          FORMAT ( 3X,I5,' (',I5,') Written file for station ',A )
              CALL FLUSH ( 6 )
         END IF
 4240 CONTINUE 
!
! --- Build summary file name
!
      FILSUM = DIR_BDSP(1:I_LEN(DIR_BDSP))//'/'//SUMMARY_BDS_FILE
!
! --- Open summary file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILSUM, STATUS='UNKNOWN', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS=',IUER
           IUER = -1
           CALL ERR_LOG ( 4829, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &         'an attempt to open summary file date '//FILSUM )
           CALL EXIT ( 1 )
      END IF
!
! --- Write the header of summery file
!
      WRITE ( LUN, '(A)' ) BINDISP_SUMMARY__LABEL
!
! --- Write current date
!
      WRITE ( LUN, '(A)' ) 'LAST_UPDATE: '//GET_CDATE()
!
! --- Write minimal date of displacement
!
      IUER = -1
      WRITE ( LUN, 140 ) 'MIN_EPOCH: ', MJD_BEG, TAI_BEG, &
     &                    MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
!
! --- Write maximal date of displacement
!
      IUER = -1
      WRITE ( LUN, 140 ) 'MAX_EPOCH: ', MJD_END, TAI_END, &
     &                    MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
 140  FORMAT ( A, I5,' ',F7.1,' ',A )
!
! --- Write the total number of epochs
!
      WRITE ( LUN, '(A,I9)'  ) 'L_EPC: ', L_EPC_ALL
!
! --- Write the total number of sites
!
      WRITE ( LUN, '(A,I9)'  ) 'L_STA: ', L_STA_ALL
!
! --- ... and the total number of displacements
!
      WRITE ( LUN, '(A,I15)' ) 'L_DSP: ', L_EPC_ALL*L_STA_ALL
!
! --- Write the area of displacements validity
!
      WRITE ( LUN, '(A,2X,F11.5,2X,F11.5,2X,"sec")' ) 'SMP_INTRV:', TIM_STEP, TIM_STEP
      WRITE ( LUN, '(A,1X,F14.6)' ) 'RD_AREA:', MALO__RD_AREA
      WRITE ( LUN, '(A)' )  '#'
      WRITE ( LUN, '(A,I2)' )  'L_MOD: ', L_MOD
      DO 4330 J33=1,L_MOD
         WRITE ( LUN, '(A,I2,1X,A,1X,A,1X,A)' )  'MODEL: ', J33, &
     &           BDSM(J33)%LOAD_TYPE, BDSM(J33)%LOAD_NAME, BDSM(J33)%LOAD_VERS
 4330 CONTINUE 
      WRITE ( LUN, '(A)' )  '#'
      WRITE ( LUN, '(A)' )  '#     Num Site ID  Dates begin         / Date end'// &
     &                      '             N points    smp_intrv      X-coordinate'// &
     &                      '  Y coordinate  Z coordinate'
      WRITE ( LUN, '(A)' )  '#'
!
! --- Cycle over stations
!
      DO 4340 J34=1,L_STA_ALL
         CALL CLRCH ( STR  )
         CALL LIB$MOVC3 ( LEN__BDSUM, %REF(STR), BDSUM )
         BDSUM%REC_ID = 'STA:'
         BDSUM%FILL_4 = ' / '
         CALL CLRCH ( STR1 )
!
! ------ Write information about the station:
! ------ name,
! ------ coordinates,
! ------ start date,
! ------ end date,
! ------ number of displacements for this station
! ------ sampling interval in days
! ------ X coordinate
! ------ Y coordinate
! ------ Z coordinate
!
         WRITE ( UNIT=BDSUM%SITE_IND, FMT='(I4)' ) J34
         BDSUM%SITE_ID = C_STA(J34)
!
         IF ( MJD_BEG .GT. 0 ) THEN
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
              IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
              BDSUM%DATE_BEG = STR1(1:19)
            ELSE
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) 'Name: ', C_STA(J34)
              WRITE ( 6, * ) 'J34=',J34,' MJD_BEG = ', MJD_BEG
              IUER = -1
              CALL ERR_LOG ( 6834, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &            'internal control: wrong MJD_BEG'  )
              CALL EXIT ( 1 )
        END IF
!
         IF ( MJD_END .GT. 0 ) THEN
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
              IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
              BDSUM%DATE_END = STR1(1:19)
            ELSE
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) 'J34= ',J34, ' MJD_END = ', MJD_END
              IUER = -1
              CALL ERR_LOG ( 6834, IUER, 'MEPH_TO_BDSP', 'Trap of '// &
     &            'internal control: wrong MJD_END'  )
              CALL EXIT ( 1 )
         END IF
!
         WRITE ( UNIT=BDSUM%NUM_PTS, FMT='(I9)' ) L_EPC_ALL
         WRITE ( UNIT=BDSUM%SAMPLE_INT, FMT='(F16.11)' ) &
     &           TIM_STEP/86400.0D0
         IF ( L_EPH > 0 ) THEN         
              WRITE ( UNIT=BDSUM%X_COORD, FMT='(F13.4)' ) MAL(1)%STA(J34)%COO(1)
              WRITE ( UNIT=BDSUM%Y_COORD, FMT='(F13.4)' ) MAL(1)%STA(J34)%COO(2)
              WRITE ( UNIT=BDSUM%Z_COORD, FMT='(F13.4)' ) MAL(1)%STA(J34)%COO(3)
            ELSE 
              WRITE ( UNIT=BDSUM%X_COORD, FMT='(F13.4)' ) STA_COO_HPS(1,J34,1)
              WRITE ( UNIT=BDSUM%Y_COORD, FMT='(F13.4)' ) STA_COO_HPS(2,J34,1)
              WRITE ( UNIT=BDSUM%Z_COORD, FMT='(F13.4)' ) STA_COO_HPS(3,J34,1)
         END IF

         BDSUM%ENDIAN_FMT = ENDIAN_FMT
         BDSUM%FLOAT_FMT  = FLOAT_FMT
!
         CALL LIB$MOVC3 ( LEN__BDSUM, BDSUM, %REF(STR) )
         WRITE ( LUN, '(A)' ) STR(1:LEN__BDSUM)
 4340 CONTINUE
!
! --- Close summary file
!
      CLOSE ( UNIT=LUN )
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) ' Summary file: '//FILSUM(1:I_LEN(FILSUM))
      END IF
!
! --- Deal done!
!
!
      END  PROGRAM  MEPH_TO_BDSP  !#!#
