      PROGRAM    SPD_RES_AZEL
! ************************************************************************
! *                                                                      *
! *   Program SPD_RES_AZEL 
! *                                                                      *
! * ###  10-JAN-2024   SPD_RES_AZEL  v1.0 (c) L. Petrov 11-JAN-2024  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE   ), POINTER :: SPD_DEL(:)
      TYPE       ( SPD_AZELS__TYPE ) :: SPD_RES
      CHARACTER  SPD_DIR*128, FILIN*128, FILOUT*128, STR*128, ERRSTR*128
      CHARACTER  OUT*32768, TMPL1*217, TMPL2*61
      DATA TMPL1 / 'Sta: xxxxxxxx | Time: xxxxxxxxxxxxxxxxxxx  MJD: xxxxx Tai: xxxxxxx sec | Az: xxxxxxx deg | El: xxxxxx deg | Del_tot: xxxxxxx ns | Del_wat: xxxxxxx ns | Sur_pres: xxxxxxx Pa | Pwp_pres: xxxxxxx Pa | Air_temp: xxxxx K |' /
      DATA TMPL2 / ' Freqs(xxx): xxxxxxxxxx MHz Opacity: xxxxxxxx Tatm: xxxxx K |' /
      INTEGER*4    SPD__M_HDR_AZEL
      PARAMETER  ( SPD__M_HDR_AZEL = 16 )
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, MJD_OBS, IDAY, LUN, IUER
      REAL*8     TAI_OBS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
!
      IF ( IARGC() < 3 ) THEN 
           WRITE ( 6, '(A)' ) 'Usage: spd_res_azel spd_dir input_file output_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, SPD_DIR )
           CALL GETARG ( 2, FILIN   )
           CALL GETARG ( 3, FILOUT  )
      END IF
!
! --- Check arguments
!
      IF ( .NOT. IS_DIR_EXIST ( SPD_DIR, ERRSTR ) ) THEN
           IUER = -1
           CALL ERR_LOG ( 4801, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &         'attempt to open diectory with computed slant path delay, '// &
     &         'atmospheric opacity and brightness temperature '//TRIM(SPD_DIR)// &
     &         ' -- '//ERRSTR )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 4802, IUER, 'SPD_RES_AZEL', 'Input file '// &
     &          TRIM(FILIN)//' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL PARSE_SPD_RES_AZEL ( FILIN, SPD_RES, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4803, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &         'parsing input file for spd computation '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SPD_DEL(SPD_RES%N_STA) )
!
      IUER = -1
      CALL SPD_RES_INTRP ( SPD_DIR, SPD_RES%N_STA, SPD_RES%C_STA, &
     &                     SPD_RES%MJD_BEG, SPD_RES%TAI_BEG, &
     &                     SPD_RES%MJD_END, SPD_RES%TAI_END, &
     &                     SPD_DEL, 'azel', IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4804, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &        'interpolation of spd results' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GET_SPD_RES_AZEL ( SPD_DEL, SPD_RES, IUER  )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4805, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &        'computation of spd results' )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( SPD_DEL )
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4806, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &        'openning output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( UNIT=LUN, FMT='(A)' ) SPD_RES_AZEL__LABEL
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Results of slant path computation'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# SPD_DIR: '//TRIM(SPD_DIR)
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Generated on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Format:'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#    6-13  A8    Station name'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   23-41  A19   TAI date in format YYYY.MM.DD-HH:SS'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   49-53  I5    MJD in days'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   49-53  I5    MJD in days'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   60-66  F7.1  TAI after the midnight in sec'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   78-84  F7.3  Azimuth   in deg'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#   96-101 F6.3  Elevation in deg'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  118-124 F7.3  Total path delay in moist air in nanoseconds'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  140-146 F7.3  Wet constituent of the path delay in moist air in nanoseconds'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  163-169 F7.0  Surface atmospheric pressure in Pa  '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  186-192 F7.0  Surface partial pressure of water vapor in Pa  '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  209-213 F5.1  Surface air temperature in K'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      IF ( SPD_RES%N_FRQ > 0 ) THEN
           WRITE ( UNIT=LUN, FMT='(A)' ) '#  227-229 I3    Frequency index'
           WRITE ( UNIT=LUN, FMT='(A)' ) '#  233-242 F10.3 Sky frequency in GHz'
           WRITE ( UNIT=LUN, FMT='(A)' ) '#  257-264 F10.3 Atmosphere opacity'
           WRITE ( UNIT=LUN, FMT='(A)' ) '#  272-276 F5.1  Atmosphere briskness temperature in K'
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A,I3,A)' ) '#  The previous block is repeated ', SPD_RES%N_FRQ, ' times'
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      END IF
!
      WRITE ( UNIT=LUN, FMT='("N_FRQ: ", I3)' ) SPD_RES%N_FRQ 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 410 J1=1,SPD_RES%N_AZEL
         MJD_OBS = SPD_RES%MJD_BEG
         TAI_OBS = SPD_RES%TAI_BEG + SPD_RES%AZEL(J1)%TIM
         IF ( TAI_OBS > 86400.0D0 ) THEN
              IDAY = IDINT(TAI_OBS/86400.0D0)
              TAI_OBS = TAI_OBS - IDAY*86400.0D0
              MJD_OBS = MJD_OBS + IDAY
         END IF
         OUT = TMPL1
         OUT(6:13) = SPD_RES%AZEL(J1)%STA_NAM
         STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IUER  )
         OUT(23:41) = STR(1:19)
         WRITE ( UNIT=OUT(49:53),   FMT='(I5)'   ) MJD_OBS
         WRITE ( UNIT=OUT(60:66),   FMT='(F7.1)' ) TAI_OBS
         WRITE ( UNIT=OUT(78:84),   FMT='(F7.3)' ) SPD_RES%AZEL(J1)%AZ/DEG__TO__RAD
         WRITE ( UNIT=OUT(96:101),  FMT='(F6.3)' ) SPD_RES%AZEL(J1)%EL/DEG__TO__RAD
         WRITE ( UNIT=OUT(118:124), FMT='(F7.3)' ) SPD_RES%AZEL(J1)%DELS(SPD__TOT)*1.D9
         WRITE ( UNIT=OUT(140:146), FMT='(F7.3)' ) SPD_RES%AZEL(J1)%DELS(SPD__WAT)*1.D9
         WRITE ( UNIT=OUT(163:169), FMT='(F7.0)' ) SPD_RES%AZEL(J1)%PRES
         WRITE ( UNIT=OUT(186:192), FMT='(F7.0)' ) SPD_RES%AZEL(J1)%PWP
         WRITE ( UNIT=OUT(209:213), FMT='(F5.1)' ) SPD_RES%AZEL(J1)%TEMP
!
         IF ( SPD_RES%N_FRQ > 0 ) THEN
              DO 420 J2=1,SPD_RES%N_FRQ
                 STR = TMPL2
                 WRITE ( UNIT=STR(8:10),  FMT='(I3)'    ) J2
                 WRITE ( UNIT=STR(14:23), FMT='(F10.1)' ) SPD_RES%FREQ(J2)*1.D-6
                 WRITE ( UNIT=STR(38:45), FMT='(F8.4)'  ) SPD_RES%AZEL(J1)%OPA(J2)
                 WRITE ( UNIT=STR(53:57), FMT='(F5.1)'  ) SPD_RES%AZEL(J1)%TATM(J2)
                 OUT(217+(J2-1)*61+1:217+(J2-1)*61+61) = STR(1:61)
 420          CONTINUE 
         END IF
         WRITE ( LUN, '(A)' ) TRIM(OUT)
 410  CONTINUE 
      CLOSE ( UNIT=LUN )
!
      END PROGRAM  SPD_RES_AZEL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_SPD_RES_AZEL ( FILIN, SPD_RES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_SPD_RES_AZEL
! *                                                                      *
! * ### 10-JAN-2024 PARSE_SPD_RES_AZEL v1.0 (c) L. Petrov 10-JAN-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_AZELS__TYPE ) :: SPD_RES
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = SPD__M_STA )
      PARAMETER  ( MIND =        128 )
      CHARACTER  BUF(MBUF)*512, STR*128, STR1*128
      REAL*8     TIM, FRQ
      INTEGER*4  J1, J2, J3, J4, IFRQ, MJD, ISTA, NBUF, &
     &           LIND, IND(2,MIND), K_AZEL, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, LTM_DIF
      INTEGER*8, EXTERNAL :: IFIND_PL8 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4811, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &         'reading input file for spd computation '//FILIN )
           RETURN 
     END IF
!
      SPD_RES%N_STA = 0
      SPD_RES%N_FRQ = 0
      SPD_RES%MJD_BEG = 0
      SPD_RES%MJD_END = 0
      SPD_RES%N_AZEL  = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 8 ) THEN
              CALL ERR_LOG ( 4812, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'processing the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- it has less than 8 words' )
              RETURN 
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Sta:' ) THEN
              ISTA = ADD_CLIST ( SPD__M_STA, SPD_RES%N_STA, SPD_RES%C_STA, &
     &                            BUF(J1)(IND(1,2):IND(2,2)), IER )
            ELSE
              CALL ERR_LOG ( 4813, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'processing the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- the second word is '// &
     &            BUF(J1)(IND(1,1):IND(2,1))//' while Sta: was expected' )
              RETURN 
         END IF
         SPD_RES%N_AZEL = SPD_RES%N_AZEL + 1
         IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'Time:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,4):IND(2,4)), MJD, TIM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4814, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &                 'processing the '//TRIM(STR)//'th line of the input '// &
     &                 'file '//TRIM(FILIN)//' -- error in parsing time field' )
                   RETURN 
              END IF
              IF ( SPD_RES%MJD_BEG == 0 ) THEN
                   SPD_RES%MJD_BEG = MJD
                   SPD_RES%TAI_BEG = TIM
                 ELSE
                   SPD_RES%MJD_BEG = MIN ( SPD_RES%MJD_BEG, MJD )
                   SPD_RES%TAI_BEG = MIN ( SPD_RES%TAI_BEG, TIM )
              END IF
              IF ( SPD_RES%MJD_END == 0 ) THEN
                   SPD_RES%MJD_END = MJD
                   SPD_RES%TAI_END = TIM
                 ELSE 
                   SPD_RES%MJD_END = MAX ( SPD_RES%MJD_END, MJD )
                   SPD_RES%TAI_END = MAX ( SPD_RES%TAI_END, TIM )
              END IF
            ELSE
              CALL ERR_LOG ( 4815, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'processing the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- the third word is '// &
     &            BUF(J1)(IND(1,3):IND(2,3))//' while Time: was expected' )
              RETURN 
         END IF
         IF ( BUF(J1)(IND(1,5):IND(2,5)) == 'Az:' ) THEN
              CONTINUE 
            ELSE
              CALL ERR_LOG ( 4816, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'processing the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- the fifth word is '// &
     &            BUF(J1)(IND(1,5):IND(2,5))//' while Az: was expected' )
              RETURN 
         END IF
         IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'El:' ) THEN
              CONTINUE 
            ELSE
              CALL ERR_LOG ( 4817, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'processing the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- the seventh word is '// &
     &            BUF(J1)(IND(1,7):IND(2,7))//' while Az: was expected' )
              RETURN 
         END IF
         IF ( LIND > 9 ) THEN
              IF ( BUF(J1)(IND(1,9):IND(2,9)) == 'Freqs:' ) THEN
                   CONTINUE 
                 ELSE
                   CALL ERR_LOG ( 4818, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &                 'processing the '//TRIM(STR)//'th line of the input '// &
     &                 'file '//TRIM(FILIN)//' -- the ninth word is '// &
     &                  BUF(J1)(IND(1,9):IND(2,9))//' while Freqs: was expected' )
                   RETURN 
              END IF
!
              DO 420 J2=10,LIND
                 READ ( UNIT=BUF(J1)(IND(1,J2):IND(2,J2)), FMT='(F12.5)', IOSTAT=IER ) FRQ
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR1 ) 
                      CALL INCH  ( J2, STR1 )
                      CALL ERR_LOG ( 4819, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &                    'reading the '//TRIM(STR1)//'th word while processing '// &
     &                    'the '//TRIM(STR)//'th line of the input '// &
     &                    'file '//TRIM(FILIN)//' -- '//BUF(J1)(IND(1,J2):IND(2,J2)) )
                      RETURN 
                 END IF
                 IF ( SPD_RES%N_FRQ == 0 ) THEN
                      SPD_RES%N_FRQ = 1
                      SPD_RES%FREQ_I8(SPD_RES%N_FRQ) = INT8(FRQ)
                      SPD_RES%FREQ(SPD_RES%N_FRQ) = FRQ
                    ELSE
                      IFRQ = IFIND_PL8 ( INT(SPD_RES%N_FRQ,KIND=8), SPD_RES%FREQ_I8, INT8(FRQ) )
                      IF ( IFRQ < 1 ) THEN
                           SPD_RES%N_FRQ = SPD_RES%N_FRQ + 1
                           SPD_RES%FREQ_I8(SPD_RES%N_FRQ) = INT8(FRQ)
                           SPD_RES%FREQ(SPD_RES%N_FRQ) = FRQ
                      END IF
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      ALLOCATE ( SPD_RES%AZEL(SPD_RES%N_AZEL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4820, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &         'an attempt to allocate memore for SPD_RES' )
           RETURN 
      END IF
      CALL SORT_CH ( SPD_RES%N_STA, SPD_RES%C_STA )
      CALL SORT_I8 ( INT(SPD_RES%N_FRQ,KIND=8), SPD_RES%FREQ_I8 )
      CALL SORT_R8 (     SPD_RES%N_FRQ,         SPD_RES%FREQ    )
!
      K_AZEL = 0
      DO 430 J3=1,NBUF
         IF ( BUF(J3)(1:1) == '#' ) GOTO 430
         CALL CLRCH ( STR )
         CALL INCH  ( J3, STR )
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         ISTA = LTM_DIF ( 0, SPD_RES%N_STA, SPD_RES%C_STA, BUF(J3)(IND(1,2):IND(2,2)) )
         K_AZEL = K_AZEL + 1
         CALL DATE_TO_TIME ( BUF(J3)(IND(1,4):IND(2,4)), MJD, TIM, IER )
         SPD_RES%AZEL(K_AZEL)%STA_NAM = BUF(J3)(IND(1,2):IND(2,2)) 
         SPD_RES%AZEL(K_AZEL)%TIM = (MJD - SPD_RES%MJD_BEG)*86400.0D0 + &
     &                              (TIM - SPD_RES%TAI_BEG)
!
         READ ( UNIT=BUF(J3)(IND(1,6):IND(2,6)), FMT='(F12.5)', IOSTAT=IER ) SPD_RES%AZEL(K_AZEL)%AZ
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4821, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'reading the sixth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,6):IND(2,6)) )
              RETURN 
         END IF
         IF ( SPD_RES%AZEL(K_AZEL)%AZ < 0.0D0 ) THEN
              CALL ERR_LOG ( 4822, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'parsing the sixth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,6):IND(2,6))// &
     &            ' < 0.0' )
              RETURN 
         END IF
         IF ( SPD_RES%AZEL(K_AZEL)%AZ > 360.0D0 ) THEN
              CALL ERR_LOG ( 4823, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'parsing the sixth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,6):IND(2,6))// &
     &            ' > 360.0' )
              RETURN 
         END IF
         SPD_RES%AZEL(K_AZEL)%AZ = SPD_RES%AZEL(K_AZEL)%AZ*DEG__TO__RAD
!
         READ ( UNIT=BUF(J3)(IND(1,8):IND(2,8)), FMT='(F12.5)', IOSTAT=IER ) SPD_RES%AZEL(K_AZEL)%EL
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4824, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'reading the eighth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,8):IND(2,8)) )
              RETURN 
         END IF
         IF ( SPD_RES%AZEL(K_AZEL)%EL < SPD__DEL_MIN_DEG ) THEN
              CALL ERR_LOG ( 4825, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'parsing the eighth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,8):IND(2,8))// &
     &            ' < 3.0' )
              RETURN 
         END IF
         IF ( SPD_RES%AZEL(K_AZEL)%EL > 90.0D0 ) THEN
              CALL ERR_LOG ( 4826, IUER, 'SPD_RES_AZEL', 'Error in '// &
     &            'parsing the eighth word while processing '// &
     &            'the '//TRIM(STR)//'th line of the input '// &
     &            'file '//TRIM(FILIN)//' -- '//BUF(J3)(IND(1,8):IND(2,8))// &
     &            ' > 90.0' )
              RETURN 
         END IF
         SPD_RES%AZEL(K_AZEL)%EL = SPD_RES%AZEL(K_AZEL)%EL*DEG__TO__RAD
         SPD_RES%AZEL(K_AZEL)%STATUS = SPD__READ
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_SPD_RES_AZEL  !#!  
