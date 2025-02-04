      SUBROUTINE SPD_RES_INTRP ( SPD_DIR, N_STA, C_STA, MJD_BEG, TAI_BEG, &
     &                           MJD_END, TAI_END, SPD_DEL, MODE_STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_RES_INTRP
! *                                                                      *
! * ### 05-JAN-2024 SPD_RES_INTRP  v2.0 (c)  L. Petrov  27-JAN-2024  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  N_STA, MJD_BEG, MJD_END, IUER 
      CHARACTER  SPD_DIR*(*), C_STA(N_STA)*(*), MODE_STR*(*)
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL(N_STA)
      REAL*8     TAI_BEG, TAI_END
      INTEGER*4    M_FIL
      PARAMETER  ( M_FIL = 128*1024 )
      CHARACTER    C_FIL(M_FIL)*128, FINAM*128, DATE_BEG_STR*32, &
     &             DATE_2ND_STR*32, DATE_END_STR*32, DATE_OBS_STR*32, &
     &             ERR_STR*128, STR*128, STR1*128
      TYPE     ( SPD__ASCII__TYPE ), POINTER :: SAT(:)
      INTEGER*8  DIR_DESC(16), SIZE_I8
      REAL*8     TIM_BEG_FIL, TIM_2ND_FIL, TIM_END_FIL, TIM_OBS_FIL, &
     &           TIM_STP, TAI_OBS
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, &
     &           MJD_BEG_FIL, MJD_2ND_FIL, MJD_END_FIL, MJD_OBS_FIL, &
     &           IL, IS, LEV, L_FIL, IND_BEG, IND_END, IND_MRG, &
     &           L_TIM, I_TIM, ISTA, IDEL, DIMS(4), DIMSD(4), &
     &           IVRB, IER
      REAL*8     EPS_TIM, MARGIN_TIM
      PARAMETER  ( EPS_TIM = 0.01D0 )
      PARAMETER  ( MARGIN_TIM = 86400.01D0 )
      PARAMETER  ( IND_MRG = 3  )
      REAL*8, EXTERNAL    :: DEL_ISA
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
!
      IVRB = 3
!
      IF ( .NOT. IS_DIR_EXIST ( SPD_DIR, ERR_STR ) ) THEN
           CALL ERR_LOG ( 4541, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &         'attempt to open diectory with computed slant path delay, '// &
     &         'atmospheric opacity and brightness temperature '//TRIM(SPD_DIR)// &
     &         ' -- '//ERR_STR )
           RETURN 
      END IF
!
! --- Travel the directory tree and collect relevant files
!
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, SPD_DIR, FINAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4542, IUER, 'PIMA_OPAL', 'Error in '// &
     &                 'reading input directory '//SPD_DIR )
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FINAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FINAM, '~' ) .GT. 0 ) GOTO 410
         IL = ILEN(FINAM)
         IF ( IL < 18 ) GOTO 410
         IF ( FINAM(IL-3:IL) == '.spd' .OR. FINAM(IL-7:IL) == '.spd.bz2' ) THEN
!
! ----------- Extract the date of data by parsing file names
!
              IL = ILEN(FINAM)
              IF ( FINAM(IL-7:IL) == '.spd.bz2' ) IL = IL - 4
              DATE_OBS_STR = FINAM(IL-16:IL-13)//'.'//FINAM(IL-12:IL-11)//'.'// &
     &                       FINAM(IL-10:IL-6)//':'//FINAM(IL-5:IL-4)//':00.0'

!
! ----------- Transform the filename into date
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_OBS_STR, MJD_OBS_FIL, TIM_OBS_FIL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4543, IUER, 'SPD_RES_INTRP', 'Wrong format of the '// &
     &                 'data file name '//TRIM(FINAM)// &
     &                 ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &                 'a valid calendar date' )
                   RETURN 
              END IF
              IF ( (MJD_OBS_FIL - MJD_BEG)*86400.0D0 - (TIM_OBS_FIL - TAI_BEG) > -MARGIN_TIM .AND. &
     &             (MJD_OBS_FIL - MJD_END)*86400.0D0 - (TIM_OBS_FIL - TAI_END) <  MARGIN_TIM       ) THEN
                   L_FIL = L_FIL + 1
                   IF ( L_FIL > M_FIL) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_FIL, STR )
                        CALL ERR_LOG ( 4544, IUER, 'SPD_RES_INTRP', 'Too many files '// &
     &                      'in directory '//TRIM(SPD_DIR)//' -- '// &
     &                      'more than '//STR )
                        RETURN 
                   END IF
                   C_FIL(L_FIL) = FINAM
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL < 1 ) THEN
           CALL ERR_LOG ( 4545, IUER, 'SPD_RES_INTRP', 'No valid data files '// &
     &         'with atmosphere opatcity/brightness were found in the '// &
     &         'directory '//SPD_DIR )
           RETURN
      END IF
      IF ( L_FIL == 1 ) THEN
           CALL ERR_LOG ( 4546, IUER, 'SPD_RES_INTRP', 'Only one valid data '// &
     &         'files with atmosphere opatcity/brightness were found '// &
     &         'in the input directory '//TRIM(SPD_DIR)// &
     &         ' while at least two data files are needed' )
           RETURN 
      END IF
!
! --- Sort data files in alphabetic order which is equivalent to time order
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
! --- Extract the date of data by parsing file names
!
      IL = ILEN(C_FIL(1))
      IF ( C_FIL(1)(IL-7:IL) == '.spd.bz2' ) IL = IL - 4
      DATE_BEG_STR = C_FIL(1)(IL-16:IL-13)//'.'//C_FIL(1)(IL-12:IL-11)//'.'// &
     &               C_FIL(1)(IL-10:IL-6)//':'//C_FIL(1)(IL-5:IL-4)//':00.0'
!
      IL = ILEN(C_FIL(L_FIL))
      IF ( C_FIL(L_FIL)(IL-7:IL) == '.spd.bz2' ) IL = IL - 4
      DATE_END_STR = C_FIL(L_FIL)(IL-16:IL-13)//'.'//C_FIL(L_FIL)(IL-12:IL-11)//'.'// &
     &               C_FIL(L_FIL)(IL-10:IL-6)//':'//C_FIL(L_FIL)(IL-5:IL-4)//':00.0'
!
! --- Check the dates. Opacity data should start before the 1st observations
! --- and end after the last observations.
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG_FIL, TIM_BEG_FIL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4547, IUER, 'SPD_RES_INTRP', 'Wrong format of the '// &
     &         'data file name '//TRIM(C_FIL(1))// &
     &         ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &         'a valid calendar date' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_END_STR, MJD_END_FIL, TIM_END_FIL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4548, IUER, 'SPD_RES_INTRP', 'Wrong format of the '// &
     &         'data file name '//TRIM(C_FIL(L_FIL))// &
     &         ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &         'a valid calendar date' )
           RETURN 
      END IF
      IF ( (MJD_BEG_FIL-MJD_BEG)*86400.0 + (TIM_BEG_FIL-TAI_BEG) > -EPS_TIM ) THEN
           MJD_BEG = MJD_BEG_FIL
           TAI_BEG = TIM_BEG_FIL
      END IF
      IF ( (MJD_END_FIL-MJD_END)*86400.0 + (TIM_END_FIL-TAI_END) <  EPS_TIM ) THEN
           MJD_END = MJD_END_FIL
           TAI_END = TIM_END_FIL
      END IF
!
      L_TIM = L_FIL
!
! --- Allocate memory for the ascii representation of data files with opacity
!
      ALLOCATE ( SAT(L_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( L_TIM*SIZEOF(SAT(1)), STR )
           CALL ERR_LOG ( 4549, IUER, 'SPD_RES_INTRP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes if dynamic memory for array '// &
     &         'SAT' )
           RETURN
      END IF
!
      DO 420 J2=1,N_STA
         CALL ERR_PASS ( IUER, IER )
         CALL SPD_DEL_INIT ( SPD_DEL(J2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4550, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &            'attempt to initialize SPD_DEL object' )
              RETURN
         END IF
 420  CONTINUE 
!
      DO 430 J3=1,L_TIM
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 210 ) J3, L_TIM
 210          FORMAT ( 'SPD_RES_INTP procesing epoch ', I6, ' of ', I6 )
         END IF
         IL = ILEN(C_FIL(J3))
         IF ( C_FIL(J3)(IL-7:IL) == '.spd.bz2' ) IL = IL - 4
         DATE_OBS_STR = C_FIL(J3)(IL-16:IL-13)//'.'//C_FIL(J3)(IL-12:IL-11)//'.'// &
     &                  C_FIL(J3)(IL-10:IL-6)//':'//C_FIL(J3)(IL-5:IL-4)//':00.0'
!
         CALL ERR_PASS ( IUER, IER )
         SAT%PROC_O_MODE = SPD__YES
         IF ( MODE_STR(1:4) == 'zen_' ) THEN
              CALL SPD_3D_READ_ASCII_ZEN ( C_FIL(J3), MODE_STR(5:), SAT(J3), IER )
            ELSE 
              CALL SPD_3D_READ_ASCII ( C_FIL(J3), SAT(J3), IER )
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4551, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &            'attempt to read input file with atmosphere opacity '// &
     &            'and radiative temperature '//C_FIL(J3) )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_OBS_STR, MJD_OBS_FIL, TIM_OBS_FIL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4552, IUER, 'SPD_RES_INTRP', 'Wrong format of the '// &
     &            'data file name '//TRIM(C_FIL(1))// &
     &            ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &            'a valid calendar date' )
              RETURN 
         END IF
!
! ------ Find ISTA -- index of the J3-th station from the PIM%C_STA list
! ------ in SAT station list
!
         DO 440 J4=1,N_STA
            ISTA = 0
            DO 450 J5=1,SAT(J3)%NS
               IF ( SAT(J3)%SLINE(J5)%STA_NAME == C_STA(J4) ) THEN
                    ISTA = J5  ! Index of the J5-th station in the SAT array 
               END IF
 450        CONTINUE 
            IF ( ISTA == 0 ) THEN
                 CALL ERR_LOG ( 4553, IUER, 'SPD_RES_INTRP', 'Opacity and '// &
     &               'atmosphere brightness temperature were not computed '// &
     &               'for station '//C_STA(J4)//' in the spd input file '// &
     &                C_FIL(J3) )
                RETURN
            END IF
!
            IF ( J3 == 1 ) THEN
                 SPD_DEL(J4)%MJD_OBS_FIRST = MJD_OBS_FIL
                 SPD_DEL(J4)%TAI_OBS_FIRST = TIM_OBS_FIL
                 SPD_DEL(J4)%ELV%N_EL = SAT(J3)%NE
                 SPD_DEL(J4)%AZM%N_AZ = SAT(J3)%NA
                 SPD_DEL(J4)%N_TIM    = L_TIM
                 SPD_DEL(J4)%N_FRQ    = SAT(J3)%NF
!
                 IF ( SAT(J3)%O_MODE == SPD__NAZ ) THEN
                      DIMS(1) = SPD_DEL(J4)%N_FRQ
                      DIMS(2) = SPD_DEL(J4)%ELV%N_EL
                      DIMS(3) = SPD_DEL(J4)%AZM%N_AZ
                      DIMS(4) = SPD_DEL(J4)%N_TIM
                      SPD_DEL(J4)%MODE_OPA_TAT = SPD__NAZ
                   ELSE IF ( SAT(J3)%O_MODE == SPD__1AZ ) THEN
                      DIMS(1) = SPD_DEL(J4)%N_FRQ
                      DIMS(2) = SPD_DEL(J4)%ELV%N_EL
                      DIMS(3) = SPD_DEL(J4)%N_TIM
                      DIMS(4) = 1
                      SPD_DEL(J4)%MODE_OPA_TAT = SPD__1AZ
                   ELSE
                      CALL ERR_LOG ( 4554, IUER, 'SPD_RES_INTRP', 'Opacity and '// &
     &                    'atmospere brighness temperature were not computed '// &
     &                    'for station '//C_STA(J4)//' in the spd input file '// &
     &                     C_FIL(J3) )
                      RETURN
                 END IF
!
                 DIMSD(1) = SPD_DEL(J4)%ELV%N_EL
                 DIMSD(2) = SPD_DEL(J4)%AZM%N_AZ
                 DIMSD(3) = SPD_DEL(J4)%N_TIM
                 DIMSD(4) = 2
                 IF ( MODE_STR == 'azel' ) THEN
                      ALLOCATE ( SPD_DEL(J4)%OPA(1-SPD__MDEG:DIMS(1),   &
     &                                       1-SPD__MDEG:DIMS(2),   &
     &                                       1-SPD__MDEG:DIMS(3),   &
     &                                       1-SPD__MDEG:DIMS(4)),  &
                                 SPD_DEL(J4)%TAT(1-SPD__MDEG:DIMS(1),   &
     &                                       1-SPD__MDEG:DIMS(2),   &
     &                                       1-SPD__MDEG:DIMS(3),   &
     &                                       1-SPD__MDEG:DIMS(4)),  &
                                 SPD_DEL(J4)%DELS(1-SPD__MDEG:DIMSD(1),  &
     &                                            1-SPD__MDEG:DIMSD(2),  &
     &                                            1-SPD__MDEG:DIMSD(3),  &
     &                                            1-SPD__MDEG:DIMSD(4)), &
     &                           SPD_DEL(J4)%SUR_PRS(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_PWP(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_TEM(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%MAP_ARR(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%ELEV(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%MAP(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%AZM%AZIM(SPD_DEL(J4)%AZM%N_AZ), &
     &                           SPD_DEL(J4)%TIM_ARR(SPD_DEL(J4)%N_TIM),    &
     &                           SPD_DEL(J4)%FRQ_ARR(SPD_DEL(J4)%N_FRQ),    &
     &                           SPD_DEL(J4)%ZEN_DEL(SPD_DEL(J4)%N_TIM,SPD__MWAV), &
     &                                       STAT=IER   )
                   ELSE IF ( MODE_STR == 'zen_opa' .AND. &
     &                       SAT(J3)%O_MODE == SPD__1AZ  ) THEN
                      ALLOCATE ( SPD_DEL(J4)%OPA(1-SPD__MDEG:DIMS(1),   &
     &                                           1-SPD__MDEG:DIMS(3),   &
     &                                           1,   &
     &                                           1),  &
     &                           SPD_DEL(J4)%SUR_PRS(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_PWP(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_TEM(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%MAP_ARR(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%ELEV(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%MAP(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%AZM%AZIM(SPD_DEL(J4)%AZM%N_AZ), &
     &                           SPD_DEL(J4)%TIM_ARR(SPD_DEL(J4)%N_TIM),    &
     &                           SPD_DEL(J4)%FRQ_ARR(SPD_DEL(J4)%N_FRQ),    &
     &                           SPD_DEL(J4)%ZEN_DEL(SPD_DEL(J4)%N_TIM,SPD__MWAV), &
     &                           STAT=IER   )
                   ELSE IF ( MODE_STR == 'zen_tatm'     .AND. &
     &                       SAT(J3)%O_MODE == SPD__1AZ       ) THEN
                      ALLOCATE ( SPD_DEL(J4)%TAT(1-SPD__MDEG:DIMS(1),   &
     &                                           1-SPD__MDEG:DIMS(3),   &
     &                                           1,   &
     &                                           1),  &
     &                           SPD_DEL(J4)%SUR_PRS(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_PWP(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_TEM(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%MAP_ARR(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%ELEV(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%MAP(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%AZM%AZIM(SPD_DEL(J4)%AZM%N_AZ), &
     &                           SPD_DEL(J4)%TIM_ARR(SPD_DEL(J4)%N_TIM),    &
     &                           SPD_DEL(J4)%FRQ_ARR(SPD_DEL(J4)%N_FRQ),    &
     &                           SPD_DEL(J4)%ZEN_DEL(SPD_DEL(J4)%N_TIM,SPD__MWAV), &
     &                           STAT=IER   )
                   ELSE IF ( MODE_STR == 'zen_del'  .OR. &
     &                       MODE_STR == 'zen_deld' .OR. &
     &                       MODE_STR == 'zen_delw' .OR. &
     &                       MODE_STR == 'zen_delt'      ) THEN
                      ALLOCATE ( SPD_DEL(J4)%DELS(1-SPD__MDEG:DIMSD(3), &
     &                                            1:DIMSD(4), &
     &                                            1,   &
     &                                            1),  &
     &                           SPD_DEL(J4)%SUR_PRS(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_PWP(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%SUR_TEM(1-SPD__MDEG:SPD_DEL(J4)%N_TIM), &
     &                           SPD_DEL(J4)%MAP_ARR(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%ELEV(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%ELV%MAP(SPD_DEL(J4)%ELV%N_EL), &
     &                           SPD_DEL(J4)%AZM%AZIM(SPD_DEL(J4)%AZM%N_AZ), &
     &                           SPD_DEL(J4)%TIM_ARR(SPD_DEL(J4)%N_TIM),    &
     &                           SPD_DEL(J4)%FRQ_ARR(SPD_DEL(J4)%N_FRQ),    &
     &                           SPD_DEL(J4)%ZEN_DEL(SPD_DEL(J4)%N_TIM,SPD__MWAV), &
     &                           STAT=IER   )
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      SIZE_I8 = INT8(4)*INT8(2)* &
     &                                    INT8(DIMS(1)+SPD__MDEG)*    &
     &                                    INT8(DIMS(2)+SPD__MDEG)*    &
     &                                    INT8(DIMS(3)+SPD__MDEG)*    &
     &                                    INT8(DIMS(4)+SPD__MDEG) +   &
     &                         INT8(4)*INT8(2)*                       &
     &                                    INT8(DIMSD(1)+SPD__MDEG)*   &
     &                                    INT8(DIMSD(2)+SPD__MDEG)*   &
     &                                    INT8(DIMSD(3)+SPD__MDEG) +  &
     &                         INT8(4)*INT8(8)*INT8(SPD_DEL(J4)%N_TIM)     + &
     &                         INT8(4)*INT8(4)*INT8(SPD_DEL(J4)%ELV%N_EL)  + &
     &                         INT8(4)*INT8(1)*INT8(SPD_DEL(J4)%AZM%N_AZ)  + &
     &                         INT8(4)*INT8(1)*INT8(SPD_DEL(J4)%N_FRQ) 
                      CALL CLRCH ( STR )
                      CALL INCH8 ( SIZE_I8, STR )
                      CALL ERR_LOG ( 4555, IUER, 'SPD_RES_INTRP', 'Failure '// &
     &                    'to allocate '//TRIM(STR)//' bytes of dynamic memory' )
                      RETURN 
                 END IF
!
                 SPD_DEL(J4)%STATUS = SPD__ALLO
                 IF ( MODE_STR(1:4) == 'azel' ) THEN
!
! ------------------- Generate the array of elevation angles and mapping functions
!
                      DO 470 J7=1,SPD_DEL(J4)%ELV%N_EL
                         READ ( UNIT=SAT(J3)%ELINE(J7)%ANG, FMT='(F10.6)', IOSTAT=IER ) SPD_DEL(J4)%ELV%ELEV(J7)
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 4556, IUER, 'SPD_RES_INTRP', 'Trap of internal '// &
     &                            'control: wrong elevation angle format: '//SAT(J3)%ELINE(J7)%ANG )
                              RETURN 
                         END IF
                         SPD_DEL(J4)%ELV%ELEV(J7) = SPD_DEL(J4)%ELV%ELEV(J7)*DEG__TO__RAD
!
                         SPD_DEL(J4)%ELV%MAP(J7)  = DEL_ISA ( DBLE(SPD_DEL(J4)%ELV%ELEV(J7)) )/ DEL_ISA ( P2I )
                         SPD_DEL(J4)%MAP_ARR(J7)  = SPD_DEL(J4)%ELV%MAP(J7)  
 470                  CONTINUE 
!
! ----------------- Generate the array of azimuth angles
!
                      DO 480 J8=1,SPD_DEL(J4)%AZM%N_AZ
                         READ ( UNIT=SAT(J3)%ALINE(J8)%ANG, FMT='(F10.6)', IOSTAT=IER ) SPD_DEL(J4)%AZM%AZIM(J8)
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 4557, IUER, 'SPD_RES_INTRP', 'Trap of internal '// &
     &                            'control: wrong azimuth angle format: '//SAT(J3)%ALINE(J8)%ANG )
                              RETURN 
                         END IF
                         SPD_DEL(J4)%AZM%AZIM(J8) = SPD_DEL(J4)%AZM%AZIM(J8)*DEG__TO__RAD
 480                  CONTINUE 
                 END IF
!
! -------------- Generate the frequency array
!
                 DO 490 J9=1,SPD_DEL(J4)%N_FRQ
                    READ ( UNIT=SAT(J3)%FLINE(J9)%FRQ, FMT='(F15.5)', IOSTAT=IER ) SPD_DEL(J4)%FRQ_ARR(J9)
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 4558, IUER, 'SPD_RES_INTRP', 'Trap of internal '// &
     &                       'control: wrong frequency format: '//SAT(J3)%FLINE(J9)%FRQ )
                         RETURN 
                    END IF
 490             CONTINUE 
            END IF !  end of J3 == 1
!
            IF ( MODE_STR == 'azel' ) THEN
                 IF ( SAT(J3)%O_MODE == SPD__NAZ ) THEN
                      DO 4100 J10=1,SPD_DEL(J4)%AZM%N_AZ
                         DO 4110 J11=1,SPD_DEL(J4)%ELV%N_EL
                            READ ( UNIT=SAT(J3)%DLINE(J11,J10,ISTA)%DEL1, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%DELS(J11,J10,J3,SPD__TOT)
                            READ ( UNIT=SAT(J3)%DLINE(J11,J10,ISTA)%DEL2, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%DELS(J11,J10,J3,SPD__WAT)
!
                            DO 4120 J12=1,SPD_DEL(J4)%N_FRQ
                               READ ( UNIT=SAT(J3)%OLINE(J12,J11,J10,ISTA)%OPA, FMT='(F8.4)', IOSTAT=IER ) &
     &                                SPD_DEL(J4)%OPA(J12,J10,J10,J3)
                               READ ( UNIT=SAT(J3)%OLINE(J12,J10,J10,ISTA)%TAT, FMT='(F8.4)', IOSTAT=IER ) &
     &                                SPD_DEL(J4)%TAT(J12,J10,J10,J3)
 4120                       CONTINUE 
 4110                     CONTINUE 
 4100                 CONTINUE 
                   ELSE IF ( SAT(J3)%O_MODE == SPD__1AZ ) THEN
                      DO 4130 J13=1,SPD_DEL(J4)%ELV%N_EL
                         DO 4140 J14=1,SPD_DEL(J4)%N_FRQ
                            READ ( UNIT=SAT(J3)%OLINE(J14,J13,1,ISTA)%OPA, FMT='(F6.4)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%OPA(J14,J13,J3,1)
                            READ ( UNIT=SAT(J3)%OLINE(J14,J13,1,ISTA)%TAT, FMT='(F6.1)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%TAT(J14,J13,J3,1)
 4140                    CONTINUE 
!
                         DO 4150 J15=1,SPD_DEL(J4)%AZM%N_AZ
                            READ ( UNIT=SAT(J3)%DLINE(J13,J15,ISTA)%DEL1, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%DELS(J13,J15,J3,SPD__TOT)
                            READ ( UNIT=SAT(J3)%DLINE(J13,J15,ISTA)%DEL2, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                             SPD_DEL(J4)%DELS(J13,J15,J3,SPD__WAT)
 4150                    CONTINUE 
 4130                 CONTINUE 
                 END IF
               ELSE IF ( MODE_STR == 'zen_opa' ) THEN
                 DO 4160 J16=1,SPD_DEL(J4)%N_FRQ
                    READ ( UNIT=SAT(J3)%OLINE(J16,1,1,ISTA)%OPA, FMT='(F6.4)', IOSTAT=IER ) &
     &                          SPD_DEL(J4)%OPA(J16,J3,1,1)
 4160            CONTINUE 
               ELSE IF ( MODE_STR == 'zen_tatm' ) THEN
                 DO 4170 J17=1,SPD_DEL(J4)%N_FRQ
                    READ ( UNIT=SAT(J3)%OLINE(J17,1,1,ISTA)%TAT, FMT='(F6.1)', IOSTAT=IER ) &
     &                          SPD_DEL(J4)%TAT(J17,J3,1,1)
 4170            CONTINUE 
               ELSE IF ( MODE_STR == 'zen_del'  .OR. &
     &                   MODE_STR == 'zen_deld' .OR. &
     &                   MODE_STR == 'zen_delt' .OR. &
     &                   MODE_STR == 'zen_delw'      ) THEN
                 READ ( UNIT=SAT(J3)%DLINE(1,1,ISTA)%DEL1, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                  SPD_DEL(J4)%DELS(J3,SPD__TOT,1,1)
                 READ ( UNIT=SAT(J3)%DLINE(1,1,ISTA)%DEL2, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                  SPD_DEL(J4)%DELS(J3,SPD__WAT,1,1)
            END IF
!
            IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_pres' ) THEN
                 READ ( UNIT=SAT(J3)%PLINE(ISTA)%PRES, FMT='(F8.1)'             ) SPD_DEL(J4)%SUR_PRS(J3)
            END IF
            IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_pwp' ) THEN
                 READ ( UNIT=SAT(J3)%PLINE(ISTA)%WATER_VAPOR_PRES, FMT='(F8.1)' ) SPD_DEL(J4)%SUR_PWP(J3)
            END IF
            IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_temp' ) THEN
                 READ ( UNIT=SAT(J3)%PLINE(ISTA)%TEMP, FMT='(F5.1)'             ) SPD_DEL(J4)%SUR_TEM(J3)
            END IF
            SPD_DEL(J4)%TIM_ARR(J3) = (MJD_OBS_FIL - SPD_DEL(J4)%MJD_OBS_FIRST)*86400.0D0 + &
     &                                (TIM_OBS_FIL - SPD_DEL(J4)%TAI_OBS_FIRST)
 440     CONTINUE 
!
         IF ( ASSOCIATED ( SAT(J3)%MLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%MLINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%ILINE ) ) THEN
              DEALLOCATE ( SAT(J3)%ILINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%SLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%SLINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%ELINE ) ) THEN
              DEALLOCATE ( SAT(J3)%ELINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%ALINE ) ) THEN
              DEALLOCATE ( SAT(J3)%ALINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%PLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%PLINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%DLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%DLINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%FLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%FLINE )
         END IF
         IF ( ASSOCIATED ( SAT(J3)%OLINE ) ) THEN
              DEALLOCATE ( SAT(J3)%OLINE )
         END IF
 430  CONTINUE 
!
      DO 4180 J18=1,N_STA
         IF ( MODE_STR == 'azel' ) THEN
              IF ( SAT(1)%O_MODE == SPD__NAZ ) THEN
!
! ---------------- Expand atmosphere optical depth and atmosphere brightness 
! ---------------- temperature into 4D B-spline basis. Dimensions of the 
! ---------------- expansion: frequency, mapping function, azimuth angle, 
! ---------------- and time
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL BSPL4_4D_CMP ( SPD__MDEG, 0, DIMS, &
     &                                 SPD_DEL(J18)%FRQ_ARR,  SPD_DEL(J18)%MAP_ARR, &
     &                                 SPD_DEL(J18)%AZM%AZIM, SPD_DEL(J18)%TIM_ARR, &
     &                                 SPD_DEL(J18)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG), &
     &                                 IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4559, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                      'attempt to expand opacity into 4D B-spline basis' )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL BSPL4_4D_CMP ( SPD__MDEG, 0, DIMS, &
     &                                 SPD_DEL(J18)%FRQ_ARR,  SPD_DEL(J18)%MAP_ARR, &
     &                                 SPD_DEL(J18)%AZM%AZIM, SPD_DEL(J18)%TIM_ARR, &
     &                                 SPD_DEL(J18)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG), &
     &                                 IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4560, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                      'attempt to expand opacity into 4D B-spline basis' )
                        RETURN 
                   END IF
                 ELSE IF ( SAT(1)%O_MODE == SPD__1AZ ) THEN
!
! ---------------- Expand atmosphere optical depth and atmosphere brightness temperature
! ---------------- into 3D B-spline basus. Dimensions of the expansion:
! ---------------- mapping function, azimuth angle, and time
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMS, SPD_DEL(J18)%FRQ_ARR, &
     &                                 SPD_DEL(J18)%MAP_ARR, SPD_DEL(J18)%TIM_ARR, &
     &                                 SPD_DEL(J18)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1), &
     &                                 IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4561, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                      'attempt to expand atmospere radiative temperature '// &
     &                      'into the 3D B-spline basis' )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMS, SPD_DEL(J18)%FRQ_ARR, &
     &                                 SPD_DEL(J18)%MAP_ARR, &
     &                                 SPD_DEL(J18)%TIM_ARR, &
     &                                 SPD_DEL(J18)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1), &
     &                                 IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4562, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                      'attempt to expand atmospere radiative temperature '// &
     &                      'into the 3D B-spline basis' )
                        RETURN 
                   END IF
              END IF
!
              DO 4190 J19=1,2
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMSD, SPD_DEL(J18)%MAP_ARR, &
     &                               SPD_DEL(J18)%AZM%AZIM, &
     &                               SPD_DEL(J18)%TIM_ARR, &
     &                               SPD_DEL(J18)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J19), &
     &                               IER )
                 IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 4563, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                       'attempt to expand slant path delay '// &
     &                       'into the 3D B-spline basis' )
                         RETURN 
                 END IF
 4190         CONTINUE 
           ELSE IF ( MODE_STR == 'zen_opa' ) THEN
!
! ----------- Compute the B-spline expansion for the zenith opacity
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_2D_CMP ( SPD__MDEG, 0, DIMS(1), DIMS(3), &
     &                            SPD_DEL(J18)%FRQ_ARR,  &
     &                            SPD_DEL(J18)%TIM_ARR, &
     &                            SPD_DEL(J18)%OPA(1-SPD__MDEG,1-SPD__MDEG,1,1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4564, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &                 'an attempt to compute B-spline expansion for the '// &
     &                 'zenith opacity at station '//C_STA(J18) )
                   RETURN 
              END IF
           ELSE IF ( MODE_STR == 'zen_tatm' ) THEN
!
! ----------- Compute the B-spline expansion for the zenith opacity
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_2D_CMP ( SPD__MDEG, 0, DIMS(1), DIMS(3), &
     &                            SPD_DEL(J18)%FRQ_ARR,  &
     &                            SPD_DEL(J18)%TIM_ARR, &
     &                            SPD_DEL(J18)%TAT(1-SPD__MDEG,1-SPD__MDEG,1,1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4565, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &                 'an attempt to compute B-spline expansion for the '// &
     &                 'zenith opacity at station '//C_STA(J18) )
                   RETURN 
              END IF
           ELSE IF ( MODE_STR == 'zen_del'  .OR. &
     &               MODE_STR == 'zen_deld' .OR. &
     &               MODE_STR == 'zen_delt' .OR. &
     &               MODE_STR == 'zen_delw'      ) THEN
!
              DO 4200 J20=1,2
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD_DEL(J18)%N_TIM, &
     &                               SPD_DEL(J18)%TIM_ARR, &
     &                               SPD_DEL(J18)%DELS(1-SPD__MDEG,J20,1,1), &
     &                               IER )
                 IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 4566, IUER, 'SPD_RES_INTRP', 'Failure in an '// &
     &                       'attempt to expand slant path delay '// &
     &                       'into the 3D B-spline basis' )
                         RETURN 
                 END IF
 4200         CONTINUE 
         END IF
         IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_pres' ) THEN
!
! ----------- Compute the B-spline expansion for surface pressure
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD_DEL(J18)%N_TIM, &
     &                            SPD_DEL(J18)%TIM_ARR, &
     &                            SPD_DEL(J18)%SUR_PRS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4567, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &                 'an attempt to compute B-spline expansion for the '// &
     &                 'surface atmosperic pressire at station '// &
     &                  C_STA(J18) )
                   RETURN 
              END IF
         END IF
         IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_pwp' ) THEN
!
! ----------- Compute the B-spline expansion for surface pressure
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD_DEL(J18)%N_TIM, &
     &                            SPD_DEL(J18)%TIM_ARR, &
     &                            SPD_DEL(J18)%SUR_PWP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4568, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &                 'an attempt to compute B-spline expansion for the '// &
     &                 'surface atmosperic pressire at station '// &
     &                  C_STA(J18) )
                   RETURN 
              END IF
         END IF
!
! ------ Compute the B-spline expansion for surface temperature
!
         IF ( MODE_STR == 'azel' .OR. MODE_STR == 'zen_temp' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD_DEL(J18)%N_TIM, &
     &                            SPD_DEL(J18)%TIM_ARR, &
     &                            SPD_DEL(J18)%SUR_TEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4569, IUER, 'SPD_RES_INTRP', 'Error in '// &
     &                 'an attempt to compute B-spline expansion for the '// &
     &                 'surface air temperature at station '//C_STA(J18) )
                   RETURN 
              END IF
         END IF
         SPD_DEL(J18)%STATUS = SPD__INTR
 4180 CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_RES_INTRP  !#!#
