      FUNCTION GET_SPD_RES ( PAR_STR, SPD_DEL, MJD, TAI, FREQ, AZ, EL, IUER )
! ************************************************************************
! *                                                                      *
! *   Function GET_SPD_RES
! *                                                                      *
! * ### 08-JAN-2024    GET_SPD_RES    v2.0 (c) L. Petrov 27-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PAR_STR*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      REAL*8     GET_SPD_RES
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  MJD, IUER
      REAL*8     TAI, FREQ, EL, AZ, TIM, MAP
      REAL*4     ARGS(4)
      INTEGER*4  SHA(4)
      CHARACTER  MODE_STR*32, STR*128, STR1*128, STR2*128, STR3*128
      INTEGER*4  DIMS(4), INDS(4), J1, J2, IER
      INTEGER*4, EXTERNAL :: IXMN4
      REAL*8,    EXTERNAL :: DEL_ISA
      REAL*4,    EXTERNAL :: VAL_1D_BSPL4, VAL_2D_BSPL4, VAL_3D_BSPL4, VAL_4D_BSPL4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( SPD_DEL%STATUS .NE. SPD__INTR ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( SPD_DEL%STATUS, STR )
           CALL ERR_LOG ( 4771, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &         'control: SPD_DEL has status '//TRIM(STR)//' while SPD__INTR '// &
     &         'was expected'  )
           RETURN 
      END IF
!
      TIM = (MJD - SPD_DEL%MJD_OBS_FIRST)*86400.0D0 + (TAI - SPD_DEL%TAI_OBS_FIRST)
      IF ( SPD_DEL%MODE_OPA_TAT == SPD__NAZ ) THEN
           DIMS(1) = SPD_DEL%N_FRQ
           DIMS(2) = SPD_DEL%ELV%N_EL
           DIMS(3) = SPD_DEL%AZM%N_AZ
           DIMS(4) = SPD_DEL%N_TIM
           SPD_DEL%MODE_OPA_TAT = SPD__NAZ
        ELSE IF ( SPD_DEL%MODE_OPA_TAT == SPD__1AZ ) THEN
           DIMS(1) = SPD_DEL%N_FRQ
           DIMS(2) = SPD_DEL%ELV%N_EL
           DIMS(3) = SPD_DEL%N_TIM
           DIMS(4) = 1
           SPD_DEL%MODE_OPA_TAT = SPD__1AZ
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( SPD_DEL%MODE_OPA_TAT, STR )
           CALL ERR_LOG ( 4772, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &         'control: wrong value of SPD_DEL%MODE_OPA_TAT: '//STR )
           RETURN 
      END IF
      IF ( PAR_STR == 'opa' ) THEN
           SHA = SHAPE( SPD_DEL%OPA )
           IF ( SHA(3) == 1 ) THEN
                MODE_STR = 'zen_opa'
              ELSE 
                MODE_STR = 'azel'
           END IF
        ELSE IF ( PAR_STR == 'tatm' ) THEN
           SHA = SHAPE( SPD_DEL%TAT )
           IF ( SHA(3) == 1 ) THEN
                MODE_STR = 'zen_opa'
              ELSE 
                MODE_STR = 'azel'
           END IF
         ELSE IF ( PAR_STR(1:3) == 'del' ) THEN
           SHA = SHAPE( SPD_DEL%DELS )
           IF ( SHA(3) == 1 ) THEN
                MODE_STR = 'zen_'//PAR_STR
              ELSE 
                MODE_STR = 'azel'
           END IF
      END IF
!
      IF ( PAR_STR == 'opa' .OR. &
     &     PAR_STR == 'tatm'     ) THEN
           INDS(1) = IXMN4 ( SPD_DEL%N_FRQ, SPD_DEL%FRQ_ARR, SNGL(FREQ) )
           IF ( INDS(1) < 1 ) THEN
                CALL CLRCH ( STR  )
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                WRITE ( UNIT=STR(1:13),  FMT='(1PD13.6)' ) FREQ
                WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(1)
                WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(SPD_DEL%N_FRQ)
                CALL INCH ( J1, STR3 )
                CALL ERR_LOG ( 4773, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &              'control: the frequency '//TRIM(STR)//' is '// &
     &              'of of range ['//TRIM(STR2)//', '//TRIM(STR3)//'] Hz'  )
                RETURN 
           END IF
!
           IF ( MODE_STR == 'azel' ) THEN
                MAP = DEL_ISA ( EL )/ DEL_ISA ( P2I )
                INDS(2) = IXMN4 ( INT(SPD_DEL%ELV%N_EL,KIND=4), SPD_DEL%MAP_ARR, SNGL(MAP) )
                IF ( INDS(2) < 1 ) THEN
                     CALL CLRCH ( STR )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     WRITE ( UNIT=STR(1:13), FMT='(1PD13.6)' ) EL/DEG__TO__RAD
                     WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(1)/DEG__TO__RAD
                     WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(SPD_DEL%ELV%N_EL)/DEG__TO__RAD
                     CALL ERR_LOG ( 4774, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: elevation '//TRIM(STR)//' is out '// &
                         'of range ['//TRIM(STR1)//', '//TRIM(STR2)//'] deg' )
                     RETURN 
                END IF 
!
                IF ( SPD_DEL%MODE_OPA_TAT == SPD__NAZ ) THEN
                     INDS(3) = IXMN4 ( INT(SPD_DEL%AZM%N_AZ,KIND=4), SPD_DEL%AZM%AZIM, SNGL(AZ) )
                     IF ( INDS(3) < 1 ) THEN
                          CALL CLRCH ( STR  )
                          CALL CLRCH ( STR1 )
                          CALL CLRCH ( STR2 )
                          WRITE ( UNIT=STR(1:13),  FMT='(1PD13.6)' ) AZ/DEG__TO__RAD
                          WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%AZM%AZIM(1)/DEG__TO__RAD
                          WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%AZM%AZIM(SPD_DEL%AZM%N_AZ)/DEG__TO__RAD
                          CALL ERR_LOG ( 4775, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                        'control: azimuth '//TRIM(STR)//' is out of range ['// &
                               TRIM(STR1)//', '//TRIM(STR2)//'] deg' )
                          RETURN 
                     END IF 
!
                     INDS(4) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
                     IF ( INDS(4) < 1 ) THEN
                          CALL CLRCH ( STR  )
                          CALL CLRCH ( STR1 )
                          CALL CLRCH ( STR2 )
                          STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                          STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                          STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                            SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                          CALL ERR_LOG ( 4776, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                        'control: time '//TRIM(STR)//' is out of range ['// &
                               TRIM(STR1)//', '//TRIM(STR2)//']' )
                          RETURN 
                     END IF 
                  ELSE IF ( SPD_DEL%MODE_OPA_TAT == SPD__1AZ ) THEN
                     INDS(3) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
                     IF ( INDS(3) < 1 ) THEN
                          CALL CLRCH ( STR  )
                          CALL CLRCH ( STR1 )
                          CALL CLRCH ( STR2 )
                          STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                          STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                          STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                            SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                          CALL ERR_LOG ( 4777, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                        'control: time '//TRIM(STR)//' is out of range ['// &
                               TRIM(STR1)//', '//TRIM(STR2)//']' )
                          RETURN 
                     END IF
                END IF
              ELSE
                DIMS(1) = SPD_DEL%N_FRQ
                DIMS(2) = SPD_DEL%N_TIM
!
! ------------- Zenith opacity or Tatm
!
                INDS(2) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
                IF ( INDS(2) < 1 ) THEN
                     CALL CLRCH ( STR  )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                     STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                     STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                       SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                     CALL ERR_LOG ( 4778, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: time '//TRIM(STR)//' is out of range ['// &
                          TRIM(STR1)//', '//TRIM(STR2)//']' )
                     RETURN 
                END IF
           END IF
         ELSE IF ( PAR_STR(1:3) == 'del' ) THEN
           IF ( MODE_STR == 'azel' ) THEN
                MAP = DEL_ISA ( EL )/ DEL_ISA ( P2I )
                INDS(1) = IXMN4 ( INT(SPD_DEL%ELV%N_EL,KIND=4), SPD_DEL%MAP_ARR, SNGL(MAP) )
                IF ( INDS(1) < 1 ) THEN
                     CALL CLRCH ( STR )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     WRITE ( UNIT=STR(1:13), FMT='(1PD13.6)' ) EL/DEG__TO__RAD
                     WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(1)/DEG__TO__RAD
                     WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(SPD_DEL%ELV%N_EL)/DEG__TO__RAD
                     CALL ERR_LOG ( 4779, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: elevation '//TRIM(STR)//' is out '// &
                         'of range ['//TRIM(STR1)//', '//TRIM(STR2)//'] deg' )
                     RETURN 
                END IF 
                INDS(2) = IXMN4 ( INT(SPD_DEL%AZM%N_AZ,KIND=4), SPD_DEL%AZM%AZIM, SNGL(AZ) )
                IF ( INDS(2) < 1 ) THEN
                     CALL CLRCH ( STR  )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     WRITE ( UNIT=STR(1:13),  FMT='(1PD13.6)' ) AZ/DEG__TO__RAD
                     WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%AZM%AZIM(1)/DEG__TO__RAD
                     WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%AZM%AZIM(SPD_DEL%AZM%N_AZ)/DEG__TO__RAD
                     CALL ERR_LOG ( 4780, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: azimuth '//TRIM(STR)//' is out of range ['// &
                          TRIM(STR1)//', '//TRIM(STR2)//'] deg' )
                     RETURN 
                END IF 
!
                INDS(3) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
                IF ( INDS(3) < 1 ) THEN
                     CALL CLRCH ( STR  )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                     STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                     STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                       SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                     CALL ERR_LOG ( 4781, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: time '//TRIM(STR)//' is out of range ['// &
                          TRIM(STR1)//', '//TRIM(STR2)//']' )
                     RETURN 
                END IF 
              ELSE
                INDS(1) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
                IF ( INDS(1) < 1 ) THEN
                     CALL CLRCH ( STR  )
                     CALL CLRCH ( STR1 )
                     CALL CLRCH ( STR2 )
                     STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                     STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                     STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                       SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                     CALL ERR_LOG ( 4782, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &                   'control: time '//TRIM(STR)//' is out of range ['// &
                          TRIM(STR1)//', '//TRIM(STR2)//']' )
                     RETURN 
                END IF 
           END IF 
         ELSE IF ( PAR_STR(1:4) == 'pres' .OR. &
     &             PAR_STR(1:3) == 'pwp'  .OR. &
     &             PAR_STR(1:4) == 'temp'      ) THEN
           DIMS(1) = SPD_DEL%N_TIM
           INDS(1) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
           IF ( INDS(1) < 1 ) THEN
                CALL CLRCH ( STR  )
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                STR  = MJDSEC_TO_DATE ( MJD, TAI, IER  )
                STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST, IER  )
                STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST, SPD_DEL%TAI_OBS_FIRST + &
     &                                  SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                CALL ERR_LOG ( 4783, IUER, 'GET_SPD_RES', 'Trap of internal '// &
     &              'control: time '//TRIM(STR)//' is out of range ['// &
                     TRIM(STR1)//', '//TRIM(STR2)//']' )
                RETURN 
           END IF 
      END IF
!
      GET_SPD_RES = 0.0D0
      IF ( SPD_DEL%MODE_OPA_TAT == SPD__NAZ ) THEN
           ARGS(1) = FREQ
           ARGS(2) = MAP
           ARGS(3) = AZ
           ARGS(4) = TIM
           IF ( PAR_STR == 'opa' ) THEN
                GET_SPD_RES = VAL_4D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                            SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, &
     &                            SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                            SPD_DEL%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG) )
             ELSE IF ( PAR_STR == 'tatm' ) THEN
                GET_SPD_RES = VAL_4D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                            SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, &
     &                            SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                            SPD_DEL%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG) )
           END IF
        ELSE IF ( SPD_DEL%MODE_OPA_TAT == SPD__1AZ ) THEN
           ARGS(1) = FREQ
           ARGS(2) = MAP
           ARGS(3) = TIM
           IF ( MODE_STR == 'azel' ) THEN
                IF ( PAR_STR == 'opa' ) THEN
                     GET_SPD_RES = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                 SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, SPD_DEL%TIM_ARR, &
     &                                 SPD_DEL%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) )
                   ELSE IF ( PAR_STR == 'tatm' ) THEN
                     GET_SPD_RES = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                 SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, SPD_DEL%TIM_ARR, &
     &                                 SPD_DEL%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) )
                END IF
              ELSE
                ARGS(1) = FREQ
                ARGS(2) = TIM
                IF ( PAR_STR == 'opa' ) THEN
                     GET_SPD_RES = VAL_2D_BSPL4 ( ARGS(1), ARGS(2), DIMS(1), DIMS(2), SPD__MDEG, &
     &                                            INDS(1), INDS(2), &
     &                                            SPD_DEL%FRQ_ARR,  SPD_DEL%TIM_ARR, &
     &                                            SPD_DEL%OPA(1-SPD__MDEG,1-SPD__MDEG,1,1) )
                   ELSE IF ( PAR_STR == 'tatm' ) THEN
                     GET_SPD_RES = VAL_2D_BSPL4 ( ARGS(1), ARGS(2), DIMS(1), DIMS(2), SPD__MDEG, &
     &                                            INDS(1), INDS(2), &
     &                                            SPD_DEL%FRQ_ARR,  SPD_DEL%TIM_ARR, &
     &                                            SPD_DEL%TAT(1-SPD__MDEG,1-SPD__MDEG,1,1) )
                END IF
           END IF
      END IF
      IF ( PAR_STR == 'del' .OR. PAR_STR == 'deld' ) THEN
           IF ( MODE_STR == 'azel' ) THEN
                MAP = DEL_ISA ( EL )/ DEL_ISA ( P2I )
                DIMS(1) = SPD_DEL%ELV%N_EL
                DIMS(2) = SPD_DEL%AZM%N_AZ
                DIMS(3) = SPD_DEL%N_TIM
                ARGS(1) = MAP
                ARGS(2) = AZ
                ARGS(3) = TIM
                GET_SPD_RES = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                       SPD_DEL%MAP_ARR, SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                                       SPD_DEL%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) )
              ELSE 
                DIMS(1) = SPD_DEL%N_TIM
                ARGS(1) = TIM
                GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                       SPD_DEL%TIM_ARR, SPD_DEL%DELS(1-SPD__MDEG,1,1,1) )
           END IF
        ELSE IF ( PAR_STR == 'delw' ) THEN
           IF ( MODE_STR == 'azel' ) THEN
                MAP = DEL_ISA ( EL )/ DEL_ISA ( P2I )
                DIMS(1) = SPD_DEL%ELV%N_EL
                DIMS(2) = SPD_DEL%AZM%N_AZ
                DIMS(3) = SPD_DEL%N_TIM
                ARGS(1) = MAP
                ARGS(2) = AZ
                ARGS(3) = TIM
                GET_SPD_RES = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                       SPD_DEL%MAP_ARR, SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                                       SPD_DEL%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,2) )
              ELSE 
                DIMS(1) = SPD_DEL%N_TIM
                ARGS(1) = TIM
                GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                       SPD_DEL%TIM_ARR, SPD_DEL%DELS(1-SPD__MDEG,2,1,1) )
           END IF
        ELSE IF ( PAR_STR == 'delt' ) THEN
           IF ( MODE_STR == 'azel' ) THEN
                MAP = DEL_ISA ( EL )/ DEL_ISA ( P2I )
                DIMS(1) = SPD_DEL%ELV%N_EL
                DIMS(2) = SPD_DEL%AZM%N_AZ
                DIMS(3) = SPD_DEL%N_TIM
                ARGS(1) = MAP
                ARGS(2) = AZ
                ARGS(3) = TIM
                GET_SPD_RES = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                       SPD_DEL%MAP_ARR, SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                                       SPD_DEL%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) ) - &
     &                        VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                       SPD_DEL%MAP_ARR, SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                                       SPD_DEL%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,2) )
              ELSE 
                DIMS(1) = SPD_DEL%N_TIM
                ARGS(1) = TIM
                GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                       SPD_DEL%TIM_ARR, SPD_DEL%DELS(1-SPD__MDEG,1,1,1) ) + &
     &                        VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                       SPD_DEL%TIM_ARR, SPD_DEL%DELS(1-SPD__MDEG,2,1,1) )
           END IF
        ELSE IF ( PAR_STR(1:4) == 'pres' ) THEN
           ARGS(1) = TIM
           GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                  SPD_DEL%TIM_ARR, SPD_DEL%SUR_PRS )
        ELSE IF ( PAR_STR(1:3) == 'pwp' ) THEN
           ARGS(1) = TIM
           GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                  SPD_DEL%TIM_ARR, SPD_DEL%SUR_PWP )
        ELSE IF ( PAR_STR(1:4) == 'temp' ) THEN
           ARGS(1) = TIM
           GET_SPD_RES = VAL_1D_BSPL4 ( ARGS(1), DIMS(1), SPD__MDEG, INDS(1), &
     &                                  SPD_DEL%TIM_ARR, SPD_DEL%SUR_TEM )
        ELSE IF ( PAR_STR(1:3) == 'opa' .OR. PAR_STR(1:4) == 'tatm' ) THEN
           CONTINUE 
        ELSE 
           CALL ERR_LOG ( 4784, IUER, 'GET_SPD_RES', 'Unspupported parameter '// &
     &         'PAR_STR '//TRIM(PAR_STR)//' while opa, tatm, del, delt, delw, '// &
     &         ' deld, pres, pwp, or temp were expected' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  GET_SPD_RES  !#!#
