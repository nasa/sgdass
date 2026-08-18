      SUBROUTINE GET_OPA_TATM_TIPPING ( PAR_STR, SPD_DEL, MJD, TAI, FREQ, AZ, &
     &                                  N_EL, EL_ARR, VAL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_OPA_TATM_TIPPING
! *                                                                      *
! * ## 08-JAN-2024 GET_OPA_TATM_TIPPING v1.0 (c) L. Petrov 08-JAN-2024 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PAR_STR*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  MJD, N_EL, IUER
      REAL*8     TAI, FREQ, AZ, EL_ARR(N_EL), VAL_ARR(N_EL)
      REAL*8     TIM, MAP
      REAL*4     ARGS(4)
      CHARACTER  STR*128, STR1*128, STR2*128, STR3*128
      INTEGER*4  DIMS(4), INDS(4), J1, J2, IER
      INTEGER*4, EXTERNAL    :: IXMN4
      REAL*8,    EXTERNAL    :: DEL_ISA
      REAL*4,    EXTERNAL    :: VAL_3D_BSPL4, VAL_4D_BSPL4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
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
           CALL ERR_LOG ( 4671, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
     &         'control: wrong value of SPD_DEL%MODE_OPA_TAT: '//STR )
           RETURN 
      END IF
!
      INDS(1) = IXMN4 ( SPD_DEL%N_FRQ, SPD_DEL%FRQ_ARR, SNGL(FREQ) )
      IF ( INDS(1) < 1 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR(1:13),  FMT='(1PD13.6)' ) FREQ
           WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(1)
           WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(SPD_DEL%N_FRQ)
           CALL INCH ( J1, STR3 )
           CALL ERR_LOG ( 4672, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
     &         'control: the frequency '//TRIM(STR)//' is '// &
     &         'of of range ['//TRIM(STR2)//', '//TRIM(STR3)//'] Hz'  )
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
                CALL ERR_LOG ( 4673, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
          &         'control: azimuth '//TRIM(STR)//' is out of range ['// &
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
     &                                  SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                CALL ERR_LOG ( 4674, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
          &         'control: time '//TRIM(STR)//' is out of range ['// &
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
     &                                  SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER  )
                CALL ERR_LOG ( 4675, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
          &         'control: time '//TRIM(STR)//' is out of range ['// &
                     TRIM(STR1)//', '//TRIM(STR2)//']' )
                RETURN 
           END IF
      END IF
!
      DO 410 J1=1,N_EL
         MAP = DEL_ISA ( EL_ARR(J1) )/ DEL_ISA ( P2I )
         INDS(2) = IXMN4 ( INT(SPD_DEL%ELV%N_EL,KIND=4), SPD_DEL%MAP_ARR, SNGL(MAP) )
         IF ( INDS(2) < 1 ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL CLRCH ( STR3 )
              CALL INCH  ( J1, STR )
              WRITE ( UNIT=STR1(1:13),  FMT='(1PD13.6)' ) EL_ARR(J1)/DEG__TO__RAD
              WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(1)/DEG__TO__RAD
              WRITE ( UNIT=STR3(1:13), FMT='(1PD13.6)' ) SPD_DEL%ELV%ELEV(SPD_DEL%ELV%N_EL)/DEG__TO__RAD
              CALL ERR_LOG ( 4676, IUER, 'GET_OPA_TATM_TIPPING', 'Trap of internal '// &
     &            'control: the '//TRIM(STR)//'-th elevation '//TRIM(STR1)//' is out '// &
                  'of range ['//TRIM(STR2)//', '//TRIM(STR3)//'] deg' )
              RETURN 
         END IF 
!
         IF ( SPD_DEL%MODE_OPA_TAT == SPD__NAZ ) THEN
              ARGS(1) = FREQ
              ARGS(2) = MAP
              ARGS(3) = AZ
              ARGS(4) = TIM
              IF ( PAR_STR == 'opa' ) THEN
                   VAL_ARR(J1) = VAL_4D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                           SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, &
     &                           SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                           SPD_DEL%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG) )
                ELSE IF ( PAR_STR == 'tatm' ) THEN
                   VAL_ARR(J1) = VAL_4D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                           SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, &
     &                           SPD_DEL%AZM%AZIM, SPD_DEL%TIM_ARR, &
     &                           SPD_DEL%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG) )
              END IF
           ELSE IF ( SPD_DEL%MODE_OPA_TAT == SPD__1AZ ) THEN
              ARGS(1) = FREQ
              ARGS(2) = MAP
              ARGS(3) = TIM
              IF ( PAR_STR == 'opa' ) THEN
                   VAL_ARR(J1) = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                           SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, SPD_DEL%TIM_ARR, &
     &                           SPD_DEL%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) )
                 ELSE IF ( PAR_STR == 'tatm' ) THEN
                   VAL_ARR(J1) = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                           SPD_DEL%FRQ_ARR,  SPD_DEL%MAP_ARR, SPD_DEL%TIM_ARR, &
     &                           SPD_DEL%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1) )
              END IF
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_OPA_TATM_TIPPING  !#!#
