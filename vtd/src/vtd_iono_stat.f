      SUBROUTINE VTD_IONO_STAT ( VTD, OBS_TYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_IONO_STAT computes the average and covariance of       *
! *   the zenith ionosphere pat hdelays for this experiment.             *
! *                                                                      *
! *  ### 10-OCT-2010 VTD_IONO_STAT v1.1 (c)  L. Petrov  14-JUN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER
      CHARACTER  STR*128, SOLVE_DEBUG*32
      INTEGER*4  M__SAM
      PARAMETER  ( M__SAM = 128 )
      REAL*4,    ALLOCATABLE :: ION_ZDL(:,:)
      REAL*4     TIM_BEG, TIM_MID, TIM_STEP, TIM_EPC, ARGS(3), TEC_VAL, &
     &           EFF_FRQ_SQ
      INTEGER*4  DIMS(3), INDS(3), J1, J2, J3, J4, J5, J6, J7, IER
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( VTD%IONO%STATUS_SPL .NE. VIO__COMP ) THEN
           CALL ERR_LOG ( 2281, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &         'internal control: coefficients of TEC maps interpolating '// &
     &         'spline have not been computed' )
           RETURN 
      END IF
      CALL GETENVAR ( 'SOLVE_DEBUG', SOLVE_DEBUG )
!
! --- Get the square of the effective frequnecy
!
      IF (        OBS_TYP%DELAY_TYPE == VTD__PL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 2282, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency' )
                RETURN 
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 2283, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency' )
                RETURN 
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 2284, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency' )
                RETURN 
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                CALL ERR_LOG ( 2285, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency' )
                RETURN 
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__ML__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:15), FMT='(1PD15.8)' ) OBS_TYP%FRQ_ION_EFF(1) 
                CALL ERR_LOG ( 2286, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency: '//STR )
                RETURN 
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
                CALL ERR_LOG ( 2287, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &              'internal control: bad ionosphere frequency' )
                RETURN 
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLPH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SLSH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MLMH__DTP ) THEN
           IF ( SOLVE_DEBUG == 'GET_IONO' ) THEN
                IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &               OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
                     WRITE ( 6, * ) ' OBS_TYP%FRQ_ION_EFF(1) = ', OBS_TYP%FRQ_ION_EFF(1) 
                     WRITE ( 6, * ) ' OBS_TYP%FRQ_ION_EFF(2) = ', OBS_TYP%FRQ_ION_EFF(2) 
                     CALL ERR_LOG ( 2288, IUER, 'VTD_IONO_STAT', 'Trap of '// &
     &                   'internal control: bad ionosphere frequency' )
                     RETURN 
                END IF
                EFF_FRQ_SQ = OBS_TYP%FRQ_ION_EFF(2)**2
              ELSE 
                EFF_FRQ_SQ =  0.0D0
           END IF
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
      END IF
!
      IF ( ASSOCIATED ( VTD%IONO%AVR_STA ) ) DEALLOCATE ( VTD%IONO%AVR_STA )
!
      ALLOCATE ( VTD%IONO%AVR_STA(VTD%L_STA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*VTD%L_STA, STR )
           CALL ERR_LOG ( 2289, IUER, 'VTD_IONO_STAT', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array VTD%IONO%AVR_STA' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( VTD%IONO%COV_STA ) ) DEALLOCATE ( VTD%IONO%COV_STA )
      ALLOCATE ( VTD%IONO%COV_STA(VTD%L_STA,VTD%L_STA), STAT=IER  )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*VTD%L_STA*VTD%L_STA, STR )
           CALL ERR_LOG ( 2290, IUER, 'VTD_IONO_STAT', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array VTD%IONO%COV_STA' )
           RETURN 
      END IF
!
      ALLOCATE ( ION_ZDL(M__SAM,VTD%L_STA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
          CALL CLRCH ( STR )
          CALL INCH  ( 4*VTD%L_STA*VTD%L_STA, STR )
          CALL ERR_LOG ( 2291, IUER, 'VTD_IONO_STAT', 'Failure '// &
     &        'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &        'for array TEC_STA' )
          RETURN 
      END IF
!
      DIMS(1) = VTD%IONO%HEADER%NLON
      DIMS(2) = VTD%IONO%HEADER%NLAT
      DIMS(3) = VTD%IONO%HEADER%NEPC
!
      TIM_MID = VTD%TAI_BEG + &
     &           ((VTD%MJD_END - VTD%MJD_BEG)*86400.0D0 + &
     &            (VTD%TAI_END - VTD%TAI_BEG))/2.0D0
      TIM_BEG  = TIM_MID - 43000.0D0
      TIM_STEP = 86400.0D0/(M__SAM-1)
      DO 410 J1=1,M__SAM
         TIM_EPC = TIM_BEG + (J1-1)*TIM_STEP
         ARGS(3) = ( (VTD%MJD_BEG - VTD%IONO%HEADER%MJD_BEG)*86400.0D0 + &
     &               (TIM_EPC - VTD%IONO%HEADER%UTC_BEG) )/ &
     &               VTD%IONO%HEADER%TIM_STEP 
         INDS(3) = INT(ARGS(3)) + 1
!
! ------ Correct possible overshot/undershot due to rounding
!
         IF ( INDS(3) .LE. 0 ) INDS(3) = 1
         IF ( INDS(3) .GE. VTD%IONO%HEADER%NEPC-1 ) INDS(3) = VTD%IONO%HEADER%NEPC - 1
         DO 420 J2=1,VTD%L_STA
            ARGS(1) = VTD%STA(J2)%LONG
            IF ( ARGS(1) > PI__NUM ) ARGS(1) = ARGS(1) - PI2
            INDS(1) = INT ( (ARGS(1) - VTD%IONO%HEADER%LON_MIN)/ &
     &                       VTD%IONO%HEADER%LON_STEP ) + 1
! 
! --------- Correct possible overshot/undershot due to rounding
!
            IF ( INDS(1) .LE. 0 ) INDS(1) = 1
            IF ( INDS(1) .GE. VTD%IONO%HEADER%NLON-1 ) INDS(1) = VTD%IONO%HEADER%NLON - 1
!
            ARGS(2) = VTD%STA(J2)%LAT_GCN 
            INDS(2) = INT ( (ARGS(2) - VTD%IONO%HEADER%LAT_MIN)/VTD%IONO%HEADER%LAT_STEP ) + 1
!
! --------- Correct possible overshot/undershot due to rounding
!
            IF ( INDS(2) .LE. 0 ) INDS(2) = 1
            IF ( INDS(2) .GE. VTD%IONO%HEADER%NLAT-1 ) INDS(2) = VTD%IONO%HEADER%NLAT - 1
!
! --------- Compute the value of the total electron contents in the ionosphere
! --------- in the eznith of station J2
!
            TEC_VAL = VAL_3D_BSPL4 ( ARGS, VIO__M_DEG, DIMS, INDS, &
     &                               VTD%IONO%LON_VAL, VTD%IONO%LAT_VAL, &
     &                               VTD%IONO%TIM_VAL, VTD%IONO%TEC_SPL )
            IF ( TEC_VAL < -VTD__TEC_MAX  .OR.  TEC_VAL > VTD__TEC_MAX ) THEN
                 WRITE ( 6, * ) ' TEC_VAL = ', TEC_VAL
                 WRITE ( 6, * ) ' VTD__TEC_MIN = ', VTD__TEC_MIN
                 WRITE ( 6, * ) ' VTD__TEC_MAX = ', VTD__TEC_MAX
                 CALL ERR_LOG ( 2285, IUER, 'VTD_IONO_STAT', 'Bad TEC '// &
     &              'value for station '//VTD%STA(J2)%IVS_NAME )
                 RETURN
            END IF
            IF ( TEC_VAL < VTD__TEC_MIN ) TEC_VAL = VTD__TEC_MIN 
            IF ( EFF_FRQ_SQ > 0.0D0 ) THEN
                 ION_ZDL(J1,J2) = TEC_VAL*ALPHA__VIO/(PI2**2)/EFF_FRQ_SQ
               ELSE
                 ION_ZDL(J1,J2) = 0.0D0
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,VTD%L_STA
         VTD%IONO%AVR_STA(J3) = 0.0
         DO 440 J4=1,M__SAM
            VTD%IONO%AVR_STA(J3) = VTD%IONO%AVR_STA(J3) + ION_ZDL(J4,J3)/M__SAM
 440     CONTINUE 
 430  CONTINUE 
!
      DO 450 J5=1,VTD%L_STA
         DO 460 J6=J5,VTD%L_STA
            VTD%IONO%COV_STA(J5,J6) = 0.0D0
            DO 470 J7=1,M__SAM
               VTD%IONO%COV_STA(J5,J6) = VTD%IONO%COV_STA(J5,J6) + &
     &             (ION_ZDL(J7,J5) - VTD%IONO%AVR_STA(J5))* &
     &             (ION_ZDL(J7,J6) - VTD%IONO%AVR_STA(J6))
 470        CONTINUE 
            VTD%IONO%COV_STA(J5,J6) = VTD%IONO%COV_STA(J5,J6)/M__SAM
            VTD%IONO%COV_STA(J6,J5) = VTD%IONO%COV_STA(J5,J6) 
 460     CONTINUE 
 450  CONTINUE 
!
      DEALLOCATE ( ION_ZDL )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  VTD_IONO_STAT  !#!#
