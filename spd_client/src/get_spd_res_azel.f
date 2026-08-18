      SUBROUTINE GET_SPD_RES_AZEL ( SPD_DEL, SPD_RES, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine GET_SPD_RES_AZEL
! *                                                                      *
! * ### 11-JAN-2024 GET_SPD_RES_AZEL  v1.1 (c) L. Petrov 28-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_AZELS__TYPE ) :: SPD_RES
      TYPE       ( SPD_DEL__TYPE   ) :: SPD_DEL(SPD_RES%N_STA)
      INTEGER*4  IUER
      REAL*8     TAI_OBS
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J1, J2, MJD_OBS, IDAY, I_STA, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: GET_SPD_RES
      INTEGER*4, EXTERNAL :: LTM_DIF
!    
      DO 410 J1=1,SPD_RES%N_AZEL
         MJD_OBS = SPD_RES%MJD_BEG
         TAI_OBS = SPD_RES%TAI_BEG + SPD_RES%AZEL(J1)%TIM
         IF ( TAI_OBS > 86400.0D0 ) THEN
              IDAY = IDINT(TAI_OBS/86400.0D0)
              TAI_OBS = TAI_OBS - IDAY*86400.0D0
              MJD_OBS = MJD_OBS + IDAY
         END IF
!
         I_STA = LTM_DIF ( 0, SPD_RES%N_STA, SPD_RES%C_STA, SPD_RES%AZEL(J1)%STA_NAM )
         IF ( I_STA < 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4831, IUER, 'SPD_REG_RES_AZEL', 'Trap of '// &
     &            'iternal control -- unknown station '// &
     &            SPD_RES%AZEL(J1)%STA_NAM//' J1= '//STR(1:21) )
              RETURN
         END IF
!
! ------ Compute surface pressure
!
         CALL ERR_PASS ( IUER, IER )
         SPD_RES%AZEL(J1)%PRES = GET_SPD_RES ( 'pres', SPD_DEL(I_STA), &
     &                               MJD_OBS, TAI_OBS, 8.D9, &
     &                               SPD_RES%AZEL(J1)%AZ, &
     &                               SPD_RES%AZEL(J1)%EL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
              CALL ERR_LOG ( 4832, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &            'computation of the surface pressure for station '// &
     &            SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
              RETURN 
         END IF
!
! ------ Compute surface partial pressure of water vapour
!
         CALL ERR_PASS ( IUER, IER )
         SPD_RES%AZEL(J1)%PWP  = GET_SPD_RES ( 'pwp', SPD_DEL(I_STA), &
     &                               MJD_OBS, TAI_OBS, 8.D9, &
     &                               SPD_RES%AZEL(J1)%AZ, &
     &                               SPD_RES%AZEL(J1)%EL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
              CALL ERR_LOG ( 4833, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &            'computation of the surface partial pressure of water '// &
     &            'vapour for station '//SPD_RES%AZEL(J1)%STA_NAM// &
     &            ' at '//STR(1:21) )
              RETURN 
         END IF
!
! ------ Compute surface air temperature
!
         CALL ERR_PASS ( IUER, IER )
         SPD_RES%AZEL(J1)%TEMP = GET_SPD_RES ( 'temp', SPD_DEL(I_STA), &
     &                               MJD_OBS, TAI_OBS, 8.D9, &
     &                               SPD_RES%AZEL(J1)%AZ, &
     &                               SPD_RES%AZEL(J1)%EL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
              CALL ERR_LOG ( 4834, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &            'computation of the air temperature for station '// &
     &            SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
              RETURN 
         END IF
!
! ------ Compute slant total path delay
!
         CALL ERR_PASS ( IUER, IER )
         SPD_RES%AZEL(J1)%DELS(SPD__TOT) = GET_SPD_RES ( 'delt', SPD_DEL(I_STA), &
     &                                         MJD_OBS, TAI_OBS, 8.D9, &
     &                                         SPD_RES%AZEL(J1)%AZ, &
     &                                         SPD_RES%AZEL(J1)%EL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
              CALL ERR_LOG ( 4835, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &            'computation of the slant total path delay for station '// &
     &            SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
              RETURN 
         END IF
!
! ------ Compute slant wet path delay
!
         CALL ERR_PASS ( IUER, IER )
         SPD_RES%AZEL(J1)%DELS(SPD__WAT) = GET_SPD_RES ( 'delw', SPD_DEL(I_STA), &
     &                                         MJD_OBS, TAI_OBS, 8.D9, &
     &                                         SPD_RES%AZEL(J1)%AZ, &
     &                                         SPD_RES%AZEL(J1)%EL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
              CALL ERR_LOG ( 4836, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &            'computation of the slant wet path delay for station '// &
     &            SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
              RETURN 
         END IF
!
         IF ( SPD_RES%N_FRQ > 0 ) THEN
              DO 420 J2=1,SPD_RES%N_FRQ
                 CALL ERR_PASS ( IUER, IER )
                 SPD_RES%AZEL(J1)%OPA(J2)  = GET_SPD_RES ( 'opa', SPD_DEL(I_STA), &
     &                                          MJD_OBS, TAI_OBS, &
     &                                          SPD_RES%FREQ(J2), &
     &                                          SPD_RES%AZEL(J1)%AZ, &
     &                                          SPD_RES%AZEL(J1)%EL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
                      WRITE ( UNIT=STR1(1:11), FMT='(F11.3)' ) 1.0D6*SPD_RES%FREQ(J2)
                      CALL ERR_LOG ( 4837, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &                    'computation of the atmosphere opatcity at frequency '// &
     &                     TRIM(STR1)//' MHz for station '// &
     &                     SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
                     RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 SPD_RES%AZEL(J1)%TATM(J2) = GET_SPD_RES ( 'tatm', SPD_DEL(I_STA), &
     &                                          MJD_OBS, TAI_OBS, &
     &                                          SPD_RES%FREQ(J2), &
     &                                          SPD_RES%AZEL(J1)%AZ, &
     &                                          SPD_RES%AZEL(J1)%EL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER  )
                      WRITE ( UNIT=STR1(1:11), FMT='(F11.3)' ) 1.0D6*SPD_RES%FREQ(J2)
                      CALL ERR_LOG ( 4838, IUER, 'SPD_REG_RES_AZEL', 'Error in '// &
     &                    'computation of the atmosphere brightness temperature '// &
     &                    'at frequency '//TRIM(STR1)//' MHz for station '// &
     &                     SPD_RES%AZEL(J1)%STA_NAM//' at '//STR(1:21) )
                     RETURN 
                 END IF
 420        CONTINUE 
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_SPD_RES_AZEL  !#!#
