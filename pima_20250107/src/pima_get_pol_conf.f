      FUNCTION  PIMA_GET_POL_CONF ( PIM, IND_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_POL_CONF
! *                                                                      *
! * ### 28-DEC-2018 PIMA_GET_POL_CONF v2.0 (c) L. Petrov 25-APR-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  PIMA_GET_POL_CONF*7
      CHARACTER  STR*12, STR1*128
      INTEGER*4  POL_MODE, IND_OBS, IUER
!
      IF ( IND_OBS < 1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 9191, IUER, 'PIMA_GET_POL_CONF', 'Wrong argument '// &
     &         'IND_OBS: '//TRIM(STR)//' while a positiive integer was expected' )
           PIMA_GET_POL_CONF = '??'
           RETURN 
      END IF
      IF ( IND_OBS > PIM%NOBS  ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( IND_OBS, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( PIM%NOBS, STR1 )
           CALL ERR_LOG ( 9192, IUER, 'PIMA_GET_POL_CONF', 'Wrong argument '// &
     &         'IND_OBS: '//TRIM(STR)//' is greater than the total number '// &
     &         'of observations PIM%NOBS: '//STR1 )
           PIMA_GET_POL_CONF = '??'
           RETURN 
      END IF
      IF (         ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &             ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )       ) THEN
           PIMA_GET_POL_CONF = PIMA__PC_CC
         ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &             ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )       ) THEN
           PIMA_GET_POL_CONF = PIMA__PC_LL
         ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &             ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )       ) THEN
           PIMA_GET_POL_CONF = PIMA__PC_LC
         ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &             ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )       ) THEN
           PIMA_GET_POL_CONF = PIMA__PC_CL
         ELSE
           WRITE ( 6, * ) 'Pol_types: ', PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP, &
     &                                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP
           CALL CLRCH ( STR ) 
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 9193, IUER, 'PIMA_GET_POL_CONF', 'Trap of internal '// &
     &         'control during processing observation with idnex IND_OBS: '//TRIM(STR)// &
     &         ' at stations '//PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))//' / '// &
     &         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))//' -- its polartization '// &
     &         'type is not supported' ) 
           PIMA_GET_POL_CONF = '??'
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PIMA_GET_POL_CONF  !#!#
