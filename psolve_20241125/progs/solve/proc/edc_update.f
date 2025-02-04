      SUBROUTINE EDC_UPDATE ( EDC, IOBS, ISITE_I2, ISTAR_I2, MJD, TAI, &
     &                        FL_SUPR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EDC_UPDATE
! *                                                                      *
! *  ### 25-OCT-2007   EDC_UPDATE   v1.0 (c)  L. Petrov  25-OCT-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      INTEGER*4  IOBS, MJD, IUER
      REAL*8     TAI
      LOGICAL*4  FL_SUPR
      INTEGER*2  ISITE_I2(2), ISTAR_I2
      CHARACTER  STR*128
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( .NOT. ASSOCIATED ( EDC%OBS ) ) THEN
           CALL ERR_LOG ( 7621, IUER, 'EDC_UPDATE', 'Trap of internal '// &
     &         'control: object EDC was not initialized' )
           RETURN 
      END IF
      IF ( IOBS == 1 ) THEN
           EDC%HEA%MJD_SES = MJD
           EDC%HEA%TAI_SES = TAI
        ELSE IF ( IOBS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR )
           CALL ERR_LOG ( 7622, IUER, 'EDC_UPDATE', 'Trap of internal '// &
     &         'control: parameter IOBS < 0: '//STR )
           RETURN 
        ELSE IF ( IOBS > EDC%HEA%N_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR )
           CALL ERR_LOG ( 7623, IUER, 'EDC_UPDATE', 'Trap of internal '// &
     &         'control: parameter IOBS is too big: '//STR )
           RETURN 
      END IF
!
      EDC%OBS(IOBS)%TIM_OBS = (MJD - EDC%HEA%MJD_SES)*86400.0D0 + &
     &                        (TAI - EDC%HEA%TAI_SES)
      EDC%OBS(IOBS)%IND_STA(1) = ISITE_I2(1)
      EDC%OBS(IOBS)%IND_STA(2) = ISITE_I2(2)
      EDC%OBS(IOBS)%IND_SOU    = ISTAR_I2
      IF ( FL_SUPR ) THEN
           EDC%OBS(IOBS)%SUP_STS    = 1
         ELSE 
           EDC%OBS(IOBS)%SUP_STS    = 0
      END IF
      EDC%OBS(IOBS)%DCM_STS = EDC%OBS(IOBS)%SUP_STS    
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_UPDATE  !#!#
