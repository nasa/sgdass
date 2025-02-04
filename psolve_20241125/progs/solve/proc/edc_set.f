      SUBROUTINE EDC_SET ( EDC, EDC_PAR, IOBS, ISITE_I2, ISTAR_I2, MJD, TAI, &
     &                     SUPSTAT, AUTO_SUP, IUER )
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
      INTEGER*4  EDC_PAR, IOBS, MJD, SUPSTAT, AUTO_SUP, IUER
      REAL*8     TAI
      INTEGER*2  ISITE_I2(2), ISTAR_I2
      REAL*8     TIM_OBS, EPS
      PARAMETER  ( EPS = 0.1D0 ) 
      CHARACTER  STR*128
      INTEGER*4  IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( .NOT. ASSOCIATED ( EDC%OBS ) ) THEN
           CALL ERR_LOG ( 7671, IUER, 'EDC_SET', 'Trap of internal '// &
     &         'control: object EDC was not initialized' )
           RETURN 
      END IF
!
      IF ( IOBS < 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR ) 
           CALL ERR_LOG ( 7672, IUER, 'EDC_SET', 'Trap of internal '// &
     &         'control: wrong value of IOBS: '//STR )
           RETURN 
      END IF
!
      IF ( IOBS > EDC%HEA%N_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR(1:12)  ) 
           CALL INCH  ( EDC%HEA%N_OBS, STR(21:26) ) 
           CALL ERR_LOG ( 7673, IUER, 'EDC_SET', 'Mismatch in the '// &
     &         'observation counter: attempt to decimate observation '// &
     &         '# '//STR(1:I_LEN(STR))//' while the decimation file '// &
     &          EDC%EDC_FILE(1:I_LEN(EDC%EDC_FILE))//' defines only '// &
     &         STR(21:26)//' observations' )
           RETURN 
      END IF
!      
      IF ( EDC%OBS(IOBS)%IND_STA(1) .NE. ISITE_I2(1) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(EDC%OBS(IOBS)%IND_STA(1)), STR(11:14) )
           CALL INCH  ( INT4(ISITE_I2(1)), STR(21:24) )
           CALL INCH  ( IOBS, STR(31:36) )
           CALL ERR_LOG ( 7674, IUER, 'EDC_SET', 'Mismatch in observation '// &
     &         '# '//STR(31:36)//' in the external decimation file '// &
     &          EDC%EDC_FILE(1:I_LEN(EDC%EDC_FILE))//' -- station1 index '// &
     &         'in the database: '//STR(21:24)//' station1 index in the '// &
     &         'file: '//STR(11:14) )
           RETURN 
      END IF
!      
      IF ( EDC%OBS(IOBS)%IND_STA(2) .NE. ISITE_I2(2) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(EDC%OBS(IOBS)%IND_STA(2)), STR(11:14) )
           CALL INCH  ( INT4(ISITE_I2(2)), STR(21:24) )
           CALL INCH  ( IOBS, STR(31:36) )
           CALL ERR_LOG ( 7675, IUER, 'EDC_SET', 'Mismatch in observation '// &
     &         '# '//STR(31:36)//' in the external decimation file '// &
     &          EDC%EDC_FILE(1:I_LEN(EDC%EDC_FILE))//' -- station2 index '// &
     &         'in the database: '//STR(21:24)//' station2 index in the '// &
     &         'file: '//STR(11:14) )
           RETURN 
      END IF
!      
      IF ( EDC%OBS(IOBS)%IND_SOU .NE. ISTAR_I2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(EDC%OBS(IOBS)%IND_SOU), STR(11:14) )
           CALL INCH  ( INT4(ISTAR_I2), STR(21:24) )
           CALL INCH  ( IOBS, STR(31:36) )
           CALL ERR_LOG ( 7675, IUER, 'EDC_SET', 'Mismatch in observation '// &
     &         '# '//STR(31:36)//' in the external decimation file '// &
     &          EDC%EDC_FILE(1:I_LEN(EDC%EDC_FILE))//' -- source index '// &
     &         'in the database: '//STR(21:24)//' source index in the '// &
     &         'file: '//STR(11:14) )
           RETURN 
      END IF
!
      TIM_OBS = (MJD - EDC%HEA%MJD_SES)*86400.0D0 + &
     &          (TAI - EDC%HEA%TAI_SES)
      IF ( DABS( TIM_OBS - EDC%OBS(IOBS)%TIM_OBS ) > EPS ) THEN
           STR(1:30)  = MJDSEC_TO_DATE ( MJD, TAI, IER )
           STR(41:70) = MJDSEC_TO_DATE ( EDC%HEA%MJD_SES, &
     &                           EDC%HEA%TAI_SES + EDC%OBS(IOBS)%TIM_OBS , &
     &                           IER )
           CALL INCH  ( IOBS, STR(81:86) )
           CALL ERR_LOG ( 7675, IUER, 'EDC_SET', 'Mismatch in observation '// &
     &         '# '//STR(81:86)//' in the external decimation file '// &
     &          EDC%EDC_FILE(1:I_LEN(EDC%EDC_FILE))//' -- observation '// &
     &         'date in the database: '//STR(1:23)//' observation date '// &
     &         'in the file: '//STR(41:63) )
           RETURN 
      END IF
!
! --- Set decimation bits
!
      IF ( EDC%OBS(IOBS)%DCM_STS == EDC_PAR ) THEN
           CALL SBIT ( SUPSTAT, DECM__SPS, INT2(1) )
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(DECM__SPS) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_SET  !#!#
