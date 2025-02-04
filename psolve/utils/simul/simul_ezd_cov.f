      SUBROUTINE SIMUL_EZD_COV ( SIMUL, ISTA, NOBS, MJD, UTC, AZ, EL, &
                                 IS_NOI, EZD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMUL_EZD_COV
! *                                                                      *
! * ### 09-SEP-2021  SIMUL_EZD_COV  v2.0 (c)  L. Petrov  29-NOV-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'astro_constants.i'
      INCLUDE   'simul.i'
      INTEGER*4  ISTA, NOBS, IS_NOI, IUER
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      INTEGER*4  N, MJD(NOBS)
      REAL*8     UTC(NOBS), AZ(NOBS), EL(NOBS), EZD(NOBS)
      REAL*8,    ALLOCATABLE :: TIM_OBS(:), GAU_NOI(:), COL_NOI(:)
      INTEGER*4  M_COV
      PARAMETER  ( M_COV = 1024 )
      REAL*8     COV_SPL(M_COV), WORK(M_COV), DT, COR_VAL, DER_BEG, &
     &           DER_END, SIGMA, TIM_RANGE 
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, KK, ICOV, N2, IND_NOI, IER
      REAL*8,    EXTERNAL :: RGAUSS, DP_VV_V
      INTEGER*4, EXTERNAL :: IXMN8
!
      ICOV = 0
      DO 410 J1=1,SIMUL%N_COV
         IF ( SIMUL%COV(J1)%STA_NAM == SIMUL%STA_NAM(ISTA) ) THEN
              ICOV = J1
         END IF
 410  CONTINUE 
      IF ( ICOV == 0 ) THEN
           CALL ERR_LOG ( 7841, IUER, 'SIMUL_EZD_COV', 'Error in '// &
     &         'computation of the covariance matrix of the colored noise' )
           RETURN 
      END IF
!
      SIGMA = SIMUL%CNF%DIL_ZEN_COV * SIMUL%COV(ICOV)%RMS
!
      ALLOCATE ( GAU_NOI(SIMUL%COV(ICOV)%NO), &
     &           COL_NOI(SIMUL%COV(ICOV)%NO), &
     &           TIM_OBS(NOBS),               &
     &           STAT= IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 2*8*NOBS*NOBS + 8*NOBS, STR )
           CALL ERR_LOG ( 7842, IUER, 'SIMUL_EZD_COV', 'Error in '// &
     &         'computation of the covariance matrix of the colored noise' )
           RETURN 
      END IF
!
! --- Get Gaussian random noise
!
      DO 420 J2=1,SIMUL%COV(ICOV)%NO
         GAU_NOI(J2) = RGAUSS ( IS_NOI, SIGMA )
 420  CONTINUE 
!
! --- Transform
!
      KK = 1
      DO 430 J3=1,SIMUL%COV(ICOV)%NO
         COL_NOI(J3) = DP_VV_V ( J3, SIMUL%COV(ICOV)%COVS(KK), GAU_NOI ) 
         KK = KK + J3
 430  CONTINUE 
      DO 440 J4=1,NOBS
         TIM_OBS(J4) = ABS ( (MJD(J4) - MJD(1))*86400.0D0 + (UTC(J4) - UTC(1)) )
         IND_NOI = NINT(TIM_OBS(J4)/SIMUL%COV(ICOV)%MOD_STP) + 1
         IF ( IND_NOI > SIMUL%COV(ICOV)%NO ) IND_NOI = SIMUL%COV(ICOV)%NO 
         EZD(J4) = COL_NOI(IND_NOI)
  440 CONTINUE 
      DEALLOCATE ( GAU_NOI, COL_NOI, TIM_OBS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_EZD_COV  !#!#
