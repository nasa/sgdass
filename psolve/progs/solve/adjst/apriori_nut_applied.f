      SUBROUTINE APRIORI_NUT_APPLIED ( APR_PSI, APR_EPS, ANO_PSI, ANO_EPS, &
     &                                 APR_NTX, APR_NTY, JD_TDB_EPOCH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  APRIORI_NUT_APPLIED  computes the difference between the  *
! *   used apriori nutation angles and the reference one (currently      *
! *   (09-NOV-99) assumed IAU 1980 model).                               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * APR_PSI ( REAL*8    ) -- A priori nutation angle in nutation         *
! *                          in longitude (in radians).                  *
! * APR_EPS ( REAL*8    ) -- A priori nutation angle in nutation         *
! *                          in obliquity (in radians).                  *
! * ANO_PSI ( REAL*8    ) -- Difference in nutation angle in nutation    *
! *                          in longitude (in radians).                  *
! * ANO_EPS ( REAL*8    ) -- Difference in nutation angle in nutation    *
! *                          in obliquity (in radians).                  *
! * APR_NTX ( REAL*8    ) -- A priori nutation angle in nutation         *
! *                          in X-coordinates according to               *
! *                          Ginot-Capitaine formalism (in radians).     *
! * APR_NTY ( REAL*8    ) -- A priori nutation angle in nutation         *
! *                          in Y-coordinates according to               *
! *                          Ginot-Capitaine formalism (in radians).     *
! * JD_TDB_EPOCH ( REAL*8   ) -- JD in TDB of the nutation epoch.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 09-NOV-99 APRIORI_NUT_APPLIED v2.2 (c) L. Petrov 18-AUG-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'cals.i'
      REAL*8     TIM_SEC, APR_PSI, APR_EPS, ANO_PSI, ANO_EPS, JD_TDB_EPOCH
      REAL*8     APR_NTX, APR_NTY
!
! ----- Empirical paramters for precession rate, obliquity rate and CEP 
! ----- coordinates at J2000.0  epoch
!
        REAL*8     PSI_RATE_MHB2000, EPS_RATE_MHB2000
        REAL*8     PSI_OFFS_MHB2000, EPS_OFFS_MHB2000
        PARAMETER  ( PSI_RATE_MHB2000 =  -2.9965D0 ) ! mas/yr
        PARAMETER  ( EPS_RATE_MHB2000 =  -0.2524D0 ) ! mas/yr
        PARAMETER  ( PSI_OFFS_MHB2000 = -41.7750D0 ) ! For Calc-10 compatibility
        PARAMETER  ( EPS_OFFS_MHB2000 =  -6.8192D0 ) ! mas
      INTEGER*4  IUER
!
      TYPE ( CALS_STRU ) ::  CALS
      LOGICAL*4  FL_WAHR, FL_GEOD
      REAL*8     FJD_EPOCH(2), FJD_0, L1(4), PSG(2)
      REAL*8     TIM_CENT, L1_VAL, PSG_VAL
      INTEGER*4  J1, J2, IER
!
! --- Get calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R   ( INT2(1), 0, 1, CALS, IER )
!
! --- Learn whether Wahr nutation has been applied and whether
! --- geodetic nutation has been applied
!
      FL_WAHR = .FALSE.
      FL_GEOD = .FALSE.
!
      DO 410 J1=1,CALS%L_CONT
         IF ( CALS%CONT(J1) .EQ. 'WAHR NUT'  .AND.  CALS%CONT_AVL(J1)  .AND. &
     &        CALS%CONT_APL(J1) ) FL_WAHR = .TRUE.
         IF ( CALS%CONT(J1) .EQ. 'GDSICNUT'  .AND.  CALS%CONT_AVL(J1)  .AND. &
     &        CALS%CONT_APL(J1) ) FL_GEOD = .TRUE.
 410  CONTINUE
!
! --- Get epochs of the first and last observation (Julian dat in UTC)
!
      CALL OBSTM ( FJD_EPOCH(1), FJD_EPOCH(2) )
!
      L1(1)  =       357.52910918D0 * DEG__TO__RAD ! Mean     ! IERS TN 21 page 23
      L1(2)  = 129596581.0481D0 * ARCSEC__TO__RAD  ! anomaly  !
      L1(3)  =        -0.5532D0 * ARCSEC__TO__RAD  ! of       !
      L1(4)  =        -0.000136 * ARCSEC__TO__RAD  ! the Sum  !
      PSG(1) = -0.000153D0 * ARCSEC__TO__RAD
      PSG(2) = -0.000002D0 * ARCSEC__TO__RAD
!
! --- Compute geodetic nutation on the epoch between the first observation and
! --- the last observation
! --- (Oh, we know that the argument for this computation should be TDB,
! ---  not UTC, but we neglect this difference here)
!
      JD_TDB_EPOCH = ( FJD_EPOCH(1) + FJD_EPOCH(2))/2.0
      TIM_SEC  = ( JD_TDB_EPOCH - J2000__JD - 0.5D0 )*86400.0D0
      TIM_CENT = ( JD_TDB_EPOCH - J2000__JD - 0.5D0 )/36525.0
      L1_VAL = ( (L1(4)*TIM_CENT + L1(3) )*TIM_CENT + L1(2) )*TIM_CENT +L1(1)
      PSG_VAL = PSG(1)*DSIN(L1_VAL) + PSG(2)*DSIN(2.0*L1_VAL)
!
      ANO_PSI = 0.0D0
      ANO_EPS = 0.0D0
      APR_PSI = NUTPSI_AVE
      APR_EPS = NUTEPS_AVE
      APR_NTX = NUT_XY_AVE(1)
      APR_NTY = NUT_XY_AVE(2)
!
      IF ( FL_WAHR ) THEN
           APR_PSI = APR_PSI - NUTPSI_DIF
           APR_EPS = APR_EPS - NUTEPS_DIF
         ELSE
!
! -------- Apply difference a priori nutation. This difference has been
! -------- computed by SDBH(geto2) and kept in socom.i
!
           ANO_PSI = ANO_PSI + NUTPSI_DIF
           ANO_EPS = ANO_EPS + NUTEPS_DIF
!
           IF ( CALCV .GT. 9.99D0  .AND.  CALCV .LT. 99.99 ) THEN
!!!!
!!                ANO_PSI = ANO_PSI - (299.65*TIM_CENT + 41.775)*  &
!!     &                    ARCSEC_RAD * 1.D-3
!!                ANO_EPS = ANO_EPS - (25.24*TIM_CENT + 6.8192)*   &
!!     &                    ARCSEC_RAD * 1.D-3
!!!!
                ANO_PSI = ANO_PSI + (PSI_RATE_MHB2000*TIM_SEC/(86400*JYEAR__DAYS) + &
     &                               PSI_OFFS_MHB2000)*MAS__TO__RAD
                ANO_EPS = ANO_EPS + (EPS_RATE_MHB2000*TIM_SEC/(86400*JYEAR__DAYS) + &
     &                               EPS_OFFS_MHB2000)* MAS__TO__RAD
            ENDIF
      END IF
!
      IF ( FL_GEOD ) THEN
!
! -------- Apply geodesic nutation
!
           ANO_PSI = ANO_PSI + PSG_VAL
           APR_PSI = APR_PSI + PSG_VAL
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  APRIORI_NUT_APPLIED  #!#
