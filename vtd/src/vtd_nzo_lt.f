      SUBROUTINE VTD_NZO_LT ( VTD, ISTA1, ISTA2, ISOU, TAU_GEOM, RATE_GEOM, &
     &                        TAU_DER_EOP,  TAU_DER_STA1, TAU_DER_STA2, &
     &                        TAU_DER_POS,  TAU_DER_VEL, &
     &                        RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &                        RATE_DER_POS, RATE_DER_VEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_NZO_LT  computes for the near zone object VLBI time    *
! *   delay, its first time derivative and derivatives with respect to   *
! *   some parameters by solving iteratively light time equation.        *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #2 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #1 between events: coming the wave front to the reference  *
! *   point on the moving axis and clock synchronization. The time delay *
! *   is referred to the moment of coming the wave front to the          *
! *   reference point on the moving axis of the first antenna at time    *
! *   measured by the timescale TAI. The reference point of the station  *
! *   for which modeling is done is defined as the point on the moving   *
! *   axis which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.          *
! * RATE_GEOM ( REAL*8    ) -- First time derivative of geometric time   *
! *                            delay. Units: dimensionless.              *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! *  ###  31-DEC-2005  VTD_NZO_LT  v1.3 (c)  L. Petrov  15-MAY-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     TAU_GEOM, RATE_GEOM, TAU_DER_EOP, TAU_DER_STA1, &
     &           TAU_DER_STA2, TAU_DER_POS(3),  TAU_DER_VEL(3), &
     &           RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &           RATE_DER_POS(3), RATE_DER_VEL(3)
      REAL*8     COO1_BRS(3), COO2_BRS(3), COO2_BRS0(3), COO_SOU(3), &
     &           VEL_SOU(3), B_CRS(3), V_CRS(3), TIM_ARG, TIM_TR1, &
     &           TIM_TR2, TARG, R0J, R1A(3), R2A(3), V1A(3), V2A(3), &
     &           R1A_LEN, R2A_LEN, COO_PLAN(3), VEL_PLAN(3), &
     &           TAU_GRAV1, TAU_GRAV2, TAU_RETR1, TAU_RETR2, &
     &           SOU_STA1(3), SOU_STA2(3), PAR0, PAR1, PAR2, PAR3, &
     &           TAU_GEOM_BAR, DIST_SUN_EARTH, VE_SQ
      CHARACTER  STR*80
      INTEGER*4  J1, J2, J3, J4, J5, KNOT, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V
!
      DIST_SUN_EARTH = DSQRT (   &
     &                           ( VTD%MOM%PLAN(1,VTD__COO,VTD__EART) -     &
     &                             VTD%MOM%PLAN(1,VTD__COO,VTD__SUN) )**2 + &
     &                           ( VTD%MOM%PLAN(2,VTD__COO,VTD__EART) - &
     &                             VTD%MOM%PLAN(2,VTD__COO,VTD__SUN) )**2 + &
     &                           ( VTD%MOM%PLAN(3,VTD__COO,VTD__EART)- &
     &                             VTD%MOM%PLAN(3,VTD__COO,VTD__SUN) )**2   )
      VE_SQ = DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )
!
      PAR0 =  1.D0 - VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &             + VTD__LB
      PAR1 = -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     VTD%STA(ISTA1)%COO_CRS )/ 2.0D0/VTD__C**2
      PAR2 = -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     VTD%STA(ISTA2)%COO_CRS )/ 2.0D0/VTD__C**2
!
      CALL ADDC_VV    ( 3, PAR0, VTD%STA(ISTA1)%COO_CRS, &
     &                     PAR1, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     COO1_BRS )
      CALL ADD_VV   ( 3, COO1_BRS, VTD%MOM%PLAN(1,VTD__COO,VTD__EART) )
!
      CALL ADDC_VV  ( 3, PAR0, VTD%STA(ISTA2)%COO_CRS, &
     &                   PAR2, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                   COO2_BRS0 )
      CALL ADD_VV   ( 3, COO2_BRS0, VTD%MOM%PLAN(1,VTD__COO,VTD__EART) )
!
      IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS ) THEN
           TIM_ARG = VTD%MOM%TDB
         ELSE IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES ) THEN
           TIM_ARG = VTD%MOM%TDT
      END IF
!
      CALL COPY_R8 ( 3, VTD%SOU(ISOU)%SOU_CRS, COO_SOU )
      CALL COPY_R8 ( 3, COO2_BRS0, COO2_BRS )
      TAU_GRAV1 = 0.0D0
      TAU_GRAV2 = 0.0D0
!
      DO 410 J1=1,VTD__NZO_ITER
         CALL SUB_VV_V ( 3, COO_SOU, COO1_BRS, SOU_STA1 )
         CALL SUB_VV_V ( 3, COO_SOU, COO2_BRS, SOU_STA2 )
         TIM_TR1 = DSQRT ( SOU_STA1(1)**2 + SOU_STA1(2)**2 + &
     &                     SOU_STA1(3)**2 )/VTD__C
         TIM_TR2 = DSQRT ( SOU_STA2(1)**2 + SOU_STA2(2)**2 + &
     &                     SOU_STA2(3)**2 )/VTD__C
!
         TARG = (VTD%MOM%MJD - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG)*86400.0D0 + &
     &          (TIM_ARG     - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG) &
     &          - (TIM_TR1 + TAU_GRAV1)
         KNOT = IXMN8 ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                  VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), TARG )
         IF ( KNOT .EQ. -1 ) THEN
              STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
              STR(31:58) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                      VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG, IER )
              CALL ERR_LOG ( 2251, IUER, 'VTD_NZO_LT', 'Moment of time '// &
     &             STR(1:28)//' at TAI precedes the first epoch for the '// &
     &            'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &             STR(31:58) )
              RETURN
         END IF
!
         IF ( KNOT .EQ. -2 ) THEN
              STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
              STR(31:53) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                      VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG + &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL), IER )
              write (6 , * ) ' targ=', targ
              CALL ERR_LOG ( 2252, IUER, 'VTD_NZO_LT', 'Moment of time '// &
     &             STR(1:28)//' at TAI is after the last epoch for the '// &
     &            'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &             STR(31:58) )
              RETURN
         END IF
!
         DO 420 J2=1,3
            COO_SOU(J2) = 0.0D0
            VEL_SOU(J2) = 0.0D0
            DO 430 J3=KNOT-VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL,KNOT
               COO_SOU(J2) = COO_SOU(J2) + &
     &             VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J3,J2)* &
     &             BSPL_VAL ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J3, TARG )
               VEL_SOU(J2) = VEL_SOU(J2) + &
     &             VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J3,J2)* &
     &             BSPL_DER ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J3, TARG )
 430        CONTINUE
 420     CONTINUE
         IF ( VTD%CONF%IVRB .GE. 3 ) THEN
!              WRITE  ( 6, 110 ) J1, COO_SOU, VEL_SOU, TIM_TR1, TIM_TR2
! 110          FORMAT ( 'Iter: ', I2, ' NZO_Coo: ', 3(1PD20.12, 1X)/ &
!     &                 9X, 'NZO_Vel: ', 3(1PD20.12, 1X)/ &
!     &                 9X, 'Tim_tr1= ', 0PF15.9, ' Tim_tr2= ', 0PF15.9  )
              WRITE  ( 6, 110 ) J1, COO_SOU, VEL_SOU, TIM_TR1*VTD__C, TIM_TR2*VTD__C
 110          FORMAT ( 'Iter: ', I2, ' NZO_Coo: ', 3(1PD20.12, 1X)/ &
     &                 9X, 'NZO_Vel: ', 3(1PD20.12, 1X)/ &
     &                 9X, 'Tim_tr1= ', 1PD20.12, ' Tim_tr2= ', 1PD20.12 )
         END IF
         TAU_GRAV1 = 0.0D0
         TAU_GRAV2 = 0.0D0
         DO 440 J4=1,VTD__M_PLA
!
! --------- Get cordinates of the planet at the moment of time of arrival of
! --------- the photon at the station
!
            CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__COO,J4), COO_PLAN )
            CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__VEL,J4), VEL_PLAN )
!
! --------- Solve light cone equation by iteration
!
            CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
            CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
            CALL NORM_VEC ( 3, R1A, R1A_LEN )
            CALL NORM_VEC ( 3, R2A, R2A_LEN )
            TAU_RETR1 = -R1A_LEN/VTD__C ! Get time retardation
            TAU_RETR2 = -R2A_LEN/VTD__C
!
! --------- Compute position of the gravitating body at retarded moment
! --------- of time
!
            CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__COO,J4), &
     &                      TAU_RETR1, VEL_PLAN, COO_PLAN )
            CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
            CALL NORM_VEC ( 3, R1A, R1A_LEN )
!
            CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__COO,J4), &
     &                     TAU_RETR2, VEL_PLAN, COO_PLAN )
            CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
            CALL NORM_VEC ( 3, R2A, R2A_LEN )
            R0J = DSQRT ( (COO_PLAN(1)-COO_SOU(1))**2 + &
     &                    (COO_PLAN(2)-COO_SOU(2))**2 + &
     &                    (COO_PLAN(3)-COO_SOU(3))**2 &
     &                  )
!
! --------- NB: we bypass computation of gravitational delay due to Earth's
! ---------     gravity field for the station at "GEOCENTR". Contribution
! ---------     to gravitational delay caused by Earth's gravity field
! ---------     from that station is set to zero.
!
            IF ( R1A_LEN > VTD__REA/2.0D0 ) THEN
                 TAU_GRAV1 = TAU_GRAV1 + 2.D0*VTD__GM(J4)/VTD__C**3 * &
     &                       DLOG ( (R0J + R1A_LEN + TIM_TR1*VTD__C)/ &
     &                              (R0J + R1A_LEN - TIM_TR1*VTD__C) &
     &                            )
            END IF
            IF ( R2A_LEN > VTD__REA/2.0D0 ) THEN
                 TAU_GRAV2 = TAU_GRAV2 + 2.D0*VTD__GM(J4)/VTD__C**3 * &
     &                       DLOG ( DABS(R0J + R2A_LEN + TIM_TR2*VTD__C)/ &
     &                              DABS(R0J + R2A_LEN - TIM_TR2*VTD__C) &
     &                            )
            END IF
 440     CONTINUE
         CALL ADDC_VV  ( 3, 1.0D0, COO2_BRS0, &
     &                      (TIM_TR2 - TIM_TR1) + (TAU_GRAV2-TAU_GRAV1), &
     &                      VTD%STA(ISTA2)%VEL_CRS, COO2_BRS )
         CALL ADDC_VV  ( 3, 1.0D0, COO2_BRS, &
     &                      (TIM_TR2 - TIM_TR1) + (TAU_GRAV2-TAU_GRAV1), &
     &                      VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), COO2_BRS )
 410  CONTINUE
!
! --- Compute |coo_sou - coo2_brs| - |coo_sou - coo1_brs|
!
      CALL DIFF_MOD ( COO_SOU, COO2_BRS, COO1_BRS, TAU_GEOM )
      TAU_GEOM_BAR = TAU_GEOM/VTD__C + (TAU_GRAV2-TAU_GRAV1)
      IF ( VTD%CONF%IVRB .GE. 3 ) THEN
           WRITE  ( 6, 120 ) COO_SOU, VEL_SOU
 120       FORMAT ( 'NZO_Coo: ', 3(1PD20.12, 1X)/ &
     &              'NZO_Vel: ', 3(1PD20.12, 1X) )
      END IF
      IF ( VTD%CONF%IVRB .GE. 2 ) THEN
           WRITE  ( 6, 130 ) TAU_GRAV1, TAU_GRAV2, (TAU_GRAV2-TAU_GRAV1)
 130       FORMAT ( 'Tau_gr1= ', 1PD20.12, ' Tau_gr2= ', 1pD20.12, &
     &              ' Tau_gr= ', 1PD20.12 )
           WRITE  ( 6, 140 ) TAU_GEOM/VTD__C
 140       FORMAT ( 'Tau_geom_bar_raw: ', F20.15 )
           WRITE ( 6, * ) ' LT: ', DSQRT ( (COO_SOU(1)-COO1_BRS(1))**2 + (COO_SOU(2)-COO1_BRS(2))**2 + (COO_SOU(3)-COO1_BRS(3))**2 ), &
     &                             DSQRT ( (COO_SOU(1)-COO2_BRS(1))**2 + (COO_SOU(2)-COO2_BRS(2))**2 + (COO_SOU(3)-COO2_BRS(3))**2 ), &
     &         ( DSQRT ( (COO_SOU(1)-COO1_BRS(1))**2 + (COO_SOU(2)-COO1_BRS(2))**2 + (COO_SOU(3)-COO1_BRS(3))**2 ) - &
     &           DSQRT ( (COO_SOU(1)-COO2_BRS(1))**2 + (COO_SOU(2)-COO2_BRS(2))**2 + (COO_SOU(3)-COO2_BRS(3))**2 ) )/VTD__C
      END IF
!
      PAR3 = 1.D0 - VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 + &
     &       VE_SQ/2.0D0/VTD__C**2 + VTD__LB - VTD__LG
      CALL SUB_VV_V ( 3, COO2_BRS, COO1_BRS, B_CRS )
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%STA(ISTA1)%VEL_CRS, V_CRS )
!
      TAU_GEOM =   PAR3*TAU_GEOM_BAR &
     &           - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )/ &
     &             VTD__C**2
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_NZO_LT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIFF_MOD ( S, X1, X2, DIF )
! ************************************************************************
! *                                                                      *
! *   Auxilliary procedure computes the difference                       *
! *   | S - X1 | - | S - X2 |  where S, X1, and X2 are three-dimensional *
! *   vectors.                                                           *
! *                                                                      *
! *  ### 04-JAN-2006   DIFF_MOD    v2.0 (c)  L. Petrov  28-JUN-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     S(3), X1(3), X2(3), DIF
      REAL*8     S_MOD, S_UNIT(3), X1_S(3), X2_S(3)
#ifdef GNU
      REAL ( KIND=10 ) BETA, GAMMA, BG_DIF, ONE
#else
      REAL ( KIND=8 ) BETA, GAMMA, BG_DIF, ONE
#endif
      ONE = 1.0D0 
!
      S_MOD = SQRT ( S(1)**2 + S(2)**2 + S(3)**2 )
!
      S_UNIT(1) = S(1)/S_MOD
      S_UNIT(2) = S(2)/S_MOD
      S_UNIT(3) = S(3)/S_MOD
!
      X1_S(1) = X1(1)/S_MOD
      X1_S(2) = X1(2)/S_MOD
      X1_S(3) = X1(3)/S_MOD
!
      X2_S(1) = X2(1)/S_MOD
      X2_S(2) = X2(2)/S_MOD
      X2_S(3) = X2(3)/S_MOD
!
      BETA  = (X1(1)**2 + X1(2)**2 + X1(3)**2)/S_MOD**2 - &
     &         2.D0*( S_UNIT(1)*X1_S(1) + S_UNIT(2)*X1_S(2) + S_UNIT(3)*X1_S(3) )
      GAMMA = (X2(1)**2 + X2(2)**2 + X2(3)**2)/S_MOD**2 - &
     &         2.D0*( S_UNIT(1)*X2_S(1) + S_UNIT(2)*X2_S(2) + S_UNIT(3)*X2_S(3) )
!
! --- Two special tricks for preserving precision of the operation of subtraction
! --- of close quantities:
! --- 1) sqrt(1+b) - sqrt(1+c) = (b-c)/(sqrt(1+b)+sqrt(1+c))
! --- 2) operand are 80 or 128 bits long.
!
      BG_DIF = (BETA - GAMMA)/(SQRT ( ONE + BETA ) + SQRT ( ONE + GAMMA ))
!
      DIF = S_MOD*BG_DIF
      RETURN
      END  SUBROUTINE  DIFF_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIFF_MOD1 ( S, X1, X2, DIF )
! ************************************************************************
! *                                                                      *
! *   Auxilliary procedure computes the difference                       *
! *   | S - X1 | - | S - X2 |  where S, X1, and X2 are three-dimensional *
! *   vectors. The procedure considers that |S| > 0.01*|X1| and          *
! *   |S|>> 0.01*|X2| and organizes computation in such a way that       *
! *   precision is not lost due to rounding.                             *
! *                                                                      *
! *   It expands the expression in the series over |X1|/|S|, |X2|/|S| .  *
! *                                                                      *
! *  ### 04-JAN-2006   DIFF_MOD1   v1.0 (c)  L. Petrov  04-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     S(3), X1(3), X2(3), DIF
      REAL*8     S_MOD, S_UNIT(3), X1_S(3), X2_S(3), BETA, GAMMA
      REAL*8     C(16)
!
      S_MOD = SQRT ( S(1)**2 + S(2)**2 + S(3)**2 )
!
      S_UNIT(1) = S(1)/S_MOD
      S_UNIT(2) = S(2)/S_MOD
      S_UNIT(3) = S(3)/S_MOD
!
      X1_S(1) = X1(1)/S_MOD
      X1_S(2) = X1(2)/S_MOD
      X1_S(3) = X1(3)/S_MOD
!
      X2_S(1) = X2(1)/S_MOD
      X2_S(2) = X2(2)/S_MOD
      X2_S(3) = X2(3)/S_MOD
!
      BETA  = (X1(1)**2 + X1(2)**2 + X1(3)**2)/S_MOD**2 - &
     &         2.D0*( S_UNIT(1)*X1_S(1) + S_UNIT(2)*X1_S(2) + S_UNIT(3)*X1_S(3) )
      GAMMA = (X2(1)**2 + X2(2)**2 + X2(3)**2)/S_MOD**2 - &
     &         2.D0*( S_UNIT(1)*X2_S(1) + S_UNIT(2)*X2_S(2) + S_UNIT(3)*X2_S(3) )
!
      C(1)  = 1.0D0/2.0D0
      C(2)  = C(1)/2.0D0
      C(3)  = C(2)/2.0D0
      C(4)  = C(3)*5.0D0/4.0D0/2.0D0
      C(5)  = C(4)*7.0D0/5.0D0/2.0D0
      C(6)  = C(5)*9.0D0/6.0D0/2.0D0
      C(7)  = C(6)*11.0D0/7.0D0/2.0D0
      C(8)  = C(7)*13.0D0/8.0D0/2.0D0
      C(9)  = C(8)*15.0D0/9.0D0/2.0D0
      C(10) = C(9)*17.0D0/10.0D0/2.0D0
      C(11) = C(10)*19.0D0/11.0D0/2.0D0
      C(12) = C(11)*21.0D0/12.0D0/2.0D0
      C(13) = C(12)*23.0D0/13.0D0/2.0D0
      C(14) = C(13)*25.0D0/14.0D0/2.0D0
      C(15) = C(14)*27.0D0/15.0D0/2.0D0
      C(16) = C(15)*29.0D0/16.0D0/2.0D0
      DIF = (BETA-GAMMA)/2.0D0*S_MOD* &
     &        (1.0D0 - C(2)* (BETA+GAMMA)    &
     &               + C(3)* (BETA**2  + BETA*GAMMA     + GAMMA**2) &
     &               - C(4)* (BETA**3  + BETA**2*GAMMA  + BETA*GAMMA**2 + GAMMA**3) &
     &               + C(5)* (BETA**4  + BETA**3*GAMMA  + BETA**2*GAMMA**2  + BETA*GAMMA**3 + GAMMA**4) &
     &               - C(6)* (BETA**5  + BETA**4*GAMMA  + BETA**3*GAMMA**2  + BETA**2*GAMMA**3  + BETA*GAMMA**4 + GAMMA**5) &
     &               + C(7)* (BETA**6  + BETA**5*GAMMA  + BETA**4*GAMMA**2  + BETA**3*GAMMA**3  + BETA**2*GAMMA**4 + &
     &                        BETA*GAMMA**5 + GAMMA**6) &
     &               - C(8)* (BETA**7  + BETA**6*GAMMA  + BETA**5*GAMMA**2  + BETA**4*GAMMA**3  + BETA**3*GAMMA**4 + &
     &                        BETA**2*GAMMA**5 + BETA*GAMMA**6 + GAMMA**7) &
     &               + C(9)* (BETA**8  + BETA**7*GAMMA  + BETA**6*GAMMA**2  + BETA**5*GAMMA**3  + BETA**4*GAMMA**4 + &
     &                        BETA**3*GAMMA**5 + BETA**2*GAMMA**6 + BETA*GAMMA**7 + GAMMA**8) &
     &               - C(10)*(BETA**9  + BETA**8*GAMMA  + BETA**7*GAMMA**2  + BETA**6*GAMMA**3  + BETA**5*GAMMA**4 + &
     &                        BETA**4*GAMMA**5 + BETA**3*GAMMA**6 + BETA**2*GAMMA**7 + BETA*GAMMA**8 + GAMMA**9) &
     &               + C(11)*(BETA**10 + BETA**9*GAMMA  + BETA**8*GAMMA**2  + BETA**7*GAMMA**3  + BETA**6*GAMMA**4 + &
     &                        BETA**5*GAMMA**5 + BETA**4*GAMMA**6 + BETA**3*GAMMA**7 + BETA**2*GAMMA**8 + &
     &                        BETA*GAMMA**9 + GAMMA**10) &
     &               - C(12)*(BETA**11 + BETA**10*GAMMA + BETA**9*GAMMA**2  + BETA**8*GAMMA**3  + BETA**7*GAMMA**4 + &
     &                        BETA**6*GAMMA**5 + BETA**5*GAMMA**6 + BETA**4*GAMMA**7 + BETA**3*GAMMA**8 + &
     &                        BETA**2*GAMMA**9 + BETA*GAMMA**10 +GAMMA**11) &
     &               + C(13)*(BETA**12 + BETA**11*GAMMA + BETA**10*GAMMA**2 + BETA**9*GAMMA**3  + BETA**8*GAMMA**4 + &
     &                        BETA**7*GAMMA**5 + BETA**6*GAMMA**6 + BETA**5*GAMMA**7 + BETA**4*GAMMA**8 + &
     &                        BETA**3*GAMMA**9 + BETA**2*GAMMA**10 + BETA*GAMMA**11 + GAMMA**12) &
     &               - C(14)*(BETA**13 + BETA**12*GAMMA + BETA**11*GAMMA**2 + BETA**10*GAMMA**3 + BETA**9*GAMMA**4 + &
     &                        BETA**8*GAMMA**5 + BETA**7*GAMMA**6 + BETA**6*GAMMA**7 + BETA**5*GAMMA**8 + &
     &                        BETA**4*GAMMA**9 + BETA**3*GAMMA**10 + BETA**2*GAMMA**11 + BETA*GAMMA**12 + GAMMA**13) &
     &        )
      RETURN
      END  SUBROUTINE  DIFF_MOD1  !#!#
