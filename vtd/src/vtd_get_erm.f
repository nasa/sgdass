      SUBROUTINE VTD_GET_ERM ( MJD, TAI, M_DEG, M_ERM, L_DEG, L_ERM, &
     &                         MJD_BEG, TAI_BEG, TIM_ARR, SPL_ARR, ERR_ARR, &
     &                         COV_ARR, ERM_VAL, ERM_DER, ERM_DR2, &
     &                         ERM_ERR, ERM_ER1, ERM_ER2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_GET_ERM  computes the small vector of the perturbing  *
! *   Earth's rotation as well the first and the second derivatives and  *
! *   their formal uncertainties at the specified moment of time         *
! *   using coefficients of expansion of this vector into B-spline       *
! *   basis.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     MJD ( INTEGER*4 ) -- Modified Julian date of the moment of       *
! *                          interest.                                   *
! *     TAI ( REAL*8    ) -- TAI part of the date of the moment of       *
! *                          interest on which the EOP are to be         *
! *                          computed.                                   *
! *   M_DEG ( INTEGER*4 ) -- Maximum degree of the B-spline used for     *
! *                          parameterization of the empirical Earth     *
! *                          rotation model.                             *
! *   M_ERM ( INTEGER*4 ) -- Maximum number of knots of the B-spline for *
! *                          the Earth rotation model.                   *
! *   L_DEG ( INTEGER*4 ) -- Array of actual degree of B-spline for each *
! *                          component of the Earth rotation.            *
! *   L_ERM ( INTEGER*4 ) -- Array of actual numbers of knots of         *
! *                          B-spline for each component of the Earth    *
! *                          rotation.                                   *
! * MJD_BEG ( INTEGER*4 ) -- Modified Julian date of the first knot      *
! *                          of the B-spline for parameterization the    *
! *                          Earth rotation.                             *
! * TAI_BEG ( REAL*8    ) -- TAI part of the date of the first knot      *
! *                          of the B-spline for parameterization the    *
! *                          Earth rotation.                             *
! * TIM_ARR ( REAL*8    ) -- Array of time epochs for three components   *
! *                          of the Earth rotation. Time epochs are time *
! *                          in TAI elapsed from the first epoch. First  *
! *                          Dimension runs over extended knots.         *
! *                          Dimension: (1-M_DEG:M_ERM,3) .              *
! * SPL_ARR ( REAL*8    ) -- Array of spline coefficients of B-spline    *
! *                          which parameterizes the Earth rotation.     *
! *                          Dimension: (1-M_DEG:M_ERM,3). The first     *
! *                          dimension runs over extended knots index,   *
! *                          the second dimension runs over the          *
! *                          component in the Earth rotation.            * 
! * ERR_ARR ( REAL*8    ) -- Array of formal uncertainties in spline     *
! *                          coefficients of B-spline which              *
! *                          parameterizes the Earth rotation.           *
! *                          Dimension: (1-M_DEG:M_ERM,3). The first     *
! *                          dimension runs over extended knots index,   *
! *                          the second dimension runs over the          *
! *                          component in the Earth rotation.            * 
! * COV_ARR ( REAL*8    ) -- Array of some off-diagonal elements of the  *
! *                          covariance matrix between B-spline          *
! *                          coefficients which parametrize the Earth's  *
! *                          rotation. Dimension:                        *
! *                          (1-M_DEG:M_ERM,M_DEG,3). The first          *
! *                          dimension runs over extended knots. The     *
! *                          second dimension runs over maximum degree   *
! *                          of the spline, the third dimension runs     *
! *                          over the component of the Earth's rotation. *
! *                          For the knot k, COV_ARR(k,1,d) corresponds  *
! *                          to covariance matrix element (k,k+1),       *
! *                          COV_ARR(k,2,d) -- (k,k+2), and              *
! *                          COV_ARR(k,l_deg,d) -- (k,k+l_deg+1,d).      *
! *                          By another words, array COV_ARR keeps       *
! *                          l_deg bands of diagonal elements of the     *
! *                          covariance matrix starting from the         *
! *                          diagonal which is just below the main       *
! *                          diagonal.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * ERM_VAL ( REAL*8    ) -- Vector of the perturbing rotation of the    *
! *                          planet Earth. Dimension: 3. Units: rad.     *
! * ERM_DER ( REAL*8    ) -- The first time derivative of the vector of  *
! *                          the perturbing rotation of the planet       *
! *                          Earth.  Dimension: 3. Units: rad/s.         *
! * ERM_DR2 ( REAL*8    ) -- The second time derivative of the vector of *
! *                          the perturbing rotation of the planet       *
! *                          Earth.  Dimension: 3. Units: rad/s^2.       *
! * ERM_ERR ( REAL*8    ) -- Formal uncertainty of the vector of         *
! *                          perturbing rotation of the planet Earth.    *
! *                          Dimension: 3. Units: rad.                   *
! * ERM_ER1 ( REAL*8    ) -- Formal uncertainty of the first time        *
! *                          derivative of the vector of perturbing      *
! *                          rotation of the planet Earth.               *
! *                          Dimension: 3. Units: rad/s.                 *
! * ERM_ER2 ( REAL*8    ) -- Formal uncertainty of the second time       *
! *                          derivative of the vector of perturbing      *
! *                          rotation of the planet Earth.               *
! *                          Dimension: 3. Units: rad/s^2.               *
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
! * ### 10-APR-2006    VTD_GET_ERM   v1.0 (c) L. Petrov 20-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MJD, M_DEG, M_ERM, L_DEG(3), L_ERM(3), MJD_BEG, IUER 
      REAL*8     TAI, TAI_BEG, TIM_ARR(1-M_DEG:M_ERM,3), &
     &           SPL_ARR(1-M_DEG:M_ERM,3), &
     &           ERR_ARR(1-M_DEG:M_ERM,3), COV_ARR(1-M_DEG:M_ERM,M_DEG,3)
      REAL*8     ERM_VAL(3), ERM_DER(3), ERM_DR2(3), &
     &           ERM_ERR(3), ERM_ER1(3), ERM_ER2(3)
      CHARACTER  STR*32, STR1*32, STR2*32, STR3*32
      INTEGER*4  MAX_DEG
      PARAMETER  ( MAX_DEG = 128 ) 
      REAL*8     TIM_ARG, COV(MAX_DEG), RH(MAX_DEG), RD(MAX_DEG), RQ(MAX_DEG), &
     &           VEC(MAX_DEG)
      REAL*8     SCL_RQ
      PARAMETER  ( SCL_RQ = 1.D10 ) ! Scaling factor for RQ.
      INTEGER*4  IELM1, IELM2, J1, J2, J3, KNOT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V
      INTEGER*4  LOCS, I, J
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      TIM_ARG = (MJD - MJD_BEG)*86400.0D0 + (TAI - TAI_BEG)
!
! --- Cycle over components of the Earth's rotation
!
      DO 410 J1=1,3
!
! ------ Get the pivotal knot which corresponds to the moment of time
! ------ under considertaion.
!
         KNOT = IXMN8 ( L_ERM(J1), TIM_ARR(1,J1), TIM_ARG )
         IF ( KNOT < 1 ) THEN
              CALL NOUT_R8 ( 3, ERM_VAL )
              CALL NOUT_R8 ( 3, ERM_DER )
              CALL NOUT_R8 ( 3, ERM_DR2 )
              CALL NOUT_R8 ( 3, ERM_ERR )
              CALL NOUT_R8 ( 3, ERM_ER1 )
              CALL NOUT_R8 ( 3, ERM_ER2 )
!
              STR  = MJDSEC_TO_DATE ( MJD, TAI, -2 )
              STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -2 )
              STR2 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG + &
     &                                         TIM_ARR(L_ERM(J1),J1), -2 )
              CALL CLRCH ( STR3 )
              CALL INCH  ( J1, STR3 )
              CALL ERR_LOG ( 6411, IUER, 'VTD_GET_ERM', 'Requested epoch '// &
     &                       STR(1:23)//' is out of the interval '// &
     &                      '[ '//STR1(1:23)//', '//STR2(1:23)//'] '// &
     &                      'for the '//STR3(1:I_LEN(STR3))//' component '// &
     &                      'of the Earth rotation' )
              RETURN 
         END IF
!%%   write ( 6, * )'VTD_GET_ERM-163 knot= ', knot, ' spl_arr= ', spl_arr(knot,j1) ! %%%%%%%
!
! ------ Compute values of B-spline function, their first and second 
! ------ derivatives: RH, RD, and RQ respectively.
!
         IELM1 = 0
         DO 420 J2=KNOT-L_DEG(J1),KNOT
            IELM1 = IELM1 + 1  ! index of the first covarianve matrix element. Counter from 1
            RH(IELM1) = BSPL_VAL ( L_ERM(J1), TIM_ARR(1,J1), L_DEG(J1), &
     &                             J2, TIM_ARG )
            RD(IELM1) = BSPL_DER ( L_ERM(J1), TIM_ARR(1,J1), L_DEG(J1), &
     &                             J2, TIM_ARG )
!
! --------- NB: We use scaling factor for RQ in order to avoid numerical
! --------- underflow wiht manipulating with numbers by modulo less than 1.D-38
!
            RQ(IELM1) = BSPL_DR2 ( L_ERM(J1), TIM_ARR(1,J1), L_DEG(J1), &
     &                             J2, TIM_ARG )*SCL_RQ
!
! --------- Build the covariance matrix of dimension (L_DEG+1,L_DEG+1)
!
! --------- First, take care of the diagonal element...
!
            COV(LOCS(IELM1,IELM1)) = ERR_ARR(J2,J1)**2
            IF ( J2 .LT. KNOT ) THEN
                 IELM2 = IELM1
                 DO 430 J3=J2+1,KNOT
!
! ----------------- ... and then of the off-diagonal terms
!
                    IELM2 = IELM2 + 1 ! index of the second covariance matrix element
                    COV(LOCS(IELM1,IELM2)) = COV_ARR(J2,IELM2-IELM1,J1)
 430             CONTINUE 
            END IF
 420     CONTINUE
!
! ------ Compute the Earth orientation vector
!
         CALL MUL_MV_SV_V ( L_DEG(J1)+1, COV, L_DEG(J1)+1, RH, &
     &                      L_DEG(J1)+1, VEC, -3 )
         ERM_VAL(J1) = DP_VV_V ( L_DEG(J1)+1, RH, SPL_ARR(KNOT-L_DEG(J1),J1) )
         IF ( DP_VV_V ( L_DEG(J1)+1, RH, VEC ) < 0.0D0 ) THEN
              CALL ERR_LOG ( 8723, IUER, 'VTD_GET_ERM', 'Trap of internal '// &
     &            'control: negative sigma' )
              RETURN 
         END IF
!
! ------ Compute the time derivative of the Earth orientation vector
!
         ERM_ERR(J1) = DSQRT ( DP_VV_V ( L_DEG(J1)+1, RH, VEC ) )
!
! ------ Compute the first time derivative of the Earth orientation vector
!
         CALL MUL_MV_SV_V ( L_DEG(J1)+1, COV, L_DEG(J1)+1, RD, &
     &                      L_DEG(J1)+1, VEC, -3 )
         ERM_DER(J1) = DP_VV_V ( L_DEG(J1)+1, RD, SPL_ARR(KNOT-L_DEG(J1),J1) )
         IF ( DP_VV_V ( L_DEG(J1)+1, RD, VEC ) < 0.0D0 ) THEN
              CALL ERR_LOG ( 8724, IUER, 'VTD_GET_ERM', 'Trap of internal '// &
     &            'control: negative sigma' )
              RETURN 
         END IF
!
! ------ ... and its formtal uncertainty
!
         ERM_ER1(J1) = DSQRT ( DP_VV_V ( L_DEG(J1)+1, RD, VEC ) )
!
! ------ Compute the second time derivative of the Earth orientation vector
!
         CALL MUL_MV_SV_V ( L_DEG(J1)+1, COV, L_DEG(J1)+1, RQ, &
     &                      L_DEG(J1)+1, VEC, -3 )
         ERM_DR2(J1) = DP_VV_V ( L_DEG(J1)+1, RQ, SPL_ARR(KNOT-L_DEG(J1),J1) )/ &
     &                           SCL_RQ
         IF ( DP_VV_V ( L_DEG(J1)+1, RQ, VEC ) < 0.0D0 ) THEN
              CALL ERR_LOG ( 8725, IUER, 'VTD_GET_ERM', 'Trap of internal '// &
     &            'control: negative sigma' )
              RETURN 
         END IF
!
! ------ ... and its formal uncertainty
!
         ERM_ER2(J1) = DSQRT ( DP_VV_V ( L_DEG(J1)+1, RQ, VEC ) )/ SCL_RQ
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GET_ERM  !#!#
