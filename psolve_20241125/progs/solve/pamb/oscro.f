      SUBROUTINE OSCRO_SES ( N_OBS, DBOBJ, OBSBAS, RES, PAMBI, PAMB_BAND, &
     &                       KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  OSCRO_SES  applies OSCRO (optimal scrolling) algorithm  *
! *   for phase delay ambiguity resolution for chosen band for chosen    *
! *   session.                                                           *
! *                                                                      *
! *     Optimal scrolling is to find additional shift to postfit phase   *
! *   delay residuals produced from group delay residuals whcih after    *
! *   elimination additional ambiguities caused by that shift would      *
! *   provide minimal w.r.m.s of the residuals for each baselines.       *
! *                                                                      *
! *     Scrolling constant is calcualted for each baseline separately.   *
! *   It is obtained by mere trying. First probe vaules are the values   *
! *   uniformly distributed in the range [0, pamb_sp], where pamb_sp is  *
! *   phase delay ambiguity spacing. Then the range is narrowing.        *
! *                                                                      *
! *     Phase delay rediuals and ambiguity values are updated in fields  *
! *   of PAMBI data structure.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! * PAMB_BAND ( INTEGER*4 ) -- Switch of the band:                       *
! *                            PAMB__XBAND -- X-band data will be        *
! *                                        objected by OSCRO algorithm;  *
! *                            PAMB__SBAND -- S-band data will be        *
! *                                        objected by OSCRO algorithm.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      KAMB ( INTEGER*4 ) -- Number of ambiguities which were changed. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  18-FEB-98    OSCRO_SES   v2.2  (c)  L. Petrov 08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  N_OBS, PAMB_BAND, KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      INTEGER*4  J1, J2, J3, J4, NB, IB, K_OBS, KOBS_MIN, &
     &           IND_OBS(MO_OBS), IAMB_ARR(MO_OBS), IAMB_ADD
      REAL*8     PAMB_SP, EPS_SP
      REAL*8     PSF_SHIFT(MO_BAS), &
     &           PSF_WRMS(MO_BAS), TIM_ARR(MO_OBS), RES_ARR(MO_OBS), &
     &           WEI_ARR(MO_OBS),  ARR_TMP(MO_OBS), VAL
      PARAMETER  ( KOBS_MIN = 4 )
      PARAMETER  ( EPS_SP   = 1.D-6 )
      LOGICAL*4  FL_USED, FL_URPH 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: NSTBA, IFIND_PL
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
      KAMB = 0
!
! --- First cycle over all used baselines, over all used observations.
! --- We gather information about phase-group residuals for each baselines
!
      DO 410 J1=1,DBOBJ%U_BAS
         K_OBS = 0
         PSF_SHIFT(J1) = 0.0D0
!
! ------ Cycle over all observations
!
         DO 420 J2=1,DBOBJ%L_OBS
            NB = NSTBA ( INT4(OBSBAS(J2)%ISITE(1)), INT4(OBSBAS(J2)%ISITE(2)) )
!
            IF ( SUPMET == SUPMET__META ) THEN
                 FL_USED = META_SUPR_INQ ( OBSBAS(J2)%AUTO_SUP, &
     &                                     OBSBAS(J2)%USER_SUP, &
     &                                     OBSBAS(J2)%USER_REC, &
     &                                     USED__SPS )
                 FL_URPH = BTEST ( OBSBAS(J2)%AUTO_SUP, INT4(WPAS__SPS) )
               ELSE
                 FL_USED = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), OBSBAS(J2)%UACSUP, &
     &                                USED__SPS )
                 FL_URPH = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), OBSBAS(J2)%UACSUP, &
     &                                URPH__SPS ) 
            END IF
!
            IF ( IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB ) .EQ. J1  .AND. &
     &                 FL_USED                                        .AND. &
     &           .NOT. FL_URPH ) THEN
!
! -------------- Observations of J1-th baseline which were not eliminated will
! -------------- be undergone by optimal scrolling. We put time, residual,
! -------------- its weight and its ambiguity. We keep index of the observation
! -------------- in cross reference array IND_OBS
!
                 K_OBS = K_OBS+1
                 IND_OBS(K_OBS) = J2
                 IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
                    IF ( K_OBS .EQ. 1 ) THEN
!
! ---------------------- It was the first observation? Set ambiguity spacing!
!
                         PAMB_SP = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH
                       ELSE
                         IF ( DABS(PAMB_SP - 1.D0/OBSBAS(J2)%FREQ_OBSV_PH) .GT. &
     &                      PAMB_SP*EPS_SP ) THEN
!
! ------------------------- Ambiguities turned out to be different. OSCRO is not
! ------------------------- able to support such a case.
!
                            WRITE ( 6, * ) ' J2=',J2,' SCA=',OBSBAS(J2)%IND_SCA, &
     &                             ' PAMB_SP = ',PAMB_SP,' PAMB_SP_NEW=', &
     &                             1.D0/OBSBAS(J2)%FREQ_OBSV_PH
                            CALL ERR_LOG ( 7401, IUER, 'OSCRO_SES', &
     &                          'Inconsistent phase deleay ambiguity '// &
     &                          'constant for X-band' )
                            RETURN
                         END IF
                    END IF
!
! ---------------- S-band
!
                   ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
                    IF ( K_OBS .EQ. 1 ) THEN
                         PAMB_SP = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH_OPP
                       ELSE
                         IF ( DABS(PAMB_SP - 1.D0/OBSBAS(J2)%FREQ_OBSV_PH_OPP) &
     &                     .GT. PAMB_SP*EPS_SP ) THEN
                            WRITE ( 6, * ) ' J2=',J2,' SCA=',OBSBAS(J2)%IND_SCA, &
     &                             ' PAMB_SP = ',PAMB_SP,' PAMB_SP_NEW=', &
     &                             1.D0/OBSBAS(J2)%FREQ_OBSV_PH_OPP
                            CALL ERR_LOG ( 7402, IUER, 'OSCRO_SES', &
     &                          'Inconsistent phase delay ambiguity '// &
     &                          'constant for S-band' )
                            RETURN
                         END IF
                    END IF
                 END IF
!
! -------------- Putting time and weights in temporary arrays
!
                 TIM_ARR(K_OBS) = RES(J2)%TT
                 WEI_ARR(K_OBS) = RES(J2)%WEI_DEL
!
! -------------- Putting residuals and ambiguities in temporary arrrays
!
                 IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
                       RES_ARR(K_OBS) = PAMBI(J2)%RES_PX_GXS
                      IAMB_ARR(K_OBS) = 0
                    ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
                       RES_ARR(K_OBS) = PAMBI(J2)%RES_PS_GXS
                      IAMB_ARR(K_OBS) = 0
                 END IF
             END IF
 420     CONTINUE
         IF ( K_OBS .GT. KOBS_MIN ) THEN
!
! ----------- Now applyong OSCRO algorithm to the J1-th baseline. Algorithm
! ----------- will not be applied for the baselines which had too few
! ----------- observation
!
              CALL OSCRO_BAS ( K_OBS, PAMB_SP, TIM_ARR, RES_ARR, WEI_ARR, &
     &                         IAMB_ARR, ARR_TMP, PSF_SHIFT(J1), PSF_WRMS(J1) )
!
! ----------- Now we update all used observations for new ambiguities
!
              DO 430 J3=1,K_OBS
                 CALL AMB_UPDATE ( PAMB_BAND, IAMB_ARR(J3), OBSBAS(IND_OBS(J3)), &
     &                             PAMBI(IND_OBS(J3)), -3 )
                 IF ( IAMB_ARR(J3) .NE. 0 ) KAMB = KAMB + 1
 430          CONTINUE
         END IF
 410  CONTINUE
!
! --- No we should care about not used observations on selected baslines
!
      DO 440 J4=1,N_OBS
         NB = NSTBA ( INT4(OBSBAS(J4)%ISITE(1)), INT4(OBSBAS(J4)%ISITE(2)) )
         IB = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB )
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J4)%AUTO_SUP, &
     &                                  OBSBAS(J4)%USER_SUP, &
     &                                  OBSBAS(J4)%USER_REC, &
     &                                  USED__SPS )
              FL_URPH = BTEST ( OBSBAS(J4)%AUTO_SUP, INT4(WPAS__SPS) )
            ELSE
              FL_USED = SUPR_INQ ( OBSBAS(J4)%SUPSTAT(1), OBSBAS(J4)%UACSUP, &
     &                             USED__SPS )
              FL_URPH = SUPR_INQ ( OBSBAS(J4)%SUPSTAT(1), OBSBAS(J4)%UACSUP, &
     &                             URPH__SPS ) 
         END IF
         IF ( .NOT. FL_USED  .AND.  &
     &        .NOT. FL_URPH  .AND.  &
     &        IB .GE. 1             ) THEN
!
! ----------- Apply correction for optimal scrolling for the data eliminated
! ----------- from solution. Optimal scrolling ignors these data fro
! ----------- calculation optimal shift but nevertheless we apply optimal
! ----------- scrolling correction to these data in order to have an
! ----------- opportunity to use them in future
!
              IAMB_ADD = 0
!
! ----------- Get value of the residual corrected for optimal scrolling
! ----------- and ambiguity spacing
!
              IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
                   VAL = PAMBI(J4)%RES_PX_GXS + PSF_SHIFT(IB)
                   PAMB_SP = 1.D0/OBSBAS(J4)%FREQ_OBSV_PH
                ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
                   VAL = PAMBI(J4)%RES_PS_GXS + PSF_SHIFT(IB)
                   PAMB_SP = 1.D0/OBSBAS(J4)%FREQ_OBSV_PH_OPP
              END IF
!
! ----------- First scrolling
!
              IF ( VAL .GT. PAMB_SP/2 ) THEN
                   VAL      = VAL - PAMB_SP
                   IAMB_ADD = IAMB_ADD - 1
                ELSE IF ( VAL .LT. -PAMB_SP/2 ) THEN
                   VAL      = VAL + PAMB_SP
                   IAMB_ADD = IAMB_ADD + 1
              END IF
!
! ----------- Second scrolling
!
              IF ( VAL .GT. PAMB_SP/2 ) THEN
                   VAL      = VAL - PAMB_SP
                   IAMB_ADD = IAMB_ADD - 1
                ELSE IF ( VAL .LT. -PAMB_SP/2 ) THEN
                   VAL      = VAL + PAMB_SP
                   IAMB_ADD = IAMB_ADD + 1
              END IF
!
! ----------- Put updated residual and ambiguity back
!
              CALL AMB_UPDATE ( PAMB_BAND, IAMB_ADD, OBSBAS(J4), PAMBI(J4), -3 )
              IF ( IAMB_ADD .NE. 0 ) KAMB = KAMB + 1
              IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
                   PAMBI(J4)%RES_PX_GXS = VAL
                ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
                   PAMBI(J4)%RES_PS_GXS = VAL
              END IF
         END IF
 440  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OSCRO_SES  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE OSCRO_BAS ( NB_OBS, PAMB_SP, TT, PSF_DEL, WEI_DEL, NPHAMB, &
     &                       ARR_TMP, PSF_SHIFT, PSF_WRMS )
! ************************************************************************
! *                                                                      *
! *   Routine  OSCRO_BAS  finds the optimal scrolling shift for the      *
! *   phase delay resuiduals. After adding that shift and fixing         *
! *   ambiguities in [-0.5, 0.5] interval w.r.m.s. would be minimal.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    NB_OBS ( INTEGER*4 ) -- Number of used observations in the        *
! *                            baseline under consideration.             *
! *   PAMB_SP ( REAL*8    ) -- Constant of phase delay spacings.         *
! *        TT ( REAL*8    ) -- Array of the time of observation in       *
! *                            second, w.r.t. the first observation of   *
! *                            the session.                              *
! *   WEI_DEL ( REAL*8    ) -- Array of weights applied to phase deleay  *
! *                            residuals ( in 1/sec ).                   *
! *   ARR_TMP ( REAL*8    ) -- Working array with length NB_OBS.         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * PSF_SHIFT ( REAL*8    ) -- Shift which should be added to all        *
! *                            observations of the baseline under        *
! *                            consideration to get mimnal w.r.m.s.      *
! *                            of phase delay residuals.                 *
! *  PSF_WRMS ( REAL*8    ) -- w.r.m.s. residual after applying optimal  *
! *                            scrolling.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   PSF_DEL ( REAL*8    ) -- Array of phase delay residual (in sec).   *
! *   NPHAMB  ( INTEGER*4 ) -- Array of phase delay ambiguities. NB:     *
! *                            ambiguities are to be applied to observed *
! *                            raw phase delays, not to phase delay      *
! *                            residuals: they have allready been        *
! *                            applied to produce phase delay residuals. *
! *                                                                      *
! *  ###  01-DEC-97    OSCRO_BAS   v2.1  (c)  L. Petrov  27-FEB-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NB_OBS, NPHAMB(NB_OBS)
      REAL*8     PAMB_SP, TT(NB_OBS), PSF_DEL(NB_OBS), WEI_DEL(NB_OBS), &
     &           ARR_TMP(NB_OBS), PSF_SHIFT, PSF_WRMS
      REAL*8     AV, DS
      INTEGER*4  MPART, MREPT, IMIN, NZ, J1, J2, J3, J4
      PARAMETER  ( MPART = 16, MREPT = 8 )
      REAL*8     PSF_D, PSF_U, DR, SH, PSF_DNEW, PSF_UNEW, DS_MIN, &
     &           STEP, ACC2, WAC2
!
! --- Initial range for probe shift
!
      PSF_D = 0.0
      PSF_U = PAMB_SP
!
! --- We make MRETP attempt to find optimal shift for scrolling
!
      DO 410 J1=1,MREPT
!
! ------ Setting a step for probe shift
!
         STEP = (PSF_U - PSF_D)/(MPART-1)
         DO 420 J2=1,MPART
            PSF_SHIFT = PSF_D + (J2-1)*STEP
!
! --------- Putting delay with applied probe shift (and scrolling) to array
! --------- ARR_TMP
!
            DO 430 J3=1,NB_OBS
!
! ------------ Addition...
!
               ARR_TMP(J3) = PSF_DEL(J3) + PSF_SHIFT
!
! ------------ First scrolling
!
               IF ( ARR_TMP(J3) .GT. PAMB_SP/2 ) THEN
                    ARR_TMP(J3) = ARR_TMP(J3) - PAMB_SP
                 ELSE IF ( ARR_TMP(J3) .LT. -PAMB_SP/2 ) THEN
                    ARR_TMP(J3) = ARR_TMP(J3) + PAMB_SP
               END IF
!
! ------------ Second scrolling
!
               IF ( ARR_TMP(J3) .GT. PAMB_SP/2 ) THEN
                    ARR_TMP(J3) = ARR_TMP(J3) - PAMB_SP
                 ELSE IF ( ARR_TMP(J3) .LT. -PAMB_SP/2 ) THEN
                    ARR_TMP(J3) = ARR_TMP(J3) + PAMB_SP
               END IF
 430        CONTINUE
!
! --------- Calculation of the drift and shift for delays with applied probe
! --------- shift
!
            CALL REGRW8    ( NB_OBS, TT, ARR_TMP, WEI_DEL, %VAL(0), DR, SH, -3 )
!
! --------- Calculation dispersion of the deviation residuals from the lines
!
            CALL DISP_WTR8 ( NB_OBS, TT, ARR_TMP, WEI_DEL, DR, SH, %VAL(0), DS, &
     &                       NZ, -3 )
!
! --------- Update of index of trial shift which produce minimal dispersion
!
            IF ( J2 .EQ. 1 ) THEN
                 IMIN = 1
                 DS_MIN = DS
              ELSE
                 IF ( DS .LT. DS_MIN ) THEN
                      IMIN = J2
                      DS_MIN = DS
                 END IF
            END IF
 420     CONTINUE
!
! ------ We narrow the range of probing shift. The bounbdaries of the
! ------ new range will be one step down the optinal shift obtained at the
! ------ previous operation and one step up
!
         PSF_DNEW = PSF_D + (IMIN-2)*STEP
         PSF_UNEW = PSF_D + IMIN*STEP
!
! ------ Setting new boundary for the search interval
!
         PSF_D = PSF_DNEW
         PSF_U = PSF_UNEW
 410  CONTINUE
!
! --- Setting the final value of the shift which produce minimal dispersion
!
      PSF_SHIFT = ( PSF_DNEW + PSF_UNEW ) / 2.D0
      DS = DS_MIN
!
! --- Calculation new w.r.m.s. for optimally shifted and scrolled residuals
!
      ACC2 = 0.D0
      WAC2 = 0.D0
      DO 440 J4=1,NB_OBS
!
! ------ Updating delays and number of ambiguities
!
         PSF_DEL(J4) = PSF_DEL(J4) + PSF_SHIFT
!
! ------ First scrolling
!
         IF ( PSF_DEL(J4) .GT. PAMB_SP/2 ) THEN
              PSF_DEL(J4) = PSF_DEL(J4) - PAMB_SP
              NPHAMB(J4)  = NPHAMB(J4)  - 1
            ELSE IF ( PSF_DEL(J4) .LT. -PAMB_SP/2 ) THEN
              PSF_DEL(J4) = PSF_DEL(J4) + PAMB_SP
              NPHAMB(J4)  = NPHAMB(J4)  + 1
         END IF
!
! ------ Second scrolling
!
         IF ( PSF_DEL(J4) .GT. PAMB_SP/2 ) THEN
              PSF_DEL(J4) = PSF_DEL(J4) - PAMB_SP
              NPHAMB(J4)  = NPHAMB(J4)  - 1
            ELSE IF ( PSF_DEL(J4) .LT. -PAMB_SP/2 ) THEN
              PSF_DEL(J4) = PSF_DEL(J4) + PAMB_SP
              NPHAMB(J4)  = NPHAMB(J4)  + 1
         END IF
!
! ------ Accumulators update
!
         ACC2 = ACC2 + ( PSF_DEL(J4)*WEI_DEL(J4) ) **2
         WAC2 = WAC2 + ( WEI_DEL(J4)             ) **2
 440  CONTINUE
!
! --- Getting w.r.m.s.
!
      PSF_WRMS = SQRT ( ACC2 /WAC2 )
!
      RETURN
      END  !#!  OSCRO_BAS  #!#
