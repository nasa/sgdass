      SUBROUTINE CLOPA ( N_OBS, DBOBJ, OBSBAS, RES, PAMBI, PAMB_BAND, &
     &                   PAMB_VER, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine   CLOPA  applies CLOPA (CLOsure Permanent Ambiguiity)    *
! *   algorithm for elimintion permanent phase delay ambiguity for       *
! *   chosen band for chosen session.                                    *
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
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! * PAMB_BAND ( INTEGER*4 ) -- Switch of the band:                       *
! *                            PAMB__XBAND -- X-band data will be        *
! *                                        objected by OSCRO algorithm;  *
! *                            PAMB__SBAND -- S-band data will be        *
! *                                        objected by OSCRO algorithm.  *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity level.                          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      KAMB ( INTEGER*4 ) -- Number of ambiguities which were changed. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
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
! *  ###  15-MAR-98      CLOPA     v1.4  (c)  L. Petrov  08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  PAMB_VER, N_OBS, PAMB_BAND, KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      INTEGER*4  J1, J2, J3, J4, J5, NB, IBS, K_OBS, KOBS_MIN, IP, IER, &
     &           IST1, IST2, NST1, NST2
      INTEGER*4  L_TRI, LIS_TRI(3,MG_TRI), JMP_BAS(MG_BAS), IND_STA(MG_STA)
      REAL*8     SH, DR, CLS
      REAL*8     PAMB_SP, EPS_SP
      REAL*8     SH_BAS(MO_BAS), SH_STA(MO_STA)
      REAL*8     TIM_ARR(MO_OBS), VAL_ARR(MO_OBS), WEI_ARR(MO_OBS), &
     &           ERR_ARR(MO_OBS)
      REAL*8     RATE_CALC, RATE_OBS_X, RATE_OBS_S, RATE_ERR_X, RATE_ERR_S, &
     &           COR_RATE, ADDERR_RATE, TAUSB_OBS_X, TAUSB_OBS_S, &
     &           TAUSB_ERR_X, TAUSB_ERR_S, FREQ_RATE_X, FREQ_RATE_S, &
     &           RATE_OC, TAU_E, RATE_E, TAU_GR_OC, TAU_PH_OC
      INTEGER*2  AUTO_SUP, USER_SUP, USER_REC
      PARAMETER  ( KOBS_MIN = 4 )
      PARAMETER  ( EPS_SP   = 1.D-6 )
      INTEGER*2  IDATYP_PH
      LOGICAL*4  FL_USED, FL_URPH 
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: NSTBA, IFIND_PL
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
      KAMB = 0
!
      IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
           IDATYP_PH = PX_GXS__DTP
         ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
           IDATYP_PH = PS_GXS__DTP
      END IF
!
! --- Cycle over all used baselines
!
      DO 410 J1=1,DBOBJ%U_BAS
         K_OBS = 0
!
! ------ Cycle over all observations
!
         DO 420 J2=1,DBOBJ%L_OBS
            NB = NSTBA ( INT4(OBSBAS(J2)%ISITE(1)), INT4(OBSBAS(J2)%ISITE(2)) )
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
            IF ( IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB ) .EQ. J1  .AND.  &
     &                 FL_USED                                        .AND.  &
     &           .NOT. FL_URPH                                        )   THEN
!
                 K_OBS = K_OBS + 1
!
! -------------- 1. Make G_PXS o-c
!
                 CALL MAKE_OC ( OBSBAS(J2)%TAU_C, RATE_CALC, &
     &                OBSBAS(J2)%TAU_COR, COR_RATE, OBSBAS(J2)%TAUGR_ERR_COR, &
     &                OBSBAS(J2)%TAUPH_ERR_COR, ADDERR_RATE, &
     &                OBSBAS(J2)%TAUGR_OBS, OBSBAS(J2)%TAUGR_OBS_OPP, &
     &                OBSBAS(J2)%TAUPH_OBS, OBSBAS(J2)%TAUPH_OBS_OPP, &
     &                TAUSB_OBS_X, TAUSB_OBS_S, &
     &                OBSBAS(J2)%TAUGR_ERR, OBSBAS(J2)%TAUGR_ERR_OPP, &
     &                OBSBAS(J2)%TAUPH_ERR, OBSBAS(J2)%TAUPH_ERR_OPP, &
     &                TAUSB_ERR_X, TAUSB_ERR_S, &
     &                RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &                OBSBAS(J2)%FREQ_IONO_GR, OBSBAS(J2)%FREQ_IONO_GR_OPP, &
     &                OBSBAS(J2)%FREQ_IONO_PH, OBSBAS(J2)%FREQ_IONO_PH_OPP, &
     &                FREQ_RATE_X, FREQ_RATE_S, AUTO_SUP, &
     &                USER_SUP, USER_REC, G_GXS__DTP, OPP_STATUS, &
     &                PAMB_STATUS, TAU_GR_OC, RATE_OC, TAU_E, RATE_E )
!
! -------------- 2. Make P_PXS o-c
!
                 CALL MAKE_OC ( OBSBAS(J2)%TAU_C, RATE_CALC, &
     &                OBSBAS(J2)%TAU_COR, COR_RATE, OBSBAS(J2)%TAUGR_ERR_COR, &
     &                OBSBAS(J2)%TAUPH_ERR_COR, ADDERR_RATE, &
     &                OBSBAS(J2)%TAUGR_OBS, OBSBAS(J2)%TAUGR_OBS_OPP, &
     &                OBSBAS(J2)%TAUPH_OBS, OBSBAS(J2)%TAUPH_OBS_OPP, &
     &                TAUSB_OBS_X, TAUSB_OBS_S, &
     &                OBSBAS(J2)%TAUGR_ERR, OBSBAS(J2)%TAUGR_ERR_OPP, &
     &                OBSBAS(J2)%TAUPH_ERR, OBSBAS(J2)%TAUPH_ERR_OPP, &
     &                TAUSB_ERR_X, TAUSB_ERR_S, &
     &                RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &                OBSBAS(J2)%FREQ_IONO_GR, OBSBAS(J2)%FREQ_IONO_GR_OPP, &
     &                OBSBAS(J2)%FREQ_IONO_PH, OBSBAS(J2)%FREQ_IONO_PH_OPP, &
     &                FREQ_RATE_X, FREQ_RATE_S, AUTO_SUP, &
     &                USER_SUP, USER_REC, IDATYP_PH, OPP_STATUS, &
     &                PAMB_STATUS, TAU_PH_OC, RATE_OC, TAU_E, RATE_E )
!
! -------------- Getting appropriate constant of phase delay ambiguities
! -------------- and correct phase delay o-c for additional
! -------------- number of ambiguities
!
                 IF ( PAMB_BAND .EQ. PAMB__XBAND ) THEN
                      PAMB_SP = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH
                   ELSE IF ( PAMB_BAND .EQ. PAMB__SBAND ) THEN
                      PAMB_SP = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH_OPP
                 END IF
!
! -------------- Adding the point to the array of arguments, values and weights
! -------------- of the differences between group delay and phase delays
! -------------- for the J1-th baseline
!
                 TIM_ARR(K_OBS) = RES(J2)%TT
                 VAL_ARR(K_OBS) = TAU_PH_OC - TAU_GR_OC
                 WEI_ARR(K_OBS) = RES(J2)%WEI_DEL
                 ERR_ARR(K_OBS) = 1.D0/RES(J2)%WEI_DEL
            END IF
 420     CONTINUE
!
! ------ Calculation of the drift and shift for the differences phase-group
! ------ delays and storing the shift.
!
         IF ( K_OBS .GT. KOBS_MIN ) THEN
              CALL REGRW8 ( K_OBS, TIM_ARR, VAL_ARR, WEI_ARR, %VAL(0), &
     &                      DR, SH, -3 )
              SH_BAS(J1) = SH
!
              IF ( PAMB_VER .GE. 2 ) THEN
                   WRITE (  6, 170 ) J1, DBOBJ%C_BAS(J1), SH/PAMB_SP, DR/PAMB_SP
                   WRITE ( 23, 170 ) J1, DBOBJ%C_BAS(J1), SH/PAMB_SP, DR/PAMB_SP
 170               FORMAT ( 1X, I3,') ',A,'  shift = ',1PE15.7, &
     &                         ' drift = ',1pe15.7 )
              END IF
         END IF
 410  CONTINUE
!
! --- Calculation the number of closed trinagles
!
      L_TRI = (DBOBJ%U_STA - 2) * (DBOBJ%U_STA - 1) / 2
      IF ( DBOBJ%U_STA .LT. 3 ) L_TRI = 0
!
! --- Creating the list of closed triangles (if there are any)
!
      IF ( L_TRI .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL TRI_GRP  ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%U_BAS, &
     &                     DBOBJ%UIS_BAS, MG_TRI, L_TRI, LIS_TRI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5221, IUER, 'CLOPA', 'Error during building '// &
     &              'list of closed triangles' )
                RETURN
           END IF
      END IF
!
! --- Test: Do we have a situation, when the remnant from closure
! ---       of triangles for parameters clock shift is a multiple of the
! ---       phase delay ambiguity spacing constant? If it is just the case
! ---       then we should redistribute additional clock jumps between
! ---       baselines in order to reduce the remnants from closure of triangles
! ---       to the value less than phase delay ambiguity spacing constant.
! ---       Permanent phase delay ambiguity for each baseline is determined.
!
      IF ( PAMB_VER .GE. 2 ) THEN
                  WRITE ( 6, * ) ' idatyp_ph=',idatyp_ph
           WRITE (  6, 180 ) L_TRI, PAMB_SP
           WRITE ( 23, 180 ) L_TRI, PAMB_SP
 180       FORMAT ( 1X,' L_TRI=',I6,'  PAMB_SP =',1PE15.7,' sec' )
      END IF
      IF ( L_TRI .NE. 0 ) THEN
!
! -------- Clearing array of scanned stations
!
           CALL NOUT ( 4*MO_STA, IND_STA )
           DO 430 J3=1,DBOBJ%U_STA
!
! ------------ Search of the first not-scanned station
!
              IF ( IND_STA(J3) .EQ. 0 ) THEN
!
! ---------------- We take this station as a fidicial.
!
                   SH_STA(J3)=0.0D0
                   IND_STA(J3) = 1 ! Set up the flag "scanned"
!
! ---------------- And now scan all baselines
!
                   DO 440 J4=1,DBOBJ%U_BAS
!
! ------------------- Find IST1, IST2 -- indeces stations of J4-th baseline
! ------------------- in the list of stations
!
                      CALL NBAST ( DBOBJ%UIS_BAS(J4), NST1, NST2 )
                      IST1 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, NST1 )
                      IST2 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, NST2 )
!
! ------------------- Test: have they been already scanned?
!
                      IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                     IND_STA(IST2) .EQ. 0        ) THEN
!
! ------------------------ The first station has been scanned but
! ------------------------ the second -- has not. Determine the clock shift
! ------------------------ for the second one.
!
                           SH_STA(IST2) = SH_BAS(J4) + SH_STA(IST1)
                           IND_STA(IST2) = 1 ! Set up the flag "scanned"
                           JMP_BAS(J4)   = 0
                         ELSE IF ( IND_STA(IST1) .EQ. 0  .AND. &
     &                             IND_STA(IST2) .EQ. 1        ) THEN
!
! ------------------------ The second station has been scanned but
! ------------------------ the first -- has not. Determine the clock shift
! ------------------------ for the unscanned station.
!
                           SH_STA(IST1) = SH_STA(IST2) - SH_BAS(J4)
                           IND_STA(IST1) = 1 ! Set up the flag "scanned"
                           JMP_BAS(J4)   = 0
                         ELSE IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                             IND_STA(IST2) .EQ. 1        ) THEN
!
! ------------------------ Both stations has been already scanned. Find
! ------------------------ a clock shift on the base of scanned stations and
! ------------------------ compare it with the clock shift obtained for this
! ------------------------ baseline. Entire part of the division is just the
! ------------------------ constant group delay ambiguity for all observations
! ------------------------ of the baseline which we are loooking for.
!
                           IF ( DABS ( ( SH_BAS(J4) - &
     &                          ( SH_STA(IST2) - SH_STA(IST1) ) )/ PAMB_SP ) < 2.0D9 ) THEN
!
                                JMP_BAS(J4) = NINT( ( SH_BAS(J4) - &
     &                              ( SH_STA(IST2) - SH_STA(IST1) ) )/ PAMB_SP )
                              ELSE 
                                JMP_BAS(J4) = 0.0D0
                           END IF
!
! ------------------------ Calculcate the residual error of the closure
! ------------------------ for clock shifts
!
                           CLS = ( SH_BAS(J4) - &
     &                           ( SH_STA(IST2) - SH_STA(IST1) ) ) - &
     &                             JMP_BAS(J4)*PAMB_SP
!
! ------------------------ Subtract the permanent group delay ambiguity
! ------------------------ found from the clock shift for this baseline
!
                           SH_BAS(J4) = SH_BAS(J4) - JMP_BAS(J4)*PAMB_SP
!
                           IF ( PAMB_VER .GE. 2 ) THEN
                                IP = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                                          DBOBJ%UIS_BAS(J4) )
                                WRITE (  6, 190 ) BAND_STR(PAMB_BAND), &
     &                                 DBOBJ%C_BAS(IP), CLS*1.D12, JMP_BAS(J4)
                                WRITE ( 23, 190 ) BAND_STR(PAMB_BAND), &
     &                                 DBOBJ%C_BAS(IP), CLS*1.D12, JMP_BAS(J4)
  190                           FORMAT ( 1X,'&&&  CLOPA  ',A,'  ',A, &
     &                                 ': CLS = ',F12.2,' psec JMP=',I10 )
                             END IF
                       END IF
  440              CONTINUE
              END IF
  430      CONTINUE
!
! -------- Correct vector O-C for tau and the numher of ambiguities
! -------- for the permanent phase delay ambiguity for each baseline
!
           DO 450 J5=1,DBOBJ%L_OBS
              IBS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &                         NSTBA( INT4(OBSBAS(J5)%ISITE(1)), &
     &                                INT4(OBSBAS(J5)%ISITE(2)) ) )
              IF ( IBS .GT. 0  .AND.  PAMB_BAND .EQ. PAMB__XBAND ) THEN
!
! ---------------- X-band
!
                   CALL AMB_UPDATE ( PAMB__XBAND, -JMP_BAS(IBS), OBSBAS(J5), &
     &                               PAMBI(J5), -3 )
                   IF ( JMP_BAS(IBS) .NE. 0 ) THEN
                        KAMB = KAMB + 1
                   END IF
                ELSE IF ( IBS .GT. 0  .AND.  PAMB_BAND .EQ. PAMB__SBAND ) THEN
!
! ---------------- S-band
!
                   CALL AMB_UPDATE ( PAMB__SBAND, -JMP_BAS(IBS), OBSBAS(J5), &
     &                               PAMBI(J5), -3 )
                   IF ( JMP_BAS(IBS) .NE. 0 ) THEN
                        KAMB = KAMB + 1
                   END IF
              END IF
  450      CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CLOPA  #!#
