      SUBROUTINE RESID_ST ( THR_USE, CUT_USE, ELIM_THR, ELIM_CUT, ELIM_MSR, &
     &                      IOBS, N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  RESID_ST  calculates some statistics of postfit         *
! *   residuals and then finds the best candidate for                    *
! *   elimination/restoration. Statistics are being written into fields  *
! *   of RST data structure.                                             *
! *                                                                      *
! *     There is a possibility to bypass calculation of statistics       *
! *   section and search the best candidate outright. Field              *
! *   RST.FIRST_FIELD should be set as RST__MAXUP and RST.LAST_FILED     *
! *   should be set up as RST__STADONE. The latter field is set up by    *
! *   RSEID_ST. It is assumed that it is set only if statistics have     *
! *   been calculated already.                                           *
! *                                                                      *
! *     There is a possibility to not to calculate statistics anew, but  *
! *   make its update for the IOBS-th observation. Field RST.FIRST_FIELD *
! *   should be set as RST__STUPD, RST.LAST_FILED should be set up as    *
! *   RST__STADONE and IOBS .NE. 0. Then if IOBS > 0 statistics will be  *
! *   updated for adding IOBS-th observation, if IOBS < 0, statistics    *
! *   will be updated for subtracting -IOBS-th observation.              *
! *                                                                      *
! *   NB: Field RST.FIRST_FIELD is set up as RST__INIT after finding     *
! *   the best candidate.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  THR_USE ( LOGICAL*4 ) -- If .TRUE. then threshold criteria will be  *
! *                           used.                                      *
! *  CUT_USE ( LOGICAL*4 ) -- If .TRUE. then cutoff criteria will be     *
! *                           used. If both THR_USE and CUT_USE are      *
! *                           .FALSE. then best candidate for            *
! *                           elimination/restoration is found among     *
! *                           all observations.                          *
! * ELIM_THR ( REAL*8    ) -- Postfit threshold (sec). ELIM mode:        *
! *                           If used observation with postfit           *
! *                           residual exceeds ELIM_THR it marked as     *
! *                           outlier. MILE mode: if recoverable         *
! *                           observation has postfit residual less      *
! *                           than ELIM_THR it marked as candidate in    *
! *                           restoration.                               *
! * ELIM_CUT ( REAL*8    ) -- n-sigmas cutoff criterion. ELIM mode:      *
! *                           If used observation has normalized         *
! *                           postfit residual larger than ELIM_CUT it   *
! *                           marked as outlier. MILE mode: if           *
! *                           recoverable observation has normalized     *
! *                           postfit residual less than ELIM_THR it     *
! *                           marked as candidate in restoration.        *
! *  ELIM_MSR ( REAL*8   ) -- Maximal uncertainty. Observations with     *
! *                           formal uncertainty exceeding ELIM_MSR      *
! *                           are marked as outliers in ELIM mode and    *
! *                           are not eligible for restoration in MILE   *
! *                           mode. If ELIM_MSR < 1.D-12 then it is      *
! *                           ignored.                                   *
! *    IOBS  ( INTEGER*4 ) -- Index of the observation which should be   *
! *                           used for partial updating statistics.      *
! *                           If IOBS = 0 then statistics will be        *
! *                           updated completely.                        *
! *                           If IOBS > 0 then contribution of the       *
! *                              IOBS-th observation will be added.      *
! *                           If IOBS < 0 then contribution of the       *
! *                              IOBS-th observation will be subtracted. *
! *    N_OBS ( INTEGER*4 ) -- Number of observations.                    *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *   OBSSCA ( RECORD    ) -- Array of data structures which keeps       *
! *                           scan-dependent information about the       *
! *                           session.                                   *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
! *      RES ( RECORD    ) -- Array of data structures which keeps       *
! *                           information about residuals.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      RST ( RECORD    ) -- Data structure keeping the statistics of   *
! *                           postfit residuals.                         *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT) -- Universal error handler.                   *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-SEP-97     RESID_ST    v7.9 (c)  L. Petrov 18-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INTEGER*4  IOBS, N_OBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      LOGICAL*4  THR_USE, CUT_USE
      REAL*8     ELIM_THR, ELIM_CUT, ELIM_MSR
      CHARACTER  STR*80, STR1*80
      REAL*8     AV_ACC, AVB(MO_BAS), AVS(MO_SOU), PSF_DEL, PSF_DEL_ORIG, &
     &           AMB_SP, DEV, NDEV_G, NDEV_B
      REAL*8     SHR__AFL
      PARAMETER  ( SHR__AFL = 0.4 ) ! share of ambiguity spacing
      INTEGER*4  J1, J2, J3, J4, LEN_RST, NB, IB, NS, IS, NUMAMB_COR, IER
      INTEGER*4  KOBS, ISGN, LP_OBS, LPIS_OBS(MO_BAS), ISGN_OBS(MO_BAS)
      LOGICAL*4  ELIM_THRU, ELIM_CUT_G, ELIM_CUT_B, ELIM_UNCE, &
     &           ELIM_USE_B, ELIM_USE_G, &
     &           MILE_THRU, MILE_CUT_G, MILE_CUT_B, MILE_USE_B, MILE_USE_G
      LOGICAL*4  F_PRMT, FL_USED, FL_RECO
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL ::  SUPR_INQ, META_SUPR_INQ, AMBCHA_PERMIT, IS_R8_NAN
      INTEGER*4, EXTERNAL ::  I_LEN, NSTBA, IFIND_PL
!
! --- Check of consistency declared and actual length of data structures
!
      LEN_RST = (LOC(RST%LAST_FIELD) - LOC(RST%FIRST_FIELD)) + 4
      IF ( LEN_RST .NE. ML_RST ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_RST, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_RST, STR1 )
           CALL ERR_LOG ( 6751, IUER, 'RESID_ST', 'Internal error: '// &
     &         'Declared size of RST data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- In the case when code of operation "bypass collecting statisits, but
! --- find only the bes candidate for elimination/resotration" we bypass
! --- section of calculation of statistics
!
      IF ( RST%FIRST_FIELD  .EQ.  RST__MAXUP   .AND. &
     &     RST%LAST_FIELD   .EQ.  RST__STADONE       ) GOTO 810
!
      IF ( RST%FIRST_FIELD  .EQ.  RST__STUPD    .AND. &
     &     RST%LAST_FIELD   .EQ.  RST__STADONE  .AND.  IOBS .NE. 0 ) THEN
!
           KOBS = IABS(IOBS)
           ISGN = IOBS/IABS(IOBS)
!
           NB = NSTBA ( INT4(OBSBAS(KOBS)%ISITE(1)), &
     &                  INT4(OBSBAS(KOBS)%ISITE(2)) )
           IB = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB )
!
           NS = OBSBAS(KOBS)%IND_SCA
           IS = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, INT4(OBSSCA(NS)%ISTAR) )
!
! -------- Updating common accumulators
!
           RST%RMS_G  = SQRT ( RST%RMS_G**2 + ISGN*RES(KOBS)%PSF_DEL**2 )
           RST%WRMS_G = SQRT ( ( RST%WRMS_G**2*RST%WW_ACC + &
     &                     ISGN*(RES(KOBS)%PSF_DEL*RES(KOBS)%WEI_DEL)**2 )/ &
     &                          (RST%WW_ACC + ISGN*RES(KOBS)%WEI_DEL**2  ) )
           RST%WW_ACC = RST%WW_ACC + ISGN*RES(KOBS)%WEI_DEL**2
           RST%U_OBS  = RST%U_OBS  + ISGN*1
           RST%WDPR_G = DSQRT ( RST%WRMS_G**2*RST%WW_ACC/(RST%U_OBS-1) )
!
           IF ( IB .GT. 0 ) THEN
!
! ----------- Updating baseline-dependent accumulators
!
              RST%KU_BAS(IB) = RST%KU_BAS(IB) + ISGN*1
              IF ( RST%KU_BAS(IB) .GT. 0 ) THEN
                   RST%WRMS_B(IB) = SQRT ( ( RST%WRMS_B(IB)**2*RST%WB_ACC(IB) + &
     &                    ISGN*(RES(KOBS)%PSF_DEL*RES(KOBS)%WEI_DEL)**2 )/ &
     &                    ( RST%WB_ACC(IB) + ISGN*RES(KOBS)%WEI_DEL**2  ) )
                   RST%WB_ACC(IB) = RST%WB_ACC(IB) + ISGN*RES(KOBS)%WEI_DEL**2
                 ELSE ! = 0
                   RST%WRMS_B(IB) = RST%WRMS_G
                   RST%WB_ACC(IB) = 0.0D0
              END IF
!
              IF ( RST%KU_BAS(IB) .GT. 1 ) THEN
                   RST%WDPR_B(IB) = SQRT ( RST%WRMS_B(IB)**2*RST%WB_ACC(IB)/ &
     &                                   ( RST%KU_BAS(IB) - 1)               )
                 ELSE ! 0,1
                   RST%WDPR_B(IB) = RST%WDPR_G
              END IF
           END IF
!
           IF ( IS .GT. 0 ) THEN
!
! ----------- Updating source-dependent accumulators
!
              RST%KU_SOU(IS) = RST%KU_SOU(IS) + ISGN*1
              IF ( RST%KU_SOU(IS) .GT. 0 ) THEN
                   RST%WRMS_S(IS) = SQRT ( ( RST%WRMS_S(IS)**2*RST%WS_ACC(IS) + &
     &                    ISGN*(RES(KOBS)%PSF_DEL*RES(KOBS)%WEI_DEL)**2 )/ &
     &                    ( RST%WS_ACC(IS) + ISGN*RES(KOBS)%WEI_DEL**2  ) )
                   RST%WS_ACC(IS) = RST%WS_ACC(IS) + ISGN*RES(KOBS)%WEI_DEL**2
                 ELSE
                   RST%WRMS_S(IS) = RST%WRMS_G
                   RST%WS_ACC(IS) = 0.0D0
              END IF
!
              IF ( RST%KU_SOU(IS) .GT. 1 ) THEN
                   RST%WDPR_S(IS) = DSQRT ( RST%WRMS_S(IS)**2*RST%WS_ACC(IS)/ &
     &                                    ( RST%KU_SOU(IS) - 1)               )
                 ELSE
                   RST%WDPR_S(IS) = RST%WDPR_G
              END IF
           END IF
!
           GOTO 810
      END IF
!
 820  CONTINUE
!
! --- General initialization
!
      CALL NOUT ( ML_RST,   RST )  ! Total zeroing of RST
      CALL NOUT ( 8*MO_BAS, AVB )
      CALL NOUT ( 8*MO_SOU, AVS )
!
      RST%L_OBS  = DBOBJ%L_OBS
      RST%U_OBS  = DBOBJ%U_OBS
      RST%U_BAS  = DBOBJ%U_BAS
      RST%U_SOU  = DBOBJ%U_SOU
!
      RST%RMS_G  = 0.0D0
      RST%WRMS_G = 0.0D0
      RST%WW_ACC = 0.D0
      AV_ACC     = 0.D0
      RST%LAST_FIELD = RST__INIT
!
! --- First, calculate weighted dispersion for all observations and for each
! --- baseline separately. Only used observations will be taken into account
!
      DO 410 J1=1,N_OBS
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                            OBSBAS(J1)%USER_SUP, OBSBAS(J1)%USER_REC, &
     &                            USED__SPS ) 
              FL_RECO = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                            OBSBAS(J1)%USER_SUP, OBSBAS(J1)%USER_REC, &
     &                            RECO__SPS ) 
            ELSE 
              FL_USED = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                             USED__SPS ) 
              FL_RECO = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                             RECO__SPS ) 
         END IF
         IF ( FL_USED ) THEN
!
! ----------- Used observation: Updating common accumulators
!
              RST%RMS_G  = RST%RMS_G  +  RES(J1)%PSF_DEL**2
              RST%WRMS_G = RST%WRMS_G + (RES(J1)%PSF_DEL*RES(J1)%WEI_DEL)**2
              AV_ACC     = AV_ACC     +  RES(J1)%WEI_DEL
              RST%WW_ACC = RST%WW_ACC  + (RES(J1)%WEI_DEL)**2
              NB = NSTBA ( INT4(OBSBAS(J1)%ISITE(1)), &
     &                     INT4(OBSBAS(J1)%ISITE(2)) )
              IB = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB )
              IF ( IB .GT. 0 ) THEN
!
! ---------------- Updating baseline-dependent accumulators
!
                   RST%KU_BAS(IB) = RST%KU_BAS(IB) + 1
                   RST%WRMS_B(IB) = RST%WRMS_B(IB) + &
     &                             (RES(J1)%PSF_DEL*RES(J1)%WEI_DEL)**2
                   AVB(IB) = AVB(IB) +  RES(J1)%WEI_DEL
                   RST%WB_ACC(IB) = RST%WB_ACC(IB) + (RES(J1)%WEI_DEL)**2
              END IF
              NS = OBSBAS(J1)%IND_SCA
              IS = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, &
     &                        INT4(OBSSCA(NS)%ISTAR) )
              IF ( IS .GT. 0 ) THEN
!
! ---------------- Updating source-dependent accumulators
!
                   RST%KU_SOU(IS) = RST%KU_SOU(IS) + 1
                   RST%WRMS_S(IS) = RST%WRMS_S(IS) + &
     &                             (RES(J1)%PSF_DEL*RES(J1)%WEI_DEL)**2
                   AVS(IS) = AVS(IS) +  RES(J1)%WEI_DEL
                   RST%WS_ACC(IS) = RST%WS_ACC(IS) + (RES(J1)%WEI_DEL)**2
              END IF
         END IF
 410  CONTINUE
!
! --- Calculation of (square root from) common dispersion, w.r.m.s. and r.m.s.
!
      IF ( RST%U_OBS < 1 ) THEN
           CALL ERR_LOG ( 6752, IUER, 'RESID_ST', 'Trap of internal '// &
     &         'control: no useable observations remained' )
           RETURN
      END IF
      AV_ACC = AV_ACC/RST%U_OBS
      RST%RMS_G  = DSQRT ( RST%RMS_G/(RST%U_OBS-1)  )
      RST%WDPR_G = DSQRT ( RST%WRMS_G/(RST%U_OBS-1) )
      RST%WRMS_G = DSQRT ( RST%WRMS_G/RST%WW_ACC )
!
! --- Calculation of baseline dependent dispersion and w.r.m.s.
!
      DO 420 J2=1,RST%U_BAS
         IF ( RST%KU_BAS(J2) .GE. 2 ) THEN
!
              AVB(J2) = AVB(J2)/RST%KU_BAS(J2)
              RST%WDPR_B(J2) = DSQRT ( RST%WRMS_B(J2)/(RST%KU_BAS(J2)-1) )
              RST%WRMS_B(J2) = DSQRT ( RST%WRMS_B(J2)/RST%WB_ACC(J2) )
           ELSE IF ( RST%KU_BAS(J2) .EQ. 1 ) THEN
              RST%WRMS_B(J2) = DSQRT ( RST%WRMS_B(J2)/RST%WB_ACC(J2) )
              RST%WDPR_B(J2) = RST%WRMS_B(J2)
           ELSE IF ( RST%KU_BAS(J2) .EQ. 0 ) THEN
              RST%WDPR_B(J2) = 0
              RST%WRMS_B(J2) = 0
         END IF
 420  CONTINUE
!
! --- Calculation source dependent dispersion and w.r.m.s.
!
      DO 430 J3=1,RST%U_SOU
         IF ( RST%KU_SOU(J3) .GE. 2 ) THEN
!
              AVS(J3) = AVS(J3)/RST%KU_SOU(J3)
              RST%WDPR_S(J3) = DSQRT ( RST%WRMS_S(J3)/(RST%KU_SOU(J3)-1) )
              RST%WRMS_S(J3) = DSQRT ( RST%WRMS_S(J3)/RST%WS_ACC(J3) )
           ELSE IF ( RST%KU_SOU(J3) .EQ. 1 ) THEN
              RST%WRMS_S(J3) = DSQRT ( RST%WRMS_S(J3)/RST%WS_ACC(J3) )
              RST%WDPR_S(J3) = RST%WRMS_S(J3)
           ELSE IF ( RST%KU_SOU(J3) .EQ. 0 ) THEN
              RST%WDPR_S(J3) = 0.D0
              RST%WRMS_S(J3) = 0.D0
         END IF
 430  CONTINUE
!
      RST%LAST_FIELD = RST__STADONE
 810  CONTINUE
!
! --- Initialization...
!
      RST%WNPR_MINN_G =  1.D12
      RST%WNPR_MINN_B =  1.D12
      RST%WNPR_MINP_G =  1.D12
      RST%WNPR_MXOA_G = -1.D-20
      RST%WNPR_MXOA_B = -1.D-20
      RST%WNPR_MXOU_G = -1.D-20
      RST%WNPR_MXOU_B = -1.D-20
      RST%WNPR_MXOP_G = -1.D-20
!
      RST%INDX_MINN_G = -1
      RST%INDX_MINN_B = -1
      RST%INDX_MINP_G = -1
      RST%INDX_MXOA_G = -1
      RST%INDX_MXOA_B = -1
      RST%INDX_MXOU_G = -1
      RST%INDX_MXOU_B = -1
      RST%INDX_MXOP_G = -1
!
! --- Searching among all observations:
! --- 1) Observation with max in modula normalized residual
! ---    a) among all observations
! ---    b) among used observations
! ---    A) common dispersion of residuals used for normalization
! ---    B) dispersion of residuals of this baseline used for normalization
! --- 2) Observation with min in modula normalized residual among not used but
! ---    potentially recoverable observations
! ---    A) common dispersion of residuals used for normalization
! ---    B) dispersion of residuals of this baseline used for normalization
!
      DO 440 J4=1,N_OBS
         NB = NSTBA ( INT4(OBSBAS(J4)%ISITE(1)), INT4(OBSBAS(J4)%ISITE(2)) )
         IB = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NB )
         PSF_DEL_ORIG = RES(J4)%PSF_DEL
         PSF_DEL      = RES(J4)%PSF_DEL
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J4)%AUTO_SUP, &
     &                            OBSBAS(J4)%USER_SUP, OBSBAS(J4)%USER_REC, &
     &                            USED__SPS ) 
              FL_RECO = META_SUPR_INQ ( OBSBAS(J4)%AUTO_SUP, &
     &                            OBSBAS(J4)%USER_SUP, OBSBAS(J4)%USER_REC, &
     &                            RECO__SPS ) 
            ELSE 
              FL_USED = SUPR_INQ ( OBSBAS(J4)%SUPSTAT(1), OBSBAS(J4)%UACSUP, &
     &                             USED__SPS ) 
              FL_RECO = SUPR_INQ ( OBSBAS(J4)%SUPSTAT(1), OBSBAS(J4)%UACSUP, &
     &                             RECO__SPS ) 
         END IF
!
         IF ( .NOT. FL_USED       .AND.  &
     &              FL_RECO       .AND.  &
     &        .NOT. RES(J4)%PROT         ) THEN
!
! ----------- Case of NOT USED observation, but potentially recoverable and
! ----------- not in protection list
!
              NUMAMB_COR = 0
              AMB_SP     = 0.D0 ! this is initialization
!
! ----------- Do phase delay ambiguity resolutiuon on the fly if approporaite
! ----------- flags in DBOBJ has been set up
!
              IF ( DBOBJ%F_AMB  .AND.  .NOT. DBOBJ%F_ION ) THEN
!
! ---------------- Without ionosphere correction
!
                   AMB_SP = RES(J4)%AMB_SP
                 ELSE IF ( DBOBJ%F_AMB  .AND.  DBOBJ%F_ION ) THEN
!
! ---------------- With ionosphere correction
!
                   IF ( DBOBJ%IDATYP .EQ. P_PXS__DTP ) THEN
                        AMB_SP = RES(J4)%AMBION_SP
                     ELSE IF ( DBOBJ%IDATYP .EQ. G_GXS__DTP ) THEN
                        AMB_SP = RES(J4)%AMBION_SP
                     ELSE IF ( DBOBJ%IDATYP .EQ. PX_GS__DTP ) THEN
                        AMB_SP = RES(J4)%AMB_SP * OBSBAS(J4)%FREQ_IONO_PH**2/ &
     &                          ( OBSBAS(J4)%FREQ_IONO_PH**2 + &
     &                            OBSBAS(J4)%FREQ_IONO_GR_OPP**2 )
                     ELSE IF ( DBOBJ%IDATYP .EQ. PX_GX__DTP ) THEN
                        AMB_SP = RES(J4)%AMB_SP * OBSBAS(J4)%FREQ_IONO_PH**2/ &
     &                          ( OBSBAS(J4)%FREQ_IONO_PH**2 + &
     &                            OBSBAS(J4)%FREQ_IONO_GR**2 )
                     ELSE IF ( DBOBJ%IDATYP .EQ. PS_GS__DTP ) THEN
                        AMB_SP = RES(J4)%AMB_SP *OBSBAS(J4)%FREQ_IONO_PH_OPP**2/ &
     &                          ( OBSBAS(J4)%FREQ_IONO_PH_OPP**2 + &
     &                            OBSBAS(J4)%FREQ_IONO_GR_OPP**2 )
                     ELSE
                        CALL CLRCH ( STR )
                        CALL INCH  ( INT4(DBOBJ%IDATYP), STR )
                        CALL ERR_LOG ( 6753, IUER, 'RESID_ST', 'Trap of '// &
     &                      'internal control: unsupported solution type: '// &
     &                       STR(1:I_LEN(STR))//' for ambiguity change on '// &
     &                      'the fly' )
                        RETURN
                   END IF
              END IF
!
! ----------- Bypass observation with "Not-A-Number" value of AMB_SP
! ----------- It may occur in some patalogical cases
!
              IF ( IS_R8_NAN ( AMB_SP ) ) GOTO 440
!
              IF ( DBOBJ%F_AMB .AND. AMB_SP > 0.0D0 ) THEN
!
! ---------------- We try to resolve ambiguity on the fly
!
                   NUMAMB_COR = -PSF_DEL/AMB_SP
                   PSF_DEL    =  PSF_DEL + NUMAMB_COR*AMB_SP
                   IF ( PSF_DEL .LT. -0.5D0*AMB_SP ) THEN
                        NUMAMB_COR = NUMAMB_COR + 1
                        PSF_DEL    = PSF_DEL    + AMB_SP
                   END IF
                   IF ( PSF_DEL .GT.  0.5D0*AMB_SP ) THEN
                        NUMAMB_COR = NUMAMB_COR - 1
                        PSF_DEL    = PSF_DEL    - AMB_SP
                   END IF
                   IF ( DBOBJ%IDATYP .EQ. G_GXS__DTP   .AND. &
     &                  ( IABS(NUMAMB_COR) .GT. 32766 )     ) THEN
!
! --------------------- We prevent attempt to resolve ambiguites on the
! --------------------- fly in the case when corected ambiguity can be larger
! --------------------- by modulo than 32766 since SOLVE internally keeps
! --------------------- ambiguities as INTEGER*2
!
                        NUMAMB_COR = 0
                   END IF
!
                   IF ( NUMAMB_COR .NE. 0 ) THEN
!
! --------------------- New potential ambiguity is not zero. Then we have to
! --------------------- check: can we change ambiguuity? Will it not lead to
! --------------------- change of ambiguity of good observations?
!
                        CALL ERR_PASS ( IUER, IER )
                        F_PRMT = AMBCHA_PERMIT ( J4, DBOBJ, OBSBAS, MO_BAS, &
     &                                       LP_OBS, LPIS_OBS, ISGN_OBS, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH   ( STR )
                             CALL INCH    ( J4, STR )
                             CALL ERR_LOG ( 6754, IUER, 'RESID_ST', 'Error '// &
     &                           'in attempt to check eligility of the '// &
     &                           STR(1:I_LEN(STR))//'-th observation for '// &
     &                           'ambuguity change' )
                             RETURN
                        END IF
!
! --------------------- Now we check weight of the observation. A priori
! --------------------- formal error should not exceed the specified share
! --------------------- of the ambiguity spaciong. Otherwise we set the flag
! --------------------- "not eligible for ambiguity resotration" for that
! --------------------- observation
!
                        IF ( 1.D0/RES(J4)%WEI_DEL .GT. SHR__AFL*AMB_SP ) THEN
                             F_PRMT = .FALSE.
                        END IF
!
                        IF ( .NOT. F_PRMT ) THEN
!
! -------------------------- Alas! Change of ambiguity is not permited. Very
! -------------------------- pity. We should play back.
!
                             PSF_DEL = PSF_DEL - NUMAMB_COR*AMB_SP
                             NUMAMB_COR = 0
                        END IF
                   END IF  !  numamb_cor > 0
              END IF  !  dbobj.f_amb
         END IF  ! not used observation
!
! ------ Calculataion
! ------ DEV    -- postfit deviation
! ------ NDEV_G -- globally normalized deviation
! ------ NDEV_B -- baseline-dependently normalized deviation
!
         DEV    = PSF_DEL
         NDEV_G = PSF_DEL*RES(J4)%WEI_DEL/RST%WDPR_G
         IF ( IB .GT. 0 ) THEN
              IF ( RST%KU_BAS(IB) .GT. 0 ) THEN
                   IF ( RST%WDPR_B(IB) < 1.D-15 ) THEN
                        CALL ERR_LOG ( 6755, IUER, 'RESID_ST', 'Trap of internal '// &
     &                      'control: sum of weights is to small. Apparently all '// &
     &                      'observations at a given baseline are deselected' )
                        RETURN 
                   END IF
                   NDEV_B = PSF_DEL*RES(J4)%WEI_DEL/ RST%WDPR_B(IB)
                 ELSE
                   NDEV_B = NDEV_G
              END IF
            ELSE
              NDEV_B = NDEV_G
         END IF
!
! ------ Initilizing elim/mile flags
!
         ELIM_THRU  = .FALSE.
         ELIM_CUT_G = .FALSE.
         ELIM_CUT_B = .FALSE.
         ELIM_UNCE  = .FALSE.
         MILE_THRU  = .FALSE.
         MILE_CUT_G = .FALSE.
         MILE_CUT_B = .FALSE.
!
! ------ Now check both criterion.
! ------ Obersvation considered as passed through elimination criterion if
! ------             it should be eliminated
! ------ Obersvation considered as passed through restoration criterion if
! ------             it should be restored
!
         IF ( THR_USE ) THEN
!
! ----------- Check: does the observation suits the threshold criterion
!
              IF ( DABS(DEV) .GE. DABS(ELIM_THR) ) ELIM_THRU = .TRUE.
              IF ( DABS(DEV) .LT. DABS(ELIM_THR) ) MILE_THRU = .TRUE.
         END IF
!
         IF ( CUT_USE ) THEN
!
! ----------- Check: does the observation suits global n-sigma cutoff criteria?
!
              IF ( DABS(NDEV_G) .GE. ELIM_CUT ) ELIM_CUT_G = .TRUE.
              IF ( DABS(NDEV_G) .LT. ELIM_CUT ) MILE_CUT_G = .TRUE.
!
! ----------- Check: does the observation suits baseline-dependent n-sigma ?
! ----------- cutoff criteria
!
              IF ( DABS(NDEV_B) .GE. ELIM_CUT ) ELIM_CUT_B = .TRUE.
              IF ( DABS(NDEV_B) .LT. ELIM_CUT ) MILE_CUT_B = .TRUE.
         END IF
!
         IF ( .NOT. THR_USE  .AND.  .NOT. CUT_USE ) THEN
!
! ----------- Special case when both criterion are disabled
!
              ELIM_THRU  = .TRUE.
              ELIM_CUT_G = .TRUE.
              ELIM_CUT_B = .TRUE.
              MILE_THRU  = .TRUE.
              MILE_CUT_G = .TRUE.
              MILE_CUT_B = .TRUE.
         END IF
!
         IF ( ELIM_MSR .LT. 1.D-12 ) THEN
              ELIM_UNCE = .FALSE.
            ELSE
              IF ( RES(J4)%WEI_DEL .LE. 1.D0/ELIM_MSR ) THEN
                   ELIM_UNCE = .TRUE.
              END IF
         END IF
!
! ------ Updating (if needed) observation with max in modulo
! ------ postfit residual among all observations
!
         IF ( ELIM_THR .GE. 0.0D0 ) THEN
              ELIM_USE_G = ELIM_THRU  .OR.  ELIM_CUT_G
              ELIM_USE_B = ELIM_THRU  .OR.  ELIM_CUT_B
            ELSE 
              IF ( THR_USE .AND. CUT_USE ) THEN
                   ELIM_USE_G = ELIM_THRU  .AND.  ELIM_CUT_G
                   ELIM_USE_B = ELIM_THRU  .AND.  ELIM_CUT_B
                 ELSE 
                   ELIM_USE_G = ELIM_THRU  .OR.   ELIM_CUT_G
                   ELIM_USE_B = ELIM_THRU  .OR.   ELIM_CUT_B
              END IF
         END IF
!
         IF ( DABS(DEV) .GT. DABS( RST%WNPR_MXOP_G ) .AND. &
     &        ( ELIM_USE_G .OR. ELIM_UNCE          )       ) THEN
!
              RST%WNPR_MXOP_G = DEV
              RST%INDX_MXOP_G = J4
         END IF
!
! ------ Updating (if needed) observation with max in modulo commonly
! ------ normalized residual among all observations
!
         IF ( DABS(NDEV_G) .GT. DABS( RST%WNPR_MXOA_G ) .AND. &
     &        ( ELIM_USE_G .OR. ELIM_UNCE             )       ) THEN
!
              RST%WNPR_MXOA_G = NDEV_G
              RST%INDX_MXOA_G = J4
         END IF
!
! ------ Updating (if needed) observation with max in modulo
! ------ baseline-dependently normalized residual among all observations
!
         IF ( DABS(NDEV_B) .GT. DABS( RST%WNPR_MXOA_B ) .AND. &
     &        ( ELIM_USE_B  .OR. ELIM_UNCE            )       ) THEN
!
              RST%WNPR_MXOA_B = NDEV_B
              RST%INDX_MXOA_B = J4
         END IF
!
         IF ( FL_USED             .AND. & 
     &        .NOT. RES(J4)%PROT        ) THEN
!
! ----------- Used observation and not in protection list
!
! ----------- Updating (if needed) observation with max in modulo commonly
! ----------- normalized residual among used observations
!
              IF ( DABS(NDEV_G) .GT. DABS( RST%WNPR_MXOU_G ) .AND. &
     &             ( ELIM_USE_G .OR. ELIM_UNCE )                   ) THEN
!
                   RST%WNPR_MXOU_G = NDEV_G
                   RST%INDX_MXOU_G = J4
              END IF
!
! ----------- Updating (if needed) observation with max in modulo
! ----------- baseline-dependently normalized residual among used observations
!
              IF ( DABS(NDEV_B) .GT. DABS( RST%WNPR_MXOU_B ) .AND. &
     &             ( ELIM_USE_B .OR. ELIM_UNCE             )       ) THEN
!
                   RST%WNPR_MXOU_B = NDEV_B
                   RST%INDX_MXOU_B = J4
              END IF
            ELSE IF ( .NOT. FL_USED      .AND. &
     &                      FL_RECO      .AND. &
     &                .NOT. RES(J4)%PROT       ) THEN
!
! ----------- Case of NOT USED observation, but potentially recoverable and
! ----------- not in protection list
!
! ----------- Updating (if needed) the field which keeps information about
! ----------- observation with min residual among potentially recoverable
! ----------- observations
!
              IF ( ELIM_THR .GE. 0.0D0 ) THEN
                   MILE_USE_G = MILE_THRU  .OR.   MILE_CUT_G
                   MILE_USE_B = MILE_THRU  .OR.   MILE_CUT_B
                 ELSE 
                   IF ( THR_USE .AND. CUT_USE ) THEN
                        MILE_USE_G = MILE_THRU  .AND.  MILE_CUT_G
                        MILE_USE_B = MILE_THRU  .AND.  MILE_CUT_B
                      ELSE 
                        MILE_USE_G = MILE_THRU  .OR.   MILE_CUT_G
                        MILE_USE_B = MILE_THRU  .OR.   MILE_CUT_B
                   END IF
              END IF
!
              IF ( DABS(DEV) .LT. DABS( RST%WNPR_MINP_G ) .AND. &
     &             MILE_USE_G                             .AND. &
     &             .NOT. ELIM_UNCE ) THEN
!
                   RST%WNPR_MINP_G = PSF_DEL
                   RST%INDX_MINP_G = J4
                   RST%NAMC_MINP_G = NUMAMB_COR
                   RST%AMBS_MINP_G = AMB_SP
              END IF
!
! ----------- Updating (if needed) the field which keeps information about
! ----------- observation with min in modulo commonly normalized residual
! ----------- among potentially recoverable observations
!
              IF ( DABS( PSF_DEL*RES(J4)%WEI_DEL/RST%WDPR_G ) .LT. &
     &             DABS( RST%WNPR_MINN_G )                         .AND. &
     &             MILE_USE_G                                      .AND. &
     &             .NOT. ELIM_UNCE ) THEN
!
                   RST%WNPR_MINN_G = PSF_DEL*RES(J4)%WEI_DEL/RST%WDPR_G
                   RST%INDX_MINN_G = J4
                   RST%NAMC_MINN_G = NUMAMB_COR
                   RST%AMBS_MINN_G = AMB_SP
              END IF
!
! ----------- Updating (if needed) the field which keeps information about
! ----------- observation with min in modulo baseline-dependently normalized
! ----------- residual among potentially recoverable observations
!
              IF ( DABS(NDEV_B) .LT. DABS( RST%WNPR_MINN_B ) .AND. &
     &             MILE_USE_B                                .AND. &
     &             .NOT. ELIM_UNCE ) THEN
!
                   RST%WNPR_MINN_B = NDEV_B
                   RST%INDX_MINN_B = J4
                   RST%NAMC_MINN_B = NUMAMB_COR
                   RST%AMBS_MINN_B = AMB_SP
              END IF
!
! ----------- And now -- restoring original postfit residual
!
              RES(J4)%PSF_DEL = PSF_DEL_ORIG
         END IF
 440  CONTINUE
      RST%FIRST_FIELD = RST__INIT
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  RESID_ST  !#!#
