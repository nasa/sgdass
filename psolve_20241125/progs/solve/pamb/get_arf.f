      SUBROUTINE GET_ARF ( L_OBS, LIS_OBS, ISCA, DBOBJ, OBSBAS, OBSSCA, &
     &                     PAMBI, SCAINF, ICOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_ARF  computes station dependent ambiguity resolution  *
! *   functions (ARF) for all stations participated in the ISCA-th scan  *
! *   except the station(s) taken as a fiducial one(s).                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_OBS ( INTEGER*4 ) -- Number of potentially good observations     *
! *                          which belong to the ISCA-th scan.           *
! * LIS_OBS ( INTEGER*4 ) -- List of observations which belong to the    *
! *                          ISCA-th scan. The k-th element of the list  *
! *                          LIS_OBS is the index of the observation in  *
! *                          the database to be under consideration.     *
! *    ISCA ( INTEGER*4 ) -- Index of the scan under consideration.      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  OBSSCA ( RECORD    ) -- Array of data structures which keeps        *
! *                          scan-dependent information about the        *
! *                          session.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   ICOND ( INTEGER*4 ) -- Completion code. ICOND=1 means successful   *
! *                          completion; negative completion code means  *
! *                          that ARF for the ISCA-th scan has not been  *
! *                          computed due to some reasons.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about        *
! *                          the session.                                *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM. ARF will be written in this data    *
! *                          structure.                                  *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  23-OCT-98     GET_ARF    v1.3  (c)  L. Petrov  22-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  L_OBS, LIS_OBS(L_OBS), ISCA, ICOND, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( PAMBI__STRU ) ::  PAMBI(DBOBJ%L_OBS)
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  M_EQU, M_PAR, M_CVM
      PARAMETER  ( M_EQU = 4*MO_BAS,  M_PAR = 4*(MO_STA-1) )
      PARAMETER  ( M_CVM = (M_PAR*(M_PAR+1))/2 )
!
      INTEGER*4  L_STA, LIS_STA(MO_STA), L_BAS, LIS_BAS(MO_BAS), &
     &           LA_STA, LIA_STA(MO_STA), L_EQU
      REAL*8     EQU_MAT(M_EQU*M_PAR), EQU_VEC(M_EQU), WEI_VEC(M_EQU), &
     &           COV_MAT(M_CVM), EST_VEC(M_PAR), SIG_VEC(M_PAR), &
     &           RC, SIG0
      REAL*8     TAUPH_ERR_COR, TAUPH_ERR_COR_OPP, SCALE, SCALE_IONO
      PARAMETER  ( TAUPH_ERR_COR     =  5.0D-12 )
      PARAMETER  ( TAUPH_ERR_COR_OPP = 15.0D-12 )
      PARAMETER  ( SCALE             =  1.D12   )
      PARAMETER  ( SCALE_IONO        =  4.D9    )
      REAL*8     FC_GX, FC_GS, FC_PX, FC_PS
      REAL*8     PSF__SHA
      PARAMETER  ( PSF__SHA= 0.8 ) ! Share of noise in postfit residual
      INTEGER*4  J1, J2, J3, J4, J5, ISTA1, ISTA2, IPL1, IPL2, N_EQU, N_PAR, &
     &           IER, JPL_FD, LN_SNT, LIS_BASSNT(MO_BAS), LIS_STASNT(MO_STA), &
     &           LIS_FST(MO_STA), IUS_SNT(MO_STA), SNT_FD, IPL, &
     &           IVERB, MV, I, J
      CHARACTER  STR*20
!
      REAL*8     TMP_MAT(100000), TMP_VEC(1000)
      CHARACTER  CBAST_NUM*9
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  IFIND_PL, I_LEN
      INTEGER*8  LOCR
! --- Location function for rectangular matrix with leading dimension MV
      LOCR(MV,I,J)=( INT8(MV)*INT8(J-1) + I  )
!
! --- Check: is the scan eligible for determination of station dependent raw
! --- phase excesses?
!
      IF ( L_OBS .LE. 0 ) THEN
!
! -------- Too few observations
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Making lists of stations and baselines among observations of the scan
!
      CALL ERR_PASS ( IUER, IER )
      CALL MAKE_SCALIS ( L_OBS, LIS_OBS, DBOBJ, OBSBAS, L_STA, LIS_STA, &
     &                   L_BAS, LIS_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   (       STR )
           CALL INCH    ( ISCA, STR )
           CALL ERR_LOG ( 5371, IUER, 'GET_ARF', 'Error in making lists '// &
     &         'of stations and baselines for scan '//STR )
           RETURN
      END IF
!
      IF ( SCAINF%MSC_CONTROL ) THEN
!
! -------- Check: do some triangles have too large misclose? If misclosure
! -------- exceeds the specified limits all three baseliens are excluded from
! -------- further processing
!
           IVERB = 1
           CALL ERR_PASS  ( IUER, IER )
           CALL MSC_CHECK ( L_OBS, LIS_OBS, DBOBJ, OBSSCA, OBSBAS, PAMBI, &
     &                      SCAINF%XGR_LIM, SCAINF%SGR_LIM, &
     &                      SCAINF%XPH_LIM, SCAINF%SPH_LIM, IVERB, L_STA, &
     &                      LIS_STA, L_BAS, LIS_BAS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   (       STR )
                CALL INCH    ( ISCA, STR )
                CALL ERR_LOG ( 5272, IUER, 'GET_ARF', 'Error in checking '// &
     &               'triangle misclosure for scan '//STR  )
               RETURN
           END IF
      END IF
!
      IF ( L_STA .LE. 1 ) THEN
!
! -------- all baselines/stations were discarded by MSC_CHECK
!
           ICOND = -1
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      JPL_FD = IFIND_PL ( L_STA, LIS_STA, DBOBJ%LIS_STA(SCAINF%FID_STA) )
      IF ( JPL_FD .LE. 0 ) THEN
!
! -------- Fiducial station didn't participate in that scan
!
           ICOND = -2
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Look at the list of observation of the scan and determine how many
! --- subnetworks the scan has.
! --- LN_SNT     -- the number of subnetworks
! --- LIS_BASSNT -- array of subnetwork indices for baselines
! --- LIS_STASNT -- array of subnetwork indices for stations
!
      CALL FIND_SUBNET ( L_BAS, LIS_BAS, L_STA, LIS_STA, LN_SNT, LIS_BASSNT, &
     &                   LIS_STASNT )
!
! --- Initialize array of flags of subnetwork usage
!
      CALL NOUT_I4 ( MO_STA, IUS_SNT )
      CALL NOUT_I4 ( MO_STA, LIS_FST )
!
! --- SNT_FD -- a subnetwork to which a fiducial station belongs
!
      SNT_FD = LIS_STASNT(JPL_FD)
!
! --- LIS_FST -- an array of fiducial stations for each networks. If the j-th
! ---            station is set as a fiducial one in the k-th network, then
! ---            LIS_FST(k) = j
!
      LIS_FST(SNT_FD) = LIS_STA(JPL_FD)
      IUS_SNT(SNT_FD) = 1
!
      IF ( LN_SNT .GT. 1 ) THEN
!
! -------- Scan has several subnetworks. Search of additional fiducial
! -------- stations
!
           DO 410 J1=1,LN_SNT
              IF ( IUS_SNT(J1) .NE. 1 ) THEN
!
! ---------------- J1-th subnetwork has not been in use
!
                   DO 420 J2=1,L_STA
                      IF ( LIS_STASNT(J2) .EQ.  J1  ) THEN
!
! ------------------------ J2-th station belongs to the J1-th subnetwork.
! ------------------------ Well, it may be considered as a good choice to
! ------------------------ become an additional fiducial station for the
! ------------------------ J1-th subnetwork
!
                           IUS_SNT(J1) = 1
                           LIS_FST(J1) = LIS_STA(J2)
                           GOTO 410
                      END IF
 420               CONTINUE
!
                   ICOND = -3
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- Check all stations of the scan and creation the list of adjustable
! --- stations. Adjustable station is the station which is not fiducial one.
!
      LA_STA = 0
      DO 430 J3=1,L_STA
         IF ( IFIND_PL ( LN_SNT, LIS_FST, LIS_STA(J3) ) .LE. 0 ) THEN
              CALL ADD_LIS ( MO_STA, LA_STA, LIA_STA, LIS_STA(J3), -3 )
         END IF
 430  CONTINUE
!
! --- Compute coefficients before parameter station dependent
! --- "ionosphere content". We have to scale it in order to avoid problems
! --- with loss of precision
!
      FC_GX =  SCALE_IONO**2/ OBSBAS(LIS_OBS(1))%FREQ_IONO_GR**2
      FC_GS =  SCALE_IONO**2/ OBSBAS(LIS_OBS(1))%FREQ_IONO_GR_OPP**2
      FC_PX = -SCALE_IONO**2/ OBSBAS(LIS_OBS(1))%FREQ_IONO_PH**2
      FC_PS = -SCALE_IONO**2/ OBSBAS(LIS_OBS(1))%FREQ_IONO_PH_OPP**2
!
! --- Initilaization
!
      CALL NOUT_R8 ( M_PAR*M_EQU, EQU_MAT )
      CALL NOUT_R8 ( M_EQU,       EQU_VEC )
      L_EQU = 0
!
      IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &     SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR       ) THEN
!
           N_PAR = LA_STA*4
           N_EQU = L_OBS*4
         ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS  .OR. &
     &             SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS       ) THEN
           N_PAR = LA_STA*2
           N_EQU = L_OBS*2
      END IF
!
! --- Form equations of conditions. The order of parameters:
! --- [         1, LA_STA  ] -- Ionosphere free delay for the stations
! ---                           in order of their codes
! --- [  LA_STA+1, 2*LA_STA] -- Ionosphere content
! --- [2*LA_STA+1, 3*LA_STA] -- ARF for X-band
! --- [3*LA_STA+1, 4*LA_STA] -- ARF for S-band
!
      DO 440 J4=1,L_OBS
         ISTA1 = INT4( OBSBAS(LIS_OBS(J4))%ISITE(1) )
         ISTA2 = INT4( OBSBAS(LIS_OBS(J4))%ISITE(2) )
         IPL1  = IFIND_PL ( LA_STA, LIA_STA, ISTA1  )
         IPL2  = IFIND_PL ( LA_STA, LIA_STA, ISTA2  )
         IF ( IPL1 .LE. 0   .AND.   IPL2 .LE. 0 ) GOTO 440
!
! ------ Putting coefficients for the first station of the baseline
!
         IF ( IPL1 .GE. 1 ) THEN
              IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &             SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR        ) THEN
!
! ---------------- Ionosphere free delay
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -1.D0
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -1.D0
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+3) ) = -1.D0
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+4) ) = -1.D0
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1+LA_STA,L_EQU+1) ) = -FC_GX
                   EQU_MAT( LOCR(N_PAR,IPL1+LA_STA,L_EQU+2) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1+LA_STA,L_EQU+3) ) = -FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL1+LA_STA,L_EQU+4) ) = -FC_PS
!
! ---------------- Station-dependent phase delay differences for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+2*LA_STA,L_EQU+3) ) = -1.D0
!
! ---------------- Station-dependent phase delay differences for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+3*LA_STA,L_EQU+4) ) = -1.D0
                 ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -FC_PX
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+1*LA_STA,L_EQU+2) ) = -1.D0
                 ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+1*LA_STA,L_EQU+2) ) = -1.D0
              END IF
         END IF
!
! ------ Now putting coefficients for the second station of the baseline
!
         IF ( IPL2 .GE. 1 ) THEN
              IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &             SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR       ) THEN
!
! ---------------- Ionosphere free delay
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = 1.D0
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = 1.D0
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+3) ) = 1.D0
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+4) ) = 1.D0
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2+LA_STA,L_EQU+1) ) = FC_GX
                   EQU_MAT( LOCR(N_PAR,IPL2+LA_STA,L_EQU+2) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2+LA_STA,L_EQU+3) ) = FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL2+LA_STA,L_EQU+4) ) = FC_PS
!
! ---------------- Station-dependent phase delay differences for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+2*LA_STA,L_EQU+3) ) = 1.D0
!
! ---------------- Station-dependent phase delay differences for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+3*LA_STA,L_EQU+4) ) = 1.D0
                 ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = FC_PX
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+1*LA_STA,L_EQU+2) ) = 1.D0
                 ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+1*LA_STA,L_EQU+2) ) = 1.D0
              END IF
         END IF
!
! ------ Putting values of right parts
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,' lis_obs(j4)=',lis_obs(j4),                    ! %%%%
!     #         ' tau_ca = ',pambi(lis_obs(j4)).tau_ca,         ! %%%%
!     #  ' gx = ',obsbas(lis_obs(j4)).taugr_obs - pambi(lis_obs(j4)).tau_ca, ! %%
!     #  ' gs = ',obsbas(lis_obs(j4)).taugr_obs_opp - pambi(lis_obs(j4)).tau_ca, ! %%
!     #  ' px = ',obsbas(lis_obs(j4)).taugr_obs - pambi(lis_obs(j4)).tau_ca, ! %%
!     #  ' ps = ',obsbas(lis_obs(j4)).tauph_obs_opp - pambi(lis_obs(j4)).tau_ca ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+3) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+4) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA    - &
     &                                    PAMBI(LIS_OBS(J4))%PSF_DEL         )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA        - &
     &                                    PAMBI(LIS_OBS(J4))%PSF_DEL         )
              EQU_VEC(L_EQU+3) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+4) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
         END IF
!
! ------ Putting weights
!
         IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &        SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR       ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR**2 + &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_COR**2))
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP )
              WEI_VEC(L_EQU+3) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              WEI_VEC(L_EQU+4) = 1.D0/( SCALE* DSQRT( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR_OPP**2 + &
     &                                  TAUPH_ERR_COR_OPP**2 ) )
              L_EQU = L_EQU + 4
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP**2 + &
     &                              (PSF__SHA*PAMBI(LIS_OBS(J4))%PSF_DEL)**2 ) )
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 + &
     &                              (PSF__SHA*PAMBI(LIS_OBS(J4))%PSF_DEL)**2 ) )
              L_EQU = L_EQU + 2
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP**2 + &
     &                              (PSF__SHA*PAMBI(LIS_OBS(J4))%PSF_DEL)**2 ) )
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR_OPP**2 + &
     &                                  TAUPH_ERR_COR**2 + &
     &                              (PSF__SHA*PAMBI(LIS_OBS(J4))%PSF_DEL)**2 ) )
              L_EQU = L_EQU + 2
         END IF
 440  CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        call matview_1 ( n_par, n_equ, equ_mat )          ! %%
!        type *,' equ_vec = ', (equ_vec(j1), j1=1,n_equ )  ! %%
!        type *,' wei_vec = ', (wei_vec(j1), j1=1,n_equ )  ! %%
!       type *,' l_sta = ',l_sta, ' la_sta = ',la_sta,' n_par = ',n_par,    ! %%
!     #        ' n_equ = ',n_equ                                            ! %%
!       call copy_v ( n_equ, equ_vec, tmp_vec )
!       call copy_v ( n_par*n_equ, equ_mat, tmp_mat )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        if ( ln_snt .gt. 1 ) then                                          ! %%
!             type *,' ========= isca = ',isca                              ! %%
!             type *,' l_obs = ',l_obs                                      ! %%
!             type *,' ln_snt = ',ln_snt,' lis_bassnt = ',                  ! %%
!     #              ( lis_bassnt(j1), j1=1,l_bas )                         ! %%
!             type *,' lis_stasnt = ',( lis_stasnt(j1), j1=1,l_sta )        ! %%
!             type *,' l_sta = ',l_sta, ' la_sta = ',la_sta                 ! %%
!             type *,' n_par = ',n_par, ' n_equ = ',n_equ                   ! %%
!             type *,' lia_sta = ',( lia_sta(j1), j1=1,la_sta)              ! %%
!         end if                                                            ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Check: do we have anough equations?
!
      IF ( N_EQU .LE. N_PAR ) THEN
           ICOND = -4
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Solving the system of equations of conditions using weighted LSQ
!
      IER = -1
      CALL LSQW ( N_PAR, N_EQU, EQU_MAT, EQU_VEC, WEI_VEC, EST_VEC, SIG_VEC, &
     &            COV_MAT, RC, SIG0, IER )
!
      IF ( IER .NE. 0 ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           WRITE ( 6, * ) ' tmp_vec = ', (tmp_vec(j1), j1=1,n_equ   )            ! %%
           WRITE ( 6, * ) ' wei_vec = ', (wei_vec(j1), j1=1,n_equ   )            ! %%
           WRITE ( 6, * ) ' jpl_fd  = ',jpl_fd,' l_bas = ',l_bas                 ! %%
           WRITE ( 6, * ) ' l_sta = ',l_sta,' n_equ = ',n_equ,' n_par = ',n_par
           WRITE ( 6, * ) ' ln_snt = ',ln_snt,' lis_stasnt = ', &                   ! %%
     &                                 (lis_stasnt(j1), j1=1,l_sta )     ! %%
           WRITE ( 6, * ) ' lis_fst = ', (lis_fst(j1), j1=1,ln_snt)              ! %%
           call pause ( 'before matview' )                               ! %%
           call matview_1 ( n_par, n_equ, tmp_mat )                      ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           CALL CLRCH ( STR )
           CALL INCH  ( ISCA, STR )
           CALL ERR_LOG ( 5273, IUER, 'GET_ARF', 'Error in solving '// &
     &         'equations of conditions using weighted LSQ method for scan '// &
     &          STR )
           RETURN
      END IF
!
! --- Collecting results and putting them into SCAINF data structure
!
      DO 450 J5=1,LA_STA
         IPL = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, LIA_STA(J5) )
         IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &        SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR       ) THEN
!
! ----------- Getting adjustments
!
              SCAINF%TAU_SCA(IPL,ISCA) = EST_VEC(J5) /SCALE
              SCAINF%ION_SCA(IPL,ISCA) = EST_VEC(J5+LA_STA) /SCALE / &
     &                                                       SCALE_IONO**2
              SCAINF%XPA_SCA(IPL,ISCA) = EST_VEC(J5+2*LA_STA) /SCALE * &
     &                                   OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH
              SCAINF%SPA_SCA(IPL,ISCA) = EST_VEC(J5+3*LA_STA) /SCALE * &
     &                                   OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP
!
! ----------- ... and estimates of their uncertainties
!
              SCAINF%TAU_SCA_SIG(IPL,ISCA) = SIG_VEC(J5) /SCALE
              SCAINF%ION_SCA_SIG(IPL,ISCA) = SIG_VEC(J5+LA_STA) /SCALE / &
     &                                       SCALE_IONO**2
              SCAINF%XPA_SCA_SIG(IPL,ISCA) = SIG_VEC(J5+2*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH
              SCAINF%SPA_SCA_SIG(IPL,ISCA) = SIG_VEC(J5+3*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP
!
! ----------- Applying a floor for sigma
!
              SCAINF%XPA_SCA_SIG(IPL,ISCA) = &
     &           DSQRT ( SCAINF%XPA_SCA_SIG(IPL,ISCA)**2 + SCAINF%ARFFLO**2 )
              SCAINF%SPA_SCA_SIG(IPL,ISCA) = &
     &           DSQRT ( SCAINF%SPA_SCA_SIG(IPL,ISCA)**2 + SCAINF%ARFFLO**2 )
!
              SCAINF%USE_SCA(IPL,ISCA) = .TRUE.
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS ) THEN
              SCAINF%XPA_SCA(IPL,ISCA)     = EST_VEC(J5+1*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH
              SCAINF%XPA_SCA_SIG(IPL,ISCA) = SIG_VEC(J5+1*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH
!
! ----------- Applying a floor for sigma
!
              SCAINF%XPA_SCA_SIG(IPL,ISCA) = &
     &           DSQRT ( SCAINF%XPA_SCA_SIG(IPL,ISCA)**2 + SCAINF%ARFFLO**2 )
!
              SCAINF%USE_SCA(IPL,ISCA) = .TRUE.
            ELSE IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS ) THEN
              SCAINF%SPA_SCA(IPL,ISCA)     = EST_VEC(J5+1*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP
              SCAINF%SPA_SCA_SIG(IPL,ISCA) = SIG_VEC(J5+1*LA_STA) /SCALE * &
     &                                     OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP
!
! ----------- Applying a floor for sigma
!
              SCAINF%SPA_SCA_SIG(IPL,ISCA) = &
     &           DSQRT ( SCAINF%SPA_SCA_SIG(IPL,ISCA)**2 + SCAINF%ARFFLO**2 )
!
              SCAINF%USE_SCA(IPL,ISCA) = .TRUE.
         END IF
 450  CONTINUE
!
      ICOND = 1
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_ARF  #!#
