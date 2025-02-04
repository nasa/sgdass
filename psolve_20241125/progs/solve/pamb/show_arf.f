      SUBROUTINE SHOW_ARF ( IPAR, L_OBS, LIS_OBS, BAND, DBOBJ, OBSBAS, &
     &           OBSSCA, PAMBI, IPLFD_STA, IPL_STA, VAL, SIG, ICOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SHOW_ARF  calculates a station dependent ambiguity        *
! *   resolution functoin (ARF) for the observations of the scan.        *
! *   It calculates value and formal uncertainty of station dependent    *
! *   ARF for the station ISTA granted the station IFD_STA is taken as   *
! *   a fiducial station.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     IPAR  ( INTEGER*4 ) -- Mode switcher.                            *
! *                            IPAR=1 -- S1 arf.                         *
! *                            IPAR=2 -- S2 arf.                         *
! *                            IPAR=3 -- S3 arf.                         *
! *                            IPAR=4 -- S4 arf.                         *
! *     L_OBS ( INTEGER*4 ) -- Number of observations in the scan under  *
! *                            consideration.                            *
! *   LIS_OBS ( INTEGER*4 ) -- List of the observations in the scan      *
! *                            under consideration. It contains the      *
! *                            indices fo the observations in the        *
! *                            session. Dimensiuon: L_OBS.               *
! *      BAND ( INTEGER*4 ) -- Band code (X- or S-). Codes are defined   *
! *                            in pamb.i                                 *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! * IPLFD_STA ( INTEGER*4 ) -- Index of the fiducial station in the list *
! *                            DBOBJ.LIS_STA                             *
! *   IPL_STA ( INTEGER*4 ) -- Index of the targeted station in the list *
! *                            DBOBJ.LIS_STA                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       VAL ( REAL*8    ) -- Value of the ambiguity resolution         *
! *                            function for the targeted station.        *
! *       SIG ( REAL*8    ) -- formal uncertainty of the ambiguity       *
! *                            resolution function for the targeted      *
! *                            station.                                  *
! *     ICOND ( INTEGER*4 ) -- Comletion code.                           *
! *                            ICOND = 1 -- normal termination.          *
! *                            ICOND =-1 -- Error: too few observations  *
! *                                         for computation.             *
! *                            ICOND =-2 -- Targeted and fiducial        *
! *                                         stations are the same.       *
! *                            ICOND =-3 -- Misclosure test rejected all *
! *                                         stations/baselines.          *
! *                            ICOND =-4 -- Fiducial stations did not    *
! *                                         participated in the scan.    *
! *                            ICOND =-5 -- Targeted stations did not    *
! *                                         participated in the scan.    *
! *                            ICOND =-6 -- Failure to assign reference  *
! *                                         station for subnetwork.      *
! *                            ICOND =-7 -- Number of observations       *
! *                                         tuerned out to be            *
! *                                         insufficient for             *
! *                                         determination of ARF value.  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *
! *                                                                      *
! *  ###  02-OCT-98     SHOW_ARF    v1.1 (c)  L. Petrov  23-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(DBOBJ%L_SCA)
      TYPE ( PAMBI__STRU ) ::   PAMBI(DBOBJ%L_OBS)
      INTEGER*4  IPAR, L_OBS, LIS_OBS(L_OBS), BAND, IPLFD_STA, IPL_STA, &
     &           ICOND, IUER
      REAL*8     VAL, SIG
      REAL*8     tmp_mat(100000), tmp_vec(1000)
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
      PARAMETER  ( SCALE      = 1.D12 )
      PARAMETER  ( SCALE_IONO = 4.D9  )
      REAL*8     FC_GX, FC_GS, FC_PX, FC_PS
      REAL*8     XGR_LIM, SGR_LIM, XPH_LIM, SPH_LIM
      INTEGER*4  IPAR_MIN, IPAR_MAX
      PARAMETER  ( XGR_LIM  =  500.0D-12 )
      PARAMETER  ( SGR_LIM  = 2000.0D-12 )
      PARAMETER  ( XPH_LIM  =   20.0D-12 )
      PARAMETER  ( SPH_LIM  =   50.0D-12 )
      PARAMETER  ( IPAR_MIN = 1 )
      PARAMETER  ( IPAR_MAX = 4 )
      INTEGER*4  J1, J2, J3, J4, ISTA1, ISTA2, IPL1, IPL2, N_EQU, N_PAR, &
     &           IER, JPL_FD, LN_SNT, LIS_BASSNT(MO_BAS), LIS_STASNT(MO_STA), &
     &           LIS_FST(MO_STA), IUS_SNT(MO_STA), SNT_FD, KPL_STA, &
     &           JPL_STA, IVERB, MV, I, J
      CHARACTER  STR*20
      CHARACTER  CBAST_NUM*9
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  IFIND_PL, I_LEN
      INTEGER*8  LOCR
! --- Location function for rectangular matrix with leading dimension MV
      LOCR(MV,I,J)=( INT8(MV)*INT8(J-1) + I  )
!
! --- Check: is the scan eligible for determination station dependent raw phase
! --- excess?
!
      ICOND = -1
      IF ( L_OBS .LE. 0 ) THEN
!
! -------- Too few observations
!
           ICOND = -1
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IPL_STA .EQ. IPLFD_STA ) THEN
!
! -------- Targeted station and fiducial station are the same
!
           ICOND = -2
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IPAR .LT. IPAR_MIN  .OR.  IPAR .GT. IPAR_MAX ) THEN
           CALL CLRCH (       STR )
           CALL INCH  ( IPAR, STR )
           CALL ERR_LOG ( 5271, IUER, 'SHOW_ARF', 'Unsupported value of '// &
     &         'IPAR: '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
! --- Making lists of stations and baselines among observations of the scan
!
      CALL ERR_PASS ( IUER, IER )
      CALL MAKE_SCALIS ( L_OBS, LIS_OBS, DBOBJ, OBSBAS, L_STA, LIS_STA, &
     &                   L_BAS, LIS_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5272, IUER, 'SHOW_ARF', 'Error in making lists '// &
     &         'of stations and baselines for the scan under consideration' )
           RETURN
      END IF
!
! --- Check: do some triangles have too large misclose. If misclosure
! --- exceeds the specified limits all three baseliens are excluded from
! --- further processing
!
      IVERB = 1
      CALL ERR_PASS ( IUER, IER )
      CALL MSC_CHECK ( L_OBS, LIS_OBS, DBOBJ, OBSSCA, OBSBAS, PAMBI, XGR_LIM, &
     &                 SGR_LIM, XPH_LIM, SPH_LIM, IVERB, L_STA, LIS_STA, &
     &                 L_BAS, LIS_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5273, IUER, 'SHOW_ARF', 'Error in checking '// &
     &         'triangle misclosure' )
           RETURN
      END IF
!
      IF ( L_STA .LE. 1 ) THEN
!
! -------- all baselines/stations were discarded by MSC_CHECK
!
           ICOND = -3
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      JPL_FD = IFIND_PL ( L_STA, LIS_STA, DBOBJ%LIS_STA(IPLFD_STA) )
!
      IF ( JPL_FD .LE. 0 ) THEN
!
! -------- Fiducial station didn't participate in that scan
!
           ICOND = -4
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      JPL_STA = IFIND_PL ( L_STA, LIS_STA, DBOBJ%LIS_STA(IPL_STA) )
      IF ( JPL_STA .LE. 0 ) THEN
!
! -------- Targeted station didn't pariticipate in that scan
!
           ICOND = -5
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Look at the list of observation of the scan and determine how many
! --- subnetworks the scan has.
! --- LN_CNT     -- the number of subnetworks
! --- LIS_BASSNT -- array of subnetwork indices for baselines
! --- LIS_STASNT -- array of subnetwork indices for stations
!
      CALL FIND_SUBNET ( L_BAS, LIS_BAS, L_STA, LIS_STA, LN_SNT, LIS_BASSNT, &
     &                   LIS_STASNT )
!
! --- Initialize array of flags of subnetwork usage
!
      CALL NOUT_I4 ( MO_STA, IUS_SNT )
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
                      IF ( LIS_STASNT(J2) .EQ.  J1 ) THEN
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
                   ICOND = -6
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
      IF ( IPAR .EQ. 1 ) THEN
           N_PAR = LA_STA*4
           N_EQU = L_OBS*4
         ELSE IF ( IPAR .EQ. 2 ) THEN
           N_PAR = LA_STA*3
           N_EQU = L_OBS*4
         ELSE IF ( IPAR .EQ. 3  ) THEN
           N_PAR = LA_STA*2
           N_EQU = L_OBS*2
         ELSE IF ( IPAR .EQ. 4 ) THEN
           N_PAR = LA_STA*2
           N_EQU = L_OBS*3
      END IF
!
! --- Form equations of conditions
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
              IF ( IPAR .EQ. 1 ) THEN
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
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+2*LA_STA,L_EQU+3) ) = -1.D0
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+3*LA_STA,L_EQU+4) ) = -1.D0
                 ELSE IF ( IPAR .EQ. 2 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -FC_GX
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+3) ) = -FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+4) ) = -FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+1*LA_STA,L_EQU+3) ) = -1.D0
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+2*LA_STA,L_EQU+4) ) = -1.D0
                 ELSE IF ( IPAR .EQ. 3 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -FC_PX
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+1*LA_STA,L_EQU+2) ) = -1.D0
                 ELSE IF ( IPAR .EQ. 4 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+1) ) = -FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+2) ) = -FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL1,L_EQU+3) ) = -FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL1+1*LA_STA,L_EQU+3) ) = -1.D0
              END IF
         END IF
!
! ------ Now putting coefficients for the second station of the baseline
!
         IF ( IPL2 .GE. 1 ) THEN
              IF ( IPAR .EQ. 1 ) THEN
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
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+2*LA_STA,L_EQU+3) ) = 1.D0
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+3*LA_STA,L_EQU+4) ) = 1.D0
                 ELSE IF ( IPAR .EQ. 2 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = FC_GX
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+3) ) = FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+4) ) = FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+1*LA_STA,L_EQU+3) ) = 1.D0
!
! ---------------- Station-dependent phase delay ambiguity for S-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+2*LA_STA,L_EQU+4) ) = 1.D0
                 ELSE IF ( IPAR .EQ. 3 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = FC_PX
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+1*LA_STA,L_EQU+2) ) = 1.D0
                 ELSE IF ( IPAR .EQ. 4 ) THEN
!
! ---------------- Ionosphere content
!
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+1) ) = FC_GS
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+2) ) = FC_PX
                   EQU_MAT( LOCR(N_PAR,IPL2,L_EQU+3) ) = FC_PS
!
! ---------------- Station-dependent phase delay ambiguity for X-band
!
                   EQU_MAT( LOCR(N_PAR,IPL2+1*LA_STA,L_EQU+3) ) = 1.D0
              END IF
         END IF
!
! ------ Putting values of right parts
!
         IF ( IPAR .EQ. 1 ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+3) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+4) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( IPAR .EQ. 2 ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+3) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+4) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( IPAR .EQ. 3 ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
            ELSE IF ( IPAR .EQ. 4 ) THEN
              EQU_VEC(L_EQU+1) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUGR_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+2) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS     - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
              EQU_VEC(L_EQU+3) = SCALE*( OBSBAS(LIS_OBS(J4))%TAUPH_OBS_OPP - &
     &                                    PAMBI(LIS_OBS(J4))%TAU_CA          )
         END IF
!
! ------ Putting weights
!
         IF ( IPAR .EQ. 1  ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR**2 + &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_COR**2))
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* &
     &                                   OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP )
              WEI_VEC(L_EQU+3) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              WEI_VEC(L_EQU+4) = 1.D0/( SCALE* DSQRT( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR_OPP**2 + &
     &                                  TAUPH_ERR_COR_OPP**2 ) )
              L_EQU = L_EQU + 4
           ELSE IF ( IPAR .EQ. 2 ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR**2 + &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_COR**2) )
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP**2 + &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_COR**2) )
              WEI_VEC(L_EQU+3) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              WEI_VEC(L_EQU+4) = 1.D0/( SCALE* DSQRT( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR_OPP**2 + &
     &                                  TAUPH_ERR_COR_OPP**2 ) )
              L_EQU = L_EQU + 4
           ELSE IF ( IPAR .EQ. 3 ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP )
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              L_EQU = L_EQU + 2
           ELSE IF ( IPAR .EQ. 4 ) THEN
              WEI_VEC(L_EQU+1) = 1.D0/( SCALE* &
     &                                  OBSBAS(LIS_OBS(J4))%TAUGR_ERR_OPP )
              WEI_VEC(L_EQU+2) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              WEI_VEC(L_EQU+3) = 1.D0/( SCALE* DSQRT ( &
     &                                  OBSBAS(LIS_OBS(J4))%TAUPH_ERR_OPP**2 + &
     &                                  TAUPH_ERR_COR**2 ) )
              L_EQU = L_EQU + 3
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
!
      IF ( N_EQU .LE. N_PAR ) THEN
           ICOND = -7
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
           WRITE ( 6, * ) ' l_sta = ',l_sta, ' la_sta = ',la_sta, &
     &            ' n_par = ',n_par, ' n_equ = ',n_equ
!           type *,' tmp_vec = ', (tmp_vec(j1), j1=1,n_equ )
           WRITE ( 6, * ) ' wei_vec = ', (wei_vec(j1), j1=1,n_equ )
           WRITE ( 6, * ) ' jpl_fd  = ',jpl_fd,' l_bas = ',l_bas
           WRITE ( 6, * ) ' ln_snt = ',ln_snt,' lis_stasnt = ', &
     &                                 (lis_stasnt(j1), j1=1,l_sta )
           WRITE ( 6, * ) ' lis_fst = ', (lis_fst(j1), j1=1,ln_snt)
           call pause ( 'before matview' )
           call matview_1 ( n_par, n_equ, tmp_mat )
           CALL ERR_LOG ( 5274, IUER, 'SHOW_ARF', 'Error in solving '// &
     &         'equations of conditions using weighted LSQ method' )
           RETURN
      END IF
!
      KPL_STA = IFIND_PL ( LA_STA, LIA_STA, DBOBJ%LIS_STA(IPL_STA) )
      IF ( IPAR .EQ. 1 ) THEN
           IF ( BAND .EQ. PAMB__XBAND ) THEN
                VAL = EST_VEC(KPL_STA+2*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
                SIG = SIG_VEC(KPL_STA+2*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
              ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
                VAL = EST_VEC(KPL_STA+3*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
                SIG = SIG_VEC(KPL_STA+3*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
           END IF
         ELSE IF ( IPAR .EQ. 2 ) THEN
           IF ( BAND .EQ. PAMB__XBAND ) THEN
                VAL = EST_VEC(KPL_STA+1*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
                SIG = SIG_VEC(KPL_STA+1*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
              ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
                VAL = EST_VEC(KPL_STA+2*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
                SIG = SIG_VEC(KPL_STA+2*LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
           END IF
         ELSE IF ( IPAR .EQ. 3 ) THEN
           IF ( BAND .EQ. PAMB__XBAND ) THEN
                VAL = EST_VEC(KPL_STA+LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
                SIG = SIG_VEC(KPL_STA+LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH /SCALE
           END IF
         ELSE IF ( IPAR .EQ. 4 ) THEN
           IF ( BAND .EQ. PAMB__SBAND ) THEN
                VAL = EST_VEC(KPL_STA+LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
                SIG = SIG_VEC(KPL_STA+LA_STA) * &
     &                        OBSBAS(LIS_OBS(1))%FREQ_OBSV_PH_OPP /SCALE
           END IF
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         if ( dabs(val) .gt. 5.0d5  .or. dabs(sig) .gt. 1.5d5 ) then        ! %%
!              type *,' tmp_vec = ', (tmp_vec(j1), j1=1,n_equ   )            ! %%
!              type *,' wei_vec = ', (wei_vec(j1), j1=1,n_equ   )            ! %%
!              type *,' est_vec = ', (est_vec(j1), j1=1,n_par   )            ! %%
!              type *,' val = ',val,' sig = ',sig,' sig0=',sig0,' rc = ',rc  ! %%
!              type *,' l_obs = ',l_obs,' l_sta = ',l_sta                    ! %%
!              type *,' jpl_fd  = ',jpl_fd,' l_bas = ',l_bas                 ! %%
!              type *,' lis_bas = ', ( cbast_num(lis_bas(j1)), j1=1,l_bas )  ! %%
!              type *,' lis_sta = ', ( lis_sta(j1), j1=1,l_sta )             ! %%
!              type *,' ln_snt = ',ln_snt,' lis_stasnt = ',                  ! %%
!     #                                 (lis_stasnt(j1), j1=1,l_sta )        ! %%
!              type *,' lis_fst = ', (lis_fst(j1), j1=1,ln_snt)              ! %%
!         end if                                                             ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      ICOND = 1
      CALL ERR_PASS ( 0, IUER )
!
      RETURN
      END  !#!  SHOW_ARF  #!#
