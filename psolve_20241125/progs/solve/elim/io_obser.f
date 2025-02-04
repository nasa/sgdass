      SUBROUTINE WR_OBSER ( IWR, NN_SCA, K_SCA, NN_STA, N_OBS, K_OBS, &
     &                      TAU_COR, TAUGR_ERR_COR, TAUPH_ERR_COR, &
     &                      DBOBJ, OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WR_OBSER  writes certain fields of the oborg area to the  *
! *   corresponding fields of data structures OBSSCA, OBSSTA, OBSBAS and *
! *   it writes the status of the updates in the fields of the structure *
! *   OBSHLD also. Data structures OBSSCA, OBSSTA, OBSBAS, RES will      *
! *   contain restricted copy of oborg area -- information about         *
! *   circumstances of observations and observables which allow to       *
! *   recalculate normal matrix.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *           IWR ( INTEGER*4 ) -- Mode of work. The only value 1 is     *
! *                                supported nowdays.                    *
! *        NN_SCA ( INTEGER*4 ) -- Maximal expected number of scans in   *
! *                                database.                             *
! *        NN_STA ( INTEGER*4 ) -- Maximal expected number of stations   *
! *                                in database.                          *
! *         N_OBS ( INTEGER*4 ) -- Maximal expected number of            *
! *                                observations.                         *
! *         K_OBS ( INTEGER*4 ) -- Index of the observations to be       *
! *                                written.                              *
! *       TAU_COR ( REAL*8    ) -- Correction to delay due to            *
! *                                observation dependent calibration.    *
! * TAUGR_ERR_COR ( REAL*8    ) -- Quadratically additive correction     *
! *                                to the group delay weights.           *
! * TAUPH_ERR_COR ( REAL*8    ) -- Quadratically additive correction     *
! *                                to the phase delay weights.           *
! *         K_SCA ( INTEGER*4 ) -- Number of scan in database            *
! *                                corresponding to K_OBS-th             *
! *                                observation.                          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSHLD ( RECORD    ) -- Data structures which keeps the current   *
! *                            status.                                   *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  15-SEP-1997  WR_OBSER   v3.9 (c)  L. Petrov  20-NOV-2017  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'obser.i'
!
      REAL*8     TAU_COR, TAUGR_ERR_COR, TAUPH_ERR_COR
      INTEGER*4  IWR, NN_SCA, K_SCA, NN_STA, N_OBS, K_OBS, IP1, IP2, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( SCA_O__STRU ) ::  OBSSCA(NN_SCA)
      TYPE ( STA_O__STRU ) ::  OBSSTA(NN_STA,NN_SCA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      CHARACTER  STR*20
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      INTEGER*4, EXTERNAL :: IFIND_PL, I_LEN
!
! --- Test of some input parameters
!
      IF ( NN_SCA .GT. MO_SCA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MO_SCA, STR )
           CALL ERR_LOG ( 6731, IUER, 'WR_OBSER', 'Papameter NN_SCA '// &
     &         'exceeded the upper limit specified in obser.i: MO_SCA = '// &
     &          STR )
           RETURN
      END IF
      IF ( K_SCA .GT. NN_SCA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NN_SCA, STR )
           CALL ERR_LOG ( 6732, IUER, 'WR_OBSER', 'Papameter K_SCA '// &
     &         'exceeded the upper limit for this session: NN_SCA = '// &
     &          STR )
           RETURN
      END IF
!
      IF ( NN_STA .GT. MO_STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MO_STA, STR )
           CALL ERR_LOG ( 6733, IUER, 'WR_OBSER', 'Papameter NN_STA '// &
     &         'exceeded the upper limit specified in obser.i: MO_STA = '// &
     &          STR )
           RETURN
      END IF
!
      IF ( K_OBS .GT. MO_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MO_OBS, STR )
           CALL ERR_LOG ( 6734, IUER, 'WR_OBSER', 'Papameter K_SCA '// &
     &         'exceeded the upper limit specified in obser.i: MO_SCA = '// &
     &          STR )
           RETURN
      END IF
!
! --- Get indices of the site in the station list
!
      IP1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, INT4(ISITE(1)) )
      IP2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, INT4(ISITE(2)) )
!
      IF ( IP1 .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(ISITE(1)), STR )
           CALL ERR_LOG ( 6735, IUER, 'WR_OBSER', 'Trap of internal control: '// &
     &         'site '//STR(1:I_LEN(STR))//' was not found in the list '// &
     &         'DBOBJ%LIS_STA' )
           RETURN
      END IF
!
      IF ( IP2 .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(ISITE(2)), STR )
           CALL ERR_LOG ( 6736, IUER, 'WR_OBSER', 'Trap of internal control: '// &
     &         'site '//STR(1:I_LEN(STR))//' was not found in the list '// &
     &         'DBOBJ%LIS_STA' )
           RETURN
      END IF
!
! --- Putting information in SCAN-part of the data structures
!
      IF ( OBSHLD%SCA .NE. K_SCA ) THEN
!
! -------- This observation is from the next scan
!
           OBSSCA(K_SCA)%FJD    = FJD
           OBSSCA(K_SCA)%FRACTC = FRACTC
           OBSSCA(K_SCA)%FRACT  = FRACT
           OBSSCA(K_SCA)%ISTAR  = ISTAR
           OBSSCA(K_SCA)%DERTAU_BAS(1) = BP(1,1,1)
           OBSSCA(K_SCA)%DERTAU_BAS(2) = BP(2,1,1)
           OBSSCA(K_SCA)%DERTAU_BAS(3) = BP(3,1,1)
!
! -------- Update status data structure
!
           OBSHLD%SCA   = OBSHLD%SCA + 1
           CALL NOUT ( 4*MO_STA, OBSHLD%STA )
      END IF
!
! --- Putting information in the first STATION-part of the data structures
!
      IF ( OBSHLD%STA(IP1) .LE. 0 ) THEN
           OBSSTA(IP1,K_SCA)%ELEV               =  ELEV(1)
           OBSSTA(IP1,K_SCA)%AZIM               =  AZ(1)
           OBSSTA(IP1,K_SCA)%DERTAU_TRO         = -AP(1,1)    ! NB: sign
           OBSSTA(IP1,K_SCA)%DERTAU_TRO_GRAD(1) =  AGRAD_PART(1,1,1)
           OBSSTA(IP1,K_SCA)%DERTAU_TRO_GRAD(2) =  AGRAD_PART(1,2,1)
           OBSSTA(IP1,K_SCA)%DERTAU_AXOF        =  AXOFP(1,1)  ! NB: sign
           OBSSTA(IP1,K_SCA)%TEMPC              =  TEMPC(1)
           OBSSTA(IP1,K_SCA)%ATMPR              =  ATMPR(1)
           OBSSTA(IP1,K_SCA)%RELHU              =  RELHU(1)
           OBSSTA(IP1,K_SCA)%IND_SCA            =  K_SCA
!
! -------- Update status data structure
!
           OBSHLD%STA(IP1) = K_SCA
      END IF
!
! --- Putting information in the second STATION-part of the data structures
!
      IF ( OBSHLD%STA(IP2) .LE. 0 ) THEN
           OBSSTA(IP2,K_SCA)%ELEV               =  ELEV(2)
           OBSSTA(IP2,K_SCA)%AZIM               =  AZ(2)
           OBSSTA(IP2,K_SCA)%DERTAU_TRO         =  AP(2,1)      ! NB: sign
           OBSSTA(IP2,K_SCA)%DERTAU_TRO_GRAD(1) =  AGRAD_PART(2,1,1)
           OBSSTA(IP2,K_SCA)%DERTAU_TRO_GRAD(2) =  AGRAD_PART(2,2,1)
           OBSSTA(IP2,K_SCA)%DERTAU_AXOF        = -AXOFP(2,1)  ! NB: sign
           OBSSTA(IP2,K_SCA)%TEMPC              =  TEMPC(2)
           OBSSTA(IP2,K_SCA)%ATMPR              =  ATMPR(2)
           OBSSTA(IP2,K_SCA)%RELHU              =  RELHU(2)
           OBSSTA(IP2,K_SCA)%IND_SCA            =  K_SCA
!
! -------- Update status data structure
!
           OBSHLD%STA(IP2) = K_SCA
      END IF
!
! --- Putting information in BASELINE-part of the data structures
!
      OBSBAS(K_OBS)%TAU_C        = DT*1.D-6
      OBSBAS(K_OBS)%FRE_C        = RT
!
      OBSBAS(K_OBS)%TAUGR_OBS    = DOBS*1.D-6
      OBSBAS(K_OBS)%TAUGR_ERR    = DERR
      OBSBAS(K_OBS)%TAUSB_OBS    = DNB*1.D-6
      OBSBAS(K_OBS)%TAUSB_ERR    = DNBER
      OBSBAS(K_OBS)%TAUPH_OBS    = DPH*1.D-6
      OBSBAS(K_OBS)%TAUPH_ERR    = DPHER
      OBSBAS(K_OBS)%RATE_OBS     = ROBS
      OBSBAS(K_OBS)%FREQ_IONO_GR = EFFREQ*1.D6
      OBSBAS(K_OBS)%FREQ_IONO_PH = PHEFFREQ*1.D6
      OBSBAS(K_OBS)%FREQ_OBSV_PH = FREQ_SKY*1.D6
      OBSBAS(K_OBS)%LQUAL_CHR    = LQUAL_CHR
!
      OBSBAS(K_OBS)%TAUGR_OBS_OPP    = DOBSXS*1.D-6
      OBSBAS(K_OBS)%TAUGR_ERR_OPP    = DERRXS
      OBSBAS(K_OBS)%TAUSB_OBS_OPP    = DNB_S*1.D-6
      OBSBAS(K_OBS)%TAUSB_ERR_OPP    = DNBER_S
      OBSBAS(K_OBS)%RATE_OBS_OPP     = ROBSXS
      OBSBAS(K_OBS)%TAUPH_OBS_OPP    = DPHXS*1.D-6
      OBSBAS(K_OBS)%TAUPH_ERR_OPP    = DPHERXS
      OBSBAS(K_OBS)%FREQ_IONO_GR_OPP = EFFREQ_XS*1.D6
      OBSBAS(K_OBS)%FREQ_IONO_PH_OPP = PHEFFREQ_XS*1.D6
      IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET1__BIT ) ) THEN
!
! -------- There is no information about reference frequency for S-band in the
! -------- database. Take default value
!
           OBSBAS(K_OBS)%FREQ_OBSV_PH_OPP = 2.21299D9
         ELSE
           IF ( PHAMI8_S .LT. 1.D-12 ) PHAMI8_S = -1.0 ! Correction
           OBSBAS(K_OBS)%FREQ_OBSV_PH_OPP = 1.D0/ ( PHAMI8_S*1.D-6 )
      END IF
      OBSBAS(K_OBS)%LQUAL_CHR_OPP    = LQUALXS_CHR
      IF ( OBSBAS(K_OBS)%LQUAL_CHR_OPP == ' 0' .AND. &
     &     OBSBAS(K_OBS)%FREQ_IONO_GR_OPP  < 1.D7  ) THEN
           OBSBAS(K_OBS)%FREQ_IONO_GR_OPP  = 2.21299D9 ! Default
      END IF
      IF ( OBSBAS(K_OBS)%LQUAL_CHR == ' 0' .AND. &
     &     OBSBAS(K_OBS)%FREQ_IONO_GR < 1.D7  ) THEN
           OBSBAS(K_OBS)%FREQ_IONO_GR = 8.3D9 ! Default
      END IF
!
! --- Putting other baseline-dependent information
!
      OBSBAS(K_OBS)%DERTAU_SOU(1) = SP(1,1)
      OBSBAS(K_OBS)%DERTAU_SOU(2) = SP(2,1)
      OBSBAS(K_OBS)%DERTAU_EOP(1) = ROTP(1,1)
      OBSBAS(K_OBS)%DERTAU_EOP(2) = ROTP(2,1)
      OBSBAS(K_OBS)%DERTAU_EOP(3) = ROTP(3,1)
      OBSBAS(K_OBS)%DERTAU_EOP(4) = NUTP(1,1)
      OBSBAS(K_OBS)%DERTAU_EOP(5) = NUTP(2,1)
      OBSBAS(K_OBS)%DERTAU_GAM    = RELP(1)
      OBSBAS(K_OBS)%DERTAU_PRE    = PRCP(1)
!
      OBSBAS(K_OBS)%TAU_COR       = TAU_COR
      OBSBAS(K_OBS)%TAUGR_ERR_COR = TAUGR_ERR_COR
      OBSBAS(K_OBS)%TAUPH_ERR_COR = TAUPH_ERR_COR
!
      OBSBAS(K_OBS)%ISITE(1)      = ISITE(1)
      OBSBAS(K_OBS)%ISITE(2)      = ISITE(2)
      OBSBAS(K_OBS)%IUNW          = IUNW
      OBSBAS(K_OBS)%IUNWP         = IUNWP
      OBSBAS(K_OBS)%ICORR         = ICORR
      OBSBAS(K_OBS)%SUPSTAT(1)    = SUPSTAT(1)
      OBSBAS(K_OBS)%SUPSTAT(2)    = SUPSTAT(2)
      OBSBAS(K_OBS)%UACSUP        = UACSUP
      OBSBAS(K_OBS)%AUTO_SUP      = AUTO_SUP
      OBSBAS(K_OBS)%USER_SUP      = USER_SUP
      OBSBAS(K_OBS)%USER_REC      = USER_REC
      OBSBAS(K_OBS)%GIONSG(1)     = GIONSG(1)  
      OBSBAS(K_OBS)%GIONSG(2)     = GIONSG(2)  
      OBSBAS(K_OBS)%DTEC_ADJ      = DTEC_ADJ
      OBSBAS(K_OBS)%TEC_APR       = TEC_APR
      OBSBAS(K_OBS)%DTEC_ERR      = DTEC_ERR
      OBSBAS(K_OBS)%DTEC_FLG      = DTEC_FLG
      OBSBAS(K_OBS)%DEL_BIAS_UL   = DEL_BIAS_UL
      OBSBAS(K_OBS)%FILLER(1)     = ILAST_OBORG_I2
      OBSBAS(K_OBS)%IND_SCA       = K_SCA
!
! --- Update status data structure
!
      OBSHLD%OBS = K_OBS
!
      RES(K_OBS)%AMBION_SP   = 0.D0
      RES(K_OBS)%AMB_SP      = 0.D0
      RES(K_OBS)%NUMAMB_USED = 0
!
      IF ( DATYP_INQ ( IDATYP, GROUP__DTP ) ) THEN
!
! -------- Group delay
!
           IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
!
! ------------- We know, that we use opposite band. We imply that it is S-band
!
                RES(K_OBS)%AMB_SP      = FAMB_S
                RES(K_OBS)%AMBION_SP   = FAMB_S
                RES(K_OBS)%NUMAMB_USED = INT4( NUMAMB_S )
             ELSE
                RES(K_OBS)%AMB_SP      = FAMB
                RES(K_OBS)%AMBION_SP   = FAMB
                RES(K_OBS)%NUMAMB_USED = INT4( NUMAMB )
           END IF
!
           IF ( DATYP_INQ ( IDATYP, G_GXS__DTP ) ) THEN
                IF ( DABS(EFFREQ**2 - EFFREQ_XS**2) > 1.0D0 ) THEN
                     RES(K_OBS)%AMBION_SP = RES(K_OBS)%AMB_SP* &
     &                                      EFFREQ**2/ ( EFFREQ**2 - EFFREQ_XS**2 )
                   ELSE
!
! ------------------ Effective frequencies are bogus. Put some number to avoid
! ------------------ division by zero
!
                     RES(K_OBS)%AMBION_SP = 1.0D-7
                END IF
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay
!
           IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
!
! ------------- We know, that we use opposite band. We imply that it is S-band
!
                RES(K_OBS)%AMB_SP         = PHAMI8_S*1.D-6
                RES(K_OBS)%AMBION_SP      = PHAMI8_S*1.D-6
                RES(K_OBS)%NUMAMB_USED    = NPHAM4_S
                RES(K_OBS)%NUMAMB_GR_USED = NUMAMB_S
             ELSE
                RES(K_OBS)%AMB_SP         = PHAMI8*1.D-6
                RES(K_OBS)%AMBION_SP      = PHAMI8*1.D-6
                RES(K_OBS)%NUMAMB_USED    = NPHAM4
                RES(K_OBS)%NUMAMB_GR_USED = NUMAMB
           END IF
!
           IF ( DATYP_INQ ( IDATYP, P_PXS__DTP ) ) THEN
                RES(K_OBS)%AMBION_SP = RES(K_OBS)%AMB_SP* &
     &                   PHEFFREQ**2/ ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
           END IF
      END IF
      RES(K_OBS)%NUMAMB_NEW = RES(K_OBS)%NUMAMB_USED
      RES(K_OBS)%NUMAMB_GR_NEW = RES(K_OBS)%NUMAMB_GR_USED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WR_OBSER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RD_OBSER ( IRD, NN_SCA, NN_STA, N_OBS, K_OBS, &
     &                      TAU_COR, TAUGR_ERR_COR, TAUPH_ERR_COR, &
     &                      DBOBJ, OBSSCA, OBSSTA, OBSBAS, RES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RD_OBSER  reads data structures OBSSCA, OBSSTA, OBSBAS    *
! *   and then transfers information from them to the certain fields of  *
! *   the oborg area.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IRD ( INTEGER*4 ) -- Mode of work. The only value 1 is          *
! *                           supported nowdays.                         *
! *   NN_SCA ( INTEGER*4 ) -- Maximal expected number of scans in        *
! *                           database.                                  *
! *   NN_STA ( INTEGER*4 ) -- Maximal expected number of stations in     *
! *                           database.                                  *
! *    N_OBS ( INTEGER*4 ) -- Maximal expected number of observations.   *
! *    K_OBS ( INTEGER*4 ) -- Number of the observations to be read.     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   OBSSCA ( RECORD    ) -- Array of data structures which keeps       *
! *                           scan-dependent information about the       *
! *                           session. Dimension: NN_SCA.                *
! *   OBSSTA ( RECORD    ) -- Array of data structures which keeps       *
! *                           station dependent information about the    *
! *                           session. Dimension: (NN_STA,NN_SCA).       *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session. Dimension: NN_OBS.                *
! *      RES ( RECORD    ) -- Array of data structures keeping           *
! *                           information about residuals. Dimension:    *
! *                           NN_OBS.                                    *
! *       TAU_COR ( REAL*8    ) -- Correction to delay due to            *
! *                                observation dependent calibration.    *
! * TAUGR_ERR_COR ( REAL*8    ) -- Quadratically additive correction     *
! *                                to the group delay weights.           *
! * TAUPH_ERR_COR ( REAL*8    ) -- Quadratically additive correction     *
! *                                to the phase delay weights.           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  15-SEP-97     RD_OBSER   v3.10 (c)  L. Petrov  31-DEC-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'obser.i'
!
      INTEGER*4  IRD, NN_SCA, NN_STA, N_OBS, K_OBS, IUER
      REAL*8     TAU_COR, TAUGR_ERR_COR, TAUPH_ERR_COR
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(NN_SCA)
      TYPE ( STA_O__STRU ) ::  OBSSTA(NN_STA,NN_SCA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      CHARACTER  STR*20
      INTEGER*4  K_SCA, K_SCA1, K_SCA2, IP1, IP2
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL ::  KBIT
      INTEGER*4, EXTERNAL ::  I_LEN, IFIND_PL
!
! --- Test of some input parameters
!
      IF ( NN_STA .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NN_STA, STR )
           CALL ERR_LOG ( 6741, IUER, 'RD_OBSER', 'Papameter NN_STA '// &
     &         'has uncorrect value: NN_STA = '//STR )
           RETURN
      END IF
!
      IF ( NN_STA .GT. MO_STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MO_STA, STR )
           CALL ERR_LOG ( 6742, IUER, 'RD_OBSER', 'Papameter NN_STA '// &
     &         'exceeded the upper limit specified in obser.i: MO_STA = '// &
     &          STR )
           RETURN
      END IF
!
      IF ( K_OBS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_OBS, STR )
           CALL ERR_LOG ( 6743, IUER, 'RD_OBSER', 'Papameter K_OBS '// &
     &         'has uncorrect value: K_OBS = '//STR )
           RETURN
      END IF
!
      IF ( K_OBS .GT. MO_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MO_OBS, STR )
           CALL ERR_LOG ( 6744, IUER, 'RD_OBSER', 'Papameter K_SCA '// &
     &         'exceeded the upper limit specified in obser.i: MO_SCA = '// &
     &          STR )
           RETURN
      END IF
!
! --- Getting information in BASELINE-part of the data structures
!
      DT        = OBSBAS(K_OBS)%TAU_C*1.D6
      RT        = OBSBAS(K_OBS)%FRE_C
!
      DOBS      = OBSBAS(K_OBS)%TAUGR_OBS*1.D6
      DERR      = OBSBAS(K_OBS)%TAUGR_ERR
      DNB       = OBSBAS(K_OBS)%TAUSB_OBS*1.D6
      DNBER     = OBSBAS(K_OBS)%TAUSB_ERR
      DPH       = OBSBAS(K_OBS)%TAUPH_OBS*1.D6
      DPHER     = OBSBAS(K_OBS)%TAUPH_ERR
      ROBS      = OBSBAS(K_OBS)%RATE_OBS
      EFFREQ    = OBSBAS(K_OBS)%FREQ_IONO_GR*1.D-6
      PHEFFREQ  = OBSBAS(K_OBS)%FREQ_IONO_PH*1.D-6
      REFFREQ   = OBSBAS(K_OBS)%FREQ_OBSV_PH*1.D-6
!
      DOBSXS      = OBSBAS(K_OBS)%TAUGR_OBS_OPP*1.D6
      DERRXS      = OBSBAS(K_OBS)%TAUGR_ERR_OPP
      DNB_S       = OBSBAS(K_OBS)%TAUSB_OBS_OPP*1.D6
      DNBER_S     = OBSBAS(K_OBS)%TAUSB_ERR_OPP
      DPHXS       = OBSBAS(K_OBS)%TAUPH_OBS_OPP*1.D6
      DPHERXS     = OBSBAS(K_OBS)%TAUPH_ERR_OPP
      ROBSXS      = OBSBAS(K_OBS)%RATE_OBS_OPP
      EFFREQ_XS   = OBSBAS(K_OBS)%FREQ_IONO_GR_OPP*1.D-6
      PHEFFREQ_XS = OBSBAS(K_OBS)%FREQ_IONO_PH_OPP*1.D-6
      IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET1__BIT ) ) THEN
!
! -------- There is no information about reference frequency for S-band in the
! -------- database. Take defaul value
!
           PHAMI8_S = 1.D6* ( 1.D0/2.21299D9 )
         ELSE
           PHAMI8_S = 1.D6* ( 1.D0/OBSBAS(K_OBS)%FREQ_OBSV_PH_OPP )
      END IF
      LQUALXS_CHR = OBSBAS(K_OBS)%LQUAL_CHR_OPP
!
      TAU_COR       = OBSBAS(K_OBS)%TAU_COR
      TAUGR_ERR_COR = OBSBAS(K_OBS)%TAUGR_ERR_COR
      TAUPH_ERR_COR = OBSBAS(K_OBS)%TAUPH_ERR_COR
!
      SP(1,1)     = OBSBAS(K_OBS)%DERTAU_SOU(1)
      SP(2,1)     = OBSBAS(K_OBS)%DERTAU_SOU(2)
      ROTP(1,1)   = OBSBAS(K_OBS)%DERTAU_EOP(1)
      ROTP(2,1)   = OBSBAS(K_OBS)%DERTAU_EOP(2)
      ROTP(3,1)   = OBSBAS(K_OBS)%DERTAU_EOP(3)
      NUTP(1,1)   = OBSBAS(K_OBS)%DERTAU_EOP(4)
      NUTP(2,1)   = OBSBAS(K_OBS)%DERTAU_EOP(5)
      RELP(1)     = OBSBAS(K_OBS)%DERTAU_GAM
      PRCP(1)     = OBSBAS(K_OBS)%DERTAU_PRE
      ISITE(1)    = OBSBAS(K_OBS)%ISITE(1)
      ISITE(2)    = OBSBAS(K_OBS)%ISITE(2)
      IUNW        = OBSBAS(K_OBS)%IUNW
      IUNWP       = OBSBAS(K_OBS)%IUNWP
      LQUAL_CHR   = OBSBAS(K_OBS)%LQUAL_CHR
      ICORR       = OBSBAS(K_OBS)%ICORR
      SUPSTAT(1)  = OBSBAS(K_OBS)%SUPSTAT(1)
      SUPSTAT(2)  = OBSBAS(K_OBS)%SUPSTAT(2)
      UACSUP      = OBSBAS(K_OBS)%UACSUP
      AUTO_SUP    = OBSBAS(K_OBS)%AUTO_SUP      
      USER_SUP    = OBSBAS(K_OBS)%USER_SUP      
      USER_REC    = OBSBAS(K_OBS)%USER_REC      
      K_SCA       = OBSBAS(K_OBS)%IND_SCA
      GIONSG(1)   = OBSBAS(K_OBS)%GIONSG(1)
      GIONSG(2)   = OBSBAS(K_OBS)%GIONSG(2)
      TEC_APR     = OBSBAS(K_OBS)%TEC_APR
      DTEC_ADJ    = OBSBAS(K_OBS)%DTEC_ADJ
      DTEC_ERR    = OBSBAS(K_OBS)%DTEC_ERR
      DTEC_FLG    = OBSBAS(K_OBS)%DTEC_FLG
      DEL_BIAS_UL = OBSBAS(K_OBS)%DEL_BIAS_UL   
      ILAST_OBORG_I2 = OBSBAS(K_OBS)%FILLER(1)
!
      IF ( K_SCA .LE. 0  .OR.  K_SCA .GT. NUMSCA ) THEN
           CALL CLRCH ( STR        )
           CALL INCH  ( K_SCA, STR )
           CALL ERR_LOG ( 6745, IUER, 'RD_OBSER', 'Trap of internal error: '// &
     &         'K_SCA ='//STR(1:I_LEN(STR))//' has a wrong value' )
           RETURN
      END IF
!
! --- Get indices of the site in the station list
!
      IP1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, INT4(ISITE(1)) )
      IP2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, INT4(ISITE(2)) )
!
      IF ( IP1 .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(ISITE(1)), STR )
           CALL ERR_LOG ( 6746, IUER, 'RD_OBSER', 'Trap of internal control: '// &
     &         'site '//STR(1:I_LEN(STR))//' was not found in the list '// &
     &         'DBOBJ%LIS_STA' )
           RETURN
      END IF
!
      IF ( IP2 .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(ISITE(2)), STR )
           CALL ERR_LOG ( 6747, IUER, 'RD_OBSER', 'Trap of internal control: '// &
     &         'site '//STR(1:I_LEN(STR))//' was not found in the list '// &
     &         'DBOBJ%LIS_STA' )
           RETURN
      END IF
!
! --- Getting information in the first STATION-part of the data structures
!
      ELEV(1)           = OBSSTA(IP1,K_SCA)%ELEV
      AZ(1)             = OBSSTA(IP1,K_SCA)%AZIM
      AP(1,1)           =-OBSSTA(IP1,K_SCA)%DERTAU_TRO    ! NB: sign
      AGRAD_PART(1,1,1) = OBSSTA(IP1,K_SCA)%DERTAU_TRO_GRAD(1)
      AGRAD_PART(1,2,1) = OBSSTA(IP1,K_SCA)%DERTAU_TRO_GRAD(2)
      AXOFP(1,1)        = OBSSTA(IP1,K_SCA)%DERTAU_AXOF   ! NB: sign
      TEMPC(1)          = OBSSTA(IP1,K_SCA)%TEMPC
      ATMPR(1)          = OBSSTA(IP1,K_SCA)%ATMPR
      RELHU(1)          = OBSSTA(IP1,K_SCA)%RELHU
      K_SCA1            = OBSSTA(IP1,K_SCA)%IND_SCA
!
      IF ( K_SCA1 .NE. K_SCA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_SCA1, STR )
           CALL ERR_LOG ( 6748, IUER, 'RD_OBSER', 'Trap of internal error: '// &
     &         'K_SCA1 ='//STR(1:I_LEN(STR))//' has a wrong value' )
           RETURN
      END IF
!
! --- Getting information in the second STATION-part of the data structures
!
      ELEV(2)           = OBSSTA(IP2,K_SCA)%ELEV
      AZ(2)             = OBSSTA(IP2,K_SCA)%AZIM
      AP(2,1)           = OBSSTA(IP2,K_SCA)%DERTAU_TRO    ! NB: sign
      AGRAD_PART(2,1,1) = OBSSTA(IP2,K_SCA)%DERTAU_TRO_GRAD(1)
      AGRAD_PART(2,2,1) = OBSSTA(IP2,K_SCA)%DERTAU_TRO_GRAD(2)
      AXOFP(2,1)        =-OBSSTA(IP2,K_SCA)%DERTAU_AXOF   ! NB: sign
      TEMPC(2)          = OBSSTA(IP2,K_SCA)%TEMPC
      ATMPR(2)          = OBSSTA(IP2,K_SCA)%ATMPR
      RELHU(2)          = OBSSTA(IP2,K_SCA)%RELHU
      K_SCA2            = OBSSTA(IP2,K_SCA)%IND_SCA
!
      IF ( K_SCA2 .NE. K_SCA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_SCA2, STR )
           CALL ERR_LOG ( 6749, IUER, 'RD_OBSER', 'Trap of internal error: '// &
     &         'K_SCA2 ='//STR(1:I_LEN(STR))//' has a wrong value' )
           RETURN
      END IF
!
! --- Getting information in SCAN-part of the data structures
!
      FJD    = OBSSCA(K_SCA)%FJD
      FRACTC = OBSSCA(K_SCA)%FRACTC
      FRACT  = OBSSCA(K_SCA)%FRACT
      ISTAR  = OBSSCA(K_SCA)%ISTAR
      BP(1,1,1)  =  OBSSCA(K_SCA)%DERTAU_BAS(1)
      BP(2,1,1)  =  OBSSCA(K_SCA)%DERTAU_BAS(2)
      BP(3,1,1)  =  OBSSCA(K_SCA)%DERTAU_BAS(3)
      BP(1,2,1)  = -OBSSCA(K_SCA)%DERTAU_BAS(1)
      BP(2,2,1)  = -OBSSCA(K_SCA)%DERTAU_BAS(2)
      BP(3,2,1)  = -OBSSCA(K_SCA)%DERTAU_BAS(3)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_OBSER  #!#
