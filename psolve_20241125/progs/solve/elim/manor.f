      SUBROUTINE MANOR_OPEN()
! ************************************************************************
! *                                                                      *
! *     Routine  MANOR_OPEN  opens files and reads some common areas     *
! *   needed for making normal equations.                                *
! *                                                                      *
! *  ###  12-SEP-97   MANOR_OPEN   v1.1  (c)  L. Petrov  20-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Open the NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Get info from PARFIL
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- Get info from GLBFIL
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'ORC' )
!
      CALL ACS_OBSFIL   ( 'O' )
!
! --- Open spool file and rewind it to the end
!
      CALL USE_SPOOL ( 'O' )
!
      RETURN
      END  !#!  MANOR_OPEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MANOR_INIT ( IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, EQUMEM, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  MANOR_INIT  does some actions which are necessary for   *
! *   making normal equations and which should be done before            *
! *   processing the first observation. It sets up flyby apriori, reads  *
! *   station, source a priori substitutions files, reads user partials  *
! *   file, initialize B3D (and if needed B1B3D data structures), makes  *
! *   mapping B3D and FULL parameters lists, allocates memory for        *
! *   internal fields of B3D and B1B3D data structures; calculates       *
! *   coefficients of cubic spline for interpolation high frequency EOP. *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
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
! *  ###  12-SEP-97   MANOR_INIT   v1.2  (c)  L. Petrov  15-APR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
!
      INTEGER*2  IDB2
      CHARACTER  FNAME*255
      TYPE ( PLACE__STRU  ) ::  PLACE
      TYPE ( B3D__STRU    ) ::  B3DOBJ
      TYPE ( B1B3D__STRU  ) ::  B1B3DOBJ
      TYPE ( NCREC__STRU  ) ::  NCREC
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
!
      INTEGER*2   LDBNAM(5,15), IDBV(15)
      INTEGER*4   IDBE(15)
      CHARACTER   CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
!
      INTEGER*4  IUER, FILDES, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4   I_LEN
!
! --- Get flyby a prioris and modify PARFL common accordingly
!
      CALL FLYBY_MAP_INIT()
      CALL FLYBY_APRIOR()
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      CALL NCORT_M ( IDB2, CDBNAM, NCREC )
      CALL USE_GLBFIL_4 ( 'OWC' )
!
! --- Open user_partial scratch file, if there is one
!
      IF ( KUSER_PART) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'PART'//PRE_LETRS
           CALL BIN_OPEN ( FNAME, FILDES, 'O' )
      ENDIF
!
      IF ( FAST_MODE .EQ. F__B3D  ) THEN
!
! -------- Fill internal fields of B3DOBJ
!
! -------- Transfer database name to B3DOBJ.DBNAME
!
           B3DOBJ%DBNAME = CDBNAM(IDB2)
           B3DOBJ%DBVER  = INT4 ( IDBV(IDB2) )
!
! -------- Forming field B3DOBJ.DBNAME_MES
!
           CALL CLRCH ( B3DOBJ%DBNAME_MES )
           B3DOBJ%DBNAME_MES = B3DOBJ%DBNAME
           B3DOBJ%DBNAME_MES(12:) = '<'
           CALL INCH ( B3DOBJ%DBVER, B3DOBJ%DBNAME_MES(13:) )
           B3DOBJ%DBNAME_MES( I_LEN(B3DOBJ%DBNAME_MES)+1: ) = '>'
           B3DOBJ%DBNAME_LEN = I_LEN ( B3DOBJ%DBNAME_MES )
!
! -------- Calculation map of correspondence between parameters in FULL
! -------- and in B3D matrices
!
           CALL ERR_PASS  ( IUER, IER )
           CALL MAP_PARAM ( FAST_MODE, FAST_DBG, B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6651, IUER, 'MANOR_INIT', 'Error during '// &
     &              'calculation the map of correspondence between '// &
     &              'parameters in FULL and in B3D matrices while database '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           END IF
!
           IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR. &
     &          B3DOBJ%MEM_STAT .EQ. F__MFL        ) THEN
!
! ------------- Freing dynamic memory if it was previously allocated
!
                IER = -1
                CALL B3D_FREEMEM ( B3DOBJ, IER )
              ELSE
                B3DOBJ%MEM_STAT   = F__MFR
           END IF
!
! -------- Grabbing dynamic memory for internal fields of B3DOBJ
!
           B1B3DOBJ%MEM_STAT = 0
           CALL ERR_PASS   ( IUER, IER )
           CALL B3D_GETMEM ( FAST_COV, B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6652, IUER, 'MANOR_INIT', 'Error during '// &
     &              'getting dynamic memory for data structure used in B3D '// &
     &              'algorithm while database '//B3DOBJ%DBNAME_MES// &
     &              ' was processing' )
                RETURN
           END IF
      END IF
!
! --- Calculation coefficients of cubic spline for interpolation high
! --- frequency EOP
!
      CALL ERR_PASS ( IUER, IER )
      CALL HFINT_INIT ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6653, IUER, 'MANOR_INIT', 'Error during '// &
     &         'attempt to build coefficients of cubic spline for '// &
     &         'intepolation of high frequency EOP' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MANOR_INIT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MANOR_DO ( IVRB, IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, IDBF, &
     &                      N_OBS, NN_SCA, NN_STA, &
     &                      OBSHLD, OBSSCA, OBSSTA, OBSBAS, DBOBJ, CHIOBJ, &
     &                      IRD, IWR, IACT, ITHR, IOBS, RES, EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MANOR_DO  makes different actions concerniing normal   *
! *   equations and equations of conditions in B3D or B1B3D mode for the *
! *   IDB2-th  database in SCRATCH file.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IVRB ( INTEGER*4 ) -- Verbosity level.                          *
! *                            0 -- slilent mode;                        *
! *                            2 -- progress counter will be printed.    *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *    NN_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *    NN_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *       IRD ( INTEGER*4 ) -- Code of operation of reading the          *
! *                            parameters of the observation and         *
! *                            observables.                              *
! *                            IRD = 0 -- data will be read from scratch *
! *                                       file or superfile.             *
! *                            IRD = 1 -- data will be read from OBSSCA, *
! *                                       OBSSTA, OBSBAS, DBOBJ data     *
! *                                       structures.                    *
! *       IWR ( INTEGER*4 ) -- Code of operation of writing parameters   *
! *                            of the observation and observables.       *
! *                            IWR = 0 -- data will be not be written.   *
! *                            IWR = 1 -- data will be written to OBSHLD,*
! *                                       OBSSCA, OBSSTA, OBSBAS, DBOBJ  *
! *                                       data structures.               *
! *      IACT ( INTEGER*4 ) -- Code of action.                           *
! *                            IACT=0 -- no to build normal equations.   *
! *                            IACT=1 -- building normal equations for   *
! *                                      all used observations.          *
! *                            IACT=2 -- calcultion of postfit residuals *
! *                                      for all used observations.      *
! *                            IACT=3 -- calcultion of one equation of   *
! *                                      conditions for the IOBS-th      *
! *                                      observation. Results will be    *
! *                                      kept in PLACE data structure.   *
! *                            IACT=4 -- calcultion of postfit residual  *
! *                                      for the IOBS-th observation.    *
! *                            IACT=5 -- calcultion of postfit residuals *
! *                                      for all used observations and   *
! *                                      calculation chi-square and its  *
! *                                      mathematical expectation for    *
! *                                      whole set of observations and   *
! *                                      some subsets.                   *
! *      ITHR ( INTEGER*4 ) -- Code whether to update theoretical path   *
! *                            delay.                                    *
! *                            ITHR = 0 -- not to update theoreticals.   *
! *                            ITHR = 1 -- update theoreticals.          *
! *      IOBS ( INTEGER*4 ) -- Index of the observation. Ignored when    *
! *                            IACT = 1, 2 or 5. Points on the used      *
! *                            observation when IACT = 3 or 4.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
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
! *  ###  12-SEP-1997  MANOR_DO  v3.15 (c)  L. Petrov  28-SEP-2022  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INCLUDE   'obser.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__OBS_TYPE ) :: OBS_TYP
      TYPE     ( VTD__TYPE     ), POINTER :: VTD_PTR
!
      INTEGER*2  IDB2
      INTEGER*4  IVRB, IDBF, N_OBS, NN_SCA, NN_STA, IRD, IWR, IACT, ITHR, &
     &           IOBS, IUER
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( SCA_O__STRU   ) ::  OBSSCA(NN_SCA)
      TYPE ( STA_O__STRU   ) ::  OBSSTA(NN_STA,NN_SCA)
      TYPE ( BAS_O__STRU   ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU   ) ::  RES(N_OBS)
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
      TYPE ( TCN__TYPE     ) ::  TCN(M__TCN)
      COMMON / SAVOBS / FNAME_SAVOBS, FILDES_SAVOBS, IOBS_OFF_SAVOBS
      CHARACTER FNAME_SAVOBS*(NAME_SIZE), C_STA(MAX_ARC_STA)*8
      INTEGER*4 FILDES_SAVOBS, IOBS_OFF_SAVOBS
!
      LOGICAL*4  WAS_FIRST, LG_DUMMY, NORATE_FLAG_SAVE
      LOGICAL*4  F__NEXT_COMSEG, DATYP_INQ
      LOGICAL*2  KBIT
      REAL*8     FJD_1, FRACTC_1, TT, TT_LAST, EPS_SEC, TIM_R8, &
     &           ADDW_EXT(MAX_OBS), ADDW_FRQ, ADDW_SES_SCL
      PARAMETER  ( EPS_SEC = 0.1 ) ! max acceptable diff. in order of observ.
      INTEGER*4  MANOR__VRB
      PARAMETER  ( MANOR__VRB = 1000 )
!
      REAL*8     APP(2,2), DERR_RAW, RERR_RAW, DPHER_RAW
      LOGICAL*2  POLYONLY
!
      REAL*8     TAU_CALC,      RATE_CALC,     COR_TAU,       COR_RATE, &
     &           ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X,   TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X,   TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X,   TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,    RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,     FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X,   FREQ_RATE_S,   TAU_OC,        RATE_OC, &
     &           TAU_E,         RATE_E
      REAL*8     WEIGR_GLO_SAVE,   WEIPH_GLO_SAVE, ADDERR_GR_TAU_SAVE, COR_TAU_SAVE
      REAL*8     DERIV(M_GPA,2), MARES_B3D
!
      INTEGER*4  IREC, J1, J2, J3, J4, IND_OBS, MJD_OBS, N_OBS_USE, NUMSCA_NEW, IER
      INTEGER*4  LEN_SCA, LEN_STA, LEN_BAS, LEN_HLD, LEN_RES, LEN_CHI, L_TCN, &
     &           N_WEI, IS, NE
      CHARACTER  STR*20, STR1*20, STR2*30, STRC*8, FNAME_TEST*128, STA_NAM(2)*8
      REAL*8     DEL_GPS_IONO(2), IONO_ZEN_AVR(2), IONO_ZEN_COV(3) 
      ADDRESS__TYPE :: AD_GLO, AD_SG1, AD_SG2, AD_TMP
      REAL*8     GLO_CONT, SG1_CONT, SG2_CONT, VTD_IONO_SCALE
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), DELAY_THR, &
     &           RATE_THR, PRES_VAL, TEMP_VAL, TAI_OBS, UTC_OBS, UTC_MINUS_TAI
      INTEGER*2  NOGOOD, ISTAR_LAST, IDATYP_SAVE
      CHARACTER  BUF_EDIT(MAX_OBS)*16, BUF_ADDW(MAX_OBS)*64, VTD_CONF_USE*128, &
     &           STR_DTEC_SBA_ERR_SCL*32
      INTEGER*8        MEM_LEN, SIZEOF_VTD
      ADDRESS__TYPE :: MEM_ADR
      LOGICAL*1  FL_VTD_IONO, FL_EDIT, FL_ADDW, FL_ADDW_BW, FL_ADDW_IONO, &
     &           FL_SUP_EXT(MAX_OBS)
      LOGICAL*4  FL_FIXED, FL_SUP, FL_USED, FL_RECO, FL_GOOD, FL_CUEL, &
     &           FL_DSBS, FL_DSSO, FL_FL_SET 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ, IS_R8_NAN
      REAL*8,    EXTERNAL :: DP_VV_V, CPU_TIMER, RGAUSS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IS = 1029412
      CALL CLRCH ( STR ) 
      CALL GETENVAR ( 'MANOR_ILAST_OBORG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL CHIN ( STR, IER )
           ILAST_OBORG_I2 = IER 
      END IF
!
      CALL CLRCH    (                     STR_DTEC_SBA_ERR_SCL )
      CALL GETENVAR ( 'DTEC_SBA_ERR_SCL', STR_DTEC_SBA_ERR_SCL )
      IF ( ILEN(STR_DTEC_SBA_ERR_SCL) .NE. 0 ) THEN
           IF ( STR_DTEC_SBA_ERR_SCL(1:1) == 'N' ) THEN
                DTEC_SBA_USE = .FALSE.
              ELSE 
                DTEC_SBA_USE = .TRUE.
                READ ( UNIT=STR_DTEC_SBA_ERR_SCL, FMT=* ) DTEC_ERR_SCL
           END IF
      END IF
!
      IF ( IRD .NE. 0   .OR.   IWR .NE. 0 ) THEN
!
! -------- Important tests on equality of actual and declared lentghs of data
! -------- structures
!
           IF ( NN_SCA > 1 ) THEN
                LEN_SCA = (LOC(OBSSCA(2)%ISTAR) - LOC(OBSSCA(1)%ISTAR))
                IF ( LEN_SCA .NE. ML_SCA ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( ML_SCA, STR )
                     CALL CLRCH ( STR1 )
                     CALL INCH  ( LEN_SCA, STR1 )
                     CALL ERR_LOG ( 8211, IUER, 'MANOR_DO', 'Internal error: '// &
     &                   'Declared size of OBSSCA data structure (obser.i) '// &
     &                    STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &                   'actual size: '//STR1(1:I_LEN(STR1)) )
                    RETURN
                END IF
           END IF
!
           IF ( NN_STA > 1 ) THEN
                LEN_STA = (LOC(OBSSTA(2,1)%IND_SCA) - LOC(OBSSTA(1,1)%IND_SCA))
                IF ( LEN_STA .NE. ML_STA ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( ML_STA, STR )
                     CALL CLRCH ( STR1 )
                     CALL INCH  ( LEN_STA, STR1 )
                     CALL ERR_LOG ( 8212, IUER, 'MANOR_DO', 'Internal error: '// &
     &                   'Declared size of OBSSTA data structure (obser.i) '// &
     &                    STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &                   'actual size: '//STR1(1:I_LEN(STR1)) )
                    RETURN
                END IF
           END IF
!
!@           LEN_BAS = (LOC(OBSBAS(2)%IND_SCA) - LOC(OBSBAS(1)%IND_SCA))
           LEN_BAS = SIZEOF(OBSBAS(1))
           IF ( LEN_BAS .NE. ML_BAS ) THEN
                CALL CLRCH ( STR  )
                CALL INCH  ( ML_BAS, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( LEN_BAS, STR1 )
                CALL ERR_LOG ( 8213, IUER, 'MANOR_DO', 'Internal error: '// &
     &              'Declared size of OBSBAS data structure (obser.i) '// &
     &               STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &              'actual size: '//STR1(1:I_LEN(STR1)) )
               RETURN
           END IF
!
           LEN_HLD = (LOC(OBSHLD%LAST_FIELD) - LOC(OBSHLD%FIRST_FIELD)) + 4
           IF ( LEN_HLD .NE. ML_HLD ) THEN
                CALL CLRCH ( STR  )
                CALL INCH  ( ML_HLD, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( LEN_HLD, STR1 )
                CALL ERR_LOG ( 8214, IUER, 'MANOR_DO', 'Internal error: '// &
     &              'Declared size of OBSHLD data structure (obser.i) '// &
     &               STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &              'actual size: '//STR1(1:I_LEN(STR1)) )
               RETURN
           END IF
!
           IF ( IWR .EQ. 1 ) THEN
                CALL NOUT ( ML_HLD, OBSHLD )
             ELSE
                OBSHLD%FIRST_FIELD = 0
                OBSHLD%SCA = 0
                CALL NOUT ( MO_STA*4, OBSHLD%STA )
                OBSHLD%OBS = 0
                OBSHLD%LAST_FIELD = 0
           END IF
      END IF
!
      LEN_RES = (LOC(RES(1)%END_MARKER_RES) - LOC(RES(1)%TT)) + 1
      IF ( LEN_RES .NE. ML_RES ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_RES, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_RES, STR1 )
           CALL ERR_LOG ( 8215, IUER, 'MANOR_DO', 'Internal error: '// &
     &         'Declared size of RES data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &          'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
      LEN_CHI = (LOC(CHIOBJ%LAST_FIELD) - LOC(CHIOBJ%FIRST_FIELD)) + 4
      IF ( LEN_CHI .NE. ML_CHI ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_CHI, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_CHI, STR1 )
           CALL ERR_LOG ( 8216, IUER, 'MANOR_DO', 'Internal error: '// &
     &         'Declared size of CHI data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Initializations:
!
      IF ( IACT .EQ. 1 ) THEN
!
! -------- MODE: CALCULATION OF NORMAL SYSTEM:
! -------- Clearing accumulators in internal fields of data structure B3DOBJ
!
           CALL F__CLR_IND ( 3, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
        ELSE IF ( IACT .EQ. 2 ) THEN
!
! -------- MODE: CALCULATION OF RESIDUALS
! -------- Unscaling vector of the estimates and writing unscaled vactors E(x)
! -------- to the vector Q(x) into data structure B3DOBJ
! -------- and initialization of segment counters
!
           CALL ERR_PASS ( IUER, IER )
           CALL MARESINIT_B3D ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8217, IUER, 'MANOR_DO', 'Errors in '// &
     &              'initialization' )
                RETURN 
           END IF
           CALL F__CLR_IND ( 2, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
        ELSE IF ( IACT .EQ. 3  .OR.  IACT .EQ. 4 ) THEN
!
! -------- MODE: CALCULATION OF ONE EQUATION OF CONDITIONS
! -------- Moving saved status of place from RES data structure to PLACE
!
           IF ( IOBS .GE. 1  .AND.  IOBS .LE. N_OBS ) THEN
                PLACE%CLO_SEG      = RES(IOBS)%CLO_SEG
                PLACE%ATM_SEG      = RES(IOBS)%ATM_SEG
                PLACE%EOP_SEG      = RES(IOBS)%EOP_SEG
                PLACE%CLO_SEG_LAST = RES(IOBS)%CLO_SEG_LAST
                PLACE%ATM_SEG_LAST = RES(IOBS)%ATM_SEG_LAST
                PLACE%EOP_SEG_LAST = RES(IOBS)%EOP_SEG_LAST
                PLACE%CURR_CSG     = RES(IOBS)%CURR_CSG
           END IF
        ELSE IF ( IACT .EQ. 5 ) THEN
!
! -------- MODE: CALCULATION OF RESIDUALS
! -------- Unscaling vector of the estimates and writing unscaled vectors E(x)
! -------- to the vector Q(x) into data structure B3DOBJ.
! -------- Initialization of segment counters.
! -------- Initialization of the fields in CHIOBJ data strucutre
!
           WEIGR_GLO_SAVE = CHIOBJ%WEIGR_GLO
           WEIPH_GLO_SAVE = CHIOBJ%WEIPH_GLO
           CALL ERR_PASS ( IUER, IER )
           CALL MARESINIT_B3D ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8218, IUER, 'MANOR_DO', 'Errors in '// &
     &              'initialization' )
                RETURN 
           END IF
           CALL F__CLR_IND    ( 2, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
           CALL NOUT ( ML_CHI, CHIOBJ )
           CHIOBJ%WEIGR_GLO = WEIGR_GLO_SAVE
           CHIOBJ%WEIPH_GLO = WEIPH_GLO_SAVE
        ELSE IF ( IACT .EQ. 0 ) THEN
           CONTINUE 
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( IACT, STR )
           CALL ERR_LOG ( 8219, IUER, 'MANOR_DO', 'Action '//STR(1:I_LEN(STR))// &
     &         ' is not supproted' )
           RETURN
      END IF
!
      IF ( IRD .NE. 0  .AND.  IRD .NE. 1 ) THEN
!
! -------- Reading code is incorrect
!
           CALL CLRCH ( STR )
           CALL INCH  ( IRD, STR )
           CALL ERR_LOG ( 8220, IUER, 'MANOR_DO', 'Reading code IRD '// &
     &          STR(1:I_LEN(STR))//' is not supproted' )
           RETURN
      END IF
!
      IF ( IWR .NE. 0  .AND.  IWR .NE. 1 ) THEN
!
! -------- Writing code is incorrect
!
           CALL CLRCH ( STR )
           CALL INCH  ( IWR, STR )
           CALL ERR_LOG ( 8221, IUER, 'MANOR_DO', 'Writing code IWR '// &
     &          STR(1:I_LEN(STR))//' is not supproted' )
           RETURN
      END IF
!
      IF ( ITHR .NE. 0  .AND.  ITHR .NE. 1  .AND.  ITHR .NE. 2 ) THEN
!
! -------- Writing code is incorrect
!
           CALL CLRCH ( STR )
           CALL INCH  ( IWR, STR )
           CALL ERR_LOG ( 8222, IUER, 'MANOR_DO', 'Theoreticals code ITHR '// &
     &          STR(1:I_LEN(STR))//' is not supproted' )
           RETURN
      END IF
!
! --- Determine whether or not it is necessary to take into account rates in
! --- normal equations
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           PLACE%STATUS = F__RAT
         ELSE
           PLACE%STATUS = F__DEL
      END IF
!
      N_OBS_USE = 0
      WAS_FIRST = .FALSE.
      NORATE_FLAG_SAVE = NORATE_FLAG
      NORATE_FLAG = .TRUE.
      IF ( IRD .EQ. 0 ) THEN
           CALL ACS_OBSFIL ( 'O' )
           DBOBJ%U_OBS  = 0 ! Number of used observations
           DBOBJ%R_OBS  = 0 ! Number of good, but rejected by user observations
           DBOBJ%CG_OBS = 0 ! Number of conditionally good observations
      END IF
!
! --- Check whether obs-file is the superfile or a scratch file
!
      FNAME_TEST = PRE_SCR_DIR(1:PRE_SD_LEN)//'OBSF'//PRE_LETRS
      IF ( FNAME_SAVOBS  == FNAME_TEST ) THEN
           FL_SUP = .FALSE.
         ELSE 
           FL_SUP = .TRUE.
      END IF
!
! --- Set the name of the VTD control file, if applicable
!
      CALL CLRCH ( VTD_CONF_USE )
      IF ( ITHR == 1  .OR.  ITHR == 2 ) THEN
           IF ( FL_VTD_SES ) THEN
                VTD_CONF_USE = VTD_CONF_SES
              ELSE
                IF ( FL_VTD_GLB ) THEN
                     VTD_CONF_USE = VTD_CONF_GLB
                END IF
           END IF
      END IF
!
      IF ( ILEN(VTD_CONF_USE) > 0 ) THEN
           IF ( VTD_ADR == 0 ) THEN
                SIZEOF_VTD = SIZEOF(VTD_PTR) 
                CALL ERR_PASS ( IUER, IER )
                CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                          SIZEOF_VTD, VTD_ADR )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL IINCH ( SIZEOF_VTD, STR )
                     CALL ERR_LOG ( 8223, IUER, 'MANOR_DO', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))// &
     &                   ' bytes of dynamic memory for VTD' )
                     RETURN 
                END IF
!
                VTD_STATUS = VTD__ALLC 
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_INIT ( %VAL(VTD_ADR), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8224, IUER, 'MANOR_DO', 'Failure to '// &
     &                   'initialize vtd object' )
                     RETURN 
                END IF
                VTD_STATUS = VTD__INIT
              ELSE
                MEM_LEN = 0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PRE_VTD  ( %VAL(VTD_ADR), VTD_STATUS, VTD_CONF_USE, &
     &                     FL_VTD_IONO, VTD_IONO_SCALE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8225, IUER, 'MANOR_DO', 'Failure during '// &
     &              'attempt to execute pre-vtd routines '// &
     &              'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(EDIT_FIL) > 0 ) THEN
           FL_EDIT    = .TRUE.
           FL_SUP_EXT = .FALSE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( EDIT_FIL, MAX_OBS, BUF_EDIT, NE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8226, IUER, 'MANOR_DO', 'Error in an attempt '// &
     &              'to read external edit flag file '//EDIT_FIL )
                RETURN
           END IF
           DO 410 J2=1,NE
              IF ( BUF_EDIT(J2)(1:1) == '#' ) GOTO 410
              IF ( ILEN(BUF_EDIT(J2)) == 0  ) GOTO 410
              CALL CHIN ( BUF_EDIT(J2), IND_OBS )
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   FL_SUP_EXT(IND_OBS) = .TRUE.
              END IF
 410       CONTINUE 
        ELSE 
           FL_EDIT    = .FALSE.
           FL_SUP_EXT = .FALSE.
      END IF
!
      IF ( ILEN(ADDW_FIL) > 0 ) THEN
           FL_ADDW = .TRUE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( ADDW_FIL, MAX_OBS, BUF_ADDW, N_WEI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8227, IUER, 'MANOR_DO', 'Error in an attempt '// &
     &              'to read external additive flag file '//ADDW_FIL )
                RETURN
           END IF
!
           IF ( BUF_ADDW(1)(1:LEN(LABEL__ADDW))    .NE. LABEL__ADDW    .AND. &
     &          BUF_ADDW(1)(1:LEN(LABEL__ADDW_V1)) .NE. LABEL__ADDW_V1       ) THEN
                CALL ERR_LOG ( 8228, IUER, 'MANOR_DO', 'Wrong 1st line of the '// &
     &              'external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//TRIM(BUF_ADDW(1))//' while '//LABEL__ADDW// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_ADDW(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                CALL ERR_LOG ( 8229, IUER, 'MANOR_DO', 'Experiment name '// &
     &              'defined in external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//BUF_ADDW(3)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBNAME_CH )
                RETURN
           END IF
           ADDW_EXT = 0.0D0
           FL_ADDW_IONO = .FALSE.
           FL_ADDW_BW   = .FALSE.
           DO 420 J2=1,N_WEI
              IF ( BUF_ADDW(J2)(1:29) == '# Baseline weights: included'    ) FL_ADDW_BW = .TRUE.
              IF ( BUF_ADDW(J2)(1:31) == '# Baseline weights: independent' ) FL_ADDW_BW = .FALSE.
              IF ( BUF_ADDW(J2)(1:1) == '#' ) GOTO 420
              IF ( ILEN(BUF_ADDW(J2)) == 0  ) GOTO 420
              CALL CHIN ( BUF_ADDW(J2)(10:15), IND_OBS )
              IF ( BUF_ADDW(J2)(1:32) == '# Average Ionospheric frequency:' ) THEN
                   READ ( UNIT=BUF_ADDW(J2)(34:46), FMT='(F13.5)' ) ADDW_FRQ
                   FL_ADDW_IONO = .TRUE.
              END IF
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   READ ( UNIT=BUF_ADDW(J2)(27:38), FMT='(D12.5)' ) ADDW_EXT(IND_OBS)
              END IF
   420     CONTINUE 
        ELSE 
           FL_ADDW_IONO = .FALSE.
           FL_ADDW = .FALSE.
           ADDW_EXT = 0.0D0
      END IF
!
! --- Fix station names
!
      DO 430 J3=1,NUMSTA
         C_STA(J3) = ISITN_CHR(J3)
         CALL VTD_NAME_REPAIR ( C_STA(J3) )
 430  CONTINUE 
      IF ( ILEN(TEC_NOISE_FILE) > 0 ) THEN
           CALL ERR_PASS  ( IUER, IER ) 
           CALL PARSE_TCN ( TEC_NOISE_FILE, M__TCN, TCN, L_TCN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8230, IUER, 'MANOR_DO', 'Error in '// &
     &              'an attempt to parse GPS TEC noise file '// &
     &               TEC_NOISE_FILE )
                RETURN
           END IF
         ELSE 
           L_TCN = 0
      END IF
!
      DO 440 J4=1,N_OBS
         IF ( ( IACT .EQ. 1  .OR.  IACT .EQ. 2  .OR.  IACT .EQ. 5 ) .AND. &
     &        IVRB .GE. 2 ) THEN
!
              IF ( MOD(J4,MANOR__VRB) .EQ. 0  .OR.  J4 .EQ. N_OBS ) THEN
!
! ---------------- Print observations counter on the screen
!
                   CALL CLRCH  ( STRC      )
                   CALL INCH   ( J4, STRC  )
                   CALL CHASHR ( STRC      )
                   CALL PRCH   ( STRC      )
                   CALL CURL   ( LEN(STRC) )
              END IF
         END IF
!
         IREC = IDBF + J4-1
!
! ------ Skipping not matching observation in one-observation mode
!
         IF ( ( IACT .EQ. 3 .OR. IACT .EQ. 4 )  .AND.  J4 .NE. IOBS ) GOTO 440
         FL_FL_SET = .FALSE.
         IF ( IRD .EQ. 0 ) THEN
!
! ----------- Option: reading observations form OBS-file
!
              CALL USE_OBSFIL ( IOBSFIL, IREC, 'R' )
!
              IF ( ISITE(1) .LE. 0   .OR.  ISITE(2) .LE. 0 ) THEN
                   WRITE ( 6, * ) ' before flyby isite(1)=',isite(1), &
     &                    ' isite(2)=',isite(2)
                   CALL ERR_LOG ( 8231, IUER, 'MANOR_DO', 'Error during '// &
     &                 'analysis of '//B3DOBJ%DBNAME_MES//' : some scratch '// &
     &                 'files should be updated' )
                   NORATE_FLAG = NORATE_FLAG_SAVE
                   RETURN
              END IF
              IF ( ILEN(VTD_CONF_USE) > 0 ) THEN
!   
! ---------------- Load meteorological parameters of the first station
! ---------------- into the VTD record
!
                   PRES_VAL = ATMPR(1)*100.0D0
                   TEMP_VAL = TEMPC(1) + 273.16D0
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(1)), &
     &                                 PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8232, IUER, 'MANOR_DO', 'Error in '// &
     &                      'an attempt to load meteorological parameters '// &
     &                      'for station '//C_STA(ISITE(1)) )
                        RETURN
                   END IF
!
! ---------------- Load meteorologial parameters for the second station
!
                   PRES_VAL = ATMPR(2)*100.0D0
                   TEMP_VAL = TEMPC(2) + 273.16D0
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(2)), &
     &                                 PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8233, IUER, 'MANOR_DO', 'Error in '// &
     &                      'an attempt to load meteorological parameters '// &
     &                      'for station '//C_STA(ISITE(2)) )
                        RETURN
                   END IF
!
! ---------------- NB: we need to transform FJD+FRACTC by parts, otherwise
! ---------------- we will lose precision whcih will result in an additional
! ---------------- noise in delay with rms 10-15 ps!!!
!
                   CALL JD_TO_MJD_SEC  ( FJD, MJD_OBS, UTC_OBS )
                   UTC_OBS = UTC_OBS + FRACT*86400.0D0
                   IF ( J4 == 1 ) THEN
!
! --------------------- This trick is done since VLBI formatter strores pseudo-UTC.
! --------------------- We need to record UTC-TAI(t) at the beginning of the
! --------------------- experiment and apply it to all observations, regardless
! --------------------- whether the new clock jump took place duing the experiment
!
                        CALL VTD_UTC_TO_TAI ( %VAL(VTD_ADR), MJD_OBS, UTC_OBS, &
     &                                        TAI_OBS, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 8234, IUER, 'MANOR_DO', 'Error '// &
     &                            'in an attempt to get UTC-minus TAI' )
                             RETURN
                        END IF
                        UTC_MINUS_TAI = UTC_OBS - TAI_OBS
                        IF ( UTC_MINUS_TAI >  43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI - 86400.0D0
                        IF ( UTC_MINUS_TAI < -43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI + 86400.0D0
                   END IF
                   UTC_M_TAI = UTC_MINUS_TAI
                   TAI_OBS = UTC_OBS - UTC_M_TAI
!
! ---------------- Set fields of OBS_TYP
!
                   CALL SET_OBSTYP ( OBS_TYP )
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_DELAY ( ISTRN_CHR(ISTAR), C_STA(ISITE(1)), &
     &                              C_STA(ISITE(2)), MJD_OBS, TAI_OBS, &
     &                              OBS_TYP, %VAL(VTD_ADR), DELAY_THR, &
     &                              RATE_THR, DER_DEL, DER_RAT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8235, IUER, 'MANOR_DO', 'Error '// &
     &                      'in an attempt to compute VLBI time delay' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL POST_VTD ( J4, %VAL(VTD_ADR), TRP_USE, %VAL(ADR_TRP), &
     &                             STS_TRP, DELAY_THR, RATE_THR, DER_DEL, &
     &                             DER_RAT, FL_NO_IONO_DEL, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8236, IUER, 'MANOR_DO', 'Error '// &
     &                      'in an attempt to collect results of VTD' )
                        RETURN
                   END IF
                   IF ( .NOT. FL_SUP ) THEN
                        CALL USE_OBSFIL ( IOBSFIL, IREC, 'W' )
                   END IF
              END IF
!
              FL_FIXED = .FALSE.
!
! ----------- Try to fix pathological combination of suppression flags
!
              IF (       KBIT ( ICORR, INT2(1) ) .AND. &
     &                   KBIT ( ICORR, INT2(4) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(2) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(6) ) .AND. &
     &             DABS(GIONSG(1)) .GT. 1.D-20         ) THEN
!
! ---------------- Correction: if ionosphere flags indicate that the ionosphere
! ---------------- correction was available, but the matching observation had
! ---------------- quality code 1-7, we say that it was "good", but
! ---------------- the observation was manually suppressed if and only if its
! ---------------- suppression flag was not zero. This trick allows us to
! ---------------- restore this observation in future if a user would find
! ---------------- it is desirable.
!
                   IF ( IUNW .NE. INT2(0) ) THEN
                        IUNW  = INT2(1)
                        IUNWP = INT2(1)
                   END IF
                   CALL SBIT ( ICORR, INT2(1), INT2(0) )
                   CALL SBIT ( ICORR, INT2(7), INT2(0) )
                   IF ( SUPMET .NE.  SUPMET__META ) THEN
                        CALL UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
                        CALL SBIT ( SUPSTAT, GION__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO1__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO3__SPS, INT2(0) )
                   END IF
                   FL_FIXED = .TRUE.
              END IF
              IF (       KBIT ( ICORR, INT2(3) ) .AND. &
     &                   KBIT ( ICORR, INT2(4) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(2) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(6) ) .AND. &
     &             DABS(GIONSG(1)) .GT. 1.D-20         ) THEN
!
! ---------------- Correction: if ionosphere flags indicate that the ionosphere
! ---------------- correction was available, but the matching observation had
! ---------------- quality code 1-7, we say that it was "good", but
! ---------------- the observation was manually suppressed if and only if its
! ---------------- suppression flag was not zero. This trick allows us to
! ---------------- restore this observation in future if a user would find
! ---------------- it is desirable.
!
                   IF ( IUNW .NE. INT2(0) ) THEN
                        IUNW  = INT2(1)
                        IUNWP = INT2(1)
                   END IF
                   CALL SBIT ( ICORR, INT2(3), INT2(0) )
                   CALL SBIT ( ICORR, INT2(9), INT2(0) )
                   IF ( SUPMET .NE.  SUPMET__META ) THEN
                        CALL UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
                        CALL SBIT ( SUPSTAT, GION__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO1__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO3__SPS, INT2(0) )
                   END IF
                   FL_FIXED = .TRUE.
              END IF
!
              IF (       KBIT ( ICORR, INT2(5) ) .AND. &
     &                   KBIT ( ICORR, INT2(4) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(2) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(6) ) .AND. &
     &             DABS(GIONSG(1)) .GT. 1.D-20         ) THEN
!
! ---------------- Correction: if the ionosphere flags indicate that the
! ---------------- ionosphere correction was available, but "bad", we say that
! ---------------- it was "good", but the observation was manually suppressed
! ---------------- (if it had IUNW not zero). This trick allows us to restore
! ---------------- this observation in future if user would find it is
! ---------------- desirable.
!
                   IUNW  = INT2(1)
                   IUNWP = INT2(1)
                   CALL SBIT ( ICORR, INT2( 5), INT2(0) )
                   CALL SBIT ( ICORR, INT2(11), INT2(0) )
                   IF ( SUPMET .NE.  SUPMET__META ) THEN
                        CALL UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
                        CALL SBIT ( SUPSTAT, GION__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO1__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO3__SPS, INT2(0) )
                   END IF
                   FL_FIXED = .TRUE.
              END IF
!
              IF (       KBIT ( ICORR, INT2(4) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(2) ) .AND. &
     &             .NOT. KBIT ( ICORR, INT2(6) ) .AND. &
     &             IUNW .EQ. INT2(8)             .AND. &
     &             DABS(GIONSG(1)) .GT. 1.D-20         ) THEN
!
! ---------------- Fixing a pathological case: ionosphere calibration is
! ---------------- avialable, but IUNW=8
!
                   IUNW  = INT2(1)
                   IUNWP = INT2(1)
                   IF ( SUPMET .NE.  SUPMET__META ) THEN
                        CALL UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
                        CALL SBIT ( SUPSTAT, GION__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO1__SPS, INT2(0) )
                        CALL SBIT ( SUPSTAT, GIO3__SPS, INT2(0) )
                   END IF
                   FL_FIXED = .TRUE.
              END IF
!
              IF ( FL_FIXED  .AND.  .NOT. FL_SUP ) THEN
!
! ---------------- Write down the record if this is not a superfile
!
                   CALL USE_OBSFIL ( IOBSFIL, IREC, 'W' )
              END IF
!
! ----------- Making flyby calibration: adding to DT (theoretical time delay)
! ----------- and to RT (theoretical delay rate) some corrections:
! -----------
! ----------- 1) station substitution
! ----------- 2) source substitution
! ----------- 3) precession-nutation substitution (7 terms)
! ----------- 4) substitution nutation daily offsets
! ----------- 5) UT1/PM substitution
! ----------- 6) High-frequency EOP parameters
!
              CALL FLYBY_MAP()
!
              IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D  ) THEN
!
! ---------------- Store corrected DT and RT (theoretical for delay and rate)
!
                   B3DOBJ%FIRST_FIELD = INT4(IDB2)
                   B3DOBJ%DT(J4) = DT
                   B3DOBJ%RT(J4) = RT
              END IF
!
              IF ( INDEX ( '123456789', LQUAL_CHR(2:2)    ) .GT. 0  .OR. &
     &             INDEX ( '123456789', LQUALXS_CHR(2:2)  ) .GT. 0  .OR. &
     &             SUPMET .EQ. SUPMET__PRE91                             ) THEN
!
! ---------------- Calculation different mapping functions for using them in
! ---------------- partials on troposphere delay in zenith direction
!
                   CALL ATMPART_M ( IDB2, NCREC )
!
! ---------------- Making calibration: adding to DT (theoretical time delay)
! ---------------- and to RT (theoretical delay rate) some corrections:
!
! ---------------- 1) observation dependent contributions where requested;
! ---------------- 2) non-flyby calibrations;
! ---------------- 3) Apply the selected flyby calibrations:
! ---------------- 4) Searching over stations and across the calibration bits
! ----------------    in JCAFFL, and apply the calibrations where requested.
! ----------------    Signs have been selected in SDBH
! ---------------- 5) Add troposphere noise based on average atmosphere delay
! ----------------    (roughly elevation dependent)
! ---------------- 6) add ionosphere calibration and modify errors;
! ---------------- 7) setting flag of goodness of the observation due to
! ----------------    ionosphere
! ---------------- 8) Apply reweighting constants
!
                   CALL SOCAL_M ( IDB2, NCREC, APP, DERR_RAW, RERR_RAW, &
     &                      DPHER_RAW, TAU_CALC, RATE_CALC, COR_TAU, &
     &                      COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &                      TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &                      TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &                      TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &                      RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &                      FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &                      FREQ_RATE_X, FREQ_RATE_S, TAU_OC, RATE_OC, TAU_E, &
     &                      RATE_E, NOGOOD )
                 ELSE
!
! ---------------- Bad quality code: no detection at both bands
!
                   TAU_CALC     = DT/1.D6
                   RATE_CALC    = RT
                   TAUGR_OBS_X  = DOBS/1.D6
                   TAUGR_OBS_S  = DOBSXS/1.D6
                   TAUPH_OBS_X  = DPH/1.D6
                   TAUPH_OBS_S  = DPHXS/1.D6
                   TAUSB_OBS_X  = DNB/1.D6
                   TAUSB_OBS_S  = DNB_S/1.D6
                   TAUGR_ERR_X  = DERR
                   TAUGR_ERR_S  = DERRXS
                   TAUPH_ERR_X  = DPHER
                   TAUPH_ERR_S  = DPHERXS
                   TAUSB_ERR_X  = DNBER
                   TAUSB_ERR_S  = DNBER_S
                   RATE_OBS_X   = ROBS
                   RATE_OBS_S   = ROBSXS
                   RATE_ERR_X   = RERR
                   RATE_ERR_S   = RERRXS
                   IF ( .NOT. FL_EQUAL_EFF_FREQ ) THEN
                        FREQ_GR_X    = EFFREQ*1.D6
                        FREQ_GR_S    = EFFREQ_XS*1.D6
                        FREQ_PH_X    = PHEFFREQ*1.D6
                        FREQ_PH_S    = PHEFFREQ_XS*1.D6
                        FREQ_RATE_X  = REFFREQ*1.D6
                        FREQ_RATE_S  = REFFREQ_XS*1.D6
                      ELSE
                        FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
                        FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
                        FREQ_PH_X    = MEAN_EFF_FREQ(3)*1.D6
                        FREQ_PH_S    = MEAN_EFF_FREQ(4)*1.D6
                        FREQ_RATE_X  = MEAN_EFF_FREQ(5)*1.D6
                        FREQ_RATE_S  = MEAN_EFF_FREQ(6)*1.D6
                   END IF
!
                   COR_TAU       = 0.0D0
                   COR_RATE      = 0.0D0
                   ADDERR_GR_TAU = 0.0D0
                   ADDERR_PH_TAU = 0.0D0
                   ADDERR_RATE   = 0.0D0
!
                   TAU_OC  = TAUGR_OBS_X - TAU_CALC
                   RATE_OC = RATE_OBS_X  - RATE_CALC
              END IF
!
! ----------- Special place! We subtract a prioi clock model correction
! ----------- from theoretical tau add it to COR_TAU. The same for rate.
! ----------- We do it to keep "pure theoreticals tau and rate" when they
! ----------- are needed for some corrections
!
              COR_TAU = COR_TAU + TAU_ACM
              TAU_CALC = TAU_CALC - TAU_ACM
              DT = ( DT*1.D-6 - TAU_ACM  ) *1.D6
!
              COR_RATE = COR_RATE + RATE_ACM
              RATE_CALC = RATE_CALC - RATE_ACM
              RT = RT - RATE_ACM
!
! ----------- Test of the order of the observations
!
              IF ( WAS_FIRST ) THEN
                   TT = (FJD - FJD_1) + (FRACTC - FRACTC_1)
                   IF ( (TT_LAST - TT)*86400.0D0 .GT. EPS_SEC ) THEN
                      CALL CLRCH ( STR  )
                      CALL INCH  ( J4, STR )
                      CALL CLRCH ( STR1 )
                      WRITE ( UNIT=STR1, FMT='(F20.6)' ) (TT_LAST - TT)*86400.D0
                      CALL CHASHL  ( STR1 )
                      CALL ERR_LOG ( 8237, IUER, 'MANOR_DO', 'Wrong order '// &
     &                   'of observations detected in the session '// &
     &                    B3DOBJ%DBNAME_MES//' : '//STR(1:I_LEN(STR))// &
     &                   '-th observation occured BEFORE the previous '// &
     &                   'one at '//STR1(1:I_LEN(STR1))// &
     &                   ' sec. Database file should be cured!!!' )
                      NORATE_FLAG = NORATE_FLAG_SAVE
                      RETURN
                   END IF
                   IF ( DABS(TT_LAST - TT)*86400.0D0 .GT. EPS_SEC  .OR. &
     &                  ISTAR_LAST .NE. ISTAR ) THEN
!
                        NUMSCA_NEW = NUMSCA_NEW + 1
                   END IF
                   TT_LAST = TT
                   ISTAR_LAST = ISTAR
                 ELSE
!
! ---------------- It is the first analyzed observation
!
                   FJD_1      = FJD
                   FRACTC_1   = FRACTC
                   TT_LAST    = 0.D0
                   ISTAR_LAST = ISTAR
                   WAS_FIRST  = .TRUE.
                   NUMSCA_NEW = 1
              END IF
!
! ----------- Setting flags of suppression status
!
              IF ( SUPMET == SUPMET__META ) THEN
                   CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
                   IF ( FL_EDIT ) THEN
                        IF ( FL_SUP_EXT(J4) ) THEN
                             AUTO_SUP = IBSET ( AUTO_SUP, INT4(EXTS__SPS) )
                           ELSE 
                             IF ( .NOT. BTEST ( AUTO_SUP, INT4(EXTS__SPS) ) ) THEN
                                  USER_SUP = IBCLR ( USER_SUP, INT4(IDATYP) )
                             END IF
                        END IF
                   END IF
!
                   FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       USED__SPS )
                   FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       RECO__SPS )
                   FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       GOOD__SPS )
                   FL_CUEL = BTEST ( AUTO_SUP, INT4(CUEL__SPS) )
                   FL_DSBS = BTEST ( AUTO_SUP, INT4(DSBS__SPS) )
                   FL_DSSO = BTEST ( AUTO_SUP, INT4(DSSO__SPS) )
                 ELSE
                   CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                                ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                                NCREC%JSITI, NCREC%ITT, ISTAR, ELEV, &
     &                                KIONO, SNR, SNR_S, SUPSTAT, UACSUP )
!
! ---------------- Determine: is this observation used?
!
                   FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                   FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
                   FL_GOOD = SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )
                   FL_CUEL = SUPR_INQ ( SUPSTAT, UACSUP, CUEL__SPS )
                   FL_DSBS = SUPR_INQ ( SUPSTAT, UACSUP, DSBS__SPS )
                   FL_DSSO = SUPR_INQ ( SUPSTAT, UACSUP, DSSO__SPS )
              END IF
              FL_FL_SET = .TRUE.
!
! ----------- Increment various observation counters
!
              IF ( FL_USED ) THEN
                   DBOBJ%U_OBS = DBOBJ%U_OBS + 1
                ELSE IF ( FL_RECO ) THEN
                   DBOBJ%R_OBS = DBOBJ%R_OBS + 1
              END IF
              IF ( FL_GOOD ) THEN
                   DBOBJ%CG_OBS = DBOBJ%CG_OBS + 1
              END IF
!
              IF ( IREC .EQ. IDBF ) THEN
!
! ---------------- Storing the date of the first observation
!
                   DBOBJ%FJD_F = FJD
                   DBOBJ%UTC_F = FRACT*86400.D0
                   DBOBJ%TCT_F = FRACTC*86400.D0
              END IF
              IF ( IREC .EQ. IDBF+N_OBS-1 ) THEN
!
! ---------------- Storing the date of the last observation
!
                   DBOBJ%FJD_L = FJD
                   DBOBJ%UTC_L = FRACT*86400.D0
                   DBOBJ%TCT_L = FRACTC*86400.D0
                   DBOBJ%SES_SPAN = DBOBJ%TCT_L - DBOBJ%TCT_F
              END IF
            ELSE  IF ( IRD .EQ. 1 ) THEN
!
! ----------- Option: reading observaions from OBSER data structures
!
              CALL ERR_PASS ( IUER, IER )
              CALL RD_OBSER ( IRD, NN_SCA, NN_STA, N_OBS, J4, &
     &                        COR_TAU, ADDERR_GR_TAU, ADDERR_PH_TAU, DBOBJ, &
     &                        OBSSCA, OBSSTA, OBSBAS, RES, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH   ( STR )
                   CALL INCH    ( J4, STR )
                   CALL ERR_LOG ( 8238, IUER, 'MANOR_DO', 'Error '// &
     &                 'during reading '//STR(1:I_LEN(STR))//'-th '// &
     &                 'observation of the database '//B3DOBJ%DBNAME_MES// &
     &                 ' from obser data structure' )
                   NORATE_FLAG = NORATE_FLAG_SAVE
                   RETURN
              END IF
              IF ( J4 .EQ. 1 ) THEN
                   FJD_1    = FJD
                   FRACTC_1 = FRACTC
              END IF
!
! ----------- Extraction information from oborg
!
              TAU_CALC     = DT/1.D6
              RATE_CALC    = RT
              TAUGR_OBS_X  = DOBS/1.D6
              TAUGR_OBS_S  = DOBSXS/1.D6
              TAUPH_OBS_X  = DPH/1.D6
              TAUPH_OBS_S  = DPHXS/1.D6
              TAUSB_OBS_X  = DNB/1.D6
              TAUSB_OBS_S  = DNB_S/1.D6
              TAUGR_ERR_X  = DERR
              TAUGR_ERR_S  = DERRXS
              TAUPH_ERR_X  = DPHER
              TAUPH_ERR_S  = DPHERXS
              TAUSB_ERR_X  = DNBER
              TAUSB_ERR_S  = DNBER_S
              RATE_OBS_X   = ROBS
              RATE_OBS_S   = ROBSXS
              RATE_ERR_X   = RERR
              RATE_ERR_S   = RERRXS
              IF ( .NOT. FL_EQUAL_EFF_FREQ ) THEN
                   FREQ_GR_X    = EFFREQ*1.D6
                   FREQ_GR_S    = EFFREQ_XS*1.D6
                   FREQ_PH_X    = PHEFFREQ*1.D6
                   FREQ_PH_S    = PHEFFREQ_XS*1.D6
                   FREQ_RATE_X  = REFFREQ*1.D6
                   FREQ_RATE_S  = REFFREQ_XS*1.D6
                 ELSE
                   FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
                   FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
                   FREQ_PH_X    = MEAN_EFF_FREQ(3)*1.D6
                   FREQ_PH_S    = MEAN_EFF_FREQ(4)*1.D6
                   FREQ_RATE_X  = MEAN_EFF_FREQ(5)*1.D6
                   FREQ_RATE_S  = MEAN_EFF_FREQ(6)*1.D6
              END IF
!
              IF ( IS_R8_NAN ( FREQ_GR_X ) ) THEN
!
! ---------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ---------------- with Not-A-Numbers
!
                   FREQ_GR_X = 8.2D9
              END IF
              IF ( IS_R8_NAN ( FREQ_GR_S ) ) THEN
!
! ---------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ---------------- with Not-A-Numbers
!
                   FREQ_GR_S = 2.2D9
              END IF
!
              IF ( IS_R8_NAN ( FREQ_PH_X ) ) THEN
!
! ---------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ---------------- with Not-A-Numbers
!
                   FREQ_PH_X = 8.2D9
              END IF
              IF ( IS_R8_NAN ( FREQ_PH_S ) ) THEN
!
! ---------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ---------------- with Not-A-Numbers
!
                   FREQ_PH_S = 2.2D9
              END IF
!
              IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) .AND. &
     &             FREQ_GR_X < FREQ__SOLVE_MIN  .OR. FREQ_GR_S < FREQ__SOLVE_MIN ) THEN
                   FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
                   FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
              END IF
!
! ----------- Aprioi clock model is approximately equal correction to delay
! ----------- due to different calibrations
!
              TAU_ACM = COR_TAU
!
! ----------- There is no saved information about rate calibration
!
              COR_RATE      = 0.0D0
              ADDERR_RATE   = 0.0D0
         END IF
!
         IF ( .NOT. FL_FL_SET ) THEN
!
! ----------- Set flags related to the suppression status
!
              IF ( SUPMET == SUPMET__META ) THEN
                   CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
                   FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       USED__SPS )
                   FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       RECO__SPS )
                   FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       GOOD__SPS )
                   FL_CUEL = BTEST ( AUTO_SUP, INT4(CUEL__SPS) )
                   FL_DSBS = BTEST ( AUTO_SUP, INT4(DSBS__SPS) )
                   FL_DSSO = BTEST ( AUTO_SUP, INT4(DSSO__SPS) )
                ELSE
                   CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                                ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                                NCREC%JSITI, NCREC%ITT, ISTAR, ELEV, &
     &                                KIONO, SNR, SNR_S, SUPSTAT, UACSUP )
!
! ---------------- Determine: is this observation used?
!
                   FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                   FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
                   FL_GOOD = SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )
                   FL_CUEL = SUPR_INQ ( SUPSTAT, UACSUP, CUEL__SPS )
                   FL_DSBS = SUPR_INQ ( SUPSTAT, UACSUP, DSBS__SPS )
                   FL_DSSO = SUPR_INQ ( SUPSTAT, UACSUP, DSSO__SPS )
              END IF
         END IF
!
         IF ( FL_VTD_IONO .AND. L_TCN > 0 ) THEN
              STA_NAM(1) = ISITN_CHR(ISITE(1))
              STA_NAM(2) = ISITN_CHR(ISITE(2))
              CALL VTD_NAME_REPAIR ( STA_NAM(1) )
              CALL VTD_NAME_REPAIR ( STA_NAM(2) )
!
              CALL ERR_PASS ( IUER, IER )
              CALL GET_IONO_AVR_COV ( %VAL(VTD_ADR), &
     &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                 IONO_ZEN_AVR, IONO_ZEN_COV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8239, IUER, 'MANOR_DO', 'Error in an '// &
     &                 'attempt to extract ionosphere path delay '// &
     &                 'statistics' )
                   RETURN 
              END IF
              DEL_GPS_IONO(1) = DER_DEL(VTD__IONO1)
              DEL_GPS_IONO(2) = DER_DEL(VTD__IONO2)
              CALL APPLY_TCN ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                         DEL_GPS_IONO, FREQ_GR_X, ELEV, DER_DEL, &
     &                         ADDERR_GR_TAU )
         END IF
         IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) .AND. &
     &        FREQ_GR_X < FREQ__SOLVE_MIN  .OR. FREQ_GR_S < FREQ__SOLVE_MIN ) THEN
              FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
              FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
         END IF
!
! ------ Making O-C
!
         IF ( FL_ADDW ) THEN
              ADDW_SES_SCL = ADDW_SCL
              IF ( FL_ADDW_IONO ) THEN
!
! ---------------- Case when we have additive weigth corrections due to ionospheric 
! ---------------- contribution. These additive weigtht corrections are compuited 
! ---------------- at frequency ADDW_FRQ which in general not the same as 
! ---------------- the effecive ionospheric frequency of the obsevation.
! ---------------- Therefore, we scale that additive weight correction by 
! ---------------- the ratio of frequencies
!
                   IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) ) THEN
                        ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ))**2
                      ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                        ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ_XS))**2
                   END IF
              END IF
!
! ------------- Apply external additive in quadrature reciprocal weight correction
!
                IF ( FL_ADDW_BW ) THEN
!
! ------------------ Baseline weights are considered included in the external weights.
! ------------------ The total baseline weight is the maximum among the baseline weight and
! ------------------ additive weight.
!
                     ADDERR_GR_TAU = MAX ( ADDW_SES_SCL*ADDW_EXT(J4), ADDERR_GR_TAU )
                   ELSE
!
! ------------------ External weights and baseline weights are considered independent
!
                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (ADDW_SES_SCL*ADDW_EXT(J4))**2 )
                END IF
         END IF
         ADDERR_GR_TAU_SAVE = ADDERR_GR_TAU
         COR_TAU_SAVE       = COR_TAU
         IF ( FUSED_STATUS == IONOV__LOADED   .AND. &
     &        .NOT. FL_SUP_EXT(J4)            .AND. &
     &        ( DATYP_INQ ( IDATYP, FUSED__DTP  ) .OR. ILAST_OBORG_I2 == 7777 ) ) THEN
!
! ----------- Compute delay corrections and additive in quadrature uncertainty
! ----------- in the fused data type
!
              IF ( BTEST ( DTEC_FLG, DTHL__STS ) ) THEN
!
! ---------------- Both band provided data usable when the ionospheric mode was computed
!
                   CONTINUE 
                 ELSE IF ( BTEST ( DTEC_FLG, DTH__STS ) ) THEN
!
! ---------------- Only the upper band provided data usable when the ionospheric mode was computed
!
                   COR_TAU = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                                 VIO__CONST/FREQ_GR_X**2 - &
     &                                 DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                   ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
                 ELSE IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
!
! ---------------- Only the lower band has provided data usable when the ionospheric mode is computed
!
                   COR_TAU = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                                  VIO__CONST/FREQ_GR_S**2 - &
     &                                  DEL_BIAS_UL*FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                   ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
              END IF
         END IF
         IF ( DTEC_SBA_USE ) THEN
!
! ----------- A special mode when the contribution of the TEC adjustment is applied
!
              IF ( DATYP_INQ ( IDATYP, GX__DTP  ) ) THEN
!
! ---------------- Upper band
!
                   COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_X**2
                   ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
!!    write  ( 6, * ) 'MANOR-1516 j4= ', j4, ' bas= ', isitn_chr(isite(1))//' / '//isitn_chr(isite(2)), ' d_del= ', sngl(dtec_adj* vio__const/freq_gr_x**2), ' d_err= ', sngl(dtec_err*vio__const/freq_gr_x**2), ' dtec= ', sngl(dtec_adj), sngl(dtec_err)  ! %%%%%%%%%%%%
              END IF
              IF ( DATYP_INQ ( IDATYP, GS__DTP  ) ) THEN
!
! ---------------- Low band
!
                   COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_S**2
                   ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
              END IF
         END IF
!
         IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTH__STS ) ) THEN
              IDATYP_SAVE = IDATYP
              IDATYP = GX__DTP 
         END IF
         IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTL__STS ) ) THEN
              IDATYP_SAVE = IDATYP
              IDATYP = GS__DTP 
         END IF
!
         CALL MAKE_OC ( TAU_CALC, RATE_CALC, COR_TAU, &
     &        COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &        TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &        TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &        TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &        RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &        FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &        FREQ_RATE_X, FREQ_RATE_S, &
     &        AUTO_SUP, USER_SUP, USER_REC, DTEC_FLG, IDATYP, OPP_STATUS, &
     &        PAMB_STATUS, TAU_OC, RATE_OC, TAU_E, RATE_E )
!
         IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTH__STS ) ) IDATYP = IDATYP_SAVE 
         IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTL__STS ) ) IDATYP = IDATYP_SAVE 
!
         ADDERR_GR_TAU = ADDERR_GR_TAU_SAVE 
         COR_TAU       = COR_TAU_SAVE       
!
         IF ( IWR .EQ. 1 ) THEN
!
! ----------- Writing observables and some partials in OBSER data structures
!
              CALL ERR_PASS ( IUER , IER )
              CALL WR_OBSER ( IWR, NN_SCA, NUMSCA_NEW, NN_STA, N_OBS, J4, &
     &                        COR_TAU, ADDERR_GR_TAU, ADDERR_PH_TAU, DBOBJ, &
     &                        OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH   ( STR )
                   CALL INCH    ( J4, STR )
                   CALL ERR_LOG ( 8240, IUER, 'MANOR_DO', 'Error '// &
     &                 'during writing '//STR(1:I_LEN(STR))//'-th '// &
     &                 'observation of the database '//B3DOBJ%DBNAME_MES// &
     &                 ' to obser data structure' )
                   NORATE_FLAG = NORATE_FLAG_SAVE
                   RETURN
              END IF
         END IF
!
! ------ Calculate the partial derivatives and an equation of conditions
!
         IF ( ( IACT.EQ.1 .AND. FL_USED ) .OR. &
     &        ( ( IACT.EQ.2   .OR. IACT.EQ.3  .OR. &
     &            IACT.EQ.4   .OR. IACT.EQ.5       )  .AND. &
     &            .NOT. FL_CUEL                       .AND. &
     &            .NOT. FL_DSBS                       .AND. &
     &            .NOT. FL_DSSO                    )        ) THEN
              IF ( IACT .EQ. 1  .OR.  IACT .EQ. 2  .OR.  IACT .EQ. 5 ) THEN
!
! ---------------- Storing current status of PLACE into RES data structure
!
                   RES(J4)%CLO_SEG      = PLACE%CLO_SEG
                   RES(J4)%ATM_SEG      = PLACE%ATM_SEG
                   RES(J4)%EOP_SEG      = PLACE%EOP_SEG
!
                   RES(J4)%CLO_SEG_LAST = PLACE%CLO_SEG_LAST
                   RES(J4)%ATM_SEG_LAST = PLACE%ATM_SEG_LAST
                   RES(J4)%EOP_SEG_LAST = PLACE%EOP_SEG_LAST
!
                   RES(J4)%CURR_CSG     = PLACE%CURR_CSG
              END IF
!
              IF ( EQUMEM%USE_FLAG .AND. EQUMEM%STATUS .EQ. EQM__DON ) THEN
                   CALL F__CLR_IND ( 0, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
                   CALL ERR_PASS   ( IUER, IER )
                   CALL EQUMEM_GET ( J4, EQUMEM, PLACE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH   ( STR )
                        CALL INCH    ( J4, STR )
                        WRITE ( 6, * ) ' IACT = ', IACT
                        CALL ERR_LOG ( 8241, IUER, 'MANOR_DO', 'Error '// &
     &                      'during attempt to get the equation of '// &
     &                      'conditions for the '//STR(1:I_LEN(STR))// &
     &                      '-th observation of the database '// &
     &                      B3DOBJ%DBNAME_MES//' from EQUMEM data structure' )
                        NORATE_FLAG = NORATE_FLAG_SAVE
                        RETURN
                   END IF
                 ELSE
                   CALL F__CLR_IND ( 0, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
                   CALL PARTL ( DERIV, POLYONLY, PLACE, B3DOBJ )
                   IF ( EQUMEM%USE_FLAG ) THEN
!
! --------------------- Put the equation of conditions to the EQUMEM data
! --------------------- structure
!
                        CALL ERR_PASS   ( IUER, IER )
                        CALL EQUMEM_PUT ( J4, EQUMEM, PLACE, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH   ( STR )
                             CALL INCH    ( J4, STR )
                             CALL ERR_LOG ( 8242, IUER, 'MANOR_DO', 'Error '// &
     &                           'during attempt to put the equation of '// &
     &                           'conditions for the '//STR(1:I_LEN(STR))// &
     &                           '-th observation of the database '// &
     &                            B3DOBJ%DBNAME_MES//' to EQUMEM data '// &
     &                           'structure' )
                             NORATE_FLAG = NORATE_FLAG_SAVE
                             RETURN
                        END IF
                   END IF
              END IF
            ELSE IF ( &
     &          .NOT. FL_CUEL .AND. &
     &          .NOT. FL_DSBS .AND. &
     &          .NOT. FL_DSSO .AND. &
     &           ( IACT .EQ. 1  .OR.  IACT .EQ. 2  .OR.  IACT .EQ. 5 ) .AND. &
     &          EQUMEM%USE_FLAG .AND. EQUMEM%STATUS .EQ. EQM__INI ) THEN
!
! ------------- Observation is suppressed, but EQUMEM data structure are not yet
! ------------- filled. We create the equation of conditions in order to
! ------------- put it in EQUMEM
!
                CALL F__CLR_IND ( 0, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
                CALL PARTL ( DERIV, POLYONLY, PLACE, B3DOBJ )
                IF ( EQUMEM%USE_FLAG ) THEN
!
! ------------------ Put the equation of conditions to the EQUMEM data structure
!
                     CALL ERR_PASS   ( IUER, IER )
                     CALL EQUMEM_PUT ( J4, EQUMEM, PLACE, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH   ( STR )
                          CALL INCH    ( J4, STR )
                          CALL ERR_LOG ( 8243, IUER, 'MANOR_DO', 'Error '// &
     &                        'during attempt to put the equation of '// &
     &                        'conditions for the '//STR(1:I_LEN(STR))// &
     &                        '-th observation of the database '// &
     &                         B3DOBJ%DBNAME_MES//' to EQUMEM data '// &
     &                        'structure' )
                          NORATE_FLAG = NORATE_FLAG_SAVE
                          RETURN
                      END IF
                END IF
         END IF
!
         IF ( IACT .EQ. 1 .AND. FL_USED ) THEN
!
! --------- Increment normal equations for delay measurements
!
            IF ( FAST_MODE .EQ. F__B3D ) THEN
!
!--------------- Variant for B3D algorithm
!                ~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------------- Test: has we reached the next segment boundary?
!
                 IF ( F__NEXT_COMSEG  ( PLACE, B3DOBJ ) ) THEN
!
! ----------------- Yes. Update normal equations using segmented accumulators
!
                    CALL NSG_B3D    ( PLACE%PREV_CSG, PLACE%LAST_CSG, B3DOBJ )
!
! ----------------- Clearing segmented accumulators
!
                    CALL F__CLR_IND ( 1, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
                 END IF
!
                 IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ----------------- Update segmented accumulators (delay)
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADD_B3D  ( F__DEL, TAU_OC, TAU_E, PLACE, B3DOBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J4, STR )
                         CALL ERR_LOG ( 8244, IUER, 'MANOR_DO', 'Error '// &
     &                       'during processing '//STR(1:I_LEN(STR))// &
     &                       '-th observation of the database '// &
     &                        B3DOBJ%DBNAME_MES )
                         NORATE_FLAG = NORATE_FLAG_SAVE
                         RETURN
                    END IF
                 END IF
                 IF ( PLACE%STATUS .EQ. F__RAT ) THEN
!
! ----------------- Update segmented accumulators (rate)
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADD_B3D ( F__RAT, RATE_OC, RATE_E, PLACE, B3DOBJ, IER)
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J4, STR )
                         CALL ERR_LOG ( 8245, IUER, 'MANOR_DO', 'Error '// &
     &                       'during processing '//STR(1:I_LEN(STR))//'-th '// &
     &                       'observation of the database '// &
     &                        B3DOBJ%DBNAME_MES )
                         NORATE_FLAG = NORATE_FLAG_SAVE
                         RETURN
                    END IF
                 END IF
               ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
!--------------- Variant for B1B3D algorithm
!                ~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------------- Test: have we reached the next segment boundary?
!
                 IF ( F__NEXT_COMSEG ( PLACE, B3DOBJ ) ) THEN
!
! ----------------- Yes. Updated normal equations using segmented accumulators
!
                    CALL NSG_B1B3D ( PLACE%PREV_CSG, PLACE%LAST_CSG, B3DOBJ, &
     &                               B1B3DOBJ )
!
! ----------------- Clearing segmented accumulators
!
                    CALL F__CLR_IND  ( 1, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
                 END IF
!
                 IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ----------------- Update segmented accumulators
!
                    CALL ERR_PASS  ( IUER, IER )
                    CALL ADD_B1B3D ( F__DEL, TAU_OC, TAU_E, PLACE, B3DOBJ, &
     &                               B1B3DOBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J4, STR )
                         CALL ERR_LOG ( 8246, IUER, 'MANOR_DO', 'Error '// &
     &                       'during processing '//STR(1:I_LEN(STR))//'-th '// &
     &                       'observation of the database '// &
     &                        B3DOBJ%DBNAME_MES )
                         NORATE_FLAG = NORATE_FLAG_SAVE
                         RETURN
                    END IF
                 END IF
!
                 IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
!
! ----------------- Update segmented accumulators (rate)
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADD_B1B3D ( F__RAT, RATE_OC, RATE_E, PLACE, B3DOBJ, &
     &                               B1B3DOBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J4, STR )
                         CALL ERR_LOG ( 8247, IUER, 'MANOR_DO', 'Error '// &
     &                       'during processing '//STR(1:I_LEN(STR))// &
     &                       '-th observation of the database '// &
     &                        B3DOBJ%DBNAME_MES )
                         NORATE_FLAG = NORATE_FLAG_SAVE
                         RETURN
                    END IF
                 END IF
                 N_OBS_USE = N_OBS_USE + 1
            END IF
           ELSE IF ( ( IACT .EQ. 2  .OR.  IACT .EQ. 5 ) .AND. &
     &                 .NOT. FL_CUEL                    .AND. &
     &                 .NOT. FL_DSBS                    .AND. &
     &                 .NOT. FL_DSSO                          ) THEN
!
! --------- Calculation of residuals
!
            IF ( FAST_MODE .EQ. F__B3D  ) THEN
!
! -------------- Update segment counters
!
                 LG_DUMMY = F__NEXT_COMSEG ( PLACE, B3DOBJ )
!
! -------------- Calculation of residual
!
                 CALL ERR_PASS ( IUER, IER )
                 RES(J4)%PSF_DEL = MARES_B3D ( F__DEL, TAU_OC, PLACE, B3DOBJ, &
     &                                         IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8248, IUER, 'MANOR_DO', 'Errors in '// &
     &                    'initialization' )
                      RETURN 
                 END IF
                 IF ( LQUAL_CHR == ' B' .AND. LQUAL_S_CHR == ' B' ) THEN
                      RES(J4)%PSF_DEL = 0.0D0
                 END IF 
                 IF ( LQUAL_CHR == ' C' .AND. LQUAL_S_CHR == ' C' ) THEN
                      RES(J4)%PSF_DEL = 0.0D0
                 END IF 
                 IF ( LQUAL_CHR == ' 0' .AND. LQUAL_S_CHR == ' 0' ) THEN
                      RES(J4)%PSF_DEL = 0.0D0
                 END IF 
                 IF ( IS_R8_NAN ( RES(J4)%PSF_DEL ) ) THEN
                      WRITE ( 6, * ) ' effreq = ',effreq
                      WRITE ( 6, * ) ' tau_calc = ',tau_calc
                      WRITE ( 6, * ) ' adderr_gr_tau = ',adderr_gr_tau
                      WRITE ( 6, * ) ' taugr_obs_x = ',taugr_obs_x
                      WRITE ( 6, * ) ' taugr_obs_s = ',taugr_obs_s
                      WRITE ( 6, * ) ' taugr_err_x = ',taugr_err_x
                      WRITE ( 6, * ) ' taugr_err_s = ',taugr_err_s
                      WRITE ( 6, * ) ' tau_oc = ',tau_oc,' tau_e = ',tau_e
                      WRITE ( 6, * ) ' freq_gr_x = ', freq_gr_x
                      WRITE ( 6, * ) ' freq_gr_s = ', freq_gr_s
                      WRITE ( 6, * ) ' freq_ph_x = ', freq_ph_x
                      WRITE ( 6, * ) ' freq_ph_s = ', freq_ph_s
                      WRITE ( 6, * ) ' freq_rate_x = ', freq_rate_x
                      WRITE ( 6, * ) ' freq_rate_s = ', freq_rate_s
                      WRITE ( 6, * ) ' reffreq    = ', reffreq
                      WRITE ( 6, * ) ' reffreq_xs = ', reffreq_xs
                      WRITE ( 6, * ) ' qual=  X: >'//LQUAL_CHR//'<< S: >>'//LQUAL_S_CHR//'<< '
                      WRITE ( 6, * ) ' manor j1 = ',j1,' postfit is NaN'
!
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 8249, IUER, 'MANOR_DO', 'Error '// &
     &                    'during calculation residuals for the '// &
     &                     STR(1:I_LEN(STR))//'-th observation of the '// &
     &                    'database '//B3DOBJ%DBNAME_MES//'  postfit '// &
     &                    'residual is Not-A-Number!' )
                      NORATE_FLAG = NORATE_FLAG_SAVE
                      RETURN
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 8250, IUER, 'MANOR_DO', 'Error '// &
     &                    'during calculation residuals for the '// &
     &                     STR(1:I_LEN(STR))//'-th observation of the '// &
     &                    'database '//B3DOBJ%DBNAME_MES )
                      NORATE_FLAG = NORATE_FLAG_SAVE
                      RETURN
                 END IF
!
! -------------- Putting information into RES data structure
!
                 RES(J4)%TT        = (FJD - FJD_1) + (FRACTC - FRACTC_1)
                 RES(J4)%OC_DEL    = TAU_OC
                 RES(J4)%WEI_DEL   = 1.D0/TAU_E
!
                 IF ( IACT .EQ. 5  .AND. FL_USED ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL CHI_UPDATE ( 1, 2, J4, IDB2, IDBF, N_OBS, NN_SCA, &
     &                     NN_STA, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, &
     &                     PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J4, STR )
                           CALL ERR_LOG ( 8251, IUER, 'MANOR_DO', 'Error '// &
     &                         'during updating chi-square and its '// &
     &                         'mathematical expectation for the '// &
     &                          STR(1:I_LEN(STR))//'-th observation of the '// &
     &                         'database '//B3DOBJ%DBNAME_MES )
                           NORATE_FLAG = NORATE_FLAG_SAVE
                           RETURN
                      END IF
                 END IF
            END IF
           ELSE IF ( ( IACT .EQ. 3  .OR. IACT .EQ. 4  )  ) THEN
            IF ( FAST_MODE .EQ. F__B3D  ) THEN
!
! -------------- Update of segment counters
!
                 LG_DUMMY = F__NEXT_COMSEG ( PLACE, B3DOBJ )
            END IF
         END IF
 440  CONTINUE
      IF ( IRD .EQ. 0 ) THEN
           CALL ACS_OBSFIL ( 'C' )
      END IF
      NORATE_FLAG = NORATE_FLAG_SAVE
      FNAME_TEST = PRE_SCR_DIR(1:PRE_SD_LEN)//'OBSF'//PRE_LETRS
!
      IF ( IACT .EQ. 1 ) THEN
         IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ----------- All opservations have been processed. Update normal matrices
! ----------- using accumulators filled during processing observations of the
! ----------- last segment.
!
              CALL NSG_B3D ( PLACE%LAST_CSG, PLACE%CURR_CSG, B3DOBJ )
           ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ----------- The same for the case of B1B3D mode
!
              CALL NSG_B1B3D ( PLACE%LAST_CSG, PLACE%CURR_CSG, B3DOBJ, B1B3DOBJ)
         END IF
!
         B3DOBJ%NOBS_T = N_OBS      !  Total number of observations
         B3DOBJ%NOBS_A = N_OBS_USE  !  Number of used observations
      END IF
!
      IF ( IACT .EQ. 4 ) THEN
!
! -------- Calculation of the residual for the IOBS-th observation
!
! -------- First of all, set up addresses for temporary keeping squeezzed
! -------- vectors of the estimates
!
           AD_GLO = B3DOBJ%AD_N00
           AD_SG1 = B3DOBJ%AD_N11
           AD_SG2 = B3DOBJ%AD_N22
           AD_TMP = B3DOBJ%AD_N10
!
! -------- Then calculate the contribution in residuals from the global
! -------- parameters. To do it
! -------- 1) Squeeze vector of the estimates throwing away all elements
! --------    which are the zeroes in the equation of condition;
! -------- 2) Squeeze the vector of the scales
! -------- 3) Multiply element-by-element vector of the estimates and vector
! --------    of the estimates.
! -------- 4) Calculate dot product global part of the equation of conditions
! --------    and modified vector of the estimates
!
           CALL DGATHER ( PLACE%N_GLO, PLACE%IND_GLO, %VAL(B3DOBJ%AD_E0), &
     &                    %VAL(AD_GLO) )
           CALL DGATHER ( PLACE%N_GLO, PLACE%IND_GLO, %VAL(B3DOBJ%AD_U0), &
     &                    %VAL(AD_TMP) )
           CALL VEC_MULT_VECTOR ( %VAL(AD_TMP), %VAL(AD_GLO), PLACE%N_GLO, &
     &                            %VAL(AD_GLO) )
           GLO_CONT = DP_VV_V ( PLACE%N_GLO, PLACE%EQU_GLO, %VAL(AD_GLO) )
!
! -------- Then calculate the contribution in residuals from the segmented
! -------- current parameters (doing analogously)
!
           CALL DGATHER ( PLACE%N_SG1, PLACE%IND_SG1, &
     &                    %VAL(B3DOBJ%AD_ES(PLACE%CURR_CSG)), %VAL(AD_SG1) )
           CALL DGATHER ( PLACE%N_SG1, PLACE%IND_SG1, &
     &                    %VAL(B3DOBJ%AD_US(PLACE%CURR_CSG)), %VAL(AD_TMP) )
           CALL VEC_MULT_VECTOR ( %VAL(AD_TMP), %VAL(AD_SG1), PLACE%N_SG1, &
     &                            %VAL(AD_SG1) )
           SG1_CONT = DP_VV_V ( PLACE%N_SG1, PLACE%EQU_SG1, %VAL(AD_SG1) )
!
! -------- Then calculate the contribution in residuals from the segmented
! -------- next parameters (doing analogously)
!
           IF ( PLACE%CURR_CSG+1 .LT. B3DOBJ%NBS ) THEN
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                         %VAL(B3DOBJ%AD_ES(PLACE%CURR_CSG+1)), &
     &                         %VAL(AD_SG2) )
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                         %VAL(B3DOBJ%AD_US(PLACE%CURR_CSG+1)), &
     &                         %VAL(AD_TMP) )
              ELSE IF ( PLACE%CURR_CSG+1 .EQ. B3DOBJ%NBS ) THEN
!
! ------------- Special case of the short segment
!
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                         %VAL(B3DOBJ%AD_ESX), %VAL(AD_SG2) )
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                         %VAL(B3DOBJ%AD_USX), %VAL(AD_TMP) )
           END IF
!
           CALL VEC_MULT_VECTOR ( %VAL(AD_TMP), %VAL(AD_SG2), PLACE%N_SG2, &
     &                            %VAL(AD_SG2) )
           SG2_CONT = DP_VV_V ( PLACE%N_SG2, PLACE%EQU_SG2, %VAL(AD_SG2) )
!
! -------- And at last, calculate the residual itself
!
           RES(IOBS)%PSF_DEL = TAU_OC - ( GLO_CONT + SG1_CONT + SG2_CONT )
!
! -------- Putting information into RES data structure
!
           RES(IOBS)%TT        = (FJD - FJD_1) + (FRACTC - FRACTC_1)
           RES(IOBS)%OC_DEL    = TAU_OC
           RES(IOBS)%WEI_DEL   = 1.D0/TAU_E
      END IF
!
      IF ( IWR .EQ. 1 ) THEN
!
! -------- Reading weights and putting them in data structure OBSHLD
!
           CALL ERR_PASS ( IUER, IER )
           CALL IO_WGT   ( 1, IDB2, DBOBJ, OBSHLD%WEIGR_BAS, OBSHLD%WEIPH_BAS, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8252, IUER, 'MANOR_DO', 'Error during '// &
     &              'putting weights to NAMFIL while database '// &
     &               DBOBJ%NAME//' was processing' )
                RETURN
           END IF
      END IF
      IF ( IACT .EQ. 1  .OR.  IACT .EQ. 2  .OR.  IACT .EQ. 5 ) THEN
!
! -------- Copying applied baseline-dependent weights from OBSHLD
! -------- data structure to the CHIOBJ
!
           CALL COPY_R8 ( MO_BAS, OBSHLD%WEIGR_BAS, CHIOBJ%WEIGR_BAS )
           CALL COPY_R8 ( MO_BAS, OBSHLD%WEIPH_BAS, CHIOBJ%WEIPH_BAS )
      END IF
!
      IF ( IACT .EQ. 1  .OR.  IACT .EQ. 2  .OR.  IACT .EQ.  5 ) THEN
           IF ( EQUMEM%USE_FLAG .AND. EQUMEM%STATUS .EQ. EQM__INI ) THEN
                EQUMEM%STATUS = EQM__DON
           END IF
      END IF
!
      IF ( ILEN(VTD_CONF_USE) > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( %VAL(VTD_ADR), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8253, IUER, 'MANOR_DO', 'Error in VTD_QUIT' )
                RETURN
           END IF
           IF ( MEM_LEN .NE. 0 ) THEN
                CALL FREE ( MEM_ADR )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MANOR_DO  #!#
