      SUBROUTINE UPWEI ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &           UPWEI_BATCH, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  UPWEI  is the part of SOLVE software for geodetic VLBI  *
! *   data analysis. UPWEI calculates corrections to weights which being *
! *   quadratically added to the uncertainties of the observables        *
! *   obtained from correlator modify a priori weights by such a way     *
! *   that the ratio of weighted sum of post-fit residuals (chi-square)  *
! *   to its mathematical expectation becomes to be near to unity.       *
! *   Weights to be applied in solving equations of conditions may be    *
! *   presented in such a form:                                          *
! *                                                                      *
! *          wgt = 1/sqrt ( sigma_apr**2 + rewgt**2 )                    *
! *                                                                      *
! *   where sigma_apr is uncertainty of the observable (or ionosphere    *
! *   free linear combination of observables) and rewgt is the quantity  *
! *   to be sought. Two modes of calculation of those quantities is      *
! *   supported: global mode and baseline mode. Rewgt is the same for    *
! *   all used observations in global mode. Reweight constant rewgt(ibl) *
! *   under condition that the ratio of weighted sum of post-fit         *
! *   residuals to its mathematical expectation over observations of the *
! *   baseline is found separately for each baseline in 'baseline mode'  *
! *   of calculation.                                                    *
! *                                                                      *
! *   It is assumed that the database or superfile has been read         *
! *   already by db_scan and the information about the session is        *
! *   already in the objects OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA,       *
! *   OBSBAS.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *                            It keeps O-C and coefficients of the      *
! *                            IOBS-th equation of conditions.           *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            postfit residuals.                        *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! *     F_CHI ( LOGICAL*4 ) -- Flag: .TRUE. means that the ratio of      *
! *                                 chi-square to its mathematical       *
! *                                 expectation has been calculated and  *
! *                                 available.                           *
! * UPWEI_BATCH ( LOGICAL*4 ) -- Flag: .TRUE. means that UPWEI is called *
! *                              in batch mode.                          *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *     Restrictions:                                                    *
! *                                                                      *
! *   1) UPWEI works only in B3D mode.                                   *
! *   2) The only type of solution which is supported now is "group      *
! *      delays only".                                                   *
! *                                                                      *
! *     History:                                                         *
! *                                                                      *
! *   WHEN       WHO  VERS  WHAT                                         *
! *                                                                      *
! *   22-JAN-98  pet  v0.0  Beginning of the developing.                 *
! *                                                                      *
! *   30-JAN-98  pet  v1.0  Release of the first version.                *
! *                                                                      *
! *   28-FEB-98  pet  v1.1  Added support of phase delay solution types. *
! *                                                                      *
! *   07-APR-99  pet  v1.2  Forced to save ELIM_VRB variable in the case *
! *                         of failure.                                  *
! *                                                                      *
! *   28-MAR-2000 pet v1.3  Added support of EQUMEM_FLAG                 *
! *                                                                      *
! *   29-MAR-2000 pet v1.4  Added support batch mode.                    *
! *                                                                      *
! *  ###  02-FEB-1998    UPWEI     v1.6  (c)  L. Petrov 07-OCT-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, IUER
!
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( SCA_O__STRU   ) ::  OBSSCA(L_SCA)
      TYPE ( STA_O__STRU   ) ::  OBSSTA(L_STA)
      TYPE ( BAS_O__STRU   ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU   ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU   ) ::  RST
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
      INTEGER*4  LEN_CHI, IER, IACT, M_OUT, L_OUT, IH, J1, &
     &           UPWEI_MAXIT, UPWEI_VRB, ELIM_VRB_SAVED
      REAL*8     UPWEI_FLO, UPWEI_CHITOL
      PARAMETER  ( M_OUT = 128*1024 )
      LOGICAL*4  F_CHI, UPWEI_BATCH, LEX
      CHARACTER  VER_UPWEI*21, UPWEI_TYPE*2, BUF_OUT(M_OUT)*80, FINAM*80, &
     &           HOSTNAME*32, DATE_RAW*24, DATE*18, STR*80, STR1*80, &
     &           GET_VERSION*54
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!
      VER_UPWEI = GET_VERSION ()
      VER_UPWEI(1:5) = 'UPWEI'
!
! --- Test of the lenght of RST data structure
!
      LEN_CHI = (LOC(CHIOBJ%LAST_FIELD) - LOC(CHIOBJ%FIRST_FIELD)) + 4
      IF ( LEN_CHI .NE. ML_CHI ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_CHI, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_CHI, STR1 )
           CALL ERR_LOG ( 6510, IUER, 'UPWEI', 'Internal error: '// &
     &         'Declared size of CHI data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Getting current date and time
!
      CALL FDATE ( DATE_RAW )
!
! --- ... and convert it to the international DEC format
!
      CALL TRAN  ( 11, DATE_RAW, DATE_RAW )
      CALL CLRCH ( DATE )
      IF ( DATE_RAW(9:9) .EQ. ' ' ) DATE_RAW(9:9) = '0'
      DATE = DATE_RAW(12:19)//' '// &
     &       DATE_RAW(9:10)//'-'//DATE_RAW(5:7)//'-'//DATE_RAW(23:24)
!
! --- Getting host name
!
      CALL CLRCH       ( HOSTNAME )
      CALL GETHOSTNAME ( HOSTNAME, %VAL(LEN(HOSTNAME)) )
      CALL CHASHL      ( HOSTNAME )
!
! --- Printing in spooll file information message
!
      WRITE ( 23, FMT='(A)' ) VER_UPWEI//'   Utility for weights '// &
     &                        'update'
      WRITE ( 23, FMT='(A)' ) 'UPWEI ran on '//HOSTNAME(1:I_LEN(HOSTNAME))// &
     &                        '  at '//DATE//' by '//PRE_LETRS
!
! --- Reading weights from NAMFIL and putting them into CHIOBJ data structure
!
      CALL ERR_PASS ( IUER, IER )
      CALL IO_WGT   ( 1, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'UPWEI', 'Error during getting '// &
     &         'weights from NAMFIL while database '// &
     &          DBOBJ%NAME//' was processing' )
           RETURN
      END IF
!
      CHIOBJ%WEIGR_GLO  = CHIOBJ%WEIGR_BAS(1)
      CHIOBJ%WEIPH_GLO  = CHIOBJ%WEIPH_BAS(1)
      CHIOBJ%LAST_FIELD = 1
!
! --- Saving ELIM_VRB status
!
      ELIM_VRB_SAVED    = ELIM_VRB
      IF ( UPWEI_BATCH ) THEN
           IACT = 1
      END IF
!
  910 CONTINUE
!
! --- Getting parameters of the work via menu interface
!
      IF ( .NOT. UPWEI_BATCH ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL UPWEI_MENU ( VER_UPWEI, IDB2, N_OBS, L_SCA, L_STA, &
     &          OBSHLD, DBOBJ, OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, &
     &          REWAY_FLODEL,  REWAY_CHITOL, REWAY_MAXIT, REWAY_TYPE, &
     &          REWAY_VERBOSE, F_CHI, IACT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6512, IUER, 'UPWEI', 'Error during getting '// &
     &              'parameters of the work via menu interface while '// &
     &              'database '//DBOBJ%NAME//' was processing' )
                ELIM_VRB = ELIM_VRB_SAVED
                RETURN
           END IF
      END IF
!
! --- Setting values of the variables which traces verbosity mode.
!
      IF ( REWAY_VERBOSE ) THEN
           UPWEI_VRB = 2
           ELIM_VRB  = 2
         ELSE
           IF ( UPWEI_BATCH ) THEN
!
! ------------- Batch mode -- let's keep silence
!
                UPWEI_VRB = 0
                ELIM_VRB  = 0
              ELSE
!
! ------------- Interactive mode -- let's say something otherwise user would
! ------------- think that UPWEI is dead
!
                UPWEI_VRB = 1
                ELIM_VRB  = 0
           END IF
      END IF
!
      UPWEI_FLO    = REWAY_FLODEL*1.D-12
      UPWEI_CHITOL = REWAY_CHITOL
      UPWEI_MAXIT  = INT4(REWAY_MAXIT)
      UPWEI_TYPE   = REWAY_TYPE
!
! --- Saving glbc4 block
!
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      IF ( IACT .EQ. 1 ) THEN
!
! -------- Iterative update of the weights
!
           CALL ERR_PASS ( IUER, IER )
           CALL UPWEI_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &          OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &          PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &          UPWEI_FLO,  UPWEI_CHITOL, UPWEI_MAXIT, UPWEI_TYPE, &
     &          UPWEI_VRB, ELIM_VRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6513, IUER, 'UPWEI', 'Error during iterative '// &
     &              'update of the weights while the database '// &
     &               DBOBJ%NAME//' was processing' )
                ELIM_VRB = ELIM_VRB_SAVED
                RETURN
           END IF
           IF ( UPWEI_VRB .GE. 2 ) THEN
                CALL HIT_CONT ( 'UPWEI finisfed iterations. Hit any key to '// &
     &                          'procceed '//CHAR(1), %VAL(0) )
           END IF
         ELSE IF ( IACT .EQ. 2 ) THEN
!
! -------- Update weights only once
!
           CALL ERR_PASS ( IUER, IER )
           CALL UPWEI_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &          OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &          PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &          UPWEI_FLO,  UPWEI_CHITOL, 1, UPWEI_TYPE, &
     &          UPWEI_VRB, ELIM_VRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6514, IUER, 'UPWEI', 'Error during '// &
     &              'update of the weights while the database '// &
     &               DBOBJ%NAME//' was processing' )
                ELIM_VRB = ELIM_VRB_SAVED
                RETURN
           END IF
           IF ( UPWEI_VRB .GE. 2 ) THEN
                CALL HIT_CONT ( 'UPWEI finisfed iterations. Hit any key to '// &
     &                          'procceed '//CHAR(1), %VAL(0) )
           END IF
         ELSE IF ( IACT .EQ. 3 ) THEN
!
! -------- Create text buffer with weights, w.r.m.s and chi/ndg
! -------- for all baselines, sources and all observations totally
!
           CALL UPWEI_INFO ( F_CHI, DBOBJ, RST, CHIOBJ, M_OUT, L_OUT, BUF_OUT )
!
! -------- Make file name for writing down this buffer
!
           CALL CLRCH ( FINAM )
           FINAM = PRE_SCR_DIR(1:PRE_SD_LEN)//'UPWEI'//PRE_LETRS
           INQUIRE ( FILE=FINAM(1:I_LEN(FINAM)), EXIST=LEX )
           IF ( LEX ) CALL SYSTEM ( 'rm '//FINAM(1:I_LEN(FINAM))//CHAR(0) )
!
! -------- Writing down this buffer in the file FINAM
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( L_OUT, BUF_OUT, FINAM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6515, IUER, 'UPWEI', 'Error in an attempt '// &
     &              'to write in the ouptu file '//FINAM )
                RETURN
           END IF
!
! -------- Displaying the buffer at the screen
!
           IER = -3
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'UPWEI statistics', 0, IER )
           GOTO 910
         ELSE IF ( IACT .EQ. 4 ) THEN
!
! -------- Create text buffer with weights, w.r.m.s and chi/ndg
! -------- for all baselines, sources and all observations totally
!
           CALL UPWEI_INFO ( F_CHI, DBOBJ, RST, CHIOBJ, M_OUT, L_OUT, BUF_OUT )
!
! -------- Writing it in SPOOL-FILE
!
           DO 410 J1=1,L_OUT
              WRITE ( 23, FMT='(A)' ) BUF_OUT(J1)(1:I_LEN(BUF_OUT(J1)))
 410       CONTINUE
!
! -------- Getting curtrent date and time
!
           CALL FDATE ( DATE_RAW )
!
! -------- ... and convert it to the international DEC format
!
           CALL TRAN  ( 11, DATE_RAW, DATE_RAW )
           CALL CLRCH ( DATE )
           IF ( DATE_RAW(9:9) .EQ. ' ' ) DATE_RAW(9:9) = '0'
           DATE = DATE_RAW(12:19)//' '// &
     &            DATE_RAW(9:10)//'-'//DATE_RAW(5:7)//'-'//DATE_RAW(23:24)
!
! -------- Writing farwell message in spool-file
!
           WRITE ( 23, FMT='(A)' ) 'UPWEI completed its work on at '//DATE
           ELIM_VRB = ELIM_VRB_SAVED
           CALL USE_GLBFIL_4 ( 'OWC' )
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( UPWEI_BATCH ) THEN
           IACT = 4
      END IF
      GOTO 910
!
      END  !#!  UPWEI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPWEI_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &           UPWEI_FLO, UPWEI_CHITOL, UPWEI_MAXIT, UPWEI_TYPE, &
     &           UPWEI_VRB, ELIM_VRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPWEI_DO makes iterative update of the weights. It         *
! *   calculates corrections to weights which being quadratically added  *
! *   to the uncertainties of the observables obtained from correlator   *
! *   modify a priori weights by such a way that the ratio of weighted   *
! *   sum of post-fit residuals or chi-square to its mathematical        *
! *   expectation (chi/ndg) becomes to be near to unity.                 *
! *                                                                      *
! *   The way of calulation is the following:                            *
! *   0) If no solution (with current weights) is available solution is  *
! *      prodiced, residuals are calculated and their statistcs.         *
! *   1) Corrections to weights are calculated using the following       *
! *      expression:                                                     *
! *                                                                      *
! *            chisq_i -  [ n_i - summa ( a_i(T)*V*a_i * w_i**2 ) ]      *
! *      q_i = ---------------------------------------------------       *
! *            summa ( w_i**2 ) - summa ( a_i(T)*V*a_i * w_i**4 )        *
! *                                                                      *
! *                                                                      *
! *      IF ( q >= 0 ) THEN                                              *
! *           rwt_i =   sqrt (  q_i )                                    *
! *        ELSE                                                          *
! *           rwt_i = - sqrt ( -q_i )                                    *
! *      END IF                                                          *
! *                                                                      *
! *      Where                                                           *
! *        a) chisq_i -- chi-square over observations of the i-th        *
! *                      baseline;                                       *
! *        b) n_i     -- the number of obvservatoins at the i-th         *
! *                      baseline;                                       *
! *        c) a_i     -- i-th equation of conditions;                    *
! *        d) V       -- covariance matrix;                              *
! *        e) w_i     -- weight of the i-th observation;                 *
! *                                                                      *
! *      Summimg is done over all used observations of the i-th          *
! *      baseline in baseline mode and over all uysed observation in     *
! *      global mode.                                                    *
! *                                                                      *
! *   2) New baseline-dependent corrected weights are strored, weights   *
! *      for each observation are updated in according with new          *
! *      corrections, OBSHLD, OBSBAS objects are updated to keep new     *
! *      weights.                                                        *
! *                                                                      *
! *   3) New solution with new weights is produced. Residuals and their  *
! *      statistics are calculated anew. Chi/ndg is calculated for each  *
! *      baseline.                                                       *
! *                                                                      *
! *   4) Test: wether new iteration should be done is carried out unless *
! *      the counter of the current iteration reached UPWEI_MAXIT.       *
! *      All baselines which                                             *
! *          a) has number of used observations more or equal to the     *
! *             specified limit ( constant NEQU_MIN );                   *
! *          b) has reweight constant applied exceeding the floor;       *
! *      tested: does chi/ndg among used observation for this baseline   *
! *      differs from unity less than UPWEI_CHITOL constant. If it       *
! *      differs from unity more for at least one baseline then new      *
! *      iteration is made.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         IDB2 ( INTEGER*2 ) -- Index of the considered database in    *
! *                               the scratch file.                      *
! *        N_OBS ( INTEGER*4 ) -- Total number of observations in the    *
! *                               session.                               *
! *        L_SCA ( INTEGER*4 ) -- Number of common scans.                *
! *        L_STA ( INTEGER*4 ) -- Number of participated stations.       *
! *        DBOBJ ( RECORD    ) -- Data structure which keeps general     *
! *                               information about the database such as *
! *                               lists of the objects.                  *
! *       OBSSCA ( RECORD    ) -- Array of data structures which keeps   *
! *                               scan-dependent information about the   *
! *                               session.                               *
! *       OBSSTA ( RECORD    ) -- Array of data structures which keeps   *
! *                               station dependent information about    *
! *                               the session.                           *
! *    UPWEI_FLO ( REAL*8    ) -- Reweiting floor: minimal acceptable    *
! *                               value of the correction to weights (in *
! *                               sec). Correction to weights will never *
! *                               be less than this value.               *
! * UPWEI_CHITOL ( REAL*8    ) -- Tolerance criterion for the iterations *
! *                               convergence: Iterations will be        *
! *                               stopped when the ratio of chi-square   *
! *                               to its mathematical expectation will   *
! *                               differ from unity by less than         *
! *                               UPWEI_CHITOL.                          *
! *  UPWEI_MAXIT ( INTEGER*4 ) -- Maximal number of iterations.          *
! *  UPWEI_TYPE  ( CHARACTER ) -- Type of reweight: 'GL' -- global,      *
! *                               baseline-independent; 'BA' --          *
! *                               baseline-dependent. Acceptable input   *
! *                               value are 'ST' (the same as BA) of     *
! *                               'NO' -- no preference. If the input    *
! *                               value of REWAY_TYPE is 'NO' then it    *
! *                               changed by UPWEI_MENU to 'GL' if is    *
! *                               all old reweight constant are the same *
! *                               for all baselines or to 'BA'           *
! *                               otherwise.                             *
! *  UPWEI_VRB  ( INTEGER*4  ) -- Verbosity level. If > 0 then some      *
! *                               intermediary infromation will be       *
! *                               printed at the screen and in           *
! *                               SPOOL-file.                            *
! *   ELIM_VRB  ( INTEGER*4  ) -- Verbosity level for ELIM. If > 0 then  *
! *                               some intermediary infromation from     *
! *                               ELIM will be printed at the screen and *
! *                               in SPOOL-file.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *                            It keeps O-C and coefficients of the      *
! *                            IOBS-th equation of conditions.           *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs of  *
! *                            postfit residuals.                        *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *     F_CHI ( LOGICAL*4 ) -- Flag: .TRUE. means that the ratio of      *
! *                                 chi-square to its mathematical       *
! *                                 expectation has been calculated and  *
! *                                 available.                           *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-JAN-98    UPWEI_DO    v1.3  (c) L. Petrov  29-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, UPWEI_MAXIT, UPWEI_VRB, ELIM_VRB, &
     &           IUER
!
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( NCREC__STRU ) ::  NCREC
      TYPE ( SCA_O__STRU ) ::  OBSSCA(L_SCA)
      TYPE ( STA_O__STRU ) ::  OBSSTA(L_STA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      LOGICAL*4  F_CHI
      CHARACTER  UPWEI_TYPE*(*)
      REAL*8     UPWEI_FLO,  UPWEI_CHITOL
!
      REAL*8     CRS_GLO2, CRS_GLO, CRS_BAS2(MO_BAS), CRS_BAS(MO_BAS), &
     &           SIG_NEW, SIG_OLD, CHI_NDG, DEV_MAX, WEI_GLO, WEI_BAS
      INTEGER*4  J1, J2, J3, J4, J5, IER, NEQU_MIN
      LOGICAL*4  ITER_CONT, DATYP_INQ
      REAL*8     FLO_MIN
      PARAMETER  ( FLO_MIN  = 1.D-14 ) ! minimal floor for reweighting constant
      PARAMETER  ( NEQU_MIN = 4      ) ! min number of observations
!                                      ! at the baseline to apply tolerance
!                                      ! criterion for continuation of the
!                                      ! iterations
      CHARACTER  STR*80
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( .NOT. F_CHI ) THEN
!
! -------- Printing status message at the screen and into SPOOL-file
!
           IF ( UPWEI_VRB .GE. 1 ) THEN
                WRITE  (  6, 110 ) 0, UPWEI_TYPE
           END IF
           WRITE  ( 23, 110 ) 0, UPWEI_TYPE
 110       FORMAT ( ' === UPWEI: Iteration ',I3,' type: ',A2 )
!
! -------- Making solution once more.
!
           CALL ERR_PASS ( IUER, IER )
           CALL SOL_INIT ( ELIM_VRB, 1, 0, N_OBS, L_STA, L_SCA, IDB2, IDBF, &
     &                     PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, DBOBJ, NCREC, &
     &                     OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, EQUMEM, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6531, IUER, 'UPWEI_DO', 'Error during '// &
     &              'making preliminary solution while the database '// &
     &               DBOBJ%NAME//' was processing' )
                RETURN
           END IF
!
           F_CHI = .TRUE.
         ELSE
           IF ( UPWEI_VRB .GE. 1 ) THEN
                WRITE  (  6, * ) '=== UPWEI: Started calculations'
           END IF
      END IF
!
      DO 410 J1=1,UPWEI_MAXIT
!
! ------ Calulation correction to the weights
!
         IF ( UPWEI_TYPE .EQ. 'GL' ) THEN
!
! ----------- Global type of correction
!
              CRS_GLO2 = ( CHIOBJ%CHI_GLO - &
     &                     ( CHIOBJ%NEQU_GLO - CHIOBJ%CHIMAT_GLO ) )/ &
     &                     ( CHIOBJ%WEI2_GLO - CHIOBJ%CHIMA4_GLO )
              IF ( CRS_GLO2 .GE. 0.0 ) THEN
                   CRS_GLO =  DSQRT(DABS(CRS_GLO2))
                 ELSE
                   CRS_GLO = -DSQRT(DABS(CRS_GLO2))
              END IF
!
! ----------- Putting new weights in CHIOBJ
!
              DO 420 J2=1,DBOBJ%L_BAS
                 IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                      SIG_OLD = 1.D0/CHIOBJ%WEIPH_BAS(J2)
                    ELSE
                      SIG_OLD = 1.D0/CHIOBJ%WEIGR_BAS(J2)
                 END IF
!
                 IF ( CRS_GLO .GE. 0.0D0 ) THEN
                      SIG_NEW = DSQRT ( SIG_OLD**2 + CRS_GLO**2 )
                   ELSE
                      IF ( (SIG_OLD**2 - CRS_GLO**2) .GE. FLO_MIN**2 ) THEN
                           SIG_NEW = DSQRT ( SIG_OLD**2 - CRS_GLO**2 )
                         ELSE
                           SIG_NEW = FLO_MIN
                      END IF
                 END IF
!
! -------------- Test: do we need substitute calculated correction by its
! -------------- floor value? If yes, do it.
!
                 IF ( SIG_NEW .LT. UPWEI_FLO ) SIG_NEW = UPWEI_FLO
                 IF ( SIG_NEW .LT. FLO_MIN   ) SIG_NEW = FLO_MIN
!
                 IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                      CHIOBJ%WEIPH_BAS(J2)  = 1.D0/SIG_NEW
                 END IF
!
! -------------- Printing message at the screen if verbose mode is set on
!
                 IF ( UPWEI_VRB .GE. 2 ) THEN
                      WRITE  (  6, 120 ) J2, DBOBJ%C_BAS(J2), &
     &                                   SIG_OLD*1.D12, CRS_GLO*1.D12, &
     &                                   SIG_NEW*1.D12
                      WRITE  ( 23, 120 ) J2, DBOBJ%C_BAS(J2), &
     &                                   SIG_OLD*1.D12, CRS_GLO*1.D12, &
     &                                   SIG_NEW*1.D12
 120                  FORMAT ( I3,') ',A17,' sig_old=',F10.1, &
     &                         ' crs_glo=',F10.1,' sig_new=',F10.1 )
                 END IF
 420          CONTINUE
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   CHIOBJ%WEIPH_GLO = CHIOBJ%WEIPH_BAS(1)
                 ELSE
                   CHIOBJ%WEIGR_GLO = CHIOBJ%WEIGR_BAS(1)
              END IF
            ELSE IF ( UPWEI_TYPE .EQ. 'BA'  .OR.  UPWEI_TYPE .EQ. 'ST' ) THEN
!
! ----------- Baseline type of correction
!
              DO 430 J3=1,DBOBJ%L_BAS
                 IF ( CHIOBJ%NEQU_BAS(J3) .GE. 1 ) THEN
                    CRS_BAS2(J3) = ( CHIOBJ%CHI_BAS(J3) - &
     &                      ( CHIOBJ%NEQU_BAS(J3) - CHIOBJ%CHIMAT_BAS(J3) ) )/ &
     &                      ( CHIOBJ%WEI2_BAS(J3) - CHIOBJ%CHIMA4_BAS(J3) )
!
! ----------------- Square root: take care about the sign!
!
                    IF ( CRS_BAS2(J3) .GE. 0.0 ) THEN
                         CRS_BAS(J3) =  DSQRT(DABS(CRS_BAS2(J3)))
                      ELSE
                        CRS_BAS(J3) = -DSQRT(DABS(CRS_BAS2(J3)))
                    END IF
!
! ----------------- Getting old weights from CHIOBJ
!
                    IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                         SIG_OLD = 1.D0/CHIOBJ%WEIPH_BAS(J3)
                      ELSE
                         SIG_OLD = 1.D0/CHIOBJ%WEIGR_BAS(J3)
                    END IF
!
                    IF ( CRS_BAS(J3) .GE. 0.0D0 ) THEN
                         SIG_NEW = DSQRT ( SIG_OLD**2 + CRS_BAS(J3)**2 )
                      ELSE
                         IF ( (SIG_OLD**2-CRS_BAS(J3)**2) .GE. FLO_MIN**2) THEN
                               SIG_NEW = DSQRT ( SIG_OLD**2 - CRS_BAS(J3)**2 )
                             ELSE
                               SIG_NEW = FLO_MIN
                         END IF
                    END IF
!
! ----------------- Check: should we change sigma forcible in accodring
! ----------------- with floor?
!
                    IF ( SIG_NEW .LT. UPWEI_FLO ) SIG_NEW = UPWEI_FLO
                    IF ( SIG_NEW .LT. FLO_MIN   ) SIG_NEW = FLO_MIN
!
! ----------------- Putting new weights in CHIOBJ
!
                    IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                         CHIOBJ%WEIPH_BAS(J3)  = 1.D0/SIG_NEW
                      ELSE
                         CHIOBJ%WEIGR_BAS(J3)  = 1.D0/SIG_NEW
                    END IF
!
! ----------------- Printing message at the screen if verbose mode is set on
!
                    IF ( UPWEI_VRB .GE. 2 ) THEN
                         WRITE  (  6, 130 ) J3, DBOBJ%C_BAS(J3), &
     &                                      SIG_OLD*1.D12, CRS_BAS(J3)*1.D12, &
     &                                      SIG_NEW*1.D12
                         WRITE  ( 23, 130 ) J3, DBOBJ%C_BAS(J3), &
     &                                      SIG_OLD*1.D12, CRS_BAS(J3)*1.D12, &
     &                                      SIG_NEW*1.D12
 130                     FORMAT ( I3,') ',A17,' sig_old=',F10.1, &
     &                              ' crs_bas=',F10.1,' sig_new=',F10.1 )
                    END IF
                 END IF
 430          CONTINUE
         END IF
!
! ------ Refresh the weights in the data structure OBSBAS. Weights of all
! ------ obsservations are updated for the difference between the old
! ------ baseline-dependent correction kept in OBSHLD data structure and
! ------ new baseline-dependent correction saved in CHIOBJ
!
         CALL ERR_PASS ( IUER, IER )
         CALL REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, CHIOBJ%WEIGR_BAS, &
     &                     CHIOBJ%WEIPH_BAS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6532, IUER, 'UPWEI_MENU', 'Error during '// &
     &            'refreshing weights in data structure OBSBAS while '// &
     &            'database '//DBOBJ%NAME//' was processing' )
              RETURN
         END IF
!
! ------ And now refreshing weights in the data structure OBSHLD.
!
         DO 440 J4=1,DBOBJ%L_BAS
            IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                 OBSHLD%WEIPH_BAS(J4) = CHIOBJ%WEIPH_BAS(J4)
               ELSE 
                 OBSHLD%WEIGR_BAS(J4) = CHIOBJ%WEIGR_BAS(J4)
            END IF
 440     CONTINUE
!
! ------ Making solution with new weights
!
         CALL ERR_PASS ( IUER, IER )
         CALL SOL_INIT ( ELIM_VRB, 1, 0, N_OBS, L_STA, L_SCA, IDB2, IDBF, &
     &                   PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, DBOBJ, NCREC, &
     &                   OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, EQUMEM, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6533, IUER, 'UPWEI_DO', 'Error during '// &
     &            'making solution at the '//STR(1:I_LEN(STR))// &
     &            '-th iteration while the database '//DBOBJ%NAME// &
     &            ' was processing' )
              RETURN
         END IF
!
! ------ Check of convergence criterion. If the ration chi/ndg has not
! ------ reached unity within the specified range then iteratins are continued
!
         ITER_CONT = .FALSE.
         IF ( UPWEI_TYPE .EQ. 'GL' ) THEN
!
! ----------- Calculate the ratio of chi-square to its mathematical expectation
!
              CHI_NDG = CHIOBJ%CHI_GLO/(CHIOBJ%NEQU_GLO - CHIOBJ%CHIMAT_GLO )
!
! ----------- Printing status message at the screen and into SPOOL-file
!
              IF ( UPWEI_VRB .GE. 1 ) THEN
                   WRITE  (  6, 140 ) J1, UPWEI_TYPE, CHI_NDG
              END IF
              WRITE  ( 23, 140 ) J1, UPWEI_TYPE, CHI_NDG
 140          FORMAT ( ' === UPWEI: Iteration ',I3,' type: ',A2,' chi_ndg = ', &
     &                   F10.3 )
!
! ----------- If it differs from 1.0 by more than UPWEI_CHITOL -- continuation
! ----------- of iterations
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   WEI_GLO = CHIOBJ%WEIPH_GLO
                 ELSE
                   WEI_GLO = CHIOBJ%WEIGR_GLO
              END IF
!
              IF ( DABS( 1.D0/WEI_GLO - UPWEI_FLO ) .GT. 2.D0*FLO_MIN .AND. &
     &             DABS( CHI_NDG      - 1.0D0     ) .GT. UPWEI_CHITOL     ) THEN
!
! ---------------- No, the limit is not reached yet
!
                   ITER_CONT=.TRUE.
              END IF
            ELSE IF ( UPWEI_TYPE .EQ. 'BA'  .OR.  UPWEI_TYPE .EQ. 'ST' ) THEN
!
              DEV_MAX = 0.0
              DO 450 J5=1,DBOBJ%L_BAS
!
! -------------- Convergence criterion is applied only for the baselines with
! -------------- the number of observations exceeding the specified limit
!
                 IF ( CHIOBJ%NEQU_BAS(J5) .GE. NEQU_MIN ) THEN
!
! ------------------- Calculate the ratio of chi-square among observations at
! ------------------- the J5-th baseline to its mathematical expectation
!
                      CHI_NDG = CHIOBJ%CHI_BAS(J5)/ &
     &                    (CHIOBJ%NEQU_BAS(J5) - CHIOBJ%CHIMAT_BAS(J5))
!
! ------------------- If correction to weight is far from floor then we check
! ------------------- whether the convergence limit is reached
!
                      IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                           WEI_BAS = CHIOBJ%WEIPH_BAS(J5)
                        ELSE
                           WEI_BAS = CHIOBJ%WEIGR_BAS(J5)
                      END IF
!
                      IF ( DABS(1.D0/WEI_BAS-UPWEI_FLO) .GT. 2.D0*FLO_MIN ) THEN
!
! ------------------------ If it differs from 1.0 by more than UPWEI_CHITOL --
! ------------------------ continuation of iterations
!
                           IF ( DABS (CHI_NDG - 1.0D0) .GT. UPWEI_CHITOL ) THEN
!
! ----------------------------- No, the limit is not reached yet
!
                                ITER_CONT = .TRUE.
                           END IF
                           IF ( DABS ( CHI_NDG - 1.0D0 ) .GT. DEV_MAX ) THEN
                                DEV_MAX = DABS ( CHI_NDG - 1.0D0 )
                           END IF
                      END IF
                 END IF
 450          CONTINUE
!
! ----------- Printing status message at the screen and into SPOOL-file
!
              CHI_NDG = CHIOBJ%CHI_GLO/( CHIOBJ%NEQU_GLO - CHIOBJ%CHIMAT_GLO )
              IF ( UPWEI_VRB .GE. 1 ) THEN
                   WRITE  (  6, 150 ) J1, UPWEI_TYPE, CHI_NDG, DEV_MAX
              END IF
              WRITE  ( 23, 150 ) J1, UPWEI_TYPE, CHI_NDG, DEV_MAX
 150          FORMAT ( ' === UPWEI: Iteration ',I3,' type: ',A2,' chi_ndg = ', &
     &                   F10.3,' dev_max = ',F10.3 )
         END IF
!
! ------ Convergence criterion was passed through -- then the end of iterations
!
         IF ( .NOT. ITER_CONT ) GOTO 810
 410  CONTINUE
 810  CONTINUE
!
! --- Writing weights to NAMFIL
!
      CALL ERR_PASS ( IUER, IER )
      CALL IO_WGT   ( 2, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6534, IUER, 'UPWEI', 'Error during getting '// &
     &         'weights from NAMFIL while database '// &
     &          DBOBJ%NAME//' was processing' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  UPWEI_DO  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPWEI_INFO ( F_CHI, DBOBJ, RST, CHIOBJ, M_OUT, L_OUT, BUF_OUT )
! ************************************************************************
! *                                                                      *
! *   Routine  UPWEI_INFO  puts at the output array  BUF_OUT the table   *
! *   of the w.r.m.s., applied baseline-dependednt weights annd ratio of *
! *   chi-square to its mathematical expectation for each used baseline, *
! *   each used source sperately and for all used observations together. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   F_CHI ( LOGICAL*4 ) -- Flag: .TRUE. means that the ratio of        *
! *                          chi-square to its mathematical expectation  *
! *                          has been calculated and available.          *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *     RST ( RECORD    ) -- Data structure keeping the statisitcs of    *
! *                          postfit residuals.                          *
! *  CHIOBJ ( RECORD    ) -- Object with data structure for keeping      *
! *                          accumulators of the chi-squares and their   *
! *                          mathematical expectations.                  *
! *   M_OUT ( INTEGER*4 ) -- The maximal number of lines in the output   *
! *                          buffer.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_OUT ( INTEGER*4 ) -- The number of lines in the output buffer.   *
! * BUF_OUT ( CHARACTER ) -- Text output buffer. Array should be         *
! *                          declared as BUF_OUT(M_OUT). Only L_OUT      *
! *                          lines of the array will be filled by        *
! *                          UPWEI_INFO.                                 *
! *                                                                      *
! *  ###  28-JAN-98    UPWEI_INFO  v1.3  (c) L. Petrov  04-APR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      LOGICAL*4  F_CHI
      REAL*8     CHI_NDG, WRMS, WEI_BAS
      INTEGER*4  M_OUT, L_OUT
      CHARACTER  BUF_OUT(M_OUT)*(*), DATYP_OUT*21
      INTEGER*4  J0, J1, J2, IP_BAS, IP_SOU
      LOGICAL*4  DATYP_INQ
      INTEGER*4  I_LEN, IFIND_PL
!
! --- Initialization
!
      DO 400 J0=1,M_OUT
         CALL CLRCH ( BUF_OUT(J0) )
 400  CONTINUE
!
! --- Top header
!
      L_OUT = 0
      L_OUT = L_OUT+1
      BUF_OUT(L_OUT) = '================================='// &
     &                 '================================='
      L_OUT = L_OUT+1
!
! --- Get description of solution type
!
      CALL CLRCH ( DATYP_OUT )
      CALL DATYP_SHOW ( DBOBJ%IDATYP, DATYP_OUT )
!
      WRITE ( UNIT=BUF_OUT(L_OUT), FMT=110 ) DATYP_OUT, &
     &        ( DBOBJ%NAME(1:I_LEN(DBOBJ%NAME)) )
 110  FORMAT ( ' "',A,'" solution for the session ',A )
!
! --- Baseline statistics header
!
      L_OUT = L_OUT+1
      BUF_OUT(L_OUT) = '---------------------------------'// &
     &                 '---------------------------------'
      L_OUT = L_OUT+1
      WRITE ( UNIT=BUF_OUT(L_OUT), FMT=* ) '         Baseline       '// &
     &       ' used/total   w.r.m.s.  add_wgt  chi-sq/ndg'
      L_OUT = L_OUT+1
!
! --- Baseline statistics
!
      DO 410 J1=1,DBOBJ%L_BAS
         L_OUT  = L_OUT+1
!
         IP_BAS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, DBOBJ%LIS_BAS(J1) )
!
         IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
              WEI_BAS = CHIOBJ%WEIPH_BAS(J1)
            ELSE
              WEI_BAS = CHIOBJ%WEIGR_BAS(J1)
         END IF
!
         IF ( F_CHI ) THEN
              IF ( CHIOBJ%NEQU_BAS(J1) .GE. 1  .AND.  IP_BAS .GT. 0 ) THEN
                   CHI_NDG = CHIOBJ%CHI_BAS(J1)/ &
     &                      (CHIOBJ%NEQU_BAS(J1) - CHIOBJ%CHIMAT_BAS(J1))
                   WRMS    = RST%WRMS_B(IP_BAS)
                ELSE
                   CHI_NDG = 0.0
                   WRMS    = 0.0
              END IF
              WRITE ( UNIT=BUF_OUT(L_OUT), FMT=120 ) J1, DBOBJ%C_BAS(J1), &
     &                CHIOBJ%NEQU_BAS(J1), DBOBJ%KL_BAS(J1), WRMS*1.D12, &
     &                1.D12/WEI_BAS, CHI_NDG
!    #
 120          FORMAT ( I4,') ',A,'  ',I6,'/',I6,'  ',F8.1,'  ', F7.1,'  ', &
     &                 F10.3 )
           ELSE
              WRITE ( UNIT=BUF_OUT(L_OUT), FMT=125 ) J1, DBOBJ%C_BAS(J1), &
     &                CHIOBJ%NEQU_BAS(J1), DBOBJ%KL_BAS(J1), &
     &                1.D12/WEI_BAS
 125          FORMAT ( I3,') ',A,'  ',I6,'/',I6, 7X, 'N/A', '  ', F7.1, &
     &                 '         N/A' )
         END IF
 410  CONTINUE
!
! --- Header of source statistics
!
      L_OUT = L_OUT+1
      BUF_OUT(L_OUT) = '----------------------------------------------------'// &
     &                '-----------------'
      L_OUT = L_OUT+1
      BUF_OUT(L_OUT) = '     Source       used/total    w.r.m.s.  '// &
     &                 'chi-sq/ndg'
      L_OUT = L_OUT+1
!
! --- Source statistics
!
      DO 420 J2=1,DBOBJ%L_SOU
         L_OUT  = L_OUT+1
         IP_SOU = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%LIS_SOU(J2) )
         IF ( F_CHI ) THEN
              IF ( CHIOBJ%NEQU_SOU(J2) .GE. 1  .AND.  IP_SOU .GT. 0 ) THEN
                   CHI_NDG = CHIOBJ%CHI_SOU(J2)/ &
     &                      (CHIOBJ%NEQU_SOU(J2) - CHIOBJ%CHIMAT_SOU(J2))
                   WRMS    = RST%WRMS_S(IP_SOU)
                 ELSE
                   CHI_NDG = 0.0
                   WRMS    = 0.0
              END IF
!
              WRITE  ( UNIT=BUF_OUT(L_OUT), FMT=130 ) J2, DBOBJ%C_SOU(J2), &
     &               CHIOBJ%NEQU_SOU(J2), DBOBJ%KL_SOU(J2), WRMS*1.D12, CHI_NDG
 130          FORMAT ( I3,') ',A,'  ',I6,'/',I6,'  ',F8.1,'  ', F10.3 )
            ELSE
              WRITE  ( UNIT=BUF_OUT(L_OUT), FMT=135 ) J2, DBOBJ%C_SOU(J2), &
     &                 CHIOBJ%NEQU_SOU(J2), DBOBJ%KL_SOU(J2)
 135          FORMAT ( I3, ') ', A, '  ', I6, '/', I6, 7X, 'N/A', 9X, 'N/A' )
         END IF
 420  CONTINUE
!
! --- Header of total statistics
!
      L_OUT = L_OUT + 1
      BUF_OUT(L_OUT) = '--------------------------------------------------'
      L_OUT = L_OUT + 1
      BUF_OUT(L_OUT) = '      Total      used/total   w.r.m.s.  chi-sq/ndg'
      L_OUT = L_OUT + 1
!
! --- Total statistics
!
      IF ( F_CHI ) THEN
           L_OUT = L_OUT + 1
           WRITE  ( UNIT=BUF_OUT(L_OUT), FMT=140 ) CHIOBJ%NEQU_GLO, &
     &              DBOBJ%L_OBS, RST%WRMS_G*1.D12, &
     &              CHIOBJ%CHI_GLO/(CHIOBJ%NEQU_GLO-CHIOBJ%CHIMAT_GLO)
 140       FORMAT ( 13X, '  ', I6, '/', I6, '  ', F8.1, '  ', F10.3 )
         ELSE
           WRITE  ( UNIT=BUF_OUT(L_OUT), FMT=145 ) CHIOBJ%NEQU_GLO, DBOBJ%L_OBS
 145       FORMAT ( 13X, '  ', I6, '/', I6, 7X, 'N/A', 9X, 'N/A' )
      END IF
!
! --- Botton line
!
      L_OUT = L_OUT + 1
      BUF_OUT(L_OUT) = '__________________________________________________'
!
      RETURN
      END  !#!  UPWEI_INFO  #!#
