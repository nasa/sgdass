      SUBROUTINE PARU_ELIM ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Interface routine for calling ELIM/MILE (outliers elimination or   *
! *   restoration of previously suppressed observations) from PARU.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in       *
! *                            the scratch file.                         *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_SCA ( INTEGER*4 ) -- Total number of scans in the session.     *
! *     L_STA ( INTEGER*4 ) -- Total number of stations in the session.  *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSSCA ( RECORD    ) -- Data structure which keeps scan-dependent *
! *                            information about the session.            *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *    PLACE  ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs     *
! *                            of postfit residuals.                     *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and       *
! *                            their mathematical expectations.          *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! *  ###  19-MAR-98    PARU_ELIM   v1.2 (c)  L. Petrov  31-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, IUER
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
      INTEGER*4  K_RJC, K_RST, IER
!
      IF ( ELIM_MOD ) THEN
!
! -------- Do iterative outlier elimination
!
           CALL ERR_PASS ( IUER, IER )
           CALL ELIM_DO  ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &          OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &          PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &          ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &          ELIM_MSR, ELIM_UPD, K_RJC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6381, IUER, 'PARU_ELIM', 'Error during '// &
     &              'outlier elimination occurred while the database '// &
     &               DBOBJ%NAME//' was processing' )
               RETURN
           END IF
         ELSE
!
! ---------Recalculation of the statistics
!
           CALL ERR_PASS ( IUER, IER )
           CALL RESID_ST ( .FALSE., .FALSE., 0.D0, 0.D0, 0.D0, 0, N_OBS, DBOBJ, &
     &                     OBSSCA, OBSBAS, RES, RST, IER )
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6382, IUER, 'PARU_ELIM', 'Error during '// &
     &               'calculation statisics for the postfit residuals '// &
     &               'while database '//DBOBJ%NAME//' was processing' )
                 RETURN
           END IF
!
! -------- Do iterative restoration previously suppressed observations
!
           CALL MILE_DO  ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &          OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &          PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &          ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &          ELIM_MSR, ELIM_UPD, ELIM_AMB, ELIM_ION, K_RST, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6383, IUER, 'PARU_ELIM', 'Error during '// &
     &              'resotration suppresed observations occurred while the '// &
     &              'database '//DBOBJ%NAME//' was processing' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARU_ELIM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARU_UPWEI ( UPWEI_TYPE, UPWEI_MAXIT, UPWEI_CHITOL, &
     &           UPWEI_INIT, UPWEI_INIWEI, UPWEI_FLO, UPWEI_VRB, &
     &           IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Interface routine for calling UPWEI (weighjt update in order to    *
! *   make the ratio of square sum of weighted residuals to its          *
! *   mathematical expectation to be near to unity) from PARU.           *
! *                                                                      *
! *   Interface routine for calling UPWEI (weighjt update in order to    *
! *   make the ratio of square sum of weighted residuals to its          *
! *   mathematical expectation to be near to unity) from PARU.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in       *
! *                            the scratch file.                         *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_SCA ( INTEGER*4 ) -- Total number of scans in the session.     *
! *     L_STA ( INTEGER*4 ) -- Total number of stations in the session.  *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSSCA ( RECORD    ) -- Data structure which keeps scan-dependent *
! *                            information about the session.            *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *    PLACE  ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs     *
! *                            of postfit residuals.                     *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and       *
! *                            their mathematical expectations.          *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! *  ###  19-MAR-98   PARU_UPWEI   v1.1  (c)  L. Petrov 31-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, UPWEI_MAXIT, UPWEI_VRB, IUER
      LOGICAL*4  UPWEI_INIT
      REAL*8     UPWEI_INIWEI
      CHARACTER  UPWEI_TYPE*(*)
      REAL*8     UPWEI_FLO,  UPWEI_CHITOL
      INTEGER*4  ELIM_VRB_SAVED
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
      TYPE ( CHIACC__STRU ) ::  EQUMEM
      LOGICAL*4  F_CHI, DATYP_INQ
      INTEGER*4  M_OUT, L_OUT, J1, J2, J3, IER
      PARAMETER  ( M_OUT = 128*1024 )
      CHARACTER  BUF_OUT(M_OUT)*80
      INTEGER*4  I_LEN
!
      IF ( UPWEI_INIT ) THEN
!
! -------- Substitution to the initial value of baseline dependent correction
! -------- to weights
!
           DO 410 J1=1,DBOBJ%L_BAS
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) ) THEN
                   CHIOBJ%WEIGR_BAS(J1) = 1.D0/UPWEI_INIWEI
                ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   CHIOBJ%WEIPH_BAS(J1) = 1.D0/UPWEI_INIWEI
              END IF
 410       CONTINUE
!
! -------- Check: do any baselines have weights less then floor? If yes then
! -------- we change these weights
!
           DO 420 J2=1,DBOBJ%L_BAS
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) ) THEN
                   IF ( 1.D0/CHIOBJ%WEIGR_BAS(J2) .LT. UPWEI_FLO ) THEN
                        CHIOBJ%WEIGR_BAS(J2) = 1.D0/UPWEI_FLO
                   END IF
                ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   IF ( 1.D0/CHIOBJ%WEIPH_BAS(J2) .LT. UPWEI_FLO ) THEN
                        CHIOBJ%WEIPH_BAS(J2) = 1.D0/UPWEI_FLO
                   END IF
              END IF
 420       CONTINUE
!
! -------- Write weights to NAMFIL
!
           CALL ERR_PASS ( IUER, IER )
           CALL IO_WGT   ( 2, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6371, IUER, 'PARU_UPWEI', 'Error duiring '// &
     &              'putting weights to NAMFIL while database '//DBOBJ%NAME// &
     &              ' was processing' )
                RETURN
           END IF
      END IF
!
      ELIM_VRB_SAVED    = ELIM_VRB
      IF ( UPWEI_VRB .GE. 3 ) THEN
           ELIM_VRB = 1
         ELSE
           ELIM_VRB = 0
      END IF
!
      CHIOBJ%WEIGR_GLO  = CHIOBJ%WEIGR_BAS(1)
      CHIOBJ%WEIPH_GLO  = CHIOBJ%WEIPH_BAS(1)
      CHIOBJ%LAST_FIELD = 1
!
! --- Refresh the weights in the data structure OBSBAS
!
      CALL ERR_PASS    ( IUER, IER )
      CALL REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, CHIOBJ%WEIGR_BAS, &
     &                   CHIOBJ%WEIPH_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6372, IUER, 'PARU_UPWEI', 'Error duiring '// &
     &         'refreshing weights in data structure OBSBAS while '// &
     &         'database '//DBOBJ%NAME//' was processing' )
           RETURN
      END IF
!
! --- Refreshing weights in the data data structure OBSHLD
!
      CALL COPY_V ( MO_BAS, CHIOBJ%WEIGR_BAS, OBSHLD%WEIGR_BAS )
      CALL COPY_V ( MO_BAS, CHIOBJ%WEIPH_BAS, OBSHLD%WEIPH_BAS )
      F_CHI = .TRUE.
!
! --- Do iterative weight update
!
      CALL ERR_PASS ( IUER, IER )
      CALL UPWEI_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &     OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &     PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &     UPWEI_FLO,  UPWEI_CHITOL, UPWEI_MAXIT, UPWEI_TYPE, &
     &     UPWEI_VRB, ELIM_VRB, IER )
      ELIM_VRB = ELIM_VRB_SAVED
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6373, IUER, 'PARU_UPWEI', 'Error during '// &
     &         'iterative update of the weights while the database '// &
     &          DBOBJ%NAME//' was processing' )
           RETURN
      END IF
!
      IF ( UPWEI_VRB .GE. 2 ) THEN
!
! -------- Generate buffer with statistics
!
           CALL UPWEI_INFO ( F_CHI, DBOBJ, RST, CHIOBJ, M_OUT, L_OUT, BUF_OUT )
!
! -------- Write buffer with statistics on screen and in file
!
           DO 430 J3=1,L_OUT
              WRITE (  6, '(A)' ) BUF_OUT(J3)(1:I_LEN(BUF_OUT(J3)))
              WRITE ( 23, '(A)' ) BUF_OUT(J3)(1:I_LEN(BUF_OUT(J3)))
 430       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARU_UPWEI  #!#
