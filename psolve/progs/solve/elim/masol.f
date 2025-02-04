      SUBROUTINE SOL_INIT ( IVRB, IRD, ITHR, N_OBS, L_STA, L_SCA, IDB2, IDBF, &
     &                      PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, DBOBJ, NCREC, &
     &                      OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, &
     &                      EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SOL_INIT  makes the system of normal equation for LSQ in  *
! *   B3D mode, finds estimates and their covariance matrix, calculates  *
! *   residuals and their statistics.                                    *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      IVRB ( INTEGER*4 ) -- Verbosity level.                          *
! *                            0 -- silent mode;                         *
! *                            2 -- progress counter will be printed.    *
! *       IRD ( INTEGER*4 ) -- Code of operation of reading the          *
! *                            parameters of the observation and         *
! *                            observables.                              *
! *                            IRD = 0 -- data will be read from scratch *
! *                                       file or superfile.             *
! *                            IRD = 1 -- data will be read from OBSSCA, *
! *                                       OBSSTA, OBSBAS, DBOBJ data     *
! *                                       structures.                    *
! *      ITHR ( INTEGER*4 ) -- Code whether to update theoretical path   *
! *                            delay.                                    *
! *                            ITHR = 0 -- not to update theoreticals.   *
! *                            ITHR = 1 -- update theoreticals.          *
! *                            ITHR = 2 -- update theoreticals, but not  *
! *                                        to make the system of normal  *
! *                                        equations.                    *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            postfit residuals.                        *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *  ###  26-SEP-97    SOL_INIT    v1.9  (c)  L. Petrov 09-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INCLUDE   'heo.i'
      INTEGER*2  IDB2
      INTEGER*4  IVRB, IRD, N_OBS, L_STA, L_SCA, IDBF, IUER
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( NCREC__STRU ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(L_SCA)
      TYPE ( STA_O__STRU ) ::  OBSSTA(L_STA,L_SCA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR, ADR_WEI_CNS
      INTEGER*4  IER, IWR, ITHR, IACT, FAST_COV_SAVE
      CHARACTER  STR*80
      LOGICAL*4  FL_NOFD_IGNORE, FL_FULL_WEI 
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IRD .NE. 0  .AND.  IRD .NE. 1 ) THEN
           CALL CLRCH ( STR )
           CALL  INCH ( IRD, STR )
           CALL ERR_LOG ( 6681, IUER, 'SOL_INIT', 'Wrong value of the '// &
     &         'parameter IRD:  IRD='//STR )
           RETURN
      END IF
      FAST_COV_SAVE = FAST_COV  ! Save FAST_COV from GLBC4
      CALL USE_GLBFIL   ( 'ORC' )
      CALL USE_GLBFIL_3 ( 'ORC' )
      CALL USE_GLBFIL_4 ( 'ORC' )
      IF ( L_HEO > 0 .AND. ADR_HEO .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER ) 
           CALL GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, ADR_HEO, &
     &                 HEO_EPOCH_SEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6688, IUER, 'SOL_INIT', 'Error in an '// &
     &              'attempt to read expansion of the harmonic '// &
     &              'Earth orientation parameters from the file '// &
     &              FINAM_HEO )
                RETURN 
           END IF
           STAT_HEO  = HEO__READ
      END IF
      FAST_COV = FAST_COV_SAVE  ! Restore FAST_COV
!
! --- Do some things needed before processing the first observation
!
      CALL ERR_PASS   ( IUER, IER )
      CALL MANOR_INIT ( IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6682, IUER, 'SOL_INIT', 'Error during '// &
     &         'initialization of internal data structure detected '// &
     &         'while database '//DBOBJ%NAME//' was processed' )
           RETURN
      END IF
!
      IF ( ELIM_VRB .GE. 1 ) THEN
           WRITE ( 6, FMT='(A)' ) 'Normal equations for the database '// &
     &                             DBOBJ%NAME//' are being made'
      END IF
!
! --- Now we should decide whether we
! --- a) should read from scratch file and to write in OBS- data strucutures
! --- b) should read from OBS- data structures and write nothing
!
      IF ( IRD .EQ. 0 ) IWR = 1
      IF ( IRD .EQ. 1 ) IWR = 0
!
! --- Making system of normal equations
!
      IF ( IVRB .GE. 2 ) THEN
           CALL PRCH ( 'Normal equations: '//char(1) )
      END IF
      IF ( ITHR == 2 ) THEN
           IACT = 0
         ELSE 
           IACT = 1
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL MANOR_DO ( IVRB, IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, IDBF, N_OBS, &
     &                L_SCA, L_STA, OBSHLD, OBSSCA, OBSSTA, OBSBAS, DBOBJ, &
     &                CHIOBJ, IRD, IWR, IACT, ITHR, 0, RES, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6683, IUER, 'SOL_INIT', 'Error during making '// &
     &         'normal equations detected while database '//DBOBJ%NAME// &
     &         ' was processed' )
           RETURN
      END IF
!
      IF ( ITHR == 2 ) THEN
!
! -------- Nothing more to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( ELIM_VRB .GE. 1 ) THEN
           IF ( ELIM_VRB .GE. 2 )  CALL CLSTR
           WRITE ( 6, FMT='(A)' ) 'Normal equations for the database '// &
     &                             DBOBJ%NAME//' have been made'
      END IF
!
! --- Applying constraints. Constraints change some elements of the
! --- normal matrix
!
      IF ( (CONSTRAINT_BITS .NE. 0) .OR. KSRC_CONST .OR. KBIT(DEFCMP,5) ) THEN
!
! -------- Bbuild constraint equations
!
           CNSTROBJ%N_MATEL = 0
           CNSTROBJ%N_TYCNS = 0
           CNSTROBJ%N_VECEL = 0
           CNSTROBJ%N_EQUAT = 0
           CNSTROBJ%N_ECNST = 0
           CNSTROBJ%N_OFD   = 0
           CALL NOUT_I4 ( MAX_ECNST, CNSTROBJ%EQU_INE )
           CALL NOUT_I4 ( MAX_ECNST, CNSTROBJ%EQU_INP )
           CALL NOUT_R8 ( MAX_ECNST, CNSTROBJ%EQU_CNS )
           CALL NOUT_R8 ( MAX_EQUAT, CNSTROBJ%RTP_CNS )
           CALL NOUT_R8 ( MAX_OFD,   CNSTROBJ%WEI_OFD )
!
           CALL ERR_PASS ( IUER, IER )
           CALL CNSTR ( B3DOBJ, B1B3DOBJ, %VAL(0), %VAL(0), CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6684, IUER, 'SOL_INIT', 'Failure in '// &
     &              'attempt to apply constraints while the database '// &
     &              B3DOBJ%DBNAME_MES//' was processed' )
                RETURN
           END IF
!
           CALL GETENVAR ( 'NOFD_IGNORE', STR )
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' .OR.  STR(1:2) == 'ON' ) THEN
                FL_NOFD_IGNORE = .TRUE.
             ELSE 
                FL_NOFD_IGNORE = .FALSE.
           END IF
!
           IF ( CNSTROBJ%N_OFD > 0  .AND.  .NOT. FL_NOFD_IGNORE ) THEN
!
! ------------- Grab dynamic memory for weight matrix of constraints
!
                CALL ERR_PASS ( IUER, IER )
                CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                           8*(CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2, &
     &                           ADR_WEI_CNS )
                IF ( IER .NE. 0 ) THEN
                     CALL IINCH   ( MEM_LEN, STR )
                     CALL ERR_LOG ( 6685, IUER, 'SOL_INIT', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                   'memory for weight matrix of constraints' )
                     RETURN
                END IF
                FL_FULL_WEI = .TRUE.
              ELSE 
                FL_FULL_WEI = .FALSE.
           END IF 
!
! -------- Applying constraints. Constraints changes some elements of the normal
! -------- matrix
!
           CALL ERR_PASS    ( IUER, IER )
           CALL APPLY_CNSTR ( FAST_MODE, FAST_DBG, CNSTROBJ, B3DOBJ%N_GLO, &
     &                        %VAL(0), %VAL(0), &
     &                        B3DOBJ, B1B3DOBJ, FL_FULL_WEI, &
     &                        %VAL(ADR_WEI_CNS), IER )
           IF ( FL_FULL_WEI ) THEN
                CALL FREE_MEM ( MEM_ADR ) ! Free dynamic memory
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6686, IUER, 'SOL_INIT', 'Error in an attempt '// &
     &              'to apply constraints' )
                RETURN
           END IF
      END IF
!
! --- Solving system of normal equations using B3D algorithm
!
      IF ( ELIM_VRB .GE. 1 ) THEN
           WRITE ( 6, FMT='(A)' ) 'Normal equations for the database '// &
     &                             DBOBJ%NAME//' are being solved'
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL NORML_B3D ( FAST_COV, .FALSE., FAST_DBG, B3DOBJ, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6687, IUER, 'SOL_INIT', 'Failure to solve '// &
     &         'normal system using B3D algorithm while database '// &
     &          B3DOBJ%DBNAME_MES//' was processed' )
           RETURN
      END IF
!
! --- Writing down condition number in GLBFIL
!
      CALL USE_GLBFIL_4 ( 'OWC' )
      IF ( ELIM_VRB .GE. 1 ) THEN
           WRITE ( 6, FMT='(A)' ) 'Normal equations for the database '// &
     &                             DBOBJ%NAME//' have been solved'
      END IF
!
! --- Calculation of residuals
!
      IF ( ELIM_VRB .GE. 2 ) THEN
           CALL PRCH ( 'Residuals: '//CHAR(1) )
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL MANOR_DO ( IVRB, IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, IDBF, N_OBS, &
     &                L_SCA, L_STA, OBSHLD, OBSSCA, OBSSTA, OBSBAS, DBOBJ, &
     &                CHIOBJ, 1, 0, 5, 0, ITHR, RES, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6688, IUER, 'SOL_INIT', 'Error during '// &
     &         'calculation residuals detected while database '// &
     &          DBOBJ%NAME//' was processed' )
           RETURN
      END IF
!
! --- Calculation statistics of the residuals
!
      CALL ERR_PASS ( IUER, IER )
      CALL RESID_ST ( .FALSE., .FALSE., 0.0D0, 0.0D0, 0.D0, 0, N_OBS, DBOBJ, &
     &                 OBSSCA, OBSBAS, RES, RST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6689, IUER, 'SOL_INIT', 'Error during '// &
     &         'calculation statisics for the postfit residuals '// &
     &         'while database '//B3DOBJ%DBNAME_MES//' was processed' )
           RETURN
      END IF
      IF ( ELIM_VRB .GE. 2 ) THEN
           CALL CLSTR()
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SOL_INIT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SOL_UPDATE ( IP, IOBS, IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SOL_UPDATE  calculates IOBS-th equation of conditions,    *
! *   updates LSQ solution made in B3D mode (vector of the estimates and *
! *   and full covariance matrix), recalcultes the residual for the      *
! *   IOBS-th observation after updating of the solution.                *
! *                                                                      *
! *   Statistics in data structure is not updated.                       *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *        IP ( INTEGER*4 ) -- Type of update:                           *
! *                            IP=-1 -- update solution for elimination  *
! *                                     of the IOBS-th observation which *
! *                                     was in solution.                 *
! *                            IP= 1 -- update solution for adding       *
! *                                     the IOBS-th observation into     *
! *                                     solution which was not in        *
! *                                     solution before it.              *
! *      IOBS ( INTEGER*4 ) -- Index of the observations concerned.      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *     PLACE ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *                            It keeps O-C and coefficients of the      *
! *                            IOBS-th equation of conditions.           *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs of  *
! *                            postfit residuals.                        *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will pe put   *
! *                                   on stdout.                         *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  26-SEP-97   SOL_UPDATE   v1.2  (c)  L. Petrov  29-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IP, IOBS, IDBF, N_OBS, L_SCA, L_STA, IUER
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
      ADDRESS__TYPE :: AD_GLO, AD_SG1, AD_SG2
      INTEGER*8  SA, SAX
      INTEGER*4  NK, IER
!
! --- Calculation of one equation of conditions
!
      CALL ERR_PASS ( IUER, IER )
      CALL MANOR_DO ( 0, IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, IDBF, N_OBS, &
     &                L_SCA, L_STA, OBSHLD, OBSSCA, OBSSTA, OBSBAS, DBOBJ, &
     &                CHIOBJ, 1, 0, 3, 0, IOBS, RES, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6691, IUER, 'SOL_UPDATE', 'Error during '// &
     &         'calculation of a single equation of conditions detected '// &
     &         'while database '//DBOBJ%NAME//' was processed' )
           RETURN
      END IF
!
! --- Gathering global, segmented(current) and segmented(next) part
! --- of the equation of condition
!
! --- First copying the addresses (Thanks god, they have more than enough room)
!
      AD_GLO = B3DOBJ%AD_N00
      AD_SG1 = B3DOBJ%AD_N10
      AD_SG2 = B3DOBJ%AD_N11
!
! --- Initial zeroing
!
      CALL NOUT ( 8*B3DOBJ%N_GLO, %VAL(AD_GLO) )
      CALL NOUT ( 8*B3DOBJ%SB,    %VAL(AD_SG1) )
      CALL NOUT ( 8*B3DOBJ%SB,    %VAL(AD_SG2) )
!
! --- And expanded an equation of conditions from compressed from to full form
!
      CALL DSCATTER ( PLACE%N_GLO, PLACE%IND_GLO, PLACE%EQU_GLO, %VAL(AD_GLO) )
      CALL DSCATTER ( PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1, %VAL(AD_SG1) )
      CALL DSCATTER ( PLACE%N_SG2, PLACE%IND_SG2, PLACE%EQU_SG2, %VAL(AD_SG2) )
!
      SA  = (INT8(B3DOBJ%SB)*INT8(B3DOBJ%SB+1))/2
      SAX = (INT8(B3DOBJ%SX)*INT8(B3DOBJ%SX+1))/2
      NK  = ((B3DOBJ%NBS-2)*(B3DOBJ%NBS-1))/2
!
! --- Making update of the vector of the estimates (scaled) and its covariance
! --- matrix
!
      CALL ERR_PASS   ( IUER, IER )
      CALL B3D_UPDATE ( IP, 1, B3DOBJ%NBS, B3DOBJ%N_GLO, B3DOBJ%SB, &
     &     B3DOBJ%SX, SA, SAX, NK, PLACE%CURR_CSG, &
     &     %VAL(AD_GLO), %VAL(AD_SG1), %VAL(AD_SG2), &
     &     RES(IOBS)%OC_DEL,     RES(IOBS)%WEI_DEL, &
     &     %VAL(B3DOBJ%AD_U0),   %VAL(B3DOBJ%AD_US(1)), %VAL(B3DOBJ%AD_USX), &
     &     %VAL(B3DOBJ%AD_B0), &
     &     %VAL(B3DOBJ%AD_B(1)), %VAL(B3DOBJ%AD_C(1)),  %VAL(B3DOBJ%AD_D(1)), &
     &     %VAL(B3DOBJ%AD_BX),   %VAL(B3DOBJ%AD_CX),    %VAL(B3DOBJ%AD_DX), &
     &                                                  %VAL(B3DOBJ%AD_CVF), &
     &     %VAL(B3DOBJ%AD_E0),   %VAL(B3DOBJ%AD_ES(1)), %VAL(B3DOBJ%AD_ESX), &
     &     %VAL(B3DOBJ%AD_Q0),   %VAL(B3DOBJ%AD_QS(1)), %VAL(B3DOBJ%AD_QSX), &
     &     %VAL(B3DOBJ%AD_VG0),  %VAL(B3DOBJ%AD_VS1),   %VAL(B3DOBJ%AD_VS2), &
     &     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6692, IUER, 'SOL_UPDATE', 'Error during making '// &
     &         'update of the solution while database '//B3DOBJ%DBNAME_MES// &
     &         ' was processed' )
           RETURN
      END IF
!C
!C --- Update accumulators in RST for elimination/restoration of the IOBS-th
!C --- observation taking into account OLD residual
!C
!      RST.FIRST_FIELD = RST__STUPD
!      CALL RESID_ST ( .FALSE., .FALSE., 0.0D0, 0.D0, 0.0D0, IP*IOBS, N_OBS,
!     #                DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
!      IF ( IER .NE. 0 ) THEN
!           CALL ERR_LOG ( 6693, IUER, 'SOL_UPDATE', 'Error during making '//
!     #         'update of the statistics while database '//B3DOBJ.DBNAME_MES//
!     #         ' was processed' )
!           RETURN
!      END IF
!
! --- Recalculation of the residual of the IOBS-th observaion
!
      CALL ERR_PASS ( IUER, IER )
      CALL MANOR_DO ( 0, IDB2, PLACE, B3DOBJ, B1B3DOBJ, NCREC, IDBF, N_OBS, &
     &                L_SCA, L_STA, OBSHLD, OBSSCA, OBSSTA, OBSBAS, DBOBJ, &
     &                CHIOBJ, 1, 0, 4, 0, IOBS, RES, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6694, IUER, 'SOL_UPDATE', 'Error during '// &
     &         'calculation of single equation of conditions '// &
     &         'detected while database '//DBOBJ%NAME//' was '// &
     &         'processed' )
           RETURN
      END IF
!
! --- Update of the accumulators in CHIOBJ
!
      CALL ERR_PASS ( IUER, IER )
      CALL CHI_UPDATE ( IP, 2, IOBS, IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &                  DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, &
     &                  PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6695, IUER, 'SOL_UPDATE', 'Error during '// &
     &         'update of chi-square accumulators detected while database '// &
     &          DBOBJ%NAME//' was processed' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SOL_UPDATE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SEARCH_FOR_ZERO_DIAG ( N_MAT, MAT )
      IMPLICIT   NONE 
      INTEGER*4  N_MAT
      REAL*8     MAT(*)
      INTEGER*4  J1, K
      INTEGER*4  I, J
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(MIN(I,J)) + ( INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
      do 410 j1=1,n_mat
         if ( mat(locs(j1,j1)) == 0.0d0 ) then
              k = -1
              write ( 6, * ) 'Zero diagonal element at ', j1
              write ( 6, * ) mat(k)
         end if
 410  CONTINUE 
      RETURN
      END  !#!  
