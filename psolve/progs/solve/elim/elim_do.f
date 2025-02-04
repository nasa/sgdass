      SUBROUTINE ELIM_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &           ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &           ELIM_MSR, ELIM_UPD, K_RJC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ELIM_DO  makes elimination of outliers for the LSQ        *
! *   solution in B3D mode.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
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
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            postfit residuals.                        *
! *  ELIM_MOD ( LOGICAL*4 ) -- mode switch. .TRUE. if ELIM is called in  *
! *                            elimination mode, .FALSE. if called in    *
! *                            restoration mode.                         *
! *  ELIM_THR ( REAL*8    ) -- Postfit threshold (sec). ELIM mode:       *
! *                            If used observation with postfit          *
! *                            residual exceeds ELIM_THR it marked as    *
! *                            outlier. MILE mode: if recoverable        *
! *                            observation has postfit residual less     *
! *                            than ELIM_THR it marked as candidate in   *
! *                            restoration.                              *
! *  ELIM_CUT ( REAL*8    ) -- n-sigmas cutoff criterion. ELIM mode:     *
! *                            If used observation has normalized        *
! *                            postfit residual larger than ELIM_CUT it  *
! *                            marked as outlier. MILE mode: if          *
! *                            recoverable observation has normalized    *
! *                            postfit residual less than ELIM_THR it    *
! *                            marked as candidate in restoration.       *
! *  ELIM_MSR ( REAL*8    ) -- Maximal uncertainty. Observations with    *
! *                            formal uncertainty exceeding ELIM_MSR     *
! *                            are marked as outliers in ELIM mode and   *
! *                            are not eligible for restoration in MILE  *
! *                            mode. If ELIM_MSR < 1.D-12 then it is     *
! *                            ignored.                                  *
! *  ELIM_TYP ( CHARACTER ) -- Mode of normalization. Normalization of   *
! *                            "normalized postfit residuals" may be     *
! *                            done in two modes: 'global' mode when     *
! *                            dispersion of postfit residuals used for  *
! *                            normalization is calculated for all used  *
! *                            observations of the database or in        *
! *                            'baseline' mode when this dispersion is   *
! *                            calculated for used observations made at  *
! *                            the same baseline as the considered       *
! *                            observation. Two values are allowed:      *
! *                            'BA' or 'GL'.                             *
! *  ELIM_VRB ( REAL*8    ) -- Verbosity level. 0 -- silent mode.        *
! *                            1 -- verbose mode ( suitable for short    *
! *                                 sessions ); 2 -- more verbose mode   *
! *                                 sutable for long (more than 5000     *
! *                                 observations) sessions.              *
! *  ELIM_CNF ( INTEGER*4 ) -- Confirmation mode switch. If .TRUE. then  *
! *                            confirmation before each elimination/     *
! *                            restoration will be inquired.             *
! *  ELIM_UPD ( INTEGER*4 ) -- Parameter, showing how frequently         *
! *                            residuals will be updated. Residuals are  *
! *                            updated after ELIM_UPD operation of       *
! *                            elimination/restoration.                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    PLACE  ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *     K_RJC ( INTEGER*4 ) -- Number of eliminated observations.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
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
! *  WHEN       WHO  WHAT                                                *
! *  24-SEP-97  pet  1.0 First version.                                  *
! *                                                                      *
! *  31-OCT-97  pet  2.0 Acceleration parameter ELIM_UPD added. Logic    *
! *                      changed to make update of the residuals once    *
! *                      after ELIM_UPD updates of the solution.         *
! *                                                                      *
! *  29-JAN-98  pet  2.2 Added some new parameters in the interface to   *
! *                      accomodate changes in MANOR_DO.                 *
! *                                                                      *
! *  24-MAR-98  pet  2.3 Changed the logic of work when threshold        *
! *                      criterion is in use. Now ELIM_DO scans all      *
! *                      observation in such a case. It eliminate the    *
! *                      observation if it fails threshold (or cutoff)   *
! *                      criterion, but it puts it in protection list    *
! *                      if it comes through both criterion and take     *
! *                      into consideration the next observation.        *
! *                                                                      *
! *  12-JUL-98  pet  3.0 Added singularity check. Rewrite the place      *
! *                      where reparameterization of the solution was    *
! *                      made.                                           *
! *                                                                      *
! *  02-AUG-98  pet  3.1 Added support of ELIM_VRB=2 more verbose mode.  *
! *                                                                      *
! *  03-AUG-98  pet  3.2 Changed logic to avoid decrease computational   *
! *                      time when threshold critera is in use.          *
! *                                                                      *
! *  22-NOV-98  pet  3.3 Added support of parameter ELIM_MSR -- Maximal  *
! *                      uncertainty. Observations with formal           *
! *                      uncertainty exceeding ELIM_MSR are marked as    *
! *                      outliers. If ELIM_MSR < 1.D-12 sec then it is   *
! *                      ignored.                                        *
! *                                                                      *
! *  23-NOV-98  pet  3.4 Prohibited the search of the best candidate for *
! *                      resolving ambiguities on the fly. It was        *
! *                      senseless in elimination mode but took a lot of *
! *                      time.                                           *
! *                                                                      *
! *  ###  19-SEP-97     ELIM_DO    v3.4  (c)  L. Petrov  23-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, ELIM_VRB, K_RJC, IUER
      CHARACTER  ELIM_TYP*2
      LOGICAL*4  ELIM_CNF, ELIM_MOD
      REAL*8     ELIM_THR, ELIM_CUT, ELIM_MSR
      INTEGER*4  ELIM_UPD
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
      LOGICAL*4  E_CNF, F_ACT, THR_USE, CUT_USE, F_STA
      INTEGER*4  M_OUT
      PARAMETER  ( M_OUT=512 )
      INTEGER*4  J1, IER, IOBS, IP, IT, IL, ISOU, ISTA, IBAS, ISC, E_TYP, &
     &           I_RJC, IOBS_LAST, NAMC, SNGCHK_CMP, ISIM, I1, I2
      CHARACTER  STR*128, STR1*20, STR_B*128, STR_A*128, OUT*128, ASIM*1, ESC*1, &
     &           COLOR_SETUP_GRAY*48, COLOR_SETUP_GREEN*48
      REAL*8     WRMS_B, WRMS_A, WNPR_B, WNPR_A, PSF_B, AMBS
      CHARACTER  ELIM_TYP__SAVE*2
      LOGICAL*4  ELIM_MOD__SAVE, ELIM_CNF__SAVE
      REAL*8     ELIM_CUT__SAVE, ELIM_THR__SAVE, ELIM_MSR__SAVE
      INTEGER*4  QUALCODE_GOOD_LIM__SAVE, ELIM_UPD__SAVE, ELIM_VRB__SAVE
      LOGICAL*4  FAMB_OLD, USE_TERM_COLOR, FL_OUT_TERM 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
  real*8 tim_r8, cpu_timer ! %%%%%%%%%%%%%%%%%%%%%
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, NSTBA, IFIND_PL, &
     &                       OTL_LINE, MAIN_OUTOBS, ELIM_LIST
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
#else
      LOGICAL*4, EXTERNAL ::  ISATTY_
#endif
!
      ESC = CHAR(27)
      CALL CLRCH ( COLOR_SETUP_GREEN )
      CALL CLRCH ( COLOR_SETUP_GRAY )
#ifdef GNU
      FL_OUT_TERM = ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#else
      FL_OUT_TERM = ISATTY_ ( 6 ) ! Flag whether the unit 6 is a terminal
#endif
!
! --- Inquiring information about terminal type
!
!@      CALL SHOW_IO ( IT, %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
      IT = 66
!
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
!
! -------- Making color setup
!
           COLOR_SETUP_GRAY  = ESC//'&v2S'//ESC//'&v0m0.77x0.81y0.86z2I'
           COLOR_SETUP_GREEN = ESC//'&v3S'//ESC//'&v0m0.71x0.86y0.65z3I'
      END IF
!
      E_CNF = ELIM_CNF
      K_RJC = 0 ! Counter of all rejected points
      I_RJC = 0 ! Counter of rejected points after the last resuduals update
      IOBS  = -1
!
! --- Initial depriving of "protection" from all observations
!
      CALL UNPROT_RES ( N_OBS, RES )
!
! --- Setting flags: were CUT-off and/or threshold criterion in use?
!
      IF ( DABS(ELIM_THR) .GT. 1.D-13 ) THEN
           THR_USE = .TRUE.
         ELSE
           THR_USE = .FALSE.
      END IF
!
      IF ( ELIM_CUT .GT. 0.01D0 ) THEN
           CUT_USE = .TRUE.
         ELSE
           CUT_USE = .FALSE.
      END IF
!
      DO 410 J1=1,DBOBJ%U_OBS-2
         IF ( ELIM_VRB .GE. 2 ) THEN
              CALL PRCH ( '  Solution statistics is being updated ...' )
         END IF
!
! ------ Save old value of F_AMB -- we don't need check possibility to resolve
! ------ ambiguity on the fly in eliimination mode
!
         FAMB_OLD    = DBOBJ%F_AMB
         DBOBJ%F_AMB = .FALSE.
!
! ------ Recalculation of the statistics in order to find the next main
! ------ outlier (excluding protected ones)
!
         RST%FIRST_FIELD = RST__MAXUP
         CALL ERR_PASS ( IUER, IER )
         CALL RESID_ST ( THR_USE, CUT_USE, ELIM_THR, ELIM_CUT, ELIM_MSR, &
     &                   0, N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
         DBOBJ%F_AMB = FAMB_OLD  !  Restore old value of F_AMB
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6631, IUER, 'ELIM_DO', 'Error during '// &
     &            'calculation statisics for the postfit residuals '// &
     &            'while database '//B3DOBJ%DBNAME_MES//' was processing' )
              RETURN
         END IF
!
         F_STA = .FALSE. ! Flag that there is no true statistics yet
!
         IF ( ELIM_VRB .GE. 2 ) THEN
              CALL PRCH ( '  Solution statistics is updated.' )
              CALL PRCH ( CHAR(10)//CHAR(13) )
         END IF
!
         IOBS_LAST = IOBS
         IOBS = MAIN_OUTOBS ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, RES, RST, &
     &                        PSF_B, WNPR_B, NAMC, AMBS )
!
! ------ Outlier has not been found. Wonderfull! We finish our business.
!
         IF ( IOBS .LE. 0 ) GOTO 810
!
! ------ Storing the values BEFORE elimination:
! ------ WRMS_B -- total WRMS for the experiment;
! ------ WNPR_B -- the normalized residual of the main outlier
! ------  PSF_B -- postfit residual of the main outlier
!
         WRMS_B = RST%WRMS_G
         IF ( ELIM_TYP .EQ. 'GL' ) THEN
              WNPR_B = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/RST%WDPR_G
            ELSE IF ( ELIM_TYP .EQ. 'BA' ) THEN
              IBAS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &                          NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                                 INT4(OBSBAS(IOBS)%ISITE(2)) ) )
              IF ( IBAS .GT. 0 ) THEN
                   WNPR_B = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/ &
     &                            RST%WDPR_B(IBAS)
                 ELSE
                   WNPR_B = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/ &
     &                            RST%WDPR_G
              END IF
         END IF
!
! ------ Making the information line about expelling outlier
!
         IOBS = OTL_LINE ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, DBOBJ, OBSSCA, &
     &                     OBSSTA, OBSBAS, RES, RST, STR_B )
!
! ------ Print it at the screen and spool file
!
         IF ( E_CNF  .OR.  ELIM_VRB .GE. 1 ) THEN
!
! ----------- Changing background color
!
              IF ( E_CNF  .AND.  USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                   CALL PRCH ( COLOR_SETUP_GRAY )
              END IF
              WRITE (  6, FMT='(A)' ) STR_B(1:79)
         END IF
         WRITE ( 23, FMT='(A)' ) STR_B(1:79)
!
         F_ACT = .TRUE.
         IF ( E_CNF ) THEN
!
! ----------- In the case of confimation mode -- generate a request to the user
! ----------- to confirm outlier rejection
!
              CALL CLRCH    ( STR )
              CALL INCH     ( DBOBJ%L_OBS, STR )
              STR = '  L_OBS='//STR(1:I_LEN(STR))//'('
!
              CALL INCH     ( DBOBJ%U_OBS, STR(I_LEN(STR)+1:) )
              STR(I_LEN(STR)+1:)= '){'
!
              CALL INCH     ( DBOBJ%R_OBS, STR(I_LEN(STR)+1:) )
              STR(I_LEN(STR)+1:)= '}'
!
              STR(55:) = 'Reject  (Y/N/A/S) [Y]  ? '
!
! ----------- At last printing et the screen prepared line...
!
              IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                   CALL PRCH ( COLOR_SETUP_GRAY )
              END IF
              CALL PRCH ( STR )
!
! ----------- ... and awaiting for the user's response
!
              CALL INSIM ( ASIM, ISIM )
!
! ----------- Clearing the line where prompt has been printed and reprinted
! ----------- a line above (to turn the background color to the initial
!
              CALL CLSTR()
              CALL CURU ( 1 )
              CALL CLSTR()
              WRITE     ( 6, FMT='(A)' ) STR_B(1:79)
!
! ----------- ... transforming the reply in the upper rgistr
!
              CALL TRAN ( 11, ASIM, ASIM )
!
! ----------- ... and decoding it
!
              IF ( ASIM .EQ. 'A' ) E_CNF = .FALSE.
              IF ( ASIM .EQ. 'N' ) F_ACT = .FALSE.
              IF ( ASIM .EQ. 'S' ) GOTO 810
         END IF
!
         IF ( F_ACT ) THEN
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution is being updated ...' )
              END IF
!
! ----------- Update of the solutiuon for elimination of the IOBS-th observation
!
              CALL ERR_PASS ( IUER, IER )
              CALL SOL_UPDATE ( -1, IOBS, IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &             OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &             PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IOBS, STR1 )
                   CALL ERR_LOG ( 6632, IUER, 'ELIM_DO', 'Error duiring '// &
     &                 'update of the '//STR1(1:I_LEN(STR1))//'-th '// &
     &                 'observation was detected while database '// &
     &                  DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
              K_RJC = K_RJC + 1
              I_RJC = I_RJC + 1
!
! ----------- Depriving the right to participate in estimation from this
! ----------- observation
!
              CALL ERR_PASS( IUER, IER )
              IL = ELIM_LIST ( N_OBS, IOBS, DBOBJ, OBSSCA, OBSBAS, E_TYP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6633, IUER, 'ELIM_DO', 'Error during '// &
     &                 'updating the list after elimination the observations '// &
     &                 'while the database '//B3DOBJ%DBNAME_MES// &
     &                 ' was processing' )
                   RETURN
              END IF
!
              IF ( E_TYP .EQ. E__SOU  .OR.  E_TYP .EQ. E__BAS  .OR. &
     &             E_TYP .EQ. E__STA1 .OR.  E_TYP .EQ. E__STA2      ) THEN
!
! ---------------- All observation of the certain source or the certain station
! ---------------- or the certain baseline have been eliminated.
! ---------------- Preparing line of the message
!
                   CALL CLRCH ( OUT )
                   IF ( E_TYP .EQ. E__SOU ) THEN
!
! --------------------- Find the index of rejected source in the source list
!
                        ISC = OBSBAS(IOBS)%IND_SCA
                        ISOU = IFIND_PL ( DBOBJ%L_SOU, DBOBJ%LIS_SOU, &
     &                                    INT4(OBSSCA(ISC)%ISTAR) )
                        OUT='All observations of the source '// &
     &                       DBOBJ%C_SOU(ISOU)//' have been eliminated'
                   END IF
!
! ---------------- Find the index of rejected station in the station list
!
                   IF ( E_TYP .EQ. E__STA1 ) THEN
                        ISTA = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                    INT4(OBSBAS(IOBS)%ISITE(1) ) )
                   END IF
!
                   IF ( E_TYP .EQ. E__STA2 ) THEN
                        ISTA = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                    INT4(OBSBAS(IOBS)%ISITE(2) ) )
                   END IF
!
                   IF ( E_TYP .EQ. E__STA1  .OR.  E_TYP .EQ. E__STA2 ) THEN
                        OUT='All observations at the station '// &
     &                  DBOBJ%C_STA(ISTA)//' have been eliminated'
                   END IF
!
                   IF ( E_TYP .EQ. E__BAS ) THEN
!
! --------------------- Find the index of rejected baseline in the baseline list
!
                        IBAS = NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                                INT4(OBSBAS(IOBS)%ISITE(2)) )
                        OUT='All observations at the baseline '// &
     &                       DBOBJ%C_BAS(IBAS)//' have been eliminated'
                   END IF
!
! ---------------- Printing message about it
!
                   IF ( ELIM_VRB .GE. 1 ) THEN
!
! --------------------- Changing background color
!
                        IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                             CALL PRCH ( COLOR_SETUP_GREEN )
                        END IF
                        WRITE (  6, FMT='(A)' ) OUT(1:79)
                   END IF
                   WRITE ( 23, FMT='(A)' ) OUT(1:79)
              END IF
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution is updated.' )
                   CALL PRCH ( CHAR(10)//CHAR(13) )
              END IF
!
              IF ( ELIM_VRB .GE. 3 ) THEN
                   CALL PRCH ( '  Singularity check is being made ... ' )
              END IF
!
! ----------- Saving global values in glbc4.i
!
              ELIM_MOD__SAVE = ELIM_MOD
              ELIM_TYP__SAVE = ELIM_TYP
              ELIM_CUT__SAVE = ELIM_CUT
              ELIM_THR__SAVE = ELIM_THR
              ELIM_MSR__SAVE = ELIM_MSR
              ELIM_UPD__SAVE = ELIM_UPD
              ELIM_VRB__SAVE = ELIM_VRB
              ELIM_CNF__SAVE = ELIM_CNF
!
! ----------- Make singularity check test
!
              CALL ERR_PASS ( IUER, IER )
              CALL ELIM_SNGCHK ( DBOBJ, N_OBS, L_SCA, OBSBAS, OBSSCA, &
     &                           SNGCHK_CMP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6634, IUER, 'ELIM_DO', 'Error during '// &
     &                 'singularity check after elimination of the outlier' )
                   RETURN
              END IF
!
! ----------- Restoring global values in glbc4.i
!
              ELIM_MOD = ELIM_MOD__SAVE
              ELIM_TYP = ELIM_TYP__SAVE
              ELIM_CUT = ELIM_CUT__SAVE
              ELIM_THR = ELIM_THR__SAVE
              ELIM_MSR = ELIM_MSR__SAVE
              ELIM_UPD = ELIM_UPD__SAVE
              ELIM_VRB = ELIM_VRB__SAVE
              ELIM_CNF = ELIM_CNF__SAVE
              IF ( N_OBS == 0 ) THEN
                   CALL ERR_LOG ( 6635, IUER, 'ELIM_DO', 'No used '// &
     &                 'observations remained in the database '//DBOBJ%NAME )
                   RETURN
              END IF
!
              IF ( ELIM_VRB .GE. 3 ) THEN
                   CALL PRCH ( '  Singularity check is done.' )
                   CALL PRCH ( CHAR(10)//CHAR(13) )
              END IF
!
              IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__CONT ) THEN
                   CONTINUE
                 ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__BACK ) THEN
!
! ---------------- Free memory used by B3DOBJ
!
                   CALL B3D_FREEMEM ( B3DOBJ, IER )
!
! ---------------- Then printing the message
!
                   CALL CLRCH ( OUT )
                   OUT='The number of parameters changed. Solution will be '// &
     &                 'wholly recalculated anew'
                   IF ( ELIM_VRB .GE. 1 ) THEN
!
! --------------------- Changing background color
!
                        IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                             CALL PRCH ( COLOR_SETUP_GREEN )
                        END IF
                        WRITE (  6, FMT='(A)' ) OUT(1:79)
                   END IF
                   WRITE ( 23, FMT='(A)' ) OUT(1:79)
!
! ---------------- Full recalculation of the solution
!
!
! ---------------- Saving global values in glbc4.i
!
                   ELIM_MOD__SAVE = ELIM_MOD
                   ELIM_TYP__SAVE = ELIM_TYP
                   ELIM_CUT__SAVE = ELIM_CUT
                   ELIM_THR__SAVE = ELIM_THR
                   ELIM_MSR__SAVE = ELIM_MSR
                   ELIM_UPD__SAVE = ELIM_UPD
                   ELIM_VRB__SAVE = ELIM_VRB
                   ELIM_CNF__SAVE = ELIM_CNF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL EQUMEM_END ( EQUMEM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6636, IUER, 'ELIM_DO', 'Error '// &
     &                      'during attemnpt to free EQUMEM data structure' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SOL_INIT ( ELIM_VRB, 1, 0, N_OBS, L_STA, L_SCA, IDB2, &
     &                             IDBF, PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, &
     &                             DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, &
     &                             RST, CHIOBJ, EQUMEM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6637, IUER, 'ELIM_DO', 'Error '// &
     &                      'during full update of the of the solution '// &
     &                      'for changing the number of estimated parameters'// &
     &                      ' was detected while database '//DBOBJ%NAME// &
     &                      ' was processing' )
                        RETURN
                   END IF
!
! ---------------- Restoring global values in glbc4.i
!
                   ELIM_MOD = ELIM_MOD__SAVE
                   ELIM_TYP = ELIM_TYP__SAVE
                   ELIM_CUT = ELIM_CUT__SAVE
                   ELIM_THR = ELIM_THR__SAVE
                   ELIM_MSR = ELIM_MSR__SAVE
                   ELIM_UPD = ELIM_UPD__SAVE
                   ELIM_VRB = ELIM_VRB__SAVE
                   ELIM_CNF = ELIM_CNF__SAVE
!
                   I_RJC = 0 ! Zeroing the counter of rejected points after the
!                            ! last residuals update
!
                   F_STA = .TRUE. ! Flag that there is true statistics
                 ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__STOP ) THEN
                   CALL ERR_LOG ( 6638, IUER, 'ELIM_DO', 'ELIM_SNGCHK' )
                   RETURN
                 ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__SKIP ) THEN
                   CONTINUE
                 ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__FAIL ) THEN
                   CALL ERR_LOG ( 6639, IUER, 'ELIM_DO', 'ELIM_SNGCHK' )
                   STOP 'PROC(LOOP): Abnormal termination'
              END IF  !   SNGCHK_CMP
!
              IF ( I_RJC .GE. ELIM_UPD   .OR.  IOBS .EQ. IOBS_LAST ) THEN
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Residuals are being updated ...' )
                 END IF
!
! -------------- Recalculation of all residuals (We will not do it if full
! -------------- update of the solution took place).
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL MANOR_DO ( ELIM_VRB, IDB2, PLACE, B3DOBJ, B1B3DOBJ, &
     &                           NCREC, IDBF, N_OBS, L_SCA, L_STA, OBSHLD, &
     &                           OBSSCA, OBSSTA, OBSBAS, DBOBJ, CHIOBJ, &
     &                           1, 0, 5, 0, IOBS, RES, EQUMEM, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6640, IUER, 'ELIM_DO', 'Error duiring '// &
     &                    'calculation of single equation of conditions '// &
     &                    'detected while database '//DBOBJ%NAME//' was '// &
     &                    'processing' )
                      RETURN
                 END IF
                 I_RJC = 0 ! Zeroing counter of rejected points after the
!                          ! last resuduals update
!
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Residuals are updated.' )
                      CALL PRCH ( CHAR(10)//CHAR(13) )
                 END IF
              END IF
!
              IF ( .NOT. F_STA )  THEN
!
! -------------- Update of statistics
!
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Solution statistics is being updated ..' )
                 END IF
!
! -------------- Save old value of F_AMB -- we don't need check popssibility
! -------------- to resolve ambiguity on the fly in eliimination mode
!
                 FAMB_OLD    = DBOBJ%F_AMB
                 DBOBJ%F_AMB = .FALSE.
!
! -------------- Recalculation of the statistics anew
!
                 RST%FIRST_FIELD = RST__INIT
                 CALL ERR_PASS ( IUER, IER )
                 CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, &
     &                ELIM_MSR, 0, N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
                 DBOBJ%F_AMB = FAMB_OLD  !  Restore old value of F_AMB
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6641, IUER, 'ELIM_DO', 'Error during '// &
     &                    'calculation statisics for the postfit residuals '// &
     &                    'while database '//B3DOBJ%DBNAME_MES//' was '// &
     &                    'processing' )
                      RETURN
                 END IF
!
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Solution statistics is updated.' )
                      CALL PRCH ( CHAR(10)//CHAR(13) )
                 END IF
                 F_STA = .TRUE. ! Flag that there is true statistics
              END IF ! of update od statistics
!
! ----------- Storing the values AFTER elimination:
! ----------- WRMS_A -- total WRMS for the experiment;
! ----------- WNPR_A -- the normalized residual of the main outlier
! -----------  PSF_A -- postfit residual of the main outlier
!
              WRMS_A = RST%WRMS_G
              IF ( ELIM_TYP .EQ. 'GL' ) THEN
                   WNPR_A = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/RST%WDPR_G
                 ELSE IF ( ELIM_TYP .EQ. 'BA' ) THEN
                   IBAS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &                               NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                                      INT4(OBSBAS(IOBS)%ISITE(2)) ) )
                   IF ( IBAS .GT. 0 ) THEN
                        WNPR_A = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/ &
     &                           RST%WDPR_B(IBAS)
                     ELSE
                        WNPR_A = RES(IOBS)%PSF_DEL*RES(IOBS)%WEI_DEL/ &
     &                           RST%WDPR_G
                   END IF
              END IF
!
              IF ( IOBS .NE. IOBS_LAST ) THEN
!
! ---------------- Formatting the output line (if it was not the case of
! ---------------- compalsory recalculation residuals and statistics )
!
                   CALL CLRCH ( STR_A )
                   CALL CLRCH ( STR1  )
                   CALL INCH  ( K_RJC, STR1 )
                   STR_A = STR1(1:I_LEN(STR1))//') eliminated:'
!
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1, FMT='(F8.1)' ) WRMS_A*1.D12
                   CALL CHASHL ( STR1 )
                   IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
                   STR_A(18:)=STR_B(18:27)//'wrms='//STR1
!
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1, FMT='(F8.1)' ) WRMS_B*1.D12
                   CALL CHASHL ( STR1 )
                   IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
                   STR_A(I_LEN(STR_A)+1:)='('//STR1(1:I_LEN(STR1))//')'
!
                   IP = INDEX ( STR_B, 'p=' ) - 1
                   IF ( ILEN(STR_A)+2 .GT. IP  ) IP = ILEN(STR_A)+2
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1, FMT='(F9.1)' ) RES(IOBS)%PSF_DEL*1.D12
                   CALL CHASHL ( STR1 )
                   IF ( STR1(1:1) .EQ. '.'  ) STR1='0'//STR1
                   IF ( STR1(1:2) .EQ. '-.' ) STR1='-0'//STR1(2:)
                   STR_A(IP:)='*p='//STR1
!
                   IP = INDEX ( STR_B, 'd=' )
                   IF ( ILEN(STR_A)+2 .GT. IP  ) IP = ILEN(STR_A)+2
                   WRITE ( UNIT=STR1, FMT='(F9.1)' ) WNPR_A
                   CALL CHASHL ( STR1 )
                   IF ( STR1(1:1) .EQ. '.'  ) STR1='0'//STR1
                   IF ( STR1(1:2) .EQ. '-.' ) STR1='-0'//STR1(2:)
                   STR_A(IP:)='d='//STR1(1:I_LEN(STR1))//'*'
!
! ---------------- Printint it at the screen and spool file
!
                   IF ( E_CNF  .OR.  ELIM_VRB .GE. 1 ) THEN
                        WRITE (  6, FMT='(A)' ) STR_A(1:79)
                   END IF
                   WRITE ( 23, FMT='(A)' ) STR_A(1:79)
              END IF
            ELSE ! .NOT. F_ACT
!
! ----------- If user refused to eliminate this observation then we put it in
! ----------- protection list in order not to bid it any more to user
!
              RES(IOBS)%PROT = .TRUE.
         END IF ! F_ACT
 410  CONTINUE
!
! --- Unexpected termination of the process (frankly speaking: the lack of
! --- termination).
!
      CALL ERR_LOG ( 6642, IUER, 'ELIM_DO', 'Oj! All observations but 2 '// &
     &              'have been rejected. Something is going really wrong...' )
      CALL UNPROT_RES ( N_OBS, RES )
      CALL HIT_CONT ( %VAL(0), %VAL(0) )
      RETURN
!
 810  CONTINUE
!
! --- Final messages
!
      IF ( K_RJC .GT. 0 ) THEN
!
! -------- Some outliers has been eliminated
!
           CALL CLRCH ( OUT )
           CALL INCH  ( K_RJC, OUT )
           IF ( IOBS .LE. 0 ) THEN
                OUT(I_LEN(OUT)+2:) = 'outliers have been rejected. No more '// &
     &                               'outliers were detected'
              ELSE
                OUT(I_LEN(OUT)+2:) = 'outliers have been rejected. Some '// &
     &                               'others remained '
           END IF
           IF ( ELIM_VRB .GE. 1 ) THEN
                IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                     CALL PRCH ( COLOR_SETUP_GREEN )
                END IF
                WRITE (  6, FMT='(A)' ) OUT(1:I_LEN(OUT))
           END IF
           WRITE ( 23, FMT='(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( K_RJC .EQ. 0 ) THEN
!
! -------- No on outlier has been rejected
!
           CALL CLRCH ( OUT )
           OUT(1:) = 'No one outlier has been detected'
           IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
                CALL PRCH ( COLOR_SETUP_GREEN )
           END IF
           WRITE (  6, FMT='(A)' ) OUT(1:I_LEN(OUT))
           WRITE ( 23, FMT='(A)' ) OUT(1:I_LEN(OUT))
      END IF
!
      IF ( USE_TERM_COLOR() .AND. FL_OUT_TERM ) THEN
           CALL PRCH ( COLOR_SETUP_GREEN )
      END IF
!
! --- Removing protection form outliers
!
      CALL UNPROT_RES ( N_OBS, RES )
!
! --- And calculation the best candidate once more
!
      RST%FIRST_FIELD = RST__INIT
      CALL ERR_PASS ( IUER, IER )
      CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, ELIM_MSR, 0, &
     &                N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6643, IUER, 'ELIM_DO', 'Error during '// &
     &         'calculation statistics for the postfit residuals while '// &
     &         'database '//B3DOBJ%DBNAME_MES//' was processing' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_DO  #!#
