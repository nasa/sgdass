      SUBROUTINE MILE_DO ( IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &           PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &           ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &           ELIM_MSR, ELIM_UPD, ELIM_AMB, ELIM_ION, K_RST, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MILE_DO  updates solution made by LSQ method in B3D mode  *
! *   for restoration of the observations which have been eliminated     *
! *   previously or have been rejected by DBEDIT from the very beginning.*
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
! *                                 suitable for long (more than 5000    *
! *                                 observations) sessions.              *
! *  ELIM_CNF ( INTEGER*4 ) -- Confirmation mode switch. If .TRUE. then  *
! *                            confirmation before each elimination/     *
! *                            restoration will be inquired.             *
! *                            parameters will be revised.               *
! *  ELIM_UPD ( INTEGER*4 ) -- Parameter, showing how frequently         *
! *                            residuals will be updated. Residuals are  *
! *                            updated after ELIM_UPD operation of       *
! *                            elimination/restoration.                  *
! *  ELIM_AMB ( LOGICAL*4 ) -- If .TRUE. MILE when search of the best    *
! *                            candidate to restoration will resolve     *
! *                            ambiguity (group or phase in according    *
! *                            with solution type) and try to recover    *
! *                            observation with resolved ambiguity.      *
! *  ELIM_ION ( LOGICAL*4 ) -- If .TRUE. and ELIM_AMB is also .TRUE.     *
! *                            then MILE when search of the best         *
! *                            candidate to restoration will resolve     *
! *                            ambiguity (group or phase in according    *
! *                            with solution type) and take into account *
! *                            change of residuals for changes           *
! *                            ionosphere correction occurred because    *
! *                            change of ambiguity.                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    PLACE  ( RECORD    ) -- Object with data structure for place of   *
! *                            parameters in the list of derivatives.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE.                       *
! *     K_RST ( INTEGER*4 ) -- Number of restored observations.          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
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
! *  25-SEP-97  pet  1.0 First version.                                  *
! *  31-OCT-97  pet  2.0 Acceleration parameter ELIM_UPD added. Logic    *
! *                      changed to make update of the residuals once    *
! *                      after ELIM_UPD updates of the solution.         *
! *  29-JAN-98  pet  2.2 Added some new parameters in the interface to   *
! *                      accommodate changes in MANOR_DO.                *
! *  23-MAR-98  pet  3.0 Added support case when ambiguities and         *
! *                      changes of ionosphere correction due to         *
! *                      changes in ambiguities are updated in the fly.  *
! *  24-MAR-98  pet  3.1 Added recalculation the best candidate after    *
! *                      unprotecting all observation at the very end    *
! *                      of the process.                                 *
! *                                                                      *
! *  02-AUG-98  pet  3.2 Added support of ELIM_VRB=2 more verbose mode.  *
! *                                                                      *
! *  03-AUG-98  pet  3.3 Changed logic to avoid decrease computational   *
! *                      time when threshold criteria is in use.         *
! *                                                                      *
! *  03-NOV-98  pet  3.4 Change the logic of support resolving           *
! *                      ambiguities on the fly in restoration mode. New *
! *                      logic conserves misclosures excess. It is       *
! *                      assumed that misclosure excess for observables  *
! *                      doesn't excess one ambiguity jump.              *
! *                                                                      *
! *  22-NOV-98  pet  3.5 Added support of parameter ELIM_MSR -- Maximal  *
! *                      uncertainty. Observations with formal           *
! *                      uncertainty exceeding ELIM_MSR are not eligible *
! *                      for restoration. If ELIM_MSR < 1.D-12 sec then  *
! *                      it is ignored.                                  *
! *                                                                      *
! *  ###  25-SEP-97     MILE_DO    v3.5  (c)  L. Petrov  22-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, L_SCA, L_STA, ELIM_VRB, ELIM_UPD, K_RST, IUER
      CHARACTER  ELIM_TYP*2
      LOGICAL*4  ELIM_CNF, ELIM_MOD, ELIM_AMB, ELIM_ION
      REAL*8     ELIM_THR, ELIM_CUT, ELIM_MSR
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
      LOGICAL*4  E_CNF, F_ACT, THR_USE, CUT_USE, F_PRMT
      INTEGER*4  J1, J2, IER, IOBS, IP, IT, IBAS, I_RST, IOBS_LAST, NAMC
      INTEGER*4  LP_OBS, LPIS_OBS(MO_BAS), ISGN_OBS(MO_BAS), ISIM
      CHARACTER  STR*128, STR1*20, STR_B*128, STR_A*128, OUT*128, ASIM*1, ESC*1, &
     &           COLOR_SETUP_YELLOW*48, COLOR_SETUP_GREEN*48
      REAL*8     WRMS_B, WRMS_A, WNPR_B, WNPR_A, PSF_B, AMBS, AMBION_SP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: DATYP_INQ, AMBCHA_PERMIT, USE_TERM_COLOR
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, NSTBA, IFIND_PL, &
     &                       OTL_LINE, MAIN_OUTOBS
!
! --- Making color setup
!
      ESC = CHAR(27)
      CALL CLRCH ( COLOR_SETUP_YELLOW )
      CALL CLRCH ( COLOR_SETUP_GREEN )
!
! --- Inquiring information about terminal type
!
!@      CALL SHOW_IO ( IT, %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
      IT = 66
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() ) THEN
           COLOR_SETUP_YELLOW = ESC//'&v2S'//ESC//'&v0m0.86x0.84y0.66z2I'
           COLOR_SETUP_GREEN  = ESC//'&v3S'//ESC//'&v0m0.71x0.86y0.65z3I'
      END IF
!
      E_CNF = ELIM_CNF
      K_RST = 0 ! Counter of all restrored points
      I_RST = 0 ! Counter of restrored points after the last resuduals update
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
      DO 410 J1=1,DBOBJ%R_OBS
         IF ( ELIM_VRB .GE. 2 ) THEN
              CALL PRCH ( '  Solution statistics is being updated ...' )
         END IF
!
! ------ Recalculation of the statistics in order to find the next main
! ------ outlier (excluding protected ones)
!
         RST%FIRST_FIELD = RST__MAXUP
         CALL ERR_PASS ( IUER, IER )
         CALL RESID_ST ( THR_USE, CUT_USE, ELIM_THR, ELIM_CUT, ELIM_MSR, 0, &
     &                   N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6641, IUER, 'MILE_DO', 'Error during '// &
     &            'calculation statisics for the postfit residuals '// &
     &            'while database '//B3DOBJ%DBNAME_MES//' was processing' )
              RETURN
         END IF
!
         IF ( ELIM_VRB .GE. 2 ) THEN
              CALL PRCH ( '  Solution statistics is updated.' )
              CALL PRCH ( CHAR(10)//CHAR(13) )
         END IF
!
! ------ Storing the values BEFORE restoration:
! ------ WRMS_B -- total WRMS for the experiment;
! ------ WNPR_B -- the normalized residual of the best candidate
! ------  PSF_B -- postfit residual of the best candidate
!
         IOBS_LAST = IOBS
         IOBS = MAIN_OUTOBS ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, RES, RST, &
     &                        PSF_B, WNPR_B, NAMC, AMBS )
         WRMS_B = RST%WRMS_G
!
! ------ No one candidate in restoration has been found
!
         IF ( IOBS .LE. 0 ) GOTO 810
!
! ------ Making the information line about restoring best candidate
!
         IOBS = OTL_LINE ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, DBOBJ, &
     &                     OBSSCA, OBSSTA, OBSBAS, RES, RST, STR_B )
!
! ------ Print it at the screen and spool file
!
         IF ( E_CNF  .OR.  ELIM_VRB .GE. 1 ) THEN
!
! ----------- Changing background color
!
              IF ( E_CNF  .AND.  USE_TERM_COLOR() ) THEN
                   CALL PRCH ( COLOR_SETUP_YELLOW )
              END IF
              WRITE (  6, FMT='(A)' ) STR_B(1:79)
         END IF
         WRITE ( 23, FMT='(A)' ) STR_B(1:79)
!
         F_ACT = .TRUE.
         IF ( E_CNF ) THEN
!
! ----------- In the case of confimation mode -- generate a request to the user
! ----------- to confirm restoration
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
              STR(55:) = 'Restore (Y/N/A/S) [Y]  ? '
!
! ----------- At last printing at the screen prepared line...
!
              IF ( USE_TERM_COLOR() ) THEN
                   CALL PRCH ( COLOR_SETUP_YELLOW )
              END IF
              CALL PRCH ( STR )
!
! ----------- ... and awaiting for the user's response
!
              CALL INSIM ( ASIM, ISIM )
!
! ----------- Clearing the line where prompt has been printed and reprinted
! ----------- a line above (to turn the background color to the initial stature)
!
              CALL CLSTR()
              CALL CURU ( 1 )
              CALL CLSTR()
              WRITE     ( 6, FMT='(A)' ) STR_B(1:79)
!
! ----------- ... transforming the reply in the upper registr
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
              IF ( NAMC .NE. 0 ) THEN
!
! ---------------- Best candicate in restoration requires change of ambiguities.
! ---------------- 1) We update counter of new ambiguities for all observations
! ----------------    forming the chain
! ---------------- 2) We update O-C in RES data structure for all observations
! ----------------    forming the chain
! ---------------- 3) We update observables in OBSBAS
!
                   CALL ERR_PASS ( IUER, IER )
!
! ---------------- Ask permission and form the list of observatins forming
! ---------------- the chain. Change of the ambiguity for the best candidates
! ---------------- results in change of the ambiguities of other observations
! ---------------- from ing the chain in order to preserve misclosure
!
                   F_PRMT = AMBCHA_PERMIT ( IOBS, DBOBJ, OBSBAS, MO_BAS, &
     &                                      LP_OBS, LPIS_OBS, ISGN_OBS, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH   ( STR )
                        CALL INCH    ( IOBS, STR )
                        CALL ERR_LOG ( 6642, IUER, 'MILE_DO', 'Error '// &
     &                      'in attempt to check eligility of the '// &
     &                       STR(1:I_LEN(STR))//'-th observation for '// &
     &                      'ambuguity change' )
                        RETURN
                   END IF
!
                   IF ( .NOT. F_PRMT ) THEN
                        CALL ERR_LOG ( 6643, IUER, 'MILE_DO', 'Error '// &
     &                      'of internal control: ambiguity change was not '// &
     &                      'permited' )
                        RETURN
                   END IF
!
! ---------------- Update the ambiguity and observables for all observations
! ---------------- forming the chain
!
                   DO 420 J2=1,LP_OBS
                      IP = LPIS_OBS(J2)
                      RES(IP)%NUMAMB_NEW = RES(IP)%NUMAMB_NEW + &
     &                                     ISGN_OBS(J2)*NAMC
                      IF ( ELIM_ION ) THEN
!
! ------------------------ Calculate AMBION_SP  -- effective ambiguity spacing
! ------------------------ for combination. Value of combination will be changed
! ------------------------ by AMBION_SP when ambiguity is changed by one phase
! ------------------------ turn
!
                           IF ( DBOBJ%IDATYP .EQ. P_PXS__DTP ) THEN
                                AMBION_SP = RES(IP)%AMBION_SP
                             ELSE IF ( DBOBJ%IDATYP .EQ. G_GXS__DTP ) THEN
                                AMBION_SP = RES(IP)%AMBION_SP
                             ELSE IF ( DBOBJ%IDATYP .EQ. PX_GS__DTP ) THEN
                                AMBION_SP = RES(IP)%AMB_SP * &
     &                                      OBSBAS(IP)%FREQ_IONO_PH**2/ &
     &                                    ( OBSBAS(IP)%FREQ_IONO_PH**2 + &
     &                                      OBSBAS(IP)%FREQ_IONO_GR_OPP**2 )
                             ELSE IF ( DBOBJ%IDATYP .EQ. PS_GS__DTP ) THEN
                                AMBION_SP = RES(IP)%AMB_SP * &
     &                                      OBSBAS(IP)%FREQ_IONO_PH_OPP**2/ &
     &                                    ( OBSBAS(IP)%FREQ_IONO_PH_OPP**2 + &
     &                                      OBSBAS(IP)%FREQ_IONO_GR_OPP**2 )
                             ELSE
                                CALL CLRCH ( STR )
                                CALL INCH  ( INT4(DBOBJ%IDATYP), STR )
                                CALL ERR_LOG ( 6644, IUER, 'MILE_DO', 'Trap '// &
     &                              'of internal control: unsupported '// &
     &                              'solution type :'//STR(1:I_LEN(STR))// &
     &                              ' for ambiguity change on the fly' )
                                RETURN
                           END IF
!
! ------------------------ Residual and o-c is corrected on the fly for
! ------------------------ changes of ambiguity spacing and as a consequence
! ------------------------ of that the change in ionosphere calibration
!
                           RES(IP)%OC_DEL  = RES(IP)%OC_DEL + &
     &                                       ISGN_OBS(J2)*NAMC*AMBION_SP
                           RES(IP)%PSF_DEL = RES(IP)%PSF_DEL + &
     &                                       ISGN_OBS(J2)*NAMC*AMBION_SP
                         ELSE
!
! ------------------------ Residual and o-c is corrected on the fly for
! ------------------------ changes of ambiguity spacing but not for changes
! ------------------------ in ionosphere calibration
!
                           RES(IP)%OC_DEL  = RES(IP)%OC_DEL + &
     &                                       ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                           RES(IP)%PSF_DEL = RES(IP)%PSF_DEL + &
     &                                       ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                      END IF
!
! ------------------- Change of ambiguity in primary observables
!
                      IF ( DBOBJ%IDATYP .EQ. P_PXS__DTP ) THEN
                           OBSBAS(IP)%TAUPH_OBS = OBSBAS(IP)%TAUPH_OBS + &
     &                                          ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                         ELSE IF ( DBOBJ%IDATYP .EQ. G_GXS__DTP ) THEN
                           OBSBAS(IP)%TAUGR_OBS = OBSBAS(IP)%TAUGR_OBS + &
     &                                          ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                         ELSE IF ( DBOBJ%IDATYP .EQ. PX_GS__DTP ) THEN
                           OBSBAS(IP)%TAUPH_OBS = OBSBAS(IP)%TAUPH_OBS + &
     &                                          ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                         ELSE IF ( DBOBJ%IDATYP .EQ. PS_GS__DTP ) THEN
                           OBSBAS(IP)%TAUPH_OBS_OPP = OBSBAS(IP)%TAUPH_OBS_OPP + &
     &                                          ISGN_OBS(J2)*NAMC*RES(IP)%AMB_SP
                         ELSE
                           CALL CLRCH ( STR )
                           CALL INCH  ( INT4(DBOBJ%IDATYP), STR )
                           CALL ERR_LOG ( 6645, IUER, 'MILE_DO', 'Trap '// &
     &                         'of internal control: unsupported solution '// &
     &                         'type :'//STR(1:I_LEN(STR))//' for ambiguity '// &
     &                         'change on the fly' )
                           RETURN
                      END IF
 420               CONTINUE
!
! ---------------- Set flag that ambiguities have been changed
!
                   DBOBJ%F_AMB_CHANGED = .TRUE.
              END IF
!
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution is being updated ...' )
              END IF
!
! ----------- Update of the solution for restoration of the IOBS-th observation
!
              CALL ERR_PASS ( IUER, IER )
              CALL SOL_UPDATE ( 1, IOBS, IDB2, IDBF, N_OBS, L_SCA, L_STA, &
     &             OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &             PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IOBS, STR1 )
                   CALL ERR_LOG ( 6646, IUER, 'MILE_DO', 'Error during '// &
     &                 'update of the '//STR1(1:I_LEN(STR1))//'-th '// &
     &                 'observation was detected while database '// &
     &                  DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
!
              K_RST = K_RST + 1
              I_RST = I_RST + 1
!
! ----------- Updating the lists and full reabilitation in the rights to
! ----------- participate in the estiamtion for this observation
!
              CALL MILE_LIST ( N_OBS, IOBS, DBOBJ, OBSSCA, OBSBAS )
!
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution is updated.' )
                   CALL PRCH ( CHAR(10)//CHAR(13) )
              END IF
!
  820         CONTINUE
              IF ( ( I_RST .GE. ELIM_UPD )  .OR.  ( IOBS .EQ. IOBS_LAST ) ) THEN
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Residuals are being updated ...' )
                 END IF
!
! -------------- Recalculation of all residuals
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL MANOR_DO ( ELIM_VRB, IDB2, PLACE, B3DOBJ, B1B3DOBJ, &
     &                           NCREC, IDBF, N_OBS, L_SCA, L_STA, OBSHLD, &
     &                           OBSSCA, OBSSTA, OBSBAS, DBOBJ, CHIOBJ, &
     &                           1, 0, 5, 0, IOBS, RES, EQUMEM, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6647, IUER, 'MILE_DO', 'Error during '// &
     &                    'calculation of single equation of conditions '// &
     &                    'detected while database '//DBOBJ%NAME//' was '// &
     &                    'processing' )
                      RETURN
                 END IF
                 I_RST = 0 ! Zeroing counter of restored points after the
!                          ! last resuduals update
!
                 IF ( ELIM_VRB .GE. 2 ) THEN
                      CALL PRCH ( '  Residuals are updated.' )
                      CALL PRCH ( CHAR(10)//CHAR(13) )
                 END IF
              END IF
!
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution statistics is being updated ..' )
              END IF
!
! ----------- Recalculation of the statistics anew
!
              RST%FIRST_FIELD = RST__INIT
              CALL ERR_PASS ( IUER, IER )
              CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, ELIM_MSR, &
     &                      IOBS, N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6648, IUER, 'MILE_DO', 'Error during '// &
     &                 'calculation statisics for the postfit residuals '// &
     &                 'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                   RETURN
              END IF
!
              IF ( ELIM_VRB .GE. 2 ) THEN
                   CALL PRCH ( '  Solution statistics is updated.' )
                   CALL PRCH ( CHAR(10)//CHAR(13) )
              END IF
!
! ----------- Storing the values AFTER restoration:
! ----------- WRMS_A -- total WRMS for the experiment;
! ----------- WNPR_A -- the normalized residual of the best candidate
! -----------  PSF_A -- postfit residual of the best candidate
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
! ----------- Formatting the output line
!
              CALL CLRCH ( STR_A )
              CALL CLRCH ( STR1  )
              CALL INCH  ( K_RST, STR1 )
              STR_A = STR1(1:I_LEN(STR1))//') restored:'
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
! ----------- Printint it at the screen and spool file
!
              IF ( E_CNF  .OR.  ELIM_VRB .GE. 1 ) THEN
                   WRITE (  6, FMT='(A)' ) STR_A(1:79)
              END IF
              WRITE ( 23, FMT='(A)' ) STR_A(1:79)
!
            ELSE
!
! ----------- If user refused to restore this observation then we put it in
! ----------- protection list in order not to bid it any more to user
!
              RES(IOBS)%PROT = .TRUE.
         END IF
 410  CONTINUE
      IOBS = 0
!
 810  CONTINUE
!
! --- Final messages
!
      IF ( K_RST .GT. 0 ) THEN
!
! -------- Some observations have been restored
!
           CALL CLRCH ( OUT )
           CALL INCH  ( K_RST, OUT )
           IF ( IOBS .LE. 0 ) THEN
                IF ( DBOBJ%R_OBS .EQ. 0 ) THEN
                     OUT(I_LEN(OUT)+2:) = 'obs. were restrored. There is no '// &
     &                                  'more candidates in restoration at all'
                   ELSE
                     OUT(I_LEN(OUT)+2:) = 'obs. were restrored. No more '// &
     &                               'candidates in restoration were detected'
                END IF
              ELSE
                OUT(I_LEN(OUT)+2:) = 'obs. were restrored. Some others '// &
     &                               'candidates in restoration remained '
           END IF
           IF ( ELIM_VRB .GE. 1 ) THEN
                IF ( USE_TERM_COLOR() ) THEN
                     CALL PRCH ( COLOR_SETUP_GREEN )
                END IF
                WRITE (  6, FMT='(A)' ) OUT(1:I_LEN(OUT))
           END IF
           WRITE ( 23, FMT='(A)' ) OUT(1:I_LEN(OUT))
         ELSE IF ( K_RST .EQ. 0 ) THEN
!
! -------- No on observation has been restrored
!
           CALL CLRCH ( OUT )
           OUT = 'No one candidate in restoration has been detected'
           IF ( USE_TERM_COLOR() ) THEN
                CALL PRCH ( COLOR_SETUP_GREEN )
           END IF
           WRITE (  6, FMT='(A)' ) OUT(1:I_LEN(OUT))
           WRITE ( 23, FMT='(A)' ) OUT(1:I_LEN(OUT))
      END IF
!
      IF ( USE_TERM_COLOR() ) THEN
           CALL PRCH ( COLOR_SETUP_GREEN )
      END IF
!
! --- Removing protection from all observations
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
           CALL ERR_LOG ( 6649, IUER, 'MILE_DO', 'Error during '// &
     &         'calculation statisics for the postfit residuals while '// &
     &         'database '//B3DOBJ%DBNAME_MES//' was processing' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MILE_DO  #!#
