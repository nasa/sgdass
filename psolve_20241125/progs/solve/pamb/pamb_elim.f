      SUBROUTINE PAMB_ELIM ( IACT, PAMB_VER, N_OBS, IDBF, IDB2, &
     &           DBOBJ, NCREC, OBSHLD, OBSSCA, OBSSTA, OBSBAS, RES, PAMBI, &
     &           PLACE, B3DOBJ, B1B3DOBJ, RST, CHIOBJ, OBORG_UPDATE, &
     &           EQUMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_ELIM  makes outliers elimination/restoration and/or  *
! *   weights update. It is assumed that it is called from PAMB (Phase   *
! *   delay AMBiguity resolution).                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IACT ( INTEGER*4 ) -- Code of the procedure to be run:          *
! *                            131 -- outlier elimination procedure.     *
! *                            132 -- restoration observations previously*
! *                                   suppressed.                        *
! *                            133 -- weights update: calculation such a *
! *                                   quantity (global of                *
! *                                   baseline-dependent), which being   *
! *                                   quadratically added to the formal  *
! *                                   error will bring the ratio of      *
! *                                   the square sum of weighted         *
! *                                   residuals to its value to be near  *
! *                                   to unity.                          *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity level.                          *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in       *
! *                            the scratch file.                         *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *     NCREC ( RECORD    ) -- Data structure for transferring           *
! *                            parameters between SOLVE cutil            *
! *                            subroutines: NCORT, SOCAL, ATMPART.       *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
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
! *       RST ( RECORD    ) -- Data structure keeping the statistics     *
! *                            of postfit residuals.                     *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and       *
! *                            their mathematical expectations.          *
! *    EQUMEM ( RECORD    ) -- Object with data structure for keeping    *
! *                            equations of conditions in memory.        *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * OBORG_UPDATE ( LOGICAL*4 ) -- Flag: if .TRUE. oborg area should      *
! *                               be updated, since upweight (and may be *
! *                               ambiguity) status has been changed.    *
! *                                                                      *
! *  ###  19-MAR-98    PAMB_ELIM   v1.4  (c)  L. Petrov 31-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'fast.i'
      INCLUDE   'pamb.i'
      INCLUDE   'equmem.i'
      INTEGER*4  IACT, IDBF, N_OBS, PAMB_VER, OBORG_UPDATE, IUER
      INTEGER*4  IDB2
      TYPE ( NCREC__STRU ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( STA_O__STRU ) ::  OBSSTA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( EQUMEM__STRU ) ::  EQUMEM
      CHARACTER  DBNAME*16
      LOGICAL*4  DATYP_INQ
      LOGICAL*4   F_OPTIN, F_SAVE, F_UPDATE, F_UPWEI, F_CHI, F_SUP, UPWEI_WAS, &
     &            F_UPWEI_INIT, F_SUPSTAT
      INTEGER*4   KG_RJC, KG_RST, K_RJC, K_RST, ISIM, IER
      CHARACTER   STR*32, STR1*32, STR2*32, ASIM*1
      CHARACTER   VER_ELIM*19, VER_MILE*19
      PARAMETER  ( VER_ELIM = 'PAMB_ELIM 21-DEC-98' )
      PARAMETER  ( VER_MILE = 'PAMB_MILE 21-DEC-98' )
      LOGICAL*4   SUP_UPDATE
      INTEGER*4   I_LEN
!
! --- Initializations
!
      CALL CLRCH ( DBNAME )
      DBNAME = DBOBJ%NAME
      KG_RJC    =  0
      KG_RST    =  0
      K_RJC     =  0
      K_RST     =  0
      F_UPDATE  = .FALSE.
      UPWEI_WAS = .FALSE.
      F_OPTIN   = .FALSE.
!
! --- Unprotecting all observations (protecting means that some
! --- observations may be barred from elimination or restoration)
!
      CALL UNPROT_RES ( N_OBS, RES )
      F_CHI     = .TRUE.  ! Flag: chi/ndg has been calculated
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      IF ( IACT .EQ. 131 ) THEN
!
! -------- Outlier elimination mode
!
           ELIM_MOD     = .TRUE.
           F_UPWEI      = .FALSE.
           F_UPWEI_INIT = .FALSE.
         ELSE IF ( IACT .EQ. 132 ) THEN
!
! -------- Restoration of suppressed observations mode
!
           ELIM_MOD = .FALSE.
           F_UPWEI  = .FALSE.
           F_UPWEI_INIT = .FALSE.
         ELSE IF ( IACT .EQ. 133 ) THEN
!
! -------- Weight update mode
!
           F_UPWEI      = .TRUE.
           F_UPWEI_INIT = .TRUE.
      END IF
!
 910  CONTINUE
      DO WHILE ( .NOT. F_OPTIN )
!
! ------ Calling menu. Parameters of the ELIM (variables with prefix ELIM)
! ------ may change their values.
! ------ Flags F_OPTIN, F_SAVE, F_UPDATE may bve set up
! ------ F_OPTIN -- exit from the program
! ------ F_SAVE  -- suboption of the exit: F_SAVE = .TRUE. -- downweight flag
! ------            in scratch file will be updated, F_SAVE = .FALSE. -- not to
! ------            do it. Results of the work of ELIM/MILE will be lost in
! ------            that case
! ------ F_UPDATE -- make only update of the statistics but not
! ------             elimination/restoration
!
         IF ( .NOT. F_UPWEI_INIT ) THEN
              CALL ERR_PASS  ( IUER, IER )
              CALL ELIM_MENU ( VER_ELIM, VER_MILE, N_OBS, DBOBJ, OBSSCA, &
     &             OBSSCA, OBSBAS, RES, RST, CHIOBJ, ELIM_MOD, ELIM_THR, &
     &             ELIM_CUT, ELIM_MSR, ELIM_TYP, ELIM_VRB, ELIM_CNF, ELIM_UPD, &
     &             ELIM_AMB, ELIM_ION, QUALCODE_GOOD_LIM, SUPMET, EQUMEM_FLAG, &
     &             SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, SNGCHK_BASMIN, &
     &             F_CHI, F_OPTIN, F_SAVE, F_UPDATE, F_UPWEI, F_SUPSTAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6301, IUER, 'PAMB_ELIM', 'Error during '// &
     &                 'work of ELIM_MENU detected while database '//DBNAME// &
     &                 ' was processing' )
                  GOTO 810
              END IF
         END IF
!
         IF ( F_OPTIN ) GOTO 810   ! End of work
         CALL USE_GLBFIL_4 ( 'OWC' )   !  Storing the results of menu changes
         IF ( .NOT. F_UPDATE  .AND.  .NOT. ELIM_MOD  .AND. &
     &        .NOT. F_UPWEI   .AND.  .NOT. F_SUPSTAT       ) THEN
!
! ----------- Preparing messages which will be put into scratch file
!
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR, FMT='(F9.2)' ) ELIM_CUT
              IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) &
     &        STR(I_LEN(STR):I_LEN(STR)) = ' '
              CALL CHASHL ( STR )
              STR = STR(1:I_LEN(STR))//' sigma'
!
              CALL CLRCH ( STR1 )
              IF ( ELIM_THR .GT. 1.D-13 ) THEN
                   WRITE ( UNIT=STR1, FMT='(F8.1)' ) ELIM_THR*1.D12
                   IF ( STR1(I_LEN(STR1):I_LEN(STR1)) .EQ. '0' ) &
     &             STR1(I_LEN(STR1):I_LEN(STR1)) = ' '
                   CALL CHASHL ( STR1 )
                   STR1 = STR1(1:I_LEN(STR1))//' psec'
                ELSE
                   STR1 = '"not specified"'
              END IF
!
              CALL CLRCH ( STR2 )
              IF ( ELIM_TYP .EQ. 'GL' ) STR2='"Global"'
              IF ( ELIM_TYP .EQ. 'BA' ) STR2='"Baseline"'
!
              WRITE ( 23, FMT='(A)' ) 'ELIM  Threshold = '// &
     &                                 STR1(1:I_LEN(STR1))// &
     &                                ',  Cutoff = '//STR(1:I_LEN(STR))// &
     &                                ',  Type = '//STR2(1:I_LEN(STR2))
!
              CALL CLRCH ( STR )
              CALL INCH  ( QUALCODE_GOOD_LIM, STR )
              WRITE ( 23, FMT='(A)' ) 'ELIM  Qualcode_good_lim = '// &
     &                                 STR(1:I_LEN(STR))
          END IF
!
          IF ( .NOT. F_UPDATE  .AND.  ELIM_MOD        .AND. &
     &         .NOT. F_UPWEI   .AND.  .NOT. F_SUPSTAT       ) THEN
               WRITE ( 23, FMT='(A)' ) 'ELIM  ran in "elimination" mode'
!
! ------------ Executing iterative outliers elimination
!
               CALL ERR_PASS ( IUER, IER )
               CALL ELIM_DO  ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &              OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &              ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &              ELIM_MSR, ELIM_UPD, K_RJC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6302, IUER, 'PAMB_ELIM', 'Error during '// &
     &                 'elimination of the outliers' )
                    GOTO 810
               END IF
               KG_RJC = KG_RJC + K_RJC
!
               CALL PRCH ( 'Hit any key to proceed '//CHAR(1) )
!
! ------------ Awaiting for the reaction of the user
!
               CALL INSIM ( ASIM, ISIM )
          END IF
!
          IF ( .NOT. F_UPDATE  .AND.  .NOT. ELIM_MOD  .AND. &
     &         .NOT. F_UPWEI   .AND.  .NOT. F_SUPSTAT       ) THEN
               WRITE ( 23, FMT='(A)' ) 'ELIM  ran in "resoration" mode'
               IF ( ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &                DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP )      )  ) THEN
!
! ----------------- ... but only for combinations
!
                    DBOBJ%F_AMB = ELIM_AMB
                    DBOBJ%F_ION = ELIM_ION
                  ELSE
!
! ------------------ .... for other types of solution we set flag as .FASLE.
!
                    DBOBJ%F_AMB = .FALSE.
                    DBOBJ%F_ION = .FALSE.
               END IF
!
               DBOBJ%F_AMB_CHANGED = .FALSE.
!
! ------------ Executing iterative restoration of the observations prevously
! ------------ eliminated from the solution
!
               CALL ERR_PASS ( IUER, IER )
               CALL MILE_DO  ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &              OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &              ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &              ELIM_MSR, ELIM_UPD, ELIM_AMB, ELIM_ION, K_RST, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6303, IUER, 'PAMB_ELIM', 'Error during '// &
     &                 'restoration of the observations' )
                    GOTO 810
               END IF
               KG_RST = KG_RST + K_RST
!
               CALL PRCH ( 'Hit any key to proceed '//CHAR(1) )
!
! ------------ Awaiting for the reaction of the user
!
               CALL INSIM ( ASIM, ISIM )
          END IF
!C
          IF ( F_UPWEI  .AND.  .NOT. F_SUPSTAT ) THEN
!
! ------------ Weights update
!
               UPWEI_WAS = .TRUE.
               CALL ERR_PASS ( IUER, IER )
               CALL UPWEI ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &              OBSHLD, DBOBJ, NCREC, OBSSCA, OBSSTA, OBSBAS, RES, RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, .FALSE., &
     &              IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6304, IUER, 'PAMB_ELIM', 'Error during '// &
     &                 'updating weights' )
                    GOTO 810
               END IF
          END IF
!C
          IF ( F_SUPSTAT ) THEN
               IF ( ELIM_VRB .GE. 1 ) THEN
                    WRITE ( 6, FMT='(A)' ) 'Suppression status for '// &
     &                    'observations from '//DBNAME//' is being updated'
               END IF
!
! ------------ Update of suppression status for the changes of suppression
! ------------ method or quality code limit
!
               CALL ERR_PASS ( IUER, IER )
               F_SUP = SUP_UPDATE ( N_OBS, DBOBJ, OBSSCA, OBSBAS, &
     &                              QUALCODE_GOOD_LIM, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6305, IUER, 'PAMB_ELIM', 'Error during '// &
     &                  'update of suppression status' )
                    GOTO 810
               END IF
!
               IF ( F_SUP ) THEN
!
! ----------------- Status of observation: used or not used in solution was
! ----------------- changed.
!
! ----------------- Making initial LSQ solution. Estimates and FULL covariance
! ----------------- matrix are calculaterd. Postfit residuals and their
! ----------------- statistics are also calculated and stored in temporary
! ----------------- data structures
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL SOL_INIT ( ELIM_VRB, 1, 0, N_OBS, DBOBJ%L_STA, &
     &                              DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, &
     &                              B1B3DOBJ, OBSHLD, DBOBJ, NCREC, OBSSCA, &
     &                              OBSSTA, OBSBAS, RES, RST, CHIOBJ, EQUMEM, &
     &                              IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6306, IUER, 'PAMB_ELIM', 'Error '// &
     &                       'during the attempt to obtain initial solution '// &
     &                       'while database '//B3DOBJ%DBNAME_MES// &
     &                       ' was processing' )
                         GOTO 810
                    END IF
                    F_CHI = .TRUE.  ! Flag: chi/ndg has been calculated
                  ELSE
!
! ----------------- If we don't need to update solution then we should at least
! ----------------- update residuals
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL RESID_ST ( .FALSE., .FALSE., 0.D0, 0.D0, 0.0D0, 0, &
     &                            N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6307, IUER, 'PAMB_ELIM', 'Error '// &
     &                       'during calculation statisics for the postfit '// &
     &                       'residuals while database '//B3DOBJ%DBNAME_MES// &
     &                       ' was processing' )
                         GOTO 810
                    END IF
               END IF
          END IF
!
          F_UPWEI_INIT = .FALSE.
      END DO
!
! --- END OF CYCLE...
!
 810  CONTINUE
!
! --- Setting OBORG_UPDATE flag: do we need to save the results of our work?
!
      OBORG_UPDATE = .FALSE.
      IF ( F_SAVE   .AND.   ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) ) THEN
!
! -------- Updating the scratch file in order to save results of our work
!
           OBORG_UPDATE = .TRUE.
         ELSE IF ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) THEN
           CALL PRCH ( 'Unweight flag for some observations has been changed' )
           CALL PRCH ( CHAR(13)//CHAR(10) )
           CALL PRCH ( 'Are you really going to discard these changes '// &
     &                 ' (Y/N) [Y]  ? '//CHAR(1) )
!
! -------- Awaiting for the user's reply
!
           CALL INSIM ( ASIM, ISIM )
           CALL TRAN  ( 11, ASIM, ASIM )
           IF ( ASIM .EQ. 'N' ) THEN
                F_OPTIN = .FALSE.
                GOTO 910
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAMB_ELIM  #!#
