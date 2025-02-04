      PROGRAM    GAMB
! ************************************************************************
! *                                                                      *
! *     Program  GAMB  is the part of SOLVE software for geodetic VLBI   *
! *   data analysis. GAMB solves simultaneously the following problems:  *
! *   1) resolving group delay ambiguity for both X- and S- band         *
! *   observations; 2) finds preliminary adjustments to clock polynomial *
! *   models for clocks of all stations except one stations chosen as a  *
! *   fiducial station: clock shift, clock drift, frequency drift;       *
! *   3) eliminate outliers; 4) finds ionosphere calibration for group   *
! *   delay observables.                                                 *
! *                                                                      *
! *     It is assumed that GAMB is called either from OPTIN ( in         *
! *   interactive mode) or from BATCH (non-interactive mode).            *
! *                                                                      *
! *      Information about GAMB may be found in                          *
! *   1) SOLVE_HELP_DIR directory file gamb_01.hlp                       *
! *   2) SOLVE_HELP_DIR directory file gamb_04.hlp                       *
! *   3) SOLVE_HELP_DIR directory file gamb_03_hlp.ps                    *
! *   4) Petrov, L.Y. "Secondary data analysis of geodetic VLBI          *
! *      observations. III Estimation of model's parameters",            *
! *      Communications of the Institute of Applied Astronomy N76,       *
! *      Institute of Applied Astronomy, St.Petersburg, 47 pages.        *
! *      (In Russian).                                                   *
! *                                                                      *
! *   History:                                                           *
! *                                                                      *
! *   Who  When       What                                               *
! *   ------------------------------------------------------------------ *
! *   pet  29-JUL-97  Start of work for developing GAMB. Algorithms used *
! *                   in PREPES routine from software VORIN developed    *
! *                   in 1991-96 were taken as the basis.                *
! *                                                                      *
! *   pet  23-AUG-97  1.0  First release GAMB.                           *
! *                                                                      *
! *   pet  25-AUG-97  1.1  Added goodness codes: failure of the solution,*
! *                        "some baseline rejected before solution";     *
! *                        corrected spelling errors in messages.        *
! *                                                                      *
! *   pet  02-MAR-98  1.2  Added support of deselection lists:           *
! *                        observation made at the deselected baseline   *
! *                        or observation of the deselected source is    *
! *                        not taken into account.                       *
! *                                                                      *
! *   pet  05-MAR-98  1.3  Added new feature: GAMB will force to migrate *
! *                        9 additional parameters from S- to X-band     *
! *                        oborg area: 1) total phase for S-band; 2) SNR *
! *                        for S-band; 3) Correlation coefficient for    *
! *                        S-band; 4) Narrow band group delay for        *
! *                        S-band; 5) Original group delay observable    *
! *                        for S-band; 6) Original phase delay           *
! *                        observable for S-band; 7) Group delay         *
! *                        ambiguity spacing for S-band; 8) Phase delay  *
! *                        ambiguity spacing for S-band; 9) Group delay  *
! *                        ambiguity for S-band; 10) Phase delay         *
! *                        ambiguity for S-band.                         *
! *                                                                      *
! *   pet  06-MAR-98  1.4  Added capacity to resolve group delay         *
! *                        ambiguities, recalculate group delay          *
! *                        ionosphere calibration and etc for both bands *
! *                        while only database/superfile for the X-band  *
! *                        was read provided it contains all necessary   *
! *                        information.                                  *
! *                                                                      *
! *   pet  01-MAY-98  1.5  Changed scheme for tracing information about  *
! *                        suppression status of observations. Fixed bug:*
! *                        previous version took into account baseline   *
! *                        with unresolved ambiguities when made         *
! *                        permanent ambiguities redistribution. New     *
! *                        version bypasses such baselines. Trap of      *
! *                        internal control was added to block to write  *
! *                        in oborg area ambiguities exceeding by modulo *
! *                        32767.                                        *
! *                                                                      *
! *   pet  07-MAY-98  1.6  Changed treating not used observations:       *
! *                        a) GAMB doesn't see observation of deselected *
! *                           sources and at deselected baselines, or    *
! *                           observations no fringes detected and       *
! *                           doesn't update their status.               *
! *                        b) Not used observations (but at selected     *
! *                           baselines and sources and with fringes)    *
! *                           don't participate in calculation of clock  *
! *                           function but their ambiguity and (if it    *
! *                           was specified) ionosphere calibration are  *
! *                           calculated and written in scratch file.    *
! *                                                                      *
! *   pet  18-SEP-98  1.7  a) Disabled an attempt to lift status         *
! *                           "suppressed" for observations which GAMB   *
! *                           finds good. NO all observations which were *
! *                           marked as "suppressed by user" will remain *
! *                           "suppressed by user". But if GAMB find     *
! *                           some observations which seems it bad, GAMB *
! *                           set "suppressed by user" flag.             *
! *                        b) Made GAMB to lift forcibly ICORR bits      *
! *                           1, 5, 7, 10. These bits are actually       *
! *                           obsolete but they may badly interfere with *
! *                           s-SOLVE scheme of handling suppressed      *
! *                           observations.                              *
! *                                                                      *
! *   pet  28-APR-99  1.8  a) Disabled setting flag "ionosphere          *
! *                           calibration is available and applied" for  *
! *                           the deselected stations.                   *
! *                        b) Lifted flag availability ionosphere in     *
! *                           reading data. Thus availability of         *
! *                           ionosphere calibration status is not taken *
! *                           into account in reading data and in        *
! *                           getting suppression status.                *
! *                        c) Forced GAMB to mark observations with      *
! *                           "ambiguities found during final control    *
! *                            run of resolving ambiguities in group     *
! *                            delay ionosphere corrections" as          *
! *                            outliers. Usually these observations      *
! *                            has subambiguities.                       *
! *                                                                      *
! *   pet  1999.11.11  1.81   Extended a list of atmosphere calibrations *
! *                           to avoid false warnings.                   *
! *                                                                      *
! *   pet  1999.11.11  1.82   Added test of a tolerance range in         *
! *                           iono_amb in order to avoid an error in     *
! *                           re-distribution of permanent ambiguity in  *
! *                           ionosphere contribution.                   *
! *                                                                      *
! *   pet  1999.11.30  1.83   Corrected a bug: ambiguity jumps for the   *
! *                           baseline which has been removed during the *
! *                           process of ambiguity resolution were not   *
! *                           initialized.                               *
! *                                                                      *
! *   pet  2000.03.03  1.9    Enchanced algorithm: added the final       *
! *                           inspection of residuals in order to detect *
! *                           remaining abiguities after re-distribution *
! *                           of ambiguities between two bands. If GAMB  *
! *                           findsobservations with ambiguities in the  *
! *                           control run, it resolves them, but issues  *
! *                           a warning since such observations may      *
! *                           indication on presence of subambiguities.  *
! *                                                                      *
! *   pet  2000.03.22  1.91   Fixed a bug: the previous version set      *
! *                           ionosphere flag "undefined" if GAMB_GET    *
! *                           didn't find a matching observation at the  *
! *                           opposite band for the last observation.    *
! *                           The new version sets this flag if it       *
! *                           didn't find any matching observation.      *
! *                                                                      *
! *   pet  2000.03.22  1.92   Changed a logic of handing observatoins    *
! *                           with very large ambiguities (more than     *
! *                           32766) which cannot be stored in INTEGER*2 *
! *                           format. New logic is to set them to zero   *
! *                           and set a suppression status.              *
! *                                                                      *
! *   pet  2000.12.06  1.93   Fixed some errors initialization. The      *
! *                           previous version set garbage in the 21     *
! *                           lcodes of S-band whcih migrate to the      *
! *                           X-band database if there was no matching   *
! *                           S-band observation.                        *
! *                                                                      *
! *   pet  2001.01.05  1.94   The previous version copied atmosphere     *
! *                           parameters amd calibration from X-band     *
! *                           database to the S-band database. This      *
! *                           feature caused abnormal termination of     *
! *                           PROC and CRES ain the case if S-band       *
! *                           database didn't have atmopshere parameters *
! *                           and there are observations at S-band       *
! *                           which didn't have counterparts at X-band.  *
! *                                                                      *
! *  ###  29-JUL-97      GAMB    v1.94 (c)  L. Petrov  05-JAN-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'gamb.i'
      INCLUDE   'cals.i'
      INTEGER*8  MEM_LEN
      ADDRESS__TYPE :: MEM_ADDR, OBS_ADDR, GAMB_ADDR
      INTEGER*4  IUER, IH, HIT_CONT
      LOGICAL*2  KBIT, K_MN
      INTEGER*2   NUMDB, LDBNAM(5,15), IDBV(15)
      INTEGER*4   IDBE(15)
      CHARACTER   CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      LOGICAL*4  F_OPTIN, F_OPP_BAND, F_BATCH
      INTEGER*4  STATUS_GEN, &
     &           STATUS_X_BAS, STATUS_X_NZ, STATUS_X_CLS, STATUS_X_WHL, &
     &           STATUS_S_BAS, STATUS_S_NZ, STATUS_S_CLS, STATUS_S_WHL, &
     &           ION_MOD
      CHARACTER  VER_GAMB*21, BAND*6, STR*32, GET_VERSION*54
      TYPE ( CALS_STRU ) ::  CALX, CALS
      INTEGER*4  I_LEN
!
! --- Setting version date
!
      CALL PRE_PROG()
      INCLUDE 'gamb_version.i' ! Set revision date of the current version
      CALL SET_PATHS() ! Setting environment variables to PGPLOT and SOLVE_HELP_DIR
      K_MN = KSCREEN                  .AND. &   ! Screen mode
     &       KBIT ( PRE_IP ( 2 ), INT2(6) )  ! Interactide mode
      VER_GAMB = GET_VERSION ( )
!
! --- Openning necessary files and loading common saved area
!
      CALL GAMB_OPEN()
!
! --- Getting parameters of group delay ambiguity resaolution algorithm in
! --- interactive mode
!
      MEM_LEN = -1
 910  CONTINUE
      IF ( KBATCH ) THEN
!
! -------- Setting forcibly some falgs for batch solution
!
           F_OPTIN     = .FALSE.
           CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
           GAMB_F_X_BAND = .FALSE.
           GAMB_F_S_BAND = .FALSE.
           IF ( CDBNAM(1)(9:9) .EQ. 'X' ) GAMB_F_X_BAND = .TRUE.
           IF ( CDBNAM(1)(9:9) .EQ. 'S' ) GAMB_F_S_BAND = .TRUE.
           GAMB_F_ION    = .FALSE.
           CALL USE_GLBFIL_4 ( 'OWC' )
         ELSE
           CALL SET_SIGNAL_CTRLC ( 3 )
!
! -------- Calling menu interface to ask user to change values of some
! -------- default parameters
!
           IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT )  .AND. &
     &          KBIT ( OPP_STATUS, OPP_SET2__BIT )         ) THEN
!
                F_OPP_BAND = .TRUE.
             ELSE
                F_OPP_BAND = .FALSE.
           END IF
           CALL GAMB_MENU ( VER_GAMB, F_OPP_BAND, GAMB_F_X_BAND, GAMB_F_S_BAND, &
     &          GAMB_F_ION, GAMB_F_PREUSE, GAMB_F_SAVE, GAMB_CUTOFF, &
     &          GAMB_MINOBS, GAMB_SPACING_CONST, GAMB_IT, QUALCODE_GOOD_LIM, &
     &          F_OPTIN )
      END IF
      IF ( F_OPTIN ) CALL END_PROG  ! Good bye!
      IF ( MEM_LEN .LT. 0 ) THEN
!
! -------- Grabbing dynamic memory
!
           IUER=-1
           CALL GRAB_MEM ( IUER,  MEM_LEN,            MEM_ADDR,  2, &
     &                            INT8(OBS__SIZE),    OBS_ADDR,     &
     &                            INT8(2*GAMB__SIZE), GAMB_ADDR     )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7701, -1, 'GAMB', 'Error during grabbing '// &
     &              'memory for internal data structures for group delay '// &
     &              'ambiguity resolution algorithm' )
                STOP 'GAMB(gamb) Abnormal termination'
           END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'   MEM_LEN = ',MEM_LEN, ' MEM_ADDR = ',MEM_ADDR  ! %%%%%%%
!        type *,' OBS__SIZE = ',OBS__SIZE, ' OBS_ADDR = ',OBS_ADDR  ! %%%%%
!        type *,' GAMB__SIZE= ',GAMB__SIZE,' GAMB_ADDR =  ',GAMB_ADDR  ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      END IF
!
! --- Getting data from internal SOLVE obsorg data structure
!
      IUER = -1
      CALL GAMB_GET ( %VAL(OBS_ADDR), %VAL(GAMB_ADDR), CALX, CALS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7702, -1, 'GAMB', 'Error during getting data for '// &
     &          'group delay ambiguity resolution algorithm' )
           IF ( K_MN ) THEN
                IH = HIT_CONT ( 'Hit any key to proceed  '//CHAR(1), 1 )
                CALL END_PROG()
             ELSE
                STOP 'GAMB(gamb) Abnormal termination'
           END IF
      END IF
!
! --- Resolving group delay ambiguities
!
      IUER = -1
      IF (       KBATCH ) F_BATCH = .TRUE.
      IF ( .NOT. KBATCH ) F_BATCH = .FALSE.
      CALL PREPES ( VER_GAMB, %VAL(OBS_ADDR), F_BATCH, GAMB_F_X_BAND, &
     &              GAMB_F_S_BAND, GAMB_F_PREUSE, GAMB_F_ION, GAMB_CUTOFF, &
     &              GAMB_MINOBS, GAMB_SPACING_CONST, GAMB_IT, &
     &              QUALCODE_GOOD_LIM, CALX, CALS, %VAL(GAMB_ADDR), STATUS_GEN, &
     &              STATUS_X_BAS, STATUS_X_NZ, STATUS_X_CLS, STATUS_X_WHL, &
     &              STATUS_S_BAS, STATUS_S_NZ, STATUS_S_CLS, STATUS_S_WHL, &
     &              ION_MOD, IUER )
      IF ( IUER .EQ. 3301  .OR.  IUER .EQ. 3302  .OR.  IUER .EQ. 3303 ) THEN
!
! -------- User took decision to refuse from ambiguity resolution and
! -------- to go to GAMB menu once more. It must be a wise decision.
!
           CALL FREE_MEM ( MEM_ADDR )
           MEM_LEN = -1
           GOTO 910
      END IF
!
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7703, -1, 'GAMB', 'Group delay ambiguities '// &
     &         'have not been resolved due to some reasons.' )
           IF ( K_MN ) THEN
                IH = HIT_CONT ( 'Hit any key to proceed  '//CHAR(1), 1 )
                CALL END_PROG()
             ELSE
                STOP 'GAMB(gamb) Abnormal termination'
           END IF
      END IF
      IF ( STATUS_GEN .EQ. GAMB__STS_OK ) THEN
!
! -------- Solution was OK. And thanks God.
!
           IF ( GAMB_F_SAVE ) THEN
!
! ------------- Saving results in oborg area of scratch file
!
                IUER = -1
                CALL GAMB_PUT ( VER_GAMB, %VAL(OBS_ADDR), %VAL(GAMB_ADDR), &
     &                          IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7704, -1, 'GAMB', 'Error during putting '// &
     &                   'the results in scratch file' )
                     IF ( K_MN ) THEN
                          IH = HIT_CONT ( 'Hit any key to proceed  '//CHAR(1),1)
                          CALL END_PROG()
                       ELSE
                          STOP 'GAMB(gamb) Abnormal termination'
                     END IF
                END IF
           END IF
!
           WRITE ( 23, FMT='(1X,A)' ) 'GAMB  Group delay ambiguities '// &
     &            'resolution finshed. Results look OK.'
           IF ( .NOT. KBATCH ) THEN
                IH = HIT_CONT ( ' ---  GAMB finished its work. OK. '// &
     &               'Hit any key to get back to OPTIN  ---  '//CHAR(1), &
     &               %VAL(0) )
           END IF
        ELSE
!
! -------- Alas! Some problems were detected during group delay ambiguities
! -------- resolution
!
! -------- Printing at screen and at spool file descriptive message about
! -------- problems in solution.
!
           IF ( GAMB_F_X_BAND ) THEN
              BAND = 'X-band'
!
! ----------- Look at baseline rejection criteria
!
              IF ( STATUS_X_BAS .EQ. GAMB__BAS_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': number of '// &
     &                 'baselines is not changed' )
                ELSE IF ( STATUS_X_BAS .EQ. GAMB__BAS_SUS ) THEN
                   CALL PRCH_23 ( GAMB_IT, 0, ' GAMB  $$$ '//BAND//': some '// &
     &                 'baselines were rejected before solution $$$' )
                ELSE IF ( STATUS_X_BAS .EQ. GAMB__BAS_BAD ) THEN
                   CALL PRCH_23 ( GAMB_IT, 0, ' GAMB  $$$ '//BAND//': number '// &
     &                 'of baselines is changed $$$' )
              END IF
!
! ----------- Look at outlier detection rate criteria
!
              IF ( STATUS_X_NZ .EQ. GAMB__NZ_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': rejected '// &
     &                           'observations rate is OK' )
                ELSE IF ( STATUS_X_NZ .EQ. GAMB__NZ_SUS ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': many '// &
     &                 'observations were rejected: it looks suspicious $$$' )
                ELSE IF ( STATUS_X_NZ .EQ. GAMB__NZ_BAD ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': too many '// &
     &                 'observations were rejected: it looks bad $$$' )
              END IF
!
! ----------- Look at triangle closure residuals criteria
!
              IF ( STATUS_X_CLS .EQ. GAMB__CLS_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': triangles '// &
     &                           'closure residuals are OK' )
                ELSE IF ( STATUS_X_CLS .EQ. GAMB__CLS_SUS ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': triangles '// &
     &                 'closure residuals are large: it looks suspicious $' )
              END IF
!
! ----------- Look at RMS of whole solution criteria
!
              IF ( STATUS_X_WHL .EQ. GAMB__WHL_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': r.m.s. '// &
     &                           'of whole solution is OK' )
                ELSE IF ( STATUS_X_WHL .EQ. GAMB__WHL_SUS ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': r.m.s. '// &
     &                 'of whole solution is high: it looks suspicious $$$' )
                ELSE IF ( STATUS_X_WHL .EQ. GAMB__WHL_BAD ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': r.m.s. '// &
     &                 'of whole solution is too large: it looks bad $$$' )
                ELSE IF ( STATUS_X_WHL .EQ. GAMB__WHL_FAI ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': group '// &
     &                 'delay ambiguities have not been resolved. Sorry. $$$' )
              END IF
!
              IF ( ION_MOD .GT. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( ION_MOD, STR )
                   CALL PRCH_23 ( 0, 0, ' GAMB  $ '//STR(1:I_LEN(STR))// &
     &                 ' obs. with ambiguities found in control run: '// &
     &                 'it looks suspicious $' )
              END IF
           END IF
!C
           IF ( GAMB_F_S_BAND ) THEN
!
! ----------- The same business for S-band if it was in solution
!
              BAND = 'S-band'
!
! ----------- Look at baseline rejection criteria
!
              IF ( STATUS_S_BAS .EQ. GAMB__BAS_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': number of '// &
     &                 'baselines is not changed' )
                ELSE IF ( STATUS_S_BAS .EQ. GAMB__BAS_SUS ) THEN
                   CALL PRCH_23 ( GAMB_IT, 0, ' GAMB  $$$ '//BAND//': some '// &
     &                 'baselines were rejected before solution $$$' )
                ELSE IF ( STATUS_S_BAS .EQ. GAMB__BAS_BAD ) THEN
                   CALL PRCH_23 ( GAMB_IT, 0, ' GAMB  $$$ '//BAND//': number '// &
     &                 'of baselines is changed $$$' )
              END IF
!
! ----------- Look at outlier detection rate criteria
!
              IF ( STATUS_S_NZ .EQ. GAMB__NZ_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': rejected '// &
     &                           'observations rate is OK' )
                ELSE IF ( STATUS_S_NZ .EQ. GAMB__NZ_SUS ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': many '// &
     &                 'observations were rejected: it looks suspicious $$$' )
                ELSE IF ( STATUS_S_NZ .EQ. GAMB__NZ_BAD ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': too many '// &
     &                 'observations were rejected: it looks bad $$$' )
              END IF
!
! ----------- Look at RMS of whole solution criteria
!
              IF ( STATUS_S_WHL .EQ. GAMB__WHL_OK ) THEN
                   CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  '//BAND//': r.m.s. '// &
     &                           'of whole solution is OK' )
                ELSE IF ( STATUS_S_WHL .EQ. GAMB__WHL_SUS ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': r.m.s. '// &
     &                 'of whole solution is high: it looks suspicious $$$' )
                ELSE IF ( STATUS_S_WHL .EQ. GAMB__WHL_BAD ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': r.m.s. '// &
     &                 'of whole solution is too large: it looks bad $$$' )
                ELSE IF ( STATUS_S_WHL .EQ. GAMB__WHL_FAI ) THEN
                   CALL PRCH_23 ( 0, 0, ' GAMB  $$$ '//BAND//': group '// &
     &                 'delay ambiguities have not been resolved. Sorry. $$$' )
              END IF
           END IF
!CCCC
           CALL NEG()
           IF ( STATUS_GEN .EQ. GAMB__STS_SUS  ) THEN
                CALL PRCH_23 ( 0, 0, ' GAMB  $s$ group ambiguities are '// &
     &              'resolved, but solution seems suspicious '//CHAR(1) )
              ELSE IF ( STATUS_GEN .EQ. GAMB__STS_BAD ) THEN
                CALL PRCH_23 ( 0, 0, ' GAMB  $b$ group ambiguities are '// &
     &              'resolved, but solution looks bad '//CHAR(1) )
              ELSE IF ( STATUS_GEN .EQ. GAMB__STS_FAI ) THEN
                CALL PRCH_23 ( 0, 0, ' GAMB  $f$ group ambiguities are '// &
     &              'not resolved '//CHAR(1) )
           END IF
           CALL UN_NEG()
!CCCC
           IF ( GAMB_F_SAVE ) THEN
!
! ----------- Let's save solution in scratch file
!
              IF ( KBATCH ) THEN
                 IH = ICHAR('S')
                ELSE
                 CALL PRCH ( 'Are you sure, that you really need to save '// &
     &                       'these results in scratch file ?' )
 920             CONTINUE
                 CALL PRCH ( CHAR(13)//CHAR(10) )
                 CALL PRCH ( 'Enter (O) -- go back to OPTIN without '// &
     &                       'saving results, ' )
                 CALL PRCH ( CHAR(13)//CHAR(10) )
                 CALL PRCH ( '      (A) -- go back to GAMB  without '// &
     &                       'saving results, or ' )
                 CALL PRCH ( CHAR(13)//CHAR(10) )
                 IH = HIT_CONT ( '      (S) -- save results and then '// &
     &                           'goto to OPTIN          '//CHAR(1), %VAL(0) )
                 IF ( IH .EQ. ICHAR('S') .OR. IH .EQ. ICHAR('s') ) THEN
!
! ------------------- Saving results in oborg area of scratch file
!
                      IUER = -1
                      CALL GAMB_PUT ( VER_GAMB, %VAL(OBS_ADDR), &
     &                                %VAL(GAMB_ADDR), IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7705, -1, 'GAMB', 'Error during '// &
     &                         'putting the results in scratch file' )
                           IF ( K_MN ) THEN
                                IH = HIT_CONT ( 'Hit any key to proceed  '// &
     &                                           CHAR(1), 1 )
                                CALL END_PROG()
                             ELSE
                                STOP 'GAMB(gamb) Abnormal termination'
                           END IF
                      END IF
                      GOTO 810
                 END IF
                 IF ( IH .EQ. ICHAR('O') .OR. IH .EQ. ICHAR('o') ) GOTO 810
                 IF ( IH .EQ. ICHAR('A') .OR. IH .EQ. ICHAR('a') ) GOTO 910
                 CALL CURU ( 3 )
                 GOTO 920
              END IF
            ELSE
!
! ------------- It was not necessary to save solution
!
                IF ( KBATCH ) THEN
                     GOTO 810
                   ELSE
 930                 CONTINUE
                     CALL PRCH ( CHAR(13)//CHAR(10) )
                     IH = HIT_CONT ( ' GAMB finished its work. Hit (A) to go '// &
     &                               'back to GAMB, or (O) to go to OPTIN  '// &
     &                                CHAR(1), 1 )
                     IF ( IH .EQ. ICHAR('O') .OR. IH .EQ. ICHAR('o') ) GOTO 810
                     IF ( IH .EQ. ICHAR('A') .OR. IH .EQ. ICHAR('a') ) GOTO 910
                     CALL CURU ( 1 )
                     GOTO 930
               END IF
           END IF
      END IF
 810  CONTINUE
!
! --- Close some files
!
      CALL GAMB_CLOSE()
!
! --- ... And freeing dynamic memory
!
      CALL FREE_MEM ( MEM_ADDR )
      MEM_LEN = -1
!
! --- Good bye!
!
      CALL END_PROG()
      END  !#!  GAMB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAMB_OPEN ()
! ************************************************************************
! *                                                                      *
! *   Auxilary routine GAMB_OPEN  opens some files and load content of   *
! *   some include blocks which will be used by GAMB.                    *
! *                                                                      *
! *  ###  29-JUL-97    GAMB_OPEN   v1.0  (c)  L. Petrov  01-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
! --- Get info from PARFIL
!
      CALL USE_PARFIL('ORC')
!
! --- Loading socom.i
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Open the NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Open spool file and rewind it to the end
!
      CALL USE_SPOOL ( 'O' )
!
! --- Get info from OBSFIL
!
      CALL ACS_OBSFIL ( 'O' )
!
! --- Get info from GLBFIL
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Get flyby a prioris and modify PARFL common accordingly
!
      CALL FLYBY_APRIOR()
      RETURN
      END  !#!  GAMB_OPEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAMB_CLOSE ()
! ************************************************************************
! *                                                                      *
! *   Auxilary routine GAMB_OPEN  closes some files which used by GAMB.  *
! *                                                                      *
! *  ###  08-AUG-97    GAMB_CLOSE   v1.1 (c)  L. Petrov  20-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
! --- Close spool file and rewind it to the end
!
      CALL USE_SPOOL  ( 'C' )
      CALL USE_COMMON ( 'OWC' )
      CALL ACS_OBSFIL ( 'C' )
      RETURN
      END  !#!  GAMB_CLOSE  #!#
