      SUBROUTINE SCADAM_MAIN ( PAMB_VER, DBOBJ, OBSSCA, OBSBAS, PAMBI, SCAINF, &
     &                         ICOND, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SCADAM_MAIN  calls menu for gathering parameters for      *
! *   SCADAM algorithm and then resolves phase delay ambiguities using   *
! *   this algorithm. It changes baseline-dependent phase delay          *
! *   ambiguity at both bands and suppression status of phase delay      *
! *   observables.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity level.                          *
! *                            0  -- silent mode;                        *
! *                            1  -- usual mode;                         *
! *                            >2 -- debugging mode;                     *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM. Values  of ARF will be written in   *
! *                          this data structure.                        *
! *   ICOND ( INTEGER*4 ) -- Completion code.                            *
! *                          ICOND=0 means that user refused to run      *
! *                                  SCADAM algrotihm.                   *
! *                          ICOND=1 means that the routine implementing *
! *                                  SCADAM algorithm ran up to the end; *
! *                          ICOND=-1 means that error occurred in       *
! *                                   resolving phase delay ambiguities; *
! *    KAMB ( INTEGER*4 ) -- Total number of baseline-dependent          *
! *                          ambiguities at X-band and S-band which were *
! *                          changed.                                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-OCT-98   SCADAM_MAIN   v1.0  (c) L. Petrov  03-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  PAMB_VER, ICOND, KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( PAMBI__STRU ) ::  PAMBI(DBOBJ%L_OBS)
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  IER
!
      KAMB = 0
!
! --- Getting information about SCADAM parameters in the menu interface
!
      CALL ERR_PASS ( IUER, IER )
      CALL SCADAM_MENU ( DBOBJ, SCAINF, ICOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5321, IUER, 'SCADAM_MAIN', 'Error in attempt to '// &
     &         'get information for the the SCADAM menu' )
           RETURN
      END IF
      IF ( ICOND .EQ. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Call the routine which actually im[plement SCADAM algorithm
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SCADAM_DO ( PAMB_VER, DBOBJ, OBSSCA, OBSBAS, PAMBI, SCAINF, KAMB, &
     &                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5322, IUER, 'SCADAM_MAIN', 'Error in SCADAM_DO was '// &
     &         'deteceted' )
           ICOND = -1
           RETURN
      END IF
!
      ICOND = 1
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SCADAM_MAIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SCADAM_DO ( PAMB_VER, DBOBJ, OBSSCA, OBSBAS, PAMBI, SCAINF, &
     &                       KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Reotune  SCADAM_DO  resolves phase delay ambiguities at both X and *
! *   S bands using SCADAM algoritm. It changes baseline-dependent phase *
! *   delay ambiguities at both bands and suppression status.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PAMB_VER ( INTEGER*4 ) -- Verbosity level.                          *
! *                            0  -- silent mode;                        *
! *                            1  -- usual mode;                         *
! *                            >2 -- debugging mode;                     *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM. Values of ARF will be written in    *
! *                          this data structure.                        *
! *    KAMB ( INTEGER*4 ) -- Total number of baseline-depenedent         *
! *                          ambiguities at X-band and S-band which were *
! *                          changed.                                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  23-OCT-98    SCADAM_DO   v1.3  (c)  L. Petrov  08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INCLUDE   'lspline.i'
      INTEGER*4  PAMB_VER, KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU   ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU   ) ::  OBSSCA(*)
      TYPE ( PAMBI__STRU   ) ::  PAMBI(DBOBJ%L_OBS)
      TYPE ( SCAINF__STRU  ) ::  SCAINF
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S(2,MO_STA)
!
      INTEGER*4  LIS_OBS(MO_BAS), L_OBS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, ICOND, ISTA1, ISTA2, IPL_STA1, IPL_STA2, IPL_BAS, &
     &           ISCA__COU, IER
      PARAMETER  ( ISCA__COU = 25 )
      REAL*8     RAT
      INTEGER*2  ISCA, ISCA_OLD
      CHARACTER  STR*32
      LOGICAL*4  FL_USED, FL_URPH 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: FSTREAM, IFIND_PL, NSTBA, I_LEN
!
! --- Check solutiuon type
!
      IF ( DBOBJ%IDATYP .NE. G_GXS__DTP  .AND. &
     &     DBOBJ%IDATYP .NE. PX_GS__DTP         ) THEN
!
           CALL DATYP_SHOW ( DBOBJ%IDATYP, STR )
           CALL ERR_LOG ( 5261, IUER, 'SCADAM_DO', 'Attempt to use '// &
     &         'SCADAM algorithm while solution type "'//STR(1:I_LEN(STR))// &
     &         '" is set up. Please, change slution type to G-Gxs or Px-Gs '// &
     &         'combination' )
           RETURN
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( MG_STA*MG_SCA, SCAINF%XPA_SCA(1,1) )
      CALL NOUT_R8 ( MG_STA*MG_SCA, SCAINF%SPA_SCA(1,1) )
      CALL NOUT_R8 ( MG_STA*MG_SCA, SCAINF%TAU_SCA(1,1) )
      CALL NOUT_R8 ( MG_STA*MG_SCA, SCAINF%ION_SCA(1,1) )
!
      DO 410 J1=1,MG_STA
         DO 420 J2=1,MG_SCA
            SCAINF%USE_SCA(J1,J2) = .FALSE.
 420     CONTINUE
!
         SCAINF%SCA_TOT(J1) = 0
 410  CONTINUE
      ISCA_OLD = INT2(0)
      L_OBS    = 0
      KAMB     = 0
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, * ) 'Algorithm for getting ARF values '// &
     &                     'is running'
      END IF
!
! --- First, look at all obsrvations and gather observations at groups.
! --- Each group contains observations at one scan which are made at
! --- the selected stations only
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE  (  6, 110 )
 110       FORMAT ( ' SCADAM_DO:  scan ',$ )
           WRITE  (  6, 120 ) 0, DBOBJ%L_SCA
 120       FORMAT ( I4,'(',I4,')  '$ )
      END IF
!
      DO 430 J3=1,DBOBJ%L_OBS
         SCAINF%UPD_OBS(1,J3) = .FALSE.
         SCAINF%UPD_OBS(2,J3) = .FALSE.
         ISCA = OBSBAS(J3)%IND_SCA
         IF ( PAMB_VER .GE. 1  .AND. &
     &      ( MOD(INT4(ISCA),ISCA__COU) .EQ. 0  .OR. &
     &        ISCA .EQ. DBOBJ%L_SCA )                ) THEN
!
              CALL CURL   ( 12 )
              WRITE       (  6, 120 ) ISCA, DBOBJ%L_SCA
         END IF
!
! ------ TIM_SCA -- approximate moment of the observation of the ISCA-th scan.
! ------            We neglect here differences in epoch at the different
! ------            stations
!
         SCAINF%TIM_SCA(ISCA)=(OBSSCA(ISCA)%FJD - OBSSCA(1)%FJD )*86400.D0 + &
     &                        (OBSSCA(ISCA)%FRACTC - OBSSCA(1)%FRACTC )*86400.D0
         IF ( OBSBAS(J3)%IND_SCA .NE. ISCA_OLD  .OR. J3 .EQ. DBOBJ%L_OBS ) THEN
!
! ----------- New scan
!
              IF ( L_OBS .GE. 3 ) THEN
!
! ---------------- Calling GET_ARF to handle a set of recoverable
! ---------------- observations which belong to the ISCA_OLD-th scan
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL GET_ARF  ( L_OBS, LIS_OBS, INT4(ISCA_OLD), DBOBJ, &
     &                             OBSBAS, OBSSCA, PAMBI, SCAINF, ICOND, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( INT4(ISCA_OLD), STR )
                        CALL ERR_LOG ( 5262, IUER, 'SCADAM_DO', 'Error in '// &
     &                      'resolving phase delay ambiguities for the '// &
     &                       STR(1:I_LEN(STR))//'-th scan' )
                        RETURN
                   END IF
              END IF
!
! ----------- Now we look at the list of observation of the scan and determine
! ----------- did the J4-th station participated in the observations of the
! ----------- scan. If yes we increment the counter of scans for that station.
! ----------- Special care is taken to avoid double counting
!
              DO 440 J4=1,DBOBJ%L_STA
                 DO 450 J5=1,L_OBS
                    ISTA1 = INT4( OBSBAS(LIS_OBS(J5))%ISITE(1) )
                    ISTA2 = INT4( OBSBAS(LIS_OBS(J5))%ISITE(2) )
                    IF ( ISTA1 .EQ. J4  .OR.  ISTA2 .EQ. J4 ) THEN
                         SCAINF%SCA_TOT(J4) = SCAINF%SCA_TOT(J4) + 1
                         GOTO 440
                    END IF
 450          CONTINUE
 440          CONTINUE
!
              ISCA_OLD = OBSBAS(J3)%IND_SCA
              L_OBS = 0
         END IF
!
! ------ Filtering section
!
! ------ First, we filter out bad observations:
! ------ a) not used for group delay solutions
! ------ b) unrecoverable for phase delay solution
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J3)%AUTO_SUP, &
     &                                  OBSBAS(J3)%USER_SUP, &
     &                                  OBSBAS(J3)%USER_REC, &
     &                                  USED__SPS )
              FL_URPH = BTEST ( OBSBAS(J3)%AUTO_SUP, INT4(WPAS__SPS) )
            ELSE
              FL_USED = SUPR_INQ ( OBSBAS(J3)%SUPSTAT(1), OBSBAS(J3)%UACSUP, &
     &                             USED__SPS )
              FL_URPH = SUPR_INQ ( OBSBAS(J3)%SUPSTAT(1), OBSBAS(J3)%UACSUP, &
     &                             URPH__SPS ) 
         END IF
         IF ( .NOT. FL_USED ) GOTO 430
         IF (       FL_URPH ) GOTO 430
!
! ------ Then we filter out observations made at the stations which were
! ------ deselected in processing by SCADAM algorithm
!
         ISTA1 = INT4( OBSBAS(J3)%ISITE(1) )
         ISTA2 = INT4( OBSBAS(J3)%ISITE(2) )
         IPL_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
         IPL_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
         IF (  IPL_STA1 .LE. 0             ) GOTO 430
         IF (  IPL_STA2 .LE. 0             ) GOTO 430
         IF ( .NOT. SCAINF%P_STA(IPL_STA1) ) GOTO 430
         IF ( .NOT. SCAINF%P_STA(IPL_STA2) ) GOTO 430
!
! ------ Add the J3-th observation to the list of observations of the scan
!
         L_OBS          = L_OBS + 1
         LIS_OBS(L_OBS) = J3
!
! ------ Set (temporarily) the status "ambiguity at X- and S- band are resolved
!
         CALL SBIT ( OBSBAS(J3)%SUPSTAT(1), XAMB__SPS, INT2(0) )
         CALL SBIT ( OBSBAS(J3)%SUPSTAT(1), SAMB__SPS, INT2(0) )
         OBSBAS(J3)%AUTO_SUP = IBCLR ( OBSBAS(J3)%AUTO_SUP, INT4(XAMB__SPS) )
         OBSBAS(J3)%AUTO_SUP = IBCLR ( OBSBAS(J3)%AUTO_SUP, INT4(SAMB__SPS) )
 430  CONTINUE
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, * ) ' '
           WRITE (  6, * ) 'Algorithm for getting ARF values '// &
     &                     'finished'
      END IF
      IF ( SCAINF%PLOT_INI .EQ. 1 ) THEN
!
! -------- Plotting ARF values before filtering
!
           CALL ERR_PASS    ( IUER, IER )
           CALL SCADAM_PLOT ( 1, DBOBJ, OBSSCA, SCAINF, LSPLINE_S, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5263, IUER, 'SCADAM_DO', 'Error in attempt to '// &
     &              'plot ARF values before filtering' )
                RETURN
           END IF
      END IF
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, * ) 'Algorithm for filtering ARF values '// &
     &                     'is runing'
           WRITE ( 23, * ) 'Algorithm for filtering ARF values '// &
     &                     'is runing'
      END IF
!
! --- Filtering ARF values
!
      CALL ERR_PASS   ( IUER, IER )
      CALL ARF_FILTER ( PAMB_VER, DBOBJ, OBSSCA, OBSBAS, PAMBI, SCAINF, KAMB, &
     &                  LSPLINE_S, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5264, IUER, 'SCADAM_DO', 'Error in filtering '// &
     &                   'ARF values' )
           RETURN
      END IF
!
! --- Generating statistics and putting it on the screen and in spool file
!
      DO 460 J6=1,2  ! cycle over the bands
         IF ( ( J6 .EQ. PAMB__XBAND  .AND. &
     &          ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &            SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR  .OR. &
     &            SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS       ) ) .OR. &
     &          ( J6 .EQ. PAMB__SBAND  .AND. &
     &            ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM  .OR. &
     &              SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR  .OR. &
     &              SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS       ) )     ) THEN
!
             IF ( PAMB_VER .GE. 1 ) THEN
                  WRITE  (  6, 130 ) BAND_STR(J6)
                  WRITE  ( 23, 130 ) BAND_STR(J6)
 130              FORMAT ( 1X/ &
     &                     1X,'SCADAM  Statistics of resolving phase delay ', &
     &                        'ambiguities at ',A/ &
     &                     1X,65('~') )
             END IF
!
! ---------- Station's statistics
!
             DO 470 J7=1,DBOBJ%L_STA
                IF ( SCAINF%IUSE(J6,J7) .NE. 0 ) THEN
                     RAT = DFLOAT(SCAINF%ISUC(J6,J7))/DFLOAT(SCAINF%IUSE(J6,J7))
                   ELSE
                     RAT = 0.0
                END IF
!
                IF ( PAMB_VER .GE. 1 ) THEN
                   IF ( SCAINF%SCA_TOT(J7) .EQ. 0     ) THEN
                         WRITE (  6, 140 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7)
                         WRITE ( 23, 140 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7)
 140                     FORMAT ( 1X,I2,') ',A,' tot: ',I4,' used: ',I4, &
     &                            ' suc: ',I4 )
                      ELSE IF ( J7 .EQ. SCAINF%FID_STA ) THEN
!
                         WRITE (  6, 150 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7), RAT*100.0
                         WRITE ( 23, 150 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7), RAT*100.0
 150                     FORMAT ( 1X,I2,') ',A,' tot: ',I4,' used: ',I4, &
     &                            ' suc: ',I4,' rate: ',F6.2,'%  ' )
                      ELSE
                         WRITE (  6, 160 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7), RAT*100.0, SCAINF%WRMS(J6,J7)
                         WRITE ( 23, 160 ) J7, DBOBJ%C_STA(J7), &
     &                      SCAINF%SCA_TOT(J7), SCAINF%IUSE(J6,J7), &
     &                      SCAINF%ISUC(J6,J7), RAT*100.0, SCAINF%WRMS(J6,J7)
 160                     FORMAT ( 1X,I2,') ',A,' tot: ',I4,' used: ',I4, &
     &                            ' suc: ',I4,' rate: ',F6.2,'%  wrms: ',F5.3 )
                    END IF
                 END IF
 470         CONTINUE
!
! ---------- Initialization of baseline dependent statistics
!
             DO 480 J8=1,DBOBJ%L_BAS
                SCAINF%BAS_TOT(J8)   = 0
                SCAINF%BAS_USE(J6,J8) = 0
                SCAINF%BAS_SUC(J6,J8) = 0
 480         CONTINUE
!
! ---------- Gathering baseline statistics
!
             DO 490 J9=1,DBOBJ%L_OBS
                ISCA  = OBSBAS(J9)%IND_SCA
                ISTA1 = INT4( OBSBAS(J9)%ISITE(1) )
                ISTA2 = INT4( OBSBAS(J9)%ISITE(2) )
                IPL_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
                IPL_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
                IPL_BAS  = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                                NSTBA ( ISTA1, ISTA2 )            )
!
! ------------- Discard bad observations from statistics
!
                IF ( SUPMET == SUPMET__META ) THEN
                     FL_USED = META_SUPR_INQ ( OBSBAS(J9)%AUTO_SUP, &
     &                                         OBSBAS(J9)%USER_SUP, &
     &                                         OBSBAS(J9)%USER_REC, &
     &                                         USED__SPS )
                     FL_URPH = BTEST ( OBSBAS(J9)%AUTO_SUP, INT4(WPAS__SPS) )
                   ELSE
                     FL_USED = SUPR_INQ ( OBSBAS(J9)%SUPSTAT(1), &
     &                                    OBSBAS(J9)%UACSUP, USED__SPS )
                     FL_URPH = SUPR_INQ ( OBSBAS(J9)%SUPSTAT(1), &
     &                                    OBSBAS(J9)%UACSUP, URPH__SPS ) 
                END IF
                IF ( .NOT. FL_USED                ) GOTO 490
                IF (       FL_URPH                ) GOTO 490
                IF (  IPL_STA1 .LE. 0             ) GOTO 490
                IF (  IPL_STA2 .LE. 0             ) GOTO 490
                IF ( .NOT. SCAINF%P_STA(IPL_STA1) ) GOTO 490
                IF ( .NOT. SCAINF%P_STA(IPL_STA2) ) GOTO 490
!
! ------------- Updated counters of total number of observations,
! ------------- number of used observations, number of successfull observations
!
                SCAINF%BAS_TOT(IPL_BAS) = SCAINF%BAS_TOT(IPL_BAS) + 1
!
                IF ( SCAINF%USE_SCA(IPL_STA1,ISCA) .AND. &
     &               SCAINF%USE_SCA(IPL_STA2,ISCA)       ) THEN
                     SCAINF%BAS_USE(J6,IPL_BAS) = SCAINF%BAS_USE(J6,IPL_BAS) + 1
                END IF
!
                IF ( SCAINF%SUC_SCA(J6,IPL_STA1,ISCA) .AND. &
     &               SCAINF%SUC_SCA(J6,IPL_STA2,ISCA)       ) THEN
                     SCAINF%BAS_SUC(J6,IPL_BAS) = SCAINF%BAS_SUC(J6,IPL_BAS) + 1
                END IF
 490         CONTINUE
!
! ---------- Printing baseline statistics
!
             IF ( PAMB_VER .GE. 1 ) THEN
                  WRITE  (  6, FMT='(1X,65("-"))' )
                  WRITE  ( 23, FMT='(1X,65("-"))' )
                  DO 4100 J10=1,DBOBJ%L_BAS
                     IF ( SCAINF%BAS_TOT(J10) .NE. 0 ) THEN
                          RAT = DFLOAT(SCAINF%BAS_SUC(J6,J10))/ &
     &                          DFLOAT(SCAINF%BAS_TOT(J10))
                        ELSE
                          RAT = 0.0
                     END IF
!
                     WRITE (  6, 170 ) J10, DBOBJ%C_BAS(J10), &
     &                      SCAINF%BAS_TOT(J10), SCAINF%BAS_USE(J6,J10), &
     &                      SCAINF%BAS_SUC(J6,J10), RAT*100.0
                     WRITE ( 23, 170 ) J10, DBOBJ%C_BAS(J10), &
     &                      SCAINF%BAS_TOT(J10), SCAINF%BAS_USE(J6,J10), &
     &                      SCAINF%BAS_SUC(J6,J10), RAT*100.0
 170                        FORMAT ( 1X,I2,') ',A,' tot: ',I4,' used: ',I4, &
     &                               ' suc: ',I4,' res: ',F6.2,'%  ' )
 4100             CONTINUE
                  WRITE  (  6, FMT='(1X,65("="))' )
                  WRITE  ( 23, FMT='(1X,65("="))' )
             END IF
         END IF
 460  CONTINUE
      IF ( SCAINF%PLOT_FIN .EQ. 1 ) THEN
!
! -------- Plotting ARF values after filtering
!
           CALL ERR_PASS    ( IUER, IER )
           CALL SCADAM_PLOT ( 2, DBOBJ, OBSSCA, SCAINF, LSPLINE_S, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5265, IUER, 'SCADAM_DO', 'Error in attempt to '// &
     &              'plot ARF values after filetering' )
                RETURN
           END IF
      END IF
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, * ) 'Algorithm for filterung ARF values '// &
     &                     'finished'
           WRITE ( 23, * ) 'Algorithm for filterung ARF values '// &
     &                     'finished'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SCADAM_DO  #!#
