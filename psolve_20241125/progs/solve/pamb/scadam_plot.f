      SUBROUTINE SCADAM_PLOT ( IPAR, DBOBJ, OBSSCA, SCAINF, LSPLINE_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SCADAM_PLOT  makes plots of ARF values -- station         *
! *   dependent ionosphere free differences: phase delay minus group     *
! *   delay at both X and S bands for the stations specified by user.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IPAR ( INTEGER*4 ) -- Mode switcher.                              *
! *                          IPAR=1 -- ARF values before filtering will  *
! *                                    be plotted at the screen in       *
! *                                    interactive mode.                 *
! *                          IPAR=2 -- ARF values after filtering will   *
! *                                    be plotted at the screen in       *
! *                                    interactive mode.                 *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  OBSSCA ( RECORD    ) -- Array of data structures which keeps        *
! *                          scan-dependent information about the        *
! *                          session.                                    *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM.                                     *
! * LSPLINE_S ( RECORD    ) -- Array of data structures for computation  *
! *                            of linear spline.                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  26-OCT-98   SCADAM_PLOT  v1.3  (c)  L. Petrov  21-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INCLUDE   'diagi_local.i'
      INCLUDE   'diagi.i'
      INCLUDE   'lspline.i'
      INTEGER*4  IPAR, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( SCAINF__STRU ) ::  SCAINF
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S(2,MO_STA)
      LOGICAL*4  LSEL_STA(MO_STA), LSUP_STA(MO_STA), FSEL, SELSTA
      CHARACTER  STR*20, MES*80
      REAL*8     T1(MO_SCA), X1(MO_SCA), E1(MO_SCA)
      REAL*8     T2(MO_SCA), X2(MO_SCA), E2(MO_SCA)
      REAL*8     T3(M_LSP),  X3(M_LSP),  E3(M_LSP)
      REAL*8     ARG_UP(2),  VAL_UP(2),  ARG_DOWN(2), VAL_DOWN(2)
      INTEGER*4  ISTA, ISTA_LAST, IER, K1, K2, K3, J1, J2, J3, J4, DIA_LEN
      INTEGER*4  I_LEN
!
! --- Initialiszation.
! --- We set "station has been already seleceted" flag as false.
! --- We set "station suppressed from menu" flag in according its status in
! --- SCADAM algorithm
!
      DO 410 J1=1,DBOBJ%L_STA
         LSEL_STA(J1) = .FALSE.
         LSUP_STA(J1) = .NOT. SCAINF%P_STA(J1)
         IF ( J1 .EQ. SCAINF%FID_STA ) LSUP_STA(J1) = .FALSE.
 410  CONTINUE
      ISTA_LAST = 0
!C
 910  CONTINUE
         CALL CLRCH ( MES )
         IF ( IPAR .EQ. 1 ) THEN
!
! ----------- Type of plot: ARF values before filtering
!
              MES = 'ARF values before filtering'
            ELSE IF ( IPAR .EQ. 2 ) THEN
              MES = 'ARF values after filtering'
            ELSE
              CALL CLRCH   ( STR )
              CALL INCH    ( IPAR, STR )
              CALL ERR_LOG ( 5381, IUER, 'SCADAM_PLOT', 'Wrong value of '// &
     &                      'parameter IPAR: '//STR )
              RETURN
         END IF
!
! ------ Selection a station to be plotted
!
         CALL ERR_PASS ( IUER, IER )
         FSEL = SELSTA ( 1, MES, DBOBJ, LSUP_STA, LSEL_STA, ISTA, ISTA_LAST, &
     &                   SCAINF%FID_STA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5382, IUER, 'SCADAM_PLOT', 'Error in '// &
     &            'selection the station for ARF plot' )
              RETURN
         END IF
!
         IF ( .NOT. FSEL            .OR. &
     &        ISTA .LE. 0           .OR. &
     &        ISTA .GT. DBOBJ%L_STA      ) THEN
!
! ----------- No station has been selected -- end of work
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
         END IF
!
! ------ Gathering ARF values from SCAINF data structure for further plotting
!
         DO 420 J2=1,2 ! Cycle over bands: first X-band, then S-band
            K1 = 0
            K2 = 0
            IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS  .AND. &
     &           J2 .EQ. PAMB__SBAND                        ) GOTO 420
            IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS  .AND. &
     &           J2 .EQ. PAMB__XBAND                        ) GOTO 420
!
            DO 430 J3=1,DBOBJ%L_SCA ! Scan over all scans
               IF ( ( IPAR .EQ. 1  .AND.  SCAINF%USE_SCA(ISTA,J3)    ) .OR. &
     &              ( IPAR .EQ. 2  .AND.  SCAINF%SUC_SCA(J2,ISTA,J3) )    ) THEN
!
! ----------------- Well, new ARF values for the ISTA-th station and J3-th scan
! ----------------- has been calculated.
!
                    K1 = K1 + 1 ! Increment of point counter
                    T1(K1) = SCAINF%TIM_SCA(J3)/3600. ! Time is in hours!
                    IF ( J2 .EQ. PAMB__XBAND ) THEN
!
! ---------------------- X-band ARF
!
                         X1(K1) = SCAINF%XPA_SCA(ISTA,J3)
                         E1(K1) = SCAINF%XPA_SCA_SIG(ISTA,J3)
                       ELSE IF ( J2 .EQ. PAMB__SBAND ) THEN
!
! ---------------------- S-band ARF
!
                         X1(K1) = SCAINF%SPA_SCA(ISTA,J3)
                         E1(K1) = SCAINF%SPA_SCA_SIG(ISTA,J3)
                    END IF
                  ELSE ! not successful observation
                    IF ( IPAR .EQ. 2 ) THEN
!
! ---------------------- Well, old ARF values for the ISTA-th station and
! ---------------------- J3-th scan will be used
!
                         IF ( J2 .EQ. PAMB__XBAND  .AND. &
     &                        SCAINF%USE_SCA(ISTA,J3) ) THEN
!
! --------------------------- X-band ARF
!
                              K2 = K2 + 1 ! Increment of point counter
                              T2(K2) = SCAINF%TIM_SCA(J3)/3600.
                              X2(K2) = SCAINF%XPA_SCA(ISTA,J3)
                              E2(K2) = SCAINF%XPA_SCA_SIG(ISTA,J3)
                            ELSE IF ( J2 .EQ. PAMB__SBAND  .AND. &
     &                                SCAINF%USE_SCA(ISTA,J3) ) THEN
!
! --------------------------- S-band ARF
!
                              K2 = K2 + 1 ! Increment of point counter
                              T2(K2) = SCAINF%TIM_SCA(J3)/3600.
                              X2(K2) = SCAINF%SPA_SCA(ISTA,J3)
                              E2(K2) = SCAINF%SPA_SCA_SIG(ISTA,J3)
                         END IF
                    END IF
               END IF
 430        CONTINUE
!
            IF ( IPAR .EQ. 2 ) THEN
!
! -------------- Calculate the values of linear spline at all time boundaries:
! -------------- K3 -- number of time epochs;
! -------------- T3 -- array of argumetns for segment boundaries;
! -------------- X3 -- arrays of values of linear spline at segment boundaries;
! -------------- E3 -- array of formal sigmas of linear spline at segment
! --------------       boundaries;
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL LSPLINE_GETSEG ( M_LSP, LSPLINE_S(J2,ISTA), K3, T3, &
     &                                 X3, E3, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5383, IUER, 'SCADAM_PLOT', 'Error in '// &
     &                    'getting values of linear spline at bounbdary '// &
     &                    'points' )
                      RETURN
                 END IF
!
! -------------- Scaling argument and setting (almost) zero formal error
!
                 DO 440 J4=1,K3
                    T3(J4) = T3(J4)/3600.0
                    E3(J4) = 0.0001
 440             CONTINUE
            END IF
!
            IF ( K1 .GE. 1 ) THEN
!
! -------------- Initial zeroing
!
                 DIA_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
                 CALL NOUT ( DIA_LEN, DIAGI_S )
!
! -------------- Set the screen size
!
                 CALL CLRCH ( STR )
                 CALL GETENVAR ( 'DIAGI_SCREEN', STR )
                 CALL TRAN   ( 11, STR, STR )
                 DIAGI_S%IDEV = IXS__DEF
                 IF ( STR(1:4) .EQ. 'TINY' ) THEN
                      DIAGI_S%IDEV = 1
                    ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
                      DIAGI_S%IDEV = 2
                    ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
                      DIAGI_S%IDEV = 3
                    ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
                      DIAGI_S%IDEV = 4
                    ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
                      DIAGI_S%IDEV = 5
                 END IF 
!
! -------------- Filling data structure DIAGI_S for preparation plotting
! -------------- ARF values
!
                 CALL CLRCH ( DIAGI_S%ZAG )
                 WRITE ( UNIT=STR, FMT='(F8.3)' ) SCAINF%WRMS(J2,ISTA)
                 CALL CHASHL ( STR )
                 IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
                 DIAGI_S%ZAG = DBOBJ%NAME//' '//DBOBJ%C_STA(ISTA)//' '// &
     &                         ARFNAME(SCAINF%ARF_TYPE)//' at '// &
     &                         BAND_STR(J2)//' wrms='// &
     &                         STR(1:I_LEN(STR))//' (turns)'
!
                 DIAGI_S%ICLR      = 1
                 DIAGI_S%NCLR      = 1
                 DIAGI_S%NPOI(1)   = K1
                 DIAGI_S%ADR_X8(1) = LOC(T1)
                 DIAGI_S%ADR_Y8(1) = LOC(X1)
                 DIAGI_S%ADR_E8(1) = LOC(E1)
                 DIAGI_S%LER (1)   = .TRUE.
                 DIAGI_S%ICOL(1)   = 1
!
                 DIAGI_S%ICOL(1)   = 1
                 DIAGI_S%IOST(1)   = 1
                 DIAGI_S%IPST(1)   = 5
                 DIAGI_S%IWST(1)   = 1
                 DIAGI_S%IBST(1)   = 2
                 DIAGI_S%ILST(1)   = 1
!
                 IF ( IPAR .EQ. 2 ) THEN
!
! ------------------- Setting two black lines for showing min and max boundary
! ------------------- for phase ( 1/2 of phase turn )
!
                      ARG_DOWN(1) = T1(1)
                      ARG_DOWN(2) = T1(K1)
                      VAL_DOWN(1) = -0.5
                      VAL_DOWN(2) = -0.5
!
                      ARG_UP(1)   = T1(1)
                      ARG_UP(2)   = T1(K1)
                      VAL_UP(1)   = 0.5
                      VAL_UP(2)   = 0.5
!
! ------------------- Number of functions
!
                      DIAGI_S%NCLR      = 5
!
! ------------------- Deselected points
!
                      DIAGI_S%NPOI(2)   = K2
                      DIAGI_S%ADR_X8(2) = LOC(T2)
                      DIAGI_S%ADR_Y8(2) = LOC(X2)
                      DIAGI_S%ADR_E8(2) = LOC(E2)
                      DIAGI_S%LER (2)   = .TRUE.
                      DIAGI_S%ICOL(2)   = 1
!
                      DIAGI_S%ICOL(2)   = 3
                      DIAGI_S%IOST(2)   = 1
                      DIAGI_S%IPST(2)   = 5
                      DIAGI_S%IWST(2)   = 1
                      DIAGI_S%IBST(2)   = 2
                      DIAGI_S%ILST(2)   = 1
!
! ------------------- Setting for linear spline curve
!
                      DIAGI_S%NPOI(3)   = K3
                      DIAGI_S%ADR_X8(3) = LOC(T3)
                      DIAGI_S%ADR_Y8(3) = LOC(X3)
                      DIAGI_S%ADR_E8(3) = LOC(E3)
                      DIAGI_S%LER (3)   = .TRUE.
                      DIAGI_S%ICOL(3)   = 2
!
                      DIAGI_S%ICOL(3)   = 2
                      DIAGI_S%IOST(3)   = 1
                      DIAGI_S%IPST(3)   = 4
                      DIAGI_S%IWST(3)   = 2
                      DIAGI_S%IBST(3)   = 0
                      DIAGI_S%ILST(3)   = 2
!
! ------------------- Down
!
                      DIAGI_S%NPOI(4)   = 2
                      DIAGI_S%ADR_X8(4) = LOC(ARG_DOWN)
                      DIAGI_S%ADR_Y8(4) = LOC(VAL_DOWN)
                      DIAGI_S%ADR_E8(4) = 0
                      DIAGI_S%LER (4)   = .FALSE.
                      DIAGI_S%ICOL(4)   = 13
                      DIAGI_S%IOST(4)   = 1
                      DIAGI_S%IPST(4)   = 1
                      DIAGI_S%IWST(4)   = 1
                      DIAGI_S%IBST(4)   = 0
                      DIAGI_S%ILST(4)   = 2
!
! ------------------- Up
!
                      DIAGI_S%NPOI(5)   = 2
                      DIAGI_S%ADR_X8(5) = LOC(ARG_UP)
                      DIAGI_S%ADR_Y8(5) = LOC(VAL_UP)
                      DIAGI_S%ADR_E8(5) = 0
                      DIAGI_S%LER (5)   = .FALSE.
                      DIAGI_S%ICOL(5)   = 13
                      DIAGI_S%IOST(5)   = 1
                      DIAGI_S%IPST(5)   = 1
                      DIAGI_S%IWST(5)   = 1
                      DIAGI_S%IBST(5)   = 0
                      DIAGI_S%ILST(5)   = 2
                 END IF
!
                 DIAGI_S%XMIN      = T1(1)
                 DIAGI_S%XMAX      = T1(K1)
!
                 DIAGI_S%YMIN      =  1.0  ! To force Diagi to calculate
                 DIAGI_S%YMAX      = -1.0  ! boundaries anew
                 DIAGI_S%ITRM      = 0
                 DIAGI_S%IBATCH    = 0
                 CALL CLRCH ( DIAGI_S%NAME )
                 DIAGI_S%STATUS    = DIA__DEF
!
! -------------- Calling the main routine of DiaGI
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DIAGI    ( DIAGI_S, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5384, IUER, 'SCADAM_PLOT', 'Error in '// &
     &                    'attempt to dislppaty a plot using DiaGI' )
                      RETURN
                 END IF
               ELSE
                 WRITE ( 6, * ) 'Station '//DBOBJ%C_STA(ISTA)//': too few points '// &
     &                  'to plot: ',K1
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
                 GOTO 910
            END IF
 420     CONTINUE
      GOTO 910
      END  !#!  SCADAM_PLOT  #!#
