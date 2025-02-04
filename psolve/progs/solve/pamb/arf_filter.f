      SUBROUTINE ARF_FILTER ( PAMB_VER, DBOBJ, OBSSCA, OBSBAS, PAMBI, &
     &                        SCAINF, KAMB, LSPLINE_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ARF_FILTER  filters  ambiguity resolution function (ARF)  *
! *   for all stations selected in SCADAM algoritm for both              *
! *   X and S bands. Parameters of the filter are specified in the       *
! *   fields of the data structure SCAINF.                               *
! *                                                                      *
! *   Filetering means redistribution of integer ambiguities in order    *
! *   to satisfy condition of smoothness. We assume that ARF is more or  *
! *   less smooth function contaminated by ambiguity jumps. Data         *
! *   structure SCAINF keeps paramters of smoothness criterion:          *
! *   1) .DEFRG -- defragrmentation limit. It is assumed that good       *
! *                observations deviates from linear spline by no more   *
! *                than .DEFRG phase turns.                              *
! *   3) .FRZTR -- frozen tranzition limit. We assume that rate of       *
! *                change of ARF does not exceed the limit to be         *
! *                reciprocal to .FRZTR .                                *
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
! *    KAMB ( INTEGER*4 ) -- Number of ambiguities (at each band) to     *
! *                          have been changed.                          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *    SCAINF ( RECORD    ) -- Data structure which keeps a) values of   *
! *                            parameters which control work of          *
! *                            algorithm SCADAM; b) result of work of    *
! *                            algorithm SCADAM.                         *
! * LSPLINE_S ( RECORD    ) -- Array of data structures for computation  *
! *                            of linear spline.                         *
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
! *  ###  26-OCT-98    ARF_FILTER   v1.2 (c)  L. Petrov  18-DEC-98  ###  *
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
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( PAMBI__STRU ) ::  PAMBI(DBOBJ%L_OBS)
      TYPE ( SCAINF__STRU ) ::  SCAINF
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S(2,MO_STA)
      INTEGER*4  IAMB(MO_SCA), INDS(MO_SCA)
      REAL*8     TIM(MO_SCA), ARF_VAL(MO_SCA), ARF_SIG(MO_SCA)
      REAL*8     VAL, SIG, ARF_STA1, ARF_STA2, ARF_BAS, ARF_OBS
      LOGICAL*4  FUSE(MO_SCA), F_PRMT
      LOGICAL*4  AMBCHA_PERMIT
      INTEGER*4  J1, J2, J3, J4, J5, J6, IS, KP, KSCA, ISCA, KUSE, KSUC1, &
     &           KSUC2, ISTA1, ISTA2, IPL_STA1, IPL_STA2, IAMB_OBS, IER
      INTEGER*4  LP_OBS, LPIS_OBS(MO_BAS), ISGN_OBS(MO_BAS)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: FSTREAM, IFIND_PL
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE ( 6, 110 )
 110       FORMAT ( ' ARF_FILTER:  station ',$ )
           WRITE  ( 6, 120 ) 0, DBOBJ%L_STA
 120       FORMAT ( I2,'(',I2,')  '$ )
      END IF
!
      DO 410 J1=1,DBOBJ%L_STA ! Cycle over stations
         IF ( PAMB_VER .GE. 1 ) THEN
              CALL CURL   ( 8 )
              WRITE  ( 6, 120 ) J1, DBOBJ%L_STA
         END IF
!
! ------ Bypass not selected stations and fiducial station
!
         IF ( SCAINF%P_STA(J1)  .AND.  J1 .NE. SCAINF%FID_STA ) THEN
!
            DO 420 J2=1,2 ! Cycle on bands: X-band and S-band
               KP = 0
!
! ------------ Skip unused band
!
               IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS  .AND. &
     &              J2 .EQ. PAMB__SBAND                       ) GOTO 420
               IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS  .AND. &
     &              J2 .EQ. PAMB__XBAND                       ) GOTO 420
!
! ------------ Filling arrays:
! ------------ TIM     -- time in second w.r.t. the first observation in session
! ------------ IAMB    -- new scan-dep ambiguity (0)
! ------------ INDS    -- scan index
! ------------ ARF_VAL -- ARF value
! ------------ ARF_SIG -- ARF formal uncertainty
! ------------ FUSE    -- flag: usable for phase delay solutions
!
               SCAINF%IUSE(J2,J1) = 0
               DO 430 J3=1,DBOBJ%L_SCA ! Scan over all scans
                  IF ( SCAINF%USE_SCA(J1,J3) ) THEN
                       SCAINF%SUC_SCA(J2,J1,J3) = .TRUE.
                       SCAINF%IUSE(J2,J1) = SCAINF%IUSE(J2,J1) + 1
!
! -------------------- Well, ARF value for the J1-th station and J3-th scan
! -------------------- has been calculated.
!
                       KP = KP + 1 ! Increment point counter
                       TIM(KP)  = SCAINF%TIM_SCA(J3)
                       INDS(KP) = J3
!
! -------------------- Initialize ambiguity and usage flag
!
                       IAMB(KP) = 0
                       FUSE(KP) = .TRUE.
!
! -------------------- Gather X-band and S-band ARF values
!
                       IF ( J2 .EQ. PAMB__XBAND ) THEN
!
! ------------------------- X-band ARF
!
                            ARF_VAL(KP) = SCAINF%XPA_SCA(J1,J3)
                            ARF_SIG(KP) = SCAINF%XPA_SCA_SIG(J1,J3)
                          ELSE IF ( J2 .EQ. PAMB__SBAND ) THEN
!
! ------------------------- S-band ARF
!
                            ARF_VAL(KP) = SCAINF%SPA_SCA(J1,J3)
                            ARF_SIG(KP) = SCAINF%SPA_SCA_SIG(J1,J3)
                       END IF
                    ELSE
!
! -------------------- Not used observation
!
                       SCAINF%USE_SCA(J1,J3)    = .FALSE.
                       SCAINF%SUC_SCA(J2,J1,J3) = .FALSE.
                  END IF
 430           CONTINUE
!
! ------------ Now time came to filter ARF values at the J1-th station
! ------------ at the J2-th band
!
               CALL ERR_PASS   ( IUER, IER )
               CALL AMB_FILTER ( KP, TIM, ARF_VAL, ARF_SIG, SCAINF, IAMB, FUSE, &
     &                           KSCA, LSPLINE_S(J2,J1), SCAINF%SHF(J2,J1), &
     &                           SCAINF%WRMS(J2,J1), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 5381, IUER, 'ARF_FILTER', 'Error in '// &
     &                  'filtering station '//DBOBJ%C_STA(J1)// &
     &                  ' in processing '//BAND_STR(J2)//' was detected' )
                    RETURN
               END IF
!
! ------------ Update of baseline-dependent ambiguities and suppression flag
! ------------ in OBSBAS, PAMBI  data structures
!
               CALL ERR_PASS ( IUER, IER )
               CALL STA_AMBUPD ( J1, J2, KP, INDS, IAMB, FUSE, DBOBJ, SCAINF, &
     &                           OBSBAS, PAMBI, KAMB, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 5382, IUER, 'ARF_FILTER', 'Error in '// &
     &                  'filtering updating phase delay ambiguities '// &
     &                  'after filtering ARF values at the station '// &
     &                   DBOBJ%C_STA(J1)//' in processing '//BAND_STR(J2)// &
     &                  ' was detected' )
                    RETURN
               END IF
!
! ------------ Update of station dependent ambiguities and suppression flag
! ------------ in SCAINF data structures
!
               SCAINF%ISUC(J2,J1) = 0
               DO 440 J4=1,KP
                  IS = INDS(J4)
                  IF ( .NOT. FUSE(J4) ) THEN
                       SCAINF%SUC_SCA(J2,J1,IS) = .FALSE.
                  END IF
!
! --------------- Update of values
!
                  IF ( J2 .EQ. PAMB__XBAND ) THEN
                       SCAINF%XPA_SCA(J1,IS) = SCAINF%XPA_SCA(J1,IS) +IAMB(J4) - &
     &                                         SCAINF%SHF(J2,J1)
                     ELSE IF ( J2 .EQ. PAMB__SBAND ) THEN
                       SCAINF%SPA_SCA(J1,IS) = SCAINF%SPA_SCA(J1,IS) +IAMB(J4) - &
     &                                         SCAINF%SHF(J2,J1)
                  END IF
!
! --------------- Update of statistics accumulators
!
                  IF ( SCAINF%SUC_SCA(J2,J1,IS) ) SCAINF%ISUC(J2,J1) = &
     &                                            SCAINF%ISUC(J2,J1) + 1
 440           CONTINUE
 420        CONTINUE
         END IF
 410  CONTINUE
!
! --- Now set status "used" and successfull for fiducial station
!
      KUSE  = -1
      KSUC1 = -1
      KSUC2 = -1
      DO 450 J5=1,DBOBJ%L_OBS
!
! ------ Scan is considered as successfull for fiducial station if there is
! ------ at least one observation between fiducial station and the station
! ------ which had successfull observation for this scan
!
         ISCA  = INT4( OBSBAS(J5)%IND_SCA  )
         ISTA1 = INT4( OBSBAS(J5)%ISITE(1) )
         ISTA2 = INT4( OBSBAS(J5)%ISITE(2) )
         IPL_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
         IPL_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
!
         IF ( IPL_STA1 .EQ. SCAINF%FID_STA  .OR. IPL_STA2 .EQ. SCAINF%FID_STA ) &
     &   THEN
!
! ----------- First check status "used"
!
              IF ( KUSE .NE. ISCA ) THEN
                   IF ( SCAINF%USE_SCA(IPL_STA1,ISCA) .OR. &
     &                  SCAINF%USE_SCA(IPL_STA2,ISCA)     ) THEN
!
                        SCAINF%USE_SCA(SCAINF%FID_STA,ISCA) = .TRUE.
                        SCAINF%IUSE(1,SCAINF%FID_STA) = &
     &                                SCAINF%IUSE(1,SCAINF%FID_STA ) + 1
                        SCAINF%IUSE(2,SCAINF%FID_STA) = &
     &                                SCAINF%IUSE(2,SCAINF%FID_STA ) + 1
                        KUSE = ISCA
                   END IF
              END IF
!
! ----------- Then check status "successfull" at X-band
!
              IF ( KSUC1 .NE. ISCA ) THEN
                   IF ( SCAINF%SUC_SCA(1,IPL_STA1,ISCA) .OR. &
     &                  SCAINF%SUC_SCA(1,IPL_STA2,ISCA)     ) THEN
!
                        SCAINF%SUC_SCA(1,SCAINF%FID_STA,ISCA) = .TRUE.
                        SCAINF%ISUC(1,SCAINF%FID_STA) = &
     &                                SCAINF%ISUC(1,SCAINF%FID_STA ) + 1
                        KSUC1 = ISCA
                   END IF
              END IF
!
! ----------- Then check status "successfull" at S-band
!
              IF ( KSUC2 .NE. ISCA ) THEN
                   IF ( SCAINF%SUC_SCA(2,IPL_STA1,ISCA) .OR. &
     &                  SCAINF%SUC_SCA(2,IPL_STA2,ISCA)     ) THEN
!
                        SCAINF%SUC_SCA(2,SCAINF%FID_STA,ISCA) = .TRUE.
                        SCAINF%ISUC(2,SCAINF%FID_STA) = &
     &                                SCAINF%ISUC(2,SCAINF%FID_STA ) + 1
                        KSUC2 = ISCA
                   END IF
              END IF
         END IF
!
! ------ Now we try to resolve ambiguity for the observations which were not
! ------ used by SCADAM algorithm. We restrict ourselves only the case of
! ------ observations which does'not require chain changs of ambioguities
! ------ on order to keep phase delay closure
!
         DO 460 J6=1,2
            IF ( .NOT. SCAINF%UPD_OBS(J6,J5) ) THEN
!
! -------------- The observation was not updated and it was made at the
! -------------- stations which particilated in SCADAM
!
                 IF ( SCAINF%P_STA(IPL_STA1) .AND. SCAINF%P_STA(IPL_STA2) .AND. &
     &              ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM                   .OR. &
     &                SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR                   .OR. &
     &               (SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS .AND. J6 .EQ. 1 ) .OR. &
     &               (SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS .AND. J6 .EQ. 2 ) ) ) &
     &           THEN
!
! ------------------- Get value of ARF for the first station of the basleine
!
                      IF ( IPL_STA1 .EQ. SCAINF%FID_STA ) THEN
                           ARF_STA1 = 0.0D0
                         ELSE
                           CALL ERR_PASS ( IUER, IER )
                           CALL LSPLINE_GET ( SCAINF%TIM_SCA(ISCA), &
     &                          LSPLINE_S(J6,IPL_STA1), VAL, SIG, IER )
                           IF ( IER .NE. 0 ) THEN
                                WRITE ( 6, * ) ' J6=',J6,' IPL_STA1 = ',IPL_STA1
                                CALL ERR_LOG ( 5383, IUER, 'ARF_FILTER', &
     &                              'Error during computation of the value '// &
     &                              'of linear spline' )
                                RETURN
                           END IF
                      END IF
                      ARF_STA1 = VAL + SCAINF%SHF(J6,IPL_STA1)
!
! ------------------- Get value of ARF for the second station of the basleine
!
                      IF ( IPL_STA2 .EQ. SCAINF%FID_STA ) THEN
                           ARF_STA2 = VAL + SCAINF%SHF(J6,IPL_STA2)
                         ELSE
                           CALL ERR_PASS ( IUER, IER )
                           CALL LSPLINE_GET ( SCAINF%TIM_SCA(ISCA), &
     &                                   LSPLINE_S(J6,IPL_STA2), VAL, SIG, IER )
                           IF ( IER .NE. 0 ) THEN
                                WRITE ( 6, * ) ' J6=',J6,' IPL_STA2 = ',IPL_STA2
                                CALL ERR_LOG ( 5384, IUER, 'ARF_FILTER', &
     &                              'Error during computation of the value '// &
     &                              'of linear spline' )
                                RETURN
                           END IF
                      END IF
!
! ------------------- ARF_BAS -- ARF computed for the baseline on the basis of
! ------------------- linear spline model for ARF for the participating stations
!
                      ARF_BAS = ARF_STA2 - ARF_STA1
!
                      IF ( DABS(ARF_BAS) < 2.D9 ) IAMB_OBS = NINT(ARF_BAS)
                      IF ( ARF_BAS - IAMB_OBS .LT. -0.5 ) THEN
                           IAMB_OBS = IAMB_OBS + 1
                           ARF_OBS  = ARF_OBS  + 1.D0
                      END IF
!
                      IF ( ARF_BAS - IAMB_OBS .GT.  0.5 ) THEN
                           IAMB_OBS = IAMB_OBS - 1
                           ARF_OBS  = ARF_OBS  - 1.D0
                      END IF
                      IF ( IAMB_OBS .NE. 0 ) THEN
!
! ------------------------ Check: can we change ambiguity of the J5-th
! ------------------------ observation without changing ambiguities of other
! ------------------------ observations in order to keep closure
!
                           CALL ERR_PASS ( IUER, IER )
                           F_PRMT = AMBCHA_PERMIT ( J5, DBOBJ, OBSBAS, MO_BAS, &
     &                              LP_OBS, LPIS_OBS, ISGN_OBS, IER )
                           IF ( LP_OBS .EQ. 1 ) THEN
!
! ----------------------------- Actual ambiguity update
!
                                CALL AMB_UPDATE ( J6, IAMB_OBS, OBSBAS(J5), &
     &                                            PAMBI(J5), IER )
                           END IF
                      END IF
                 END IF
            END IF
 460     CONTINUE
 450  CONTINUE
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE ( 6, * ) ' '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ARF_FILTER  #!#
