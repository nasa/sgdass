        SUBROUTINE PREPES_MB ( OBS, GAMB, IUER )
! ************************************************************************
! *                                                                      *
! *       Routine  PREPES_MB  makes pre-estimation of the database with  *
! *     VLBI observations for the certain band (X- or S-) of the certain *
! *     session.                                                         *
! *                                                                      *
! *       Pre-estimation is computation procedure which                  *
! *     1) adjustments of parameters clock polynomial model for all      *
! *        stations except one chosen as the reference one: clock shift, *
! *        clock drift, frequency drift;                                 *
! *     2) resolving group delay ambiguity observations;                 *
! *     3) elimination observations whose deviations from the model      *
! *        exceeds the specified limit;                                  *
! *     4) elimination all observations on the whose baselines where the *
! *        number of observations after making rejection of outliers     *
! *        appeared less than specified limit.                           *
! *                                                                      *
! *     Clock function is calculated using the following expression:     *
! *                                                                      *
! *     CL = GAMB.DCL_SH + GAMB.DCL_DR*TT + GAMB.DCL_SQ*(1.5*TT**2 - 0.5)*
! *                                                                      *
! *     where TT = ( T - GAMB.TAVALL )/GAMB.DCL_TLNALL, T -- time (scale *
! *     TAI), elapsed from the first observation of the session.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       OBS ( RECORD    ) -- Data structure which contains             *
! *                            band-independent information: time of     *
! *                            observation, baseline, lists of objects,  *
! *                            status flags etc.                         *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      GAMB ( RECORD    ) -- Data structures for group delay ambiguity *
! *                            resolution software for the database      *
! *                            under consideration.                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                         Input:  switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                         Output: 0 in the case of successful          *
! *                                 completion and non-zero in the case  *
! *                                 of error.                            *
! *                                                                      *
! *  ###  10-AUG-94    PREPES_MB    v4.4 (c) L. Petrov 03-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE   'solve.i'
        INCLUDE   'gamb.i'
        TYPE ( OBS__STRU ) ::  OBS
        TYPE ( GAMB__STRU ) ::  GAMB
        INTEGER*4  IUER
!
        CHARACTER  STR*80, STR1*80, IS_BAS*3, IS_WHL*3
        CHARACTER  KBAST*7, CBAST*17
        INTEGER*4  MEST
        PARAMETER ( MEST=MG_BAS*2 )
        INTEGER*8  MEM_LEN
        ADDRESS__TYPE :: MEM_ADR, EQU_MAT, EQU_VEC, NOR_MAT, NOR_VEC, EST
        INTEGER*4  J1, J2, J3, J4, J5, J6, J7, NST1, NST2, IER, &
     &             IL, KOBS, IP, N_VAR, IND_STA(MG_STA), &
     &             L_BASOLD, LIS_BASOLD(MG_BAS), K_BASOLD(MG_BAS), &
     &             JMP_BAS(MG_BAS), IST1, IST2, IBS, OLD_L_BAS, L_BAS_RBF
        INTEGER*4  ILEN, I_LEN, IFIND_PL
        REAL*8     DS_ALL, SH_STA(MG_STA), OC, RJMP_BAS
        REAL*8     TEFF, CLS
!
        IF ( OBS%IT .GT. 0 ) THEN
             WRITE (  6, 110 ) GAMB%DBASE
        END IF
        WRITE ( 23, 110 ) GAMB%DBASE
  110   FORMAT ( 1X,'&&&  PREPES_MB:    Session  ',A, &
     &              ' is being analysed.   &&&' )
        CALL CLRCH ( STR )
        WRITE ( UNIT=STR, FMT='(F9.3)' ) GAMB%GAMB_SP*1.D9
        CALL CHASHL ( STR )
        IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
        CALL CLRCH ( STR1 )
        WRITE ( UNIT=STR1, FMT='(F9.3)' ) GAMB%GAMBC*1.D9
        CALL CHASHL ( STR1 )
        IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
        IF ( OBS%IT .GT. 0 ) THEN
             WRITE (  6, 120 ) STR(1:I_LEN(STR)), STR1(1:I_LEN(STR1))
 120         FORMAT ( 1X,'&&&  PREPES_MB:    GPDLAMBG   was: ',A,' nsec,  ', &
     &              ' in use: ',A,' nsec' )
        END IF
        WRITE ( 23, 120 ) STR(1:I_LEN(STR)), STR1(1:I_LEN(STR1))
!
! ----- I. Calculation of adjustments of clock function and resolving group
! -----    delay ambiguity for each basline separatly.
!          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        KOBS=0
        IF ( OBS%IT .GT. 2 ) THEN
             WRITE  (  6, 130 )
             WRITE  ( 23, 130 )
  130        FORMAT ( 1X,'&&&  PREPES_MB:    Pre-pre-estimation.' )
        END IF
!
! ----- Average moment of observations is calculated on the base the
! ----- first and the second observation of the session and therefore they
! ----- don't depend on the presence of outliers.
!
        IP = OBS%NOBS  ! silly compiler doesn't do that it should do
        OBS%TAVALL = ( OBS%TT(IP) + OBS%TT(1) )/2.D0  ! Calculate average moment
        OBS%TLNALL = ( OBS%TT(IP) - OBS%TT(1) )/2.D0  ! and semilength
!
! ----- Saving current list of baselines
!
        L_BASOLD = GAMB%L_BAS
        CALL COPY_I4 ( L_BASOLD, GAMB%LIS_BAS, LIS_BASOLD )
        CALL COPY_I4 ( L_BASOLD, GAMB%K_BAS,     K_BASOLD )
!
        GAMB%STATUS_WHL = GAMB__UNF
        GAMB%STATUS_NZ  = GAMB__NZ_OK
!
! ----- Initialization
!
        CALL NOUT_R8 ( MG_BAS, GAMB%AV_BAS )
        CALL NOUT_R8 ( MG_BAS, GAMB%DS_BAS )
!
! ----- Variable  GAMB.NEW_L_BAS  will trace actual number of non-rejected
! -----                           baselines
! ----- Variable  OLD_L_BAS       will keep the number of baselines before
! -----                           ambiguity resolution
! ----- Variable  L_BAS_RBF       will trace number of baselines rejected before
! -----                           ambiguity resolution soince they contained too
! -----                           few observations
!
        GAMB%NEW_L_BAS  = GAMB%L_BAS
        OLD_L_BAS       = GAMB%L_BAS  !
        L_BAS_RBF       = 0
        DO 410 J1=1,GAMB%L_BAS
           IF ( GAMB%K_BAS(J1) .GE. OBS%MINOBS ) THEN
!
! ------------- Obtain estimates of the parabolic clock function and values
! ------------- of the array group delay ambiguity JMP for j1-th baseline
!
                CALL ERR_PASS  ( IUER, IER )
                CALL PREPES_SB ( OBS%NOBS, GAMB%LIS_BAS(J1), 6, GAMB%GAMBC, &
     &               OBS%CUTOFF, OBS%MINOBS, OBS%IT, OBS%IBA, OBS%TT, GAMB%OCT, &
     &               GAMB%OCF, OBS%TAVALL, OBS%TLNALL, GAMB%USE, GAMB%JMP, &
     &               GAMB%SH_BAS(J1), GAMB%DR_BAS(J1), GAMB%SQ_BAS(J1), &
     &               GAMB%NZ_BAS(J1), IER )
                IF ( IER.EQ.7571 ) IER=7579  !  These errors means that too
                IF ( IER.EQ.7572 ) IER=7579  !  few observations remained and
                IF ( IER.EQ.7573 ) IER=7579  !  this baseline will be rejected
                IF ( IER.NE.0 .AND. IER.NE.7579 ) THEN
                     CALL ERR_LOG ( 7531, IUER, 'PREPES_MB', &
     &                   'PREPES_SB detected error while baseline '// &
     &                    CBAST ( OBS, GAMB%LIS_BAS(J1))//' from the '// &
     &                   'database '//GAMB%DBASE//' was being analysed' )
                     RETURN
                END IF
                IF ( IER .EQ. 7579 ) THEN
                     GAMB%NEW_L_BAS  =  GAMB%NEW_L_BAS - 1
                END IF
             ELSE
!
! ------------- J1-th baseline will not be analyzed further since it contains
! ------------- too few observations.
!
                IER=7578
                L_BAS_RBF = L_BAS_RBF + 1
                GAMB%NEW_L_BAS  =  GAMB%NEW_L_BAS - 1
           END IF
           IF ( IER.NE.0 ) THEN
                GAMB%SUC_BAS(J1) = .FALSE.
!
! ------------- An error was detected during analysis of this session
!
                CALL CLRCH ( STR )
                IF ( IER.EQ.7578 ) THEN
                      STR(1:32)='Rejected: was too few observ. : '
                      CALL INCH ( GAMB%K_BAS(J1), STR(33:) )
                      IL=ILEN(STR)+3
                      STR(IL:)='$$$'
                  ELSE IF ( IER.EQ.7579 ) THEN
                      STR(1:33)='Rejected: too few obs% remained: '
                      CALL INCH ( GAMB%K_BAS(J1), STR(34:) )
                      IL=ILEN(STR)
                      STR(IL+1:)='/'
                      IL=IL+2
                      CALL INCH ( GAMB%NZ_BAS(J1), STR(IL:) )
                      IL=ILEN(STR)+3
                      STR(IL:)='$$$'
                  ELSE
                      STR(1:31)=' Rejected due to the error IER='
                      CALL INCH ( IER, STR(32:) )
                END IF
!
                WRITE  (  6, 140 ) KBAST ( OBS, GAMB%LIS_BAS(J1) ), &
     &                             CBAST ( OBS, GAMB%LIS_BAS(J1) ), &
     &                             STR(1:I_LEN(STR))
                WRITE  ( 23, 140 ) KBAST ( OBS, GAMB%LIS_BAS(J1) ), &
     &                             CBAST ( OBS, GAMB%LIS_BAS(J1) ), &
     &                             STR(1:I_LEN(STR))
  140           FORMAT ( 1X,'Baseline ',A,' ( ',A,' )   ',A  )
!
! ------------- Rejection of all observations made at this baseline
!
                DO 420 J2=1,OBS%NOBS
                   IF ( OBS%IBA(J2) .EQ. GAMB%LIS_BAS(J1) ) GAMB%USE(J2)=.FALSE.
  420           CONTINUE
             ELSE ! O'KEY
!
                GAMB%SUC_BAS(J1) = .TRUE.
                IF ( GAMB%K_BAS(J1) .GT. GAMB__SHORT ) THEN
!
! ------------------ Check criterion of goodness of group ambiguity resolution
! ------------------ for j1-th baseline. If the ratio remained observations/
! ------------------ used observations is below some limit then resolution
! ------------------ considered as suspitious. Thet means that data appeared too
! ------------------ noisy. This creiterion is used only for baselines which
! ------------------ contain number of observations greater some limit.
!
                     IF ( FLOAT(GAMB%NZ_BAS(J1))/FLOAT(GAMB%K_BAS(J1)) .LT. &
     &                    GAMB__TOL_SUS  .AND. &
     &                    GAMB%STATUS_NZ .EQ. GAMB__NZ_OK ) THEN
!
                          GAMB%STATUS_NZ = GAMB__NZ_SUS
                     END IF
                     IF ( FLOAT(GAMB%NZ_BAS(J1))/FLOAT(GAMB%K_BAS(J1)) .LT. &
     &                    GAMB__TOL_BAD ) THEN
                          GAMB%STATUS_NZ = GAMB__NZ_BAD
                     END IF
                END IF
!
! ------------- Increasing total remaining observations counter
!
                IF ( GAMB%NZ_BAS(J1) .GE. OBS%MINOBS ) THEN
                     KOBS = KOBS + GAMB%NZ_BAS(J1)  !  Update of remained
!                                                   !  observations
                END IF
                IF ( OBS%IT .GT. 2 ) THEN     ! counters
!
! ---------------- Calculation  R.M.S. of deviation from regression for each
! ---------------- baseline
!
                   CALL ERR_PASS   ( IUER, IER )
!
                   CALL DISP_SQEL8 ( 1, OBS%NOBS, OBS%TT, GAMB%OCT, &
     &                  OBS%TAVALL, OBS%TLNALL, GAMB%SH_BAS(J1), &
     &                  GAMB%DR_BAS(J1), GAMB%SQ_BAS(J1), GAMB%USE, OBS%IBA, &
     &                  GAMB%LIS_BAS(J1), GAMB%AV_BAS(J1), GAMB%DS_BAS(J1), &
     &                  GAMB%NZ_BAS(J1), IER )
                   IF ( IER.NE.0 ) THEN
                        CALL ERR_LOG ( 7532, IUER, 'PREPES_MB', 'Error in '// &
     &                      'DISP_SQEL8 detected. Baseline: '// &
     &                       KBAST( OBS, GAMB%LIS_BAS(J1) )//' ('// &
     &                       CBAST( OBS, GAMB%LIS_BAS(J1) )//' )' )
                        RETURN
                   END IF
!
! ---------------- Printing R.M.S. at the screen
!
                   CALL CLRCH (  STR )
                   WRITE ( UNIT=STR, FMT='(F15.2)', IOSTAT=IER ) &
     &                                              GAMB%DS_BAS(J1)*1.D9
                   CALL CHASHL ( STR )
                   IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
                   CALL INCH ( GAMB%K_BAS(J1), STR(17:) )
                   IP=I_LEN(STR)+1
                   STR(IP:IP)='/'
                   CALL INCH ( GAMB%NZ_BAS(J1), STR(IP+1:) )
!
                   WRITE (  6, 150 ) KBAST( OBS, GAMB%LIS_BAS(J1)), &
     &                               CBAST( OBS, GAMB%LIS_BAS(J1)), &
     &                               STR(1:I_LEN(STR))
                   WRITE ( 23, 150 ) KBAST( OBS, GAMB%LIS_BAS(J1)), &
     &                               CBAST( OBS, GAMB%LIS_BAS(J1)), &
     &                               STR(1:I_LEN(STR))
  150              FORMAT ( 1X,'Baseline ',A,' ( ',A,' )   r.m.s.=',A )
!
! ---------------- Printing at the screen the adjustments of clock polinomials
!
                   IF ( OBS%IT .GT. 3 ) THEN
                        TEFF = (OBS%TT(1)-OBS%TAVALL)/OBS%TLNALL
                        WRITE (  6, 160 ) KBAST( OBS, GAMB%LIS_BAS(J1) ), &
     &                                    GAMB%SH_BAS(J1), &
     &                                    GAMB%DR_BAS(J1), &
     &                                    GAMB%SQ_BAS(J1)
                        WRITE ( 23, 160 ) KBAST( OBS, GAMB%LIS_BAS(J1) ), &
     &                                    GAMB%SH_BAS(J1), &
     &                                    GAMB%DR_BAS(J1), &
     &                                    GAMB%SQ_BAS(J1)
  160                   FORMAT ( 1X,'Baseline ',A,2X, &
     &                         ' SH=',1PD12.5,' DR=',1PD12.5,' SQ=',1PD12.5 )
                   END IF
                END IF
            END IF
  410   CONTINUE
        IF ( KOBS .LT. OBS%MINOBS ) THEN
             WRITE  (  6, 170 ) KOBS
             WRITE  ( 23, 170 ) KOBS
  170        FORMAT ( 1X,'$$$  Almost all observations are ', &
     &                   'rejected: only ',I5,' remained.  $$$'  )
             GAMB%STATUS_NZ = GAMB__NZ_BAD
             CALL ERR_LOG ( 7533, IUER, 'PREPES_MB', 'Too many observations '// &
     &                     'are rejected' )
             RETURN
        END IF
!
        IF ( OBS%IT .GT. 3 ) THEN
             WRITE  (  6, 180 )
             WRITE  ( 23, 180 )
  180        FORMAT ( 1X,'&&&  PREPES_MB:    Redistribution permanent ', &
     &                   'ambiguities,' )
        END IF
!
! ----- II. Test: Isn't it just a situation, when the remnant from closure
! -----     of triangles for parameters clock shift is a multiple of the
! -----     group delay ambiguity spacing constant? If it is just the case
! -----     then we should redistribute additional clock jumps between
! -----     baselines in order to reduce the remnants from closure of triangles
! -----     to the value less than the group delay ambiguity spacing constant.
! -----     Permanent group delay ambiguity for each baseline is determined.
!
        GAMB%STATUS_CLS = GAMB__CLS_OK
        IF ( GAMB%L_TRI .NE. 0 ) THEN
!
! ---------- Clearing array of scanned stations and array of baseline-dependent
! ---------- clock jumps
!
             CALL NOUT_I4 ( MG_STA, IND_STA )
             CALL NOUT_I4 ( MG_STA, JMP_BAS )
             DO 430 J3=1,GAMB%L_STA
!
! ------------- Search of the first not-scanned station
!
                IF ( IND_STA(J3) .EQ. 0 ) THEN
!
! ------------------ We take this station as a fidicial.
!
                     SH_STA(J3)=0.0D0
                     IND_STA(J3) = 1 ! Set up the flag "scanned"
!
! ------------------ And now scan all baselines
!
                     DO 440 J4=1,GAMB%L_BAS
                        IF ( GAMB%SUC_BAS(J4) ) THEN
!
! ------------------------ Scan only baseline where PREPES_SB resolved
! ------------------------ ambiguities successfully
!
! ------------------------ Find IST1, IST2 -- indeces stations of J4-th baseline
! ------------------------ in the list of stations
!
                           CALL NBAST ( GAMB%LIS_BAS(J4), NST1, NST2 )
                           IST1 = IFIND_PL ( GAMB%L_STA, GAMB%LIS_STA, NST1 )
                           IST2 = IFIND_PL ( GAMB%L_STA, GAMB%LIS_STA, NST2 )
!
! ------------------------ Test: have they been already scanned?
!
                           IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                          IND_STA(IST2) .EQ. 0        ) THEN
!
! ----------------------------- The first station has been scanned but
! ----------------------------- the second -- has not. Determine the clock shift
! ----------------------------- for the second one.
!
                                GAMB%SH_STA(IST2) = GAMB%SH_BAS(J4) + &
     &                                              GAMB%SH_STA(IST1)
                                IND_STA(IST2) = 1 ! Set up the flag "scanned"
                                JMP_BAS(J4)   = 0
                             ELSE IF ( IND_STA(IST1) .EQ. 0  .AND. &
     &                                 IND_STA(IST2) .EQ. 1        ) THEN
!
! ----------------------------- The second station has been scanned but
! ----------------------------- the first -- has not. Determine the clock shift
! ----------------------------- for the unscanned station.
!
                                GAMB%SH_STA(IST1) = GAMB%SH_STA(IST2) - &
     &                                              GAMB%SH_BAS(J4)
                                IND_STA(IST1) = 1 ! Set up the flag "scanned"
                                JMP_BAS(J4)   = 0
                             ELSE IF ( IND_STA(IST1) .EQ. 1  .AND. &
     &                                 IND_STA(IST2) .EQ. 1        ) THEN
!
! ----------------------------- Both stations has been already scanned. Find
! ----------------------------- a clock shift on the base of scanned stations
! ----------------------------- and compare it with the clock shift obtained
! ----------------------------- for this baseline. Entire part of the division
! ----------------------------- is just the constant group delay ambiguity for
! ----------------------------- all observations of the baseline which we are
! ----------------------------- loooking for.
!
                                RJMP_BAS = ( GAMB%SH_BAS(J4) - &
     &                             ( GAMB%SH_STA(IST2) - GAMB%SH_STA(IST1) ) )/ &
     &                               GAMB%GAMBC
                                IF ( DABS(RJMP_BAS) .GT. 1.D9 ) THEN
                                     WRITE ( 6, * ) ' RJMP_BAS =',RJMP_BAS
                                     CALL ERR_LOG ( 7537, IUER, 'PREPES_MB', &
     &                                   'Huge baseline-dependent clocks. '// &
     &                                   'Fatal error.' )
                                     RETURN
                                END IF
                                JMP_BAS(J4)=NINT( RJMP_BAS )
!
! ----------------------------- Calculcate the residual error of the closure
! ----------------------------- for clock shifts
!
                                CLS = (   GAMB%SH_BAS(J4) - &
     &                               ( GAMB%SH_STA(IST2) - GAMB%SH_STA(IST1) ) ) &
     &                               - JMP_BAS(J4)*GAMB%GAMBC
!
! ----------------------------- Subtract the permanent group delay ambiguity
! ----------------------------- found from the clock shift for this baseline
!
                                GAMB%SH_BAS(J4) = GAMB%SH_BAS(J4) - &
     &                                            JMP_BAS(J4)*GAMB%GAMBC
                                IF ( DABS(CLS) .GT. GAMB__CLT_SUS*GAMB%GAMBC ) &
     &                          THEN
                                     GAMB%STATUS_CLS = GAMB__CLS_SUS
                                     STR(1:1) = 'S'
                                  ELSE
                                     STR(1:1) = ' '
                                END IF
                                IF ( OBS%IT .GT. 3 ) THEN
                                WRITE (  6, 190 ) CBAST (OBS, GAMB%LIS_BAS(J4)), &
     &                                CLS*1.D9, JMP_BAS(J4), STR(1:1)
                                WRITE ( 23, 190 ) CBAST (OBS, GAMB%LIS_BAS(J4)), &
     &                                CLS*1.D9, JMP_BAS(J4), STR(1:1)
  190                           FORMAT ( 1X,'&&&  PREPES_MB:  Cor BAS ',A, &
     &                                 ': CLS = ',F10.2,' JMP=',I11,' ',A )
                                END IF
                           END IF
                        END IF
  440                CONTINUE
                END IF
  430        CONTINUE
!
! ---------- Correct JMP vector and vector O-C for tau for the permanent
! ---------- group delay ambiguity for each baseline
!
             DO 450 J5=1,OBS%NOBS
                IBS = IFIND_PL ( GAMB%L_BAS, GAMB%LIS_BAS, OBS%IBA(J5) )
                IF ( IBS .GT. 0 ) THEN
                     GAMB%JMP(J5) = GAMB%JMP(J5) - GAMB%GAMBC*JMP_BAS(IBS)
                     GAMB%OCT(J5) = GAMB%OCT(J5) - GAMB%GAMBC*JMP_BAS(IBS)
                END IF
  450        CONTINUE
        END IF
!
        IF ( OBS%IT .GT. 2 ) THEN
             WRITE  (  6, 1100 )
             WRITE  ( 23, 1100 )
 1100        FORMAT ( 1X,'&&&  PREPES_MB:    Final pre-estimation.' )
        END IF
!
        IF ( GAMB%NEW_L_BAS .NE. OLD_L_BAS ) THEN
!
! ---------- Since the number of baselines is changed we need recalculate
! ---------- internal GAMB lists.
!
             CALL ERR_PASS ( IUER, IER )
             CALL GAMB_LIREC ( OBS, GAMB, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7534, IUER, 'GAMB_LIREC', 'Error during '// &
     &                'recalculation internal GAMB lists' )
                  RETURN
             END IF
        END IF
!
! ----- Determine max number of variables
!
        N_VAR=3*(GAMB%L_STA-1)
!
! ----- Grab dynamic memory for estiamtion clock function using LSQ
!
        CALL GRAB_MEM ( IER, MEM_LEN,                               MEM_ADR, 5, &
     &                       INT8(8)*INT8(N_VAR)*INT8(OBS%NOBS),    EQU_MAT, &
     &                       INT8(8)*INT8(OBS%NOBS),                EQU_VEC, &
     &                       INT8(8)*(INT8(N_VAR)*INT8(N_VAR+1))/2, NOR_MAT, &
     &                       INT8(8)*INT8(N_VAR),                   NOR_VEC, &
     &                       INT8(8)*INT8(N_VAR),                   EST      )
        IF ( IER.NE.0 ) THEN
             CALL ERR_LOG ( 7537, IUER, 'PREPES_MB', 'Error in grabbing '// &
     &                     'dynamic memory' )
             RETURN
        END IF
!
! ----- III. Calculation clock fuction for all observations in one combined
! -----      solution for all stations.
!
        CALL ERR_PASS  ( IUER, IER )
        CALL PREPES_WH ( OBS%NOBS, OBS, GAMB, N_VAR, &
     &                   %VAL(EQU_MAT), %VAL(EQU_VEC), &
     &                   %VAL(NOR_MAT), %VAL(NOR_VEC), %VAL(EST), IER )
        IF ( IER.NE.0 ) THEN
             CALL ERR_LOG ( 7538, IUER, 'PREPES_MB', 'Error in determination '// &
     &           'parameters of clock function in combined solution for all '// &
     &           'stations' )
             CALL FREE_MEM ( MEM_ADR )
             RETURN
        END IF
!
! ----- Freeing dynamic memory
!
        CALL FREE_MEM ( MEM_ADR )
!
        IF ( OBS%IT .GT. 0 ) THEN
             DS_ALL=0.0
             KOBS=0
             IS_WHL='   '
             DO 460 J6=1,GAMB%L_BAS
                IS_BAS='   '
!
! ------------- Caclculation R.M.S. of deviations from regression for J6-th
! ------------- baseline
!
                IF ( GAMB%K_BAS(J6) .GE. OBS%MINOBS ) THEN
                   CALL ERR_PASS   ( IUER, IER )
                   CALL DISP_SQEL8 ( 2, OBS%NOBS, OBS%TT, GAMB%OCT, &
     &                  OBS%TAVALL, OBS%TLNALL, GAMB%SH_BAS(J6), &
     &                  GAMB%DR_BAS(J6), GAMB%SQ_BAS(J6), GAMB%USE, OBS%IBA, &
     &                  GAMB%LIS_BAS(J6), GAMB%AV_BAS(J6), GAMB%DS_BAS(J6), &
     &                  GAMB%K_BAS(J6), IER )
                   DS_ALL = DS_ALL + GAMB%DS_BAS(J6)**2*(GAMB%K_BAS(J6)-1)
                   KOBS   = KOBS + GAMB%K_BAS(J6)
                   IF ( IER.NE.0 ) THEN
                        CALL ERR_LOG ( 7539, IUER, 'PREPES_MB', 'Error in '// &
     &                      'DISP_SQEL8 for baseline '// &
     &                       CBAST( OBS, GAMB%LIS_BAS(J6)) )
                        RETURN
                   END IF
!
! ---------------- Preparing R.M.S. for printout at the screen
!
                   CALL CLRCH (  STR )
                   WRITE ( UNIT=STR, FMT='(1PG12.4)', IOSTAT=IER ) &
     &                                                GAMB%DS_BAS(J6)*1.D9
                   CALL CHASHL ( STR )
                   IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
! ---------------- Determination old number of observation at J6-th baseline
!
                   IP = IFIND_PL ( L_BASOLD, LIS_BASOLD, GAMB%LIS_BAS(J6) )
                   IF ( IP .LE. 0     .OR.   IP .GT. L_BASOLD ) THEN
                      WRITE ( 6, * ) '   GAMB%L_BAS = ',GAMB%L_BAS
                      WRITE ( 6, * ) ' GAMB%LIS_BAS = ',GAMB%LIS_BAS
                      WRITE ( 6, * ) '     L_BASOLD = ',L_BASOLD
                      WRITE ( 6, * ) '   LIS_BASOLD = ',LIS_BASOLD
                      WRITE ( 6, * ) ' J6=',J6,' GAMB%LIS_BAS(J6)=',GAMB%LIS_BAS(J6), &
     &                       ' IP=',IP
                      CALL ERR_LOG ( 7540, IUER, 'PREPES_MB', 'Error '// &
     &                    'while restoration of the old baseline list' )
                      RETURN
                   END IF
                   CALL INCH ( K_BASOLD(IP), STR(17:) )
!
! ---------------- Add an asterisk right to status line if quadratic term
! ---------------- exceeded GAMB.CUTOFF
!
                   IF ( DABS ( GAMB%SQ_BAS(J6) ) .GT. OBS%CUTOFF ) IS_BAS='*'
!
! ---------------- Add two asterisks right if quadratic term exceeded GAMB.GAMBC
!
                   IF ( DABS ( GAMB%SQ_BAS(J6) ) .GT. GAMB%GAMBC ) IS_BAS='**'
!
! ---------------- Adding key: suspecious or bad (or nothing is good)
!
                   IF ( K_BASOLD(IP) .GT. GAMB__SHORT ) THEN
                      IF ( FLOAT(GAMB%K_BAS(J6))/FLOAT(K_BASOLD(IP)) .LT. &
     &                     GAMB__TOL_SUS ) IS_BAS(3:3) = 's'
                      IF ( FLOAT(GAMB%K_BAS(J6))/FLOAT(K_BASOLD(IP)) .LT. &
     &                     GAMB__TOL_BAD ) IS_BAS(3:3) = 'b'
                   END IF
                   IF ( IS_BAS(1:1) .EQ. '*' ) IS_WHL(1:1)='*'
                   IF ( IS_BAS(2:2) .EQ. '*' ) IS_WHL(2:2)='*'
                   IF ( IS_BAS(3:3) .EQ. 's'   .AND. IS_WHL(3:3) .EQ. ' ' ) &
     &             IS_WHL(3:3)='s'
                   IF ( IS_BAS(3:3) .EQ. 'b' ) IS_WHL(3:3)='b'
!
                   IP=I_LEN(STR)+1
                   STR(IP:IP)='/'
                   CALL INCH ( GAMB%K_BAS(J6), STR(IP+1:) )
!
                   STR(28:)=IS_BAS
!
! ---------------- Printing R.M.S. at the screen
!
                   IF ( OBS%IT .GT. 1 ) THEN
                        WRITE (  6, 150 ) KBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                                    CBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                                    STR(1:I_LEN(STR))
                        WRITE ( 23, 150 ) KBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                                    CBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                                    STR(1:I_LEN(STR))
                    END IF
!
! ----------------- Printing the estimates of clock function at the screen
!
                    IF ( OBS%IT .GT. 3 ) THEN
                         TEFF = (OBS%TT(1)-OBS%TAVALL)/OBS%TLNALL
                         WRITE (  6, 160 ) KBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                         (GAMB%SH_BAS(J6) + GAMB%DR_BAS(J6)*TEFF + &
     &                          GAMB%SQ_BAS(J6)*(1.5D0*TEFF**2 - 0.5D0) ), &
     &                          GAMB%DR_BAS(J6)/OBS%TLNALL, &
     &                          3.D0/(OBS%TLNALL**2)*GAMB%SQ_BAS(J6)
                          WRITE ( 23, 160 ) KBAST( OBS, GAMB%LIS_BAS(J6) ), &
     &                          (GAMB%SH_BAS(J6) + GAMB%DR_BAS(J6)*TEFF + &
     &                           GAMB%SQ_BAS(J6)*(1.5D0*TEFF**2 - 0.5D0) ), &
     &                           GAMB%DR_BAS(J6)/OBS%TLNALL, &
     &                           3.D0/(OBS%TLNALL**2)*GAMB%SQ_BAS(J6)
                    END IF
                END IF
  460        CONTINUE
             DS_ALL=DSQRT ( DS_ALL/(KOBS-1) )
        END IF
!
        DO 470 J7=1,OBS%NOBS
!
! -------- Printing out clock jumps and O-C
!
           IF ( OBS%IT .GT. 5 ) THEN
                IF ( GAMB%USE(J7) ) THEN
                     STR(1:2) = '  '
                   ELSE
                     STR(1:2) = '$$'
                END IF
                TEFF = (OBS%TT(J7)-OBS%TAVALL)/OBS%TLNALL
                IP = IFIND_PL ( GAMB%L_BAS, GAMB%LIS_BAS, OBS%IBA(J7) )
                IF ( IP .GT. 0 ) THEN
                     OC = GAMB%OCT(J7) - &
     &                    ( GAMB%SH_BAS(IP) + &
     &                      GAMB%DR_BAS(IP)*TEFF + &
     &                      GAMB%SQ_BAS(IP)*(1.5D0*TEFF**2 - 0.5D0) &
     &                    )
                  ELSE
                     OC = GAMB%OCT(J7)
                END IF
                WRITE ( 6, 1110 )  J7, STR(1:2), CBAST ( OBS, OBS%IBA(J7) ), &
     &                         GAMB%JMP(J7)*1.D9, &
     &                         NINT ( GAMB%JMP(J7)/GAMB%GAMBC ), &
     &                         OC*1.D9, STR(1:2)
 1110           FORMAT ( 1X,'I=',I5,1X,A,' ',A,' JMP=',1PG9.2,'{',I9,'}', &
     &                   ' OCT=',1PG11.4,' (ns)', A )
           END IF
  470   CONTINUE
        IF ( OBS%IT .GT. 3 ) THEN
             WRITE (  6, * ) 'SIG = ',GAMB%SIG*(ETAU_DEF*1.D9),' nsec'
             WRITE ( 23, * ) 'SIG = ',GAMB%SIG*(ETAU_DEF*1.D9),' nsec'
        END IF
        IF ( OBS%IT .GT. 0 ) THEN
             CALL CLRCH (  STR )
             WRITE ( UNIT=STR(1:12), FMT='(1PG12.4)', IOSTAT=IER ) DS_ALL*1.D9
             CALL CHASHL ( STR )
             IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
             WRITE (  6, 1120 ) GAMB%DBASE, STR(1:12), OBS%NOBS, GAMB%UOBS, &
     &                          KOBS, IS_WHL
             WRITE ( 23, 1120 ) GAMB%DBASE, STR(1:12), OBS%NOBS, GAMB%UOBS, &
     &                          KOBS, IS_WHL
 1120        FORMAT ( 1X,'===  Database ',A,'  R.M.S.=',A, &
     &                1X,I5,'/',I5,'/',I5,' ===  ',A3 )
        END IF
!
! ----- Cheking "number of baseline" goodness criteria
!
        IF ( L_BAS_RBF .EQ. 0  .AND.  GAMB%NEW_L_BAS .EQ. OLD_L_BAS ) THEN
             GAMB%STATUS_BAS = GAMB__BAS_OK
           ELSE IF ( L_BAS_RBF .GT. 0                        .AND. &
     &               GAMB%NEW_L_BAS+L_BAS_RBF .EQ. OLD_L_BAS       ) THEN
             GAMB%STATUS_BAS = GAMB__BAS_SUS
           ELSE
             GAMB%STATUS_BAS = GAMB__BAS_BAD
        END IF
!
! ----- Check of goodnes of entirely solution
!
        GAMB%RMS = DS_ALL
        IF ( GAMB%RMS .LT. GAMB%GAMBC*GAMB__RMS_SUS ) THEN
!
! ---------- OK
!
             GAMB%STATUS_WHL = GAMB__WHL_OK
           ELSE IF ( GAMB%RMS .GE. GAMB%GAMBC*GAMB__RMS_SUS  .AND. &
     &               GAMB%RMS .LT. GAMB%GAMBC*GAMB__RMS_BAD        ) THEN
!
! ---------- Suspicious
!
             GAMB%STATUS_WHL = GAMB__WHL_SUS
           ELSE IF ( GAMB%RMS .GE. GAMB%GAMBC*GAMB__RMS_BAD  .AND. &
     &               GAMB%RMS .LT. GAMB%GAMBC*GAMB__RMS_FAI        ) THEN
!
! ---------- Bad
!
             GAMB%STATUS_WHL = GAMB__WHL_BAD
!
           ELSE IF ( GAMB%RMS .GE. GAMB%GAMBC*GAMB__RMS_BAD        ) THEN
!
! ---------- Failure
!
             GAMB%STATUS_WHL = GAMB__WHL_FAI
        END IF
!
        CALL ERR_LOG ( 0, IUER )
!
        RETURN
        END  !#!  PREPES_MB  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE PREPES_WH ( LOBS, OBS, GAMB, N_VAR, EQU_MAT, EQU_VEC, &
     &                         NOR_MAT, NOR_VEC, EST, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  PREPES_WH  calculates the coefficients of clock         *
! *   function in combine solution using observations at all baselines.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N_VAR ( INTEGER*4 ) -- The number of adjusted parameters.         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  EQU_MAT ( REAL*8    ) -- Working matrix with size N_VAR*LOBS.       *
! *  EQU_VEC ( REAL*8    ) -- Working vector with length LOBS.           *
! *  NOR_MAT ( REAL*8    ) -- Normal matrix dimension of                 *
! *                           (N_VAR*(N_VAR+1))/2                        *
! *  NOR_VEC ( REAL*8    ) -- Normal vector dimension of N_VAR.          *
! *      EST ( REAL*8    ) -- Vector of the adjustments of clock         *
! *                           functions with length  N_VAR.              *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     GAMB ( RECORD    ) -- Data structure for group delay ambiguty    *
! *                           resolution software, for the certain band. *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  10-AUG-94    PREPES_WH    V4.0 (c) L. Petrov   03-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE   'phys.i'
        INCLUDE   'solve.i'
        INCLUDE   'gamb.i'
        TYPE ( OBS__STRU ) ::  OBS
        TYPE ( GAMB__STRU ) ::  GAMB
        INTEGER*4  LOBS, N_VAR, IUER
        REAL*8     EQU_MAT(N_VAR,LOBS), EQU_VEC(LOBS), NOR_MAT(*), &
     &             NOR_VEC(N_VAR), EST(N_VAR)
!
        INTEGER*4  N_EQU, J1, J2, J3, PL_BAS, PL_ST1, PL_ST2, &
     &             IST1, IST2, ISH1, ISH2, IDR1, IDR2, ISQ1, ISQ2, IER
        REAL*8     TEFF
        INTEGER*4  IFIND_PL, NSTBA
!
! ----- Initial zeroing
!
        CALL NOUT_R8 ( N_VAR*LOBS,          EQU_MAT )
        CALL NOUT_R8 ( LOBS,                EQU_VEC )
        CALL NOUT_R8 ( N_VAR,               EST     )
        CALL NOUT_R8 ( (N_VAR*(N_VAR+1))/2, NOR_MAT )
        CALL NOUT_R8 ( N_VAR,               NOR_VEC )
!
        CALL NOUT_R8 ( MG_BAS, GAMB%SH_BAS  )
        CALL NOUT_R8 ( MG_BAS, GAMB%DR_BAS  )
        CALL NOUT_R8 ( MG_BAS, GAMB%SQ_BAS  )
!
! ----- Forming the matrix of conditional equations
!
        N_EQU=0
        DO 410 J1=1,LOBS
           IF ( GAMB%USE(J1) ) THEN
!
! ------------- Find index of the stations in the list of stations.
!
                CALL NBAST ( OBS%IBA(J1), IST1, IST2 )
                PL_ST1 = IFIND_PL ( GAMB%L_STA, GAMB%LIS_STA, IST1 )
                PL_ST2 = IFIND_PL ( GAMB%L_STA, GAMB%LIS_STA, IST2 )
                IF ( PL_ST1.LT.1  .OR.  PL_ST2.LT.1 ) THEN
                     CALL ERR_LOG ( 7551, IUER, 'PREPES_WH', 'Internal error' )
                     RETURN
                END IF
                N_EQU=N_EQU+1  !  Increment observations counter
!
! ------------- Determine ISH, IDR, ISQ -- number of the row in the matrix
! ------------- of equations of conditions where partial derivatives on
! ------------- clock shift, drift and frequency drift should be put.
!
                ISH1 = PL_ST1-1
                IDR1 = ISH1  +    (GAMB%L_STA-1)
                ISQ1 = ISH1  +  2*(GAMB%L_STA-1)
!
                ISH2 = PL_ST2-1
                IDR2 = ISH2  +    (GAMB%L_STA-1)
                ISQ2 = ISH2  +  2*(GAMB%L_STA-1)
!
! ------------- Calculate effective time of the observation
!
                TEFF = ( OBS%TT(J1) - OBS%TAVALL ) / OBS%TLNALL
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!             type *,' j1=',j1,' tt = ',obs.tt(j1),' jmp = ',gamb.jmp(j1) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------------- Put partial derivatives in the matrix of equations of conditions
!
                IF ( ISH1.GT.0 ) THEN
                     EQU_MAT(ISH1,N_EQU) = -1.0D0
                     EQU_MAT(IDR1,N_EQU) = -TEFF
                     EQU_MAT(ISQ1,N_EQU) = -(1.5D0*TEFF**2 - 0.5D0)
                END IF
                IF ( ISH2.GT.0 ) THEN
                     EQU_MAT(ISH2,N_EQU) = 1.0D0
                     EQU_MAT(IDR2,N_EQU) = TEFF
                     EQU_MAT(ISQ2,N_EQU) = (1.5D0*TEFF**2 - 0.5D0)
                END IF
!
! ------------- ... and O-C -- in the vector of the right part of equation of
! -------------     conditions
!
                EQU_VEC(N_EQU)=GAMB%OCT(J1)/ETAU_DEF
            END IF
  410   CONTINUE
!
! ----- Solution of conditional system using LSQ
!
        CALL ERR_PASS ( IUER, IER )
!        CALL LSQKR ( N_VAR, N_EQU, 0, EQU_MAT,, EQU_VEC,,
!     #               EST,, GAMB.OB, GAMB.SIG,, IER )
        CALL LSQ1 ( N_VAR, N_EQU, EQU_MAT, EQU_VEC, EST, NOR_VEC, NOR_MAT, &
     &              GAMB%OB, GAMB%SIG, IER )
        IF ( IER.NE.0 )THEN
             CALL ERR_LOG ( 7552, IUER, 'PREPES_WH', 'Error during '// &
     &                     'solution conditional system using LSQ' )
             RETURN
        END IF
!
! ----- Calculate clock shift and drift, and frequency drift for each
! ----- baseline which participated in observations using the estimates of
! ----- these parameters for each stations (except fiducial one)
!
        DO 420 J2=1,GAMB%L_STA
           DO 430 J3=1,GAMB%L_STA
              PL_BAS = IFIND_PL ( GAMB%L_BAS, GAMB%LIS_BAS, &
     &                 NSTBA ( GAMB%LIS_STA(J2), GAMB%LIS_STA(J3) ) )
              IF ( PL_BAS .GT. 0 ) THEN
                 ISH1 = J2-1
                 IDR1 = ISH1 +    GAMB%L_STA-1
                 ISQ1 = ISH1 + 2*(GAMB%L_STA-1)
!
                 ISH2 = J3-1
                 IDR2 = ISH2 +    GAMB%L_STA-1
                 ISQ2 = ISH2 + 2*(GAMB%L_STA-1)
!
                 IF ( J2.EQ.1 ) THEN
                      GAMB%SH_BAS(PL_BAS) =  EST(ISH2)*ETAU_DEF
                      GAMB%DR_BAS(PL_BAS) =  EST(IDR2)*ETAU_DEF
                      GAMB%SQ_BAS(PL_BAS) =  EST(ISQ2)*ETAU_DEF
                 END IF
                 IF ( J3.EQ.1 ) THEN
                      GAMB%SH_BAS(PL_BAS) = -EST(ISH1)*ETAU_DEF
                      GAMB%DR_BAS(PL_BAS) = -EST(IDR1)*ETAU_DEF
                      GAMB%SQ_BAS(PL_BAS) = -EST(ISQ1)*ETAU_DEF
                 END IF
                 IF ( J2.NE.1 .AND. J3.NE.1 ) THEN
                      GAMB%SH_BAS(PL_BAS) = (EST(ISH2)-EST(ISH1))*ETAU_DEF
                      GAMB%DR_BAS(PL_BAS) = (EST(IDR2)-EST(IDR1))*ETAU_DEF
                      GAMB%SQ_BAS(PL_BAS) = (EST(ISQ2)-EST(ISQ1))*ETAU_DEF
                 END IF
              END IF
  430      CONTINUE
  420   CONTINUE
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  PREPES_WH  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DISP_SQEL8 ( IPAR, N, T, X, TAV, TLN, SH, DR, SQ, &
     &                          USE, IV, IV_EL, AV, D, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine   DISP_SQEL8  calculates dispersion  -- D, average -- AV *
! *     and the number of used points: --  NZ  for the array X size of N *
! *     from which quadratic regression function is substracted:         *
! *                                                                      *
! *     If IPAR=1  then:    Y(T) = SH + DR*TT + SQ*TT**2                 *
! *                                                                      *
! *        T  -- function argument, TT=T, SH -- shift, DR -- drift and   *
! *        SQ -- drift rate of the regression.                           *
! *                                                                      *
! *     If IPAR=2  then:    Y(T) = SH + DR*TT + SQ*( 1.5*TT**2 - 0.5 )   *
! *                                                                      *
! *        T  -- argument of the function, TT = ( T - TAV )/TLN,         *
! *        SH -- shift, DR -- drift, SQ -- drift rate of the regression. *
! *                                                                      *
! *     Two "Patisipation arrays" are in use: IV, USE. Only those points *
! *     participated in calculation, for whose 1) IV(I)=IV_EL and        *
! *     USE(J1) = .TRUE. All other elements are excluded form counting.  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     IPAR  ( INTEGER*4 )  --  Mode switch.                            *
! *        N  ( INTEGER*4 )  --  The number of elements of the arrays    *
! *                              T, X, IV.                               *
! *        T  ( REAL*8    )  --  Vector of arguments of the function.    *
! *        X  ( REAL*8    )  --  Vector of the values of the function    *
! *                              for which average and dispersion of     *
! *                              deviations ftom regression is           *
! *                              calculated.                             *
! *      TAV  ( REAL*8    )  --  Average point of the argument.          *
! *      TLN  ( REAL*8    )  --  Semilength of the span of argument.     *
! *       SH  ( REAL*8    )  --  Shift parameter of the regression.      *
! *       DR  ( REAL*8    )  --  Drift parameter of the regression.      *
! *       SQ  ( REAL*8    )  --  Drift rate parameter of the regression. *
! *      USE  ( LOGICAL*1 )  --  Suppression vector. If USE(J1) = .TRUE. *
! *                              then J1-th point may be used (if        *
! *                              IV(J1)=IV_EL also). If USE(J1) =.FALSE. *
! *                              then J1-th point is rejected.           *
! *       IV  ( INTEGER*4 )  --  Participation vector, which contain     *
! *                              an integer -- an attribute of the       *
! *                              object (f.e. baseline number).          *
! *    IV_EL  ( INTEGER*4 )  --  Elelment of the selection -- an         *
! *                              attribute of the object.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       AV  ( REAL*8    )  --  Average of NZ deviation from regression *
! *        D  ( REAL*8    )  --  Square root of dispersion of the NZ     *
! *                              points of the deviation from regression *
! *       NZ  ( INTEGER*4 )  --  The number of elements which            *
! *                              participated in calculations.           *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  26-DEC-95    DISP_SQEL8   V1.1  (c) L. Petrov  03-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  IPAR, N, IV(N), IV_EL, NZ, IUER
        REAL*8     TAV, TLN, SH, DR, SQ, T(N), X(N), D, AV
        LOGICAL*1  USE(N)
        INTEGER*4  J1, J2, I_LEN
        REAL*8     TT
        CHARACTER  STR*10
!
! ----- Initial zeroings
!
        AV = 0.D0
        D  = 0.D0
        NZ = 0
!
! ----- Calculation of the average
!
        DO 410 J1=1,N
           IF ( USE(J1)  .AND.  IV(J1) .EQ. IV_EL ) THEN
                NZ=NZ+1             !  NZ  --  The number of points participated
!                                   !          in observations.
                IF ( IPAR .EQ. 1 ) THEN
                     TT = T(J1)
                     AV = AV+( X(J1) - ( SH + DR*TT + SQ*TT**2 ) )
                  ELSE IF ( IPAR.EQ. 2) THEN
                     TT = ( T(J1) - TAV )/TLN
                     AV = AV + ( X(J1) - &
     &                         ( SH + DR*TT + SQ*(1.5D0*TT**2 - 0.5D0) ) )
                END IF
           END IF
  410   CONTINUE
!
        IF ( NZ.LT.2 ) THEN
!
! ---------- It appeared that the number of observations participated
! ---------- in calculations is too few.
!
             CALL CLRCH ( STR )
             CALL INCH  ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP_SQEL8', 'Only '//STR(1:I_LEN(STR))// &
     &           ' elements participated in calculations' )
             RETURN
        END IF
        AV=AV/NZ
!
! ----- Calculation of dispersion
!
        DO 420 J2=1,N
           IF ( USE(J2)  .AND.  IV(J2) .EQ. IV_EL ) THEN
                IF ( IPAR.EQ. 1 ) THEN
                     TT = T(J2)
                     D  = D + ( (X(J2)-AV) - ( SH + DR*TT + SQ*TT**2 ) )**2
                  ELSE IF ( IPAR.EQ. 2) THEN
                     TT = ( T(J2) - TAV )/TLN
                     D  = D + ( (X(J2)-AV) - &
     &                        ( SH + DR*TT + SQ*(1.5D0*TT**2 - 0.5D0) ))**2
                END IF
           END IF
  420   CONTINUE
!
        D = DSQRT ( D/(NZ-1) )
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP_SQEL8  #!#
