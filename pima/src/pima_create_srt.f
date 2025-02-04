      SUBROUTINE PIMA_CREATE_SRT ( PIM, SNR_DETECTION_2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_CREATE_SRT
! *                                                                      *
! * ### 02-JUL-2009  PIMA_CREATE_SRT  v1.5 (c) L. Petrov 11-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      INTEGER*4  IUER
      CHARACTER  STR*128
      REAL*8,    ALLOCATABLE :: SRT(:)
      INTEGER*4, ALLOCATABLE :: SCA_OBS_IND(:,:), USED_OBS(:), FRG_USE(:)
      REAL*8     TIM_BEG_OBS(PIM__MOBS),     TIM_END_OBS(PIM__MOBS), &
     &           TIM_BEG_MOD_OBS(PIM__MBAS), TIM_END_MOD_OBS(PIM__MBAS), &
     &           TIM_BEG_ERR_OBS(PIM__MBAS), TIM_END_ERR_OBS(PIM__MBAS), &
     &           TIM_BEG_SCA, TIM_END_SCA, &
     &           TIM_BEG_MOD_SCA, TIM_END_MOD_SCA, &
     &           TIM_BEG_MOD_ERR, TIM_END_MOD_ERR, &
     &           TIM_EPS, TIM_SCA(3,PIM__MSCA), &
                 TIM_0, TAI_0, TIM_MID, SNR_DETECTION_2, SIG_INC, DET, &
     &           TIM_OBS, TIM_1ST_AP, TIM_1ST_AP_2ND, &
     &           TIM_BEG_MOD_OBS_STA(2), TIM_END_MOD_OBS_STA(2)
      PARAMETER  ( TIM_EPS = 1.D-7 )
      INTEGER*8  TIM_OBS_I8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           J23, J24, J25, IND_OBS_2ND, IP, MAX_NUM_AP, &
     &           SCA_IND, MJD_0, NTIM, L_BAS, K_BAS, N_ITR, IND_SCA, &
     &           IND_OBS, UV_IND, FRG_IND, TIM_IND, NAP_MIN, N_AP, N_SCA, &
     &           STA_IND, MOD_IND_BEG, MOD_IND_END, TIM_IND_MIN, TIM_IND_MAX, &
     &           USED_UV(PIM__MUV), USED_BAS(PIM__MBAS), USED_UV_MIN, &
     &           IND_N_MAX, IND_S_MIN, OBS_MIN_IND, MJD_YR, IDAY, NUMB_OBS, &
     &           ITIM, OBS_SCA_IND(PIM__MOBS), KOBS_LOST, KSCA_LOST, &
     &           IND_SCA_LOST(PIM__MSCA), IS, IER
      LOGICAL*4  FL_FOUND
!
      REAL*8     VAR, FUN_N(PIM__MUV), FUN_S(PIM__MUV), &
     &           ARR1(PIM__MSCA), ARR2(PIM__MSCA), SNR_1, SNR_2, FUN_N_MAX, &
     &           FUN_S_MIN 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST, ADD_LIS, MAX_I4
!
      NAP_MIN = 3 ! Minimum number of AP periods
!
! --- Get temporary arrays in dynamic memory
!
      ALLOCATE ( SRT(PIM__MSCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NSCA, STR )
           CALL ERR_LOG ( 7741, IUER, 'PIMA_CREATE_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array SRT' )
           RETURN
      END IF
      CALL NOUT_R8 ( PIM__MSCA, SRT )
!
      ALLOCATE ( USED_OBS(PIM__MOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MOBS, STR )
           CALL ERR_LOG ( 7742, IUER, 'PIMA_CREATE_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array USED_OBS' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM__MOBS, USED_OBS )
!
      ALLOCATE ( FRG_USE(PIM__MOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MOBS, STR )
           CALL ERR_LOG ( 7743, IUER, 'PIMA_CREATE_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array FRG_USE' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM__MOBS, FRG_USE )
!
      ALLOCATE ( SCA_OBS_IND(PIM__MBAS,PIM__MSCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MBAS*PIM__MSCA, STR )
           CALL ERR_LOG ( 7744, IUER, 'PIMA_CREATE_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array SCA' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM__MBAS*PIM__MSCA, SCA_OBS_IND )
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           PIM%L_MKDB = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           IF ( ASSOCIATED ( PIM%CONF%FRIB_OBS ) ) THEN
                DEALLOCATE ( PIM%CONF%FRIB_OBS )
           END IF
!
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 7746, IUER, 'PIMA_CREATE_SRT', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      N_SCA = 0
      OBS_SCA_IND = 0
      DO 420 J2=1,PIM%NSCA
         TIM_BEG_SCA =  1.D10
         TIM_END_SCA = -1.D10
         TIM_BEG_MOD_SCA = -1.D10
         TIM_END_MOD_SCA =  1.D10
!
! ------ Find TIM_BEG_SCA -- the latest begin time tag among all
! ------                     observations of this scan.
! ------ Find TIM_END_SCA -- the earliest end time tag among all
! ------                     observations of this scan.
! ------ Thus, we find the interval [TIM_BEG_SCA, TIM_END_SCA] when
! ------ all stations have valid APs
!
! ------ Find TIM_BEG_MOD_SCA -- the latest begin time for the apriori model 
! ------                         among all observations of this scan.
! ------ Find TIM_END_MOD_SCA -- the earliest end time for the apriori model 
! ------                         among all observations of this scan.
!
         TIM_IND_MIN = PIM__MEPC + 1
         TIM_IND_MAX = -1
         DO 430 J3=1,PIM%SCA(J2)%NBAS
            IND_OBS = PIM%SCA(J2)%OBS_IND(J3)
            IND_OBS_2ND = PIM%OBS(IND_OBS)%IND_OBS_2ND
            IF ( IND_OBS_2ND == 0 ) IND_OBS_2ND = IND_OBS
            IF ( IND_OBS < 1  .OR.  IND_OBS > PIM%NOBS ) GOTO 430
!
! --------- Now we check, whether this observation is marked as eligible
! --------- for fringing. If not, we bypass it
!
            FL_FOUND = .FALSE.
            DO 440 J4=1,PIM%CONF%FRIB_NOBS
               IF ( IND_OBS == PIM%CONF%FRIB_OBS(J4) ) FL_FOUND = .TRUE.
 440        CONTINUE
            IF ( .NOT. FL_FOUND .AND. PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, '(A,I6,A)' ) 'PIMA_CREATE_SRT Obs ', IND_OBS, &
     &                   ' was found in the external deselection list'
            END IF
            IF ( .NOT. FL_FOUND ) GOTO 430
!
! --------- Bypass scans with no fringes at both bands
!
            IF ( BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), REA__PIM ) ) THEN
                 IF ( BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), FAI__PIM ) ) THEN
                      SNR_1 = 0.0D0
                    ELSE
                      SNR_1 = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,1)/PIM%OBS(IND_OBS)%NOISE(1)
                 END IF
               ELSE
                 SNR_1 = 0.0D0
            END IF
            IF ( BTEST ( PIM%OBS(IND_OBS_2ND)%FRI_STS(2), REA__PIM ) ) THEN
                 IF ( BTEST ( PIM%OBS(IND_OBS_2ND)%FRI_STS(2), FAI__PIM ) ) THEN
                      SNR_2 = 0.0D0
                    ELSE
                      SNR_2 = PIM%OBS(IND_OBS_2ND)%AMPL(PIMA__DRF,2)/PIM%OBS(IND_OBS_2ND)%NOISE(2)
                 END IF
                 IF ( PIM%CONF%MKDB_FILTER == PIMA__ONLY_DET .AND. &
     &                SNR_1 < PIM%CONF%FRIB_SNR_DETECTION    .AND. &
     &                SNR_2 < SNR_DETECTION_2                      ) THEN
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                           WRITE ( 6, '(A,I6,A)' ) 'PIMA_CREATE_SRT Obs ', IND_OBS, &
     &                            ' was rejected because not detected at the 2nd band'
                      END IF
                      GOTO 430
                 END IF
               ELSE
                 IF ( PIM%CONF%MKDB_FILTER == PIMA__ONLY_DET .AND. &
     &                SNR_1 < PIM%CONF%FRIB_SNR_DETECTION          ) THEN
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                           WRITE ( 6, '(A,I6,A)' ) 'PIMA_CREATE_SRT Obs ', IND_OBS, &
     &                            ' was rejected because not detected at the 1st band'
                      END IF
                      GOTO 430
                 END IF
            END IF
            USED_OBS(IND_OBS) = 1  ! Mark the observation as usable
            IF ( J3 > 1 ) THEN
                 DO 450 J5=J3,3,-3
                    IF ( J5 < 1 ) GOTO 450
                    IF ( J5 > PIM%SCA(J2)%NUM_EPC ) GOTO 450
!
! ----------------- Now we check whether there was already an observation
! ----------------- of this scan with the same stations, but in the reversed
! ----------------- order. If yes, than we mark this observation as
! ----------------- unusable (duplicate)
!
                    IF ( PIM%OBS(PIM%SCA(J2)%OBS_IND(J5))%STA_IND(1) == PIM%OBS(IND_OBS)%STA_IND(2) .AND. &
     &                   PIM%OBS(PIM%SCA(J2)%OBS_IND(J5))%STA_IND(2) == PIM%OBS(IND_OBS)%STA_IND(1)       ) THEN
                         IF ( BTEST ( PIM%OBS(PIM%SCA(J2)%OBS_IND(J5))%FRI_STS(1), FAI__PIM ) ) THEN
                              USED_OBS(PIM%SCA(J2)%OBS_IND(J5)) = 0  ! Mark the observation as unusable duplicate
                            ELSE
                              USED_OBS(IND_OBS) = 0  ! Mark the observation as unusable duplicate
                              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                   WRITE ( 6, '(A,I6,A)' ) 'PIMA_CREATE_SRT Obs ', IND_OBS, &
     &                                   ' was rejected because there is already an '// &
     &                                   'observation with reversed order of stations'
                              END IF
                         END IF
                    END IF
 450             CONTINUE
            END IF
!
            TIM_BEG_OBS(IND_OBS) =  1.D10
            TIM_END_OBS(IND_OBS) = -1.D10
!
! --------- Find TIM_BEG_OBS(IND_OBS) and TIM_END_OBS(IND_OBS) --
! --------- time tag of the first and last valid AP at this observation
!
            N_AP = 0
!
! --------- Check all epochs
!
            MAX_NUM_AP = MAX_I4 ( PIM__MUVS, PIM%OBS(IND_OBS)%NUM_EPC )
            DO 460 J6=1,MAX_NUM_AP
               IF ( PIM%CONF%FRG_USE == PIMA__COMBINE ) THEN
!
! ----------------- If the frequency group is combined, we search for the first 
! ----------------- frequency group that is not empty, i.e. has accumulation
! ----------------- periods
!
                    DO 470 J7=PIM%CONF%FRG_LIST(1),PIM%CONF%FRG_LIST(2)
                       FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(J7)
                       IF ( FRG_IND == 0 ) GOTO 470
                       UV_IND = PIM%OBS(IND_OBS)%UV_IND(J6,FRG_IND)
                       IF ( UV_IND == 0 ) GOTO 470
                       FRG_USE(IND_OBS) = J7
                       GOTO 870
 470                CONTINUE 
 870                CONTINUE 
                  ELSE
!
! ----------------- Normal case: the frequency group index is fixed
!
                    FRG_USE(IND_OBS) = PIM%CONF%FRQ_GRP
                    FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(FRG_USE(IND_OBS))
               END IF
               IF ( FRG_IND == 0 ) GOTO 460
               UV_IND = PIM%OBS(IND_OBS)%UV_IND(J6,FRG_IND)
               IF ( UV_IND == 0 ) GOTO 460
!
               IF ( PIM%CONF%CORR_FLAG_MIN .GE. -2 ) THEN
                    IF ( PIM%OBS(IND_OBS)%CORR_FLAG(J6,FRG_IND) .LE. &
     &                   PIM%CONF%CORR_FLAG_MIN ) THEN
                         GOTO 460
                    END IF
               END IF
               IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) > 0        .AND. &
     &              ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG )      ) THEN
                    IF ( PIM%OBS(IND_OBS)%USER_FLAG(J6) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                         GOTO 460
                    END IF
               END IF
               N_AP = N_AP + 1
               TIM_IND_MIN = MIN ( TIM_IND_MIN, PIM%UV_IND(UV_IND)%TIM_IND )
               TIM_IND_MAX = MAX ( TIM_IND_MAX, PIM%UV_IND(UV_IND)%TIM_IND )
               IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) < TIM_BEG_OBS(IND_OBS) ) THEN
                    TIM_BEG_OBS(IND_OBS) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
               END IF
               IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) > TIM_END_OBS(IND_OBS) ) THEN
                    TIM_END_OBS(IND_OBS) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
               END IF
 460        CONTINUE
!
! --------- Check time epochs of the model. Epochs of the model sets
! --------- a constraint on observation-wide TIM_BEG/TIM_END
!
            IF ( PIM%NMOD == 0 ) THEN
!
! -------------- In the case when no a priori interferometric 
! -------------- model is supplied, set fictitious values
!
                 TIM_BEG_MOD_OBS(J3) = TIM_BEG_OBS(IND_OBS) - 1.0D0
                 TIM_END_MOD_OBS(J3) = TIM_END_OBS(IND_OBS) + 1.0D0
               ELSE 
!@                 DO 480 J8=1,2
!@                    IF ( PIM%OBS(IND_OBS)%MOD_IND_BEG(J8) > 0 ) THEN
!@                         STA_IND = PIM%OBS(IND_OBS)%STA_IND(J8)
!@                         MOD_IND_BEG = PIM%OBS(IND_OBS)%MOD_IND_BEG(J8)
!@                         MOD_IND_END = PIM%OBS(IND_OBS)%MOD_IND_END(J8)
!@                         IF ( J8 == 1 ) THEN
!@                              TIM_BEG_MOD_OBS(J3) = PIM%STA(STA_IND)%MOD(MOD_IND_BEG)%TIM_BEG
!@                              TIM_END_MOD_OBS(J3) = PIM%STA(STA_IND)%MOD(MOD_IND_END)%TIM_END
!@                            ELSE IF ( J8 == 2 ) THEN
!@                              TIM_BEG_MOD_OBS(J3) = MAX ( TIM_BEG_MOD_OBS(J3), &
!@     &                                              PIM%STA(STA_IND)%MOD(MOD_IND_BEG)%TIM_BEG )
!@                              TIM_END_MOD_OBS(J3) = MIN ( TIM_END_MOD_OBS(J3), &
!@     &                                              PIM%STA(STA_IND)%MOD(MOD_IND_END)%TIM_END )
!@                         END IF
!@                         IF ( TIM_BEG_MOD_OBS(J3) > TIM_BEG_MOD_SCA ) THEN
!@                              TIM_BEG_MOD_SCA = TIM_BEG_MOD_OBS(J3) 
!@                         END IF
!@                         IF ( TIM_END_MOD_OBS(J3) < TIM_END_MOD_SCA ) THEN
!@                              TIM_END_MOD_SCA = TIM_END_MOD_OBS(J3) 
!@                         END IF
!@                    END IF
!@ 480             CONTINUE
!
! -------------- Computation of the interval for the model validity for 
! -------------- observation with index J3
!
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 IF ( PIM%OBS(IND_OBS)%MOD_IND_BEG(1) < 1 ) THEN
                      IF ( PIM%CONF%CHECK_SEVERITY == 1 ) THEN
                           WRITE ( 6, 210 ) J3, 'MOD_IND_BEG', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
 210                       FORMAT ( 'PIMA_CREATE_SRT: Error in processing obsevation ', &
     &                               I6, 1X, A,  ' is wrong for station ', A/ &
     &                              'Nevertheless, continue' )
                           GOTO 430
                         ELSE IF ( PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                           CALL ERR_LOG ( 7747, IUER, 'PIMA_CREATE_SRT', 'Trap of '// &
     &                         'internal control in processing observation '//TRIM(STR)// &
     &                         'MOD_IND_BEG is wrong for station '// &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))// &
     &                          ' You can set CHECK_SEVERITY to 1 if you are sure' )
                           RETURN
                      END IF
                 END IF
                 IF ( PIM%OBS(IND_OBS)%MOD_IND_BEG(2) < 1 ) THEN
                      IF ( PIM%CONF%CHECK_SEVERITY == 1 ) THEN
                           WRITE ( 6, 210 ) J3, 'MOD_IND_BEG', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                           GOTO 430
                         ELSE IF ( PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                           CALL ERR_LOG ( 7748, IUER, 'PIMA_CREATE_SRT', 'Trap of '// &
     &                         'internal control in processing observation '//TRIM(STR)// &
     &                         'MOD_IND_BEG is wrong for station '// &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
                           RETURN
                     END IF
                 END IF
                 IF ( PIM%OBS(IND_OBS)%MOD_IND_END(1) < 1 ) THEN
                      IF ( PIM%CONF%CHECK_SEVERITY == 1 ) THEN
                           WRITE ( 6, 210 ) J3, 'MOD_IND_END', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
                           GOTO 430
                         ELSE IF ( PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                           CALL ERR_LOG ( 7749, IUER, 'PIMA_CREATE_SRT', 'Trap of '// &
     &                         'internal control in processing observation '//TRIM(STR)// &
     &                         'MOD_IND_END is wrong for station '// &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) )
                           RETURN
                      END IF
                 END IF
                 IF ( PIM%OBS(IND_OBS)%MOD_IND_END(2) < 1 ) THEN
                      IF ( PIM%CONF%CHECK_SEVERITY == 1 ) THEN
                           WRITE ( 6, 210 ) J3, 'MOD_IND_END', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                           GOTO 430
                         ELSE IF ( PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                           CALL ERR_LOG ( 7750, IUER, 'PIMA_CREATE_SRT', 'Trap of '// &
     &                         'internal control in processing observation '//TRIM(STR)// &
     &                         'MOD_IND_END is wrong for station '// &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
                          RETURN
                      END IF
                 END IF
                 DO 480 J8=1,2
                    STA_IND = PIM%OBS(IND_OBS)%STA_IND(J8)
                    MOD_IND_BEG = PIM%OBS(IND_OBS)%MOD_IND_BEG(J8)
                    MOD_IND_END = PIM%OBS(IND_OBS)%MOD_IND_END(J8)
                    TIM_BEG_MOD_OBS_STA(J8) = PIM%STA(STA_IND)%MOD(MOD_IND_BEG)%TIM_BEG
                    TIM_END_MOD_OBS_STA(J8) = PIM%STA(STA_IND)%MOD(MOD_IND_END)%TIM_END
 480            CONTINUE 
                TIM_BEG_MOD_OBS(J3) = MAX ( TIM_BEG_MOD_OBS_STA(1), TIM_BEG_MOD_OBS_STA(2) )
                TIM_END_MOD_OBS(J3) = MIN ( TIM_END_MOD_OBS_STA(1), TIM_END_MOD_OBS_STA(2) )
!
                IF ( TIM_BEG_MOD_OBS(J3) > TIM_BEG_MOD_SCA ) THEN
                     TIM_BEG_MOD_SCA = TIM_BEG_MOD_OBS(J3) 
                END IF
                IF ( TIM_END_MOD_OBS(J3) < TIM_END_MOD_SCA ) THEN
                     TIM_END_MOD_SCA = TIM_END_MOD_OBS(J3) 
                END IF
            END IF
!
! --------- Compute SIG_INC: maximum allowed increase in group delay 
! --------- error: PIM%CONF%MKDB_GD_MAX_ADD_ERROR ( specified in control file)
! --------- and (1.0D0 + PIM%CONF%MKDB_GD_MAX_SCL_ERROR) * sig_gr_del
!
            SIG_INC = MIN ( PIM%CONF%MKDB_GD_MAX_ADD_ERROR, &
     &                      (1.0D0 + PIM%CONF%MKDB_GD_MAX_SCL_ERROR)* &
     &                      PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__DRF,1) )
!
! --------- We find the interval [tim_beg_err_obs(j3), tim_end_err_obs(j3)]
! --------- where the increase in group delay is less than SIG_INC.
! --------- We do it by solving quadratic equation 
! --------- sig_new^2 = sig_old^2 + 2 sig_old*sig_rat*Dt + sig_rat*Dt**2
! --------- against Dt
!
            DET = DSQRT ( PIM%OBS(IND_OBS)%COV_GR_GD(1)**2 + &
     &                    SIG_INC**2*PIM%OBS(IND_OBS)%GR_RAT_ERR(1)**2 )
            TIM_BEG_ERR_OBS(J3) = PIM%OBS(IND_OBS)%TIM_BEG + &
     &              PIM%OBS(IND_OBS)%FRT_OFFSET(1) + &
     &              (-PIM%OBS(IND_OBS)%COV_GR_GD(1) - DET)/ &
     &                PIM%OBS(IND_OBS)%GR_RAT_ERR(1)**2
!
            IF ( DABS(TIM_BEG_ERR_OBS(J3)) .LE. 10.0D0*86400.0D0 ) THEN
                 ITIM = -IDINT(-TIM_BEG_ERR_OBS(J3))
               ELSE 
                 ITIM = -864000
            END IF
            TIM_BEG_ERR_OBS(J3) = ITIM
            TIM_END_ERR_OBS(J3) = PIM%OBS(IND_OBS)%TIM_BEG + &
     &              PIM%OBS(IND_OBS)%FRT_OFFSET(1) + &
     &              (-PIM%OBS(IND_OBS)%COV_GR_GD(1) + DET)/ &
     &                PIM%OBS(IND_OBS)%GR_RAT_ERR(1)**2
            IF ( DABS(TIM_END_ERR_OBS(J3)) .LE. 10.0D0*86400.0D0 ) THEN
                 TIM_END_ERR_OBS(J3) = IDINT(TIM_END_ERR_OBS(J3))
               ELSE 
                 ITIM = 864000
            END IF
            IF ( TIM_END_ERR_OBS(J3) < TIM_END_ERR_OBS(J3) + 0.4 ) THEN
                 TIM_END_ERR_OBS(J3) = TIM_END_ERR_OBS(J3) + 1
            END IF
!
! --------- Update scan-wide TIM_BEG/TIM_END
!
            IF ( N_AP .GE. NAP_MIN ) THEN
                 IF ( TIM_BEG_OBS(IND_OBS) < TIM_BEG_SCA ) THEN
                      TIM_BEG_SCA = TIM_BEG_OBS(IND_OBS)
                 END IF
!
                 IF ( TIM_END_OBS(IND_OBS) > TIM_END_SCA ) THEN
                      TIM_END_SCA = TIM_END_OBS(IND_OBS)
                 END IF
                 USED_BAS(J3) = 0 ! counted but not used
               ELSE
                 USED_BAS(J3) = 1 ! discarded
                 OBS_SCA_IND(IND_OBS) = -1
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, '(A,I6,A,I3,A)' ) 'PIMA_CREATE_SRT Obs ', IND_OBS, &
     &                           ' was rejected because it has only ', N_AP, ' APs'
                 END IF
            END IF
!@            PIM%OBS(IND_OBS)%TIM_BEG = TIM_BEG_OBS(IND_OBS)
!@            PIM%OBS(IND_OBS)%TIM_END = TIM_END_OBS(IND_OBS)
!@            IF ( IND_OBS_2ND > 0  .AND. IND_OBS_2ND .NE. IND_OBS ) THEN
!@                 PIM%OBS(IND_OBS_2ND)%TIM_BEG = TIM_BEG_OBS(IND_OBS)
!@                 PIM%OBS(IND_OBS_2ND)%TIM_END = TIM_END_OBS(IND_OBS)
!@            END IF
 430     CONTINUE
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
              WRITE ( 6, 220 ) J2, TIM_BEG_SCA, TIM_END_SCA, &
     &                             TIM_BEG_MOD_SCA, TIM_END_MOD_SCA, &
     &                             PIM%SCA(J2)%NBAS
 220          FORMAT ( 'Sca: ', I5, ' Tim_beg_sca: ', F8.2, &
     &                              ' Tim_end_sca: ', F8.2, &
     &                              ' Tim_beg_mod_sca: ', F8.2, &
     &                              ' Tim_end_mod_sca: ', F8.2, &
     &                              ' NBAS: ', I3  )
         END IF
!
         IF ( PIM%NMOD > 0  .AND.  TIM_BEG_MOD_SCA < -86400.0D0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, '(A,I5,A)' ) 'PIMA_CREATE_SRT Scan ', J2, &
     &                         ' is rejected because of TIM_BEG_MOD_SCA'
              END IF
              GOTO 420
         END IF
!
         IF ( DABS(TIM_BEG_SCA) < 10.0D0*86400.0D0 ) THEN
              TIM_BEG_SCA = -IDINT(-TIM_BEG_SCA)
              TIM_END_SCA =  IDINT( TIM_END_SCA)
            ELSE 
              TIM_BEG_SCA = -TIM_BEG_SCA
              TIM_END_SCA =  TIM_END_SCA
         END IF
!
! ------ The scan common time is too short to be considered
!
         IF ( TIM_END_SCA .LE. TIM_BEG_SCA ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, '(A,I5,A,A,A,F9.1,A,F9.1)' ) &
     &                         'PIMA_CREATE_SRT Scan ', J2, &
     &                         '  ', PIM%SCA(J2)%SCAN_NAME, ' is rejected '// &
     &                         ' because of TIM_END_SCA= ', TIM_BEG_SCA, &
     &                         ' and TIM_END_SCA= ', TIM_END_SCA
              END IF
              GOTO 420
         END IF 
!
         N_ITR = PIM%SCA(J2)%NBAS 
         DO 490 J9=1,N_ITR
            FUN_N = 0.0D0
            FUN_N_MAX = 0.0D0
            IND_N_MAX = 0
!
! --------- Compute function FUN_N for the array of epochs with step of 
! --------- 1 sec within the scan.
! --------- FUN_N is the number of stations that may have SRT time for that
! --------- epoch, i.e. within the range of model validity and within the 
! --------- range of acceptable errors increase.
! --------- We find FUN_N_MAX -- the maximim number of stations that 
! --------- may have common SRT
!
            DO 4100 J10=IDNINT(TIM_BEG_SCA), IDNINT(TIM_END_SCA)
               FUN_N(J10-IDNINT(TIM_BEG_SCA)+1) = 0.0
               DO 4110 J11=1,PIM%SCA(J2)%NBAS
                  IF ( USED_BAS(J11) == 0 ) THEN
                       IND_OBS = PIM%SCA(J2)%OBS_IND(J11)
                       IF ( USED_OBS(IND_OBS) == 0 ) GOTO 4110
                       FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(FRG_USE(IND_OBS))
                       IF ( FRG_IND == 0 ) GOTO 4110
!
                       IF ( J10 .GE. TIM_BEG_ERR_OBS(J11) .AND. &
     &                      J10 .GE. TIM_BEG_MOD_OBS(J11) .AND. &
     &                      J10 .LE. TIM_END_ERR_OBS(J11) .AND. &
     &                      J10 .LE. TIM_END_MOD_OBS(J11)       ) THEN
                            FUN_N(J10-IDNINT(TIM_BEG_SCA)+1) = FUN_N(J10-IDNINT(TIM_BEG_SCA)+1) + 1.0D0
                       END IF
                  END IF
 4110          CONTINUE
               IF ( FUN_N(J10-IDNINT(TIM_BEG_SCA)+1) > FUN_N_MAX ) THEN
                    FUN_N_MAX = FUN_N(J10-IDNINT(TIM_BEG_SCA)+1)
                    IND_N_MAX = J10
               END IF
 4100       CONTINUE
!
! --------- Check whether we have any observations left
!
            IF ( FUN_N_MAX < 0.1 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, '(A,I5,A,A,F9.2,A,F9.2)' ) &
     &                    'PIMA_CREATE_SRT Scan ', J2, &
     &                    ' no more valid observations were found. ', &
     &                    ' Tim_beg: ', TIM_BEG_SCA, &
     &                    ' Tim_end: ', TIM_END_SCA    
                 END IF
                 GOTO 420
            END IF
!
! --------- Now we compute another target function FUN_S, this time among
! --------- those epoch for which FUN_N == FUN_N_MAX. 
! --------- We want to find the epoch that minimizes FUN_S
!
            IND_S_MIN = 0
            FUN_S_MIN = 1.D10
            DO 4120 J12=IDNINT(TIM_BEG_SCA), IDNINT(TIM_END_SCA)
               IF ( DABS(FUN_N(J12-IDNINT(TIM_BEG_SCA)+1) - FUN_N_MAX) < 0.1 ) THEN
                    FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) = 1.D10
                    DO 4130 J13=1,PIM%SCA(J2)%NBAS
                       IF ( USED_BAS(J13) == 0 ) THEN
                            IND_OBS = PIM%SCA(J2)%OBS_IND(J13)
                            FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(FRG_USE(IND_OBS))
                            IF ( FRG_IND == 0 ) GOTO 4130
                            IF ( FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) > 1.D9 ) THEN
                                 FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) = 0.0D0
                            END IF
                            FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) = FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) + &
     &                                ( J12 - TIM_BEG_OBS(IND_OBS) - &
     &                                  PIM%OBS(IND_OBS)%FRT_OFFSET(1) )**2
                       END IF
 4130               CONTINUE 
                    IF ( FUN_S(J12-IDNINT(TIM_BEG_SCA)+1) < FUN_S_MIN ) THEN
                         FUN_S_MIN = FUN_S(J12-IDNINT(TIM_BEG_SCA)+1)
                         IND_S_MIN = J12
                    END IF
               END IF
 4120       CONTINUE 
            IF ( IND_S_MIN == 0 ) THEN
!
! -------------- For the case...
!
                 IND_S_MIN = IND_N_MAX
            END IF
!
            N_SCA = N_SCA + 1
            K_BAS = 0
            DO 4140 J14=1,PIM%SCA(J2)%NBAS
               IF ( USED_BAS(J14) == 0 ) THEN
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                         WRITE ( 6, 230 ) J2, J9, J14, PIM%SCA(J2)%OBS_IND(J14), &
     &                                    IND_S_MIN, &
     &                                    TIM_BEG_ERR_OBS(J14), &
     &                                    TIM_END_ERR_OBS(J14), &
     &                                    TIM_BEG_MOD_OBS(J14), &
     &                                    TIM_END_MOD_OBS(J14)
 230                     FORMAT ( 'Scan: ', I5, ' Iter: ', I2, &
     &                            ' Bsl: ', I3, ' Obs: ', I6, ' IM: ', I6, &
     &                            ' TBE: ', F8.1, ' TEE: ', F8.1,  &
     &                            ' TBM: ', F8.1, ' TEM: ', F8.1   )
                    END IF
                    IF ( IND_S_MIN .GE. TIM_BEG_ERR_OBS(J14) .AND. &
     &                   IND_S_MIN .GE. TIM_BEG_MOD_OBS(J14) .AND. &
     &                   IND_S_MIN .LE. TIM_END_ERR_OBS(J14) .AND. &
     &                   IND_S_MIN .LE. TIM_END_MOD_OBS(J14)       ) THEN
!
                         IND_OBS = PIM%SCA(J2)%OBS_IND(J14)
                         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(FRG_USE(IND_OBS))
                         IF ( FRG_IND == 0 ) GOTO 4140
                         K_BAS = K_BAS + 1
                         SCA_OBS_IND(K_BAS,N_SCA) = IND_OBS
                         OBS_SCA_IND(IND_OBS) = N_SCA
                         IF ( K_BAS == 1 ) THEN
                              TIM_SCA(1,N_SCA) = TIM_BEG_OBS(IND_OBS)
                              TIM_SCA(2,N_SCA) = TIM_END_OBS(IND_OBS)
                              TIM_SCA(3,N_SCA) = IND_S_MIN
                            ELSE
                              TIM_SCA(1,N_SCA) = MIN ( TIM_SCA(1,N_SCA), TIM_BEG_OBS(IND_OBS) )
                              TIM_SCA(2,N_SCA) = MAX ( TIM_SCA(2,N_SCA), TIM_END_OBS(IND_OBS) )
                         END IF
                         USED_BAS(J14) = 1
                    END IF
               END IF
 4140       CONTINUE 
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, 110 ) J2, J9, N_SCA, TIM_SCA(1:3,N_SCA), K_BAS
 110             FORMAT ( 'PIMA_CREATE_SRT pima_sca: ', I5, ' Iter: ', I2, ' db_sca: ', I6, &
     &                    ' Tim_beg: ', F9.2, ' Tim_end: ', F9.2, ' Tim_sca: ', F9.2, &
     &                    ' K_bas: ', I3 )
            END IF            
 490     CONTINUE 
 420  CONTINUE
!
      KOBS_LOST = 0
      KSCA_LOST = 0
      DO 4150 J15=1,PIM%NOBS
         IF ( FRG_USE(J15) > 0 ) THEN
              FRG_IND = PIM%OBS(J15)%REF_FRG_INDS(FRG_USE(J15))
              IF ( FRG_IND == 0 ) GOTO 4150
              IF ( OBS_SCA_IND(J15) == 0 ) THEN
                   KOBS_LOST = KOBS_LOST + 1
                   IER = -1
                   IS = ADD_LIS ( PIM__MSCA, KSCA_LOST, IND_SCA_LOST, &
     &                            INT(PIM%OBS(J15)%SCA_IND), IER )
              END IF
         END IF
 4150 CONTINUE 
      IF ( KOBS_LOST > 0 ) THEN
           WRITE ( 6, 240 ) KOBS_LOST, KSCA_LOST
  240      FORMAT ( 'PIMA_CREATE_SRT: Unfortrunately, scan reference time '/ &
     &              '                 was not assigned for ', I6, &
     &              ' obserations in ', I5, ' scans'/ &
     &              '                 Affected scans: ' )
           DO 4160 J16=1,KSCA_LOST
              WRITE ( 6, 250 ) PIM%SCA(IND_SCA_LOST(J16))%SCAN_NAME, &
     &                         PIM%C_SOU(PIM%SCA(IND_SCA_LOST(J16))%SOU_IND)
  250         FORMAT ( '                 Scan ', A, ' Sou: ', A )
 4160      CONTINUE 
           IF ( PIM%CONF%CHECK_SEVERITY > 1 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( KOBS_LOST, STR )
                CALL ERR_LOG ( 7751, IUER, 'PIMA_CREATE_SRT', 'Scan '// &
     &                        'reference time was not assigned for '// &
     &                         STR(1:I_LEN(STR))//' obbserations' )
                RETURN 
           END IF
      END IF
!
! --- Sort the array of database scans in time order
!
      DO 4220 J22=1,N_SCA
         ARR1(J22) = TIM_SCA(3,J22)
         ARR2(J22) = J22 + 0.1D0
 4220 CONTINUE
!
      CALL SORT8 ( N_SCA, ARR1, ARR2 )
!
      CALL ERR_PASS ( IUER, IER )
      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + ARR1(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           write ( 6, * ) ' arr1(1) = ', arr1(1)
           WRITE ( UNIT=STR(1:22), FMT='(1PD22.15)' ) ARR1(1)
           CALL ERR_LOG ( 7752, IUER, 'PIMA_CREATE_SRT', 'Trap of '// &
     &         'internal control: ARR1(1) = '//STR )
           RETURN
      END IF
      STR = STR(1:5)//'01.01_00:00:00.0'
      CALL DATE_TO_TIME ( STR(1:21), MJD_YR, VAR, -2 )
!
      NUMB_OBS = 0
      DO 4230 J23=1,N_SCA
         IND_SCA = ARR2(J23)
         PIM%SCADB(J23)%NOBS = 0
         DO 4240 J24=1,(PIM%NSTA*(PIM%NSTA+1))/2
            IF ( SCA_OBS_IND(J24,IND_SCA) > 0 ) THEN
                 IF ( USED_OBS(SCA_OBS_IND(J24,IND_SCA)) == 1 ) THEN
                      PIM%SCADB(J23)%NOBS = PIM%SCADB(J23)%NOBS + 1
                      PIM%SCADB(J23)%SOU_IND = PIM%OBS(SCA_OBS_IND(J24,IND_SCA))%SOU_IND
                      SCA_IND = PIM%OBS(SCA_OBS_IND(J24,IND_SCA))%SCA_IND
                 END IF
            END IF
 4240    CONTINUE
         ALLOCATE ( PIM%SCADB(J23)%OBS_IND(PIM%SCADB(J23)%NOBS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7753, IUER, 'PIMA_CREATE_SRT', 'Failure to '// &
     &            'allocate dynamic memory for PIM%SCADB%OBS_IND' )
              RETURN
         END IF
         CALL NOUT_I4 ( PIM%SCADB(J23)%NOBS, PIM%SCADB(J23)%OBS_IND )
!
! ------ NB: we round TIM_SRT to the integer number of seconds from the
! ------ midnight (but not from the nominal experiment start!)
!
         PIM%SCADB(J23)%MJD_SRT = PIM%MJD_0
         PIM%SCADB(J23)%TAI_SRT = 1.0D0*IDNINT(TIM_SCA(3,IND_SCA) + PIM%TAI_0) &
     &                            - PIM%TAI_0
         PIM%SCADB(J23)%TIM_BEG = TIM_SCA(1,IND_SCA)
         PIM%SCADB(J23)%TIM_END = TIM_SCA(2,IND_SCA)
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              STR = MJDSEC_TO_DATE ( PIM%SCADB(J23)%MJD_SRT, &
     &                               PIM%TAI_0 + PIM%SCADB(J23)%TAI_SRT, IER )
              WRITE ( 6, 120 ) J23, PIM%SCADB(J23)%TAI_SRT - PIM%SCADB(J23)%TIM_BEG, STR(1:24), PIM%SCADB(J23)%TIM_BEG
 120          FORMAT ( 'PIMA_CREATE_SRT   IND_SCA: ', I5, ' SRT_OFFSET: ', F12.5, ' TIM_SRT: ', A, ' TIM_BEG= ', F16.9 )
         END IF            
!
! ------ Now we put observation indexes to PIM%SCADB(J23)%OBS_IND and
! ------ compute SRT_OFFSET for each observation
!
         IP = 0
         DO 4250 J25=1,(PIM%NSTA*(PIM%NSTA+1))/2
            IF ( SCA_OBS_IND(J25,IND_SCA) > 0 ) THEN
                 IF ( USED_OBS(SCA_OBS_IND(J25,IND_SCA)) == 1 ) THEN
                      IP = IP + 1
                      PIM%SCADB(J23)%OBS_IND(IP) = SCA_OBS_IND(J25,IND_SCA)
                      IND_OBS = SCA_OBS_IND(J25,IND_SCA)
                      IND_OBS_2ND = PIM%OBS(IND_OBS)%IND_OBS_2ND
                      IF ( PIM%NFRG == 1 ) THEN
!
! ------------------------ Just in case... It may be not needed to consider two cases
!
                           TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND)
                           IF ( IND_OBS_2ND > 0 ) THEN
                                TIM_1ST_AP_2ND = PIM%TIM_R8(PIM%OBS(IND_OBS_2ND)%TIM_BEG_IND)
                           END IF
                         ELSE
                           FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(FRG_USE(IND_OBS))
                           TIM_1ST_AP = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND )
                           IF ( IND_OBS_2ND > 0 ) THEN
                                FRG_IND = PIM%OBS(IND_OBS_2ND)%REF_FRG_INDS(FRG_USE(IND_OBS_2ND))
                                TIM_1ST_AP_2ND = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS_2ND)%UV_IND(1,FRG_IND))%TIM_IND )
                           END IF
                      END IF
                      PIM%OBS(IND_OBS)%SRT_OFFSET = PIM%SCADB(J23)%TAI_SRT - TIM_1ST_AP
                      IND_OBS_2ND = PIM%OBS(PIM%SCADB(J23)%OBS_IND(IP))%IND_OBS_2ND
                      IF ( IND_OBS_2ND > 0  .AND. &
     &                     IND_OBS_2ND .NE. PIM%SCADB(J23)%OBS_IND(IP) ) THEN
                           PIM%OBS(IND_OBS_2ND)%SRT_OFFSET = PIM%SCADB(J23)%TAI_SRT - TIM_1ST_AP_2ND
                      END IF
!
! ------------------- A special trick: we round PIM%OBS(IND_OBS)%SRT_OFFSET in such a way,
! ------------------- that scan time would be rounded to 100 ns
!
                      TIM_OBS    = PIM%TAI_0 + TIM_1ST_AP + PIM%OBS(IND_OBS)%SRT_OFFSET 
                      TIM_OBS_I8 = NINT(TIM_OBS/TIM_EPS,KIND=8)
                      PIM%OBS(IND_OBS)%SRT_OFFSET = PIM%OBS(IND_OBS)%SRT_OFFSET - &
     &                                              (TIM_EPS*TIM_OBS_I8 - TIM_OBS)
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                           STR = MJDSEC_TO_DATE ( PIM%MJD_0, TIM_OBS, IER )
                           WRITE ( 6, 130 ) IND_OBS, STR(1:24), TIM_1ST_AP, PIM%OBS(IND_OBS)%TIM_BEG, &
     &                                      PIM%OBS(IND_OBS)%FRT_OFFSET(1), PIM%OBS(IND_OBS)%SRT_OFFSET
 130                       FORMAT ( '  PIMA_CREATE_SRT ind_obs: ', I5, ' tim_srt: ', A, &
     &                              ' tim_beg: ', F12.5, ' tim_1st_ap: ', F12.5, ' frt_offset: ', F12.5,  ' srt_offset: ', F12.5 )
                      END IF            
                 END IF
            END IF
 4250    CONTINUE
         CALL SORT_I ( PIM%SCADB(J23)%NOBS, PIM%SCADB(J23)%OBS_IND )
!
         IDAY = (PIM%MJD_0 - MJD_YR) + IDINT((PIM%TAI_0+PIM%SCADB(J23)%TAI_SRT)/86400.0D0 ) &
     &          + 1
         CALL SR_TAT ( PIM%SCADB(J23)%TAI_SRT + PIM%TAI_0, VAR )
         CALL RH_TAT ( VAR, 0, STR, -2 )
         CALL CHASHL ( STR )
!
         CALL CLRCH ( PIM%SCADB(J23)%NAME )
         PIM%SCADB(J23)%NAME = STR(1:2)//STR(4:5)//STR(7:8)
         CALL CLRCH ( STR )
         CALL INCH  ( IDAY, STR(1:3) )
         CALL CHASHR ( STR(1:3) )
         CALL BLANK_TO_ZERO ( STR(1:3) )
         PIM%SCADB(J23)%NAME = STR(1:3)//'_'// &
     &             PIM%SCADB(J23)%NAME(1:I_LEN(PIM%SCADB(J23)%NAME))
         IF ( J23 > 1 ) THEN
              IF ( PIM%SCADB(J23)%NAME(1:10) == PIM%SCADB(J23-1)%NAME(1:10) ) THEN
                   IF ( PIM%SCADB(J23-1)%NAME(11:11) == '_' ) THEN
                        IP = 0
                      ELSE
                        IP = ICHAR( PIM%SCADB(J23-1)%NAME(11:11) ) - 96
                   END IF
                   PIM%SCADB(J23)%NAME = PIM%SCADB(J23)%NAME(1:10)// &
     &                                   CHAR(96+IP+1)//PIM%SCADB(J23)%NAME(11:)
              END IF
         END IF
!
         CALL CLRCH   ( STR )
         CALL INCH    ( SCA_IND, STR )
         CALL CHASHR  ( STR(1:4) )
         CALL BLANK_TO_ZERO ( STR(1:4) )
         PIM%SCADB(J23)%NAME = PIM%SCADB(J23)%NAME(1:I_LEN(PIM%SCADB(J23)%NAME))// &
     &                             '_'//STR(1:4)
 4230 CONTINUE
      PIM%L_MKDB = N_SCA
!
      DEALLOCATE ( SCA_OBS_IND )
      DEALLOCATE ( FRG_USE  )
      DEALLOCATE ( USED_OBS )
      DEALLOCATE ( SRT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_CREATE_SRT  !#!#
