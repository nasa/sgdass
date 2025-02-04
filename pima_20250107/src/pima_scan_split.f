      SUBROUTINE PIMA_SCAN_SPLIT ( PIM, N_BAD, UV_BAD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_SCAN_SPLIT
! *                                                                      *
! * ### 08-JAN-2006  PIMA_SCAN_SPLIT v1.21 (c) L. Petrov 15-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  N_BAD, UV_BAD(PIM%NUV), IUER
      CHARACTER  STR*128, STR1*128, STR2*128, STR3*128, STR4*128, &
     &           STR_PIMAVAR_AP_TOL*128, STR_PIMAVAR_AUT_NOSOURCE_STRICT*16
      INTEGER*4, ALLOCATABLE :: SCAN_INTRV(:,:,:), OBS_SCA(:,:), AUT_SCA(:,:), &
     &                          UV_STA_IND(:,:,:), UV_BAS_IND(:,:,:), &
     &                          UV_STA_MFRG(:), UV_BAS_MFRG(:), &
     &                          STA_FRG(:,:), BAS_FRG(:,:)
      REAL*8,    ALLOCATABLE :: TIM_ARR(:,:)
      INTEGER*4    MM_BAS
      PARAMETER  ( MM_BAS = (PIM__MSTA+1)**2 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, &
     &           J26, J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, J37, &
     &           J38, J39 ,J40, J41, J42, J43, J44, J45, J46, J47, J48, &
     &           J49, J50, J51, J52, J53, &
     &           K_TIN, K_SCA, K_STA, L_SCA, M_BAS, IND_AP, K1, N_BAD_BEG, &
     &           NEW_NSCA, NSCA_STA(PIM__MSTA), NSCA_BAS(PIM__MBAS), &
     &           NBAS_SCA(PIM__MSCA), NSTA_SCA(PIM__MSCA), &
     &           NUV_STA(PIM__MSTA,PIM__MFRG), NUV_BAS(MM_BAS,PIM__MFRG), &
     &           CUR_SOU, IND_SCA, &
     &           IND_BAS, IND_STA, BAS_ID, IP, LSTA, LBAS, IND_OBS, IND_AUT, &
     &           BAS_IND, IND_AUT_UV, IND_OBS_UV, KSCA, SCA_USE(PIM__MSCA), &
     &           TIM_IND_CURR_BEG, TIM_IND_CURR_END, IND_LAST, &
     &           TIM_IND_PREV_BEG, TIM_IND_PREV_END, AUT_IND, POI_IND, &
     &           TIM_IND_BEG(PIM__MSCA), TIM_IND_END(PIM__MSCA), &
     &           SCA_IND_SOU(PIM__MSCA), TIN_LIS(PIM__MUV), L_TIN, I_TIN, &
     &           STA_IND, SOU_IND, SCA_SOU_IND, L_SCA_OLD, &
     &           GLO_FRG_IND, IND_GLO_FRG_IND, &
     &           LAST_FRG, LAST_TIMIND_FRG, IND_UV, NUM_EXC_OBS, &
     &           NUM_DIR, NUM_REV, STA_INDS(2), NUV_BAS_SAVED, &
     &           UV_TMP_ARR(PIM__MUV), NSCA_OLD, IND_SCA_ARR(PIM__MISO), &
     &           L_SCA_ARR, MAX_NUM_OBS, IER
      INTEGER*1, ALLOCATABLE :: SS_TAB(:,:)
      LOGICAL*1  FL_AUT_NOSOURCE_STRICT
      INTEGER*4  M__BACK
      REAL*8     PIMA__AP_LEN_MIN
      PARAMETER  ( PIMA__AP_LEN_MIN = 0.001D0 ) ! Minimal AP length
      PARAMETER  ( M__BACK = 8 ) ! How many scans we look back in order to
!                                ! try to consolidate related scans?
      REAL*8     TAI, AP_LEN_SCA_MIN, AP_LEN_SCA_MEAN, AP_LEN_SCA_MAX, &
     &           AP_LEN2, AP_LEN_MAX, TIM_DIFF
      LOGICAL*4  FL_FAILURE, FL_NEW_SCAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, MJDSEC_TO_VEX*22, GET_CDATE*19
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_SCA
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_SCA
#endif
      INTEGER*4, EXTERNAL :: ADD_LIS, ILEN, I_LEN, IFIND_PL, MAX_I4
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I9)' ) 'PIMA_SCAN_SPLIT  Initialization NUV= ', PIM%NUV
           CALL FLUSH ( 6 )
      END IF
      CALL GETENVAR ( 'PIMAVAR_AUT_NOSOURCE_STRICT',  STR_PIMAVAR_AUT_NOSOURCE_STRICT )
      IF ( STR_PIMAVAR_AUT_NOSOURCE_STRICT == 'YES' .OR. &
     &     STR_PIMAVAR_AUT_NOSOURCE_STRICT == 'yes'      ) THEN
!
! -------- Sets up strict check of correspondence of source name between cross- and auto-
! -------- correlation. That prevents support of mulitple phase centers
!
           FL_AUT_NOSOURCE_STRICT = .TRUE.
           WRITE ( 6, * ) 'PIMAVAR_AUT_NOSOURCE_STRICT is set'
         ELSE
           FL_AUT_NOSOURCE_STRICT = .FALSE.
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I9)' ) 'PIMA_SCAN_SPLIT  Beginning:     N_BAD= ', N_BAD
           CALL FLUSH ( 6 )
      END IF
!
      N_BAD_BEG = N_BAD
      M_BAS = (PIM%NSTA+1)**2
      ALLOCATE ( SS_TAB(PIM%NUV,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NUV*PIM%NSTA, STR )
           CALL ERR_LOG ( 7611, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory cor array SS_TAB' )
           RETURN
      END IF
      ALLOCATE ( TIM_ARR(PIM__MUV,PIM__MFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM__MUV, STR )
           CALL ERR_LOG ( 7612, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory cor array TIM_ARR' )
           RETURN
      END IF
      TIM_ARR = 0.0D0
!
! --- Create indexes for auto-correlation points.
! --- We consider observations as bad if they do not have corresponding
! --- autocorrelation. We need to determine it now, before splitting
! --- observations into scans, since the splitting algorithm shoud count
! --- only useable observations
!
      DO 410 J1=1,PIM%NUV
         IF ( PIM%UV_IND(J1)%POI_IND .LE. 0 ) GOTO 410
         IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
!
! ----------- This point is the cross-correlation point
!
              PIM%UV_IND(J1)%POI_AUT_IND = 0
              IF ( J1 > 1 ) THEN
!
! ---------------- We exploit the fact that uv points are sorted in their
! ---------------- time index order. So, we look backwards till we find the
! ---------------- corresponding autocorrelation point or reach the previous
! ---------------- time index (or reach the first point)
!
                   DO 420 J2=J1-1,1,-1
                      IF ( PIM%UV_IND(J2)%TIM_IND .NE. PIM%UV_IND(J1)%TIM_IND ) GOTO 820
                      IF ( PIM%UV_IND(J2)%FRG_IND .NE. PIM%UV_IND(J1)%FRG_IND ) GOTO 420
                      IF ( PIM%UV_IND(J2)%POI_IND .LE. 0 ) GOTO 420 ! bypass "bad" point
                      IF ( PIM%UV_IND(J2)%SOU_IND == PIM%UV_IND(J1)%SOU_IND .OR. &
     &                    .NOT. FL_AUT_NOSOURCE_STRICT ) THEN
                           IF ( PIM%UV_IND(J2)%STA_IND(1) == PIM%UV_IND(J1)%STA_IND(1) .AND. &
     &                          PIM%UV_IND(J2)%STA_IND(2) == PIM%UV_IND(J1)%STA_IND(1)       ) THEN
                                PIM%UV_IND(J1)%POI_AUT_IND(1) = J2
                           END IF
                           IF ( PIM%UV_IND(J2)%STA_IND(1) == PIM%UV_IND(J1)%STA_IND(2) .AND. &
     &                          PIM%UV_IND(J2)%STA_IND(2) == PIM%UV_IND(J1)%STA_IND(2)       ) THEN
                                PIM%UV_IND(J1)%POI_AUT_IND(2) = J2
                           END IF
                      END IF
 420               CONTINUE
 820               CONTINUE
              END IF
              IF ( J1 < PIM%NUV ) THEN
!
! ---------------- Alternatively, look forward for search of autocorrelation data
!
                   DO 430 J3=J1+1,PIM%NUV
                      IF ( PIM%UV_IND(J3)%TIM_IND .NE. PIM%UV_IND(J1)%TIM_IND ) GOTO 830
                      IF ( PIM%UV_IND(J3)%FRG_IND .NE. PIM%UV_IND(J1)%FRG_IND ) GOTO 430
                      IF ( PIM%UV_IND(J3)%POI_IND .LE. 0 ) GOTO 430 ! Bypass "bad" point
                      IF ( PIM%UV_IND(J3)%SOU_IND == PIM%UV_IND(J1)%SOU_IND .OR. &
     &                     .NOT. FL_AUT_NOSOURCE_STRICT ) THEN
                           IF ( PIM%UV_IND(J3)%STA_IND(1) == PIM%UV_IND(J1)%STA_IND(1) .AND. &
     &                          PIM%UV_IND(J3)%STA_IND(2) == PIM%UV_IND(J1)%STA_IND(1)       ) THEN
                                PIM%UV_IND(J1)%POI_AUT_IND(1) = J3
                           END IF
                           IF ( PIM%UV_IND(J3)%STA_IND(1) == PIM%UV_IND(J1)%STA_IND(2) .AND. &
     &                          PIM%UV_IND(J3)%STA_IND(2) == PIM%UV_IND(J1)%STA_IND(2)       ) THEN
                                PIM%UV_IND(J1)%POI_AUT_IND(2) = J3
                           END IF
                      END IF
 430               CONTINUE
 830               CONTINUE
              END IF
            ELSE
              PIM%UV_IND(J1)%POI_AUT_IND(1) = J1
              PIM%UV_IND(J1)%POI_AUT_IND(2) = J1
         END IF
 410  CONTINUE
!
! --- Get prelimiary scan splitting.
!
      L_SCA = 0
!
! --- We scan all sources
!
      DO 440 J4=1,PIM%NSOU
         CALL NOUT ( PIM%NUV*PIM%NSTA, SS_TAB )
         IF ( PIM%SOU(J4)%IVS_NAME == 'NOFRINGE' ) GOTO 440
!
! ------ Scan all UV points and put 1 in the array SS_TAB if a station
! ------ observed the J4-th source
!
         DO 450 J5=1,PIM%NUV
            IF ( PIM%UV_IND(J5)%SOU_IND == J4 ) THEN
                 SS_TAB(J5,PIM%UV_IND(J5)%STA_IND(1)) = 1
                 SS_TAB(J5,PIM%UV_IND(J5)%STA_IND(2)) = 1
            END IF
 450     CONTINUE
         LAST_FRG = 0
         LAST_TIMIND_FRG = 0
         DO 460 J6=1,PIM%NUV
            IF ( PIM%UV_IND(J6)%POI_IND        .LE. 0 .OR. &
     &           PIM%UV_IND(J6)%POI_AUT_IND(1) .LE. 0 .OR. &
     &           PIM%UV_IND(J6)%POI_AUT_IND(2) .LE. 0      ) GOTO 460 ! Bypass bad points
!
! --------- Count how many stations observed this source
!
            K_STA = 0
            DO 470 J7=1,PIM%NSTA
               IF ( SS_TAB(J6,J7) == 1 ) K_STA = K_STA + 1
 470        CONTINUE
!
            IF ( K_STA > 1 ) THEN
!
! -------------- At least 2 stations. Good!
!
                 IF ( L_SCA == 0  ) THEN
!
! ------------------- No scans? Create the first preliminary scan
!
                      L_SCA = 1
                      TIM_IND_BEG(L_SCA) = PIM%UV_IND(J6)%TIM_IND
                      TIM_IND_END(L_SCA) = PIM%UV_IND(J6)%TIM_IND
                      SCA_IND_SOU(L_SCA) = J4
                      AP_LEN_SCA_MIN  = 0.0D0
                      AP_LEN_SCA_MEAN = 0.0D0
                      AP_LEN_SCA_MAX  = 0.0D0
                      L_TIN = 1
                      TIN_LIS(L_TIN) = PIM%UV_IND(J6)%TIM_IND
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                           WRITE ( 6, 210 ) PIM%C_SOU(J4), &
     &                                      TIM_IND_BEG(L_SCA), &
     &                                      TIM_IND_END(L_SCA), &
     &                                      PIM%UV_IND(J6)%TIM_IND
 210                       FORMAT ( 'PIMA_SCAN_SPLIT FIRST Scan ', 25X, &
     &                              ' sou: ', A, &
     &                              ' TIM_IND_BEG: ', I7, &
     &                              ' TIM_IND_END: ', I7, &
     &                              ' UV_IND_TIM_IND: ', I7 )
                      END IF
                      LAST_FRG = PIM%UV_IND(J6)%FRG_IND
                      LAST_TIMIND_FRG = PIM%UV_IND(J6)%TIM_IND
                      GOTO 460
                 END IF
!
! -------------- Check whether the time tag of this point is an adjacent
! -------------- to the time tag of the last point of the J6-th scan
!
                 FL_NEW_SCAN = .TRUE.
                 IF ( PIM%UV_IND(J6)%SOU_IND == SCA_IND_SOU(L_SCA)  .AND.     &
     &                ( PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                  PIM%TIM_R8(TIM_IND_END(L_SCA)) ) < PIM%CONF%MAX_SCAN_GAP ) THEN
!
! ------------------- Set prelimiary status "the same scan", but make several
! ------------------- important checks
!
                      FL_NEW_SCAN = .FALSE.
                      IF ( L_TIN > 2 ) THEN
!
! ------------------------ Let us check, whether the time interval between
! ------------------------ this point and the previous point is longer than
! ------------------------ the maximum scan gap
!
                           IF ( ( PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - PIM%TIM_R8(TIN_LIS(L_TIN)) ) > &
     &                          PIM%CONF%MAX_SCAN_GAP ) THEN
!
! ----------------------------- Yes? This mean a new scan!
!
                                FL_NEW_SCAN = .TRUE.
                                IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                     WRITE ( 6, * ) 'Long interval check: ', &
     &                                              ' T1= ', PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND), &
     &                                              ' T2= ', PIM%TIM_R8(TIN_LIS(L_TIN))
                                END IF
                           END IF
!
! ------------------------ Let us check, whether the time interval between
! ------------------------ this point and the first point of the scan is a multiple
! ------------------------ of the AP length which is defined as the minimum
! ------------------------ time difference between two adjacent time epochs
!
                           AP_LEN_SCA_MIN  = PIM%TIM_R8(TIN_LIS(L_TIN)) - PIM%TIM_R8(TIN_LIS(1))
                           DO 4130 J13=2,L_TIN
                              IF ( ( PIM%TIM_R8(TIN_LIS(J13)) - PIM%TIM_R8(TIN_LIS(J13-1)) ) > PIM%CONF%AP_TOLERANCE ) THEN
                                   AP_LEN_SCA_MIN = MIN ( AP_LEN_SCA_MIN, &
     &                                                    PIM%TIM_R8(TIN_LIS(J13)) - PIM%TIM_R8(TIN_LIS(J13-1)) )
                              END IF
 4130                      CONTINUE
                           AP_LEN_SCA_MEAN = 0.0D0
                           IF ( AP_LEN_SCA_MIN < PIM%CONF%AP_TOLERANCE ) AP_LEN_SCA_MIN = PIM%CONF%AP_TOLERANCE 
                           AP_LEN_SCA_MAX  = AP_LEN_SCA_MIN
                           K_TIN = 0
                           DO 5140 J14=2,L_TIN
                              IF ( ( PIM%TIM_R8(TIN_LIS(J14)) - PIM%TIM_R8(TIN_LIS(J14-1)) - AP_LEN_SCA_MIN ) < &
     &                             PIM%CONF%AP_TOLERANCE*PIM%CONF%MAX_SCAN_LEN/AP_LEN_SCA_MIN ) THEN
                                   AP_LEN_SCA_MAX = MAX ( AP_LEN_SCA_MAX, &
     &                                                    PIM%TIM_R8(TIN_LIS(J14)) - PIM%TIM_R8(TIN_LIS(J14-1)) )
                                   AP_LEN_SCA_MEAN = AP_LEN_SCA_MEAN + PIM%TIM_R8(TIN_LIS(J14)) - PIM%TIM_R8(TIN_LIS(J14-1))
                                   K_TIN = K_TIN + 1
                              END IF
 5140                      CONTINUE
                           AP_LEN_SCA_MEAN = AP_LEN_SCA_MEAN/K_TIN
                           IF ( AP_LEN_SCA_MEAN > 1.D-10 ) THEN
                                IND_AP = IDNINT ( (PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                                             PIM%TIM_R8(TIN_LIS(1)))/AP_LEN_SCA_MEAN )
                                TIM_DIFF = PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                                     PIM%TIM_R8(TIN_LIS(1)) - IND_AP*AP_LEN_SCA_MEAN
                                IF ( DABS(TIM_DIFF) > PIM%CONF%AP_TOLERANCE .AND. &
     &                               DABS(TIM_DIFF) > IND_AP*(AP_LEN_SCA_MAX - AP_LEN_SCA_MIN) ) THEN
!
! ---------------------------------- Yes? This means a new scan!
!
                                     FL_NEW_SCAN = .TRUE.
                                     IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                          WRITE ( 6, * ) 'Commensurate AP check: ', &
     &                                                   ' TT= ', PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND), &
     &                                                   ' TIN_LIS(1)= ', PIM%TIM_R8(TIN_LIS(1)), &
     &                                                   ' AP_MEAN= ', AP_LEN_SCA_MEAN, &
     &                                                   ' IND_AP= ', IND_AP, &
     &                                                   ' DIF= ', PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                                                             PIM%TIM_R8(TIN_LIS(1)) - IND_AP*AP_LEN_SCA_MEAN, &
     &                                                   ' PIM%CONF%AP_TOLERANCE= ', PIM%CONF%AP_TOLERANCE
                                    END IF
                                END IF
                           END IF
                      END IF
!
! ------------------- Check how long the scan with including this point will be
!
                      IF ( PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                     PIM%TIM_R8(TIM_IND_BEG(L_SCA)) > PIM%CONF%MAX_SCAN_LEN  ) THEN
!
! ------------------------ This preliminary scan is already reached the limit
! ------------------------ of scan length. Start a new scan!
!
                           FL_NEW_SCAN = .TRUE.
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                WRITE ( 6, * ) 'Reached the limit of scan length: '
                                WRITE ( 6, * ) ' ind_tb= ', TIM_IND_BEG(L_SCA), ' tb= ', PIM%TIM_R8(TIM_IND_BEG(L_SCA)), &
     &                                         ' ind_te= ', PIM%UV_IND(J6)%TIM_IND, ' te= ', pim%tim_r8(pim%uv_ind(j6)%tim_ind), &
     &                                         ' tim_dif: ', &
     &                                         pim%tim_r8(pim%uv_ind(j6)%tim_ind) - pim%tim_r8(tim_ind_beg(l_sca))
                           END IF
                      END IF
!
                      IF ( PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                     PIM%TIM_R8(TIM_IND_END(L_SCA)) > PIM%CONF%MAX_SCAN_LEN  ) THEN
!
! ------------------------ The gap between the current point and the last point
! ------------------------ of the J6-th scan is greater than the limit
!
                           FL_NEW_SCAN = .TRUE.
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                WRITE ( 6, * ) 'The gap between points  is greater than the limit'
                           END IF
                      END IF
!
                      IF ( PIM%UV_IND(J6)%FRG_IND .NE. LAST_FRG  .AND. &
     &                     (PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - PIM%TIM_R8(LAST_TIMIND_FRG)) > &
     &                      PIM%CONF%MAX_SCAN_GAP ) THEN
!
! ------------------------ The frequency group changed with respect to
! ------------------------ the previous AP, but the frquency group stood
! ------------------------ the same for longer than PIM%CONF%MAX_SCAN_LEN .
! ------------------------ We consider this situation as frequency change,
! ------------------------ which triggers scan change
!
                           FL_NEW_SCAN = .TRUE.
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                WRITE ( 6, * ) 'The frequency group changed '// &
     &                                         'wrt to the previous AP'
                           END IF
                           LAST_FRG = PIM%UV_IND(J6)%FRG_IND
                           LAST_TIMIND_FRG = PIM%UV_IND(J6)%TIM_IND
!
                           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND), -2 )
                      END IF
                    ELSE
                 END IF
!
                 IF ( LAST_FRG == 0 ) THEN
                      LAST_FRG = PIM%UV_IND(J6)%FRG_IND
                      LAST_TIMIND_FRG = PIM%UV_IND(J6)%TIM_IND
                 END IF
                 IF ( PIM%UV_IND(J6)%FRG_IND .NE. LAST_FRG ) THEN
                      IF ( PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND) - &
     &                     PIM%TIM_R8(LAST_TIMIND_FRG) > PIM%CONF%MAX_SCAN_GAP ) THEN
                           FL_NEW_SCAN = .TRUE.
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                WRITE ( 6, * ) 'Change of the freq group and ' // &
     &                                         'the gap is greater than the limit'
                           END IF
                      END IF
                      LAST_FRG = PIM%UV_IND(J6)%FRG_IND
                      LAST_TIMIND_FRG = PIM%UV_IND(J6)%TIM_IND
                 END IF
!
                 IF ( FL_NEW_SCAN ) THEN
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                           WRITE ( 6, 220 ) L_SCA+1, &
     &                                      PIM%C_SOU(SCA_IND_SOU(L_SCA)), &
     &                                      PIM%C_SOU(PIM%UV_IND(J6)%SOU_IND), &
     &                                      TIM_IND_BEG(L_SCA), &
     &                                      TIM_IND_END(L_SCA), &
     &                                      PIM%UV_IND(J6)%TIM_IND, L_TIN
 220                       FORMAT ( 'PIMA_SCAN_SPLIT NEW Scan: ',I4, &
     &                              ' old_sou: ', A, ' new_sou: ', A, &
     &                              ' TIM_IND_BEG: ', I7, &
     &                              ' TIM_IND_END: ', I7, &
     &                              ' UV_IND_TIM_IND: ', I7,' L_TIN: ', I7 )
                      END IF
                      L_SCA = L_SCA + 1
                      IF ( L_SCA > PIM__MSCA ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( PIM__MSCA, STR )
                           CALL ERR_LOG ( 7613, IUER, 'PIM_SCAN_SPLIT', &
     &                         'Trap of internal control: too many '// &
     &                         'preliminary scans: more than '//STR )
                           RETURN
                      END IF
                      AP_LEN2 = AP_LEN_SCA_MEAN
                      AP_LEN_SCA_MEAN = 0.0D0
                      TIM_IND_BEG(L_SCA) = PIM%UV_IND(J6)%TIM_IND
                      TIM_IND_END(L_SCA) = PIM%UV_IND(J6)%TIM_IND
                      SCA_IND_SOU(L_SCA) = J4
                      L_TIN = 1
                      TIN_LIS(L_TIN) = PIM%UV_IND(J6)%TIM_IND
                    ELSE
!
! ------------------- Continue the J6-th scan
!
                      TIM_IND_END(L_SCA) = PIM%UV_IND(J6)%TIM_IND
                      IER = 0
                      I_TIN = ADD_LIS ( PIM__MUV, L_TIN, TIN_LIS, &
     &                               PIM%UV_IND(J6)%TIM_IND, IER )
                      IF ( IER > 0 ) THEN
                           CALL CLRCH ( STR )
                           STR(1:22)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                  PIM%TIM_R8(TIM_IND_BEG(L_SCA)), -2 )
                           STR(31:52) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                  PIM%TIM_R8(PIM%UV_IND(J6)%TIM_IND), -2 )
                           CALL INCH  ( PIM__MUV, STR(61:72) )
                           CALL INCH  ( L_SCA, STR(81:92) )
                           CALL ERR_LOG ( 7614, IUER, 'PIMA_SCAN_SPLIT', &
     &                         'trap of internal control: too many AP in '// &
     &                         'a scan of source '//PIM%C_SOU(J4)// &
     &                         ' at the interval of time [ '//STR(1:22)// &
     &                         ' , '//STR(31:52)//' ] -- more than '// &
     &                         'PIM__MUV='//STR(61:I_LEN(STR(61:72))+60)// &
     &                         ' for scan # '//STR(81:I_LEN(STR(81:92))+80) )
                          RETURN
                      END IF
                 END IF
            END IF
 460     CONTINUE
!
! ------ Scan consolidation: try to glue adjacent scans of the same source,
! ------ if it is possible
!
         L_SCA_OLD = L_SCA
         DO 480 J8=2,L_SCA_OLD
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 WRITE ( 6, 230  ) J8, PIM%C_SOU(SCA_IND_SOU(J8)), &
     &                                 PIM%C_SOU(SCA_IND_SOU(J8-1)), &
     &                          TIM_IND_BEG(J8-1), TIM_IND_END(J8-1), &
     &                          TIM_IND_BEG(J8), TIM_IND_END(J8), &
     &                          PIM%TIM_R8(TIM_IND_BEG(J8)) - &
     &                          PIM%TIM_R8(TIM_IND_END(J8-1)), &
     &                          PIM%TIM_R8(TIM_IND_END(J8)) - &
     &                          PIM%TIM_R8(TIM_IND_BEG(J8-1))
 230             FORMAT ( 'Sca_ind: ', i4, ' sou: ', a, 2x, a, &
     &                   ' tim_ind_prev= ', i6, 1x, i6, &
     &                   ' tim_end_curr= ', i6, 1x, i6/ &
     &                   14x,'diff1= ', f15.7, ' diff2= ', f15.7 )
            END IF
!
            IF ( J8 > L_SCA ) GOTO 480
!
! --------- Check whether AP_LEN are the same
!
            IF ( AP_LEN_SCA_MEAN > 0.0D0 .AND. AP_LEN2 > 0.0D0 .AND. &
     &           DABS(AP_LEN_SCA_MEAN - AP_LEN2) > PIM%CONF%AP_TOLERANCE ) GOTO 480
!
! --------- Check whether the interval between two adjacents scans that
! --------- are candidate for consolidation, is commensurate to the AP_LEN
! --------- or not. NB: if the scan has less than 2 AP, ap_len is zero.
!
            IF ( AP_LEN_SCA_MEAN > PIM%CONF%AP_TOLERANCE ) THEN
                 IND_AP = IDNINT ( (PIM%TIM_R8(TIM_IND_BEG(J8)) - &
     &                              PIM%TIM_R8(TIM_IND_END(J8-1)))/AP_LEN_SCA_MEAN )
                 IF ( DABS(PIM%TIM_R8(TIM_IND_BEG(J8)) - PIM%TIM_R8(TIM_IND_END(J8-1)) - &
     &                     AP_LEN_SCA_MEAN*IND_AP) > PIM%CONF%AP_TOLERANCE ) GOTO 480
            END IF
            IF ( AP_LEN2 > PIM%CONF%AP_TOLERANCE ) THEN
                 IND_AP = IDNINT ( (PIM%TIM_R8(TIM_IND_BEG(J8)) - &
     &                              PIM%TIM_R8(TIM_IND_END(J8-1)))/AP_LEN2 )
                 IF ( DABS(PIM%TIM_R8(TIM_IND_BEG(J8)) - PIM%TIM_R8(TIM_IND_END(J8-1)) - &
     &                     AP_LEN2*IND_AP) > PIM%CONF%AP_TOLERANCE ) GOTO 480
            END IF
!
! --------- It is possible if the gap between scans is less than the limit
! --------- and the length of the consolidated scans is less than the limit
!
            IF ( ( SCA_IND_SOU(J8) == SCA_IND_SOU(J8-1) )                    .AND. &
     &           (   PIM%TIM_R8(TIM_IND_BEG(J8)) &
     &             - PIM%TIM_R8(TIM_IND_END(J8-1)) < PIM%CONF%MAX_SCAN_GAP ) .AND. &
     &           (   PIM%TIM_R8(TIM_IND_END(J8)) &
     &             - PIM%TIM_R8(TIM_IND_BEG(J8-1)) < PIM%CONF%MAX_SCAN_LEN + &
     &                                               PIM%CONF%MIN_SCAN_LEN ) ) THEN
!
! --------- OK. We glue the J8-1 -th and J8 -th scans togeather
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 WRITE ( 6, 240 ) PIM%C_SOU(SCA_IND_SOU(J8)), &
     &                                 PIM%TIM_R8(TIM_IND_BEG(J8-1)), &
     &                                 PIM%TIM_R8(TIM_IND_END(J8-1)), &
     &                                 PIM%TIM_R8(TIM_IND_BEG(J8)),   &
     &                                 PIM%TIM_R8(TIM_IND_END(J8)),   &
     &                                 TIM_IND_BEG(J8-1), TIM_IND_END(J8-1), &
     &                                 TIM_IND_BEG(J8), TIM_IND_END(J8)
 240             FORMAT ( 'PIMA_SCAN_SPLIT Scan consolidation: ',A, &
     &                              ' TIM_BEG_LAST: ', F16.6, &
     &                              ' TIM_END_LAST: ', F16.6, &
     &                              ' TIM_BEG_THIS: ', F16.6, &
     &                              ' TIM_END_THIS: ', F16.6, &
     &                              ' IND_BEG_LAST: ', I7, &
     &                              ' IND_END_LAST: ', I7, &
     &                              ' IND_BEG_THIS: ', I7, &
     &                              ' IND_END_THIS: ', I7  )
                 END IF
                 TIM_IND_END(J8-1) = TIM_IND_END(J8)
                 IF ( J8 < L_SCA ) THEN
                      DO 490 J9=J8,L_SCA-1
                         TIM_IND_BEG(J9) = TIM_IND_BEG(J9+1)
                         TIM_IND_END(J9) = TIM_IND_END(J9+1)
                         SCA_IND_SOU(J9) = SCA_IND_SOU(J9+1)
 490                  CONTINUE
                 END IF
                 L_SCA = L_SCA - 1
            END IF
 480     CONTINUE
!
! ------ Remove too short scans
!
         L_SCA_OLD = L_SCA
         DO 4100 J10=1,L_SCA_OLD
            IF ( PIM%TIM_R8(TIM_IND_END(J10)) - PIM%TIM_R8(TIM_IND_BEG(J10)) < &
     &           PIM%CONF%MIN_SCAN_LEN ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                      WRITE ( 6, 250 ) PIM%C_SOU(SCA_IND_SOU(J10)), &
     &                                 TIM_IND_BEG(J10), TIM_IND_END(J10)
 250                  FORMAT ( 'PIMA_SCAN_SPLIT Scan removal for source ',A, &
     &                              ' TIM_IND_BEG: ', I7, &
     &                              ' TIM_IND_END: ', I7  )
                 END IF
                 DO 4110 J11=1,PIM%NUV
!
! ----------------- Bypass the bad point
!
                    IF ( PIM%UV_IND(J11)%POI_IND .LE. 0 ) GOTO 4110
!
! ----------------- Bypass the autocorrelation point
!
                    IF ( PIM%UV_IND(J11)%STA_IND(1) == PIM%UV_IND(J11)%STA_IND(2) ) GOTO 4110
!
                    IF ( PIM%UV_IND(J11)%SOU_IND .EQ. SCA_IND_SOU(J10) .AND. &
     &                   PIM%UV_IND(J11)%TIM_IND .GE. TIM_IND_BEG(J10) .AND. &
     &                   PIM%UV_IND(J11)%TIM_IND .LE. TIM_IND_END(J10)       ) THEN
!
                         N_BAD = N_BAD + 1
                         UV_BAD(N_BAD) = J11
!
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                              WRITE ( 6, 260 ) PIM%C_SOU(SCA_IND_SOU(J10)), &
     &                                         TIM_IND_BEG(J10), TIM_IND_END(J10), &
     &                                         J11, PIM%UV_IND(J11)%ORIG_IND, &
     &                                         PIM%UV_IND(J11)%POI_IND
 260                          FORMAT ( 'PIMA_SCAN_SPLIT UV removal for source ',A, &
     &                              ' TIM_IND_BEG: ', I7, &
     &                              ' TIM_IND_END: ', I7, &
     &                              ' UV: ', I9, ' UV_orig: ', I9, &
     &                              ' POI_IND: ', I9 )
                         END IF
                         PIM%UV_IND(J11)%POI_IND = PIMA__SHORT
                    END IF
 4110            CONTINUE
                 IF ( J10 < L_SCA ) THEN
                      DO 4120 J12=J10,L_SCA-1
                         TIM_IND_BEG(J12) = TIM_IND_BEG(J12+1)
                         TIM_IND_END(J12) = TIM_IND_END(J12+1)
                         SCA_IND_SOU(J12) = SCA_IND_SOU(J12+1)
 4120                 CONTINUE
                 END IF
                 L_SCA = L_SCA - 1
            END IF
 4100    CONTINUE
 440  CONTINUE
!
! --- Put information about scans into PIM%SCA
!
      PIM%NSCA = L_SCA
      DO 4140 J14=1,L_SCA
         PIM%SCA(J14)%TIM_IND = TIM_IND_BEG(J14)
         PIM%SCA(J14)%NUM_EPC = TIM_IND_END(J14) - TIM_IND_BEG(J14) + 1
         PIM%SCA(J14)%SOU_IND = SCA_IND_SOU(J14)
 4140 CONTINUE
!
! --- Check whether any of the scans had in-beam multpiple source(s)
! --- If yeas, then we split each such scan
!
      NSCA_OLD = PIM%NSCA
      DO 4150 J15=1,NSCA_OLD
         PIM%SCA(J15)%IND_ROOT = 0
         PIM%SCA(J15)%UNSRT_ID = J15  ! Scan index in the unsorted scan list
         IF ( PIM%SOU(PIM%SCA(J15)%SOU_IND)%NISO > 0 ) THEN
              DO 4160 J16=1,PIM%SOU(PIM%SCA(J15)%SOU_IND)%NISO
!
! -------------- Add the copy of the scan to the end and change its source name
!
                 PIM%NSCA = PIM%NSCA + 1
                 PIM%SCA(PIM%NSCA) = PIM%SCA(J15)
                 PIM%SCA(PIM%NSCA)%IND_ROOT = J15  ! Index of the of the root scan
                 PIM%SCA(PIM%NSCA)%SOU_IND  = PIM%SOU(PIM%SCA(J15)%SOU_IND)%ISO_IND(J16)
                 PIM%SCA(PIM%NSCA)%UNSRT_ID = PIM%NSCA
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, * ) 'PIMA_SCAN_SPLIT PIM%SCA(J15)%SOU_IND= ', PIM%SCA(J15)%SOU_IND ; CALL FLUSH ( 6 )
                      WRITE ( 6, * ) 'PIMA_SCAN_SPLIT PIM%SOU(PIM%SCA(J15)%SOU_IND)%ISO_IND(J16) = ', PIM%SOU(PIM%SCA(J15)%SOU_IND)%ISO_IND(J16) ; CALL FLUSH ( 6 )
                      WRITE ( 6, * ) 'PIMA_SCAN_SPLIT DOUBLE SOURCE: ', &
     &                                PIM%SOU(PIM%SCA(PIM%NSCA)%SOU_IND)%IVS_NAME, &
     &                               ' orig_name: ', PIM%SOU(PIM%SCA(J15)%SOU_IND)%IVS_NAME, &
     &                               ' OLD_SOU= ', PIM%C_SOU(PIM%SCA(J15)%SOU_IND), &
     &                               ' NEW_SOU= ', PIM%C_SOU(PIM%SOU(PIM%SCA(J15)%SOU_IND)%ISO_IND(J16))
                 END IF
 4160         CONTINUE
         END IF
 4150 CONTINUE
!
! --- Sort scans according to time epoch of the first uv datum
!
      CALL FOR_QSORT ( PIM%SCA, PIM%NSCA, SIZEOF(PIM%SCA(1)), PIMA_COMPAR_SCA )
!
! --- Set scan names
!
      DO 4170 J17=1,PIM%NSCA
         IF ( PIM%SCA(J17)%IND_ROOT > 0 ) THEN
              DO 4180 J18=1,PIM%NSCA
                 IF ( PIM%SCA(J17)%IND_ROOT == PIM%SCA(J18)%UNSRT_ID ) THEN
                      PIM%SCA(J17)%IND_ROOT = J18
                      GOTO 8180
                 END IF
 4180         CONTINUE
 8180         CONTINUE
         END IF
!
         IER = -1
         STR = MJDSEC_TO_VEX ( PIM%MJD_0, PIM%TAI_0 + &
     &                         PIM%TIM_R8(PIM%SCA(J17)%TIM_IND), -3 )
         PIM%SCA(J17)%SCAN_NAME = STR(6:8)//'-'//STR(10:11)//STR(13:14)//'  '
         IF ( J17 > 1 ) THEN
              IF ( PIM%SCA(J17)%SCAN_NAME(1:8) == PIM%SCA(J17-1)%SCAN_NAME(1:8) ) THEN
                   IP = ICHAR(PIM%SCA(J17-1)%SCAN_NAME(9:9)) - 97
                   IF ( IP < 0  ) IP = 0
                   IF ( IP == 0 ) PIM%SCA(J17-1)%SCAN_NAME(9:9) = 'a'
                   IP = IP + 1
                   PIM%SCA(J17)%SCAN_NAME(9:9) = CHAR(IP+97)
              END IF
         END IF
!
         IF ( J17 > 1 ) THEN
              TIM_IND_CURR_BEG = PIM%SCA(J17)%TIM_IND
              TIM_IND_CURR_END = PIM%SCA(J17)%TIM_IND + PIM%SCA(J17)%NUM_EPC - 1
              TIM_IND_PREV_BEG = PIM%SCA(J17-1)%TIM_IND
              TIM_IND_PREV_END = PIM%SCA(J17-1)%TIM_IND + PIM%SCA(J17-1)%NUM_EPC - 1
              IF ( TIM_IND_CURR_BEG .LE. TIM_IND_PREV_END           .AND. &
     &             PIM%SCA(J17)%SOU_IND .EQ. PIM%SCA(J17-1)%SOU_IND       ) THEN
!
                   STR(1:22)   = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND_PREV_BEG), -3 )
                   STR(31:52)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND_PREV_END), -3 )
                   STR(61:82)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND_CURR_BEG), -3 )
                   STR(91:112) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND_CURR_END), -3 )
                   WRITE ( 6, * ) 'Source_cur  : ', PIM%SOU(PIM%SCA(J17)%SOU_IND)%IVS_NAME
                   WRITE ( 6, * ) 'Source_prev : ', PIM%SOU(PIM%SCA(J17-1)%SOU_IND)%IVS_NAME
                   WRITE ( 6, * ) 'Scan ',J17-1,' Time range: ', &
     &                             TIM_IND_PREV_BEG, TIM_IND_PREV_END, &
     &                             '                     ', STR(1:22), ' ', STR(31:52)
                   WRITE ( 6, * ) 'Scan ',J17,' Time range: ', &
     &                             TIM_IND_CURR_BEG, TIM_IND_CURR_END, &
     &                             '                     ', STR(61:82), ' ', STR(91:112)
                   CALL ERR_LOG ( 7615, IUER, 'PIMA_SCAN_SPLIT', 'Failure '// &
     &                 'in the alogrithm for scan splitting: two overlapping '// &
     &                 'scans were detected' )
                   RETURN
              END IF
         END IF
 4170 CONTINUE
!
      ALLOCATE ( OBS_SCA(MM_BAS,PIM%NSCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MM_BAS*PIM%NSCA, STR )
           CALL ERR_LOG ( 7616, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      ALLOCATE ( AUT_SCA(PIM__MSTA,PIM%NSCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MSTA*PIM%NSCA, STR )
           CALL ERR_LOG ( 7617, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      CALL NOUT_I4 ( MM_BAS*PIM%NSCA,     OBS_SCA )
      CALL NOUT_I4 ( PIM__MSTA*PIM%NSCA,  AUT_SCA )
      CALL NOUT_I4 ( PIM__MSCA, NSTA_SCA )
      CALL NOUT_I4 ( PIM__MSCA, NBAS_SCA )
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_SCAN_SPLIT  Search for UV indexes'
      END IF
      FL_FAILURE = .FALSE.
      DO 4190 J19=1,PIM%NUV
         IF ( PIM%UV_IND(J19)%POI_IND        .LE. 0 .OR. &
     &        PIM%UV_IND(J19)%POI_AUT_IND(1) .LE. 0 .OR. &
     &        PIM%UV_IND(J19)%POI_AUT_IND(2) .LE. 0      ) GOTO 4190 ! Bypass bad points
!
! ------ Find scan index for the J19-th UV point
!
         L_SCA_ARR = 0
         DO 4200 J20=1,PIM%NSCA
            IF ( PIM%UV_IND(J19)%TIM_IND  .GE. PIM%SCA(J20)%TIM_IND     .AND. &
     &           PIM%UV_IND(J19)%TIM_IND  .LE. PIM%SCA(J20)%TIM_IND +         &
     &                                         PIM%SCA(J20)%NUM_EPC - 1 .AND. &
     &           PIM%UV_IND(J19)%SOU_IND  .EQ. PIM%SCA(J20)%SOU_IND     .AND. &
     &           PIM%SCA(J20)%IND_ROOT    .EQ. 0                              ) THEN
!
                 IF ( L_SCA_ARR > 0 ) THEN
                      CALL CLRCH (     STR )
                      CALL INCH  ( J19, STR )
                      IER = -1
                      CALL ERR_LOG ( 7618, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                    'of internal control: UV point '//STR(1:I_LEN(STR))// &
     &                    ' is in more than one scan' )
                      STR(1:22)   = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                  PIM%TIM_R8(PIM%SCA(J20)%TIM_IND), -3 )
                      STR(31:52)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                  PIM%TIM_R8(PIM%SCA(J20)%TIM_IND+PIM%SCA(J20)%NUM_EPC-1), -3 )
                      WRITE ( 6, * ) 'Source index: ',PIM%SCA(J20)%SOU_IND, &
     &                               ' Time index: ', PIM%UV_IND(J19)%TIM_IND
                      WRITE ( 6, * ) 'Current scan range: ',   &
     &                                PIM%SCA(J20)%TIM_IND,     &
     &                                PIM%SCA(J20)%TIM_IND +    &
     &                                PIM%SCA(J20)%NUM_EPC - 1, &
     &                                ' Scan index: ',J20, &
     &                                ' Sou: ', PIM%SOU(PIM%SCA(J20)%SOU_IND)%IVS_NAME, &
     &                                ' Time range: [ ', STR(1:22), ', ', STR(31:52), ' ]'
                      WRITE ( 6, * ) 'Old scan range:     ',        &
     &                                PIM%SCA(IND_SCA_ARR(1))%TIM_IND,     &
     &                                PIM%SCA(IND_SCA_ARR(1))%TIM_IND +    &
     &                                PIM%SCA(IND_SCA_ARR(1))%NUM_EPC - 1, &
     &                                ' Scan index: ', IND_SCA_ARR(1), &
     &                                ' Source name: ', PIM%SOU(PIM%SCA(IND_SCA_ARR(1))%SOU_IND)%IVS_NAME
                      FL_FAILURE = .TRUE.
                      GOTO 4190
                 END IF
                 L_SCA_ARR = 1
                 IND_SCA_ARR(L_SCA_ARR) = J20
               ELSE IF ( PIM%UV_IND(J19)%TIM_IND  .GE. PIM%SCA(J20)%TIM_IND     .AND. &
     &                   PIM%UV_IND(J19)%TIM_IND  .LE. PIM%SCA(J20)%TIM_IND +         &
     &                                                 PIM%SCA(J20)%NUM_EPC - 1 .AND. &
     &                   PIM%SCA(J20)%IND_ROOT    >    0                              ) THEN
                 IF ( PIM%UV_IND(J19)%SOU_IND == PIM%SCA(PIM%SCA(J20)%IND_ROOT)%SOU_IND ) THEN
                      L_SCA_ARR = L_SCA_ARR + 1
                      IND_SCA_ARR(L_SCA_ARR) = J20
                 END IF
            END IF
 4200    CONTINUE
!
         IF ( L_SCA_ARR == 0 ) THEN
              IF ( PIM%UV_IND(J19)%STA_IND(1) == PIM%UV_IND(J19)%STA_IND(2) ) THEN
!
! ---------------- It is considered as normal if autocorrelation UV point
! ---------------- does not belong to any scan. We ignore such a point
!
                   GOTO  4190
                 ELSE
!
! ---------------- Check, whether the point is in the list of bad points
!
                   IF ( IFIND_PL ( N_BAD, UV_BAD, J19) .LE. 0 ) THEN
                        FL_FAILURE = .TRUE.
                        CALL CLRCH (     STR )
                        CALL INCH  ( PIM%UV_IND(J19)%ORIG_IND, STR )
                        CALL ERR_PASS ( IUER, IER )
                        CALL ERR_LOG ( 7619, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                      'of internal control: UV point '//STR(1:I_LEN(STR))// &
     &                      ' (orig) was not marked as belonging to any scan' )
                        FL_FAILURE = .TRUE.
                        PIM%UV_IND(J19)%POI_IND = PIMA__NO_SCA
                        N_BAD = N_BAD + 1
                        UV_BAD(N_BAD) = J19
                        IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                             WRITE ( 6, 270 ) PIM%C_SOU(PIM%UV_IND(J19)%SOU_IND), &
     &                                        PIM%C_STA(PIM%UV_IND(J19)%STA_IND(1)), &
     &                                        PIM%C_STA(PIM%UV_IND(J19)%STA_IND(2)),    &
     &                                        J19, PIM%UV_IND(J19)%ORIG_IND, &
     &                                        ' Not belong to a scan', 0
 270                         FORMAT ( 'PIMA_SCAN_SPLIT UV removal for source ',A, &
     &                                ' Stations: ', A, 1X, A, &
     &                                ' UV: ', I9, ' UV_orig: ', I9, &
     &                                2X, A, 1X, I1 )
                        END IF
                   END IF
                   GOTO 4190
             END IF
         END IF
!
         DO 4210 J21=1,L_SCA_ARR
             IF ( PIM%UV_IND(J19)%STA_IND(1) == PIM%UV_IND(J19)%STA_IND(2) ) THEN
                  IF ( AUT_SCA(PIM%UV_IND(J19)%STA_IND(1),IND_SCA_ARR(J21)) == 0 ) THEN
                       PIM%NAUT = PIM%NAUT + 1
                       NSTA_SCA(IND_SCA_ARR(J21)) = NSTA_SCA(IND_SCA_ARR(J21)) + 1
                  END IF
                  AUT_SCA(PIM%UV_IND(J19)%STA_IND(1),IND_SCA_ARR(J21)) = &
     &                    AUT_SCA(PIM%UV_IND(J19)%STA_IND(1),IND_SCA_ARR(J21)) + 1
               ELSE
                  STA_INDS(1) = MIN ( PIM%UV_IND(J19)%STA_IND(1), &
     &                                PIM%UV_IND(J19)%STA_IND(2)  )
                  STA_INDS(2) = MAX ( PIM%UV_IND(J19)%STA_IND(1), &
     &                                PIM%UV_IND(J19)%STA_IND(2)  )
                  IND_BAS = STA_INDS(1)*PIM__MSTA + STA_INDS(2)
                  IF ( OBS_SCA(IND_BAS,IND_SCA_ARR(J21)) == 0 ) THEN
                       PIM%NOBS = PIM%NOBS + 1
                       NBAS_SCA(IND_SCA_ARR(J21)) = NBAS_SCA(IND_SCA_ARR(J21)) + 1
                  END IF
                  OBS_SCA(IND_BAS,IND_SCA_ARR(J21)) = OBS_SCA(IND_BAS,IND_SCA_ARR(J21)) + 1
            END IF
 4210    CONTINUE
 4190 CONTINUE
!
      IF ( PIM%CONF%CHECK_SEVERITY > 1 .AND.  FL_FAILURE ) THEN
           CALL ERR_LOG ( 7620, IUER, 'PIMA_SCAN_SPLIT', 'Fits files for '// &
     &         'experiment '//PIM%CONF%SESS_CODE//' did not pass a test: '// &
     &         'some points do not belong to any scan or belong to more '// &
     &         'than one scan' )
           RETURN
      END IF
!
      DEALLOCATE ( OBS_SCA )
      DEALLOCATE ( AUT_SCA )
!
      ALLOCATE ( PIM%OBS(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NOBS*SIZEOF(PIM%OBS(1)), STR )
           CALL ERR_LOG ( 7621, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      ALLOCATE ( PIM%AUT(PIM%NAUT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NAUT*SIZEOF(PIM%OBS(1)), STR )
           CALL ERR_LOG ( 7622, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL PIMA_OBS_INIT ( PIM%NOBS, PIM%OBS )
      CALL PIMA_OBS_INIT ( PIM%NAUT, PIM%AUT )
!
      ALLOCATE ( UV_STA_IND(PIM__MFRG,PIM__MUV,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MFRQ*PIM__MUV*PIM%NSTA, STR )
           CALL ERR_LOG ( 7623, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      ALLOCATE ( UV_BAS_IND(PIM__MFRG,PIM__MUV,M_BAS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MFRQ*PIM__MUV*M_BAS, STR )
           CALL ERR_LOG ( 7624, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           DEALLOCATE ( UV_STA_IND )
           RETURN
      END IF
!
      ALLOCATE ( UV_STA_MFRG(PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MSTA, STR )
           CALL ERR_LOG ( 7625, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM%NSTA, UV_STA_MFRG )
!
      ALLOCATE ( UV_BAS_MFRG(M_BAS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_BAS, STR )
           CALL ERR_LOG ( 7626, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_I4 ( M_BAS, UV_BAS_MFRG )
!
      ALLOCATE ( STA_FRG(PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__MSTA, STR )
           CALL ERR_LOG ( 7627, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM%NFRG*PIM%NSTA, STA_FRG )
!
      ALLOCATE ( BAS_FRG(PIM%NFRG,M_BAS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_BAS, STR )
           CALL ERR_LOG ( 7628, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM%NFRG*M_BAS, BAS_FRG )
!
      IND_OBS = 0
      IND_AUT = 0
      FL_FAILURE = .FALSE.
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           CALL TIM_INIT ( )
      END IF
      NUM_EXC_OBS = 0
      DO 4220 J22=1,PIM%NSCA
         IF ( PIM%SCA(J22)%IND_ROOT == 0 ) THEN
              PIM%SCA(J22)%NBAS = NBAS_SCA(J22)
              PIM%SCA(J22)%NSTA = NSTA_SCA(J22)
              SCA_SOU_IND = PIM%SCA(J22)%SOU_IND
            ELSE
              PIM%SCA(J22)%NBAS = NBAS_SCA(PIM%SCA(J22)%IND_ROOT)
              PIM%SCA(J22)%NSTA = NSTA_SCA(PIM%SCA(J22)%IND_ROOT)
              SCA_SOU_IND = PIM%SCA(PIM%SCA(J22)%IND_ROOT)%SOU_IND
         END IF
         ALLOCATE ( PIM%SCA(J22)%OBS_IND(PIM%SCA(J22)%NBAS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7629, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &            'allocate dynamic memory for OBS_IND at a scan' )
              DEALLOCATE ( UV_STA_IND  )
              DEALLOCATE ( UV_BAS_IND  )
              DEALLOCATE ( UV_STA_MFRG )
              DEALLOCATE ( UV_BAS_MFRG )
              RETURN
         END IF
!
         ALLOCATE ( PIM%SCA(J22)%AUT_IND(PIM%SCA(J22)%NSTA), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7630, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &            'allocate dynamic memory for AUT_IND at a scan' )
              DEALLOCATE ( UV_STA_IND  )
              DEALLOCATE ( UV_BAS_IND  )
              DEALLOCATE ( UV_STA_MFRG )
              DEALLOCATE ( UV_BAS_MFRG )
              RETURN
         END IF
!
         CALL NOUT_I4 ( PIM__MSTA,           NSCA_STA )
         CALL NOUT_I4 ( PIM__MSTA*PIM__MFRG, NUV_STA   )
         CALL NOUT_I4 ( MM_BAS*PIM__MFRG,    NUV_BAS   )
!
         CALL NOUT_I4 ( PIM__MFRG*PIM__MUV*PIM%NSTA, UV_STA_IND )
         CALL NOUT_I4 ( PIM__MFRG*PIM__MUV*M_BAS,    UV_BAS_IND )
         CALL NOUT_I4 ( PIM%NSTA, UV_STA_MFRG )
         CALL NOUT_I4 ( M_BAS,    UV_BAS_MFRG )
!
         LSTA = 0
         LBAS = 0
         DO 4230 J23=1,PIM%NUV
            IF ( PIM%UV_IND(J23)%POI_IND        .LE. 0 .OR. &
     &           PIM%UV_IND(J23)%POI_AUT_IND(1) .LE. 0 .OR. &
     &           PIM%UV_IND(J23)%POI_AUT_IND(2) .LE. 0      ) GOTO 4230 ! Bypass bad points
!
            IF ( PIM%UV_IND(J23)%TIM_IND .GE. PIM%SCA(J22)%TIM_IND     .AND. &
     &           PIM%UV_IND(J23)%TIM_IND .LE. PIM%SCA(J22)%TIM_IND +         &
     &                                        PIM%SCA(J22)%NUM_EPC - 1 .AND. &
     &           PIM%UV_IND(J23)%SOU_IND .EQ. SCA_SOU_IND                    ) THEN
!
! -------------- Augment list NSCA_STA/LSTA -- list of stations that
! -------------- participated in scan J22
!
                 IF ( PIM%UV_IND(J23)%STA_IND(1) == PIM%UV_IND(J23)%STA_IND(2) ) THEN
!
! ------------------- Case of autocorrelation
!
                      CALL ERR_PASS ( IUER, IER )
                      IND_STA = ADD_LIS ( PIM__MSTA, LSTA, NSCA_STA, &
     &                                    INT(PIM%UV_IND(J23)%STA_IND(1),KIND=4), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7631, IUER, 'PIMA_SCAN_SPLIT', &
     &                          'Trap of internal control: failure to '// &
     &                          'update the temporary scan list' )
                           DEALLOCATE ( UV_STA_IND  )
                           DEALLOCATE ( UV_BAS_IND  )
                           DEALLOCATE ( UV_STA_MFRG )
                           DEALLOCATE ( UV_BAS_MFRG )
                           RETURN
                      END IF
!
! ------------------- Compute the index of the frequency group for
! ------------------- the J23 -th UV point in the global frequency groups list
!
                      CALL ERR_PASS ( IUER, IER )
                      IND_GLO_FRG_IND = ADD_LIS ( PIM%NFRG, &
     &                                            UV_STA_MFRG(IND_STA), &
     &                                            STA_FRG(1,IND_STA), &
     &                                            INT(PIM%UV_IND(J23)%FRG_IND,KIND=4), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7632, IUER, 'PIMA_SCAN_SPLIT', &
     &                          'Trap of internal control: failure to '// &
     &                          'update the frequency group station list' )
                           DEALLOCATE ( UV_STA_IND  )
                           DEALLOCATE ( UV_BAS_IND  )
                           DEALLOCATE ( UV_STA_MFRG )
                           DEALLOCATE ( UV_BAS_MFRG )
                           RETURN
                      END IF
!
! ------------------- Increment the number of UV_STA -- the number of epochs
! ------------------- for the IND_STA -th station
!
                      NUV_STA(IND_STA,IND_GLO_FRG_IND) = NUV_STA(IND_STA,IND_GLO_FRG_IND) + 1
                      IF ( PIM%UV_IND(J23)%FRG_IND < 1 ) THEN
!
! ------------------------ Fix wrong frequency group index
!
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                                WRITE ( 6, * ) 'PIMA_SCAN_SPLIT-1025 Set frequency group index = 1 for UV ', J23
                           END IF
                           PIM%UV_IND(J23)%FRG_IND = 1
                      END IF
! 
                      UV_STA_IND(PIM%UV_IND(J23)%FRG_IND,NUV_STA(IND_STA,IND_GLO_FRG_IND),IND_STA) = J23
                    ELSE
!
! ------------------- Case of cross correlation
!
! ------------------- Augment list NSCA_BAS/LBAS -- list of baselines that
! ------------------- participated in the J22 -th scan
!
                      CALL ERR_PASS ( IUER, IER )
                      STA_INDS(1) = MIN ( PIM%UV_IND(J23)%STA_IND(1), &
     &                                    PIM%UV_IND(J23)%STA_IND(2)  )
                      STA_INDS(2) = MAX ( PIM%UV_IND(J23)%STA_IND(1), &
     &                                    PIM%UV_IND(J23)%STA_IND(2)  )
                      BAS_ID  = (PIM%NSTA+1)*STA_INDS(1) + STA_INDS(2)
                      CALL ERR_PASS ( IUER, IER )
                      IND_BAS = ADD_LIS ( M_BAS, LBAS, NSCA_BAS, BAS_ID, IER  )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7633, IUER, 'PIMA_SCAN_SPLIT', &
     &                          'Trap of internal control: failure to '// &
     &                          'update the temporary scan list' )
                           DEALLOCATE ( UV_STA_IND  )
                           DEALLOCATE ( UV_BAS_IND  )
                           DEALLOCATE ( UV_STA_MFRG )
                           DEALLOCATE ( UV_BAS_MFRG )
                           RETURN
                      END IF
!
! ------------------- Compute the index of the frequency group for
! ------------------- the J23 -th UV point in the global frequency groups list
!
                      CALL ERR_PASS ( IUER, IER )
                      IND_GLO_FRG_IND = ADD_LIS ( PIM%NFRG, &
     &                                            UV_BAS_MFRG(IND_BAS), &
     &                                            BAS_FRG(1,IND_BAS), &
     &                                            INT(PIM%UV_IND(J23)%FRG_IND,KIND=4), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7634, IUER, 'PIMA_SCAN_SPLIT', &
     &                          'Trap of internal control: failure to '// &
     &                          'update the frequency group baseline list' )
                           DEALLOCATE ( UV_STA_IND  )
                           DEALLOCATE ( UV_BAS_IND  )
                           DEALLOCATE ( UV_STA_MFRG )
                           DEALLOCATE ( UV_BAS_MFRG )
                           RETURN
                      END IF
!
! ------------------- Increment UV_BAS -- the number of epochs
! ------------------- for the IND_BAS -th baseline
!
                      NUV_BAS(IND_BAS,IND_GLO_FRG_IND) = NUV_BAS(IND_BAS,IND_GLO_FRG_IND) + 1
                      IF ( PIM%UV_IND(J23)%FRG_IND < 1 ) THEN
!
! ------------------------ Fix wrong frequency group index
!
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                                WRITE ( 6, * ) 'PIMA_SCAN_SPLIT-1085 Set frequency group index = 1 for UV ', J23
                           END IF
                           PIM%UV_IND(J23)%FRG_IND = 1
                      END IF
!
! ------------------- Store the UV index for
! ------------------- the NUV_BAS(IND_BAS) -th observation,
! ------------------- at the IND_BAS -th baseline,
! ------------------- the GLO_FRG_IND -th frequency group
!
                      UV_BAS_IND(PIM%UV_IND(J23)%FRG_IND,NUV_BAS(IND_BAS,IND_GLO_FRG_IND),IND_BAS) = J23
                 END IF
!
                 PIM%UV_IND(J23)%SCA_IND = J22
            END IF
 4230    CONTINUE
!
! ------ Create the auto-correlation objects for all stations observed in this
! ------ J22 -th scan
!
         DO 4240 J24=1,LSTA
            IND_AUT =IND_AUT + 1
            PIM%SCA(J22)%AUT_IND(J24) = IND_AUT
            PIM%AUT(IND_AUT)%NUM_EPC  = 0
            DO 4250 J25=1,UV_STA_MFRG(J24)
               IF ( STA_FRG(J25,J24) < 1 ) THEN
!
! ----------------- Fix wrong frequency group index
!
                    STA_FRG(J25,J24) = 1
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                         WRITE ( 6, * ) 'PIMA_SCAN_INDEX-1109 Reset frequency group index = 1 for UV ', J25
                    END IF
               END IF
               IF ( UV_STA_IND(STA_FRG(J25,J24),1,J24) > 0 ) THEN
                    IF ( PIM%AUT(IND_AUT)%TIM_BEG_IND < 1 ) THEN
                         PIM%AUT(IND_AUT)%TIM_BEG_IND = &
     &                       PIM%UV_IND(UV_STA_IND(STA_FRG(J25,J24),1,J24))%TIM_IND
                       ELSE
                         PIM%AUT(IND_AUT)%TIM_BEG_IND = MIN ( PIM%AUT(IND_AUT)%TIM_BEG_IND, &
     &                       PIM%UV_IND(UV_STA_IND(STA_FRG(J25,J24),1,J24))%TIM_IND )
                    END IF
                    PIM%AUT(IND_AUT)%NUM_EPC(J25) = MAX ( NUV_STA(J24,J25), PIM%AUT(IND_AUT)%NUM_EPC(J25) )
               END IF
 4250       CONTINUE
            PIM%AUT(IND_AUT)%STA_IND(1) = NSCA_STA(J24)
            PIM%AUT(IND_AUT)%STA_IND(2) = NSCA_STA(J24)
            PIM%AUT(IND_AUT)%SOU_IND    = PIM%SCA(J22)%SOU_IND
            PIM%AUT(IND_AUT)%SCA_IND    = J22
!
! --------- Allocate memory with indexes of autocorrelation UV data
!
            PIM%AUT(IND_AUT)%NUVS = UV_STA_MFRG(J24)
            MAX_NUM_OBS = MAX_I4 ( PIM%AUT(IND_AUT)%NUVS, PIM%AUT(IND_AUT)%NUM_EPC )
            ALLOCATE ( PIM%AUT(IND_AUT)%UV_IND(MAX_NUM_OBS,PIM%AUT(IND_AUT)%NUVS), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( PIM%NOBS*SIZEOF(PIM%OBS(1)), STR )
                 DEALLOCATE ( UV_STA_IND  )
                 DEALLOCATE ( UV_BAS_IND  )
                 DEALLOCATE ( UV_STA_MFRG )
                 DEALLOCATE ( UV_BAS_MFRG )
                 CALL ERR_LOG ( 7635, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &               'allocate dynamic memory for the autocorrelation' )
                 RETURN
            END IF
!
            DO 4260 J26=1,MAX_NUM_OBS
               DO 4270 J27=1,UV_STA_MFRG(J24)
                  IND_AUT_UV = UV_STA_IND(STA_FRG(J27,J24),J26,J24)
                  PIM%AUT(IND_AUT)%UV_IND(J26,J27) = IND_AUT_UV
 4270          CONTINUE
 4260       CONTINUE
            PIM%AUT(IND_AUT)%REF_FRG_INDS = 0
            DO 4280 J28=1,PIM%AUT(IND_AUT)%NUVS
               PIM%AUT(IND_AUT)%GLO_FRG_INDS(J28) = STA_FRG(J28,J24)
               PIM%AUT(IND_AUT)%REF_FRG_INDS(STA_FRG(J28,J24)) = J28
 4280       CONTINUE
 4240    CONTINUE
!
         IF ( LBAS > PIM%SCA(J22)%NBAS ) THEN
              WRITE ( 6, * ) ' J22=',J22, ' LBAS = ', LBAS, &
     &                       ' PIM%SCA(J22)%NBAS = ', PIM%SCA(J22)%NBAS
              WRITE ( 6, * ) ' NSCA_BAS = ', NSCA_BAS(1:LBAS)
              CALL ERR_LOG ( 7636, IUER, 'PIMA_SCAN_SPLIT', 'Trap of '// &
     &            'internal control' )
              RETURN
         END IF
!
         PIM%MAX_EPC_OBS = -1
         DO 4290 J29=1,LBAS
            IND_OBS = IND_OBS + 1
            PIM%SCA(J22)%OBS_IND(J29) = IND_OBS
            PIM%OBS(IND_OBS)%NUM_EPC = 0
!
! --------- Check whether we have amount UV points for scan J22, baseline
! --------- J29  UV points with both station order:
! --------- direct  (ID of the first station is less than ID of the second station) and
! --------- reverse (ID of the first station is greater than ID of the second station)
!
! --------- We count non-zero UV points with direct and reverse order
!
            NUM_DIR = 0
            NUM_REV = 0
!
            DO 4300 J30=1,UV_BAS_MFRG(J29)
               DO 4310 J31=1,NUV_BAS(J29,J30)
                  IF ( BAS_FRG(J30,J29) < 1 ) THEN 
                       BAS_FRG(J30,J29) = 1 ! This is a pathological case
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                            WRITE ( 6, * ) 'WARNING: PIMA_SCAN_SPLIT fixed zero frequency group for J30= ', J30, ' J29= ', J29
                       END IF
                  END IF
                  IND_UV = UV_BAS_IND(BAS_FRG(J30,J29),J31,J29)
                  IF ( IND_UV > 0 ) THEN
                       IF ( PIM%UV_IND(IND_UV)%STA_IND(1) < &
     &                      PIM%UV_IND(IND_UV)%STA_IND(2)   ) THEN
                            NUM_DIR = NUM_DIR + 1
                          ELSE
                            NUM_REV = NUM_REV + 1
                       END IF
                  END IF
 4310          CONTINUE
 4300       CONTINUE
!
! --------- Normally, either NUM_DIR or NUM_REV should be zero
!
            IF ( .NOT. ( NUM_DIR == 0 .OR. NUM_REV == 0 ) .AND. &
     &                   PIM%CONF%CHECK_SEVERITY > 0            ) THEN
!
! -------------- This time it is not zero. We need to exclude UV
! -------------- points either in direct order or in reverse order.
! -------------- We exclude those points which total number is less
!
                 DO 4320 J32=1,UV_BAS_MFRG(J29)
                    DO 4330 J33=1,NUV_BAS(J29,J32)
                       IF ( BAS_FRG(J32,J29) < 1 ) THEN
                            BAS_FRG(J32,J29) = 1  ! This is a pathological case
                       END IF
                       IND_UV = UV_BAS_IND(BAS_FRG(J32,J29),J33,J29)
                       IF ( IND_UV > 0 ) THEN
                            IF ( PIM%UV_IND(IND_UV)%STA_IND(1) < &
     &                           PIM%UV_IND(IND_UV)%STA_IND(2)   ) THEN
                                 IF ( NUM_DIR .LE. NUM_REV ) THEN
!
! ----------------------------------- Set the UV_IND for that point to zero and update the counter
! ----------------------------------- of the number of points at this baseline.
!
                                      UV_BAS_IND(BAS_FRG(J32,J29),J33,J29) = 0
!
! ----------------------------------- Mark this point as duplicate
!
                                      PIM%UV_IND(IND_UV)%POI_IND = -PIM%UV_IND(IND_UV)%POI_IND
                                      PIM%UV_IND(IND_UV)%POI_AUT_IND(1) = -PIM%UV_IND(IND_UV)%POI_AUT_IND(1)
                                      PIM%UV_IND(IND_UV)%POI_AUT_IND(2) = -PIM%UV_IND(IND_UV)%POI_AUT_IND(2)
                                      N_BAD = N_BAD + 1
                                      UV_BAD(N_BAD) = IND_UV
                                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                           WRITE ( 6, 270 ) PIM%C_SOU(PIM%UV_IND(IND_UV)%SOU_IND), &
     &                                            PIM%C_STA(PIM%UV_IND(IND_UV)%STA_IND(1)), &
     &                                            PIM%C_STA(PIM%UV_IND(IND_UV)%STA_IND(2)),    &
     &                                            IND_UV, PIM%UV_IND(IND_UV)%ORIG_IND, &
     &                                            ' Reversed baseline DIR', J32
                                      END IF
                                 END IF
                               ELSE
                                 IF ( NUM_REV < NUM_DIR ) THEN
!
! ----------------------------------- The same, but this time we remove the point in
! ----------------------------------- the reverse order, sinve it is minority now
!
                                      UV_BAS_IND(BAS_FRG(J32,J29),J33,J29) = 0
!
                                      PIM%UV_IND(IND_UV)%POI_IND = -PIM%UV_IND(IND_UV)%POI_IND
                                      PIM%UV_IND(IND_UV)%POI_AUT_IND(1) = -PIM%UV_IND(IND_UV)%POI_AUT_IND(1)
                                      PIM%UV_IND(IND_UV)%POI_AUT_IND(2) = -PIM%UV_IND(IND_UV)%POI_AUT_IND(2)
                                      N_BAD = N_BAD + 1
                                      UV_BAD(N_BAD) = IND_UV
                                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                           WRITE ( 6, 270 ) PIM%C_SOU(PIM%UV_IND(IND_UV)%SOU_IND), &
     &                                            PIM%C_STA(PIM%UV_IND(IND_UV)%STA_IND(1)), &
     &                                            PIM%C_STA(PIM%UV_IND(IND_UV)%STA_IND(2)),    &
     &                                            IND_UV, PIM%UV_IND(IND_UV)%ORIG_IND, &
     &                                            ' Reversed baseline REV', J32
                                      END IF
                                END IF
                            END IF
                       END IF
 4330               CONTINUE
 4320            CONTINUE
!
! -------------- Now remove those points in UV_BAS_IND which are zero
! -------------- by shrinking the array. Decrement the points counter NUV_BAS
!
                 DO 4340 J34=1,UV_BAS_MFRG(J29)
                    NUV_BAS_SAVED = NUV_BAS(J29,J34)
                    DO 4350 J35=1,NUV_BAS_SAVED
                       UV_TMP_ARR(J35) = UV_BAS_IND(BAS_FRG(J34,J29),J35,J29)
 4350               CONTINUE
                    NUV_BAS(J29,J34) = 0
                    DO 4360 J36=1,NUV_BAS_SAVED
                       IF ( UV_TMP_ARR(J36) > 0 ) THEN
                            NUV_BAS(J29,J34) = NUV_BAS(J29,J34) + 1
                            UV_BAS_IND(BAS_FRG(J34,J29),NUV_BAS(J29,J34),J29) = UV_TMP_ARR(J36)
                       END IF
 4360               CONTINUE
 4340           CONTINUE
            END IF
!
            DO 4370 J37=1,UV_BAS_MFRG(J29)
               DO 4380 J38=1,NUV_BAS(J29,J37)
                  IND_UV = UV_BAS_IND(BAS_FRG(J37,J29),J38,J29)
                  IF ( IND_UV > 0 ) THEN
                       IF ( PIM%OBS(IND_OBS)%TIM_BEG_IND < 1 ) THEN
                            PIM%OBS(IND_OBS)%TIM_BEG_IND = PIM%UV_IND(IND_UV)%TIM_IND
                          ELSE
                            PIM%OBS(IND_OBS)%TIM_BEG_IND = MIN ( PIM%OBS(IND_OBS)%TIM_BEG_IND, &
     &                                                           PIM%UV_IND(IND_UV)%TIM_IND )
                       END IF
                       PIM%OBS(IND_OBS)%NUM_EPC(J37) = MAX ( NUV_BAS(J29,J37), PIM%OBS(IND_OBS)%NUM_EPC(J37) )
                       GOTO 8380
                  END IF
 4380         CONTINUE
 8380         CONTINUE
 4370       CONTINUE
!
            MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%OBS(IND_OBS)%NUM_EPC )
            IF ( MAX_NUM_OBS > PIM%MAX_EPC_OBS ) THEN
                 PIM%MAX_EPC_OBS = MAX_NUM_OBS
            END IF
            IF ( NUM_DIR > NUM_REV ) THEN
                 PIM%OBS(IND_OBS)%STA_IND(1)  = NSCA_BAS(J29)/(PIM%NSTA+1)
                 PIM%OBS(IND_OBS)%STA_IND(2)  = NSCA_BAS(J29) - &
     &                            PIM%OBS(IND_OBS)%STA_IND(1)*(PIM%NSTA+1)
               ELSE
                 PIM%OBS(IND_OBS)%STA_IND(2)  = NSCA_BAS(J29)/(PIM%NSTA+1)
                 PIM%OBS(IND_OBS)%STA_IND(1)  = NSCA_BAS(J29) - &
     &                            PIM%OBS(IND_OBS)%STA_IND(2)*(PIM%NSTA+1)
            END IF
            PIM%OBS(IND_OBS)%SOU_IND      = PIM%SCA(J22)%SOU_IND
            IF ( PIM%SCA(J22)%IND_ROOT > 0 ) THEN
                 PIM%OBS(IND_OBS)%ROOT_SOU_IND = PIM%SCA(PIM%SCA(J22)%IND_ROOT)%SOU_IND
               ELSE
                 PIM%OBS(IND_OBS)%ROOT_SOU_IND = PIM%SCA(J22)%SOU_IND
            END IF
            PIM%OBS(IND_OBS)%SCA_IND      = J22
            PIM%OBS(IND_OBS)%TSYS_IND     = 0
            PIM%OBS(IND_OBS)%STMO_IND     = 0
            PIM%OBS(IND_OBS)%PCAL_IND     = 0
            PIM%OBS(IND_OBS)%MOD_IND_BEG  = 0
            PIM%OBS(IND_OBS)%MOD_IND_END  = 0
!
            PIM%OBS(IND_OBS)%FRI_STS      = 0
            PIM%OBS(IND_OBS)%IND_OBS_2ND  = 0
!
            PIM%OBS(IND_OBS)%RES_MB_DEL   = -1.D10
            PIM%OBS(IND_OBS)%RES_PH_RAT   = -1.D10
            PIM%OBS(IND_OBS)%RES_PHS      = -1.D10
            PIM%OBS(IND_OBS)%RES_SB_DEL   = -1.D10
            PIM%OBS(IND_OBS)%RES_GR_RAT   = -1.D10
!
            PIM%OBS(IND_OBS)%RES_MB_DEL   = -1.D10
            PIM%OBS(IND_OBS)%RES_PH_RAT   = -1.D10
            PIM%OBS(IND_OBS)%RES_PHS      = -1.D10
!
            PIM%OBS(IND_OBS)%PCAL_GDEL    = -1.D10
            PIM%OBS(IND_OBS)%WVR_DEL_AVR  = -1.D10
            PIM%OBS(IND_OBS)%TEC          = -1.D10
            PIM%OBS(IND_OBS)%TEC_RATE     = -1.D10
!
            PIM%OBS(IND_OBS)%APR_GC_DEL   = -1.D10
            PIM%OBS(IND_OBS)%APR_GC_PHS   = -1.D10
            PIM%OBS(IND_OBS)%APR_GC_RAT   = -1.D10
            PIM%OBS(IND_OBS)%APR_GR_DEL   = -1.D10
            PIM%OBS(IND_OBS)%APR_RAT      = -1.D10
            PIM%OBS(IND_OBS)%APR_PHS      = -1.D10
!
            PIM%OBS(IND_OBS)%THE_GR_DEL   = -1.D10
            PIM%OBS(IND_OBS)%THE_PH_DEL   = -1.D10
            PIM%OBS(IND_OBS)%THE_RATE     = -1.D10
!
            PIM%OBS(IND_OBS)%TOT_MB_DEL   = -1.D10
            PIM%OBS(IND_OBS)%TOT_PH_RAT   = -1.D10
            PIM%OBS(IND_OBS)%TOT_PHS      = -1.D10
            PIM%OBS(IND_OBS)%TOT_PHS_GC   = -1.D10
            PIM%OBS(IND_OBS)%TOT_SB_DEL   = -1.D10
!
            PIM%OBS(IND_OBS)%PH_RAT_ERR   = -1.D10
            PIM%OBS(IND_OBS)%SB_DEL_ERR   = -1.D10
            PIM%OBS(IND_OBS)%GR_RAT_ERR   = -1.D10
!
            PIM%OBS(IND_OBS)%MB_DEL_ERR   = -1.D10
            PIM%OBS(IND_OBS)%PH_DEL_ERR   = -1.D10
            PIM%OBS(IND_OBS)%PH_DEL_ERR   = -1.D10
            PIM%OBS(IND_OBS)%PH_DEL_ERR   = -1.D10
            PIM%OBS(IND_OBS)%PH_DEL_ERR   = -1.D10
!
            PIM%OBS(IND_OBS)%REF_FREQ     = -1.D10
            PIM%OBS(IND_OBS)%FRT_OFFSET   = -1.D10
            PIM%OBS(IND_OBS)%SRT_OFFSET   = -1.D10
            PIM%OBS(IND_OBS)%EFF_FRQ      = -1.D10
            PIM%OBS(IND_OBS)%UVW          = -1.E10
            PIM%OBS(IND_OBS)%ELEV         = -1.E10
            PIM%OBS(IND_OBS)%AZ           = -1.E10
            PIM%OBS(IND_OBS)%AMPL         = -1.E10
            PIM%OBS(IND_OBS)%NOISE        = -1.E10
            PIM%OBS(IND_OBS)%PA_USED      = -1.E10
            PIM%OBS(IND_OBS)%POLAR_USED   = '??'
!
            PIM%OBS(IND_OBS)%SCAN_DURA    = -1.D10
            PIM%OBS(IND_OBS)%GRAMBSP      = -1.D10
            PIM%OBS(IND_OBS)%FEED_ANG     = -1.D10
            PIM%OBS(IND_OBS)%TIM_BEG      = -1.D10
            PIM%OBS(IND_OBS)%TIM_END      = -1.D10
            PIM%OBS(IND_OBS)%TIM_END_IND  =  0
!
            PIM%OBS(IND_OBS)%PRES         = -1.E10
            PIM%OBS(IND_OBS)%TEMP         = -1.E10
            PIM%OBS(IND_OBS)%HUMID        = -1.E10
            PIM%OBS(IND_OBS)%CABLE        = -1.E10
!
            PIM%OBS(IND_OBS)%POLARIZ(1)   = PIMA__POL(PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1))// &
     &                                      PIMA__POL(PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2))
            PIM%OBS(IND_OBS)%POLARIZ(2)   = PIMA__POL(PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1))// &
     &                                      PIMA__POL(PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2))
!
            PIM%OBS(IND_OBS)%NUVS = UV_BAS_MFRG(J29)
            PIM%OBS(IND_OBS)%REF_FRG_INDS = 0
            DO 4390 J39=1,PIM%OBS(IND_OBS)%NUVS
               PIM%OBS(IND_OBS)%GLO_FRG_INDS(J39) = BAS_FRG(J39,J29)
               PIM%OBS(IND_OBS)%REF_FRG_INDS(BAS_FRG(J39,J29)) = J39
 4390       CONTINUE
!
            MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%OBS(IND_OBS)%NUM_EPC )
            ALLOCATE ( PIM%OBS(IND_OBS)%UV_IND(MAX_NUM_OBS,PIM%OBS(IND_OBS)%NUVS), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 4*MAX_NUM_OBS*PIM%OBS(IND_OBS)%NUVS, STR )
                 CALL ERR_LOG ( 7637, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &               'allocate dynamic memory for the autocorrelation' )
                 DEALLOCATE ( UV_STA_IND  )
                 DEALLOCATE ( UV_BAS_IND  )
                 DEALLOCATE ( UV_STA_MFRG )
                 DEALLOCATE ( UV_BAS_MFRG )
                 RETURN
            END IF
!
            ALLOCATE ( PIM%OBS(IND_OBS)%CORR_FLAG(MAX_NUM_OBS,PIM%NFRG), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 4*MAX_NUM_OBS, STR )
                 CALL ERR_LOG ( 7638, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &               'allocate dynamic memory for the correlation flags' )
                 DEALLOCATE ( UV_STA_IND  )
                 DEALLOCATE ( UV_BAS_IND  )
                 DEALLOCATE ( UV_STA_MFRG )
                 DEALLOCATE ( UV_BAS_MFRG )
                 RETURN
            END IF
            PIM%OBS(IND_OBS)%CORR_FLAG = 4 ! Default
!
            ALLOCATE ( PIM%OBS(IND_OBS)%RES_FRN(PIM%NFRQ,PIM%NBND), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%NFRQ, STR )
                 CALL ERR_LOG ( 7639, IUER, 'PIMA_SCAN_SPLIT', 'Failure to '// &
     &               'allocate dynamic memory for residuals cross correlation' )
                 DEALLOCATE ( UV_STA_IND  )
                 DEALLOCATE ( UV_BAS_IND  )
                 DEALLOCATE ( UV_STA_MFRG )
                 DEALLOCATE ( UV_BAS_MFRG )
                 RETURN
            END IF
!
! --------- Initialize fringe residuals
!
            CALL NOUT_R4 ( 2*PIM%NFRQ*PIM%NBND, PIM%OBS(IND_OBS)%RES_FRN )
!
            PIM%OBS(IND_OBS)%AP_LEN = 1.D10
            AP_LEN_MAX = -1.0D10
            DO 4400 J40=1,MAX_NUM_OBS
               DO 4410 J41=1,PIM%OBS(IND_OBS)%NUVS
                  IND_OBS_UV = UV_BAS_IND(BAS_FRG(J41,J29),J40,J29)
                  PIM%OBS(IND_OBS)%UV_IND(J40,J41) = IND_OBS_UV
                  IF ( IND_OBS_UV == 0 ) GOTO 4410
!
                  PIM%UV_IND(IND_OBS_UV)%OBS_IND = IND_OBS
!
! --------------- Update the time tag for the first and last AP of the
! --------------- J40 -th observation
!
                  IF ( PIM%OBS(IND_OBS)%TIM_BEG == -1.D10 .OR. &
     &                 PIM%TIM_R8(PIM%UV_IND(IND_OBS_UV)%TIM_IND) < PIM%OBS(IND_OBS)%TIM_BEG ) THEN
                       PIM%OBS(IND_OBS)%TIM_BEG = PIM%TIM_R8(PIM%UV_IND(IND_OBS_UV)%TIM_IND)
                  END IF
                  IF ( PIM%OBS(IND_OBS)%TIM_END_IND < PIM%UV_IND(IND_OBS_UV)%TIM_IND ) THEN
                       PIM%OBS(IND_OBS)%TIM_END_IND = PIM%UV_IND(IND_OBS_UV)%TIM_IND
                  END IF
                  PIM%OBS(IND_OBS)%TIM_END = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_END_IND)
                  TIM_ARR(J40,J41) = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_END_IND)
                  IF ( J40 > 1 ) THEN
                       IF ( TIM_ARR(J40,J41) - TIM_ARR(J40-1,J41) > PIMA__AP_LEN_MIN ) THEN
                            PIM%OBS(IND_OBS)%AP_LEN = MIN ( PIM%OBS(IND_OBS)%AP_LEN, &
     &                                                      TIM_ARR(J40,J41) - TIM_ARR(J40-1,J41) )
                       END IF
                       IF ( ( TIM_ARR(J40,J41) - TIM_ARR(J40-1,J41) - PIMA__AP_LEN_MIN )*AP_LEN_SCA_MIN < &
     &                        PIM%CONF%AP_TOLERANCE*PIM%CONF%MAX_SCAN_LEN ) THEN
                            AP_LEN_MAX = MAX ( AP_LEN_MAX, &
     &                                         TIM_ARR(J40,J41) - TIM_ARR(J40-1,J41) )
                       END IF
                  END IF
!
! --------------- Find index of the autocorrelation uv for the first station
!
                  IND_STA = IFIND_PL ( LSTA, NSCA_STA, &
     &                                 INT(PIM%UV_IND(IND_OBS_UV)%STA_IND(1),KIND=4) )
                  IF ( FL_AUT_NOSOURCE_STRICT .AND.        &
     &                 PIM%CONF%DEBUG_LEVEL .GE. 1  .AND. &
     &                 IND_STA .LE. 0                     ) THEN
!
                       CALL CLRCH ( STR )
                       CALL INCH  ( IND_OBS_UV, STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( PIM%UV_IND(IND_OBS_UV)%OBS_IND, STR1 )
                       CALL CLRCH ( STR2 )
                       CALL INCH  ( PIM%UV_IND(IND_OBS_UV)%ORIG_IND, STR2 )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7641, IER, 'PIMA_SCAN_SPLIT', 'Trap of '// &
     &                     'internal control: cannot find autocorrelation '// &
     &                     'data for the first station for uv point #'// &
     &                      STR(1:I_LEN(STR))//', observation '// &
     &                      STR1(1:I_LEN(STR1))//'  orig_ind '//  &
     &                      STR2(1:I_LEN(STR2))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(IND_OBS_UV)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(IND_OBS_UV)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(IND_OBS_UV)%SOU_IND)%IVS_NAME )
                       WRITE ( 6, * ) ' LSTA= ',LSTA
                       IF ( LSTA > 0 ) WRITE ( 6, * ) ' NSCA_STA= ', NSCA_STA(1:LSTA)
                       IF ( LSTA > 0 ) WRITE ( 6, * ) ' CSTA= ', ( PIM%C_STA(NSCA_STA(K1)), K1=1,LSTA )
                       IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) FL_FAILURE = .TRUE.
                    ELSE IF ( PIM%CONF%DEBUG_LEVEL .LT. 1  .AND. &
     &                        IND_STA .LE. 0 ) THEN
                       GOTO 4400
                  END IF
!
! --------------- Find index of autocorrelation for the second station
!
                  IND_STA = IFIND_PL ( LSTA, NSCA_STA, &
     &                                 INT(PIM%UV_IND(IND_OBS_UV)%STA_IND(2),KIND=4) )
                  IF ( FL_AUT_NOSOURCE_STRICT       .AND. &
     &                 PIM%CONF%DEBUG_LEVEL .GE. 1  .AND. &
     &                 IND_STA .LE. 0                     ) THEN
!
                       CALL CLRCH ( STR )
                       CALL INCH  ( IND_OBS_UV, STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( PIM%UV_IND(IND_OBS_UV)%OBS_IND, STR1 )
                       CALL CLRCH ( STR2 )
                       CALL INCH  ( PIM%UV_IND(IND_OBS_UV)%ORIG_IND, STR2 )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7642, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                     'of internal control: cannot find autocorrelation '// &
     &                     'data for the second station for uv point #'// &
     &                      STR(1:I_LEN(STR))//', observation '// &
     &                      STR1(1:I_LEN(STR1))//'  orig_ind '//  &
     &                      STR2(1:I_LEN(STR2))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(IND_OBS_UV)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(IND_OBS_UV)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(IND_OBS_UV)%SOU_IND)%IVS_NAME )
                       WRITE ( 6, * ) ' LSTA= ',LSTA
                       IF ( LSTA > 0 ) WRITE ( 6, * ) ' NSCA_STA= ', NSCA_STA(1:LSTA)
                       IF ( LSTA > 0 ) WRITE ( 6, * ) ' CSTA= ', ( PIM%C_STA(NSCA_STA(K1)), K1=1,LSTA )
                       IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) FL_FAILURE = .TRUE.
                    ELSE IF ( PIM%CONF%DEBUG_LEVEL .LT. 1  .AND. IND_STA .LE. 0 ) THEN
                       GOTO 4400
                  END IF
 4410          CONTINUE
 4400       CONTINUE
            IF ( PIM%OBS(IND_OBS)%AP_LEN > PIM%TIM_R8(PIM%NEPC) ) THEN
                 PIM%OBS(IND_OBS)%AP_LEN = PIM%TIM_R8(2) - PIM%TIM_R8(1)
            END IF
!
! --------- Check time tags. They should be a multiple of accumulation period
! --------- length. We also compute NUM_AP_SPAN: the number of AP slots occupied
! --------- by the observation
!
            DO 4420 J42=1,PIM%NFRG
               PIM%OBS(IND_OBS)%NUM_AP_SPAN(J42) = 0
               IF ( PIM%OBS(IND_OBS)%NUM_EPC(J42) == 0 ) GOTO 4420
               DO 4430 J43=1,PIM%OBS(IND_OBS)%NUM_EPC(J42)
                  IND_AP = IDNINT ( (TIM_ARR(J43,J42) - TIM_ARR(1,J42))/PIM%OBS(IND_OBS)%AP_LEN )
!
                  IF ( ( ( DABS(TIM_ARR(J43,J42) - TIM_ARR(1,J42) - &
     &                     IND_AP*PIM%OBS(IND_OBS)%AP_LEN) > PIM%CONF%AP_TOLERANCE ) .AND. &
     &                   ( DABS(TIM_ARR(J43,J42) - TIM_ARR(1,J42) - IND_AP*PIM%OBS(IND_OBS)%AP_LEN) > &
     &                     IND_AP*(AP_LEN_MAX - PIM%OBS(IND_OBS)%AP_LEN) ) )          .OR. &
     &                 (AP_LEN_MAX - PIM%OBS(IND_OBS)%AP_LEN) > PIM%CONF%AP_TOLERANCE      ) THEN
!
! -------------------- Two cases of bad ap length:
! -------------------- 1) The time difference to the end of scan exceeded both the tolerance AND
! --------------------    the AP jitter multiplied by the number of AP
! -------------------- 2) AP length jutter exceeds the tolerance
!
                       WRITE ( 6, * ) 'UV points with different AP lengths were found:'
                       WRITE ( 6, * ) '  IND_OBS= ', IND_OBS, ' J42= ', INT2(J42), ' J43= ', INT2(J43), &
     &                                '  NUM_EPC= ', PIM%OBS(IND_OBS)%NUM_EPC
                       WRITE ( 6, * ) '  TIM_ARR(1,J42) = ', TIM_ARR(1,J42)
                       IF ( PIM%OBS(IND_OBS)%NUM_EPC(1) > 0 ) THEN
                            WRITE ( 6, * ) '  TIM_END= ', TIM_ARR(PIM%OBS(IND_OBS)%NUM_EPC(1),J42)
                       END IF
                       WRITE ( 6, * ) '  TIM_ARR(J43,J42) = ', TIM_ARR(J43,J42)
                       WRITE ( 6, * ) '  AP_LEN2 = ', AP_LEN2, ' AP_LEN_SCA_MEAN = ', AP_LEN_SCA_MEAN
                       WRITE ( 6, * ) '  IND_AP = ', IND_AP, ' UV_IND= ', PIM%OBS(IND_OBS)%UV_IND(J43,J42)
                       WRITE ( 6, * ) '  DIFF= ', TIM_ARR(J43,J42) - TIM_ARR(1,J42) - &
     &                                            IND_AP*PIM%OBS(IND_OBS)%AP_LEN, &
     &                                ' tim= ', MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_ARR(J42,1), -2 )
                       WRITE ( 6, * ) '  AP_LEN  = ', PIM%OBS(IND_OBS)%AP_LEN, ' AP_LEN_MAX = ', AP_LEN_MAX, &
     &                                '  AP_DIF = ', AP_LEN_MAX - PIM%OBS(IND_OBS)%AP_LEN
                       WRITE ( 6, * ) ' ' 
!@                       CALL ERR_LOG ( 7643, IUER, 'PIMA_SCAN_SPLIT', 'Trap of '// &
!@     &                     'internal control: time tag within an observation '// &
!@     &                     'is not a multiple of the accummulation period length' )
!@                       RETURN
                       GOTO 8430
                  END IF
                  PIM%OBS(IND_OBS)%NUM_AP_SPAN(J42) = IND_AP + 1
 4430          CONTINUE
 8430       CONTINUE
 4420       CONTINUE
 4290    CONTINUE
 4220 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
      END IF
!
      DEALLOCATE ( UV_STA_IND  )
      DEALLOCATE ( UV_BAS_IND  )
      DEALLOCATE ( UV_STA_MFRG )
      DEALLOCATE ( UV_BAS_MFRG )
      DEALLOCATE ( STA_FRG )
      DEALLOCATE ( BAS_FRG )
!
      IF ( FL_FAILURE ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%CHECK_SEVERITY, STR )
           CALL ERR_LOG ( 7644, IUER, 'PIMA_SCAN_SPLIT', 'Fits files for '// &
     &         'experiment '//PIM%CONF%SESS_CODE//' did not pass test at '// &
     &         'severity check level '//STR )
           RETURN
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IF ( NUM_EXC_OBS > 0 ) THEN
                WRITE  ( 6, 280 ) NUM_EXC_OBS
 280            FORMAT ( 'PIMA_SCA_SPLIT  ', I7, ' Observations with reversed '// &
     &                   'baselines have been removed' )
           END IF
           WRITE ( 6, 290 ) 'PIMA_SCAN_SPLIT  Scans check at severity level ', &
     &                       PIM%CONF%CHECK_SEVERITY
 290       FORMAT ( A, 1X, I3 )
      END IF
!
! --- Make additional checks, for example, reverse lookup
!
      FL_FAILURE = .FALSE.
      DO 4440 J44=1,PIM%NUV
         IF ( PIM%UV_IND(J44)%POI_IND .LE. 0 ) GOTO 4440
         IND_SCA = PIM%UV_IND(J44)%SCA_IND
!
         IF ( PIM%UV_IND(J44)%STA_IND(1) .NE. PIM%UV_IND(J44)%STA_IND(2) ) THEN
              DO 4450 J45=1,2
                 IF ( PIM%UV_IND(J44)%POI_AUT_IND(J45) .LE. 0 ) THEN
                      STA_IND = PIM%UV_IND(J44)%STA_IND(J45)
                      SOU_IND = PIM%UV_IND(J44)%SOU_IND
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      CALL CLRCH ( STR2 )
                      CALL CLRCH ( STR3 )
                      CALL CLRCH ( STR4 )
                      CALL INCH  ( PIM%UV_IND(J44)%ORIG_IND, STR  )
                      CALL INCH  ( PIM%UV_IND(J44)%OBS_IND,  STR1 )
                      CALL INCH  ( IND_SCA,  STR2 )
                      IF ( STA_IND > 0 ) STR3 = PIM%STA(STA_IND)%IVS_NAME
                      IF ( SOU_IND > 0 ) STR4 = PIM%SOU(SOU_IND)%IVS_NAME
!
                      IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_AUTO ) THEN
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                WRITE ( 6, 270 ) PIM%C_SOU(PIM%UV_IND(J44)%SOU_IND), &
     &                                  PIM%C_STA(PIM%UV_IND(J44)%STA_IND(1)), &
     &                                  PIM%C_STA(PIM%UV_IND(J44)%STA_IND(2)),    &
     &                                  J44, PIM%UV_IND(J44)%ORIG_IND, &
     &                                  ' No valid AUTO_CORR', 0
                           END IF
                         ELSE IF ( PIM%CONF%CHECK_SEVERITY .GE. 1 ) THEN
                           CALL ERR_PASS ( IUER, IER )
                           CALL ERR_LOG  ( 7645, IER, 'PIMA_SCAN_SPLIT', 'No '// &
     &                         'autocorrelation data were found for '// &
     &                         'UV point '//STR(1:I_LEN(STR))//' ; '// &
     &                         ' (orig) Obs index '//STR1(1:I_LEN(STR1))//' ; '// &
     &                         'Scan index '//STR2(1:I_LEN(STR2))//' ; '// &
     &                         'Station '//STR3(1:I_LEN(STR3))//' ; '// &
     &                         'Source '//STR4(1:I_LEN(STR4)) )
                           FL_FAILURE = .TRUE.
                      END IF
!
                      PIM%UV_IND(J44)%POI_IND = PIMA__AUTO_2
                      N_BAD = N_BAD + 1
                      UV_BAD(N_BAD) = J44
                 END IF
 4450         CONTINUE
         END IF
         IF ( IND_SCA == 0 ) THEN
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 1 .AND. &
     &             PIM%UV_IND(J44)%POI_IND > 0    .AND. &
     &             PIM%UV_IND(J44)%STA_IND(1) .NE. PIM%UV_IND(J44)%STA_IND(2) ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J44, STR(1:10) )
                   CALL INCH  ( PIM%UV_IND(J44)%ORIG_IND, STR(21:30) )
                   CALL ERR_PASS ( IUER, IER )
                   IF ( PIM%UV_IND(J44)%SOU_IND > 0 ) THEN
                        STR(41:48) = PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME
                      ELSE
                        STR(41:48) = 'unknown '
                   END IF
                   CALL ERR_LOG ( 7646, IUER, 'PIMA_SCAN_SPLIT', 'An orphan '// &
     &                 'observation uv_ind: '//STR(1:10)//'  orig_ind: '// &
     &                  STR(21:30)//' was found: '//  &
     &                  PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                  PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                  STR(41:48)//' -- no scan index' )
                   FL_FAILURE = .TRUE.
              END IF
        END IF
        IF ( PIM%CONF%CHECK_SEVERITY .GE. 3 .AND. PIM%UV_IND(J44)%POI_IND > 0 ) THEN
!
! ---------- Reverse lookup for auto-correlation points
!
             IF ( PIM%UV_IND(J44)%STA_IND(1) .EQ. PIM%UV_IND(J44)%STA_IND(2) .AND. &
     &            IND_SCA > 0                                                      ) THEN
                  IND_AUT_UV = 0
                  DO 4460 J46=1,PIM%SCA(IND_SCA)%NSTA
                     DO 4470 J47=1,PIM%SCA(IND_SCA)%AUT_IND(J46)
                        IF ( PIM%AUT(J47)%STA_IND(1) == PIM%UV_IND(J44)%STA_IND(1) ) THEN
                             MAX_NUM_OBS = MAX_I4 ( PIM%AUT(J47)%NUVS, PIM%AUT(J47)%NUM_EPC )
                             DO 4480 J48=1,MAX_NUM_OBS
                                DO 4490 J49=1,PIM%AUT(J47)%NUVS
                                   IF ( PIM%AUT(J47)%UV_IND(J48,J49) == J44 ) THEN
                                        IND_AUT_UV = J48
                                   END IF
 4490                           CONTINUE
 4480                        CONTINUE
                        END IF
 4470                CONTINUE
 4460             CONTINUE
!
                  IF ( IND_AUT_UV == 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J44, STR )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7647, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                     'of internal control: falure to make a reverse '// &
     &                     'lookup for observation '//STR(1:I_LEN(STR))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME )
                       FL_FAILURE = .TRUE.
                  END IF
                ELSE IF ( IND_SCA > 0 ) THEN
!
! --------------- Cross-correlation.
! --------------- Check fir the observation index
!
                  IF ( PIM%UV_IND(J44)%OBS_IND == 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J44, STR )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG ( 7648, IER, 'PIMA_SCAN_SPLIT', 'An orphan '// &
     &                     'observation '//STR(1:I_LEN(STR))//' was found: '//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME// &
     &                     ' no observation index' )
                       FL_FAILURE = .TRUE.
                  END IF
!
                  IND_OBS_UV = 0
!
                  DO 4500 J50=1,PIM%SCA(IND_SCA)%NSTA
                     DO 4510 J51=1,PIM%SCA(IND_SCA)%AUT_IND(J50)
                        IF ( PIM%OBS(J51)%STA_IND(1) == PIM%UV_IND(J44)%STA_IND(1) .AND. &
     &                       PIM%OBS(J51)%STA_IND(2) == PIM%UV_IND(J44)%STA_IND(2) ) THEN
                             MAX_NUM_OBS = MAX_I4 ( PIM%OBS(J51)%NUVS, PIM%OBS(J51)%NUM_EPC )
                             DO 4520 J52=1,MAX_NUM_OBS
                                DO 4530 J53=1,PIM%AUT(J51)%NUVS
                                   IF ( PIM%OBS(J51)%UV_IND(J52,J53) == J44 ) THEN
                                        IND_OBS_UV = J52
                                   END IF
 4530                           CONTINUE
 4520                        CONTINUE
                        END IF
 4510                CONTINUE
 4500             CONTINUE
!
                  IF ( IND_OBS_UV == 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J44, STR )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7649, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                     'of internal control: falure to make a reverse '// &
     &                     'lookup for observation '//STR(1:I_LEN(STR))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME )
                       FL_FAILURE = .TRUE.
                  END IF
!
                  IF ( PIM%UV_IND(J44)%POI_AUT_IND(1) .EQ. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J44, STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( PIM%UV_IND(J44)%OBS_IND, STR1 )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7650, IER, 'PIMA_SCAN_SPLIT', 'Trap '// &
     &                     'of internal control: index to the autocorrelation '// &
     &                     'array of the first station is zero for uv point #'// &
     &                      STR(1:I_LEN(STR))//', observation '// &
     &                      STR1(1:I_LEN(STR1))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME )
                       FL_FAILURE = .TRUE.
                  END IF
!
                  IF ( PIM%UV_IND(J44)%POI_AUT_IND(2) .EQ. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J44, STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( PIM%UV_IND(J44)%OBS_IND, STR1 )
                       CALL ERR_PASS ( IUER, IER )
                       CALL ERR_LOG  ( 7651, IER, 'PIMA_SCAN_SPLIT', 'Trap of '// &
     &                     'internal control: index to the autocorrelation '// &
     &                     'array of the second station is zero for uv point #'// &
     &                       STR(1:I_LEN(STR))//', observation '// &
     &                      STR1(1:I_LEN(STR1))//'  '//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME//'/'//  &
     &                      PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME//' ' // &
     &                      PIM%SOU(PIM%UV_IND(J44)%SOU_IND)%IVS_NAME )
                       FL_FAILURE = .TRUE.
                  END IF
             END IF
         END IF
 4440 CONTINUE
!
      IF ( PIM%CONF%CHECK_SEVERITY > 1 .AND. FL_FAILURE ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%CHECK_SEVERITY, STR )
           CALL ERR_LOG ( 7652, IUER, 'PIMA_SCAN_SPLIT', 'Fits files for '// &
     &         'experiment '//PIM%CONF%SESS_CODE//' did not pass test at '// &
     &         'severity check level '//STR )
           RETURN
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           IF ( N_BAD - N_BAD_BEG == 0 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_SCAN_SPLIT  No bad points have been found'
             ELSE
                WRITE ( 6, '(A,I7,A)' ) 'PIMA_SCAN_SPLIT  ', N_BAD - N_BAD_BEG, &
     &                ' bad points were added to the list'
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I6,1X,A)' ) 'PIMA_SCAN_SPLIT  End of work PIM%NSCA: ', &
     &                                 PIM%NSCA, GET_CDATE()
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           WRITE ( 6, '(A,I9)' ) 'PIMA_SCAN_SPLIT  End:           N_BAD= ', N_BAD
           CALL FLUSH ( 6 )
      END IF
      DEALLOCATE ( SS_TAB  )
      DEALLOCATE ( TIM_ARR )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SCAN_SPLIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_OBS_INIT ( NOBS, OBS )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_OBS_INIT initialize OBS object.            *
! *                                                                      *
! *  ### 12-JUN-2020 PIMA_OBS_INIT v1.0 (c)  L. Petrov  12-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INTEGER*4  NOBS
      TYPE     ( PIM_OBS__TYPE ) :: OBS(NOBS)
      INTEGER*4  J1, ISIZE
!
      DO 410 J1=1,NOBS
         ISIZE = LOC(OBS(J1)%REF_FRG_INDS(1)) - LOC(OBS(J1)%RES_MB_DEL(1,1))
         CALL NOUT ( ISIZE, OBS(J1)%RES_MB_DEL )
         OBS(J1)%REF_FRG_INDS = 0
         OBS(J1)%WVR_FLAG     = 0
         OBS(J1)%UV_IND       => NULL() ! UV indices
         OBS(J1)%CORR_FLAG    => NULL() ! Flag set by the correlator
         OBS(J1)%RES_FRN      => NULL() ! Uncalibrated postfit residuals
         OBS(J1)%USER_FLAG    => NULL() ! User-supplied time dependent weights
         OBS(J1)%UV           => NULL() ! Visibility data
         OBS(J1)%UV_IF        => NULL() ! Visibility data averaged over the IF
         OBS(J1)%UV_BAND      => NULL() ! Visibility data averaged over the band
         OBS(J1)%AC           => NULL() ! Autocorrelation data
         OBS(J1)%AC_AVR_TIM   => NULL() ! Autocorrelation data averaged over time PIM%NCHN,LFRQ,2,PIM%NSTK
         OBS(J1)%AC_MEAN      => NULL() ! Autocorrelation data averaged over time after sampling correction LFRQ,2,PIM%NSTK
         OBS(J1)%WEI_1D       => NULL() ! 1-dimension weights
         OBS(J1)%WVR_DELAY    => NULL() ! WVR delay
         OBS(J1)%TSRF         => NULL() ! Tsys Renormalization factor to account for discarding spactral channels LFRQ,2,PIM%NSTK
 410  CONTINUE 
      RETURN
      END  !#!  
