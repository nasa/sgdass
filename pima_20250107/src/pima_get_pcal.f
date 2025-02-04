      SUBROUTINE PIMA_GET_PCAL ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_PCAL parses FITS-IDI files and extracts          *
! *   information about phase calibration measurements.                  *
! *                                                                      *
! * ### 09-JAN-2006   PIMA_GET_PCAL  v4.13 (c) L. Petrov 25-JUL-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     )  :: PIM
      TYPE     ( PIM_PCAL__TYPE )  :: PCAL_TMP
      TYPE     ( PIM_CABLE__TYPE ) :: CAB_TMP
      INTEGER*4  IUER
      INTEGER*4  PIM__MPHC, PIM__MACP
      PARAMETER  ( PIM__MPHC = 32 ) ! maximum number of phase centers
      PARAMETER  ( PIM__MACP = 0  ) ! maximum nuymber of accumulation periods for checking common duration of obs of the same pointing with the different phase centers
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, &
     &           J26, J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, &
     &           J37, J38, J39, J40, LUN_PCA, N_MIS, &
     &           IS, IFRG, IFRQ, USE_REC(PIM__MSTA*PIM__MSCA), IOS, &
     &           IND_TAB(PIM__MFIL),  IND_ANT(PIM__MFIL), IND_SOU(PIM__MFIL),  &
     &           IND_TIME(PIM__MFIL), IND_CAB(PIM__MFIL), IND_SPAN(PIM__MFIL), &
     &           IND_FRQ(PIM__MFIL),  IND_FRG(PIM__MFIL), &
     &           IND_REA(PIM__MFIL),  IND_IMA(PIM__MFIL), IND_RAT(PIM__MFIL),  &
     &           IND_REA_2(PIM__MFIL),  IND_IMA_2(PIM__MFIL), IND_RAT_2(PIM__MFIL), &
     &           STA_ID, SOU_ID, FRG_ID, NO_TONES, LP, IL, NPCAL, IND_OBS_FRG, &
     &           IND_PCAL, KP(PIM__MFIL), IND_FREQ_MAX, L_FRQ, I_FRQ, &
     &           IND_BEG_FRQ1, IND_END_FRQ1, IND_BEG_FRQ2, IND_END_FRQ2, &
     &           NO_TONES_OLD, NFRG, N_PHC, IND_SOU_PHC(PIM__MPHC), IER
      REAL*4     PCAL_REA(PIM__MTON*PIM__MFRQ), PCAL_IMA(PIM__MTON*PIM__MFRQ)
      REAL*4     PCAL_REA_2(PIM__MTON*PIM__MFRQ), PCAL_IMA_2(PIM__MTON*PIM__MFRQ)
      REAL*4     PCAL_PHS(PIM__MTON*PIM__MFRQ),   PCAL_AMP(PIM__MTON*PIM__MFRQ)
      REAL*4     PCAL_PHS_2(PIM__MTON*PIM__MFRQ), PCAL_AMP_2(PIM__MTON*PIM__MFRQ)
      REAL*4     R4_VAL
      REAL*8     R8_VAL, CHN_MGN
      REAL*8     PCAL_FREQ_R8(PIM__MTON*PIM__MFRQ),  &
     &           PCAL_RATE_R8(PIM__MTON*PIM__MFRQ),  &
     &           PCAL_RATE_2_R8(PIM__MTON*PIM__MFRQ)
      REAL*8     EPS_TIME, EPS_FREQ
      PARAMETER  ( EPS_TIME  = 0.1D0 ) ! sec
      PARAMETER  ( EPS_FREQ  = 1.0D3 ) ! Hz
      REAL*8     TIM_PCAL_BEG, TIM_PCAL_END, FREQ_BEG, FREQ_END, FREQ_DIF, FREQ_DIF_MAX, &
     &           TIM_OBS_BEG, TIM_OBS_END, TIM_EPC_DIF, TIM_EPC_DIF_MIN
      REAL*8,    ALLOCATABLE :: ARR1(:), ARR2(:)
      REAL*8       PIM__CHAN_WIDTH_MIN
      PARAMETER  ( PIM__CHAN_WIDTH_MIN = 2.4999D5 )
      LOGICAL*4  FL_FRQ_SAME, FL_ZERO_FRQ, FL_PRINT_RAW_PCAL
      CHARACTER  FIL_PCA*128, STR_DATE(3)*32, STR*128, STR1*128, STR2*128, STR3*128, &
     &           STR_PCAL_NOAVR*8
      LOGICAL*4, EXTERNAL :: IS_R8_INF, IS_R8_NAN, IS_R4_NAN, IS_R4_INF, &
     &           ARR_R8_IS_ZERO, ARR_R4_HAS_NAN, ARR_R8_HAS_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, PCAL_FRQ_FIX, GET_UNIT, IFIND_PL
!
      FL_PRINT_RAW_PCAL = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_PRINT_RAW_PCAL', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_PRINT_RAW_PCAL = .TRUE.
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_PCAL Started  on '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
      CALL CLRCH    ( STR_PCAL_NOAVR )
      CALL GETENVAR ( 'PIMAVAR_PCAL_NOAVR', STR_PCAL_NOAVR )
      IF ( ILEN(STR_PCAL_NOAVR) > 0 ) THEN
           CALL TRAN ( 11, STR_PCAL_NOAVR, STR_PCAL_NOAVR )
           IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                WRITE ( 6, * ) 'PIMA_GET_PCAL PIMAVAR_PCAL_NOAVR: '//STR_PCAL_NOAVR
           END IF
      END IF
!
      LUN_PCA = GET_UNIT()
      FIL_PCA = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'.pcl'
      OPEN ( UNIT=LUN_PCA, FILE=FIL_PCA, IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 7411, IUER, 'PIMA_GET_PCAL', 'Error in openning '// &
     &         'output phase calibration file '//FIL_PCA )
           RETURN 
      END IF
!
      NPCAL = 0
      IND_TAB   = 0
      IND_ANT   = 0
      IND_SOU   = 0
      IND_TIME  = 0 
      IND_CAB   = 0 
      IND_SPAN  = 0 
      IND_FRQ   = 0 
      IND_FRG   = 0
      IND_REA   = 0 
      IND_IMA   = 0 
      IND_RAT   = 0
      IND_REA_2 = 0
      IND_IMA_2 = 0
      IND_RAT_2 = 0
      NO_TONES_OLD = 0
!
      DO 410 J1=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J1)%NAME, PIM%FILE(J1)%FITS_DESC, &
     &                     'OLD', IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7412, IUER, 'PIMA_GET_PCAL', 'Error in an '// &
     &            'attempt to open FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
!
         IND_FRQ(J1)   = -1
         IND_IMA(J1)   = -1
         IND_REA(J1)   = -1
         IND_IMA_2(J1) = -1
         IND_REA_2(J1) = -1
!
         DO 420 J2=1,PIM%FILE(J1)%L_HDR
            DO 430 J3=1,PIM%FILE(J1)%L_KWD(J2)
               IF ( PIM%FILE(J1)%KEY(J3,J2)(1:8) == 'EXTNAME' ) THEN
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'PHASE-CAL'" ) THEN
                         IND_TAB(J1) = J2
                    END IF
               END IF
               IF ( IND_TAB(J1) == J2 ) THEN
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'ANTENNA_NO'" ) THEN
                         IND_ANT(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'SOURCE_ID'" ) THEN
                         IND_SOU(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'FREQID  ' " ) THEN
                         IND_FRG(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'TIME    '" ) THEN
                         IND_TIME(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:25) == "'TIME_INTERVAL'" ) THEN
                         IND_SPAN(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'CABLE_CAL'" ) THEN
                         IND_CAB(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_FREQ_1'" ) THEN
                         IND_FRQ(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_REAL_1'" ) THEN
                         IND_REA(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_IMAG_1'" ) THEN
                         IND_IMA(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_RATE_1'" ) THEN
                         IND_RAT(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_REAL_2'" ) THEN
                         IND_REA_2(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_IMAG_2'" ) THEN
                         IND_IMA_2(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'PC_RATE_2'" ) THEN
                         IND_RAT_2(J1) = J3
                    END IF
               END IF
 430        CONTINUE
 420     CONTINUE
!
         IF ( IND_TAB(J1) == 0 ) THEN
              IF ( PIM%CONF%WARNING ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_GET_PCAL: no pcal table was '// &
     &                                'found in uv-file '// &
     &                     PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))
              END IF
           ELSE
              IF ( IND_ANT(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7413, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'ANTENNA_NO was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_CAB(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7414, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'CABLE_CAL was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_FRQ(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7415, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'PC_FREQ_1 was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_REA(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7416, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'PC_REAL_1 was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_IMA(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7417, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'PC_IMAG_1 was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_RAT(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7418, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'PC_RATE_1 was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_TIME(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7419, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'TIME was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_FRG(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7420, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'FREQID was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_SPAN(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7421, IUER, 'PIMA_GET_PCAL', 'Keyword '// &
     &                 'TIME_INTERVAL was not found in the PHASE-CAL table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, 'PHASE-CAL', 'NAXIS2', KP(J1), &
     &                               IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7422, IUER, 'PIMA_GET_PCAL', 'Failure to '// &
     &                 'get the number of points of phase-cal measurements '// &
     &                 'in FITS-IDI file '//PIM%FILE(J1)%NAME  )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, 'PHASE-CAL', 'NO_TONES', &
     &                               NO_TONES, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7423, IUER, 'PIMA_GET_PCAL', 'Failure to '// &
     &                 'get the number of tones of phase-cal measurements '// &
     &                 'in FITS-IDI file '//PIM%FILE(J1)%NAME  )
                   RETURN
              END IF
              IF ( NO_TONES > PIM__MTON ) THEN
                   CALL CLRCH ( STR  ) 
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( NO_TONES,  STR  ) 
                   CALL INCH  ( PIM__MTON, STR1 ) 
                   CALL ERR_LOG ( 7424, IUER, 'PIMA_GET_PCAL', 'Too many '// &
     &                 'tones of phase-cal measurements '// &
     &                 'in FITS-IDI file '// &
     &                  PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))// &
     &                 ' more than '//STR(1:I_LEN(STR))//' while '// &
     &                 'the maximum number of tones defeined in '// &
     &                 'variable PIM__MTON is '//STR1 )
                   RETURN
              END IF
              IF ( NO_TONES_OLD > 0 .AND. NO_TONES .NE. NO_TONES_OLD ) THEN
                   CALL CLRCH ( STR  ) 
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( NO_TONES,  STR  ) 
                   CALL INCH  ( NO_TONES_OLD, STR1 ) 
                   IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                        CALL ERR_LOG ( 7425, IUER, 'PIMA_GET_PCAL', 'The '// &
     &                      'number of phase-cal tones in file '// &
     &                       PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))// &
     &                      ' -- '//STR(1:I_LEN(STR))//' is different than '// &
     &                      'the number of phase-cal tones: '//STR1(1:I_LEN(STR1))// &
     &                      ' from the previous file' )
                         RETURN
                      ELSE 
                         IER = IUER
                         CALL ERR_LOG ( 7426, IER, 'PIMA_GET_PCAL', 'The '// &
     &                      'number of phase-cal tones in file '// &
     &                       PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))// &
     &                      ' -- '//STR(1:I_LEN(STR))//' is different than '// &
     &                      'the number of phase-cal tones: '//STR1(1:I_LEN(STR1))// &
     &                      ' from the previous file. Nevertheless, continue' )
                         WRITE ( 6, * ) 'Nevertheless, continue'
                   END IF
              END IF
              NO_TONES_OLD = NO_TONES
!
              NPCAL = NPCAL + KP(J1)
!
              DO 440 J4=1,KP(J1)
                 USE_REC(J4) = 1
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1), J4,   &
     &                              PIM%FILE(J1)%KEY(IND_ANT(J1),IND_TAB(J1)), &
     &                              1, STA_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 7427, IUER, 'PIMA_GET_PCAL', 'Error in '// &
     &                    'getting station id for the '//STR(1:I_LEN(STR))// &
     &                    '-th PCAL of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 IF ( STA_ID .LE. 0  .OR.  STA_ID > PIM%NSTA ) THEN
                      IF ( PIM%CONF%WARNING ) THEN
                           WRITE ( 6, 210 ) J1, PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME)), &
     &                                      STA_ID, PIM%NSTA, J4
 210                       FORMAT ( 'PHASE_CAL for non-existent station ', &
     &                              'in the ', I4, '-th file ', A, ' STA_ID: ', &
     &                               I3, ' PIM%NSTA: ',I3, ' record in the ', &
     &                              'PHASE_CAL table: ',I4 )
                      END IF
                      USE_REC(J4) = 0
                      GOTO 440
                 END IF
!
                 IF ( PIM%REF_STA(STA_ID,J1) .LE. 0  .OR. &
     &                PIM%REF_STA(STA_ID,J1) > PIM%NSTA   ) THEN
                      IF ( PIM%CONF%WARNING ) THEN
                           WRITE ( 6, 220 ) J1, PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME)), &
     &                                      STA_ID, PIM%REF_STA(STA_ID,J1), J4
 220                       FORMAT ( 'PHASE_CAL for station with non-existent index ', &
     &                              'in the ', I4, '-th file ', A, ' STA_ID: ', &
     &                               I3, ' REF_STA: ',I3, ' record in the ', &
     &                              'PHASE_CAL table: ',I4 )
                      END IF
                      USE_REC(J4) = 0
                      GOTO 440
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1),   &
     &                      J4, PIM%FILE(J1)%KEY(IND_FRG(J1),IND_TAB(J1)), &
     &                      1, FRG_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J7, STR )
                      CALL ERR_LOG ( 7428, IUER, 'PIMA_GET_PCAL', &
     &                    'Error in getting frequency group id for the '// &
     &                     STR(1:I_LEN(STR))//'-th PCAL of the '// &
     &                    'FITS-IDI file '//PIM%FILE(J6)%NAME )
                     RETURN
                 END IF
                 IFRG = PIM%FILE(J1)%REF_FRG(1,FRG_ID)
                 IF ( PIM%VIRT_NFRG > 1 ) THEN
                      IF ( IFRG < 1 .OR. IFRG > PIM%VIRT_NFRG ) GOTO 440
                    ELSE
                      IF ( IFRG < 1 .OR. IFRG > PIM%NFRG ) GOTO 440
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1),   &
     &                         J4, PIM%FILE(J1)%KEY(IND_SOU(J1),IND_TAB(J1)), &
     &                         1, SOU_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J7, STR )
                      CALL ERR_LOG ( 7429, IUER, 'PIMA_GET_PCAL', &
     &                    'Error in getting source id for the '// &
     &                     STR(1:I_LEN(STR))//'-th PCAL of the '// &
     &                    'FITS-IDI file '//PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
                 IF ( SOU_ID .LE. 0 ) THEN
                      WRITE ( 6, 230 ) 'YYYY-1 NaN', PIM%STA(PIM%REF_STA(STA_ID,J1))%IVS_NAME, &
     &                                 J4          
 230                  FORMAT ( 'PIMA_GET_PCAL ', A, ' Sta: ',A, ' No SOU_ID ', &
     &                         ' Ind: ', I5 )
                      USE_REC(J4) = 0
                      GOTO 440
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1),   &
     &                         J4, PIM%FILE(J1)%KEY(IND_FRQ(J1),IND_TAB(J1)), &
     &                         NO_TONES*PIM%FILE(J1)%NFRQ, PCAL_FREQ_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 7430, IUER, 'PIMA_GET_PCAL', &
     &                    'Error in getting '//STR(1:I_LEN(STR))// &
     &                    '-th point of pcal frequency from the FITS-IDI '// &
     &                    'file '//PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!@                 IF ( PIM%CONF%DEBUG_LEVEL == 11 ) THEN
!@                      WRITE ( 16, * ) 'J4= ', INT2(J4), ' STA_ID= ', STA_ID, ' Sta: ', PIM%STA(PIM%REF_STA(STA_ID,J1))%IVS_NAME, ' PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J1)%NFRQ)
!@                 END IF
!
                 IF ( ARR_R8_HAS_NAN ( NO_TONES*PIM%FILE(J1)%NFRQ, PCAL_FREQ_R8  ) ) THEN
                      IS = PCAL_FRQ_FIX ( NO_TONES, PIM%FILE(J1)%NFRQ, PCAL_FREQ_R8 )
                    ELSE
                      IS = 0
                 END IF 
!
                 IF ( IS == 1 ) THEN 
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                           WRITE ( 6, 240 ) 'YYYY-2 NaN ', &
     &                                       PIM%STA(PIM%REF_STA(STA_ID,J1))%IVS_NAME, &
     &                                       PIM%C_SOU(SOU_ID), J4          
                           WRITE ( 6, * ) ' PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J1)%NFRQ)
 240                       FORMAT ( 'PIMA_GET_PCAL ', A, ' Sta: ',A, ' Sou: ', A, &
     &                              ' Ind: ', I5 )
                      END IF
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           WRITE (  6, * ) 'NO_TONES= ', NO_TONES
                           WRITE (  6, * ) 'PIM%FILE(J1)%NFRQ = ', PIM%FILE(J1)%NFRQ
                           WRITE (  6, * ) 'PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J1)%NFRQ)
                      END IF
!
                      USE_REC(J4) = 0
                      GOTO 440
                 END IF
                 IF ( ARR_R8_IS_ZERO ( NO_TONES*PIM%FILE(J1)%NFRQ, PCAL_FREQ_R8  ) ) THEN
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                           WRITE ( 6, 240 ) 'YYYY-3 Zero', PIM%STA(PIM%REF_STA(STA_ID,J1))%IVS_NAME, &
     &                                      PIM%C_SOU(SOU_ID), J4          
                      END IF
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           WRITE (  6, * ) 'NO_TONES= ', NO_TONES
                           WRITE (  6, * ) 'PIM%FILE(J1)%NFRQ = ', PIM%FILE(J1)%NFRQ
                           WRITE (  6, * ) 'PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J1)%NFRQ)
                      END IF
                      USE_REC(J4) = 0
                      GOTO 440
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J1)%FITS_DESC,  IND_TAB(J1),  &
     &                              J4, PIM%FILE(J1)%KEY(IND_REA(J1),IND_TAB(J1)), &
     &                              NO_TONES*PIM%FILE(J1)%NFRQ, PCAL_REA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 7431, IUER, 'PIMA_GET_PCAL', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of pcal real part from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J1)%FITS_DESC,  IND_TAB(J1),  &
     &                              J4, PIM%FILE(J1)%KEY(IND_IMA(J1),IND_TAB(J1)), &
     &                              NO_TONES*PIM%FILE(J1)%NFRQ, PCAL_IMA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 7432, IUER, 'PIMA_GET_PCAL', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of pcal real part from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J1)%NAME )
                     RETURN
                 END IF
!
! -------------- Replace NaN or Inf with a very small pcal amplitude
!
                 DO 450 J5=1,NO_TONES*PIM%FILE(J1)%NFRQ
                    IF ( IS_R4_NAN(PCAL_REA(J5)) .OR. &
     &                   IS_R4_NAN(PCAL_IMA(J5)) .OR. &
     &                   IS_R4_INF(PCAL_REA(J5)) .OR. &
     &                   IS_R4_INF(PCAL_IMA(J5))      ) THEN
!
                         PCAL_REA(J5) = PIMA__PCAL_AMP_MIN/10.0
                         PCAL_IMA(J5) = 0.0
                    END IF
!
                    IF ( ABS(PCAL_REA(J5))  > PIMA__PCAL_AMP_MAX .OR. &
     &                   ABS(PCAL_IMA(J5))  > PIMA__PCAL_AMP_MAX      ) THEN
                         PCAL_REA(J5) = PIMA__PCAL_AMP_MIN/10.0
                         PCAL_IMA(J5) = 0.0
                    END IF
 450             CONTINUE 
!
                 PIM%STA(PIM%REF_STA(STA_ID,J1))%PCAL(IFRG)%NPOI = &
     &                   PIM%STA(PIM%REF_STA(STA_ID,J1))%PCAL(IFRG)%NPOI + 1
 440          CONTINUE
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J1)%FITS_DESC, IER ) 
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7433, IUER, 'PIMA_GET_PCAL', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
 410  CONTINUE
!
      PIM%NPCT = 0
      IF ( PIM%FRG_USE == PIMA__SINGLE .OR. PIM%FRG_USE == PIMA__COMBINED ) THEN
           NFRG = PIM%NFRG
        ELSE 
           NFRG = PIM%VIRT_NFRG 
      END IF
      DO 460 J6=1,NFRG
        DO 470 J7=1,PIM%NSTA
           PIM%STA(J7)%PCAL(J6)%ISTA = J7
           IF ( PIM%STA(J7)%PCAL(J6)%NPOI > 0 ) THEN
!
              PIM%STA(J7)%PCAL(J6)%PCAL_AVAIL = .TRUE.
              PIM%STA(J7)%PCAL(J6)%PCAL_USE   = .TRUE.
              PIM%STA(J7)%PCAL(J6)%NO_TONES   = NO_TONES
              PIM%STA(J7)%PCAL(J6)%NPOL       = PIM%NPOL
              PIM%STA(J7)%CABLE%NPOI          = PIM%STA(J7)%PCAL(J6)%NPOI
              PIM%STA(J7)%CABLE%CAB_AVAIL     = .TRUE.
              PIM%STA(J7)%CABLE%MEAN_CABLE    = 0.0D0
              PIM%NPCT = MAX ( PIM%NPCT, PIM%STA(J7)%PCAL(J6)%NO_TONES )
!
! ----------- NB: sign: it is negative. As of 2009.08.05, I do not know
! ----------- why it should be negative.
!
              PIM%STA(J7)%CABLE%CABLE_SIGN = -1 ! Do not know why. Tradition?
!
              PIM%STA(J7)%PCAL(J6)%PCAL_SCA = .FALSE.
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%FREQ(NO_TONES,PIM%NFRQ, &
     &                                         PIM%STA(J7)%PCAL(J6)%NPOI)   )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%AMPL(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%PHAS(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%RATE(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL)   )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%IAMB(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOL)   )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%IPOI_SCA(PIM%STA(J7)%PCAL(J6)%NPOI) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%ISCA_POI(PIM%STA(J7)%PCAL(J6)%NPOI) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%AMPL_RGR(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%PHAS_RGR(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
!
! ----------- NB: we allocate more memory for PHAS_SCA, AMPL_SCA, and PRAT_SCA
! -----------     because we do not know the number of scans at this point
!
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%PHAS_SCA(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%AMPL_SCA(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%PRAT_SCA(NO_TONES,PIM%NFRQ, &
     &                   PIM%STA(J7)%PCAL(J6)%NPOI,PIM%STA(J7)%PCAL(J6)%NPOL) )
              ALLOCATE ( PIM%STA(J7)%CABLE%TIM_CAB(PIM%STA(J7)%CABLE%NPOI)  )
              ALLOCATE ( PIM%STA(J7)%CABLE%CAB_DEL(PIM%STA(J7)%CABLE%NPOI)  )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%TIME_MID_R8 (PIM%STA(J7)%PCAL(J6)%NPOI) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%TIME_SCA_R8 (PIM%STA(J7)%PCAL(J6)%NPOI) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(PIM%STA(J7)%PCAL(J6)%NPOI) )
              ALLOCATE ( PIM%STA(J7)%PCAL(J6)%SOU_IND(PIM%STA(J7)%PCAL(J6)%NPOI) )
!
              PIM%STA(J7)%PCAL(J6)%PHAS = 0.0
              PIM%STA(J7)%PCAL(J6)%AMPL = 0.0
              PIM%STA(J7)%PCAL(J6)%PHAS_RGR = 0.0
              PIM%STA(J7)%PCAL(J6)%AMPL_RGR = 0.0
              PIM%STA(J7)%PCAL(J6)%PHAS_SCA = 0.0
              PIM%STA(J7)%PCAL(J6)%AMPL_SCA = 0.0
              PIM%STA(J7)%PCAL(J6)%PRAT_SCA = 0.0
              PIM%STA(J7)%PCAL(J6)%FREQ = 0.0D0
              PIM%STA(J7)%PCAL(J6)%RATE = 0.0D0
              PIM%STA(J7)%PCAL(J6)%IAMB = 0
              PIM%STA(J7)%PCAL(J6)%IPOI_SCA = 0
              PIM%STA(J7)%PCAL(J6)%ISCA_POI = 0
              PIM%STA(J7)%PCAL(J6)%SOU_IND = 0
              PIM%STA(J7)%PCAL(J6)%TIME_MID_R8  = 0.0D0
              PIM%STA(J7)%PCAL(J6)%TIME_SCA_R8  = 0.0D0
              PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4 = 0.0
              PIM%STA(J7)%CABLE%TIM_CAB  = 0.0D0
              PIM%STA(J7)%CABLE%CAB_DEL  = 0.0D0
!
            LP = 0
            DO 480 J8=1,PIM%L_FIL
!
! ------------ Open the J8-th fits file
!
               CALL ERR_PASS   ( IUER, IER )
               CALL FFITS_OPEN ( PIM%FILE(J8)%NAME, &
     &                           PIM%FILE(J8)%FITS_DESC, 'OLD', IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7434, IUER, 'PIMA_GET_PCAL', &
     &                  'Error in an attempt to open FITS UV-file '// &
     &                   PIM%FILE(J8)%NAME )
                    RETURN
               END IF
!
! ------------ ... abnd read KP(J8) records from it
!
               DO 490 J9=1,KP(J8)
!
! --------------- Get frequency group ID
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),   &
     &                       J9, PIM%FILE(J8)%KEY(IND_FRG(J8),IND_TAB(J8)), &
     &                       1, FRG_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J9, STR )
                       CALL ERR_LOG ( 7435, IUER, 'PIMA_GET_PCAL', &
     &                     'Error in getting frequency group id for the '// &
     &                      STR(1:I_LEN(STR))//'-th PCAL of the '// &
     &                     'FITS-IDI file '//PIM%FILE(J8)%NAME )
                      RETURN
                  END IF
!
! --------------- We currently consider only points for the J6-th
! --------------- frequency group
!
                  IF ( PIM%FILE(J8)%REF_FRG(1,FRG_ID) .NE. J6 ) GOTO 490
                  IF ( USE_REC(J9) == 0 ) GOTO 490
!
! --------------- Get station ID
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8), J9,   &
     &                               PIM%FILE(J8)%KEY(IND_ANT(J8),IND_TAB(J8)), &
     &                               1, STA_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J9, STR )
                       CALL ERR_LOG ( 7436, IUER, 'PIMA_GET_PCAL', 'Error '// &
     &                     'in getting station id for the '// &
     &                      STR(1:I_LEN(STR))//'-th PCAL of the FITS-IDI '// &
     &                     'file '//PIM%FILE(J8)%NAME )
                      RETURN
                  END IF
!
                  IF ( PIM%REF_STA(STA_ID,J8) .EQ. J7  .AND. LP < PIM%STA(J7)%PCAL(J6)%NPOI ) THEN
                     LP = LP + 1 ! LP is the index of the used record
!
! ------------------ Get UTC time tag at the middle epoch of the pcal
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR8 ( PIM%FILE(J8)%FITS_DESC,   IND_TAB(J8),  &
     &                          J9, PIM%FILE(J8)%KEY(IND_TIME(J8),IND_TAB(J8)), &
     &                          1, PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          CALL ERR_LOG ( 7437, IUER, 'PIMA_GET_PCAL',  &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of time tag from the FITS-IDI file '// &
     &                         PIM%FILE(J8)%NAME )
                         RETURN
                     END IF
!
! ------------------ Convert UTC time tag to TAI
!
                     IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                          CONTINUE 
                       ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                          PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) = &
     &                        PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) - PIM%UTC_MTAI/86400.0D0
                     END IF
!
! ------------------ Convert time to amount of seconds elapsed from
! ------------------ MJD_0, TAI_0
!
                     PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) = &
     &                       PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP)*86400.0D0 - &
     &                       (PIM%MJD_0 - PIM%FILE(J8)%MJD_REF)*86400.D0 - &
     &                       PIM%TAI_0
                     PIM%STA(J7)%CABLE%TIM_CAB(LP) = PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP)
!
! ------------------ Get the pcal averaging interval duration
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J8)%FITS_DESC,   IND_TAB(J8),  &
     &                          J9, PIM%FILE(J8)%KEY(IND_SPAN(J8),IND_TAB(J8)), &
     &                          1, PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          CALL ERR_LOG ( 7438, IUER, 'PIMA_GET_PCAL', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of time span from the FITS-IDI file '// &
     &                         PIM%FILE(J8)%NAME )
                         RETURN
                     END IF
!
! ------------------ ... and convert it to seconds
!
                     PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP) = &
     &                           PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP)*86400.0E0
!
! ------------------ Get the source ID
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),   &
     &                          J9, PIM%FILE(J8)%KEY(IND_SOU(J8),IND_TAB(J8)), &
     &                          1, SOU_ID, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          CALL ERR_LOG ( 7439, IUER, 'PIMA_GET_PCAL', &
     &                        'Error in getting source id for the '// &
     &                         STR(1:I_LEN(STR))//'-th PCAL of the '// &
     &                        'FITS-IDI file '//PIM%FILE(J8)%NAME )
                          RETURN
                     END IF
                     IF ( SOU_ID == 0 ) THEN
                          IER = -1
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          CALL ERR_LOG ( 7440, IER, 'PIMA_GET_PCAL', &
     &                        'FITS-IDI file '//PIM%FILE(J8)%NAME//' is broken: '// &
     &                        'zero source ID was found in record '// &
     &                        STR(1:I_LEN(STR))//' -- skipping ill record '// &
     &                        'and continue' )
                          LP = LP - 1
                          GOTO 490
                     END IF
                     PIM%STA(J7)%PCAL(J6)%SOU_IND(LP) = PIM%REF_SOU(SOU_ID,J8)
!
! ------------------ Format the date strings for the dump output
!
                     STR_DATE(1) = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                               PIM%TAI_0 + PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) &
     &                               - PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP)/2.0, -2 )
                     STR_DATE(2) = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                               PIM%TAI_0 + PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) &
     &                               + PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP)/2.0, -2 )
                     STR_DATE(3) = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                              PIM%TAI_0 + PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP), -2 )
!
                     WRITE ( LUN_PCA, 250 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                      J8, J9, STR_DATE(1)(1:21), STR_DATE(2)(1:21), &
     &                                      PIM%FILE(J8)%NFRQ, NO_TONES, TRIM(PIM%FILE(J8)%NAME) 
 250                 FORMAT ( 'PIMA_GET_PCAL-REC  Sta: ', A, ' Sou: ', A, ' Fil_ind: ', I5, &
     &                        ' Rec_ind: ', I5, ' Utc_range:   [ ', A, ' , ', A, ' ] ', &
     &                        ' N_Frq: ', I4, ' No_tones: ', I4, ' Fil_nam: ', A )
!
! ------------------ Get cable calibration delay
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR8 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),   &
     &                          J9, PIM%FILE(J8)%KEY(IND_CAB(J8),IND_TAB(J8)), &
      &                         1, PIM%STA(J7)%CABLE%CAB_DEL(LP), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          CALL ERR_LOG ( 7441, IUER, 'PIMA_GET_PCAL', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of cable calibration from the '// &
     &                        'FITS-IDI file '//PIM%FILE(J8)%NAME )
                         RETURN
                     END IF
                     IF ( IS_R8_INF ( PIM%STA(J7)%CABLE%CAB_DEL(LP) ) ) PIM%STA(J7)%CABLE%CAB_DEL(LP) = 0.0D0
                     IF ( IS_R8_NAN ( PIM%STA(J7)%CABLE%CAB_DEL(LP) ) ) PIM%STA(J7)%CABLE%CAB_DEL(LP) = 0.0D0
!
! ------------------ Get pcal frequency
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR8 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),   &
     &                          J9, PIM%FILE(J8)%KEY(IND_FRQ(J8),IND_TAB(J8)), &
     &                          NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_FREQ_R8, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                               CALL ERR_LOG ( 7442, IUER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal frequency from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               RETURN
                             ELSE
                               IER = IUER
                               CALL ERR_LOG ( 7443, IER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal frequency from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               WRITE ( 6, * ) 'Nevertheless, continue'
                               PCAL_FREQ_R8 = 0.0D0
                          END IF
                     END IF
!
                     IF ( FL_PRINT_RAW_PCAL ) THEN
!
! ----------------------- Print the raw frequency table
!
                          WRITE ( LUN_PCA, 260 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                           J8, J9, STR_DATE(3)(1:21), 'Raw_pcal_freq:  ', &
     &                                           PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
 260                      FORMAT ( 'PIMA_GET_PCAL-FRQ  Sta: ', A, ' Sou: ', A, ' Fil_ind: ', I5, &
     &                             ' Rec_ind: ', I5, ' Tai_mid_epoch: ', A, 2X, A, 1X, 8192(1PD15.7, 1X) )
                     END IF
                     IF ( ARR_R8_IS_ZERO ( NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_FREQ_R8  ) ) THEN
                          WRITE ( 6, * ) 'PIMA_GET_PCAL: all tones are zero J6= ',J6, ' J7= ', J7
                     END IF
!
! ------------------ Check whether PCAL frequency has a NaN value
!
                     IF ( ARR_R8_HAS_NAN ( NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_FREQ_R8  ) ) THEN
!
! ----------------------- Try to fix it
!
                          IS = PCAL_FRQ_FIX ( NO_TONES, PIM%FILE(J8)%NFRQ, PCAL_FREQ_R8 )
                        ELSE
                          IS = 0
                     END IF 
                     IF ( IS == 1 ) THEN 
!
! ----------------------- An attempt to fix NaN (i.e. extrapolate) failed
!
                          IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                               WRITE ( 6, 240 ) 'YYYY-5 NaN ', &
     &                                           PIM%STA(PIM%REF_STA(STA_ID,J8))%IVS_NAME, &
     &                                           PIM%C_SOU(SOU_ID), J4          
                               WRITE ( 6, * ) ' PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J8)%NFRQ)
                          END IF
                          IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                               WRITE (  6, * ) 'NO_TONES= ', NO_TONES
                               WRITE (  6, * ) 'PIM%FILE(J8)%NFRQ = ', PIM%FILE(J8)%NFRQ
                               WRITE (  6, * ) 'PCAL_FREQ_R8= ', PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J8)%NFRQ)
                          END IF
!
                          USE_REC(J8) = 0
                          GOTO 480
                     END IF
!
! ------------------ Print the fixed frequency table
!
                     WRITE ( LUN_PCA, 260 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                      J8, J9, STR_DATE(3)(1:21), 'Fixed_pcal_freq:', &
     &                                      PCAL_FREQ_R8(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
!
! ------------------ Get phase calibration rate
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR8 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),   &
     &                          J9, PIM%FILE(J8)%KEY(IND_RAT(J8),IND_TAB(J8)), &
     &                          NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_RATE_R8, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                               CALL ERR_LOG ( 7444, IUER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal rate from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               RETURN
                             ELSE
                               IER = IUER
                               CALL ERR_LOG ( 7445, IER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal rate from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               WRITE ( 6, * ) 'Nevertheless, continue'
                               PCAL_RATE_R8 = 0.0D0
                          END IF
                     END IF
!
! ------------------ Get real part of phase calibration signal
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J8)%FITS_DESC,  IND_TAB(J8),  &
     &                          J9, PIM%FILE(J8)%KEY(IND_REA(J8),IND_TAB(J8)), &
     &                          NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_REA, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                               CALL ERR_LOG ( 7446, IUER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal real part from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               RETURN
                             ELSE
                               IER = IUER
                               CALL ERR_LOG ( 7447, IER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal rate from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J8)%NAME )
                               WRITE ( 6, * ) 'Nevertheless, continue'
                               PCAL_REA = 0.0D0
                          END IF
                     END IF
!
! ------------------ Get image part of phase calibration signal
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J8)%FITS_DESC,  IND_TAB(J8),  &
     &                          J9, PIM%FILE(J8)%KEY(IND_IMA(J8),IND_TAB(J8)), &
     &                          NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_IMA, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J9, STR )
                          IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                               CALL ERR_LOG ( 7448, IUER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal image part from the '// &
     &                             'FITS-IDI file '//PIM%FILE(J8)%NAME )
                               RETURN
                             ELSE
                               IER = IUER
                               CALL ERR_LOG ( 7449, IER, 'PIMA_GET_PCAL', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of pcal image part from the '// &
     &                             'FITS-IDI file '//PIM%FILE(J8)%NAME )
                               WRITE ( 6, * ) 'Nevertheless, continue'
                               PCAL_IMA = 0.0D0
                          END IF
                     END IF
!
! ------------------ Convert real/image pcal to phase/amplitude
!
                     DO 4100 J10=1,NO_TONES*PIM%FILE(J8)%NFRQ
                        IF ( IS_R4_NAN(PCAL_REA(J10)) .OR. &
     &                       IS_R4_NAN(PCAL_IMA(J10)) .OR. &
     &                       IS_R4_INF(PCAL_REA(J10)) .OR. &
     &                       IS_R4_INF(PCAL_IMA(J10))      ) THEN
!
                             PCAL_REA(J10) = PIMA__PCAL_AMP_MIN/10.0
                             PCAL_IMA(J10) = 0.0
                        END IF
                        IF ( ABS(PCAL_REA(J10))  > PIMA__PCAL_AMP_MAX .OR. &
     &                       ABS(PCAL_IMA(J10))  > PIMA__PCAL_AMP_MAX      ) THEN
                             PCAL_REA(J10) = PIMA__PCAL_AMP_MIN/10.0
                             PCAL_IMA(J10) = 0.0
                        END IF
!
                        IF ( ABS(PCAL_REA(J10)) > PIMA__PCAL_AMP_MIN .OR. &
     &                       ABS(PCAL_IMA(J10)) > PIMA__PCAL_AMP_MIN      ) THEN
!
                             PCAL_PHS(J10) = ATAN_CS_R4 ( PCAL_REA(J10), PCAL_IMA(J10)   )
                             PCAL_AMP(J10) = SQRT ( PCAL_REA(J10)**2  + PCAL_IMA(J10)**2 )
                           ELSE
                             PCAL_PHS(J10) = 0.0
                             PCAL_AMP(J10) = 0.0
                        END IF
 4100                CONTINUE 
!
! ------------------ Print phase/amplitude of phase calibration in the output dump file
!
                     WRITE ( LUN_PCA, 280 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                      J8, J9, 1, STR_DATE(3)(1:21), LP, &
     &                                      PCAL_PHS(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
                     WRITE ( LUN_PCA, 290 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                      J8, J9, 1, STR_DATE(3)(1:21), LP, &
     &                                      PCAL_AMP(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
 280                 FORMAT ( 'PIMA_GET_PCAL-PHS  Sta: ', A, ' Sou: ', A, ' Fil_ind: ', I5, &
     &                        ' Rec_ind: ', I5, ' Pol_ind: ', I1, ' Tai_mid_epoch: ', A, 1X, &
     &                        ' Pcal_ind: ', I5, ' Pcal_phs: ', 8192(F9.6, 1X) )
 290                 FORMAT ( 'PIMA_GET_PCAL-AMP  Sta: ', A, ' Sou: ', A, ' Fil_ind: ', I5, &
     &                        ' Rec_ind: ', I5, ' Pol_ind: ', I1, ' Tai_mid_epoch: ', A, 1X, &
     &                        ' Pcal_ind: ', I5, ' Pcal_amp: ', 8192(F9.6, 1X) )
!
                     IF ( IND_RAT_2(J8) > 0 .AND. PIM%NPOL == 2 ) THEN
!
! ----------------------- Case of dual-pol. Extract phase calibration rate
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR8 ( PIM%FILE(J8)%FITS_DESC, IND_TAB(J8),  &
     &                             J9, PIM%FILE(J8)%KEY(IND_RAT_2(J8),IND_TAB(J8)), &
     &                             NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_RATE_2_R8, &
     &                             IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J9, STR )
                               IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                                    CALL ERR_LOG ( 7450, IUER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal rate_2 from the FITS-IDI '// &
     &                                  'file '//PIM%FILE(J8)%NAME )
                                    RETURN
                                 ELSE
                                    IER = IUER
                                    CALL ERR_LOG ( 7451, IER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal rate_2 from the FITS-IDI '// &
     &                                  'file '//PIM%FILE(J8)%NAME )
                                    WRITE ( 6, * ) 'Nevertheless, continue'
                                    PCAL_RATE_2_R8 = 0.0D0
                               END IF
                          END IF
                        ELSE 
                          PCAL_RATE_2_R8 = 0.0D0
                     END IF
!
                     IF ( IND_REA_2(J8) > 0 .AND. PIM%NPOL == 2 ) THEN
!
! ----------------------- Get real part of the phase calibration signal 
! ----------------------- for the second polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J8)%FITS_DESC,  IND_TAB(J8),  &
     &                               J9, PIM%FILE(J8)%KEY(IND_REA_2(J8),IND_TAB(J8)), &
     &                               NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_REA_2, &
     &                               IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J9, STR )
                               IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                                    CALL ERR_LOG ( 7452, IUER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal real_2 part from the '// &
     &                                  'FITS-IDI file '//PIM%FILE(J8)%NAME )
                                    RETURN
                                 ELSE
                                    IER = IUER
                                    CALL ERR_LOG ( 7453, IER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal real_2 part from the '// &
     &                                  'FITS-IDI file '//PIM%FILE(J8)%NAME )
                                    WRITE ( 6, * ) 'Nevertheless, continue'
                                    PCAL_REA_2 = 0.0D0
                               END IF
                          END IF
                        ELSE 
                          PCAL_REA_2 = 0.0
                     END IF
!
                     IF ( IND_IMA_2(J8) > 0 .AND. PIM%NPOL == 2 ) THEN
!
! ----------------------- Get image part of the phase calibration signal 
! ----------------------- for the second polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J8)%FITS_DESC,  IND_TAB(J8),  &
     &                               J9, PIM%FILE(J8)%KEY(IND_IMA_2(J8),IND_TAB(J8)), &
     &                               NO_TONES*PIM%FILE(J8)%NFRQ, PCAL_IMA_2, &
     &                               IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J9, STR )
                               IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                                    CALL ERR_LOG ( 7454, IUER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal image_2 part from the '// &
     &                                  'FITS-IDI file '//PIM%FILE(J8)%NAME )
                                    RETURN
                                 ELSE
                                    IER = IUER
                                    CALL ERR_LOG ( 7455, IER, 'PIMA_GET_PCAL', &
     &                                  'Error in getting '//STR(1:I_LEN(STR))// &
     &                                  '-th point of pcal image_2 part from the '// &
     &                                  'FITS-IDI file '//PIM%FILE(J8)%NAME )
                                    WRITE ( 6, * ) 'Nevertheless, continue'
                                    PCAL_IMA_2 = 0.0D0
                               END IF
                          END IF
                        ELSE 
                          PCAL_IMA_2 = 0.0
                     END IF
                     IF ( IND_IMA_2(J8) > 0      .AND. &
     &                    PIM%NPOL == 2                ) THEN
!
! ----------------------- Print phase and aplitude of phase calibration signal for the second 
! ----------------------- polarization
!
                          DO 4110 J11=1,NO_TONES*PIM%FILE(J8)%NFRQ
                             IF ( IS_R4_NAN(PCAL_REA_2(J11)) .OR. &
     &                            IS_R4_NAN(PCAL_IMA_2(J11)) .OR. &
     &                            IS_R4_INF(PCAL_REA_2(J11)) .OR. &
     &                            IS_R4_INF(PCAL_IMA_2(J11))      ) THEN
!
                                  PCAL_REA_2(J11) = PIMA__PCAL_AMP_MIN/10.0
                                  PCAL_IMA_2(J11) = 0.0
                            END IF
                            IF ( ABS(PCAL_REA_2(J11))  > PIMA__PCAL_AMP_MAX .OR. &
     &                           ABS(PCAL_IMA_2(J11))  > PIMA__PCAL_AMP_MAX      ) THEN
                                 PCAL_REA_2(J11) = PIMA__PCAL_AMP_MIN/10.0
                                 PCAL_IMA_2(J11) = 0.0
                             END IF
                             IF ( ABS(PCAL_REA_2(J11)) > PIMA__PCAL_AMP_MIN .OR. &
     &                            ABS(PCAL_IMA_2(J11)) > PIMA__PCAL_AMP_MIN      ) THEN
!
                                  PCAL_PHS(J11) = ATAN_CS_R4 ( PCAL_REA_2(J11), PCAL_IMA_2(J11)   )
                                  PCAL_AMP(J11) = SQRT ( PCAL_REA_2(J11)**2  + PCAL_IMA_2(J11)**2 )
                                ELSE
                                  PCAL_PHS(J11) = 0.0
                                  PCAL_AMP(J11) = 0.0
                             END IF
 4110                     CONTINUE 
                          WRITE ( LUN_PCA, 280 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                           J8, J9, 2, STR_DATE(3)(1:21), LP, &
     &                                           PCAL_PHS(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
                          WRITE ( LUN_PCA, 290 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(LP)), &
     &                                           J8, J9, 2, STR_DATE(3)(1:21), LP, &
     &                                           PCAL_AMP(1:NO_TONES*PIM%FILE(J8)%NFRQ) 
                     END IF
!
                     DO 4120 J12=1,NO_TONES*PIM%FILE(J8)%NFRQ
!
! --------------------- Associate the J12-th pcal frequency with IFs
!
                        IFRQ = 0
                        FREQ_DIF = 1.D30
                        IND_FREQ_MAX = 0
                        FL_ZERO_FRQ = .FALSE.
                        DO 4130 J13=1,PIM%NFRQ
                           IF ( PIM%FRQ(J13,J6)%CHAN_WIDTH .GE. PIM__CHAN_WIDTH_MIN ) THEN
!
! ----------------------------- Wide channel width
!
                                CHN_MGN = 0.99D0
                              ELSE
                                CHN_MGN = 1.00D0
                           END IF
                           IF ( PIM%FRQ(J13,J6)%SIDE_BAND == 1 ) THEN
                                FREQ_BEG =   PIM%FRQ(J13,J6)%FREQ
                                FREQ_END =   PIM%FRQ(J13,J6)%FREQ &
     &                                     + PIM%FRQ(J13,J6)%BAND_WIDTH
                             ELSE
                                FREQ_BEG =   PIM%FRQ(J13,J6)%FREQ &
     &                                     - CHN_MGN*PIM%FRQ(J13,J6)%CHAN_WIDTH
                                FREQ_END =   PIM%FRQ(J13,J6)%FREQ &
     &                                     + PIM%FRQ(J13,J6)%BAND_WIDTH &
     &                                     - CHN_MGN*PIM%FRQ(J13,J6)%CHAN_WIDTH
                           END IF 
                           IF ( PCAL_FREQ_R8(J12) .GE. FREQ_BEG - EPS_FREQ .AND. &
     &                          PCAL_FREQ_R8(J12) .LE. FREQ_END + EPS_FREQ .AND. &
     &                          IFRQ == 0                                       ) THEN
                                IFRQ = J13
                              ELSE 
                                IF ( DABS(PCAL_FREQ_R8(J13) - FREQ_BEG) < FREQ_DIF ) THEN
                                     FREQ_DIF = DABS(PCAL_FREQ_R8(J13) - FREQ_BEG) 
                                     IND_FREQ_MAX = J13
                                END IF
                                IF ( DABS(PCAL_FREQ_R8(J13) - FREQ_END) < FREQ_DIF ) THEN
                                     FREQ_DIF = DABS(PCAL_FREQ_R8(J13) - FREQ_END) 
                                     IND_FREQ_MAX = J13
                                END IF
                           END IF
 4130                   CONTINUE
!
                        IF ( IFRQ == 0  .AND.  FREQ_DIF < 1.000001D6 ) THEN
                             IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                                  WRITE ( 6, 2100 ) J12, PCAL_FREQ_R8(J12), PIM%C_STA(J7), &
     &                                             PIM%FILE(J8)%NAME(1:I_LEN(PIM%FILE(J8)%NAME))
 2100                             FORMAT ( 'PHASE_CAL  The ', I6,' -th ', &
     &                                     'frequency ',F12.0, ' MHz is more ', &
     &                                     'than 1MHz out of range '// &
     &                                     'for station ', A, ' file ', A )
                             END IF
                             IFRQ = IND_FREQ_MAX 
                        END IF
!
                        IF ( PIM%CONF%DEBUG_LEVEL .GE. 36 ) THEN
                             WRITE ( 6, * ) ' PIMA_GET_PCAL-1110 IFRG= ', INT2(J6), &
     &                                      ' STA_IND(J6) = ',  INT2(J7),  &
     &                                      ' STA_NAME: ',      PIM%C_STA(J7), &
     &                                      ' FIL_IND(J7)  = ', INT2(J8),  &
     &                                      ' PCAL_IND(J8) = ', INT2(J9),  &
     &                                      ' FRQ_IND(J9)  = ', INT2(J12), &
     &                                      ' IFRQ = ', INT2(IFRQ)
                        END IF
                        IF ( IFRQ == 0 .AND. PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J12, STR )
                             CALL CLRCH ( STR1 )
                             WRITE ( UNIT=STR1, FMT='(1PD15.8)' ) PCAL_FREQ_R8(J12)
                             IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                                  CALL ERR_LOG ( 7456, IUER, 'PIMA_GET_PCAL', &
     &                                'Cannot associate the '//STR(1:I_LEN(STR))// &
     &                                ' th pcal frequency '// &
     &                                 STR1(1:I_LEN(STR1))//' Hz within the '// &
     &                                'frequency band of any IF for station '// &
     &                                 PIM%C_STA(J7)//' while parsing the '// &
     &                                'FITS-IDI file '//PIM%FILE(J8)%NAME(1:I_LEN(PIM%FILE(J8)%NAME))// &
     &                                '. You need to use CHECK_SEVERITY 1 or 0 if you would '// &
     &                                'like to ignore this error' )
                                  RETURN
                                ELSE 
                                  CALL CLRCH ( STR2 )
                                  CALL INCH  ( J9, STR2 )
                                  CALL CLRCH ( STR3 )
                                  CALL INCH  ( KP(J8), STR3 )
                                  CALL ERR_PASS ( IUER, IER )
                                  IER = IUER
                                  CALL ERR_LOG ( 7457, IER, 'PIMA_GET_PCAL', &
     &                                 'Cannot associate the '//STR(1:I_LEN(STR))// &
     &                                 ' th pcal frequency '// &
     &                                 STR1(1:I_LEN(STR1))//' Hz within the band '// &
     &                                 'frequency of any IF for station '// &
     &                                 PIM%C_STA(J7)//', pcal data '// &
     &                                 STR2(1:I_LEN(STR2))//'('//STR3(1:I_LEN(STR3))// &
     &                                 ') while parsing the FITS-IDI file '// &
     &                                 PIM%FILE(J8)%NAME(1:I_LEN(PIM%FILE(J8)%NAME))// &
     &                                ', nevertheless, continue' )
                                   IFRQ = 1
                                   PCAL_FREQ_R8(J12) = J12*1.D-6
                                   FL_ZERO_FRQ = .TRUE.
                              END IF
                        END IF
!
                        IF ( IFRQ .NE. 0 ) THEN
                             DO 4140 J14=1,NO_TONES
                                IF ( PIM%STA(J7)%PCAL(J6)%FREQ(J14,IFRQ,LP) < EPS_FREQ ) THEN
                                     IF ( FL_ZERO_FRQ ) THEN 
                                          PIM%STA(J7)%PCAL(J6)%FREQ(J14,IFRQ,LP) = PCAL_FREQ_R8(J12) + J14*1.D-12
                                        ELSE
                                          PIM%STA(J7)%PCAL(J6)%FREQ(J14,IFRQ,LP) = PCAL_FREQ_R8(J12)
                                     END IF
!
                                     IF ( IS_R4_NAN ( PCAL_REA(J12) ) .OR. &
     &                                    IS_R4_NAN ( PCAL_IMA(J12) ) .OR. &
     &                                    IS_R4_INF ( PCAL_REA(J12) ) .OR. &
     &                                    IS_R4_INF ( PCAL_IMA(J12) )      ) THEN
                                          PCAL_REA(J12) = PIMA__PCAL_AMP_MIN/10.0
                                          PCAL_IMA(J12) = 0.0
                                     END IF
                                     IF ( ABS(PCAL_REA(J12))  > PIMA__PCAL_AMP_MAX .OR. &
     &                                    ABS(PCAL_IMA(J12))  > PIMA__PCAL_AMP_MAX      ) THEN
                                          PCAL_REA(J12) = PIMA__PCAL_AMP_MIN/10.0
                                          PCAL_IMA(J12) = 0.0
                                     END IF
                                     IF ( IS_R4_NAN ( PCAL_REA_2(J12) ) .OR. &
     &                                    IS_R4_NAN ( PCAL_IMA_2(J12) ) .OR. &
     &                                    IS_R4_INF ( PCAL_REA_2(J12) ) .OR. &
     &                                    IS_R4_INF ( PCAL_IMA_2(J12) )      ) THEN
                                          PCAL_REA_2(J12) = PIMA__PCAL_AMP_MIN/10.0
                                          PCAL_IMA_2(J12) = 0.0
                                     END IF
                                     IF ( ABS(PCAL_REA_2(J12))  > PIMA__PCAL_AMP_MAX .OR. &
     &                                    ABS(PCAL_IMA_2(J12))  > PIMA__PCAL_AMP_MAX      ) THEN
                                          PCAL_REA_2(J12) = PIMA__PCAL_AMP_MIN/10.0
                                          PCAL_IMA_2(J12) = 0.0
                                     END IF
!
                                     PIM%STA(J7)%PCAL(J6)%RATE(J14,IFRQ,LP,1) = PCAL_RATE_R8(J12)
                                     IF ( ABS(PCAL_REA(J12)) > PIMA__PCAL_AMP_MIN .OR. &
     &                                    ABS(PCAL_REA(J12)) > PIMA__PCAL_AMP_MIN      ) THEN
                                          PIM%STA(J7)%PCAL(J6)%AMPL(J14,IFRQ,LP,1) = &
     &                                                         SQRT ( PCAL_REA(J12)**2 + &
     &                                                                PCAL_IMA(J12)**2 )
                                          PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,1) = &
     &                                                         ATAN_CS_R4 ( PCAL_REA(J12), &
     &                                                                      PCAL_IMA(J12)  )
                                        ELSE
                                          PIM%STA(J7)%PCAL(J6)%AMPL(J14,IFRQ,LP,1) = 0.00
                                          PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,1) = 0.0
                                     END IF
                                     IF ( PIM%FRQ(IFRQ,J6)%SIDE_BAND == -111 ) THEN
                                          PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,1) = &
     &                                      -PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,1) 
                                     END IF
!
                                     IF ( PIM%STA(J7)%PCAL(J6)%NPOL == 2 ) THEN
                                          PIM%STA(J7)%PCAL(J6)%RATE(J14,IFRQ,LP,2) = PCAL_RATE_2_R8(J12)
                                          IF ( ABS(PCAL_REA_2(J12)) > PIMA__PCAL_AMP_MIN .OR. &
     &                                         ABS(PCAL_REA_2(J12)) > PIMA__PCAL_AMP_MIN      ) THEN
                                               PIM%STA(J7)%PCAL(J6)%AMPL(J14,IFRQ,LP,2) = &
     &                                                     SQRT ( PCAL_REA_2(J12)**2 + &
     &                                                            PCAL_IMA_2(J12)**2 )
                                               PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,2) = &
     &                                                     ATAN_CS_R4 ( PCAL_REA_2(J12), &
     &                                                                  PCAL_IMA_2(J12)  )
                                             ELSE 
                                               PIM%STA(J7)%PCAL(J6)%AMPL(J14,IFRQ,LP,2) = 0.0
                                               PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,2) = 0.0
                                          END IF
                                          IF ( PIM%FRQ(IFRQ,J6)%SIDE_BAND == -111 ) THEN
                                               PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,2) = &
     &                                         -PIM%STA(J7)%PCAL(J6)%PHAS(J14,IFRQ,LP,2) 
                                          END IF
                                     END IF
                                     GOTO 8140
                                END IF
 4140                        CONTINUE
 8140                        CONTINUE
                        END IF
 4120                CONTINUE
                     IF ( PIM%CONF%DEBUG_LEVEL .GE. 12 ) THEN
                          DO 5120 J12=1,PIM%FILE(J8)%NFRQ
                             WRITE  ( 6, 2110 ) PIM%C_STA(J7), LP, 1, J12, &
     &                                          PIM%STA(J7)%PCAL(J6)%PHAS(1:NO_TONES,J12,LP,1)
 2110                        FORMAT ( 'PIMCA_GET_PCAL Sta: ', A, ' Pcal_ind: ', I5, ' Pol_ind: ', I1, &
     &                                ' Ind_file_frq: ', I3, ' Pcal_phas: ', 1024(F8.5,1X) )
 5120                     CONTINUE 
                     END IF 
                 END IF
 490          CONTINUE
!
               CALL ERR_PASS    ( IUER, IER )
               CALL FFITS_CLOSE ( PIM%FILE(J8)%FITS_DESC, IER ) 
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7458, IUER, 'PIMA_GET_PCAL', &
     &                  'Error in an attempt to close FITS UV-file '// &
     &                   PIM%FILE(J1)%NAME )
                    RETURN
               END IF
 480        CONTINUE
!
! --------- Allocate temporary arrays
!
            ALLOCATE ( ARR1(PIM%STA(J7)%PCAL(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%STA(J7)%PCAL(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7459, IUER, 'PIMA_GET_PCAL', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory' )
                 RETURN
            END IF
!
            ALLOCATE ( ARR2(PIM%STA(J7)%PCAL(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%STA(J7)%PCAL(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7, IUER, 'PIMA_GET_PCAL', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory' )
                 RETURN
            END IF
!
! --------- Now sorting PCAL arrays according to time and remove duplicates
!
! --------- First sort indexes
!
            DO 4150 J15=1,PIM%STA(J7)%PCAL(J6)%NPOI
               ARR1(J15) = PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(J15) ! Time index
               ARR2(J15) = 1.0D0*J15   ! Point indexes
 4150       CONTINUE
!
! --------- Sort indexes
!
            CALL SORT8 ( PIM%STA(J7)%PCAL(J6)%NPOI, ARR1, ARR2 )
!
! --------- Copy PIM%STA(J7)%PCAL to a temporary array
!
            PCAL_TMP%PCAL_AVAIL = PIM%STA(J7)%PCAL(J6)%PCAL_AVAIL
            PCAL_TMP%PCAL_USE   = PIM%STA(J7)%PCAL(J6)%PCAL_USE
            PCAL_TMP%NO_TONES   = PIM%STA(J7)%PCAL(J6)%NO_TONES
            PCAL_TMP%NPOI       = PIM%STA(J7)%PCAL(J6)%NPOI
            PCAL_TMP%NPOL       = PIM%STA(J7)%PCAL(J6)%NPOL
            ALLOCATE ( PCAL_TMP%AMPL(NO_TONES,PIM%NFRQ,PCAL_TMP%NPOI,PCAL_TMP%NPOL) )
            ALLOCATE ( PCAL_TMP%PHAS(NO_TONES,PIM%NFRQ,PCAL_TMP%NPOI,PCAL_TMP%NPOL) )
            ALLOCATE ( PCAL_TMP%RATE(NO_TONES,PIM%NFRQ,PCAL_TMP%NPOI,PCAL_TMP%NPOL) )
            ALLOCATE ( PCAL_TMP%FREQ(NO_TONES,PIM%NFRQ,PCAL_TMP%NPOI) )
            ALLOCATE ( PCAL_TMP%TIME_MID_R8 (PCAL_TMP%NPOI) )
            ALLOCATE ( PCAL_TMP%TIME_SPAN_R4(PCAL_TMP%NPOI) )
            ALLOCATE ( PCAL_TMP%SOU_IND(PCAL_TMP%NPOI) )
!
            CAB_TMP%CAB_AVAIL  = PIM%STA(J7)%CABLE%CAB_AVAIL
            CAB_TMP%NPOI = PIM%STA(J7)%CABLE%NPOI
            ALLOCATE ( CAB_TMP%TIM_CAB(PCAL_TMP%NPOI)  )
            ALLOCATE ( CAB_TMP%CAB_DEL(PCAL_TMP%NPOI)  )
!
            CALL COPY_R4 ( NO_TONES* PIM%NFRQ* PCAL_TMP%NPOI* PCAL_TMP%NPOL, &
     &                     PIM%STA(J7)%PCAL(J6)%AMPL, PCAL_TMP%AMPL )
            CALL COPY_R4 ( NO_TONES* PIM%NFRQ* PCAL_TMP%NPOI* PCAL_TMP%NPOL, &
     &                     PIM%STA(J7)%PCAL(J6)%PHAS, PCAL_TMP%PHAS )
            CALL COPY_R8 ( NO_TONES* PIM%NFRQ* PCAL_TMP%NPOI* PCAL_TMP%NPOL, &
     &                     PIM%STA(J7)%PCAL(J6)%RATE, PCAL_TMP%RATE )
            CALL COPY_R8 ( NO_TONES* PIM%NFRQ* PCAL_TMP%NPOI, &
     &                     PIM%STA(J7)%PCAL(J6)%FREQ, PCAL_TMP%FREQ )
            CALL COPY_R8 ( PCAL_TMP%NPOI, &
     &                     PIM%STA(J7)%PCAL(J6)%TIME_MID_R8, PCAL_TMP%TIME_MID_R8 )
            CALL COPY_R4 ( PCAL_TMP%NPOI, PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4, &
     &                     PCAL_TMP%TIME_SPAN_R4 )
            CALL COPY_I4 ( PCAL_TMP%NPOI, PIM%STA(J7)%PCAL(J6)%SOU_IND, &
     &                     PCAL_TMP%SOU_IND )
            CALL COPY_R8 ( CAB_TMP%NPOI, &
     &                     PIM%STA(J7)%CABLE%TIM_CAB, CAB_TMP%TIM_CAB )
            CALL COPY_R8 ( CAB_TMP%NPOI, &
     &                     PIM%STA(J7)%CABLE%CAB_DEL, CAB_TMP%CAB_DEL )
!
            LP = 0 ! points counter
!
! --------- Consecutive coping sorted pcal from temporary array back to
! --------- PCAL%STA(J7)%PCAL and merging records with the same time tag, 
! --------- but different frequencies
!
            DO 4160 J16=1,PCAL_TMP%NPOI
               IND_PCAL = NINT ( ARR2(J16) )
               IF ( PIM%CONF%DEBUG_LEVEL .EQ. 21 ) THEN
                    STR  = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                     PIM%TAI_0 + PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(IND_PCAL) &
     &                                   - PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(IND_PCAL)/2.0, -2 )
                    STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                      PIM%TAI_0 + PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(IND_PCAL) &
     &                                   + PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(IND_PCAL)/2.0, -2 )
                    WRITE ( 6, 2120 ) PIM%C_STA(J7), PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(IND_PCAL)), &
     &                                STR(1:21), STR1(1:21), J6, IND_PCAL, LP
 2120               FORMAT ( 'PIMA_GET_PCAL Sta: ', A, ' Sou: ',A, &
     &                       ' utc_range: [ ', A, ' , ', A, ' ] ', &
     &                       ' ind_frg: ', I1, ' pcal_npoi: ', I5, ' Lp: ', I5 )
               END IF
               IF ( LP > 0 ) THEN
!
! ----------------- Check for records with the same time tag
!
                    IF ( ABS ( PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(IND_PCAL) - &
     &                         PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP) ) < EPS_TIME ) THEN
!
! ---------------------- Aga, these two records have the same time tag.
! ---------------------- We try to merge it. We need to learn which record 
! ---------------------- corresponds to higher frequencies
!
                         IND_BEG_FRQ1 = 0
                         IND_BEG_FRQ2 = 0
                         IND_END_FRQ1 = 0
                         IND_END_FRQ2 = 0
                         FL_FRQ_SAME = .TRUE.
                         DO 4170 J17=1,PIM%NFRQ
                            DO 4180 J18=1,NO_TONES
                               IF ( DABS(PIM%STA(J7)%PCAL(J6)%FREQ(J18,J17,IND_PCAL) - &
     &                                   PIM%STA(J7)%PCAL(J6)%FREQ(J18,J17,LP) ) > &
     &                              1.0D0 ) FL_FRQ_SAME = .FALSE.
                               IF ( PIM%STA(J7)%PCAL(J6)%FREQ(J18,J17,IND_PCAL) > PIMA__MIN_FRQ ) THEN
                                    IND_END_FRQ1 = J17
                                    IF ( IND_BEG_FRQ1 == 0 ) IND_BEG_FRQ1 = J17
                               END IF
                               IF ( PIM%STA(J7)%PCAL(J6)%FREQ(J18,J17,LP) > PIMA__MIN_FRQ ) THEN
                                    IND_END_FRQ2 = J17
                                    IF ( IND_BEG_FRQ2 == 0 ) IND_BEG_FRQ2 = J17
                               END IF
 4180                       CONTINUE 
 4170                    CONTINUE 
                         IF ( FL_FRQ_SAME ) GOTO 4160
                         IF ( (IND_END_FRQ1 - IND_BEG_FRQ1 + 1) + &
     &                        (IND_END_FRQ2 - IND_BEG_FRQ2 + 1) > PIM%NFRQ ) THEN
                              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(IND_PCAL), -2 )
                              WRITE  ( 6, * ) ' IND_BRG_FRQ1= ', IND_BEG_FRQ1, &
     &                                        ' IND_BEG_FRQ2= ', IND_BEG_FRQ2, &
     &                                        ' IND_END_FRQ1= ', IND_END_FRQ1, &
     &                                        ' IND_END_FRQ2= ', IND_END_FRQ2, &
     &                                        ' PIM%NFRQ= ', PIM%NFRQ
                              CALL ERR_LOG ( 7461, IUER, 'PIMA_GET_PCAL', &
     &                            'Trap of interanl control: pcal records '// &
     &                            ' at station '//PIM%STA(J7)%IVS_NAME// &
     &                            ' at '//STR(1:21)//' in total, have '// &
     &                            'too many frequencies' )
                              RETURN
                         END IF
!
                         IF ( PIM%STA(J7)%PCAL(J6)%FREQ(1,IND_BEG_FRQ2,LP) > &
     &                        PIM%STA(J7)%PCAL(J6)%FREQ(1,IND_BEG_FRQ1,IND_PCAL) ) THEN
!
! --------------------------- The LP -th record has the higher frequencies
!
                              I_FRQ = IND_END_FRQ1
                              DO 4190 J19=IND_BEG_FRQ2,IND_END_FRQ2
                                 I_FRQ = I_FRQ + 1
                                 DO 4200 J20=1,NO_TONES
                                    PIM%STA(J7)%PCAL(J6)%FREQ(J20,I_FRQ,IND_PCAL) = &
     &                                  PIM%STA(J7)%PCAL(J6)%FREQ(J20,J19,LP)
                                    PIM%STA(J7)%PCAL(J6)%PHAS(J20,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%PHAS(J20,J19,LP,1)
                                    PIM%STA(J7)%PCAL(J6)%AMPL(J20,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%AMPL(J20,J19,LP,1)
                                    PIM%STA(J7)%PCAL(J6)%RATE(J20,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%RATE(J20,J19,LP,1)
!
                                    IF ( PIM%STA(J7)%PCAL(J6)%NPOL == 2 ) THEN
                                         PIM%STA(J7)%PCAL(J6)%PHAS(J20,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%PHAS(J20,J19,LP,2)
                                         PIM%STA(J7)%PCAL(J6)%AMPL(J20,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%AMPL(J20,J19,LP,2)
                                         PIM%STA(J7)%PCAL(J6)%RATE(J20,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%RATE(J20,J19,LP,2)
                                    END IF
 4200                            CONTINUE 
 4190                         CONTINUE 
                            ELSE IF ( PIM%STA(J7)%PCAL(J6)%FREQ(1,IND_BEG_FRQ2,LP) < &
     &                                PIM%STA(J7)%PCAL(J6)%FREQ(1,IND_BEG_FRQ1,IND_PCAL) ) THEN
!
! --------------------------- The LP-th record has the lower frequencies.
! --------------------------- We need first to move the data in the IND_PCAL -th
! --------------------------- record to the end and make a hole for the 
! --------------------------- LP-th record
!
                              I_FRQ = IND_END_FRQ2+1
                              DO 4210 J21=IND_END_FRQ1,IND_BEG_FRQ1,-1
                                 DO 4220 J22=1,NO_TONES
                                    PIM%STA(J7)%PCAL(J6)%FREQ(J22,I_FRQ,IND_PCAL) = &
     &                                  PIM%STA(J7)%PCAL(J6)%FREQ(J22,J21,IND_PCAL)
                                    PIM%STA(J7)%PCAL(J6)%PHAS(J22,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%PHAS(J22,J21,IND_PCAL,1)
                                    PIM%STA(J7)%PCAL(J6)%AMPL(J22,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%AMPL(J22,J21,IND_PCAL,1)
                                    PIM%STA(J7)%PCAL(J6)%RATE(J22,I_FRQ,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%RATE(J22,J21,IND_PCAL,1)
                                    IF ( PIM%STA(J7)%PCAL(J6)%NPOL == 2 ) THEN
                                         PIM%STA(J7)%PCAL(J6)%PHAS(J22,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%PHAS(J22,J21,IND_PCAL,2)
                                         PIM%STA(J7)%PCAL(J6)%AMPL(J22,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%AMPL(J22,J21,IND_PCAL,2)
                                         PIM%STA(J7)%PCAL(J6)%RATE(J22,I_FRQ,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%RATE(J22,J21,IND_PCAL,2)
                                    END IF
 4220                            CONTINUE 
                                 I_FRQ = I_FRQ - 1
 4210                         CONTINUE 
!
! --------------------------- Now we put the data from the IND_PCAL -th record
! --------------------------- into the hole
!
                              DO 4230 J23=IND_BEG_FRQ2,IND_END_FRQ2
                                 DO 4240 J24=1,NO_TONES
                                    PIM%STA(J7)%PCAL(J6)%FREQ(J24,J23,IND_PCAL) = &
     &                                  PIM%STA(J7)%PCAL(J6)%FREQ(J24,J23,LP)
                                    PIM%STA(J7)%PCAL(J6)%PHAS(J24,J23,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%PHAS(J24,J23,LP,1)
                                    PIM%STA(J7)%PCAL(J6)%AMPL(J24,J23,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%AMPL(J24,J23,LP,1)
                                    PIM%STA(J7)%PCAL(J6)%RATE(J24,J23,IND_PCAL,1) = &
     &                                  PIM%STA(J7)%PCAL(J6)%RATE(J24,J23,LP,1)
                                    IF ( PIM%STA(J7)%PCAL(J6)%NPOL == 2 ) THEN
                                         PIM%STA(J7)%PCAL(J6)%PHAS(J24,J23,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%PHAS(J24,J23,LP,2)
                                         PIM%STA(J7)%PCAL(J6)%AMPL(J24,J23,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%AMPL(J24,J23,LP,2)
                                         PIM%STA(J7)%PCAL(J6)%RATE(J24,J23,IND_PCAL,2) = &
     &                                       PIM%STA(J7)%PCAL(J6)%RATE(J24,J23,LP,2)
                                    END IF
 4240                            CONTINUE 
 4230                         CONTINUE 
                         END IF
!
! ---------------------- We merged IND_PCAL -th records and LP-th record.
! ---------------------- Now we discard the LP-th record
!
                         GOTO 4160
                    END IF  !   end of record merging
               END IF
               LP = LP + 1 ! increcment of points counter
!
! ------------ Copying back the stuff
!
               PIM%STA(J7)%CABLE%CAB_DEL(LP) = CAB_TMP%CAB_DEL(IND_PCAL)
               PIM%STA(J7)%CABLE%TIM_CAB(LP) = CAB_TMP%TIM_CAB(IND_PCAL)
               IF ( J6 == 1 ) THEN
                    PIM%STA(J7)%CABLE%MEAN_CABLE  = PIM%STA(J7)%CABLE%MEAN_CABLE &
     &                                            + PIM%STA(J7)%CABLE%CAB_DEL(LP)
               END IF
               PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(LP)  = PCAL_TMP%TIME_MID_R8(IND_PCAL)
               PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(LP) = PCAL_TMP%TIME_SPAN_R4(IND_PCAL)
               PIM%STA(J7)%PCAL(J6)%SOU_IND(LP) = PCAL_TMP%SOU_IND(IND_PCAL)
!
               DO 4250 J25=1,PIM%NFRQ
                  DO 4260 J26=1,NO_TONES
                     PIM%STA(J7)%PCAL(J6)%FREQ(J26,J25,LP)   = PCAL_TMP%FREQ(J26,J25,IND_PCAL)
                     PIM%STA(J7)%PCAL(J6)%PHAS(J26,J25,LP,1) = PCAL_TMP%PHAS(J26,J25,IND_PCAL,1)
                     PIM%STA(J7)%PCAL(J6)%AMPL(J26,J25,LP,1) = PCAL_TMP%AMPL(J26,J25,IND_PCAL,1)
                     PIM%STA(J7)%PCAL(J6)%RATE(J26,J25,LP,1) = PCAL_TMP%RATE(J26,J25,IND_PCAL,1)
                     IF ( PIM%STA(J7)%PCAL(J6)%NPOL == 2 ) THEN
                          PIM%STA(J7)%PCAL(J6)%PHAS(J26,J25,LP,2) = PCAL_TMP%PHAS(J26,J25,IND_PCAL,2)
                          PIM%STA(J7)%PCAL(J6)%AMPL(J26,J25,LP,2) = PCAL_TMP%AMPL(J26,J25,IND_PCAL,2)
                          PIM%STA(J7)%PCAL(J6)%RATE(J26,J25,LP,2) = PCAL_TMP%RATE(J26,J25,IND_PCAL,2)
                     END IF
 4260             CONTINUE
 4250          CONTINUE
 4160       CONTINUE
            PIM%STA(J7)%PCAL(J6)%NPOI = LP
            IF ( J6 == 1 ) THEN
                 PIM%STA(J7)%CABLE%MEAN_CABLE = PIM%STA(J7)%CABLE%MEAN_CABLE/LP
            END IF
!
            DO 4270 J27=1,PIM%STA(J7)%PCAL(J6)%NPOI 
               PIM%STA(J7)%CABLE%CAB_DEL(J27) = PIM%STA(J7)%CABLE%CAB_DEL(J27) &
     &                                        - PIM%STA(J7)%CABLE%MEAN_CABLE
 4270       CONTINUE 
!
            DEALLOCATE ( PCAL_TMP%AMPL )
            DEALLOCATE ( PCAL_TMP%PHAS )
            DEALLOCATE ( PCAL_TMP%FREQ )
            DEALLOCATE ( PCAL_TMP%RATE )
            DEALLOCATE ( PCAL_TMP%TIME_MID_R8  )
            DEALLOCATE ( PCAL_TMP%TIME_SPAN_R4 )
            DEALLOCATE ( PCAL_TMP%SOU_IND )
            DEALLOCATE ( CAB_TMP%TIM_CAB )
            DEALLOCATE ( CAB_TMP%CAB_DEL )
            DEALLOCATE ( ARR1 )
            DEALLOCATE ( ARR2 )
         END IF
!
         IF ( PIM%STA(J7)%PCAL(J6)%NPOI > 0 ) THEN
              IF ( STR_PCAL_NOAVR .NE. 'YES' ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL PIMA_PCAL_SCAN_AVR ( PIM, PIM%STA(J7)%PCAL(J6), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7462, IUER, 'PIMA_GET_PCAL', 'Error in '// &
     &                      'scan computing scan averaged phase calibration for '// &
     &                      'station '//PIM%C_STA(J7) )
                        RETURN 
                   END IF
              END IF
            ELSE            
              PIM%STA(J7)%PCAL(J6)%PCAL_SCA = .FALSE.
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              DO 4280 J28=1,PIM%STA(J7)%PCAL(J6)%NPOI
                 TIM_PCAL_BEG = PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(J28) - &
     &                          PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(J28)/2.0D0
                 TIM_PCAL_END = PIM%STA(J7)%PCAL(J6)%TIME_MID_R8(J28) + &
     &                          PIM%STA(J7)%PCAL(J6)%TIME_SPAN_R4(J28)/2.0D0
                 STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_PCAL_BEG, -2 )
                 STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_PCAL_END, -2 )
                 WRITE ( 6, 2130 ) PIM%C_STA(J7), J28, &
     &                            PIM%C_SOU(PIM%STA(J7)%PCAL(J6)%SOU_IND(J28)), &
     &                            STR(1:22), STR1(1:22)
 2130            FORMAT ( 'Station ',A, ' Pcal_id: ', I7, ' Sou: ', A, &
     &                    ' Time range [ ', A, ' , ', A, ' ]' )
 4280         CONTINUE 
           END IF
 470    CONTINUE
 460  CONTINUE
!
! --- Now put indexes of pcal into the obs data structure
!
      N_MIS = 0 
      DO 4290 J29=1,PIM%NFRG
         DO 4300 J30=1,PIM%NOBS
!
! --------- Get the list of sources of the same pointing, but different phase centers.
! --------- List N_PHC, IND_SOU_PHC includes the nominal source of the J30-th observation
!
            N_PHC = 0
            DO 4310 J31=1,PIM%NOBS
!
! ------------ Check that station indices are the same
!
               IF ( PIM%OBS(J31)%STA_IND(1) .NE. PIM%OBS(J30)%STA_IND(1) ) GOTO 4310
               IF ( PIM%OBS(J31)%STA_IND(2) .NE. PIM%OBS(J30)%STA_IND(2) ) GOTO 4310
!
               IF ( DABS(PIM%OBS(J31)%TIM_BEG - PIM%OBS(J30)%TIM_BEG) < (PIM__MACP+0.1)*PIM%OBS(J30)%AP_LEN .AND. &
     &              DABS(PIM%OBS(J31)%TIM_END - PIM%OBS(J30)%TIM_END) < (PIM__MACP+0.1)*PIM%OBS(J30)%AP_LEN       ) THEN
!
! ----------------- This source has start and stop time tags that differs by 
! ----------------- no more than one PIM__MACP accumulation period
!
                    N_PHC = N_PHC + 1
                    IND_SOU_PHC(N_PHC) = PIM%OBS(J31)%SOU_IND
               END IF 
 4310       CONTINUE 
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                 IF ( N_PHC == 1 ) THEN
                      WRITE ( 6, * ) 'PIMA_GET_PCAL Obs: ', J30, &
     &                               ' Sou: ', PIM%C_SOU(PIM%OBS(J30)%SOU_IND), &
     &                                SNGL(PIM%OBS(J30)%TIM_BEG), &
     &                                SNGL(PIM%OBS(J30)%TIM_END), &
     &                                SNGL(PIM%OBS(J30)%AP_LEN)
                    ELSE
                      CALL CLRCH ( STR )
                      IL = 1
                      DO 4320 J32=1,N_PHC
                         STR(IL:IL+LEN(PIM%C_SOU(1))-1) = PIM%C_SOU(IND_SOU_PHC(J32))
                         IL = IL + LEN(PIM%C_SOU(1))+1
 4320                 CONTINUE 
                      WRITE ( 6, * ) 'PIMA_GET_PCAL Obs: ', J30, ' Sou: ', &
     &                                PIM%C_SOU(PIM%OBS(J30)%SOU_IND), ' || ', STR(1:IL), &
     &                               ' TIM_beg/end_ap: ', SNGL(PIM%OBS(J30)%TIM_BEG), &
     &                                                    SNGL(PIM%OBS(J30)%TIM_END), &
     &                                                    SNGL(PIM%OBS(J30)%AP_LEN)
                 END IF
            END IF
!
! --------- Search for frequency group index
!
            IND_OBS_FRG = 0
            DO 4330 J33=1,PIM%OBS(J30)%NUVS
               IF ( PIM%OBS(J30)%GLO_FRG_INDS(J33) == J29 ) IND_OBS_FRG = J33
 4330       CONTINUE 
            IF ( IND_OBS_FRG == 0 ) GOTO 4300
            IF ( PIM%OBS(J29)%NUM_EPC(IND_OBS_FRG) .LE. 0 ) GOTO 4300
!
            TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J30)%UV_IND(1,IND_OBS_FRG))%TIM_IND)
            TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J30)%UV_IND(PIM%OBS(J30)%NUM_EPC(IND_OBS_FRG),IND_OBS_FRG))%TIM_IND)
            DO 4340 J34=1,2
               PIM%OBS(J30)%PCAL_IND(1:4,J34,J29) = 0
               IF ( PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%PCAL_AVAIL ) THEN
                    TIM_EPC_DIF_MIN = 1.D10
                    DO 4350 J35=1,PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%NPOI
                       TIM_PCAL_BEG = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J35) - &
     &                                PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J35)/2.0D0
                       TIM_PCAL_END = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J35) + &
     &                                PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J35)/2.0D0
                       IF ( ( TIM_PCAL_BEG .GE. TIM_OBS_BEG - EPS_TIME  .AND. &
     &                        TIM_PCAL_BEG .LE. TIM_OBS_END + EPS_TIME        ) .OR. &
     &                      ( TIM_PCAL_END .GE. TIM_OBS_BEG - EPS_TIME  .AND. &
     &                        TIM_PCAL_END .LE. TIM_OBS_END + EPS_TIME        ) .OR. &
     &                      ( TIM_PCAL_BEG .LE. TIM_OBS_BEG - EPS_TIME  .AND. &
     &                        TIM_PCAL_END .GE. TIM_OBS_END + EPS_TIME        ) &
     &                       ) THEN
!
                             IF ( IFIND_PL ( N_PHC, IND_SOU_PHC, &
     &                            PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%SOU_IND(J35) ) > 0 ) THEN
                                  IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0 ) PIM%OBS(J30)%PCAL_IND(1,J34,J29) = J35
                                  PIM%OBS(J30)%PCAL_IND(2,J34,J29) = J35
                                  TIM_EPC_DIF = DABS( (TIM_OBS_BEG + TIM_OBS_END)/2.0D0 - &
     &                                                 PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J35) ) 
                                  IF ( TIM_EPC_DIF < TIM_EPC_DIF_MIN ) THEN
                                       TIM_EPC_DIF_MIN = TIM_EPC_DIF
                                       PIM%OBS(J30)%PCAL_IND(3,J34,J29) = J35
                                  END IF
                             END IF
!
                             IF ( PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%PCAL_SCA ) THEN
                                  PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%PHAS_RGR(1:NO_TONES,1:PIM%NFRQ,J35,1:PIM%NPOL) = &
     &                                PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%PHAS(1:NO_TONES,1:PIM%NFRQ,J35,1:PIM%NPOL) 
                                  PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%AMPL_RGR(1:NO_TONES,1:PIM%NFRQ,J35,1:PIM%NPOL) = &
     &                                PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%AMPL(1:NO_TONES,1:PIM%NFRQ,J35,1:PIM%NPOL) 
                             END IF
                       END IF
 4350               CONTINUE
!
                    IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0 ) THEN
!
! ---------------------- Did not find? Let us extend the interval of Pcal 
! ---------------------- validity from both ends by PIM%CONF%MAX_SCAN_GAP
! ---------------------- and try again
!
                         DO 4360 J36=1,PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%NPOI
                            TIM_PCAL_BEG = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J36) - &
     &                                     PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J36)/2.0D0 - &
     &                                     PIM%CONF%MAX_SCAN_GAP
                            TIM_PCAL_END = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J36) + &
     &                                     PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J36)/2.0D0 + &
     &                                     PIM%CONF%MAX_SCAN_GAP
                            IF ( ( TIM_PCAL_BEG .GE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_BEG .LE. TIM_OBS_END       ) .OR. &
     &                             ( TIM_PCAL_END .GE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_END .LE. TIM_OBS_END       ) .OR. &
     &                             ( TIM_PCAL_BEG .LE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_END .GE. TIM_OBS_END       ) &
     &                         ) THEN
                               IF ( IFIND_PL ( N_PHC, IND_SOU_PHC, &
     &                              PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%SOU_IND(J36) ) > 0 ) THEN
                                    IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0 ) PIM%OBS(J30)%PCAL_IND(1,J34,J29) = J36
                                    PIM%OBS(J30)%PCAL_IND(2,J34,J29) = J36
                                    TIM_EPC_DIF = DABS( (TIM_OBS_BEG + TIM_OBS_END)/2.0D0 - &
     &                                                   PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J36) ) 
                                    IF ( TIM_EPC_DIF < TIM_EPC_DIF_MIN ) THEN
                                         TIM_EPC_DIF_MIN = TIM_EPC_DIF
                                         PIM%OBS(J30)%PCAL_IND(3,J34,J29) = J36
                                    END IF
                               END IF
                            END IF
 4360                   CONTINUE
                    END IF
!
                    IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0 ) THEN
!
! ---------------------- Still did not find? How can  it be?
! ---------------------- Let us extend the interval of Pcal 
! ---------------------- validity from both ends by PIM%CONF%MAX_SCAN_LEN
! ---------------------- and try again
!
                         DO 4370 J37=1,PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%NPOI
                            TIM_PCAL_BEG = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J37) - &
     &                                     PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J37)/2.0D0 - &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            TIM_PCAL_END = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J37) + &
     &                                     PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_SPAN_R4(J37)/2.0D0 + &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            IF ( ( TIM_PCAL_BEG .GE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_BEG .LE. TIM_OBS_END       ) .OR. &
     &                             ( TIM_PCAL_END .GE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_END .LE. TIM_OBS_END       ) .OR. &
     &                             ( TIM_PCAL_BEG .LE. TIM_OBS_BEG .AND. &
     &                               TIM_PCAL_END .GE. TIM_OBS_END       ) &
     &                         ) THEN
!
                               IF ( IFIND_PL ( N_PHC, IND_SOU_PHC, &
     &                              PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%SOU_IND(J37) ) > 0 ) THEN
                                    IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0 ) PIM%OBS(J30)%PCAL_IND(1,J34,J29) = J37
                                    PIM%OBS(J30)%PCAL_IND(2,J34,J29) = J37
                                    TIM_EPC_DIF = DABS( (TIM_OBS_BEG + TIM_OBS_END)/2.0D0 - &
     &                                                   PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%TIME_MID_R8(J37) ) 
                                    IF ( TIM_EPC_DIF < TIM_EPC_DIF_MIN ) THEN
                                         TIM_EPC_DIF_MIN = TIM_EPC_DIF
                                         PIM%OBS(J30)%PCAL_IND(3,J34,J29) = J37
                                    END IF
                               END IF
                           END IF
 4370                   CONTINUE
                    END IF
!
! ----------------- Put the pcal scan index into the 4th field of PIM%OBS()%PCAL_IND
!
                    IF ( PIM%OBS(J30)%PCAL_IND(3,J34,J29) .NE. 0 ) THEN
                         PIM%OBS(J30)%PCAL_IND(4,J34,J29) = PIM%STA(PIM%OBS(J30)%STA_IND(J34))%PCAL(J29)%IPOI_SCA(PIM%OBS(J30)%PCAL_IND(3,J34,J29))
                       ELSE
                         PIM%OBS(J30)%PCAL_IND(4,J34,J29) = 0
                    END IF
                    IF ( PIM%OBS(J30)%PCAL_IND(1,J34,J29) == 0  .AND.  &
     &                   ( PIM%CONF%DEBUG_LEVEL .GE. 1  .OR. &
     &                     PIM%CONF%CHECK_SEVERITY .GE. 3    )   ) THEN
!
                         CALL CLRCH ( STR )
                         CALL INCH  ( J30, STR )
                         CALL ERR_PASS ( IUER, IER )
                         STR(11:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_OBS_BEG, -2 )
                         STR(41:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_OBS_END, -2 )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( J29, STR1  )
!
                         CALL ERR_PASS ( IUER, IER )
                         WRITE ( 6, 2140 ) J30, PIM%C_SOU(PIM%OBS(J30)%SOU_IND), &
     &                                     PIM%STA(PIM%OBS(J30)%STA_IND(J34))%IVS_NAME, &
     &                                     J29, STR(11:33), STR(41:61), &
     &                                     PIM%OBS(J30)%NUM_EPC(J29)
 2140                    FORMAT ( 'PIMA_GET_PCAL: Cannot find phase cal for obs. ', I8, ' of source ', A, &
     &                            ' at station ', A,  ' frq_grp: ', I1, &
                                  ' for [ ', A, ' , ', A, ' ] Num_Epc: ', I8 )
!
                         N_MIS = N_MIS + 1
                         IF ( PIM%CONF%CHECK_SEVERITY .GE. 3 ) THEN
                              CALL ERR_LOG ( 7462, IUER, 'PIMA_GET_PCAL', &
     &                            'Cannot find phase cal for observation '//STR )
                              RETURN
                         END IF
                    END IF
               END IF
 4340       CONTINUE
 4300    CONTINUE
 4290 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 .AND. N_MIS > 0  ) THEN
           WRITE ( 6, 2150 ) N_MIS
 2150      FORMAT ( 'PIMA_GET_PCAL: In total, pcal was missing for ', I8, ' observatons' ) 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
           DO 4380 J38=1,PIM%NOBS
              WRITE ( 6, 2160 ) J38, PIM%C_STA(PIM%OBS(J38)%STA_IND(1)), &
     &                               PIM%C_STA(PIM%OBS(J38)%STA_IND(2)), &
     &                               PIM%OBS(J38)%PCAL_IND(1:4,1,1), PIM%OBS(J38)%PCAL_IND(1:4,2,1)
 2160         FORMAT ( 'PCAL_GET_PCAL: Ind_obs: ', I5, ' Bas: ', A, ' / ', A, &
     &                 ' Pcal_ind: ', 4(I6,1X), ' and ', 4(I6,1X) )
 4380      CONTINUE 
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           DO 4390 J39=1,PIM%NSTA
              DO 4400 J40=1,NFRG
                 WRITE ( 6, 2170 ) PIM%C_STA(J39), J40,               &
     &                             PIM%STA(J39)%PCAL(J40)%PCAL_AVAIL, &
     &                             PIM%STA(J39)%PCAL(J40)%NO_TONES,   &
     &                             PIM%STA(J39)%PCAL(J40)%NPOI,       &
     &                             PIM%STA(J39)%PCAL(J40)%PCAL_USE,   &
     &                             PIM%STA(J39)%CABLE%CAB_AVAIL,      &
     &                             PIM%STA(J39)%CABLE%CABLE_SIGN
 2170            FORMAT ( 'PCAL_GET_PCAL: Sta: ',A, &
     &                    ' Frq_grp: ', I1, &
     &                    ' Avail: ',L1, &
     &                    ' No_tones: ', I4, &
     &                    ' Num_poi: ', I7, &
     &                    ' Pcal_use: ', L1, &
     &                    ' Cable_avail: ', L1, &
     &                    ' Cable_sign: ', I2 )
 4400         CONTINUE 
 4390      CONTINUE 
      END IF
      CLOSE ( UNIT=LUN_PCA )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_PCAL: Finished on '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_PCAL  !#!
!
! ------------------------------------------------------------------------
!
      FUNCTION   ARR_R8_IS_ZERO ( N, ARR )
! ************************************************************************
! *                                                                      *
! *   Function  ARR_R8_IS_ZERO 
! *                                                                      *
! * ### 03-AUG-2011  ARR_R8_IS_ZERO  v1.0 (c) L. Petrov  03-AUG-2011 ### *
! *                                                                      *
! ************************************************************************
      LOGICAL*4  ARR_R8_IS_ZERO 
      INTEGER*4  N
      REAL*8     ARR(N)
      INTEGER*4  J1
!     
      ARR_R8_IS_ZERO = .TRUE.
      DO 410 J1=1,N
         IF ( ARR(J1) .NE. 0.0D0 ) THEN
              ARR_R8_IS_ZERO = .FALSE.
              RETURN
         END IF
 410  CONTINUE 
!
      RETURN
      END  FUNCTION   ARR_R8_IS_ZERO  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   ARR_R4_HAS_NAN ( N, ARR )
! ************************************************************************
! *                                                                      *
! *   Function  ARR_R4_HAS_NAN 
! *                                                                      *
! * ### 03-AUG-2011  ARR_R4_HAS_NAN  v1.0 (c) L. Petrov  03-AUG-2011 ### *
! *                                                                      *
! ************************************************************************
      LOGICAL*4  ARR_R4_HAS_NAN
      INTEGER*4  N
      REAL*4     ARR(N)
      LOGICAL*4  IS_R4_NAN
      INTEGER*4  J1
!     
      ARR_R4_HAS_NAN = .FALSE.
      DO 410 J1=1,N
         IF ( IS_R4_NAN ( ARR(J1) ) ) THEN
              ARR_R4_HAS_NAN = .TRUE.
              RETURN
         END IF
 410  CONTINUE 
!
      RETURN
      END  FUNCTION   ARR_R4_HAS_NAN  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   ARR_R8_HAS_NAN ( N, ARR )
! ************************************************************************
! *                                                                      *
! *   Function  ARR_R8_HAS_NAN 
! *                                                                      *
! * ### 03-AUG-2011  ARR_R8_HAS_NAN  v1.0 (c) L. Petrov  28-SEP-2019 ### *
! *                                                                      *
! ************************************************************************
      LOGICAL*4  ARR_R8_HAS_NAN
      INTEGER*4  N
      REAL*8     ARR(N)
      LOGICAL*4  IS_R8_NAN, IS_R8_INF
      INTEGER*4  J1
!     
      ARR_R8_HAS_NAN = .FALSE.
      DO 410 J1=1,N
         IF ( IS_R8_NAN ( ARR(J1) ) .OR. IS_R8_INF ( ARR(J1) ) ) THEN
              ARR_R8_HAS_NAN = .TRUE.
              RETURN
         END IF
 410  CONTINUE 
!
      RETURN
      END  FUNCTION   ARR_R8_HAS_NAN  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION PCAL_FRQ_FIX ( NTO, NFRQ, PCAL_FREQ_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine makes an attempt to fix 
! *                                                                      *
! *  ### 12-FEB-2020  PCAL_FRQ_FIX   v1.2 (c) L. Petrov  13-MAR-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  PCAL_FRQ_FIX 
      INTEGER*4  NTO, NFRQ
      REAL*8     PCAL_FREQ_R8(NTO,NFRQ)
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND_REF
      REAL*8     FRQ_STEP, FRQ_REF
      LOGICAL*4, EXTERNAL ::   IS_R8_NAN, IS_R8_INF
!
      FRQ_REF  = 0.0D0
      FRQ_STEP = 0.0D0
      DO 410 J1=1,NFRQ
         IND_REF = 0
         DO 420 J2=1,NTO-1
            IF ( .NOT. IS_R8_NAN(PCAL_FREQ_R8(J2,J1))   .AND. &
     &           .NOT. IS_R8_INF(PCAL_FREQ_R8(J2,J1))   .AND. &
     &           .NOT. IS_R8_NAN(PCAL_FREQ_R8(J2+1,J1)) .AND. &
     &           .NOT. IS_R8_INF(PCAL_FREQ_R8(J2+1,J1))       ) THEN
                 FRQ_STEP = PCAL_FREQ_R8(J2+1,J1) - PCAL_FREQ_R8(J2,J1)
                 FRQ_REF = PCAL_FREQ_R8(J2,J1)
                 IND_REF = J2
            END IF
 420     CONTINUE 
         IF ( IND_REF == 0 ) THEN
              PCAL_FRQ_FIX = -1
         END IF
         DO 430 J3=1,NTO
            IF ( IS_R8_NAN(PCAL_FREQ_R8(J3,J1)) .OR. &
     &           IS_R8_INF(PCAL_FREQ_R8(J3,J1))      ) THEN
!!                 PCAL_FREQ_R8(J3,J1) = FRQ_REF + (J3 - IND_REF)*FRQ_STEP
                 PCAL_FREQ_R8(J3,J1) = 0.0D0
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      PCAL_FRQ_FIX = 0
      RETURN
      END  FUNCTION  PCAL_FRQ_FIX   !#!#
