      SUBROUTINE PIMA_GET_TSYS ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_TSYS parses FITS-IDI files and extracts          *
! *   information about phase calibration measurements.                  *
! *                                                                      *
! * ### 10-JAN-2006   PIMA_GET_TSYS   v4.1 (c) L. Petrov 03-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( PIM_TSYS__TYPE ) :: TSYS_TMP
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, &
     &           J27, J28, IER, NTSYS, STA_ID, KP(PIM__MFIL), LP, SOU_ID, &
     &           IND_TS, SOU_IND, FRG_IND, FRG_ID, IND_OBS_FRG, &
     &           IND_TAB(PIM__MFIL), IND_ANT(PIM__MFIL),  IND_SPAN(PIM__MFIL), &
     &           IND_TIM(PIM__MFIL), IND_SOU(PIM__MFIL), IND_FRG(PIM__MFIL), &
     &           IND_TSYS(PIM__MFIL), IND_TSYS_2(PIM__MFIL), &
     &           IND_BEG_FRQ1, IND_BEG_FRQ2, IND_END_FRQ1, IND_END_FRQ2, I_FRQ
      INTEGER*4  K1, K2, IP
      REAL*8     EPS_TIME, TIM_MARGIN, TOL_BIG_SEC, TOL_SEC
      PARAMETER  ( EPS_TIME    = 0.1D0   ) ! sec
      PARAMETER  ( TIM_MARGIN  = 600.0D0 ) ! sec
      PARAMETER  ( TOL_SEC     =  20.0D0 ) ! sec
      PARAMETER  ( TOL_BIG_SEC = 240.0D0 ) ! sec
      REAL*8     TIM_TSYS, TAU_GR, TAU_PH, RATE_PH, AZ(2), ELEV(2), &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), FREQ_VTD, &
     &           TIM_OBS_BEG, TIM_OBS_END
      REAL*8,    ALLOCATABLE :: ARR1(:), ARR2(:), FRQ_TSYS(:,:), TMP_FRQ_TSYS(:,:)
      REAL*4     ARR_R4(PIM__MFRQ)
      CHARACTER  STR*128, VERA_TSYS_NAM*3, OBS_CODE*64, FINAM*128
      LOGICAL*4  FL_FAILURE, FL_FOUND, FL_FRQ_SAME
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      OBS_TYP%PLRZ    = 'RR'
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE  = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS  = VTD__BND
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_TSYS Started  on '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
! --- First pass. Collect information about the stations where
! --- Tsys measurements are available, the number of points and collecting
! --- indexes of tables
!
      NTSYS = 0
      DO 410 J1=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J1)%NAME, PIM%FILE(J1)%FITS_DESC, &
     &                     'OLD', IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7511, IUER, 'PIMA_GET_TSYS', 'Error in an '// &
     &            'attempt to open FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
!
         IND_TAB(J1) = 0
         IND_ANT(J1) = 0
         DO 420 J2=1,PIM%FILE(J1)%L_HDR
            DO 430 J3=1,PIM%FILE(J1)%L_KWD(J2)
               IF ( PIM%FILE(J1)%KEY(J3,J2)(1:8) == 'EXTNAME' ) THEN
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:30) == "'SYSTEM_TEMPERATURE'" ) THEN
                         IND_TAB(J1) = J2
                    END IF
               END IF
 430        CONTINUE
!
            IF ( IND_TAB(J1) == J2 ) THEN
                 DO 440 J4=1,PIM%FILE(J1)%L_KWD(J2)
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:22) == "'ANTENNA_NO'" ) THEN
                          IND_ANT(J1) = J4
                     END IF
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:22) == "'SOURCE_ID'" ) THEN
                          IND_SOU(J1) = J4
                     END IF
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:22) == "'FREQID  ' " ) THEN
                          IND_FRG(J1) = J4
                     END IF
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:20) == "'TIME    '" ) THEN
                          IND_TIM(J1) = J4
                     END IF
!
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:25) == "'TIME_INTERVAL'" ) THEN
                          IND_SPAN(J1) = J4
                     END IF
!
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:20) == "'TSYS_1  '" ) THEN
                          IND_TSYS(J1) = J4
                     END IF
!
                     IF ( PIM%FILE(J1)%KEY(J4,J2)(11:20) == "'TSYS_2  '" ) THEN
                          IND_TSYS_2(J1) = J4
                     END IF
 440             CONTINUE
            END IF
 420     CONTINUE
!
         IF ( IND_TAB(J1) == 0 ) THEN
              IF ( PIM%CONF%WARNING ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_GET_TSYS: no system '// &
     &                            'temperature table was found in uv-file '// &
     &                     PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))
              END IF
              KP(J1) = 0
            ELSE
              IF ( IND_ANT(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7512, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'ANTENNA_NO was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_SOU(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7513, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'SOURCE_ID was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_FRG(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7514, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'FREQID was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_TIM(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7515, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'TIME was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_SPAN(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7516, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'TIME_INTERVAL was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_TSYS(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7517, IUER, 'PIMA_GET_TSYS', 'Keyword '// &
     &                 'TSYS_1 was not found in the TSYS table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, 'SYSTEM_TEMPERATURE', 'NAXIS2', &
     &                               KP(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7519, IUER, 'PIMA_GET_TSYS', 'Failure to '// &
     &                 'get the number of points of system temperature '// &
     &                 'measurements in the FITS-IDI file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
!
              NTSYS = NTSYS + KP(J1)
!
              DO 450 J5=1,KP(J1)
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1), &
     &                      J5, PIM%FILE(J1)%KEY(IND_FRG(J1),IND_TAB(J1)), &
     &                      1, FRG_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J5, STR )
                      CALL ERR_LOG ( 7520, IUER, 'PIMA_GET_TSYS', &
     &                    'Error in getting frequency group id for the '// &
     &                     STR(1:I_LEN(STR))// &
     &                    '-th TSYS of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                     RETURN
                 END IF
                 FRG_IND = PIM%FILE(J1)%REF_FRG(1,FRG_ID)
                 IF ( FRG_IND < 1 .OR. FRG_IND > PIM%NFRG ) GOTO 450
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1), J5,   &
     &                              PIM%FILE(J1)%KEY(IND_ANT(J1),IND_TAB(J1)), &
     &                              1, STA_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J5, STR )
                      CALL ERR_LOG ( 7521, IUER, 'PIMA_GET_TSYS', 'Error in '// &
     &                    'getting station id for the '//STR(1:I_LEN(STR))// &
     &                    '-th TSYS of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 IF ( STA_ID .LE. 0  .OR.  STA_ID > PIM%NSTA ) THEN
                      write ( 6, * ) ' j1=', j1,' j4=',j4, ' sta_id=',sta_id
                      write ( 6, * ) ' ind_and(j1) = ', ind_ant(j1)
                      CALL ERR_LOG ( 7522, IUER, 'PIMA_GET_TSYS', 'Error in '// &
     &                    'getting station id for the '//STR(1:I_LEN(STR))// &
     &                    '-th TSYS of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 IF ( PIM%REF_STA(STA_ID,J1) .LE. 0  .OR. &
     &                PIM%REF_STA(STA_ID,J1) > PIM%NSTA   ) THEN
!
                      write ( 6, * ) ' j1=', j1,' j4=',j4, ' sta_id=',sta_id, &
     &                           ' pim%ref_sta(sta_id,j1) = ', pim%ref_sta(sta_id,j1)
                      CALL ERR_LOG ( 7523, IUER, 'PIMA_GET_TSYS', 'Trap of '// &
     &                    'internal control in getting tsys data from '// &
     &                    'FITS-IDI file '//PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 PIM%STA(PIM%REF_STA(STA_ID,J1))%TSYS(FRG_IND)%NPOI = &
     &                   PIM%STA(PIM%REF_STA(STA_ID,J1))%TSYS(FRG_IND)%NPOI + 1
 450          CONTINUE
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J1)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7524, IUER, 'PIMA_GET_TSYS', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
 410  CONTINUE
!
! --- Second pass: load the data into PIM data structure
!
      DO 460 J6=1,PIM%NFRG
        DO 470 J7=1,PIM%NSTA
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, 210 ) PIM%C_STA(J7), J6, PIM%STA(J7)%TSYS(J6)%NPOI
 210            FORMAT ( 'PIMA_GET_TSYS Sta: ', A, ' Frq_grp: ', I1, ' Num_poi: ', I6 )
           END IF
           PIM%STA(J7)%TSYS(J6)%NPOL = PIM%NPOL
           IF ( PIM%STA(J7)%TSYS(J6)%NPOI > 0 ) THEN
                PIM%STA(J7)%TSYS(J6)%AVAIL = .TRUE.
!
! --------- Allocate dynamic memory for TSYS
!
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%TSYS(PIM%NFRQ,PIM%STA(J7)%TSYS(J6)%NPOI,PIM%STA(J7)%TSYS(J6)%NPOL) )
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%TIME_MID_R8 (PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4(PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%SOU_IND(PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%AZ_R4(PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( PIM%STA(J7)%TSYS(J6)%ELEV_R4(PIM%STA(J7)%TSYS(J6)%NPOI) )
!
            ALLOCATE ( FRQ_TSYS(PIM%NFRQ,PIM%STA(J7)%TSYS(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%NFRQ*PIM%STA(J7)%TSYS(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7525, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for FRQ_TSYS' )
                 RETURN
            END IF
!
! --------- Initialization
!
            PIM%STA(J7)%TSYS(J6)%TSYS = 0.0D0
            PIM%STA(J7)%TSYS(J6)%TIME_MID_R8 = 0.0D0
            PIM%STA(J7)%TSYS(J6)%SOU_IND = 0
            PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4 = 0.0
            PIM%STA(J7)%TSYS(J6)%AZ_R4        = 0.0
            PIM%STA(J7)%TSYS(J6)%ELEV_R4      = 0.0
            FRQ_TSYS = 0.0D0
!
! --------- Collect TSYS information from each file
!
            LP = 0
            DO 490 J9=1,PIM%L_FIL
!
! ------------ IT may happen that the current file does not contain Tsys
!
               IF ( KP(J9) == 0 ) GOTO 490
               IF ( IND_ANT(J9) == 0 ) GOTO 490
               IF ( IND_TAB(J9) == 0 ) GOTO 490
!
! ------------ Open thye FITS file with data
!
               CALL ERR_PASS   ( IUER, IER )
               CALL FFITS_OPEN ( PIM%FILE(J9)%NAME, &
     &                           PIM%FILE(J9)%FITS_DESC, 'OLD', IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7526, IUER, 'PIMA_GET_TSYS', &
     &                  'Error in an attempt to open FITS UV-file '// &
     &                   PIM%FILE(J1)%NAME )
                    RETURN
               END IF
               DO 4100 J10=1,KP(J9)
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J9)%FITS_DESC, IND_TAB(J9), &
     &                          J10, PIM%FILE(J9)%KEY(IND_FRG(J9),IND_TAB(J9)), &
     &                          1, FRG_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J10, STR )
                       CALL ERR_LOG ( 7527, IUER, 'PIMA_GET_TSYS', &
     &                     'Error in getting frequency group id for the '// &
     &                      STR(1:I_LEN(STR))// &
     &                     '-th TSYS of the FITS-IDI file '// &
     &                      PIM%FILE(J9)%NAME )
                       RETURN
                  END IF
!
! --------------- At this run we consider only points from the J6-th frequency group
!
                  IF ( PIM%FILE(J9)%REF_FRG(1,FRG_ID) .NE. J6 ) GOTO 4100
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J9)%FITS_DESC, IND_TAB(J9), J10,   &
     &                               PIM%FILE(J9)%KEY(IND_ANT(J9),IND_TAB(J9)), &
     &                               1, STA_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J10, STR )
                       CALL ERR_LOG ( 7528, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &                     'in getting station id for the '// &
     &                      STR(1:I_LEN(STR))//'-th TSYS of the FITS-IDI '// &
     &                     'file '//PIM%FILE(J9)%NAME )
                      RETURN
                  END IF
!
                  IF ( PIM%REF_STA(STA_ID,J9) .EQ. J7 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J9)%FITS_DESC,   IND_TAB(J9), &
     &                          J10, PIM%FILE(J9)%KEY(IND_TIM(J9),IND_TAB(J9)), &
     &                          1, ARR_R4, IER )
                     LP = LP + 1
                     PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) = ARR_R4(1)
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J10, STR )
                          CALL ERR_LOG ( 7529, IUER, 'PIMA_GET_TSYS',  &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of time tag from the FITS-IDI file '// &
     &                         PIM%FILE(J9)%NAME )
                         RETURN
                     END IF
                     IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                          CONTINUE
                       ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                          PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) = &
     &                        PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) - &
     &                        PIM%UTC_MTAI/86400.0D0
                     END IF
!
! ------------------ Convert time to amount of seconds elapsed from
! ------------------ MJD_0, TAI_0
!
                     PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) = &
     &                       PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP)*86400.0D0 - &
     &                       (PIM%MJD_0 - PIM%FILE(J9)%MJD_REF)*86400.D0 - &
     &                       PIM%TAI_0
!
! ------------------ Check the range
!
                     IF ( PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) < &
     &                    PIM%TIM_R8(1) - TIM_MARGIN ) THEN
                          LP = LP - 1
                          GOTO 4100
                     END IF
!
                     IF ( PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) > &
     &                    PIM%TIM_R8(PIM%NEPC) + TIM_MARGIN ) THEN
                          LP = LP - 1
                          GOTO 4100
                     END IF
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J9)%FITS_DESC, IND_TAB(J9), &
     &                          J10, PIM%FILE(J9)%KEY(IND_SOU(J9),IND_TAB(J9)), &
     &                          1, SOU_ID, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J5, STR )
                          CALL ERR_LOG ( 7530, IUER, 'PIMA_GET_TSYS', &
     &                        'Error in getting source id for the '// &
     &                         STR(1:I_LEN(STR))// &
     &                        '-th TSYS of the FITS-IDI file '// &
     &                         PIM%FILE(J1)%NAME )
                         RETURN
                     END IF
                     IF ( SOU_ID < 1 ) THEN
                          LP = LP - 1
                          GOTO 4100
                     END IF
                     PIM%STA(J7)%TSYS(J6)%SOU_IND(LP) = PIM%REF_SOU(SOU_ID,J9)
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J9)%FITS_DESC, IND_TAB(J9),    &
     &                          J10, PIM%FILE(J9)%KEY(IND_TSYS(J9),IND_TAB(J9)), &
     &                          PIM%FILE(J9)%NFRQ, ARR_R4, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J10, STR )
                          WRITE ( 6, * ) 'PIM%FILE(J9)%NFRQ= ', PIM%FILE(J9)%NFRQ
                          CALL ERR_LOG ( 7531, IUER, 'PIMA_GET_TSYS', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of system temperature from the '// &
     &                        'FITS-IDI file '//PIM%FILE(J9)%NAME )
                         RETURN
                     END IF
                     DO 4110 J11=1,PIM%FILE(J9)%NFRQ
                        PIM%STA(J7)%TSYS(J6)%TSYS(PIM%FILE(J9)%REF_FRQ(J11,FRG_ID),LP,1) = ARR_R4(J11)
                        FRQ_TSYS(PIM%FILE(J9)%REF_FRQ(J11,FRG_ID),LP) = PIM%FRQ(J11,J6)%FREQ
 4110                CONTINUE
!
                     IF ( PIM%STA(J7)%TSYS(J6)%NPOL == 2  .AND. IND_TSYS_2(J9) > 0 ) THEN
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J9)%FITS_DESC, IND_TAB(J9),    &
     &                         J10, PIM%FILE(J9)%KEY(IND_TSYS_2(J9),IND_TAB(J9)), &
     &                         PIM%FILE(J9)%NFRQ, ARR_R4, IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J10, STR )
                               WRITE ( 6, * ) 'PIM%FILE(J9)%NFRQ= ', PIM%FILE(J9)%NFRQ
                               CALL ERR_LOG ( 7532, IUER, 'PIMA_GET_TSYS', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of system temperature from '// &
     &                             'the FITS-IDI file '//PIM%FILE(J9)%NAME )
                              RETURN
                         END IF
                         DO 4120 J12=1,PIM%FILE(J9)%NFRQ
                            PIM%STA(J7)%TSYS(J6)%TSYS(J12,LP,2) = ARR_R4(J12)
 4120                    CONTINUE
                     END IF
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J9)%FITS_DESC,   IND_TAB(J9),  &
     &                          J10, PIM%FILE(J9)%KEY(IND_SPAN(J9),IND_TAB(J9)), &
     &                          1, PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4(LP), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J10, STR )
                          CALL ERR_LOG ( 7534, IUER, 'PIMA_GET_TSYS', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of time span from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J9)%NAME )
                         RETURN
                     END IF
                     PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4(LP) = &
     &                           PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4(LP)*86400.0E0
                  END IF
                  IF ( PIM%CONF%DEBUG_LEVEL .EQ. 11 .AND. LP > 0 ) THEN
                       STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP), -2 )
                       WRITE ( 6, 220 ) J10, J9, PIM%FILE(J9)%NAME(1:I_LEN(PIM%FILE(J9)%NAME)), &
     &                                  PIM%C_STA(J7), &
     &                                  PIM%C_SOU(PIM%STA(J7)%TSYS(J6)%SOU_IND(LP)), &
     &                                  STR(1:24), FRG_ID, PIM%FILE(J9)%REF_FRG(1,FRG_ID)
 220                   FORMAT ( 'PIMA_GET_TSYS  Kpoi= ', I4, ' Kfil= ', I3, &
     &                          ' File: ',A, ' Sta: ', A, &
     &                          ' Sou: ', A, ' Date: ', A, ' Frg_id: ', I1, &
     &                          ' Frg_id_file: ', I1 )
                  END IF
 4100          CONTINUE
!
               CALL ERR_PASS    ( IUER, IER )
               CALL FFITS_CLOSE ( PIM%FILE(J9)%FITS_DESC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7535, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &                  'in an attempt to close FITS UV-file '// &
     &                   PIM%FILE(J9)%NAME )
                    RETURN
               END IF
 490        CONTINUE
!
            PIM%STA(J7)%TSYS(J6)%NPOI = LP
!
! --------- Allocate temporary arrays
!
            ALLOCATE ( ARR1(PIM%STA(J7)%TSYS(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%STA(J7)%TSYS(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7536, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for temporary array ARR2' )
                 RETURN
            END IF
!
            ALLOCATE ( ARR2(PIM%STA(J7)%TSYS(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%STA(J7)%TSYS(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7537, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for temporary array ARR1' )
                 RETURN
            END IF
!
! --------- Now sorting TSYS arrays according to time and remove duplicates
!
! --------- First sort indexes
!
            DO 4140 J14=1,PIM%STA(J7)%TSYS(J6)%NPOI
               ARR1(J14) = PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(J14) ! Time index
               ARR2(J14) = 1.0D0*J14   ! Point indexes
 4140       CONTINUE
!
! --------- Sort indexes
!
            CALL SORT8 ( PIM%STA(J7)%TSYS(J6)%NPOI, ARR1, ARR2 )
!
! --------- Copy PIM%STA(J7)%TSYS to a temporary array
!
            TSYS_TMP%AVAIL = PIM%STA(J7)%TSYS(J6)%AVAIL
            TSYS_TMP%NPOI  = PIM%STA(J7)%TSYS(J6)%NPOI
            TSYS_TMP%NPOL  = PIM%STA(J7)%TSYS(J6)%NPOL
            ALLOCATE ( TSYS_TMP%TSYS(PIM%NFRQ,PIM%STA(J7)%TSYS(J6)%NPOI,PIM%STA(J7)%TSYS(J6)%NPOL) )
            ALLOCATE ( TSYS_TMP%TIME_MID_R8 (PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( TSYS_TMP%TIME_SPAN_R4(PIM%STA(J7)%TSYS(J6)%NPOI) )
            ALLOCATE ( TSYS_TMP%SOU_IND(PIM%STA(J7)%TSYS(J6)%NPOI) )
!
            ALLOCATE ( TMP_FRQ_TSYS(PIM%NFRQ,PIM%STA(J7)%TSYS(J6)%NPOI), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%NFRQ*PIM%STA(J7)%TSYS(J6)%NPOI, STR )
                 CALL ERR_LOG ( 7538, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &               'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for TMP_FRQ_TSYS' )
                 RETURN
            END IF
!
            CALL COPY_R8 ( PIM%NFRQ*TSYS_TMP%NPOI*PIM%STA(J7)%TSYS(J6)%NPOL, &
     &                     PIM%STA(J7)%TSYS(J6)%TSYS, TSYS_TMP%TSYS )
            CALL COPY_R8 ( TSYS_TMP%NPOI, &
     &                     PIM%STA(J7)%TSYS(J6)%TIME_MID_R8, TSYS_TMP%TIME_MID_R8 )
            CALL COPY_R4 ( TSYS_TMP%NPOI, PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4, &
     &                     TSYS_TMP%TIME_SPAN_R4 )
            CALL COPY_I4 ( TSYS_TMP%NPOI, PIM%STA(J7)%TSYS(J6)%SOU_IND, &
     &                     TSYS_TMP%SOU_IND )
            CALL COPY_R8 ( PIM%NFRQ*PIM%STA(J7)%TSYS(J6)%NPOI, FRQ_TSYS, TMP_FRQ_TSYS )
!
            LP = 0 ! points counter
!
! --------- Consecutive coping sorted tsys from temporary array back to
! --------- TSYS(J6)%STA(J7)%TSYS with removal of duplicates
!
            DO 4150 J15=1,TSYS_TMP%NPOI
               IND_TS = NINT ( ARR2(J15) )
               IF ( LP > 0 ) THEN
!
! ----------------- Check for records with the same time tag
!
                    IF ( ABS ( TSYS_TMP%TIME_MID_R8(IND_TS) - &
     &                         PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP) ) < EPS_TIME ) THEN
!
! ---------------------- Aga, these two records have the same time tag.
! ---------------------- We try to merge them. We need to learn which record
! ---------------------- corresponds to higher frequencies
!
                         IND_BEG_FRQ1 = 0
                         IND_BEG_FRQ2 = 0
                         IND_END_FRQ1 = 0
                         IND_END_FRQ2 = 0
                         FL_FRQ_SAME = .TRUE.
                         DO 4160 J16=1,PIM%NFRQ
                            IF ( DABS(TMP_FRQ_TSYS(J16,IND_TS) - &
     &                                FRQ_TSYS(J16,LP) ) > 1.D0 ) THEN
                                 FL_FRQ_SAME = .FALSE.
                            END IF
                            IF ( TMP_FRQ_TSYS(J16,IND_TS) > PIMA__MIN_FRQ ) THEN
                                 IND_END_FRQ1 = J16
                                 IF ( IND_BEG_FRQ1 == 0 ) IND_BEG_FRQ1 = J16
                            END IF
                            IF ( TMP_FRQ_TSYS(J16,LP) > PIMA__MIN_FRQ ) THEN
                                 IND_END_FRQ2 = J16
                                 IF ( IND_BEG_FRQ2 == 0 ) IND_BEG_FRQ2 = J16
                            END IF
 4160                    CONTINUE
                         IF ( FL_FRQ_SAME ) GOTO 4150
                         IF ( (IND_END_FRQ1 - IND_BEG_FRQ1 + 1) + &
     &                        (IND_END_FRQ2 - IND_BEG_FRQ2 + 1) > PIM%NFRQ ) THEN
                              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               TSYS_TMP%TIME_MID_R8(IND_TS), -2 )
                              WRITE  ( 6, * ) ' IND_BRG_FRQ1= ', IND_BEG_FRQ1, &
     &                                        ' IND_BEG_FRQ2= ', IND_BEG_FRQ2
                              WRITE  ( 6, * ) ' J6 = FRG = ', J6
                              WRITE  ( 6, * ) ' IND_END_FRQ1= ', IND_END_FRQ1, &
     &                                        ' IND_END_FRQ2= ', IND_END_FRQ2
                              WRITE  ( 6, * ) ' PIM%NFRQ= ', PIM%NFRQ, &
     &                                        ' PIM%NFRG= ', PIM%NFRG
                              WRITE  ( 6, * ) ' IND_TS= ', IND_TS, ' LP= ' ,LP
                              WRITE  ( 6, * ) ' STA ', PIM%C_STA(J7), ' J7= ', INT2(J7)
                              WRITE  ( 6, * ) ' (orig) FRQ_TSYS: ', FRQ_TSYS(1:PIM%NFRQ,LP)
                              WRITE  ( 6, * ) ' (sort) TMP_FRQ:  ', TMP_FRQ_TSYS(1:PIM%NFRQ,IND_TS)
                              CALL ERR_LOG ( 7539, IUER, 'PIMA_GET_TSYS', &
     &                            'Trap of internal control: tsys records '// &
     &                            ' at station '//PIM%STA(J7)%IVS_NAME// &
     &                            ' at '//STR(1:21)//' in total, have '// &
     &                            'too many frequencies' )
                              RETURN
                         END IF
!
                         IF ( FRQ_TSYS(IND_BEG_FRQ2,LP) > &
     &                        TMP_FRQ_TSYS(IND_BEG_FRQ1,IND_TS) ) THEN
!
! --------------------------- The LP -th record has the higher frequencies
!
                              I_FRQ = IND_END_FRQ1
                              DO 4170 J17=IND_BEG_FRQ2,IND_END_FRQ2
                                 I_FRQ = I_FRQ + 1
                                 TSYS_TMP%TSYS(I_FRQ,IND_TS,1) = &
     &                                         PIM%STA(J7)%TSYS(J6)%TSYS(J17,LP,1)
                                 IF ( PIM%STA(J7)%TSYS(J6)%NPOL == 2 ) THEN
                                      TSYS_TMP%TSYS(I_FRQ,IND_TS,2) = &
     &                                    PIM%STA(J7)%TSYS(J6)%TSYS(J17,LP,2)
                                 END IF
 4170                         CONTINUE
                            ELSE IF ( FRQ_TSYS(IND_BEG_FRQ2,LP)   < &
     &                                TMP_FRQ_TSYS(IND_BEG_FRQ1,IND_TS) ) THEN
!
! --------------------------- The LP -th record has the lower frequencies
!
! --------------------------- We need first to move the data in the IND_TS -th
! --------------------------- record to the end and make a hole for the
! --------------------------- LP-th record
!
                              I_FRQ = IND_END_FRQ1 + IND_END_FRQ2
                              DO 4180 J18=IND_END_FRQ1,IND_BEG_FRQ1,-1
                                 TSYS_TMP%TSYS(I_FRQ,IND_TS,1) = &
     &                                         PIM%STA(J7)%TSYS(J6)%TSYS(J18,LP,1)
                                 IF ( PIM%STA(J7)%TSYS(J6)%NPOL == 2 ) THEN
                                      TSYS_TMP%TSYS(I_FRQ,IND_TS,2) = &
     &                                    PIM%STA(J7)%TSYS(J6)%TSYS(J18,LP,2)
                                 END IF
                                 I_FRQ = I_FRQ - 1
 4180                         CONTINUE
!
! --------------------------- Now we put the data from the IND_TS -th record
! --------------------------- into the hole
!
                              DO 4190 J19=IND_BEG_FRQ2,IND_END_FRQ2
                                 TSYS_TMP%TSYS(J19,IND_TS,1) = &
     &                                       PIM%STA(J7)%TSYS(J6)%TSYS(J19,LP,1)
                                 IF ( PIM%STA(J7)%TSYS(J6)%NPOL == 2 ) THEN
                                      TSYS_TMP%TSYS(J19,IND_TS,2) = &
     &                                    PIM%STA(J7)%TSYS(J6)%TSYS(J19,LP,2)
                                 END IF
 4190                         CONTINUE
                         END IF
!
! ---------------------- We merged IND_TS -th records and LP-th record.
! ---------------------- Now we discard the LP-th record
!
                         GOTO 4150
                    END IF
               END IF
               LP = LP + 1 ! increment of the point counter
!
! ------------ Copying back the stuff
!
               DO 4200 J20=1,PIM%NFRQ
                  PIM%STA(J7)%TSYS(J6)%TSYS(J20,LP,1) = TSYS_TMP%TSYS(J20,IND_TS,1)
                  IF ( PIM%STA(J7)%TSYS(J6)%NPOL == 2 ) THEN
                       PIM%STA(J7)%TSYS(J6)%TSYS(J20,LP,2) = TSYS_TMP%TSYS(J20,IND_TS,2)
                  END IF
 4200          CONTINUE
               PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP)  = TSYS_TMP%TIME_MID_R8(IND_TS)
               PIM%STA(J7)%TSYS(J6)%TIME_SPAN_R4(LP) = TSYS_TMP%TIME_SPAN_R4(IND_TS)
               PIM%STA(J7)%TSYS(J6)%SOU_IND(LP)      = TSYS_TMP%SOU_IND(IND_TS)
               FRQ_TSYS(1:PIM%NFRQ,LP)               = TMP_FRQ_TSYS(1:PIM%NFRQ,IND_TS)
!
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_DELAY ( PIM%C_SOU(PIM%STA(J7)%TSYS(J6)%SOU_IND(LP)), &
     &                       PIM%C_STA(J7), PIM%C_STA(J7), &
     &                       PIM%MJD_0, &
     &                       PIM%TAI_0 + PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP), &
     &                       OBS_TYP, VTD, TAU_GR, RATE_PH, &
     &                       DER_DEL, DER_RAT, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7540, IUER, 'PIMA_GET_TSYS', 'Error in '// &
     &                  'an attempt to compute theoretical path delay for '// &
     &                  'Tsys measurement' )
                    RETURN
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_GET_AZEL ( PIM%C_SOU(PIM%STA(J7)%TSYS(J6)%SOU_IND(LP)), &
     &                             PIM%C_STA(J7), PIM%C_STA(J7), &
     &                             PIM%MJD_0, &
     &                             PIM%TAI_0 + PIM%STA(J7)%TSYS(J6)%TIME_MID_R8(LP), &
     &                             VTD, AZ, ELEV, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J1, STR )
                    CALL ERR_LOG ( 7541, IUER, 'PIMA_GET_TSYS', 'Error in '// &
     &                  'an attempt to compute azimuth and elevation delay ' )
                    RETURN
               END IF
!
               PIM%STA(J7)%TSYS(J6)%AZ_R4(LP)   = AZ(1)
               PIM%STA(J7)%TSYS(J6)%ELEV_R4(LP) = ELEV(1)
 4150       CONTINUE
            PIM%STA(J7)%TSYS(J6)%NPOI = LP
!
            DEALLOCATE ( TSYS_TMP%TSYS )
            DEALLOCATE ( TSYS_TMP%TIME_MID_R8  )
            DEALLOCATE ( TSYS_TMP%TIME_SPAN_R4 )
            DEALLOCATE ( TSYS_TMP%SOU_IND      )
            DEALLOCATE ( FRQ_TSYS )
            DEALLOCATE ( TMP_FRQ_TSYS )
            DEALLOCATE ( ARR1     )
            DEALLOCATE ( ARR2     )
          END IF
 470    CONTINUE
 460  CONTINUE
!
      IF ( PIM%CONF%TSYS_EXT_CODE == PIMA__TSYS_EXT_VERA ) THEN
           DO 4220 J22=1,PIM%NSTA
              IF ( PIM%STA(J22)%IVS_NAME == 'VERAMZSW' ) THEN
                   VERA_TSYS_NAM = 'MIZ'
                 ELSE IF ( PIM%STA(J22)%IVS_NAME == 'VERAIRIK' ) THEN
                   VERA_TSYS_NAM = 'IRK'
                 ELSE IF ( PIM%STA(J22)%IVS_NAME == 'VERAISGK' ) THEN
                   VERA_TSYS_NAM = 'ISG'
                 ELSE IF ( PIM%STA(J22)%IVS_NAME == 'VERAOGSW' ) THEN
                   VERA_TSYS_NAM = 'OGA'
              END IF
!
              OBS_CODE = PIM%OBS_CODE
              IP = INDEX ( OBS_CODE, '-' )
              CALL CLRCH ( OBS_CODE(IP:) )
              FINAM = PIM%CONF%TSYS_EXT_PAR(1:I_LEN(PIM%CONF%TSYS_EXT_PAR))// &
     &                '/'//VERA_TSYS_NAM//'/'// &
     &                OBS_CODE(1:I_LEN(OBS_CODE))//'_Tsys_'// &
     &                VERA_TSYS_NAM//'.dat'
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_VERA_TSYS ( PIM, VTD, J22, FINAM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7542, IUER, 'PIMA_GET_TSYS', 'Error '// &
     &                 'in VERA tsys' )
                   RETURN
              END IF
 4220      CONTINUE
      END IF
!
! --- Now put indexes of system temperature into the  obs data structure
!
      FL_FAILURE = .FALSE.
      DO 4230 J23=1,PIM%NFRG
         DO 4240 J24=1,PIM%NOBS
!
! --------- Search for the frequency group index
!
            IND_OBS_FRG = 0
            DO 4250 J25=1,PIM%OBS(J24)%NUVS
               IF ( PIM%OBS(J24)%GLO_FRG_INDS(J25) == J23 ) IND_OBS_FRG = J25
 4250       CONTINUE 
            IF ( IND_OBS_FRG == 0 ) GOTO 4240
            IF ( PIM%OBS(J24)%NUM_EPC(IND_OBS_FRG) .LE. 0 ) GOTO 4240
!
            TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J24)%UV_IND(1,IND_OBS_FRG))%TIM_IND)
            TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J24)%UV_IND(PIM%OBS(J24)%NUM_EPC(IND_OBS_FRG),IND_OBS_FRG))%TIM_IND)
            DO 4260 J26=1,2
               IF ( PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%AVAIL ) THEN
                    FL_FOUND = .FALSE.
                    DO 4270 J27=1,PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%NPOI
                       TIM_TSYS = PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%TIME_MID_R8(J27)
                       IF ( TIM_TSYS .GE. TIM_OBS_BEG - TOL_SEC &
     &                      .AND.                               &
     &                      TIM_TSYS .LE. TIM_OBS_END + TOL_SEC ) THEN
!
                            PIM%OBS(J24)%TSYS_IND(J26,J23) = J27
                            FL_FOUND = .TRUE.
                       END IF
 4270               CONTINUE
 8270               CONTINUE 
!
                    IF ( .NOT. FL_FOUND ) THEN
!
! ---------------------- Not found? Another search with bigger tolerance
!
                         DO 4280 J28=1,PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%NPOI
                            TIM_TSYS = PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%TIME_MID_R8(J28)
                            SOU_IND  = PIM%STA(PIM%OBS(J24)%STA_IND(J26))%TSYS(J23)%SOU_IND(J28)
                            IF ( SOU_IND == PIM%OBS(J24)%ROOT_SOU_IND ) THEN
                                 IF ( TIM_TSYS .GE. TIM_OBS_BEG - TOL_BIG_SEC &
     &                                .AND.                                   &
     &                                TIM_TSYS .LE. TIM_OBS_END + TOL_BIG_SEC ) THEN
                                      PIM%OBS(J24)%TSYS_IND(J26,J23) = J28
                                 END IF
                            END IF
 4280               CONTINUE
                    END IF
                    IF ( PIM%OBS(J24)%TSYS_IND(J26,J23) == 0  .AND.  &
     &                   PIM%CONF%CHECK_SEVERITY .GE. 3          ) THEN
!
                         CALL CLRCH ( STR )
                         CALL INCH  ( J24, STR )
                         STR(10:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_OBS_BEG, 0 )
                         CALL ERR_PASS ( IUER, IER )
                         CALL ERR_LOG  (75430, IER, 'PIMA_GET_TSYS', &
     &                       'Cannot find system temperature for observation '// &
     &                        STR(1:I_LEN(STR(1:8)))//' of source '// &
     &                        PIM%C_SOU(PIM%OBS(J24)%SOU_IND)// &
     &                       ' at station '// &
     &                        PIM%STA(PIM%OBS(J24)%STA_IND(J26))%IVS_NAME// &
     &                       ' at '//STR(10:31)//' although TSYS is marked '// &
     &                       'as available' )
                         FL_FAILURE = .TRUE.
                    END IF
               END IF
 4260       CONTINUE
 4240    CONTINUE
 4230 CONTINUE
!
      IF ( FL_FAILURE ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%CHECK_SEVERITY, STR )
           CALL ERR_LOG ( 7544, IUER, 'PIMA_GET_TSYS', 'Check for system '// &
     &         'temoperature for experiment '//PIM%CONF%SESS_CODE// &
     &         ' did not pass test at severity check level '//STR )
           RETURN
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_TSYS Finished on '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_TSYS  !#!#
