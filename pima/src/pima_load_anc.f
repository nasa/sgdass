       SUBROUTINE PIMA_LOAD_ANC ( PIM, VTD, TELEMTR_FIL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_ANC  reads the file in with flag on-off time,    *
! *   cable calibration, Tsys, weather information and puts them         *
! *   in PIMA internal structures.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         PIM ( PIMA__TYP ) -- Object with information related to      *
! *                              program PIMA.                           *
! *         VTD ( VTD__TYP  ) -- Object with information related to      *
! *                              package VTD for computed apriori path   *
! *                              delay.                                  *
! * TELEMTR_FIL ( CHARACTER ) -- Name of the telemetry file in anc       *
! *                              format.                                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * Copyright (c) 1975-2025 United States Government as represented by   *
! * the Administrator of the National Aeronautics and Space              *
! * Administration. All Rights Reserved.                                 *
! *                                                                      *
! *  ### 20-FEB-2025 PIMA_LOAD_ANC  v1.1 (d) L. Petrov  20-JAN-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      INCLUDE   'atp.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( ANC__TYP   ) :: ANC
      CHARACTER  TELEMTR_FIL*(*)
      INTEGER*4  IUER
      INTEGER*4  MIND, M_TIM
      PARAMETER  ( M_TIM  =  32*1024 )
      PARAMETER  ( MIND   =      300 )
      CHARACTER  STR*128, STR1*128, STR2*128, STA_NAM*8, SOU_NAM*8, REG*5, &
     &           SOU_ONS(M_TIM)*8, POL_STR*1
      REAL*8     TIM_EPS, TSYS__MIN, TSYS__MAX, FRQ_DIF__MIN, TSYS__TIM_TOL, &
     &           M__CAB_SHARE, PIM__SPAN_DEF, PIM__SCL_OBS_DUR, PIM__SCL_FRQ_DIF
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',=' )
      PARAMETER  ( TIM_EPS = 1.0D0 )
      PARAMETER  ( TSYS__MIN = 2.0D0 )
      PARAMETER  ( TSYS__MAX = 30000.D0 )
      PARAMETER  ( TSYS__TIM_TOL  = 700.0D0 )
      PARAMETER  ( FRQ_DIF__MIN = 1.0D7 )
      PARAMETER  ( M__CAB_SHARE = 0.20D0 )
      PARAMETER  ( PIM__SPAN_DEF = 600.0D0 )  ! Default time span of phase-cal and Tsys
      PARAMETER  ( PIM__SCL_OBS_DUR = 2.2D0 ) ! if the difference between mid-scan and Tsys measurement < PIM__SCL_OBS_DUR*scan_duration
!                                             ! then Tsys measurement is associated with the scan
      PARAMETER  ( PIM__SCL_FRQ_DIF = 1.2D0 ) ! if the difference between 
!
      LOGICAL*1  FL_TSYS, FL_CABLE, FL_MET
      REAL*8     TIM_DIF_MIN, OBS_DURA, FRQ_DIF_MIN, AVR_CAB, MAX_DEV
      LOGICAL*4  FL_USE, FL_FOUND
!
      INTEGER*4  NBUF, J1,  J2,  J3,  J4 , J5 , J6,  J7,  J8,  J9, J10, &
     &           J11, J12, J13, J14, J15, J16, IV(M_TIM), &
     &           LIND, IND(2,MIND), IND_STA, IND_FRG, &
     &           IND_SOU, IND_SCA, IND_FRQ, IND_FRQ_1, IND_FRQ_2, &
     &           IND_TSYS, IND_STA_BAS, N_ONS, &
     &           N_CAB, N_MET, SIGN_CAB, N_TSYS, IND_OBS, N_MIS, &
     &           PIM_MIN_FRG, PIM_MAX_FRG, IND_SOU_ANC, IND_SOU_TSYS, &
     &           I_SOU(PIM__MSOU), IND_MAX_DEV, N_ITER, NP, L_SOU, IER
      REAL*8     TIM_DIF
      CHARACTER  DATE_STR*30, WORD4*64, WORD5*64
      CHARACTER, EXTERNAL ::  MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL ::  ADD_CLIST, ILEN, I_LEN, LTM_DIF
!      
      CALL GETENVAR ( 'PIMAVAR_MIN_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MIN_FRG )
        ELSE 
           PIM_MIN_FRG = 1
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_MAX_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MAX_FRG )
        ELSE 
           PIM_MAX_FRG = PIM%NFRG
      END IF
!
! --- Initlialize VTD object
!
      IF ( VTD%STATUS .NE. VTD__INIT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_INIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8241, IUER, 'PIMA_LOAD_ANC', 'Error in an '// &
     &              'attempt to initialize VTD oibject' )
                RETURN
           END IF
      END IF
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
!
! -------- Read and parse VTD configuration file
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8242, IUER, 'PIMA_LOAD_ANC', 'Error in '// &
     &              'an attempt to read configuration file '// &
     &               PIM%CONF%VTD_CONFIG_FILE )
                RETURN
           END IF
!
! -------- Require to use NERS for defining UTC(t)
!
           VTD%CONF%FINAM_LEAPSEC = VTD__NERS_STR 
!
! -------- Load catalogues, ephemerides, EOP series and other data files
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD ( VTD, PIM%NSTA, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                     PIM%MJD_0, PIM%TAI_0, PIM%MJD_0, &
     &                     PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8243, IUER, 'PIMA_LOAD_ANC', 'Error in an '// &
     &              'attempt to load the data into VTD data structure' )
                RETURN
           END IF
!
! -------- Disable automatic NERS update during the run
!
           VTD%NERS%CNF%AGE_FCS = 1.D15
           VTD%NERS%CNF%AGE_SPL = 1.D15
      END IF
!
! --- Parse telemety file in anc format
!
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_PARSE ( TELEMTR_FIL, ANC, VTD%NERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8244, IUER, 'PIMA_LOAD_ANC', 'Error in '// &
     &         'parsing telemetry file '//TELEMTR_FIL )
           RETURN 
      END IF
!
! --- Get station index that corresponds to telemetry defined in TELEMTR_FIL file
!
      IND_STA = 0
      CALL CLRCH ( STR )
      DO 410 J1=1,PIM%NSTA
         IF ( ANC%STA_NAM == PIM%C_STA(J1) ) THEN
              IND_STA = J1
         END IF
         STR(I_LEN(STR)+2:) = PIM%C_STA(J1)
 410  CONTINUE
      IF ( IND_STA == 0 ) THEN
           CALL ERR_LOG ( 8245, IUER, 'PIMA_LOAD_ANC', 'Error in '// &
     &         'attempt to load telemetry from file '// &
     &          TRIM(TELEMTR_FIL)//' for station '//ANC%STA_NAM// &
     &         ' That stations did not observe in experiment '// &
     &          TRIM(PIM%CONF%SESS_CODE)//' The list of particiating '// &
     &         'stations: '//STR )
           RETURN 
      END IF
      IF ( ANC%NUM_TSYS ==  0 ) THEN
           FL_TSYS = .FALSE.
         ELSE
           FL_TSYS = .TRUE.
      END IF
      IF ( ANC%NUM_CBL ==  0 ) THEN
           FL_CABLE = .FALSE.
           N_CAB    = 0
         ELSE
           FL_CABLE = .TRUE.
           N_CAB    = ANC%NUM_CBL
      END IF
      IF ( ANC%NUM_MET ==  0 ) THEN
           FL_MET   = .FALSE.
           N_MET    = 0
         ELSE
           FL_MET   = .TRUE.
           N_MET    = ANC%NUM_MET
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE  ( 6, '(A)' ) 'Station '//PIM%C_STA(IND_STA)// &
     &                         ' Anc_file: '//TRIM(TELEMTR_FIL)
           WRITE  ( 6, 210 ) PIM%C_STA(IND_STA), FL_TSYS, FL_CABLE, FL_MET
 210       FORMAT ( 'Station ', A, ' Found_Tsys: ', L1, &
     &              ' Found_cable: ', L1, ' Found_met: ', L1 )
           WRITE  ( 6, 220 ) PIM%C_STA(IND_STA), ANC%NUM_MET
 220       FORMAT ( 'Station ',A, ' Number of weather entries: ', I5 )
           IF ( FL_TSYS ) THEN
                WRITE  ( 6, 230 ) PIM%C_STA(IND_STA), ANC%NUM_TSYS
 230            FORMAT ( 'Station ',A, ' Number of Tsys entries:    ', I5 )
           END IF
      END IF
!
      IF ( FL_TSYS ) THEN
!
! -------- We have found Tsys in the telemtry file. 
!
           N_MIS = 0
!
! -------- Allocate dynamic memory for TSYS
!
           IF ( ASSOCIATED ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS ) ) THEN
!
! ------------- Deallocated memory for Tsys if it has been previoysly allocated
!
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS         )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8  )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4   )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4 )
           END IF
!
! -------- Allocate dynamic memorty that will hold Tsys
!
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL = .TRUE.
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOL  = PIM%NPOL
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI  = ANC%NUM_EPO_TTO
!
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(PIM%NFRQ,PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI,PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
!
! -------- ... and initialize it
!
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS         = 0.0D0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8  = 0.0D0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 = 0.0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND      = 0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4        = 0.0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4      = 0.0
!
           IND_FRG = PIM%CONF%FRQ_GRP
           DO 420 J2=1,PIM%NOBS
              IND_OBS = J2
!
! ----------- Check whether IND_STA station observed this observation and
! ----------- if yes, what was its station index in the baseline
!
              IND_STA_BAS = 0
              IF ( PIM%OBS(J2)%STA_IND(1) == IND_STA ) IND_STA_BAS = 1
              IF ( PIM%OBS(J2)%STA_IND(2) == IND_STA ) IND_STA_BAS = 2
              IF ( IND_STA_BAS == 0 ) GOTO 420
!
              IND_SCA = PIM%OBS(J2)%SCA_IND
              PIM%OBS(J2)%TSYS_IND(IND_STA_BAS,:) = 0
              OBS_DURA = PIM%OBS(J2)%TIM_END - PIM%OBS(J2)%TIM_BEG 
!
! ----------- Fill the list of source name aliases
!
! ----------- First put the B-name
!
!
              L_SOU = 1
              I_SOU(L_SOU) = PIM%OBS(J2)%SOU_IND
!
              IF ( PIM%SOU(PIM%OBS(J2)%SOU_IND)%NISO > 0 ) THEN
!
! ---------------- The check for sources in the field of view
!
                   DO 430 J3=1,PIM%SOU(PIM%OBS(J2)%SOU_IND)%NISO
                      L_SOU = L_SOU + 1
                      I_SOU(L_SOU) = PIM%SOU(PIM%OBS(J2)%SOU_IND)%ISO_IND(J3)
 430               CONTINUE 
              END IF
!
              IF ( PIM%SOU(PIM%OBS(J2)%SOU_IND)%NSYN > 0 ) THEN
!
! ---------------- Then check for synonymous
!
                   DO 440 J4=1,PIM%SOU(PIM%OBS(J2)%SOU_IND)%NSYN
                      L_SOU = L_SOU + 1
                      I_SOU(L_SOU) = PIM%SOU(PIM%OBS(J2)%SOU_IND)%SYN_IND(J4)
 440               CONTINUE 
              END IF
              IF ( PIM%SOU(PIM%OBS(J2)%SOU_IND)%IND_SWAP > 0 ) THEN
!
! ---------------- and finally, check for swap names
!
                   L_SOU = L_SOU + 1
                   I_SOU(L_SOU) = PIM%SOU(PIM%OBS(J2)%SOU_IND)%IND_SWAP
              END IF
!
! ----------- Search for the Tsys measurment for the source observed in J2-th
! ----------- observation with Tsys time tag the closest to the mid-scan epoch
!
              TIM_DIF_MIN = 1.D8
              IND_TSYS     = 0
              IND_SOU_TSYS = 0
              DO 450 J5=1,ANC%NUM_EPO_TTO
                 IND_SOU_ANC = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, ANC%TTO(J5)%SOU_NAM )
                 DO 460 J6=1,L_SOU
                    IF ( I_SOU(J6) == IND_SOU_ANC ) THEN
                         TIM_DIF = DABS ( (ANC%MJD_TTO - PIM%MJD_0)*86400.0D0 + &
     &                                    (ANC%TAI_TTO - PIM%TAI_0)           + &
     &                                    (ANC%TTO(J5)%TIM - (PIM%OBS(J2)%TIM_BEG + PIM%OBS(J2)%TIM_END)/2) )
                         IF ( TIM_DIF < TIM_DIF_MIN ) THEN
                              TIM_DIF_MIN  = TIM_DIF
                              IND_TSYS     = J5
                              IND_SOU_TSYS = IND_SOU_ANC
                         END IF
                    END IF
 460             CONTINUE 
 450          CONTINUE 
!
              IF ( TIM_DIF_MIN < MAX ( PIM__SPAN_DEF, PIM__SCL_OBS_DUR*OBS_DURA) ) THEN
!
! ---------------- It turned out Tsys measurment with index IND_TSYS of the
! ---------------- source observed in the J2-th observation satisfies 
! ---------------- the criterea that max difference in time tag is less than
! ---------------- PIM__SPAN_DEF
!
!
! ---------------- Put Tsys in all frequency groups
!
                   DO 470 J7=PIM_MIN_FRG,PIM_MAX_FRG
                      PIM%STA(IND_STA)%TSYS(J7)%TIME_SPAN_R4(IND_TSYS) = PIM__SPAN_DEF
                      PIM%STA(IND_STA)%TSYS(J7)%SOU_IND(IND_TSYS) = IND_SOU_TSYS
                      PIM%OBS(IND_OBS)%TSYS_IND(IND_STA_BAS,J7)   = IND_TSYS
                      PIM%STA(IND_STA)%TSYS(J7)%AZ_R4(IND_TSYS)   = PIM%OBS(J2)%AZ(IND_STA_BAS)
                      PIM%STA(IND_STA)%TSYS(J7)%ELEV_R4(IND_TSYS) = PIM%OBS(J2)%ELEV(IND_STA_BAS)
                      PIM%STA(IND_STA)%TSYS(J7)%TIME_MID_R8(IND_TSYS) = 0.0
!
                      DO 480 J8=1,PIM%NFRQ
!
! ---------------------- Search for the frequency slot in the telemetry data structure ANC
! ---------------------- that corresponds to the J8-th frequency. We search for that 
! ---------------------- slot that has the minimum frequency difference wrt to the J8-th
! ---------------------- frequency
!
                         IF ( PIM%NPOL == 1 ) THEN
!
! --------------------------- Single polarization case
!
                              FRQ_DIF_MIN = 1.D12
                              IND_FRQ = 0
                              DO 490 J9=1,ANC%NUM_TPS
                                 IF ( DABS(ANC%TPS(J9)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ ) < FRQ_DIF_MIN ) THEN
                                      FRQ_DIF_MIN = DABS(ANC%TPS(J9)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ )
                                      IND_FRQ = J9
                                 END IF
 490                          CONTINUE 
                              IF ( IND_FRQ == 0 ) THEN
                                   CALL CLRCH ( STR )
                                   CALL INCH  ( J8, STR )
                                   CALL ERR_LOG ( 8246, IUER, 'PIMA_LOAD_ANC', 'Cannot '// &
     &                                 'find the frequency with index '//TRIM(STR)// &
     &                                 ' at station '//PIM%C_STA(IND_STA)// &
     &                                 ' from the telemetry file '//TELEMTR_FIL )
                                   RETURN 
                              END IF
                         END IF
                         IF ( PIM%NPOL > 1 ) THEN
!
! --------------------------- Dual-pol case. We search for the fequenncy AND polarization.
!
                              FRQ_DIF_MIN = 1.D12
                              IND_FRQ = 0
                              DO 4100 J10=1,ANC%NUM_TPS
                                 IF ( PIM%STA(IND_STA)%POL_TYP(1) == ANC%TPS(J10)%POL .AND. &
     &                                DABS(ANC%TPS(J10)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ ) < FRQ_DIF_MIN ) THEN
                                      FRQ_DIF_MIN = DABS(ANC%TPS(J10)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ )
                                      IND_FRQ_1 = J10
                                 END IF
 4100                         CONTINUE 
!
                              IF ( IND_FRQ_1 == 0 ) THEN
                                   CALL CLRCH ( STR )
                                   CALL INCH  ( J8, STR )
                                   CALL ERR_LOG ( 8247, IUER, 'PIMA_LOAD_ANC', 'Cannot '// &
     &                                 'find the frequency with index '//TRIM(STR)// &
     &                                 ' polarization '// &
     &                                 PIMA__POL(PIM%STA(IND_STA)%POL_TYP(1))// &
     &                                 ' at station '//PIM%C_STA(IND_STA)// &
     &                                 ' from the telemetry file '//TELEMTR_FIL )
                                   RETURN 
                              END IF
!
! --------------------------- Now search for the second polarization and the same frequency
!
                              FRQ_DIF_MIN = 1.D12
                              IND_FRQ_2 = 0
                              DO 4110 J11=1,ANC%NUM_TPS
                                 IF ( PIM%STA(IND_STA)%POL_TYP(2) == ANC%TPS(J11)%POL .AND. &
     &                                DABS(ANC%TPS(J11)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ ) < FRQ_DIF_MIN ) THEN
                                      FRQ_DIF_MIN = DABS(ANC%TPS(J11)%SKY_FRQ - PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ )
                                      IND_FRQ_2 = J11
                                 END IF
 4110                         CONTINUE 
!
                              IF ( IND_FRQ_2 == 0 ) THEN
                                   CALL CLRCH ( STR )
                                   CALL INCH  ( J8, STR )
                                   CALL ERR_LOG ( 8248, IUER, 'PIMA_LOAD_ANC', 'Cannot '// &
     &                                 'find the frequency with index '//TRIM(STR)// &
     &                                 ' polarization '// &
     &                                 PIMA__POL(PIM%STA(IND_STA)%POL_TYP(2))// &
     &                                 ' at station '//PIM%C_STA(IND_STA)// &
     &                                 ' from the telemetry file '//TELEMTR_FIL )
                                   RETURN 
                              END IF
                         END IF
!
                         IF ( FRQ_DIF_MIN < PIM__SCL_FRQ_DIF*PIM%FRQ(1,PIM%CONF%FRQ_GRP)%BAND_WIDTH ) THEN
!
! --------------------------- We found such a frequency slot.
! --------------------------- Copy Tsys
!
                              PIM%STA(IND_STA)%TSYS(J7)%TIME_MID_R8(IND_TSYS)  = ANC%TTO(IND_TSYS)%TIM + &
     &                                    (ANC%MJD_TTO - PIM%MJD_0)*86400.0D0 + &
     &                                    (ANC%TAI_TTO - PIM%TAI_0)
                              IF ( PIM%NPOL == 1 ) THEN
                                   PIM%STA(IND_STA)%TSYS(J7)%TSYS(J8,IND_TSYS,1) = ANC%TTO(IND_TSYS)%TSYS(IND_FRQ)
                                 ELSE IF ( PIM%NPOL > 1 ) THEN
                                   PIM%STA(IND_STA)%TSYS(J7)%TSYS(J8,IND_TSYS,1) = ANC%TTO(IND_TSYS)%TSYS(IND_FRQ_1)
                                   PIM%STA(IND_STA)%TSYS(J7)%TSYS(J8,IND_TSYS,2) = ANC%TTO(IND_TSYS)%TSYS(IND_FRQ_2)
                              END IF
                         END IF
 480                  CONTINUE
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                           STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%STA(IND_STA)%TSYS(J7)%TIME_MID_R8(IND_TSYS), IER )
                           STR1 = MJDSEC_TO_DATE ( ANC%MJD_TTO, ANC%TAI_TTO + ANC%TTO(IND_TSYS)%TIM, IER )
                           IF ( PIM%NPOL == 1 ) THEN
                                WRITE ( 6, 2102 ) PIM%C_STA(IND_STA), IND_OBS, IND_SCA, &
     &                                            PIM%C_SOU(PIM%SCA(IND_SCA)%SOU_IND), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TSYS(1,IND_TSYS,1), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TSYS(2,IND_TSYS,1), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TIME_MID_R8(IND_TSYS), STR(1:19), &
     &                                            STR1(1:19), PIM%STA(IND_STA)%TSYS(J7)%SOU_IND(IND_TSYS) 
                              ELSE IF ( PIM%NPOL > 1 ) THEN
                                WRITE ( 6, 2102 ) PIM%C_STA(IND_STA), IND_OBS, IND_SCA, &
     &                                            PIM%C_SOU(PIM%SCA(IND_SCA)%SOU_IND), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TSYS(1,IND_TSYS,1), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TSYS(1,IND_TSYS,2), &
     &                                            PIM%STA(IND_STA)%TSYS(J7)%TIME_MID_R8(IND_TSYS), STR(1:19), &
     &                                            STR1(1:19), PIM%STA(IND_STA)%TSYS(J7)%SOU_IND(IND_TSYS) 
                           END IF 
 2102                      FORMAT ( 'PIMA_LOAD_ANC Sta: ', A, ' Ind_obs: ', I5, ' Ind_sca: ', I5, &
     &                              ' Sou: ', A, ' Tsys_1: ', F8.1, ' Tsys_2: ', F8.1, ' Tim= ', F8.2, ' Date: ', A, &
     &                              ' date: ', A, ' ind_sou_tsys: ', I4 )
                      END IF
 470               CONTINUE
               ELSE
!
! ---------------- Collect statistics of missing Tsys
!
                   N_MIS = N_MIS + 1
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG, -2 )
                        STR2 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_END, -2 )
                        WRITE ( 6, 250 ) PIM%C_STA(IND_STA), IND_OBS, &
     &                                   PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                   STR1(1:24), STR2(1:24)
 250                    FORMAT ( 'PIMA_LOAD_ANC: Station ',A, ' Cannot find Tsys for Obs ', I6, &
     &                           ' of source ', A, ' on [ ',A , ' , ', A , ' ]' )
                   END IF
                   GOTO 420
              END IF
 420       CONTINUE ! End of cycle over observations
!
! -------- Print Tsys statistics
!
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                IF ( N_MIS == 0 ) THEN
                     WRITE ( 6, 260 ) PIM%C_STA(IND_STA), ANC%NUM_EPO_TTO
 260                 FORMAT ( 'Station ', A, ' Tsys was found for all     ', I5, ' observations' )
                   ELSE 
                     WRITE ( 6, 270 ) PIM%C_STA(IND_STA), ANC%NUM_EPO_TTO - N_MIS, N_MIS
 270                 FORMAT ( 'Station ', A, ' Tsys is found for ', I5, &
     &                        ' obs and missing for ', I5, ' obs' )
                END IF
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, 280 ) PIM%C_STA(IND_STA), &
     &                           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL, &
     &                           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
 280            FORMAT ( 'PIMA_LOAD_ANC: Sta: ', A, ' Tsys_avail: ', L1, &
     &                   ' Num_tsys: ', I5  )
           END IF
      END IF
!
      IF ( FL_CABLE ) THEN
           IF ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_AVAIL ) THEN
                IF ( ASSOCIATED ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%TIM_CAB ) ) THEN
                     DEALLOCATE ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%TIM_CAB )
                END IF
                IF ( ASSOCIATED ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL ) ) THEN
                     DEALLOCATE ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL )
                END IF
           END IF
	   PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_AVAIL = .TRUE.
!
           PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%NPOI = N_CAB
           ALLOCATE ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%TIM_CAB(N_CAB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*N_CAB, STR )
                CALL ERR_LOG ( 8249, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%TIM_CAB '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL(N_CAB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*N_CAB, STR )
                CALL ERR_LOG ( 8250, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL '// &
     &               'object' )
                RETURN
           END IF
!
! -------- Now we need to compute robust mean cable calibration and
! -------- subtract it from cable length values.
! -------- Keep in mind, cable cal may have jmps due to the procedure
! -------- of determiing its sign. We need to compute the mean value
! -------- that is immune to these outliers.
! -------- We iteratively remove M__CAB_SHARE share of points from
! -------- mean calculation
!
           IV = 1
           N_ITER = N_CAB*M__CAB_SHARE
           DO 4120 J12=1,N_ITER
              AVR_CAB = 0.0D0
              MAX_DEV = 0.0D0
              IND_MAX_DEV = 1
!
! ----------- Compute the mean value for those elements which IV(kk) = 1
!
              NP = 0
              DO 4130 J13=1,N_CAB
                 IF ( IV(J13) == 1 ) THEN
                      AVR_CAB = AVR_CAB + ANC%CBL(J13)%DELAY
                      NP = NP + 1
                 END IF
 4130         CONTINUE
              AVR_CAB = AVR_CAB/NP
!
! ----------- Find the point with the maximum by module deviation from the
! ----------- mean among remaining points
!
              DO 4140 J14=1,N_CAB
                 IF ( DABS ( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL(J14) - AVR_CAB ) > &
     &                MAX_DEV  .AND.  IV(J14) == 1 ) THEN
!
                      MAX_DEV = DABS( PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL(J14) - AVR_CAB )
                      IND_MAX_DEV = J14
                 END IF
 4140         CONTINUE
!
! ----------- And mark it as an outlier
!
              IV(IND_MAX_DEV) = 0
 4120      CONTINUE
!
! -------- Save the mean value
!
           PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%MEAN_CABLE = AVR_CAB
           PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CABLE_SIGN = 1
!
! -------- Remove the mean cable calibration from cable lenght
!
           DO 4150 J15=1,N_CAB
              PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%TIM_CAB(J15) = ANC%CBL(J15)%TIM + &
     &                                              (ANC%MJD_CBL - PIM%MJD_0)*86400.0D0 + &
     &                                              (ANC%TAI_CBL - PIM%TAI_0)
              PIM%STA(IND_STA)%CABLE(PIM%CONF%FRQ_GRP)%CAB_DEL(J15) = ANC%CBL(J15)%DELAY - AVR_CAB
 4150      CONTINUE
      END IF
!
      IF ( FL_MET ) THEN
           IF ( PIM%STA(IND_STA)%WEATHER%AVAIL      .AND. &
     &          PIM%STA(IND_STA)%WEATHER%NPOI > 0         ) THEN
!
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_BEG )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_END )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%PRES     )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TEMP     )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%HUMID    )
           END IF
!
           PIM%STA(IND_STA)%WEATHER%AVAIL = .TRUE.
           PIM%STA(IND_STA)%WEATHER%NPOI  = N_MET
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_BEG(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8251, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_END(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8252, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%PRES(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8253, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TEMP(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8254, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%TEMP '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%HUMID(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8255, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
                     'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%HUMID '// &
     &               'object' )
                RETURN
           END IF
!
! -------- Copy meteo data
!
           DO 4160 J16=1,N_MET
              PIM%STA(IND_STA)%WEATHER%PRES(J16)     = ANC%MET(J16)%PRES 
              PIM%STA(IND_STA)%WEATHER%TEMP(J16)     = ANC%MET(J16)%TEMP
              PIM%STA(IND_STA)%WEATHER%HUMID(J16)    = ANC%MET(J16)%HUMID/100.0D0
              PIM%STA(IND_STA)%WEATHER%TIME_BEG(J16) = ANC%MET(J16)%TIM + &
     &                                                (ANC%MJD_MET - PIM%MJD_0)*86400.0D0 + &
     &                                                (ANC%TAI_MET - PIM%TAI_0) - &
     &                                                 PIM%OBS(1)%AP_LEN
              PIM%STA(IND_STA)%WEATHER%TIME_END(J16) = ANC%MET(J16)%TIM + &
     &                                                (ANC%MJD_MET - PIM%MJD_0)*86400.0D0 + &
     &                                                (ANC%TAI_MET - PIM%TAI_0) + &
     &                                                 PIM%OBS(1)%AP_LEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
                   WRITE ( 6, * ) 'J16= ', J16, ' temp/pres/humid= ', PIM%STA(IND_STA)%WEATHER%PRES(J16), PIM%STA(IND_STA)%WEATHER%TEMP(J16), PIM%STA(IND_STA)%WEATHER%HUMID(J16)
              END IF
 4160      CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_LOAD_ANC  !#!#
