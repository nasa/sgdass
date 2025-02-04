      SUBROUTINE PIMA_PCAL_SCAN_AVR ( PIM, PCAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_SCAN_AVR computes scan averaged phase            *
! *   calibration phases and ampitudes as well as phase calibration      *
! *   rate of change. 
! *                                                                      *
! * ## 13-DEC-2022 PIMA_PCAL_SCAN_AVR v1.1 (c) L. Petrov 24-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INTEGER*4  IUER
      TYPE     ( PIMA__TYPE     )  :: PIM
      TYPE     ( PIM_PCAL__TYPE )  :: PCAL
      REAL*8       EPS
      PARAMETER  ( EPS = 1.0D-2 )
      REAL*8     TIM_SCA(2,PIM__MSCA), PCAL_TIM(PIM__MUV), PCAL_REA(PIM__MUV), &
     &           PCAL_IMA(PIM__MUV), PRAT_REA, PRAT_IMA, &
     &           PCAL_REA_SCA, PCAL_IMA_SCA, PCAL_REA_POI, PCAL_IMA_POI, &
     &           DR_SIG, SH_SIG, TMIN_BEG(PIM__MSCA), TMIN_END(PIM__MSCA)
      REAL*8     T8(PIM__MUV), X1(PIM__MUV), X2(PIM__MUV)
      CHARACTER  STR*128, STR1*128, STR2*128, SCA_SOU(PIM__MSCA)*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, ISTA, IND_OBS, I_SCA, NPC, &
     &           IND_SCA(PIM__MSCA), IND_POI_SCA(2,PIM__MUV),  &
     &           IMIN_BEG(PIM__MSCA), IMIN_END(PIM__MSCA), IVRB, IER
      LOGICAL*1  FL_MISSED
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, ADD_LIS
      REAL*8,    EXTERNAL :: ATAN_CS
!
      PCAL%NSCA = 0
      TIM_SCA   = 1.0001D11
      IVRB      = 0
!
! --- Build arrach of TIM_SCA with start and stop time epochs of a given
! --- scan for all the scans observed at a given station. 
! --- It also creates a cross-refernce table of scans IND_SCA from
! --- the list of scans observed by this station to the global scan list.
! --- It also creates array of source names observed in the station-depedenet
! --- scan list
!
      DO 410 J1=1,PIM%NSCA
         DO 420 J2=1,PIM%SCA(J1)%NBAS
            IND_OBS = PIM%SCA(J1)%OBS_IND(J2)
            IF ( PIM%OBS(IND_OBS)%STA_IND(1) == PCAL%ISTA .OR. & 
                 PIM%OBS(IND_OBS)%STA_IND(2) == PCAL%ISTA      ) THEN
!
                 CALL ERR_PASS ( IUER, IER )
                 I_SCA = ADD_LIS ( PIM__MSCA, PCAL%NSCA, IND_SCA, J1, IER )
                 IF ( TIM_SCA(1,I_SCA) .GE. 1.D10 ) THEN
                      TIM_SCA(1,I_SCA) = PIM%OBS(IND_OBS)%TIM_BEG
                      TIM_SCA(2,I_SCA) = PIM%OBS(IND_OBS)%TIM_END
                    ELSE
                      TIM_SCA(1,I_SCA) = MIN ( TIM_SCA(1,I_SCA), PIM%OBS(IND_OBS)%TIM_BEG )
                      TIM_SCA(2,I_SCA) = MAX ( TIM_SCA(2,I_SCA), PIM%OBS(IND_OBS)%TIM_END )
                 END IF
                 SCA_SOU(I_SCA) = PIM%C_SOU(PIM%SCA(J1)%SOU_IND)
            END IF
 420     CONTINUE 
         TMIN_BEG(J1) = 1.001D11
         TMIN_END(J1) = 1.001D11
         IMIN_BEG(J1) = 0
         IMIN_END(J1) = 0
 410  CONTINUE 
!
      IF ( SIZE(PCAL%ISCA_POI) < PIM%NSCA ) THEN
           CALL CLRCH ( STR) 
           CALL INCH  ( SIZE(PCAL%ISCA_POI), STR )
           CALL ERR_PASS ( IUER , IER )
           CALL ERR_LOG ( 8311, IER, 'PIMA_PCAL_SCAN_AVR', 'Trap of internal '// &
     &         'conntrol: station '//PIM%C_STA(PCAL%ISTA)//' has too few '// &
     &         'pcal points: '//TRIM(STR)//', nethertheless, continue' )
           PCAL%PCAL_SCA = .FALSE.
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IND_POI_SCA = 0
!
      DO 430 J3=1,PCAL%NPOI
         DO 440 J4=1,PCAL%NSCA
            IF ( PCAL%TIME_MID_R8(J3) .GE. TIM_SCA(1,J4) - EPS .AND. &
     &           PCAL%TIME_MID_R8(J3) .LE. TIM_SCA(2,J4) + EPS       ) THEN
                 IF ( IND_POI_SCA(1,J4) == 0 ) THEN
                      IND_POI_SCA(1,J4) = J3
                 END IF
                 IND_POI_SCA(2,J4) = J3
                 PCAL%ISCA_POI(J4) = J3
                 PCAL%IPOI_SCA(J3) = J4
                 IF ( IMIN_BEG(J4) == 0 ) IMIN_BEG(J4) = J3
                 IMIN_END(J4) = J3
               ELSE
                 IF ( SCA_SOU(J4) == PIM%C_SOU(PCAL%SOU_IND(J3)) ) THEN
                      IF ( PCAL%TIME_MID_R8(J3) < TIM_SCA(1,J4) ) THEN
                           IF ( TMIN_BEG(J4) > 1.D10 ) THEN
                                TMIN_BEG(J4) = PCAL%TIME_MID_R8(J3)
                                IMIN_BEG(J4) = J3
                              ELSE 
                                IF ( PCAL%TIME_MID_R8(J3) > TMIN_BEG(J4) ) THEN
                                     TMIN_BEG(J4) = PCAL%TIME_MID_R8(J3)
                                     IMIN_BEG(J4) = J3
                                END IF
                           END IF
                      END IF
                      IF ( PCAL%TIME_MID_R8(J3) > TIM_SCA(2,J4) ) THEN
                           IF ( TMIN_END(J4) > 1.D10 ) THEN
                                TMIN_END(J4) = PCAL%TIME_MID_R8(J3)
                                IMIN_END(J4) = J3
                              ELSE 
                                IF ( PCAL%TIME_MID_R8(J3) < TMIN_END(J4) ) THEN
                                     TMIN_END(J4) = PCAL%TIME_MID_R8(J3)
                                     IMIN_END(J4) = J3
                                END IF
                           END IF
                      END IF
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      DO 450 J5=1,PCAL%NSCA
         FL_MISSED = .FALSE.
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
              STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SCA(1,J5), -2 )
              STR2 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SCA(2,J5), -2 )
              WRITE ( 6, * ) 'PIMA_PCAL_SCAN_AVR: Sta: ', PIM%C_STA(PCAL%ISTA), &
     &                       ' Ista_sca: ', INT2(J5), ' Sou: ', SCA_SOU(J5), &
     &                       ' inds= ', IND_POI_SCA(1:2,J5), &
     &                       ' Tims= ', STR1(1:21), ' ', STR2(1:21)
         END IF
         IF ( IMIN_BEG(J5) == 0 ) THEN
              FL_MISSED = .TRUE.
         END IF
         IF ( .NOT. FL_MISSED                                    .AND. &
     &        ( IND_POI_SCA(1,J5) < 1 .OR. IND_POI_SCA(2,J5) < 1 )     ) THEN
!
! ----------- We did not find phase cal points within the range of this observation
!
              IF ( TIM_SCA(1,J5) - TMIN_BEG(J5) .LE. TMIN_END(J5) - TIM_SCA(2,J5) ) THEN
                   IF ( TIM_SCA(1,J5) - TMIN_BEG(J5) < MAX ( PIM%CONF%MAX_SCAN_LEN, PIM%CONF%MAX_SCAN_GAP ) ) THEN
                        DO 460 J6=1,PCAL%NPOL
                           DO 470 J7=1,PIM%NFRQ
                              DO 480 J8=1,PCAL%NO_TONES
                                 PCAL%PRAT_SCA(J8,J7,J5,J6) = 0.0
                                 PCAL%PHAS_SCA(J8,J7,J5,J6) = PCAL%PHAS(J8,J7,IMIN_BEG(J5),J6)
                                 PCAL%AMPL_SCA(J8,J7,J5,J6) = PCAL%AMPL(J8,J7,IMIN_BEG(J5),J6)
 480                          CONTINUE 
 470                       CONTINUE 
 460                    CONTINUE 
                     ELSE
                        FL_MISSED = .TRUE.
                   END IF
                 ELSE IF ( TIM_SCA(1,J5) - TMIN_BEG(J5) .LE. TMIN_END(J5) - TIM_SCA(2,J5) ) THEN
                   IF ( TMIN_END(J5) - TIM_SCA(2,J5) < MAX ( PIM%CONF%MAX_SCAN_LEN, PIM%CONF%MAX_SCAN_GAP ) ) THEN
                        DO 490 J9=1,PCAL%NPOL
                           DO 4100 J10=1,PIM%NFRQ
                              DO 4110 J11=1,PCAL%NO_TONES
                                 PCAL%PRAT_SCA(J11,J10,J5,J9) = 0.0
                                 PCAL%PHAS_SCA(J11,J10,J5,J9) = PCAL%PHAS(J11,J10,IMIN_END(J5),J9)
                                 PCAL%AMPL_SCA(J11,J10,J5,J9) = PCAL%AMPL(J11,J10,IMIN_END(J5),J9)
 4110                         CONTINUE 
 4100                      CONTINUE 
 490                    CONTINUE 
                      ELSE
                        FL_MISSED = .TRUE.
                   END IF
              END IF
!
              IF ( FL_MISSED ) THEN
                   PCAL%PCAL_SCA = .FALSE.
                 ELSE 
                   PCAL%PCAL_SCA = .TRUE.
              END IF
              IF ( FL_MISSED ) THEN
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
                        STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SCA(1,J5), -2 )
                        STR2 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SCA(2,J5), -2 )
                        WRITE ( 6, * ) 'PIMA_PCAL_SCAN_AVR Failed to find pcal for observation '// &
     &                                 ' of ', SCA_SOU(J5), ' at station ', PIM%C_STA(PCAL%ISTA), &
     &                                 ' at epochs ', SNGL(TIM_SCA(1,J5)), SNGL(TIM_SCA(2,J5)), &
     &                                 ' [ '//STR1(1:21)//' , '//STR2(1:21)//' ] ', &
     &                                 ' TMIN_BEG_DIFF = ', SNGL(TIM_SCA(1,J5) - TMIN_BEG(J5)), &
     &                                 ' TMIN_END_DIFF = ', SNGL(TMIN_END(J5) - TIM_SCA(2,J5)), &
     &                                 ' IMIN_BEG= ', INT2(IMIN_BEG(J5)), &
     &                                 ' IMIN_END= ', INT2(IMIN_END(J5))
                   END IF
              END IF
         END IF
!
         DO 4120 J12=1,PCAL%NPOL
            DO 4130 J13=1,PIM%NFRQ
               DO 4140 J14=1,PCAL%NO_TONES
                  IF ( IND_POI_SCA(2,J5) - IND_POI_SCA(1,J5) > 1 ) THEN
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
                            WRITE ( 6, * ) 'PIMA_PCAL_SCAN_AVR Found ', IND_POI_SCA(2,J5) - IND_POI_SCA(1,J5) +  1, &
     &                             ' pcal epochs for observatios of ', SCA_SOU(J5), ' at station ', &
     &                            ' PIM%C_STA(PCAL%ISTA), at epochs ', &
     &                            SNGL(TIM_SCA(1,J5)), SNGL(TIM_SCA(2,J5))
                       END IF
                       NPC = 0
                       DO 4150 J15=IND_POI_SCA(1,J5),IND_POI_SCA(2,J5)
                          NPC = NPC + 1
                          PCAL_TIM(NPC) = PCAL%TIME_MID_R8(J15)
                          PCAL_REA(NPC) = PCAL%AMPL(J14,J13,J15,J12)*COS(PCAL%PHAS(J14,J13,J15,J12))
                          PCAL_IMA(NPC) = PCAL%AMPL(J14,J13,J15,J12)*SIN(PCAL%PHAS(J14,J13,J15,J12))
 4150                  CONTINUE 
!
                       CALL ERR_PASS   ( IUER, IER )
                       CALL RGR8_NOWEI ( NPC, PCAL_TIM, PCAL_REA, PCAL%TIME_SCA_R8(J5), &
     &                                   PRAT_REA, PCAL_REA_SCA, DR_SIG, SH_SIG, IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL CLRCH ( STR ) 
                            CALL INCH  ( PCAL%IPOI_SCA(J5), STR )
                            CALL ERR_LOG ( 8313, IUER, 'PIMA_PCAL_SCAN_AVR', 'Error in '// &
     &                          'computing linear regression for real part of phase '// &
     &                          'calibration for station '//PIM%C_STA(PCAL%ISTA)// &
     &                          ' scan '//TRIM(STR)//' with name '// &
     &                          PIM%SCA(PCAL%IPOI_SCA(J5))%SCAN_NAME )
                            RETURN 
                       END IF
!
                       CALL ERR_PASS   ( IUER, IER )
                       CALL RGR8_NOWEI ( NPC, PCAL_TIM, PCAL_IMA, PCAL%TIME_SCA_R8(J5), &
     &                                   PRAT_IMA, PCAL_IMA_SCA, DR_SIG, SH_SIG, IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL CLRCH ( STR ) 
                            CALL INCH  ( PCAL%IPOI_SCA(J5), STR )
                            CALL ERR_LOG ( 8314, IUER, 'PIMA_PCAL_SCAN_AVR', 'Error in '// &
     &                          'computing linear regression for real part of phase '// &
     &                          'calibration for station '//PIM%C_STA(PCAL%ISTA)// &
     &                          ' scan '//TRIM(STR)//' with name '// &
     &                          PIM%SCA(PCAL%IPOI_SCA(J5))%SCAN_NAME )
                            RETURN 
                       END IF
!
                       PCAL%PRAT_SCA(J14,J13,J5,J12) = ATAN_CS ( PRAT_REA, PRAT_IMA )
                       PCAL%PHAS_SCA(J14,J13,J5,J12) = ATAN_CS ( PCAL_REA_SCA, PCAL_IMA_SCA )
                       PCAL%AMPL_SCA(J14,J13,J5,J12) = DSQRT   ( PCAL_REA_SCA**2 + PCAL_IMA_SCA**2 )
                       NPC = 0
                       DO 4160 J16=IND_POI_SCA(1,J5),IND_POI_SCA(2,J5)
                          NPC = NPC + 1
                          PCAL_TIM(NPC) = PCAL%TIME_MID_R8(J16)
                          PCAL_REA_POI  = PCAL_REA_SCA + PRAT_REA*(PCAL_TIM(NPC) - PCAL%TIME_SCA_R8(J5))
                          PCAL_IMA_POI  = PCAL_IMA_SCA + PRAT_IMA*(PCAL_TIM(NPC) - PCAL%TIME_SCA_R8(J5))
                          PCAL%PHAS_RGR(J14,J13,J16,J12) = ATAN_CS ( PCAL_REA_POI,     PCAL_IMA_POI )
                          PCAL%AMPL_RGR(J14,J13,J16,J12) = DSQRT   ( PCAL_REA_POI**2 + PCAL_IMA_POI**2 )
!
                          IF ( IVRB == 2 ) THEN
                               T8(NPC) = PCAL_TIM(NPC)
                               X1(NPC) = PCAL%PHAS(J14,J13,J16,J12)
                               X2(NPC) = PCAL%PHAS_RGR(J14,J13,J16,J12)
                          END IF
 4160                  CONTINUE 
                       IF ( IVRB == 1 ) THEN
                            IF ( J13 == 4 .AND. J14 == 4 ) THEN
                                 WRITE ( 6, * ) 'PIMA+_PCAL_SCAN_AVR  J12/J13/J14= ', INT2(J12), INT2(J13), INT2(J14), 'pcal_phas_sca= ', PCAL%PHAS_SCA(J14,J13,J5,J12)
                            END IF
                       END IF 
                     ELSE IF ( IND_POI_SCA(2,J5) - IND_POI_SCA(1,J5) == 1 ) THEN
                       PCAL%TIME_SCA_R8(J5) = ( PCAL%TIME_MID_R8(IND_POI_SCA(1,J5)) + &
     &                                          PCAL%TIME_MID_R8(IND_POI_SCA(2,J5)) )/2.0D0
                       PCAL_REA_SCA = ( PCAL%AMPL(J14,J13,IND_POI_SCA(1,J5),J12)*COS(PCAL%PHAS(J14,J13,IND_POI_SCA(1,J5),J12)) + &
     &                                  PCAL%AMPL(J14,J13,IND_POI_SCA(2,J5),J12)*COS(PCAL%PHAS(J14,J13,IND_POI_SCA(2,J5),J12))   )/2.0
                       PCAL_IMA_SCA = ( PCAL%AMPL(J14,J13,IND_POI_SCA(1,J5),J12)*SIN(PCAL%PHAS(J14,J13,IND_POI_SCA(1,J5),J12)) + &
     &                                  PCAL%AMPL(J14,J13,IND_POI_SCA(2,J5),J12)*SIN(PCAL%PHAS(J14,J13,IND_POI_SCA(2,J5),J12))   )/2.0
                       IF ( PCAL%TIME_MID_R8(IND_POI_SCA(2,J5)) - PCAL%TIME_MID_R8(IND_POI_SCA(1,J5)) > PIMA__MIN_AP_LEN ) THEN
                            PCAL%PRAT_SCA(J14,J13,J5,J12) = ATAN_CS ( PCAL_REA_SCA, PCAL_REA_SCA )/ &
     &                                                              (PCAL%TIME_MID_R8(IND_POI_SCA(2,J5)) - PCAL%TIME_MID_R8(IND_POI_SCA(1,J5)))
                          ELSE
                            PCAL%PRAT_SCA(J14,J13,J5,J12) = 0.0D0
                       END IF 
                       PCAL%PHAS_SCA(J14,J13,J5,J12) = ATAN_CS ( PCAL_REA_SCA,     PCAL_IMA_SCA )
                       PCAL%AMPL_SCA(J14,J13,J5,J12) = DSQRT   ( PCAL_REA_SCA**2 + PCAL_IMA_SCA**2 )
!                       
                       PCAL%PHAS_RGR(J14,J13,IND_POI_SCA(1,J5),J12) = PCAL%PHAS_SCA(J14,J13,J5,J12) + &
     &                                                                PCAL%PRAT_SCA(J14,J13,J5,J12)* &
     &                                                                (PCAL%TIME_MID_R8(IND_POI_SCA(1,J5)) - PCAL%TIME_SCA_R8(J5))
                       PCAL%PHAS_RGR(J14,J13,IND_POI_SCA(2,J5),J12) = PCAL%PHAS_SCA(J14,J13,J5,J12) + &
     &                                                                PCAL%PRAT_SCA(J14,J13,J5,J12)* &
     &                                                                (PCAL%TIME_MID_R8(IND_POI_SCA(2,J5)) - PCAL%TIME_SCA_R8(J5))
                       PCAL%AMPL_RGR(J14,J13,IND_POI_SCA(1,J5),J12) = PCAL%AMPL_SCA(J14,J13,J5,J12)
                       PCAL%AMPL_RGR(J14,J13,IND_POI_SCA(2,J5),J12) = PCAL%AMPL_SCA(J14,J13,J5,J12)
                     ELSE IF ( IND_POI_SCA(1,J5) > 0 .AND. IND_POI_SCA(2,J5) == IND_POI_SCA(1,J5) ) THEN
                       PCAL%PRAT_SCA(J14,J13,J5,J12) = 0.0
                       PCAL%PHAS_SCA(J14,J13,J5,J12) = PCAL%PHAS(J14,J13,IND_POI_SCA(1,J5),J12)
                       PCAL%AMPL_SCA(J14,J13,J5,J12) = PCAL%AMPL(J14,J13,IND_POI_SCA(1,J5),J12)
!
                       PCAL%PHAS_RGR(J14,J13,IND_POI_SCA(1,J5),J12) = PCAL%PHAS_SCA(J14,J13,J5,J12) 
                       PCAL%AMPL_RGR(J14,J13,IND_POI_SCA(1,J5),J12) = PCAL%AMPL_SCA(J14,J13,J5,J12) 
                     ELSE
                       PCAL%PRAT_SCA(J14,J13,J5,J12) = 0.0
                       PCAL%PHAS_SCA(J14,J13,J5,J12) = 0.0
                       PCAL%AMPL_SCA(J14,J13,J5,J12) = 0.0
                   END IF
!
                   IF ( IVRB == 2 ) THEN
                        IF ( J13 == 3 .AND. J14 == 3 ) THEN   
                              WRITE ( 6, * ) 'PPSA-140 c_sta ',  PIM%C_STA(PCAL%ISTA), ' isca= ', INT2(J5), &
     &                                      ' IPOL=  ', INT2(J12), ' IFRQ= ', INT2(J13), ' ITON= ', INT2(J13), &
     &                                      ' tim= ', PCAL%TIME_SCA_R8(J5) 
                             CALL DIAGI_2 ( NPC, T8, X1, NPC, T8, X2, IER )
                        END IF
                   END IF
 4140           CONTINUE 
 4130        CONTINUE 
 4120    CONTINUE 
 450  CONTINUE 
!
      PCAL%PCAL_SCA = .TRUE.
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_SCAN_AVR  !#!#
