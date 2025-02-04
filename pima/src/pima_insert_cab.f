      SUBROUTINE PIMA_INSERT_CAB ( PIM, L_STA_CAB, C_STA_CAB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_INSERT_CAB 
! *                                                                      *
! * ### 04-AUG-2009  PIMA_INSERT_CAB v3.2 (c) L. Petrov  28-APR-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  L_STA_CAB, IUER
      CHARACTER  C_STA_CAB(PIM__MSTA)*8
      CHARACTER  STR*32
      REAL*8     TIM_PCAL_END_PREV,  SGN
      REAL*8     SPL_COE(PIM__MEPC), TMP_ARR(PIM__MEPC), &
     &           TIM_CAB(PIM__MEPC), CAB_DEL(PIM__MEPC)
      REAL*8     EPS, CAB_THR_SPIKE, CAB_THR_SMOOTH
      INTEGER*4  CAB_NEP_SPIKE, CAB_SIGN
      PARAMETER  ( EPS = 1.D-6 )
      LOGICAL*1  FL_USE(PIM__MEPC), FL_NOINTRP
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, KSP, NSP, &
     &           IS, IP, NPOI, KPOI, IND_STA, IER
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: ADD_CLIST, IXMN8, ILEN, I_LEN
      REAL*8,    EXTERNAL :: FSPL8 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      CALL GETENVAR ( 'PIMAVAR_CAB_THR_SPIKE', STR )
      IF ( ILEN(STR) == 0 ) THEN
           CAB_THR_SPIKE = PIMA__CAB_THR_SPIKE  
         ELSE
           READ ( UNIT=STR, FMT='(F15.5)' ) CAB_THR_SPIKE  
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_CAB_THR_SMOOTH', STR )
      IF ( ILEN(STR) == 0 ) THEN
           CAB_THR_SMOOTH = PIMA__CAB_THR_SMOOTH 
         ELSE
           READ ( UNIT=STR, FMT='(F15.5)' ) CAB_THR_SMOOTH 
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_CAB_NEP_SPIKE', STR )
      IF ( ILEN(STR) == 0 ) THEN
           CAB_NEP_SPIKE = PIMA__CAB_NEP_SPIKE
         ELSE
           READ ( UNIT=STR, FMT='(I6)' ) CAB_NEP_SPIKE
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_CAB_SIGN', STR )
      IF ( ILEN(STR) == 0 ) THEN
           CAB_SIGN = 1
         ELSE
           READ ( UNIT=STR, FMT='(I2)' ) CAB_SIGN
      END IF
!
      FL_NOINTRP = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_NO_INTRP', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_NOINTRP = .TRUE.
      ENDIF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE  ( 6, 110 ) CAB_THR_SPIKE, CAB_THR_SMOOTH, CAB_NEP_SPIKE, CAB_SIGN
 110       FORMAT ( 'PIMA_INSERT_CAB  CAB_THR_SPIKE: ', 1PD12.5, &
     &              ' CAB_THR_SMOOTH: ', 1PD12.5, ' CAB_NEP_SPIKE: ', I6, &
     &              ' CAB_SIGN = ', I2 )
      END IF
!
      L_STA_CAB = 0
      NSP = 0
      DO 410 J1=1,PIM%NSTA
         IF ( PIM%STA(J1)%CABLE%NPOI < 3 ) GOTO 410
         NPOI = PIM%STA(J1)%CABLE%NPOI
         DO 420 J2=1,PIM%STA(J1)%CABLE%NPOI
            IF ( IS_R8_NAN ( PIM%STA(J1)%CABLE%CAB_DEL(J2) ) ) PIM%STA(J1)%CABLE%CAB_DEL(J2) = 0.0D0
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                 WRITE ( 6, 120 ) PIM%STA(J1)%IVS_NAME, J2, &
     &                            PIM%STA(J1)%CABLE%NPOI,   &
     &                            PIM%STA(J1)%CABLE%TIM_CAB(J2), &
     &                            PIM%STA(J1)%CABLE%CAB_DEL(J2)
 120             FORMAT ( 'PIMA_INSERT_CAB Sta: ', A, ' Poi: ', I5, ' Npoi: ', I5, &
     &                    ' Tim= ', F8.2, ' Cab= ', 1PD12.5 )
            END IF          
!
            IF ( J2 > 1 .AND. NPOI == PIM%STA(J1)%CABLE%NPOI ) THEN
                 IF ( ( PIM%STA(J1)%CABLE%TIM_CAB(J2) - PIM%STA(J1)%CABLE%TIM_CAB(J2-1) ) < EPS ) THEN
                      NPOI = J2-1
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                            WRITE ( 6, 130 ) PIM%STA(J1)%IVS_NAME, NPOI, PIM%STA(J1)%CABLE%NPOI, &
     &                                                                   PIM%STA(J1)%CABLE%TIM_CAB(NPOI)
 130                        FORMAT ( 'Truncated cable calibratrion for station ', A, &
     &                               ' to ', I5, ' points of ', I5, ' at epoch ', F8.2 )
                      END IF
                 END IF
            END IF
 420     CONTINUE 
         PIM%STA(J1)%CABLE%NPOI = NPOI 
!
         FL_USE = .TRUE.
         KSP = 0
         IF ( NPOI > PIMA__CAB_NEP_SPIKE + 3 ) THEN
!
! ----------- Elimination of spikes in cable calibration
!
! ----------- Cycles over the number of epochs of the spike
!
              DO 430 J3=1,CAB_NEP_SPIKE
!
! -------------- Cycle over points
!
                 DO 440 J4=2,NPOI-J3
!
! ----------------- Check whether the difference before and after a spike is witin a limit
!
                    IF ( DABS(PIM%STA(J1)%CABLE%CAB_DEL(J4+J3) - PIM%STA(J1)%CABLE%CAB_DEL(J4-1)) < CAB_THR_SMOOTH ) THEN
!
! ---------------------- Cycle over the points in the spike test interval 
!
                         DO 450 J5=J4,J4+J3-1
                            IF ( DABS(PIM%STA(J1)%CABLE%CAB_DEL(J5) - PIM%STA(J1)%CABLE%CAB_DEL(J4-1)) > CAB_THR_SPIKE .AND. &
     &                           FL_USE(J5)  ) THEN
!
! ------------------------------ The J5-th point is identified as a spike: it differs by 
! ------------------------------ more than CAB_THR_SPIKE within the spike test interval,
! ------------------------------ while CABLE_CAL at the borders of the test intefval
! ------------------------------ differes by no more than CAB_THR_SMOOTH
!
                                 DO 460 J6=J4,J4+J3-1
                                    IF ( FL_USE(J6) ) THEN
                                         FL_USE(J6) = .FALSE.
                                         KSP = KSP + 1
                                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                                              WRITE ( 6, 140 ) PIM%STA(J1)%IVS_NAME, J6, PIM%STA(J1)%CABLE%CAB_DEL(J6)
 140                                          FORMAT ( 'PIMA_INSERT_CAB ',A, ' Poi: ' I5, ' Eliminated a spike ', 1PD12.5 )
                                         END IF
                                    END IF
 460                             CONTINUE 
                            END IF
 450                     CONTINUE 
                    END IF
 440             CONTINUE 
 430          CONTINUE 
         END IF
!
         NSP = NSP + KSP
         KPOI = 0
         DO 470 J7=1,NPOI
            IF ( FL_USE(J7) ) THEN
                 KPOI = KPOI + 1
                 TIM_CAB(KPOI) = PIM%STA(J1)%CABLE%TIM_CAB(J7)
                 CAB_DEL(KPOI) = PIM%STA(J1)%CABLE%CAB_DEL(J7)
            END IF
 470     CONTINUE 
         NPOI = KPOI
!
         IF ( .NOT. FL_NOINTRP ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, NPOI, TIM_CAB, CAB_DEL, &
     &                           0.0D0, 0.0D0, SPL_COE, TMP_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE (6, * ) 'N_cab: ', PIM%STA(J1)%CABLE%NPOI
!!                   WRITE (6, * ) 'tim_cab: ', pim%sta(j1)%cable%tim_cab ! %%%%
                   CALL ERR_PASS( IUER, IER )
                   CALL ERR_LOG ( 8271, IER, 'PIMA_INSERT_CAB', &
     &                 'Failure in an attempt to compute coefficients '// &
     &                 'of B-spline that interpolates cable calibration '// &
     &                 'for station '//PIM%C_STA(J1) )
!@                   RETURN 
              END IF
         END IF
!
         DO 480 J8=1,PIM%NOBS 
!
! --------- Initialization
!
            IF ( IS_R8_NAN( PIM%OBS(J8)%CABLE(1)) ) PIM%OBS(J8)%CABLE(1) = 0.0D0
            IF ( PIM%OBS(J8)%CABLE(1) > CABLE_DEL_MAX ) PIM%OBS(J8)%CABLE(1) = 0.0D0
            IF ( PIM%OBS(J8)%CABLE(1) < CABLE_DEL_MIN ) PIM%OBS(J8)%CABLE(1) = 0.0D0
!
            IF ( IS_R8_NAN( PIM%OBS(J8)%CABLE(2)) ) PIM%OBS(J8)%CABLE(2) = 0.0D0
            IF ( PIM%OBS(J8)%CABLE(2) > CABLE_DEL_MAX ) PIM%OBS(J8)%CABLE(2) = 0.0D0
            IF ( PIM%OBS(J8)%CABLE(2) < CABLE_DEL_MIN ) PIM%OBS(J8)%CABLE(2) = 0.0D0
!
            IF ( PIM%OBS(J8)%STA_IND(1) == J1 ) THEN
                 IP = IXMN8 ( NPOI, TIM_CAB, PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1) )
                 IF ( IP == -1 ) THEN
                      PIM%OBS(J8)%CABLE(1) = CAB_DEL(1)
                    ELSE IF ( IP == -2 ) THEN
                      PIM%OBS(J8)%CABLE(1) = CAB_DEL(NPOI)
                    ELSE 
                      IF ( .NOT. FL_NOINTRP ) THEN
                           PIM%OBS(J8)%CABLE(1) = FSPL8 ( PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1), &
                                                          NPOI, TIM_CAB, CAB_DEL, IP, SPL_COE )
                         ELSE
                           PIM%OBS(J8)%CABLE(1) = CAB_DEL(IP)
                      END IF
                 END IF
                 IF ( IS_R8_NAN ( PIM%OBS(J8)%CABLE(1) ) ) PIM%OBS(J8)%CABLE(1) = 0.0D0
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1), -2 )
                      WRITE ( 6, 150 ) J8, STR(1:23), 1, PIM%C_STA(PIM%OBS(J8)%STA_IND(1)), PIM%OBS(J8)%CABLE(1), IP, NPOI
 150                  FORMAT ( 'PIMA_INSERT_CAB Ind_obs: ', I5, ' Tai: ', A, 1X, I1, 1X, A, &
     &                         ' Cab: ', 1PD12.4, ' IP= ', I5, ' Npoi= ', I5 )
                 END IF
              ELSE IF ( PIM%OBS(J8)%STA_IND(2) == J1 ) THEN
                 IP = IXMN8 ( NPOI, TIM_CAB, PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1) )
                 IF ( IP == -1 ) THEN
                      PIM%OBS(J8)%CABLE(2) = CAB_DEL(1)
                    ELSE IF ( IP == -2 ) THEN
                      PIM%OBS(J8)%CABLE(2) = CAB_DEL(NPOI)
                    ELSE 
                      IF ( .NOT. FL_NOINTRP ) THEN
                           PIM%OBS(J8)%CABLE(2) = FSPL8 ( PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1), &
                                                          NPOI, TIM_CAB, CAB_DEL, IP, SPL_COE )
                         ELSE
                           PIM%OBS(J8)%CABLE(2) = CAB_DEL(IP)
                      END IF
                 END IF
                 IF ( IS_R8_NAN ( PIM%OBS(J8)%CABLE(2) ) ) PIM%OBS(J8)%CABLE(2) = 0.0D0
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(J8)%TIM_BEG + PIM%OBS(J8)%FRT_OFFSET(1), -2 )
                      WRITE ( 6, 150 ) J8, STR(1:23), 2, PIM%C_STA(PIM%OBS(J8)%STA_IND(2)), PIM%OBS(J8)%CABLE(2), IP, NPOI
                 END IF
            END IF
            IF ( IS_R8_NAN ( PIM%OBS(J8)%CABLE(1) ) ) PIM%OBS(J8)%CABLE(1) = 0.0D0
            IF ( IS_R8_NAN ( PIM%OBS(J8)%CABLE(2) ) ) PIM%OBS(J8)%CABLE(2) = 0.0D0
 480     CONTINUE 
!
! ------ Put this station to the list of stations with cable 
! ------ calibrations available
!
         IS = ADD_CLIST ( PIM__MSTA, L_STA_CAB, C_STA_CAB, &
     &                    PIM%C_STA(J1), IER )
 410  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IF ( NSP > 0 ) THEN
                WRITE ( 6, 160 ) NSP
 160            FORMAT ( 'PIMA_INSERT_CAB  In total, ', I6, ' spikes have been eliminated' )
              ELSE
                WRITE ( 6, 170 )
 170            FORMAT ( 'PIMA_INSERT_CAB  No spikes have been eliminated' )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_INSERT_CAB  !#!  
