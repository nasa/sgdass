      SUBROUTINE PIMA_WEA_INTRP ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_MKDB
! *                                                                      *
! * ### 02-JUL-2009  PIMA_WEA_INTRP  v1.2 (c) L. Petrov  28-JUN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      REAL*8     TIM_STA_BEG(PIM__MSTA), TIM_STA_END(PIM__MSTA)
      LOGICAL*4  FL_BEG_UPD, FL_END_UPD
      CHARACTER  STR*128
      INTEGER*4  N__EPN
      REAL*8     TIM__MAR, TIM_STEP_MIN
      PARAMETER  ( TIM__MAR = 240.0D0 ) ! Time margin for interpolation
      PARAMETER  ( TIM_STEP_MIN = 5.0 ) ! Miniumim time step in seconds
      PARAMETER  ( N__EPN   = 4 ) ! The number of extra ponts added for &
     &                            ! dealing the end problem
      REAL*8     PRES_RATE__MAX, TEMP_RATE__MAX, HUMID_RATE__MAX
      PARAMETER  ( PRES_RATE__MAX  = 0.035D0   ) ! 30 mbar/day
      PARAMETER  ( TEMP_RATE__MAX  = 0.00035D0 ) ! 30 deg/day
      PARAMETER  ( HUMID_RATE__MAX = 1.157D-5  ) ! 1.0/day
      REAL*8,    ALLOCATABLE :: TIM(:), TMP(:), &
     &                          PRES_VAL(:), TEMP_VAL(:), HUMID_VAL(:), &
     &                          PRES_SPL(:), TEMP_SPL(:), HUMID_SPL(:)
      REAL*8     TIM_0, TIM_STEP, PRES_0, TEMP_0, HUMID_0, PRES_RATE, &
     &           TEMP_RATE, HUMID_RATE, TIM_MID_OBS, DR_SIG, SH_SIG, D1, DN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           NP, NS, N_SH, IP, KP, KP0, KP_NEW, FRG_IND, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
      REAL*8,    EXTERNAL :: FSPL8
!
      DO 410 J1=1,PIM%NSTA
         TIM_STA_BEG(J1) =  1.0D10
         TIM_STA_END(J1) = -1.0D10
 410  CONTINUE 
!
      DO 420 J2=1,PIM%NOBS
         FRG_IND = PIM%OBS(J2)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
         IF ( FRG_IND < 1  .OR. FRG_IND > PIM%OBS(J2)%NUVS ) THEN
              GOTO 420
         END IF
         DO 430 J3=1,2
            TIM_STA_BEG(PIM%OBS(J2)%STA_IND(J3)) = &
     &            MIN ( TIM_STA_BEG(PIM%OBS(J2)%STA_IND(J3)), &
     &                  PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J2)%UV_IND(1,FRG_IND))%TIM_IND) )
            TIM_STA_END(PIM%OBS(J2)%STA_IND(J3)) = &
     &            MAX ( TIM_STA_END(PIM%OBS(J2)%STA_IND(J3)), &
     &                  PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J2)%UV_IND(PIM%OBS(J2)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND) )
 430     CONTINUE 
 420  CONTINUE 
!
      DO 440 J4=1,PIM%NSTA
         IF ( PIM%STA(J4)%WEATHER%NPOI .LE. 0 ) GOTO 440
         FL_BEG_UPD = .FALSE.
         FL_END_UPD = .FALSE.
         NP = PIM%STA(J4)%WEATHER%NPOI 
         IF ( PIM%STA(J4)%WEATHER%TIME_BEG(1) > (TIM_STA_BEG(J4) - TIM__MAR/2.0D0) ) THEN
              FL_BEG_UPD = .TRUE.
              NP = NP + N__EPN
         END IF
         IF ( PIM%STA(J4)%WEATHER%TIME_END(PIM%STA(J4)%WEATHER%NPOI) < &
     &        (TIM_STA_END(J4) + TIM__MAR/2.0D0) ) THEN
              FL_END_UPD = .TRUE.
              NP = NP + N__EPN
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) 'B1:  sta: ', pim%sta(j4)%ivs_name, ' fl= ', fl_end_upd, &
!     &           ' tim_sta_end: ', tim_sta_end(j4), ' np= ', np, &
!     &           ' wp = ', pim%sta(j4)%weather%time_end(pim%sta(j4)%weather%npoi) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
         ALLOCATE ( TIM(NP),       STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7851, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array TIM' )
              RETURN 
         END IF
!
         ALLOCATE ( TMP(NP),       STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7852, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array TMP' )
              RETURN 
         END IF
!
         ALLOCATE ( PRES_VAL(NP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7853, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array PRES_VAL' )
              RETURN 
         END IF
!
         ALLOCATE ( TEMP_VAL(NP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7854, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array TEMP_VAL' )
              RETURN 
         END IF
!
         ALLOCATE ( HUMID_VAL(NP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7855, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array HUMID_VAL' )
              RETURN 
         END IF
!
         ALLOCATE ( PRES_SPL(NP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7856, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array PRES_SPL' )
              RETURN 
         END IF
!
         ALLOCATE ( TEMP_SPL(NP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7857, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array TEMP_SPL' )
              RETURN 
         END IF
!
         ALLOCATE ( HUMID_SPL(NP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7858, IUER, 'PIMA_WEA_INTRP', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for array HUMID_SPL' )
              RETURN 
         END IF
!
         KP = 0
         IF ( FL_BEG_UPD ) THEN
              IF ( PIM%STA(J4)%WEATHER%NPOI .GE. 2*N__EPN  ) THEN
!
! ---------------- Compute the average pressure and its rate of change
! ---------------- near the beginning of the intraval
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG, &
     &                               PIM%STA(J4)%WEATHER%PRES, &
     &                               TIM_0, PRES_RATE, PRES_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7859, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for PRES at the beginning of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
!
! ---------------- ... but we restrict the rate of change if it seems to bee
! ---------------- too big
!
                   IF ( PRES_RATE >  PRES_RATE__MAX ) PRES_RATE =  PRES_RATE__MAX 
                   IF ( PRES_RATE < -PRES_RATE__MAX ) PRES_RATE = -PRES_RATE__MAX 
!
! ---------------- The same for air temperature
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG, &
     &                               PIM%STA(J4)%WEATHER%TEMP, &
     &                               TIM_0, TEMP_RATE, TEMP_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7860, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for TEMP at the beginning of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
                   IF ( TEMP_RATE >  TEMP_RATE__MAX ) TEMP_RATE =  TEMP_RATE__MAX 
                   IF ( TEMP_RATE < -TEMP_RATE__MAX ) TEMP_RATE = -TEMP_RATE__MAX 
!
! ---------------- The same for humidity
!
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG, &
     &                               PIM%STA(J4)%WEATHER%HUMID, &
     &                               TIM_0, HUMID_RATE, HUMID_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7861, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for HUMID at the beginning of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
                   IF ( HUMID_RATE >  HUMID_RATE__MAX ) HUMID_RATE =  HUMID_RATE__MAX 
                   IF ( HUMID_RATE < -HUMID_RATE__MAX ) HUMID_RATE = -HUMID_RATE__MAX 
                 ELSE 
!
! ---------------- Too few points. No regression. Take the first point
!
                   TIM_0 = PIM%STA(J4)%WEATHER%TIME_BEG(1)
                   PRES_0    = PIM%STA(J4)%WEATHER%PRES(1)
                   TEMP_0    = PIM%STA(J4)%WEATHER%TEMP(1)
                   HUMID_0   = PIM%STA(J4)%WEATHER%HUMID(1)
                   PRES_RATE = 0.0D0
                   TEMP_RATE = 0.0D0
                   HUMID_RATE = 0.0D0
              END IF
!
              TIM_STEP = ( PIM%STA(J4)%WEATHER%TIME_BEG(1) &
     &                     - ( TIM_STA_BEG(J4) - TIM__MAR ) &
     &                   )/N__EPN
             DO 450 J5=1,N__EPN
                 KP = KP + 1
                 TIM(KP)   = (TIM_STA_BEG(J4) - TIM__MAR) + TIM_STEP*(J5-1)
                 PRES_VAL(KP)  = PRES_0  + PRES_RATE*(TIM(KP)-TIM_0)
                 TEMP_VAL(KP)  = TEMP_0  + TEMP_RATE*(TIM(KP)-TIM_0)
                 HUMID_VAL(KP) = HUMID_0 + HUMID_RATE*(TIM(KP)-TIM_0)
 450          CONTINUE 
         END IF
!
         KP0 = KP+1
         DO 460 J6=1,PIM%STA(J4)%WEATHER%NPOI
            KP = KP + 1
            TIM(KP) = PIM%STA(J4)%WEATHER%TIME_BEG(J6) 
            IF ( J6 == PIM%STA(J4)%WEATHER%NPOI ) THEN
                 TIM(KP) = PIM%STA(J4)%WEATHER%TIME_END(J6) 
            END IF
            PRES_VAL(KP)  = PIM%STA(J4)%WEATHER%PRES(J6)
            TEMP_VAL(KP)  = PIM%STA(J4)%WEATHER%TEMP(J6)
            HUMID_VAL(KP) = PIM%STA(J4)%WEATHER%HUMID(J6)
 460     CONTINUE 
!
! ------ Sorting all arrays in time order
!
         CALL SORT84 ( KP-KP0+1, TIM(KP0), PRES_VAL(KP0), TEMP_VAL(KP0), &
     &                 HUMID_VAL(KP0) ) 
         KP_NEW = KP
!
! ------ Eliminate points with too short time intervals.
! ------ This may happen if atmosphere records were inserted in 
! ------ initial files more than once
!
         DO 470 J7=KP0+1,KP
            IF ( J7 > KP_NEW ) GOTO 470
            IF ( TIM(J7) - TIM(J7-1) < TIM_STEP_MIN ) THEN
                 IF ( J7 < KP_NEW ) THEN
                      N_SH = 0
                      DO 480 J8=J7+1,KP_NEW
                         N_SH = N_SH + 1
                         IF ( TIM(J8) - TIM(J7-1) > TIM_STEP_MIN ) THEN
                              GOTO 880
                         END IF
 480                  CONTINUE 
 880                  CONTINUE 
                      KP_NEW = KP_NEW - N_SH
                      DO 490 J9=J7,KP_NEW
                         TIM(J9) = TIM(J9+N_SH)
                         PRES_VAL(J9)  = PRES_VAL(J9+N_SH)
                         TEMP_VAL(J9)  = TEMP_VAL(J9+N_SH)
                         HUMID_VAL(J9) = HUMID_VAL(J9+N_SH)
 490                  CONTINUE 
                   ELSE 
                      KP_NEW = KP_NEW - 1
                 END IF
            END IF
            IF ( KP_NEW > 1 ) THEN
                 IF ( TIM(KP_NEW) - TIM(KP_NEW-1) < TIM_STEP_MIN ) THEN
                      KP_NEW = KP_NEW - 1
                 END IF
            END IF  
 470     CONTINUE 
         KP = KP_NEW
!
         IF ( FL_END_UPD ) THEN
              IF ( PIM%STA(J4)%WEATHER%NPOI .GE. 2*N__EPN  ) THEN
!
! ---------------- Compute the average pressure and its rate of change
! ---------------- near the end of the interval
!
                   NS = PIM%STA(J4)%WEATHER%NPOI + 1 - 2*N__EPN
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG(NS), &
     &                               PIM%STA(J4)%WEATHER%PRES(NS), &
     &                               TIM_0, PRES_RATE, PRES_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7862, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for PRES at the end of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
!
! ---------------- ... but we restrict the rate of change if it seems to bee
! ---------------- too big
!
                   IF ( PRES_RATE >  PRES_RATE__MAX ) PRES_RATE =  PRES_RATE__MAX 
                   IF ( PRES_RATE < -PRES_RATE__MAX ) PRES_RATE = -PRES_RATE__MAX 
!
! ---------------- The same for air temperature
!
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG(NS), &
     &                               PIM%STA(J4)%WEATHER%TEMP(NS), &
     &                               TIM_0, TEMP_RATE, TEMP_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7863, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for TEMP at the end of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
                   IF ( TEMP_RATE >  TEMP_RATE__MAX ) TEMP_RATE =  TEMP_RATE__MAX 
                   IF ( TEMP_RATE < -TEMP_RATE__MAX ) TEMP_RATE = -TEMP_RATE__MAX 
!
! ---------------- The same for humidity
!
                   CALL RGR8_NOWEI ( 2*N__EPN, &
     &                               PIM%STA(J4)%WEATHER%TIME_BEG(NS), &
     &                               PIM%STA(J4)%WEATHER%HUMID(NS), &
     &                               TIM_0, HUMID_RATE, HUMID_0,&
     &                               DR_SIG, SH_SIG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7864, IUER, 'PIMA_WEA_INTRP', &
     &                      'Error in an attempt to compute linear '// &
     &                      'regression for HUMID at the end of the '// &
     &                      'interval' )
                        RETURN 
                   END IF
                   IF ( HUMID_RATE >  HUMID_RATE__MAX ) HUMID_RATE =  HUMID_RATE__MAX 
                   IF ( HUMID_RATE < -HUMID_RATE__MAX ) HUMID_RATE = -HUMID_RATE__MAX 
                 ELSE 
!
! ---------------- Too few points. No regression. Take the last point
!
                   TIM_0 = PIM%STA(J4)%WEATHER%TIME_BEG(PIM%STA(J4)%WEATHER%NPOI)
                   PRES_0    = PIM%STA(J4)%WEATHER%PRES(PIM%STA(J4)%WEATHER%NPOI)
                   TEMP_0    = PIM%STA(J4)%WEATHER%TEMP(PIM%STA(J4)%WEATHER%NPOI)
                   HUMID_0   = PIM%STA(J4)%WEATHER%HUMID(PIM%STA(J4)%WEATHER%NPOI)
                   PRES_RATE = 0.0D0
                   TEMP_RATE = 0.0D0
                   HUMID_RATE = 0.0D0
              END IF
!
              TIM_STEP = ( TIM_STA_END(J4) + TIM__MAR - &
     &                     PIM%STA(J4)%WEATHER%TIME_END(PIM%STA(J4)%WEATHER%NPOI) &
     &                   )/(N__EPN-1)
!              
              DO 4100 J10=1,N__EPN
                 KP = KP + 1
                 TIM(KP) = PIM%STA(J4)%WEATHER%TIME_END(PIM%STA(J4)%WEATHER%NPOI) + &
     &                     TIM_STEP*J10
                 PRES_VAL(KP)  = PRES_0  + PRES_RATE*(TIM(KP)-TIM_0)
                 TEMP_VAL(KP)  = TEMP_0  + TEMP_RATE*(TIM(KP)-TIM_0)
                 HUMID_VAL(KP) = HUMID_0 + HUMID_RATE*(TIM(KP)-TIM_0)
 4100         CONTINUE 
         END IF
!
! ------ Compute coefficients of the interpolating spline for 
! ------ atmospheric pressure
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        kp = KP - 4
! if ( pim%sta(j4)%ivs_name == 'BR-VLBA ' ) then
!  write ( 6, * ) ' sta: ', pim%sta(j4)%ivs_name, ' kp= ', kp, ' np = ', pim%sta(j4)%weather%npoi
! write( 6, * ) ' t2= ', temp_val(kp), ' temp_last_was: ', pim%sta(j4)%weather%temp(pim%sta(j4)%weather%npoi), &  ! %%%%%%%
!!  &            ' tim(kp) = ', tim(kp), &
!  &   ' timbeg= ', pim%sta(j4)%weather%time_beg(pim%sta(j4)%weather%npoi), & ! %%%%%%
!  &   ' timend= ', pim%sta(j4)%weather%time_end(pim%sta(j4)%weather%npoi) ! %%%%%%
! do 520 j1=1,kp
!    write ( 6, * ) ' j1= ', j1, ' tim = ', tim(j1), ' pre = ', pres_val(j1), ' temp = ', temp_val(j1) ! %%%
! 520  continue 
! endif
!  call exit ( 11 )  ! %%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         CALL SORT83 ( KP, TIM, PRES_VAL, TEMP_VAL, HUMID_VAL )
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, KP, TIM, PRES_VAL, D1, DN, PRES_SPL, TMP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7865, IUER, 'PIMA_WEA_INTRP', 'Failure in '// &
     &            'an attempt to compute interpolating spline for PRES '// &
     &            'for station '//PIM%C_STA(J4) )
              RETURN 
         END IF
!
! ------ Compute coefficients of the interpolating spline for 
! ------ air temperature
!
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, KP, TIM, TEMP_VAL, D1, DN, TEMP_SPL, TMP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7866, IUER, 'PIMA_WEA_INTRP', 'Failure in '// &
     &            'an attempt to compute interpolating spline for TEMP '// &
     &            'for station '//PIM%C_STA(J4) )
              RETURN 
         END IF
!
! ------ Compute coefficients of the interpolating spline for 
! ------ air humidity
!
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, KP, TIM, HUMID_VAL, D1, DN, HUMID_SPL, TMP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7867, IUER, 'PIMA_WEA_INTRP', 'Failure in '// &
     &            'an attempt to compute interpolating spline for HUMID '// &
     &            'for station '//PIM%C_STA(J4) )
              RETURN 
         END IF
!
! ------ Now scan all observations and find those which were made at 
! ------ the J4-th station
!
         DO 4110 J11=1,PIM%NOBS
            FRG_IND = PIM%OBS(J11)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
            IF ( FRG_IND < 1  .OR. FRG_IND > PIM%OBS(J11)%NUVS ) THEN
                 GOTO 4110
            END IF
            TIM_MID_OBS = ( PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J11)%UV_IND(1,FRG_IND))%TIM_IND) + &
     &                      PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J11)%UV_IND(PIM%OBS(J11)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND) )/2.0D0
            IF ( PIM%OBS(J11)%STA_IND(1) == J4  .OR.  &
     &           PIM%OBS(J11)%STA_IND(2) == J4        ) THEN
!
! -------------- find the pivotal element...
!
                 IP = IXMN8 ( KP, TIM, TIM_MID_OBS )
            END IF
!
! --------- ... and interpolate using spline coefficients 
!
            IF ( PIM%OBS(J11)%STA_IND(1) == J4  ) THEN
                 PIM%OBS(J11)%PRES(1) = FSPL8 ( TIM_MID_OBS, KP, TIM, PRES_VAL,  IP, PRES_SPL  )
                 PIM%OBS(J11)%TEMP(1) = FSPL8 ( TIM_MID_OBS, KP, TIM, TEMP_VAL,  IP, TEMP_SPL  )
                 PIM%OBS(J11)%HUMID(1)= FSPL8 ( TIM_MID_OBS, KP, TIM, HUMID_VAL, IP, HUMID_SPL )
            END IF
            IF ( PIM%OBS(J11)%STA_IND(2) == J4  ) THEN
                 PIM%OBS(J11)%PRES(2) = FSPL8 ( TIM_MID_OBS, KP, TIM, PRES_VAL,  IP, PRES_SPL  )
                 PIM%OBS(J11)%TEMP(2) = FSPL8 ( TIM_MID_OBS, KP, TIM, TEMP_VAL,  IP, TEMP_SPL  )
                 PIM%OBS(J11)%HUMID(2)= FSPL8 ( TIM_MID_OBS, KP, TIM, HUMID_VAL, IP, HUMID_SPL )
            END IF
  4110     CONTINUE 
!
         DEALLOCATE ( HUMID_SPL )
         DEALLOCATE ( TEMP_SPL  )
         DEALLOCATE ( PRES_SPL  )
         DEALLOCATE ( HUMID_VAL )
         DEALLOCATE ( TEMP_VAL  )
         DEALLOCATE ( PRES_VAL  )
         DEALLOCATE ( TMP       )
         DEALLOCATE ( TIM       )
 440  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_WEA_INTRP  !#!  
