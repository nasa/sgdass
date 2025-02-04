      SUBROUTINE PCAL_TIME_FILTER ( ANC, IND_FRQ, TIM_DIF_MAX, NP, TIM, &
     &                              PCAL, NS, TIM_AVR, PCAL_AVR,        &
     &                              PCAL_AMP_RMS, PCAL_PHA_RMS, LP,     &
     &                              IND_ARR, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  PCAL_TIME_FILTER                                                      *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          IND_FRQ       =  Frequency index                     { INT }            *
! *                                                                                  *
! *          TIM_DIF_MAX   = Time difference between scans        { REAL } [s]       *
! *                                                                                  *
! *          IUER          =  Error Handler                       { INT, OPT }       *
! *                           If IUER=0 no error message will be printed,  even in   *
! *                           the event of an error. However, for other possible     *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will  *
! *                           print to screen. For the latter case, i.e., IUER = -3, *
! *                           after printing the program will terminate.             *
! *                           Default, IUER = -1                                     *
! *                                                                                  *
! *   OUTPUT:                                                                        *
! *          NP            =  Filtered Number of points      { INT }                 *
! *                                                                                  *
! *          TIM           =  Filtered Time array            { REAL }  (LPx1)  [s]   *
! *                                                                                  *
! *          PCAL          =  Filtered Pcal array            { CMPLX } (LPx1)  []    *
! *                                                                                  *
! *          LP            =  Filtered points used           { INT }                 *
! *                                                                                  *
! *          NS            =  Number of scans                { INT }                 *
! *                                                                                  *
! *          TIM_AVR       =  Average Time (after filters)   { REAL }  (NSx1)  [s]   *
! *                                                                                  *
! *          PCAL_AVR      =  Average Tsys (after filter)    { CMPLX } (NSx1)  []    *
! *                                                                                  *
! *          PCAL_RMS      =  RMS of Tsys (after filters     { CMPLX } (NSx1)  []    *
! *                                                                                  *
! *          IND_ARR       =  indices where scans end        { INT }   (NSx1)        *
! *                                                                                  *
! * ###  27-SEP-2022   PCAL_TIME_FILTER       v1.0 (c)  N. Habana  27-SEP-2022   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IND_FRQ, NP, LP, IND_ARR(ANC__MEPC), IUER
      REAL*8     TIM_DIF_MAX, TIM(ANC__MEPC)
      COMPLEX*8  PCAL(ANC__MEPC), PCAL_SCA(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC), TIM_SCA( ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MEPC), PCAL_PHA_RMS(ANC__MEPC)
      REAL*8     TIM_OLD, TIM_CUR
      REAL*8     TIM_OUT(ANC__MEPC)
      COMPLEX*8  PCAL_OUT(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, KF
      INTEGER*4  J1, J2, J3, J4
      INTEGER*4  I11, I12, I21, I22
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN

!
! --- Initial Values
!
      NS = 0                    ! No. of scans
      KP = 0                    ! No. of usable points in a given scan
      NP = 0                    ! No. of filtered points
      LP = 1                    ! No. of filtered points used. 
      KF = 0                    ! Number of points that were filtered out for that scan
! ---
      TIM      = 0.D0
      TIM_SCA  = 0.D0
      TIM_AVR  = 0.D0
      PCAL     = CMPLX(0.D0)
      PCAL_SCA = CMPLX(0.D0)
      PCAL_AVR = CMPLX(0.D0)
      PCAL_AMP_RMS = 0.D0
      PCAL_PHA_RMS = 0.D0
!
! --- Loop through all the time slots to return the number of points
! 
      DO 410 J1 = 1, ANC%NUM_PCAL
! ------
         TIM_CUR = ANC%PCAL(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = ANC%PCAL(1)%TIM
         END IF
!
! ------ Eliminate NaN values in the arrays
!
         IF ( .NOT. IS_R8_NAN ( ANC%PCAL(J1)%TIM  ) ) THEN
!
! --------- Store values for the total filtered points
!
            NP = NP + 1                                                 ! Number of points update
            TIM(NP)     =  ANC%PCAL(J1)%TIM
            PCAL(NP)    =  ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)
!
! --------- Store values for the next time we hit a scan
!
            KP = KP + 1
            TIM_SCA(KP)  = TIM(NP)
            PCAL_SCA(KP) = PCAL(NP)
!
! --------- Reject any values beyond threshold
! --------- Compare only the amplitude
!
            IF ( ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ))>ANC__AMP_MAX .OR. &
     &           ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ))<ANC__AMP_MIN) THEN
               NP = NP - 1
               KP = KP - 1
               KF = KF + 1
            END IF
         END IF
!
! ------ We are entering a new scan zone
!
         IF ( ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX ) .OR.       &
     &        ( J1 == ANC%NUM_PCAL )                        ) THEN
! ---------
            NS = NS + 1                 ! No. of scans update
!
! --------- Compute the average and RMS values for the scan
!           if it has any points in it.
!
            IF ( KP > 1 ) THEN
               CALL PCAL_AVER ( KP, TIM_SCA(1:KP), PCAL_SCA(1:KP),      &
     &                          TIM_AVR(NS), PCAL_AVR(NS),              &
     &                          PCAL_AMP_RMS(NS), PCAL_PHA_RMS(NS),     &
     &                          IP, IUER )
               TIM_OUT(LP:LP+IP-1)  = TIM_SCA(1:IP)
               PCAL_OUT(LP:LP+IP-1) = PCAL(1:IP)
               LP = LP + IP
!
! ------------ Array of indices of each end point of a new scan
!
               IF ( J1 > 1 ) THEN
                  IND_ARR(NS) = J1-1
               ELSE
                  IND_ARR(NS) = J1
               END IF
            ELSE
               TIM_AVR(NS)  = 0.0D0
               PCAL_AVR(NS) = CMPLX(0.D0)
               PCAL_AMP_RMS(NS) = 0.D0
               PCAL_PHA_RMS(NS) = 0.D0
            END IF
!
! --------- Reset Scan Counter
!
            KP = 0
            KF = 0
         END IF
! ------
         TIM_OLD = ANC%PCAL(J1)%TIM
 410  CONTINUE
!
      LP = LP - 1
      TIM(1:LP)  = TIM_OUT(1:LP) 
      PCAL(1:LP) = PCAL_OUT(1:LP) 
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PCAL_TIME_FILTER  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PCAL_FRQ_FILTER ( NP, TIM_DIF_MAX, TIM, PCAL, LP, NS,  &
     &                             TIM_AVR, PCAL_AVR, PCAL_AMP_RMS,     &
     &                             PCAL_PHA_RMS, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine  PCAL_FRQ_FILTER
! *                                                                      *
! * ### 27-SEP-2021 PCAL_FRQ_FILTER v1.0 (c) N. Habana  27-SEP-2021 ### *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INCLUDE   'atp.i'
      INTEGER*4  NP, LP, IUER
      REAL*8     TIM(NP), TIM_AVR(NP), TIM_DIF_MAX
      COMPLEX*8  PCAL(NP), PCAL_AVR(NP), PCAL_OUT(NP)
      REAL*8     PCAL_AMP_RMS(NP), PCAL_PHA_RMS(NP),TIM_OLD, TIM_OUT(NP)
      INTEGER*4  J1, J2, NS, KB, KE, IP
!
      NS = 0
      LP = 1
      DO 410 J1=1,NP
         IF ( J1 == 1 ) THEN
            TIM_OLD = TIM(1)
            KB = 1
         END IF
         IF ( TIM(J1) - TIM_OLD > TIM_DIF_MAX .OR. J1 == NP ) THEN
            IF ( J1 == NP ) THEN
               KE = NP
            ELSE
               KE = J1 - 1
            END IF
            NS = NS + 1
            IP = 0
            IF ( KE-KB+1 > 2 ) THEN
               CALL PCAL_AVER ( KE-KB+1, TIM(KB), PCAL(KB),             &
     &                          TIM_AVR(NS), PCAL_AVR(NS),              &
     &                          PCAL_AMP_RMS(NS), PCAL_PHA_RMS(NS),     &
     &                          IP, IUER )
               TIM_OUT(LP:LP+IP-1)  = TIM(KB:KB+IP-1)
               PCAL_OUT(LP:LP+IP-1) = PCAL(KB:KB+IP-1)
               LP = LP + IP
               KB = J1
            END IF 
         END IF 
         TIM_OLD = TIM(J1)
 410  CONTINUE 
!
      LP = LP - 1
      TIM(1:LP)  = TIM_OUT(1:LP) 
      PCAL(1:LP) = PCAL_OUT(1:LP) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PCAL_FRQ_FILTER  !#!#!
