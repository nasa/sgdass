      SUBROUTINE TSYS_TIME_FILTER ( ANC, IND_FRQ, TIM_DIF_MAX, &
     &                              NP, TIM, TSYS, EL_ARR, AZ_ARR, &
     &                              NS, TIM_AVR,   TSYS_AVR, TSYS_RMS, &
     &                              LP, IND_ARR,    &
     &                              IUER )
! ************************************************************************************
! *                                                                                  *
! *   Routine  TSYS_TIME_FILTER                                                      *
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
! *          TSYS          =  Filtered Tsys array            { REAL }  (LPx1)  [K]   *
! *                                                                                  *
! *          EL_ARR        =  Filtered Elevation Array       { REAL }  (NPx1)  [rad] *
! *                                                                                  *
! *          AZ_ARR        =  Filtered Azimuth Array         { REAL }  (NPx1)  [rad] *
! *                                                                                  *
! *          LP            =  Number of filtered points used { INT }                 *
! *                                                                                  *
! *          NS            =  Number of scans                { INT }                 *
! *                                                                                  *
! *          TIM_AVR       =  Average Time (after filters)   { REAL }  (NSx1)  [s]   *
! *                                                                                  *
! *          TSYS_AVR      =  Average Tsys (after filter)    { REAL }  (NSx1)  [K]   *
! *                                                                                  *
! *          TSYS_RMS      =  RMS of Tsys (after filters     { REAL }  (NSx1)  [K]   *
! *                                                                                  *
! *          IND_ARR       =  indices where scans end        { INT }   (NSx1)        *
! *                                                                                  *
! * ###  17-MAY-2021   TSYS_TIME_FILTER       v2.0 (c)  N. Habana  25-AUG-2021   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_FRQ, NP, LP, IND_ARR(ANC__MEPC), IUER
      REAL*8     TIM(ANC__MEPC), TSYS(ANC__MEPC), TSYS_SCA(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC), TIM_SCA( ANC__MEPC)
      REAL*8     TSYS_AVR(ANC__MEPC), TSYS_RMS(ANC__MEPC)
      REAL*8     TIM_DIF_MAX, TIM_OLD, TIM_CUR
      REAL*8     TIM_OUT(ANC__MEPC), TSYS_OUT(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP
      INTEGER*4  J1, J2, J3, J4
      INTEGER*4  I11, I12, I21, I22
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$ --- Count the number of scans and the indices where they are found          !$$
!$$                                                                             !$$
!$$       IUER = -1                                                             !$$
!$$       CALL ATP_SCANS ( ANC, NS, TIM_DIF_MAX, IND_ARR, IUER )                !$$
!$$      IF ( IUER .NE. 0 ) THEN                                                !$$
!$$         CALL ERR_LOG ( 1708, IUER, 'TSYS_TIME_FILTER',                 &    !$$
!$$     &           'Failure in separating scans ' )                            !$$
!$$         RETURN                                                              !$$
!$$      END IF                                                                 !$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --- Initial Values
!
      NS = 0                    ! No. of scans
      KP = 0                    ! No. of usable points in a given scan
      NP = 0                    ! No. of filtered points
      LP = 1                    ! No. of filtered points used. 
! ---
      TIM      = 0.D0
      TIM_SCA  = 0.D0
      TIM_AVR  = 0.D0
      TSYS     = 0.D0
      TSYS_SCA = 0.D0
      TSYS_AVR = 0.D0
      TSYS_RMS = 0.D0
!
! --- Loop through all the time slots to return the number of points
! 
      DO 410 J1 = 1, ANC%NUM_TSYS
! ------
         TIM_CUR = ANC%TSYS(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = ANC%TSYS(J1)%TIM
         END IF
!
! ------ Eliminate NaN values in the arrays
!
         IF ( .NOT. IS_R8_NAN ( ANC%TSYS(J1)%TIM  )     .AND.           &
     &        .NOT. IS_R8_NAN ( ANC%TSYS(J1)%TSYS(IND_FRQ) ) ) THEN
!
! --------- Store values for the total filtered points
!
            NP = NP + 1                                                 ! Number of points update
            TIM(NP)     =  ANC%TSYS(J1)%TIM
            TSYS(NP)    =  ANC%TSYS(J1)%TSYS(IND_FRQ)
            EL_ARR(NP)  =  ANC%TSYS(J1)%EL
            AZ_ARR(NP)  =  ANC%TSYS(J1)%AZ
!
! --------- Store values for the next time we hit a scan
!
            KP = KP + 1
            TIM_SCA(KP)  = TIM(NP)
            TSYS_SCA(KP) = TSYS(NP)
!
! --------- Reject any values beyond threshold
!
            IF ( ANC%TSYS(J1)%TSYS(IND_FRQ) > ANC__TSYS_MAX  .OR.       &
     &           ANC%TSYS(J1)%TSYS(IND_FRQ) < ANC__TSYS_MIN  ) THEN
        if ( tsys_sca(kp) .gt. 0.0 ) then
!@@!        PRINT *, "%%%%%%%% TSYS_FILTER 126 %%%%%"
    !@@!    PRINT *, "j1", J1, "ns", ns, "kp", kp, "tsys", TSYS_SCA(KP)
        end if
               NP = NP - 1
               KP = KP - 1
            END IF
         END IF
  if ( j1 .ne. -777 ) goto 410 ! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------ We are entering a new scan zone
!
         IF ( ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX ) .OR.       &
     &        ( J1 == ANC%NUM_TSYS)                            ) THEN
! ---------
            NS = NS + 1                 ! No. of scans update
!
! --------- Compute the average and RMS values for the scan
!           if it has any points in it.
!
            IF ( KP > 1 ) THEN
               CALL TSYS_AVER ( KP, TIM_SCA(1:KP), TSYS_SCA(1:KP),      &
     &                          TIM_AVR(NS), TSYS_AVR(NS),              &
     &                          TSYS_RMS(NS), IP, IUER )
               TIM_OUT(LP:LP+IP-1)  = TIM_SCA(1:IP)
               TSYS_OUT(LP:LP+IP-1) = TSYS(1:IP)
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
               TSYS_AVR(NS) = 0.0D0
               TSYS_RMS(NS) = 0.0D0
            END IF
!
! --------- Reset Scan Counter
!
            KP = 0
         END IF
! ------
         TIM_OLD = ANC%TSYS(J1)%TIM
 410  CONTINUE
!
      LP = LP - 1
      TIM(1:LP)  = TIM_OUT(1:LP) 
      TSYS(1:LP) = TSYS_OUT(1:LP) 
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TSYS_TIME_FILTER  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TSYS_TIME_FILTER_FRQ ( NP, TIM_DIF_MAX, TIM, TSYS,     &
     &                                  LP, NS, TIM_AVR, TSYS_AVR,      &
     &                                  TSYS_RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TSYS_TIME_FILTER_FRQ
! *                                                                      *
! * ### 17-MAY-2021 TSYS_TIME_FILTER v1.0 (c) L. Petrov  17-MAY-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'atp.i'
      INTEGER*4  NP, LP, IUER
      REAL*8     TIM(NP), TSYS(NP), TIM_DIF_MAX
      REAL*8     TIM_AVR(NP), TSYS_AVR(NP), TSYS_RMS(NP)
      REAL*8     TIM_OLD, TIM_OUT(NP), TSYS_OUT(NP)
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
               CALL TSYS_AVER ( KE-KB+1, TIM(KB), TSYS(KB), TIM_AVR(NS), &
     &                          TSYS_AVR(NS), TSYS_RMS(NS), IP, IUER )
               TIM_OUT(LP:LP+IP-1)  = TIM(KB:KB+IP-1)
               TSYS_OUT(LP:LP+IP-1) = TSYS(KB:KB+IP-1)
               LP = LP + IP
               KB = J1 
            END IF 
         END IF 
         TIM_OLD = TIM(J1)
 410  CONTINUE 
!
      LP = LP - 1
      TIM(1:LP)  = TIM_OUT(1:LP) 
      TSYS(1:LP) = TSYS_OUT(1:LP) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TSYS_TIME_FILTER_FRQ  !#!#!
