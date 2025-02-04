      SUBROUTINE TSYS_TIME_FILTER_2 ( ANC, IND_FRQ, TIM_DIF_MAX, NP,    &
     &                                TIM, TSYS, EL_ARR, AZ_ARR, NS,    &
     &                                TIM_AVR, TSYS_AVR, TSYS_RMS,      &
     &                                IND_SCA, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_TIME_FILTER                                                        *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          IND_FRQ       =  Frequency index                     { INT*4 }            *
! *                                                                                    *
! *          TIM_DIF_MAX   = Time difference between scans        { REAL*8 } [s]       *
! *                                                                                    *
! *          IUER          =  Error Handler                       { INT*4, OPT }       *
! *                           If IUER=0 no error message will be printed,  even in     *
! *                           the event of an error. However, for other possible       *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will    *
! *                           print to screen. For the latter case, i.e., IUER = -3,   *
! *                           after printing the program will terminate.               *
! *                           Default, IUER = -1                                       *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *          NP            =  Filtered Number of points      { INT*4 }                 *
! *                                                                                    *
! *          TIM           =  Filtered Time array            { REAL*8 }  (NPx1)  [s]   *
! *                                                                                    *
! *          TSYS          =  Filtered Tsys array            { REAL*8 }  (NPx1)  [K]   *
! *                                                                                    *
! *          EL_ARR        =  Filtered Elevation Array       { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! *          AZ_ARR        =  Filtered Azimuth Array         { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! *          NS            =  Number of scans                { INT*4 }                 *
! *                                                                                    *
! *          TIM_AVR       =  Average Time (after filters)   { REAL*8 }  (NSx1)  [s]   *
! *                                                                                    *
! *          TSYS_AVR      =  Average Tsys (after filter)    { REAL*8 }  (NSx1)  [K]   *
! *                                                                                    *
! *          TSYS_RMS      =  RMS of Tsys (after filters     { REAL*8 }  (NSx1)  [K]   *
! *                                                                                    *
! *          IND_SCA       =  indices where scans end        { INT*4 }   (NSx1)        *
! *                                                                                    *
! * ###  17-MAY-2021   TSYS_TIME_FILTER         v3.0 (c)  N. Habana  06-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_FRQ, NP, IND_SCA(ANC__MEPC), IUER
      REAL*8     TIM(ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC)
      REAL*8     TSYS_AVR(ANC__MEPC), TSYS_RMS(ANC__MEPC)
      REAL*8     TIM_DIF_MAX, TIM_OLD, TIM_CUR
      REAL*8     TIM_OUT(ANC__MEPC), TSYS_OUT(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      INTEGER*4  I11, I12, I21, I22
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Initial Values
!
      NS = 0                    ! No. of scans
      KP = 0                    ! No. of usable points in a given scan
      NP = 0                    ! No. of filtered points
! ---
      TIM      = 0.D0
      TIM_AVR  = 0.D0
      TSYS     = 0.D0
      TSYS_AVR = 0.D0
      TSYS_RMS = 0.D0
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_TSYS

       if (j1 == anc%num_tsys) PRINT *, "%%%%%% TSYS_FILTER_2 - 80"

!
! ------ 
!
         TIM_CUR = ANC%TSYS(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = ANC%TSYS(J1)%TIM
         END IF
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN (ANC%TSYS(J1)%TIM)            .AND.       &
     &        .NOT. IS_R8_NAN (ANC%TSYS(J1)%TSYS(IND_FRQ))    ) THEN

         if (j1 == anc%num_tsys) then
            PRINT *, "%%%%%% TSYS_FILTER_2 - 96"
            PRINT *, ( ANC%TSYS(J4)%TSYS(IND_FRQ), J4=1070,1080)
         end if
          
            IF ( ANC%TSYS(J1)%TSYS(IND_FRQ) .LT. ANC__TSYS_MAX ) THEN 
                  
      if (j1 == anc%num_tsys) PRINT *, "%%%%%% TSYS_FILTER_2 - 103"

               IF ( ANC%TSYS(J1)%TSYS(IND_FRQ) .GT. ANC__TSYS_MIN ) THEN
!    
! ------------ Store values for the filtered points
!
      if (j1 == anc%num_tsys) PRINT *, "%%%%%% TSYS_FILTER_2 - 109"

               NP = NP + 1                                                 ! Number of points update
               TIM(NP)     =  ANC%TSYS(J1)%TIM
               TSYS(NP)    =  ANC%TSYS(J1)%TSYS(IND_FRQ)
               EL_ARR(NP)  =  ANC%TSYS(J1)%EL
               AZ_ARR(NP)  =  ANC%TSYS(J1)%AZ
!
! ------------ We are entering a new scan zone
!
               IF ( ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX ) .OR.         &
     &              ( J1 == ANC%NUM_TSYS )              ) THEN

       if (ns == 0) PRINT *, "%%%%%%%%%%% TSYS_FILTER_2 - 122  %%%%%%"
! ---------------
                  NS = NS + 1                 ! No. of scans update
!
! --------------- Array of indices of each end point of a new scan
! --------------- N.B: The index is against the filtered data
!
                  IND_SCA(NS) = NP
!
! --------------- Number of points in scan
!
                  IF ( NS == 1 ) THEN
                     I11 = NP
                     NUM_SCA(NS) = NP
                  ELSE
                     NUM_SCA(NS) = NP - I11
                     I11 = NP
                  END IF
               END IF
               END IF
            END IF
         END IF
! ------
         TIM_OLD = ANC%TSYS(J1)%TIM
 410  CONTINUE

        PRINT *, "%%%%%%%%%% TSYS_FILTER_2 - 149 %%%%%"
        PRINT *, NS, NP, J1, ANC%NUM_TSYS

!
! --- Compute the average and RMS of each scan
!
      IF ( NS .GE. 1 ) THEN
! ------
         DO 420 J2 = 1, NS
! ---------
            IF ( NS == 1 ) THEN
               CALL TSYS_AVER ( NUM_SCA(1), TIM(1:IND_SCA(1)),          &
     &                          TSYS(1:IND_SCA(1)), TIM_AVR(1),         &
     &                          TSYS_AVR(1), TSYS_RMS(1), KP, IUER )
            ELSE
               CALL TSYS_AVER ( NUM_SCA(NS),                            &
     &                          TIM(IND_SCA(NS-1)+1:IND_SCA(NS)),       &
     &                          TSYS(IND_SCA(NS-1)+1:IND_SCA(NS)),      &
     &                          TIM_AVR(NS), TSYS_AVR(NS), TSYS_RMS(NS),&
     &                          KP, IUER )
            END IF
 420     CONTINUE
      END IF

        PRINT *, "%%%%%%%%%% TSYS_FILTER_2 - 173 %%%%%"


! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TSYS_TIME_FILTER_2  !#!
