      SUBROUTINE TPI_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, TIM, TPION,    &
     &                                 TPIOFF, EL_ARR, AZ_ARR,          &
     &                                 SOU_ARR, SCA_ARR, IUER )
! **************************************************************************************
! *                                                                                    *
! *   Routine  TPI_TIME_FILTER_RAW                                                     *
! *   N.B: - This routine does not remove outliers/spikes, those are removed when      *
! *          averaging                                                                 *
! *        - For now we assume that TPIZERO is always -99. When we have an update,     *
! *          we will fix it.                                                           *
! *        - We also assume that if TPION is observed, then so was TPIOFF. Ergo we     *
! *          don't check the latter.                                                   *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          IND_FRQ       =  Frequency index                     { INT*4 }            *
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
! *          TPION         =  Filtered TPION array           { INT*4 }   (NPx1)  [K]   *
! *                                                                                    *
! *          TPIOFF        =  Filtered Tpioff array          { INT*4 }   (NPx1)  [K]   *
! *                                                                                    *
! *          EL_ARR        =  Filtered Elevation Array       { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! *          AZ_ARR        =  Filtered Azimuth Array         { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! *          SOU_ARR       =  Filtered Source Array          { CHAR }    (NPx1)        *
! *                                                                                    *
! *          SCA_ARR       =  Filtered Scan Array            { CHAR }    (NPx1)        *
! *                                                                                    *
! *   Copyright (c) 1975-2025 United States Government as represented by               *
! *   the Administrator of the National Aeronautics and Space                          *
! *   Administration. All Rights Reserved.                                             *
! *   License: NASA Open Source Software Agreement (NOSA).                             *
! *                                                                                    *
! * ###  10-SEP-2025   TPI_TIME_FILTER_RAW      v1.0 (d)  N. Habana  10-SEP-2025   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_FRQ, NP, IUER
      REAL*8     TIM(ANC__MEPC)
      INTEGER*4  TPION(ANC__MEPC), TPIOFF(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      CHARACTER  SOU_ARR(ANC__MEPC)*8, SCA_ARR(ANC__MEPC)*10
      INTEGER*4  ITPI_CNT
!     
! --- Check format
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         ITPI_CNT = ANC%NUM_TPI
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         ITPI_CNT = ANC%NUM_EPO_TPI
      ELSE
         CALL ERR_LOG ( 2508, IUER, 'TPI_TIME_FILTER_RAW',             &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
! --- Initial Values
!
      NP = 0                    ! No. of filtered points
      TIM      = 0.D0
      TPION    = 0
      TPIOFF   = 0
      EL_ARR   = 0.D0
      AZ_ARR   = 0.D0
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ITPI_CNT
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( ( .NOT. IS_R8_NAN ( ANC%TPI(J1)%TIM ) )     .AND.         &
     &        ( ANC%TPI(J1)%TPION(IND_FRQ) .GE. ANC__FILLER_I4) ) THEN
!
! --------- Store values for the filtered points
!
            NP          =  NP + 1         ! Number of points update
            TIM(NP)     =  ANC%TPI(J1)%TIM
            TPION(NP)   =  ANC%TPI(J1)%TPION(IND_FRQ)
            TPIOFF(NP)  =  ANC%TPI(J1)%TPIOFF(IND_FRQ)
            EL_ARR(NP)  =  ANC%TPI(J1)%EL
            AZ_ARR(NP)  =  ANC%TPI(J1)%AZ
            SOU_ARR(NP) =  ANC%TPI(J1)%SOU_NAM
            SCA_ARR(NP) =  ANC%TPI(J1)%SCA_NAM
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE TPI_TIME_FILTER_RAW !#!1
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TPI_TIME_FILTER_SCAN ( ANC, IND_FRQ, DT_MAX, NP,       &
     &                                  MP_SCA, MS, NS, TIM, TPION,     &
     &                                  TPIOFF, TIM_AVR, TPION_AVR,     &
     &                                  TPION_RMS, TPIOFF_AVR,          &
     &                                  TPIOFF_RMS, IND_SCA, IND_ARR,   &
     &                                  NP_SCA, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TPI_TIME_FILTER_SCAN                                                    *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file  { DERIVED TYPE }       *
! *                                                                                    *
! *          IND_FRQ       =  Frequency index                   { INT*4 }              *
! *                                                                                    *
! *          DT_MAX        =  Time difference between scans     { REAL*8 } [s]         *
! *                                                                                    *
! *          NP            =  No. of filtered points            { INT*4 }              *
! *                                                                                    *
! *          MP_SCA        =  Max. no. of pts per scan          { INT*4 }              *
! *                                                                                    *
! *          MS            =  Max. No. of scans                 { INT*4 }              *
! *                                                                                    *
! *          TIM           =  Filtered Time array               { REAL*8 } (NPx1) [s]  *
! *                                                                                    *
! *          TPION         =  Filtered TPION array              { INT*4 }  (NPx1) [K]  *
! *                                                                                    *
! *          TPIOFF        =  Filtered Tpioff array             { INT*4 }  (NPx1) [K]  *
! *                                                                                    *
! *          IND_ARR       =  scan end index (on derived type)  { INT*4 }   (MSx1)     *
! *                                                                                    *
! *          IUER          =  Error Handler                     { INT*4, OPT }         *
! *                           If IUER=0 no error message will be printed,  even in     *
! *                           the event of an error. However, for other possible       *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will    *
! *                           print to screen. For the latter case, i.e., IUER = -3,   *
! *                           after printing the program will terminate.               *
! *                           Default, IUER = -1                                       *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *          NS            =  Number of scans                { INT*4 }                 *
! *                                                                                    *
! *          TIM_AVR       =  Average Time (after filters)   { REAL*8 }  (NSx1)  [s]   *
! *                                                                                    *
! *          TPION_AVR     =  Average TpiOn (after filter)   { INT*4 }   (NSx1)  [K]   *
! *                                                                                    *
! *          TPION_RMS     =  RMS of TpiOn (after filters    { INT*4 }   (NSx1)  [K]   *
! *                                                                                    *
! *          TPIOFF_AVR    =  Average TpiOff (after filter)  { INT*4 }   (NSx1)  [K]   *
! *                                                                                    *
! *          TPIOFF_RMS    =  RMS of TpiOff (after filters   { INT*4 }   (NSx1)  [K]   *
! *                                                                                    *
! *          IND_SCA       =  indices where scans end        { INT*4 }   (NSx1)        *
! *                                                                                    *
! *          NP_SCA        =  No. pts in filtered scans      { INT*4 }   (NSx1)        *
! *                                                                                    *
! *   Copyright (c) 1975-2025 United States Government as represented by               *
! *   the Administrator of the National Aeronautics and Space                          *
! *   Administration. All Rights Reserved.                                             *
! *   License: NASA Open Source Software Agreement (NOSA).                             *
! *                                                                                    *
! * ###  10-SEP-2025   TPI_TIME_FILTER_SCAN    v1.0 (d)  N. Habana  10-SEP-2025   ###  *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IND_FRQ, NP, IND_SCA(ANC__MEPC), IUER
      REAL*8     TIM(ANC__MEPC), DT_MAX
      INTEGER*4  TPION(ANC__MEPC), TPIOFF(ANC__MEPC)
      INTEGER*4  TPION_AVR(ANC__MEPC), TPIOFF_AVR(ANC__MEPC)
      INTEGER*4  TPION_RMS(ANC__MEPC), TPIOFF_RMS(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NP_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4, IND_ARR(ANC__MEPC)
      INTEGER*4  I11, I12, I21, I22
      INTEGER*4  MS, MP_SCA(MS)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      REAL*8     RTPION_AVR(ANC__MEPC), RTPIOFF_AVR(ANC__MEPC)
      REAL*8     RTPION_RMS(ANC__MEPC), RTPIOFF_RMS(ANC__MEPC)
!     
! --- Get the filtered scan indices
!
      IUER = -1
      CALL ATP_TPI_SCANS ( ANC, IND_FRQ, TIM, TPION, TPIOFF, MS, NP,    &
     &                     MP_SCA, IND_ARR, NS, IND_SCA, NP_SCA, IUER )
!     
! --- Loop through the scan and get averages vs RMS
!
      TIM_AVR      =  0.D0
      TPION_AVR    =  0
      TPION_RMS    =  0
      TPIOFF_AVR   =  0
      TPIOFF_RMS   =  0
      IF ( NS .GE. 1 ) THEN
         DO 420 J2 = 1, MS !NS
            IF ( NP_SCA(J2) > 0 ) THEN
               IF ( J2 == 1 ) THEN
                  CALL TPI_AVER ( NP_SCA(1), TIM(1:IND_SCA(1)),         &
     &                            TPION(1:IND_SCA(1)),                  &
     &                            TPIOFF(1:IND_SCA(1)), TIM_AVR(1),     &
     &                            TPION_AVR(1), TPIOFF_AVR(1),          &
     &                            TPION_RMS(1), TPIOFF_RMS(1),          &
     &                            KP, IUER )
               ELSE
                  CALL TPI_AVER ( NP_SCA(J2),                           &
     &                            TIM(IND_SCA(J2-1)+1:IND_SCA(J2)),     &
     &                            TPION(IND_SCA(J2-1)+1:IND_SCA(J2)),   &
     &                            TPIOFF(IND_SCA(J2-1)+1:IND_SCA(J2)),  &
     &                            TIM_AVR(J2), TPION_AVR(J2),           &
     &                            TPIOFF_AVR(J2), TPION_RMS(J2),        &
     &                            TPIOFF_RMS(J2), KP, IUER )
               END IF
            END IF
 420     CONTINUE
      END IF
!
      RETURN
      END SUBROUTINE TPI_TIME_FILTER_SCAN !#!#! 2
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TPI_FREQ_FILTER_RAW ( ANC, IND_TIM, IPOL, NP, FRQ,    &
     &                                  TPION, TPIOFF, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TPI_FRQ_FILTER_RAW                                                     *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          IND_TIM       =  Time index                          { INT*4 }            *
! *                                                                                    *
! *          IPOL          =  Polarization                        { INT*1 }            *
! *                           == 1 --> R                                               *
! *                           == 2 --> L                                               *
! *                           == 3 --> H                                               *
! *                           == 4 --> V                                               *
! *                           == 5 --> X                                               *
! *                           == 6 --> Y                                               *
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
! *          FRQ           =  Filtered Frequency array       { REAL*8 }  (NPx1)  [Hz]  *
! *                                                                                    *
! *          TPION         =  Filtered TpiOn array           { INT*4 }   (NPx1)  [K]   *
! *                                                                                    *
! *          TPIOFF        =  Filtered TpiOff array          { INT*4 }   (NPx1)  [K]   *
! *                                                                                    *
! *   Copyright (c) 1975-2025 United States Government as represented by               *
! *   the Administrator of the National Aeronautics and Space                          *
! *   Administration. All Rights Reserved.                                             *
! *   License: NASA Open Source Software Agreement (NOSA).                             *
! *                                                                                    *
! * ###  10-SEP-2025   TPI_FREQ_RAW            v1.0 (d)  N. Habana  10-SEP-2025   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_TIM, NP, IUER, IER
      INTEGER*1  IPOL
      REAL*8     FRQ(ANC__MEPC)
      INTEGER*4  TPION(ANC__MEPC), TPIOFF(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      REAL*8     DEL_MIN, DELTS
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      PARAMETER  (DEL_MIN = 1.D-6)
!     
! --- Initial Values
!
      NP       = 0                    ! No. of filtered points
      FRQ      = 0.D0
      TPION    = 0
      TPIOFF   = 0
!     
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_TPS
!     
! ------ Ignore any filler frequency points
!
         DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!     
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN ( ANC%TPS(J1)%SKY_FRQ )   .AND.           &
     &        ( ANC%TPI(IND_TIM)%TPION(J1) > ANC__FILLER_I4) ) THEN

!
! --------- Check that it is the correct polarization.
!
!@@!            IF ( IPOL .NE. ANC%TPS(J1)%POL ) GOTO 410
!     
!
! -------- Store values for the filtered points
!
            NP = NP + 1                                         ! Number of points update
            FRQ(NP)    =  ANC%TPS(J1)%SKY_FRQ
            TPION(NP)  =  ANC%TPI(IND_TIM)%TPION(J1)
            TPIOFF(NP) =  ANC%TPI(IND_TIM)%TPIOFF(J1)
         END IF
 410  CONTINUE
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
! ---
      END SUBROUTINE TPI_FREQ_FILTER_RAW !#!#!#!3

      
