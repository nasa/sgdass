      SUBROUTINE TSYS_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, TIM, TSYS,    &
     &                                  EL_ARR, AZ_ARR, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_TIME_FILTER_RAW                                                    *
! *   N.B: This routine does not remove outliers/spikes, those are removed when        *
! *        averaging                                                                   *
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
! *          TSYS          =  Filtered Tsys array            { REAL*8 }  (NPx1)  [K]   *
! *                                                                                    *
! *          EL_ARR        =  Filtered Elevation Array       { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! *          AZ_ARR        =  Filtered Azimuth Array         { REAL*8 }  (NPx1)  [rad] *
! *                                                                                    *
! * ###  07-DEC-2022   TSYS_TIME_RAW            v1.0 (c)  N. Habana  07-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_FRQ, NP, IUER
      REAL*8     TIM(ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Initial Values
!
      NP = 0                    ! No. of filtered points
      TIM      = 0.D0
      TSYS     = 0.D0
      EL_ARR   = 0.D0
      AZ_ARR   = 0.D0
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_TSYS
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN ( ANC%TSYS(J1)%TIM )            .AND.     &
     &        .NOT. IS_R8_NAN ( ANC%TSYS(J1)%TSYS(IND_FRQ) ) ) THEN
!
! --------- Grab only values within the range of acceptable TSYS values.
!
            IF ( ANC%TSYS(J1)%TSYS(IND_FRQ) .GE. ANC__TSYS_MIN .AND.    &
     &           ANC%TSYS(J1)%TSYS(IND_FRQ) .LE. ANC__TSYS_MAX ) THEN
!    
! ------------ Store values for the filtered points
!
               NP = NP + 1                                                 ! Number of points update
               TIM(NP)     =  ANC%TSYS(J1)%TIM
               TSYS(NP)    =  ANC%TSYS(J1)%TSYS(IND_FRQ)
               EL_ARR(NP)  =  ANC%TSYS(J1)%EL
               AZ_ARR(NP)  =  ANC%TSYS(J1)%AZ
            END IF
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE TSYS_TIME_FILTER_RAW !#!1
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TSYS_TIME_FILTER_SCAN ( ANC, IND_FRQ, TIM_DIF_MAX, NP, &
     &                                   TIM, TSYS, NS,                 &
     &                                   TIM_AVR, TSYS_AVR, TSYS_RMS,   &
     &                                   IND_SCA, IND_ARR, IUER )
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
! * ###  08-DEC-2022   TSYS_TIME_FILTER_SCAN    v1.0 (c)  N. Habana  08-DEC-2022   ### *
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
      INTEGER*4  J1, J2, J3, J4, IND_ARR(ANC__MEPC)
      INTEGER*4  I11, I12, I21, I22
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!  
! --- Get the filtered scan indices
!
      IUER = -1
      CALL ATP_TSYS_SCANS ( ANC, IND_FRQ, TIM_DIF_MAX, TIM, TSYS, NS,   &
     &                      NP, IND_SCA, NUM_SCA, IND_ARR, IUER )

!@          PRINT *, "%%%%%%% TSYS_FILTER_2 - 147  %%%%%%%%%"
  !@        PRINT *, "FRQ_IND:", IND_FRQ, "NS:", NS, "NP:", NP
    !@      DO J1 = 1, NS
      !@       PRINT *, "SCAN:", J1, "IND_SCA:", IND_SCA(J1), "NUM_SCA:", NUM_SCA(J1), "IND_ARR:", IND_ARR(J1)
        !@  END DO

!     
! --- Loop through the scan and get averages vs RMS
!
      TIM_AVR  = 0.D0
      TSYS_AVR = 0.D0
      TSYS_RMS = 0.D0
! ---
      IF ( NS .GE. 1 ) THEN
! ---
         DO 420 J2 = 1, NS
! ------
            IF ( J2 == 1 ) THEN
               CALL TSYS_AVER ( NUM_SCA(1), TIM(1:IND_SCA(1)),          &
     &                          TSYS(1:IND_SCA(1)), TIM_AVR(1),         &
     &                          TSYS_AVR(1), TSYS_RMS(1), KP, IUER )
            ELSE
               CALL TSYS_AVER ( NUM_SCA(J2),                            &
     &                          TIM(IND_SCA(J2-1)+1:IND_SCA(J2)),       &
     &                          TSYS(IND_SCA(J2-1)+1:IND_SCA(J2)),      &
     &                          TIM_AVR(J2), TSYS_AVR(J2),              &
     &                          TSYS_RMS(J2), KP, IUER )
            END IF
 420     CONTINUE
      END IF
!
      RETURN
      END SUBROUTINE TSYS_TIME_FILTER_SCAN !#!#!2
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TSYS_FREQ_FILTER_RAW ( ANC, IND_TIM, IPOL, NP, FRQ,    &
     &                                  TSYS, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_FRQ_FILTER_RAW                                                     *
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
! *          FRQ           =  Filtered Frequency array       { REAL*8 }  (NPx1)  [MHz] *
! *                                                                                    *
! *          TSYS          =  Filtered Tsys array            { REAL*8 }  (NPx1)  [K]   *
! *                                                                                    *
! * ###  14-DEC-2022   TSYS_FREQ_RAW            v1.0 (c)  N. Habana  14-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  IND_TIM, NP, IUER, IER
      INTEGER*1  IPOL
      REAL*8     FRQ(ANC__MEPC), TSYS(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      REAL*8     DEL_MIN, DELTS
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      PARAMETER  (DEL_MIN = 1.D-6)
     !@@@@
      REAL*8 XPLT(1000), YPLT(1000)      
!     
! --- Initial Values
!
      NP       = 0                    ! No. of filtered points
      FRQ      = 0.D0
      TSYS     = 0.D0
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
         IF ( .NOT. IS_R8_NAN ( ANC%TPS(J1)%SKY_FRQ )         .AND.     &
     &        .NOT. IS_R8_NAN ( ANC%TSYS(IND_TIM)%TSYS(J1) ) ) THEN

!     
! --------- Is there an actual TSYS value for this freq?
!
            DELTS = ABS( ANC%TSYS(IND_TIM)%TSYS(J1) - ANC__FILLER_R8 ) 
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!
! --------- Check that it is the correct polarization.
!
!@@!            IF ( IPOL .NE. ANC%TPS(J1)%POL ) GOTO 410
!     
! --------- Grab only TSYS values within an acceptable range
!
            IF ( ANC%TSYS(IND_TIM)%TSYS(J1) .GE. ANC__TSYS_MIN .AND. &
     &           ANC%TSYS(IND_TIM)%TSYS(J1) .LE. ANC__TSYS_MAX ) THEN
!
! ------------ Store values for the filtered points
!
               NP = NP + 1                                             ! Number of points update
               FRQ(NP)     =  ANC%TPS(J1)%SKY_FRQ
               TSYS(NP)    =  ANC%TSYS(IND_TIM)%TSYS(J1)
            END IF 
         END IF
 410  CONTINUE
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
! ---
      END SUBROUTINE TSYS_FREQ_FILTER_RAW !#!#!#!3
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TSYS_FREQ_FILTER_SCAN ( ANC, ISCA, IPOL, TIM_DIF_MAX,  &
     &                                   NF, NS, NP, TIM_AVR, FRQ,      &
     &                                   TSYS_FRQ_AVR, TSYS_FRQ_RMS,    &
     &                                   IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_FREQ_FILTER_SCAN                                                   *
! *   For each scan (in time) get the average of all channels                          *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          IND_TIM       =  Time index                          { INT*4 }            *
! *          ISCA          =  Scan number                         { INT*4 }            *
! *                           N.B: We are only ever likely to use this routine for the *
! *                                averaging. erego, instead of IND_TIM, we will       *
! *                                rename the variable ISCA, to show that we are       *
! *                                plotting a particular scan number                   *
! *                                                                                    *
! *          IPOL          =  Polarization                        { INT*1 }            *
! *                           == 1 --> R                                               *
! *                           == 2 --> L                                               *
! *                           == 3 --> H                                               *
! *                           == 4 --> V                                               *
! *                           == 5 --> X                                               *
! *                           == 6 --> Y                                               *
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
! *                                                                                    *
! *          NS            =  Number of scans                { INT*4 }                 *
! *                                                                                    *
! *          NF            =  Number of frequencies          { INT*4 }                 *
! *                                                                                    *
! *          TIM_AVR       =  Average Time (after filters)   { REAL*8 }  (NSx1)  [s]   *
! *                                                                                    *
! *          FRQ           =  Filtered Frequency array       { REAL*8 }  (NFx1)  [MHz] *
! *                                                                                    *
! *          TSYS_FRQ_AVR  =  Average Tsys at each freq      { REAL*8 }  (NFx1)  [K]   *
! *                                                                                    *
! *          TSYS_RMS_AVR  =  RMS of TSYS at each freq       { REAL*8 }  (NFx1)  [K]   *
! *                                                                                    *
! * ###  22-MAY-2023   TSYS_FREQ_FILTER_SCAN    v1.2 (c)  N. Habana  26-MAY-2023   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IND_TIM, NP, NS, ISCA, NF, IUER
      INTEGER*4  IND_ARR(ANC__MEPC), IND_SCA(ANC__MEPC)
      INTEGER*1  IPOL
      REAL*8     TIM(ANC__MEPC), TSYS(ANC__MEPC), FRQ(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC)
      REAL*8     TSYS_AVR(ANC__MEPC), TSYS_RMS(ANC__MEPC)
      REAL*8     TIM_DIF_MAX, TIM_OLD, TIM_CUR
      REAL*8     TSYS_FRQ_AVR(ANC__MEPC), TSYS_FRQ_RMS(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      CHARACTER  CISCA*8, CNS*8, CFRQ*12
      INTEGER*4  IP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
! ---
      NF = 0
!     
! --- Go through each frequency and get the average frequncy at the given scan
!
      DO 410 J1 = 1, ANC%NUM_TPS
!     
! ------ Do we have values for this frequency?
!
         IF ( .NOT. IS_R8_NAN ( ANC%TPS(J1)%SKY_FRQ ) ) THEN
!     
! --------- Is there an actual frequency value at this index?
!
            DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!     
! --------- Check that it is the correct polarization.
!
            IF ( IPOL .NE. ANC%TPS(J1)%POL ) GOTO 410
!     
! --------- Get the scan averages at this frequency
!
            IUER = -1            
            CALL TSYS_TIME_FILTER_RAW ( ANC, J1, NP, TIM, TSYS,    &
     &                                  EL_ARR, AZ_ARR, IUER )
! ---------
            IUER = -1
            CALL TSYS_TIME_FILTER_SCAN ( ANC, J1, TIM_DIF_MAX, NP, &
     &                                   TIM, TSYS, NS,                 &
     &                                   TIM_AVR, TSYS_AVR, TSYS_RMS,   &
     &                                   IND_SCA, IND_ARR, IUER )
!     
! --------- Did we declare a number higher than the number of scans?
!
            IF ( ISCA .LE. NS ) THEN
!     
! ------------ Grab only TSYS values within an acceptable range
!
               IF ( (TSYS_AVR(ISCA) .GE. ANC__TSYS_MIN) .AND.           &
     &              (TSYS_AVR(ISCA) .LE. ANC__TSYS_MAX)       ) THEN
!     
! --------------- Update the number of frequencies
!
                  NF = NF + 1
!
! --------------- Copy to the relevant frequency related arrays 
!
                  FRQ(NF)          = ANC%TPS(J1)%SKY_FRQ
                  TSYS_FRQ_AVR(NF) = TSYS_AVR(ISCA)
                  TSYS_FRQ_RMS(NF) = TSYS_RMS(ISCA)
               END IF
            ELSE
               CALL CLRCH ( CISCA )
               CALL CLRCH ( CNS )
               CALL CLRCH ( CFRQ )
               CALL IINCH ( ISCA, CISCA )
               CALL IINCH ( NS, CNS )
               WRITE ( CFRQ, '(F10.2)' ) ANC%TPS(J1)%SKY_FRQ
! ------------
!##!               IUER = -1
!##!               CALL ERR_LOG ( 2001, IUER, 'TSYS_FREQ_FILTER_SCAN',      &
!##!     &                 'Actual number of scans '//TRIM(CNS)//' is '//   &
!##!     &                 'less than the declared scan index of '//CISCA )
               WRITE(6,*) "WARNING: TSYS_FREQ_FILTER_SCAN "
               WRITE(6,*) TRIM(CFRQ), "participates only in ", TRIM(CNS)
               WRITE(6,*) "scans. Which is less than the declared "
               WRITE(6,*) "scan index of ", CISCA
            END IF
         END IF
 410  CONTINUE
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
!
      END SUBROUTINE TSYS_FREQ_FILTER_SCAN !#!#!#!#!4
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE TSYS_FREQ_POL_MATCH ( NUM_TPS, FREQ_ARR, POL_ARR,      &
     &                                 FRQ_VAL, IPOL, IND_FRQ, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_FREQ_POL_MATCH                                                     *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          NUM_TPS       =  Number of Tsys sensors              { REAL*8 }           *
! *                                                                                    *
! *          FREQ_ARR      =  Frequency Array                     { REAL*8 }           *
! *                                                                                    *
! *          POL_ARR       =  Polarization Array                  { REAL*8 }           *
! *                                                                                    *
! *          FRQ_VAL       =  Frequency Value                     { REAL*8 }           *
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
! *          IND_FRQ       =  Frequency index                     { INT*4 }            *
! *                                                                                    *
! * ###  07-DEC-2022   TSYS_TIME_RAW            v1.0 (c)  N. Habana  07-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      INTEGER*4  NUM_TPS, IND_FRQ, IUER
      REAL*8     FRQ_VAL, FREQ_ARR(ANC__MEPC)
      INTEGER*1  POL_ARR(ANC__MEPC), IPOL
      INTEGER*4  IDX1, IDX2
      INTEGER*4  J1, J2, J3, J4
      CHARACTER  CPOL*2, CFREQ*8
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*4, EXTERNAL :: IXMN8_S, IXMN8
!
! --- Iterate through only half the array. 
!
      IND_FRQ  = 0
      IDX1     = 0 
      IDX2     = 0
      DO 320 J1 = 1, NUM_TPS


         IF ( IDX1 .NE. 0 ) THEN
!@@!            PRINT *, "%%%%%%%%%%%%%%% TSYS_FILTER_2 - 360 %%%%"
!@@!            PRINT *, "J1=", J1, "IDX1=", IDX1, "inPOL=", ANC__POL(IPOL), "inFRQ=", FRQ_VAL, "outPOL=", ANC__POL(POL_ARR(IDX1)), "outFRQ=",FREQ_ARR(IDX1)
         END IF

         IF ( J1 == 1 ) THEN
!
! --------- Search index for the closest value to the frequency we want
!
            IDX1 = IXMN8_S (1, NUM_TPS, FREQ_ARR, FRQ_VAL )
            IF ( IDX1 == -1 ) IDX1 = 1                          ! Smaller than minimum freq.
            IF ( IDX1 == -2 ) IDX1 = NUM_TPS                    ! Larger than minimum freq.
!
! --------- Does this index match the polarization?
!
            IF ( POL_ARR(IDX1) == IPOL ) THEN
               IND_FRQ = IDX1
               GOTO 700
            END IF
         ELSE
!
! --------- Start searching from where we stopped.
!
            IDX2 = IDX1
!
            IDX1 = IXMN8_S (IDX2, NUM_TPS, FREQ_ARR, FRQ_VAL )
            IF ( IDX1 == -1 ) IDX1 = 1                          ! Smaller than minimum freq.
            IF ( IDX1 == -2 ) IDX1 = NUM_TPS                    ! Larger than minimum freq.
!
! --------- Does this index match the polarization?
!
            IF ( POL_ARR(IDX1) == IPOL ) THEN
               IND_FRQ = IDX1
               GOTO 700
            END IF
         END IF
 320  CONTINUE
! ---
      IF ( IND_FRQ == 0 ) THEN
         IUER = -1
         CPOL = ANC__POL(IPOL)
         CALL ERR_LOG ( 2912, IUER, 'TSYS_FREQ_POL_MATCH',              &
     &           'No match for the given polarization '//CPOL//         &
     &           'and frequency ' )
      END IF

! ---
 700  CONTINUE
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE TSYS_FREQ_POL_MATCH !#!#!#!#!#!5
!
! ---------------------------------------------------------------------------------------
!
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
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN (ANC%TSYS(J1)%TIM)            .AND.       &
     &        .NOT. IS_R8_NAN (ANC%TSYS(J1)%TSYS(IND_FRQ))    ) THEN

               IF ( ANC%TSYS(J1)%TSYS(IND_FRQ) .GE. ANC__TSYS_MIN .AND. &
     &              ANC%TSYS(J1)%TSYS(IND_FRQ) .LE. ANC__TSYS_MAX ) THEN
!    
! --------------- Store values for the filtered points
!
                  NP = NP + 1                                                 ! Number of points update
                  TIM(NP)     =  ANC%TSYS(J1)%TIM
                  TSYS(NP)    =  ANC%TSYS(J1)%TSYS(IND_FRQ)
                  EL_ARR(NP)  =  ANC%TSYS(J1)%EL
                  AZ_ARR(NP)  =  ANC%TSYS(J1)%AZ
               END IF
         END IF
 410  CONTINUE
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TSYS_TIME_FILTER_2  !#!
