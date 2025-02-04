      SUBROUTINE PCAL_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, TIM, PCAL,    &
     &                                  IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  PCAL_TIME_FILTER                                                      *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          IND_FRQ       =  Frequency index                     { INT*4 }          *
! *                                                                                  *
! *          IUER          =  Error Handler                       { INT*4, OPT }     *
! *                           If IUER=0 no error message will be printed,  even in   *
! *                           the event of an error. However, for other possible     *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will  *
! *                           print to screen. For the latter case, i.e., IUER = -3, *
! *                           after printing the program will terminate.             *
! *                           Default, IUER = -1                                     *
! *                                                                                  *
! *   OUTPUT:                                                                        *
! *          NP            =  Filtered Number of points      { INT*4 }               *
! *                                                                                  *
! *          TIM           =  Filtered Time array            { REAL*8 }  (LPx1)  [s] *
! *                                                                                  *
! *          PCAL          =  Filtered Pcal array            { CMPLX*8 } (NPx1)  []  *
! *                                                                                  *
! *                                                                                  *
! * ###  27-SEP-2022   PCAL_TIME_FILTER_RAW   v2.0 (c)  N. Habana  07-MAR-2023   ### *
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
      PCAL     = CMPLX(0.D0)
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_PCAL
!
! ------ Eliminate NaN values
!
         IF ( .NOT. IS_R8_NAN ( ANC%PCAL(J1)%TIM ) ) THEN
!
! --------- Grab only values within the range of acceptable Amplitude.
!
            IF ( ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)) .GE. ANC__AMP_MIN &
     &           .AND.                                                  &
     &           ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)) .LE. ANC__AMP_MAX &
     &         ) THEN
!    
! ------------ Store values for the filtered points
!
               NP = NP + 1                                                 ! Number of points update
               TIM(NP)    =  ANC%PCAL(J1)%TIM
               PCAL(NP)   =  ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)
            END IF
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE PCAL_TIME_FILTER_RAW !#!1
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE PCAL_TIME_FILTER_SCAN ( ANC, IND_FRQ, TIM_DIF_MAX, NP, &
     &                                   TIM, PCAL, NS, TIM_AVR,        &
     &                                   PCAL_AVR, PCAL_AMP_RMS,        &
     &                                   PCAL_PHA_RMS, IND_SCA,         &
     &                                   IND_ARR, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  PCAL_TIME_FILTER                                                        *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file   { DERIVED TYPE }      *
! *                                                                                    *
! *          IND_FRQ       =  Frequency index                    { INT*4 }             *
! *                                                                                    *
! *          TIM_DIF_MAX   = Time difference between scans       { REAL*8 } [s]        *
! *                                                                                    *
! *          NP            =  Filtered Number of points          { INT*4 }             *
! *                                                                                    *
! *          PCAL          =  Raw Tsys (pre-filter)              { CMPLX*8 } (NSx1) [] *
! *                                                                                    *
! *          IUER          =  Error Handler                      { INT*4, OPT }        *
! *                           If IUER=0 no error message will be printed,  even in     *
! *                           the event of an error. However, for other possible       *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will    *
! *                           print to screen. For the latter case, i.e., IUER = -3,   *
! *                           after printing the program will terminate.               *
! *                           Default, IUER = -1                                       *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *          NS            =  Number of scans                  { INT*4 }               *
! *                                                                                    *
! *          TIM_AVR       =  Average Time (post filters)      { REAL*8 }  (NSx1)  [s] *
! *                                                                                    *
! *          PCAL_AVR      =  Average PCAL (post filter)       { CMPLX*8 } (NSx1)  []  *
! *                                                                                    *
! *          PCAL_AMP_RMS  =  RMS of Amplitude (post-filters)  { REAL*8 }  (NSx1)  []  *
! *                                                                                    *
! *          PCAL_PHA_RMS  =  RMS of phase (post-filters)      { REAL*8 }  (NSx1)  []  *
! *                           N.B: pcal_pha_rms == [-pi, pi]                           *
! *                                                                                    *
! *          IND_SCA       =  indices where scans end          { INT*4 }   (NSx1)      *
! *                                                                                    *
! * ###  10-MAR-2023   PCAL_TIME_FILTER_SCAN    v1.0 (c)  N. Habana  10-MAR-2023  ###  *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IND_FRQ, NP, IND_SCA(ANC__MEPC), IUER
      REAL*8     TIM(ANC__MEPC), PCAL(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MEPC), PCAL_PHA_RMS(ANC__MEPC)
      REAL*8     TIM_DIF_MAX, TIM_OLD, TIM_CUR
      REAL*8     TIM_OUT(ANC__MEPC)
      COMPLEX*8  PCAL_OUT(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4, IND_ARR(ANC__MEPC)
      INTEGER*4  I11, I12, I21, I22
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!  
! --- Get the filtered scan indices
!
      IUER = -1
      CALL ATP_PCAL_SCANS ( ANC, IND_FRQ, TIM_DIF_MAX, TIM, PCAL, NS, &
     &                        NP, IND_SCA, NUM_SCA, IND_ARR, IUER )
!
! --- Loop through the scan and get averages vs RMS
!
      TIM_AVR  = 0.D0
      PCAL_AVR = CMPLX(0.D0)
      PCAL_AMP_RMS = 0.D0
      PCAL_PHA_RMS = 0.D0
! ---
      IF ( NS .GE. 1 ) THEN
! ---
         DO 420 J2 = 1, NS
! ------
            IF ( J2 == 1 ) THEN
               CALL PCAL_AVER ( NUM_SCA(1), TIM(1:IND_SCA(1)),          &
     &                          PCAL(1:IND_SCA(1)), TIM_AVR(1),         &
     &                          PCAL_AVR(1), PCAL_AMP_RMS(1),           &
     &                          PCAL_PHA_RMS(1), KP, IUER )
            ELSE
               CALL PCAL_AVER ( NUM_SCA(J2),                            &
     &                          TIM(IND_SCA(J2-1)+1:IND_SCA(J2)),       &
     &                          PCAL(IND_SCA(J2-1)+1:IND_SCA(J2)),      &
     &                          TIM_AVR(J2), PCAL_AVR(J2),              &
     &                          PCAL_AMP_RMS(J2),  PCAL_PHA_RMS(J2),    &
     &                          KP, IUER )
            END IF
 420     CONTINUE
      END IF
!
      RETURN
      END SUBROUTINE PCAL_TIME_FILTER_SCAN !#!#!2
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE PCAL_FREQ_FILTER_RAW ( ANC, IND_TIM, IPOL, NP, FRQ,    &
     &                                  PCAL, IUER )
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
! *          PCAL          =  Filtered Pcal array            { CMPLX*8 } (NPx1)  []    *
! *                                                                                    *
! * ###  14-DEC-2022   PCAL_FREQ_FILTER_RAW     v1.0 (c)  N. Habana  23-MAY-2023   ### *
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
      COMPLEX*8  PCAL(ANC__MEPC), PCAL_SCA(ANC__MEPC)
      PARAMETER  (DEL_MIN = 1.D-6)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Initial Values
!
      NP       = 0                    ! No. of filtered points
      FRQ      = 0.D0
      PCAL     = CMPLX ( 0.D0 )
!
! --- Filter all points, and emerge with the clean raw data
!
      DO 410 J1 = 1, ANC%NUM_PCS
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN ( ANC%PCS(J1)%SKY_FRQ ) ) THEN
!
! --------- Is there an actual PCAL value for this freq?
! --------- Check this through the amplitude             
!
            DELTS = DABS ( ABS(ANC%PCAL(IND_TIM)%PCAL_CMPL(J1)) -       &
     &                     ANC__FILLER_R8 )
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!
! --------- Check that it is the correct polarization.
!
            IF ( IPOL .NE. ANC%PCS(J1)%POL ) GOTO 410
!
! --------- Eliminate any values where the frequency is not defined.
! 
            DELTS = ABS( ANC%PCS(J1)%SKY_FRQ - ANC__FILLER_R8 )            
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!     
! --------- Grab only values within the range of acceptable Amplitude.
!
            IF ( ABS(ANC%PCAL(IND_TIM)%PCAL_CMPL(J1)) .GE. ANC__AMP_MIN &
     &           .AND.                                                  &
     &           ABS(ANC%PCAL(IND_TIM)%PCAL_CMPL(J1)) .LE. ANC__AMP_MAX &
     &         ) THEN

!
! ------------ Store values for the filtered points
!
               NP = NP + 1                                             ! Number of points update
               FRQ(NP)     =  ANC%PCS(J1)%SKY_FRQ
               PCAL(NP)    =  ANC%PCAL(IND_TIM)%PCAL_CMPL(J1)
            END IF 
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
! ---
      END SUBROUTINE PCAL_FREQ_FILTER_RAW !#!#!#!3
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE PCAL_FREQ_FILTER_SCAN ( ANC, ISCA, IPOL, TIM_DIF_MAX,  &
     &                                   NF, NS, NP, TIM_AVR, FRQ,      &
     &                                   PCAL_FRQ_AVR, PHA_FRQ_RMS,     &
     &                                   AMP_FRQ_RMS, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  TSYS_FREQ_FILTER_SCAN                                                   *
! *   For each scan (in time) get the average of all channels                          *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
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
! *          PCAL_FRQ_AVR  =  Average Tsys at each freq      { REAL*8 }  (NFx1)  [K]   *
! *                                                                                    *
! *          PHA_RMS_AVR   =  RMS of PHAS at each freq       { REAL*8 }  (NFx1)  [K]   *
! *                                                                                    *
! *          AMP_RMS_AVR   =  RMS of AMPL at each freq       { REAL*8 }  (NFx1)  [K]   *
! *                                                                                    *
! * ###  31-MAY-2023   TSYS_FREQ_FILTER_SCAN    v1.0 (c)  N. Habana  31-MAY-2023   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  NP, NS, ISCA, NF, IUER
      INTEGER*4  IND_ARR(ANC__MEPC), IND_SCA(ANC__MEPC)
      INTEGER*1  IPOL
      COMPLEX*8  PCAL_FRQ_AVR(ANC__MEPC), PCAL(ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MEPC)
      REAL*8     TIM(ANC__MEPC), FRQ(ANC__MEPC)
      REAL*8     TIM_AVR(ANC__MEPC)
      REAL*8     PHA_FRQ_RMS(ANC__MEPC), AMP_FRQ_RMS(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MEPC), PCAL_PHA_RMS(ANC__MEPC)
      REAL*8     TIM_DIF_MAX, TIM_OLD, TIM_CUR
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      CHARACTER  CISCA*8, CNS*8
      INTEGER*4  IP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
! ---
      NF = 0
!     
! --- Go through each frequency and get the average frequncy at the given scan
!
      DO 410 J1 = 1, ANC%NUM_PCS
!     
! ------ Do we have values for this frequency?
!
         IF ( .NOT. IS_R8_NAN ( ANC%PCS(J1)%SKY_FRQ ) ) THEN
!     
! --------- Is there an actual frequency value at this index?
!
            DELTS = ABS( ANC%PCS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!     
! --------- Check that it is the correct polarization.
!
            IF ( IPOL .NE. ANC%PCS(J1)%POL ) GOTO 410
!     
! --------- Get the scan averages at this frequency
!
            IUER = -1            
            CALL PCAL_TIME_FILTER_RAW ( ANC, J1, NP, TIM, PCAL, IUER )
! ---------
            IUER = -1
            CALL PCAL_TIME_FILTER_SCAN ( ANC, J1, TIM_DIF_MAX, NP, TIM, &
     &                                   PCAL, NS, TIM_AVR, PCAL_AVR,   &
     &                                   PCAL_AMP_RMS, PCAL_PHA_RMS,    &
     &                                   IND_SCA, IND_ARR, IUER )
!
! --------- Did we declare a number higher than the number of scans?
!
            IF ( ISCA .LE. NS ) THEN
!     
! ------------ Grab only values within the range of acceptable Amplitude.
!
               IF ( ABS(ANC%PCAL(ISCA)%PCAL_CMPL(J1)) .GE. ANC__AMP_MIN &
     &                         .AND.                                    &
     &              ABS(ANC%PCAL(ISCA)%PCAL_CMPL(J1)) .LE. ANC__AMP_MAX &
     &            ) THEN
!     
! --------------- Update the number of frequencies
!
                  NF = NF + 1
!
! --------------- Copy to the relevant frequency related arrays 
!
                  FRQ(NF)          = ANC%PCS(J1)%SKY_FRQ
                  PCAL_FRQ_AVR(NF) = PCAL_AVR(ISCA)
                  PHA_FRQ_RMS(NF)  = PCAL_PHA_RMS(ISCA)
                  AMP_FRQ_RMS(NF)  = PCAL_AMP_RMS(ISCA)                  
                 
               END IF
            ELSE
               CALL CLRCH ( CISCA )
               CALL CLRCH ( CNS )
               CALL IINCH ( ISCA, CISCA )
               CALL IINCH ( NS, CNS )
! ------------
!##!               IUER = -1
!##!               CALL ERR_LOG ( 2001, IUER, 'PCAL_FREQ_FILTER_SCAN',      &
!##!     &                 'Actual number of scans '//TRIM(CNS)//' is '//   &
!##!     &                 'less than the declared scan index of '//CISCA )
               WRITE(6,*) 'WARNING: PCAL_FREQ_FILTER_SCAN '
               WRITE(6,*) 'Actual number of scans ',TRIM(CNS),' is'
               WRITE(6,*) 'less than the declared scan index of ', CISCA
            END IF
         END IF
 410  CONTINUE
!
! --- Did we find any points to plot?
!
      IF ( NF == 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 2002, IUER, 'PCAL_FREQ_FILTER_SCAN',      &
     &           'There are no points to plot for this scan.')
      END IF
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE PCAL_FREQ_FILTER_SCAN !#!#!#!#!4




