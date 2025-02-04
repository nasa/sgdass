      SUBROUTINE SEFD_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, TIM, SEFD,    &
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
! *          SEFD          =  Filtered SEFD array            { REAL*8 }  (NPx1)  [Jy]   *
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
      REAL*8     TIM(ANC__MEPC), TSYS(ANC__MEPC), GAIN(ANC__MEPC)
      REAL*8     SEFD(ANC__MEPC), TCAL(ANC__MEPC), TRAT(ANC__MEPC)
      REAL*8     EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP, NUM_SCA(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Initial Values
!
      NP = 0                    ! No. of filtered points
      TIM      = 0.D0
      SEFD     = 0.D0
      EL_ARR   = 0.D0
      AZ_ARR   = 0.D0
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_SEFD
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN ( ANC%SEFD(J1)%TIM )            .AND.     &
     &        .NOT. IS_R8_NAN ( ANC%SEFD(J1)%SEFD(IND_FRQ) ) ) THEN
!
! --------- Grab only values within the range of acceptable SEFD values.
!
            IF ( ANC%SEFD(J1)%SEFD(IND_FRQ) .GE. ANC__SEFD_MIN .AND.    &
     &           ANC%SEFD(J1)%SEFD(IND_FRQ) .LE. ANC__SEFD_MAX ) THEN
!    
! ------------ Store values for the filtered points
!
               NP = NP + 1                                                 ! Number of points update
               TIM(NP)     =  ANC%SEFD(J1)%TIM
               SEFD(NP)    =  ANC%SEFD(J1)%SEFD(IND_FRQ)
               EL_ARR(NP)  =  ANC%SEFD(J1)%EL
               AZ_ARR(NP)  =  ANC%SEFD(J1)%AZ
            END IF
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE SEFD_TIME_FILTER_RAW !#!1
!
! ---------------------------------------------------------------------------------------
!
      SUBROUTINE SEFD_FREQ_FILTER_RAW ( ANC, NPOL, IPOLS, ITIM, NP_SEFD, &
     &                                  SEFD_FRQ, SEFD, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  SEFD_FRQ_FILTER_RAW                                                     *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          ITIM          =  Time index                          { INT*4 }            *
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
! *          NPOL          =  Number of polarizations             { INT*4 }            *
! *                                                                                    *
! *          IPOLS         =  Polarizations                       { INT*1 } (NPOLx1)   *
! *                           == 1 --> R                                               *
! *                           == 2 --> L                                               *
! *                           == 3 --> H                                               *
! *                           == 4 --> V                                               *
! *                           == 5 --> X                                               *
! *                           == 6 --> Y                                               *
! *                                                                                    *
! *          NP_SEFD       =  Filtered Number of points      { INT*4 }                 *
! *                                                                                    *
! *          SEFD_FRQ      =  Filtered Frequency array       { REAL*8 }  (NPx1)  [MHz] *
! *                                                                                    *
! *          SEFD          =  Filtered SEFD array            { REAL*8 }  (NPx1)  [Jy]   *
! *                                                                                    *
! * ###  14-DEC-2022   SEFD_FREQ_RAW            v1.0 (c)  N. Habana  14-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  ITIM, IUER, IER, NPOL, NP_SEFD(ANC__MNPOL)
      INTEGER*1  IPOLS(ANC__MNPOL)
      REAL*8     SEFD_FRQ(ANC__MNPOL,ANC__MEPC), SEFD(ANC__MNPOL,ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4
      REAL*8     DEL_MIN, DELTS
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      PARAMETER  (DEL_MIN = 1.D-6)
!@      REAL*8     T1(ANC__MEPC), T2(ANC__MEPC), X1(ANC__MEPC), X2(ANC__MEPC) 
!     
! --- Initial Values
!
      NP_SEFD    = 0                    ! No. of filtered points
      SEFD_FRQ   = 0.D0
      SEFD       = 0.D0
!
! --- Get the number of polazrizations
!
      IUER = -1
      CALL GET_POL_TPS ( ANC, NPOL, IPOLS, IUER )
!
! --- Go through all the polarizations
!
      DO 420 J2 = 1, NPOL
!     
! ------ Filter all points, and emerge with the clean raw data 
!
         DO 410 J1 = 1, ANC%NUM_TPS
!     
! --------- Ignore any filler frequency points
!
            DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
            IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!     
! --------- Eliminate NaN values, and filter out points
!
            IF ( .NOT. IS_R8_NAN ( ANC%TPS(J1)%SKY_FRQ )         .AND.     &
     &           .NOT. IS_R8_NAN ( ANC%SEFD(ITIM)%SEFD(J1) ) ) THEN

!     
! ------------ Is there an actual SEFD value for this freq?
!
               DELTS = ABS( ANC%SEFD(ITIM)%SEFD(J1) - ANC__FILLER_R8 ) 
               IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!
! ------------ grab only the current polazrization values
!
               IF ( IPOLS(J2) == ANC%TPS(J1)%POL ) THEN
!     
! --------------- Grab only SEFD values within an acceptable range
!
                  IF ( ANC%SEFD(ITIM)%SEFD(J1) .GE. ANC__SEFD_MIN .AND. &
     &                 ANC%SEFD(ITIM)%SEFD(J1) .LE. ANC__SEFD_MAX ) THEN
!
! ------------------ Store values for the filtered points
!
                     NP_SEFD(J2) = NP_SEFD(J2) + 1                                             ! Number of points update
                     SEFD_FRQ(J2,NP_SEFD(J2)) =  ANC%TPS(J1)%SKY_FRQ
                     SEFD(J2,NP_SEFD(J2))     =  ANC%SEFD(ITIM)%SEFD(J1)
                  END IF
               END IF
            END IF
 410     CONTINUE
 420  CONTINUE
! ---      
      CALL ERR_LOG ( 0, IUER )
      RETURN
! ---
      END SUBROUTINE SEFD_FREQ_FILTER_RAW !#!#!#!2
!
! --------------------------------------------------------------------------
!
