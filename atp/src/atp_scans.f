      SUBROUTINE  ATP_SCANS ( ANC, DT_MAX, IVAR_FLAG, NS,          &
     &                        IND_ARR, MP_SCA, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_SCANS                                                             *
! *   Scans as indexed in the derived type (before filtering)                        *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          DT_MAX   =  Time difference between scans       { REAL*8 } [s]     *
! *                                                                                  *
! *          IVAR_FLAG     =  Variable of interest flag           { INT*4 }          *
! *                           if == 1 --> Tsys                                       *
! *                           if == 2 --> Phas | Ampl_phas                           *
! *                           if == 3 --> TPI                                        *
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
! *          NS            =  Number of scans                         { INT*4 }      *
! *                                                                                  *
! *          IND_ARR       =  Array of indices where scans end        { INT*4 }      *
! *                                                                                  *
! *          MP_SCA        =  Maximum Number of points in scan        { INT*4 }      *
! *                                                                                  *
! *   Copyright (c) 1975-2025 United States Government as represented by             *
! *   the Administrator of the National Aeronautics and Space                        *
! *   Administration. All Rights Reserved.                                           *
! *   License: NASA Open Source Software Agreement (NOSA).                           *
! *                                                                                  *
! * ###  25-AUG-2022   ATP_SCANS              v3.0 (d)  N. Habana  19-NOV-2025   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_ARR(ANC__MEPC), IVAR_FLAG, NS
      INTEGER*4   MP_SCA(ANC__MEPC), J1, J2, J3, J4, J5, I11
      INTEGER*4   K1
      REAL*8      TIM_OLD, TIM_CUR, DT_MAX
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IVAR_CNT, ITCNT
      REAL*8      DT
!     
! --- Check if the flag is correct
!
      IF ( IVAR_FLAG == 1 .OR.                                          &
     &     IVAR_FLAG == 2 .OR.                                          &
     &     IVAR_FLAG == 3      ) THEN
         CONTINUE
      ELSE
         CALL IINCH ( IVAR_FLAG, STR1 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_SCANS',                        &
     &           'Incorrect variable flag '//TRIM(STR1)//'. '//         &
     &           'Expected 1 (Tsys) or 2 (phas/ampl) or 3 (TPI)' )
      END IF
!
! --- Check format
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
!
         IF ( IVAR_FLAG == 1 ) THEN
            IVAR_CNT = ANC%NUM_TSYS
         ELSEIF ( IVAR_FLAG == 2 ) THEN
            IVAR_CNT = ANC%NUM_PCAL
         ELSEIF ( IVAR_FLAG == 3 ) THEN
            IVAR_CNT = ANC%NUM_TPI
         END IF
!
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
!
         IF ( IVAR_FLAG == 1 ) THEN
            IVAR_CNT = ANC%NUM_EPO_TTO
         ELSEIF ( IVAR_FLAG == 2 ) THEN
            IVAR_CNT = ANC%NUM_EPO_PCAL
         ELSEIF ( IVAR_FLAG == 3 ) THEN
            IVAR_CNT = ANC%NUM_EPO_TPI
         END IF
!
      ELSE
         CALL ERR_LOG ( 2508, IUER, 'ATP_SCANS',                        &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
! --- Scans in a VLBI experiment
!
      IF ( ANC%EXP_TYPE == 'VLBI' ) THEN
!     
! ------ We assume that scans correspond to the defined zones in DOO
!        Therefore number of scans is just the number of DOO
!
         NS     = ANC%NUM_DOO
         MP_SCA = 0
!     
         IF ( IVAR_FLAG == 1 ) THEN
!
! --------- Get the time difference between initial Tsys and Initial
!           Data On 
!           N.B: This assumes that initial date of DOO is never a filler
!
            DT = (ANC%MJD_TTO*86400.D0 + ANC%TAI_TTO) -                 &
     &           (ANC%MJD_DOO*86400.D0 + ANC%TAI_DOO)
!
! --------- Loop through the Tsys timesteps and Data on. Also count
!           number of observations in each scan
!
            J3 = 1
!         
            DO 410 J1 = 1, NS 
               DO 420 J2 = J3, IVAR_CNT
!
! ------------ Initial time of TTO may vary from that of DOO, so adjust
!              for this difference.
!
                  TIM_CUR = ANC%TTO(J2)%TIM + DT
!
                  IF ( (TIM_CUR .GE. ANC%DOO(J1)%TIM(1)) .AND.          &
     &                 (TIM_CUR .LE. ANC%DOO(J1)%TIM(2))      ) THEN
!
! ------------------ Number of points in this scan
!
                     MP_SCA(J1) = MP_SCA(J1) + 1
!
! ------------------ Array index of end of scan
!
                     IND_ARR(J1) = J2
                  ELSE
                     J3 = J2
                     EXIT
                  END IF
 420           CONTINUE
 410        CONTINUE
!
! -------- Phase cal or Phase Amplitude
!
         ELSEIF ( IVAR_FLAG == 2 ) THEN
!
! --------- Get the time difference between initial Phase and Initial
!           Data On 
!           N.B: This assumes that initial date of DOO is never a filler
!
            DT = (ANC%MJD_PCAL*86400.D0 + ANC%TAI_PCAL) -               &
     &           (ANC%MJD_DOO*86400.D0  + ANC%TAI_DOO )
!
! --------- Loop through the Tsys timesteps and Data on. Also count
!           number of observations in each scan
!
            J3 = 1
!         
            DO 411 J1 = 1, NS 
               DO 421 J2 = J3, IVAR_CNT
!
! --------------- Initial time of TTO may vary from that of DOO, so
!                 adjust for this difference.
!
                  TIM_CUR = ANC%PCAL(J2)%TIM + DT
!
                  IF ( (TIM_CUR .GE. ANC%DOO(J1)%TIM(1)) .AND.          &
     &                 (TIM_CUR .LE. ANC%DOO(J1)%TIM(2))      ) THEN
!
! ------------------ Number of points in this scan
!
                     MP_SCA(J1) = MP_SCA(J1) + 1
!
! ------------------ Array index of end of scan
!
                     IND_ARR(J1) = J2
                  ELSE
                     J3 = J2
                     EXIT
                  END IF
 421           CONTINUE
 411        CONTINUE
!
! --------- Tsys
!
         ELSEIF ( IVAR_FLAG == 3 ) THEN
!
! --------- Get the time difference between initial TPI and Initial
!           Data On
!           N.B: This assumes that initial date of DOO is never a filler
!
            DT = (ANC%MJD_TPI*86400.D0 + ANC%TAI_TPI) -                 &
     &           (ANC%MJD_DOO*86400.D0 + ANC%TAI_DOO)
!
! --------- Loop through the Tsys timesteps and Data on. Also count
!           number of observations in each scan
!
            J3 = 1
!         
            DO 412 J1 = 1, NS 
               DO 422 J2 = J3, IVAR_CNT
!
! --------------- Initial time of TPI may vary from that of DOO, so 
!                 adjust for this difference.
!
                  TIM_CUR = ANC%TPI(J2)%TIM + DT
!
                  IF ( (TIM_CUR .GE. ANC%DOO(J1)%TIM(1)) .AND.          &
     &                 (TIM_CUR .LE. ANC%DOO(J1)%TIM(2))      ) THEN
!
! ------------------ Number of points in this scan
!
                     MP_SCA(J1) = MP_SCA(J1) + 1
!
! ------------------ Array index of end of scan
!
                     IND_ARR(J1) = J2
                  ELSE
                     J3 = J2
                     EXIT
                  END IF
 422           CONTINUE
 412        CONTINUE
         END IF
!
! --- Scans in a Single Dish Experiment
!
      ELSE
!
         NS      = 1
         MP_SCA  = 0
         IND_ARR = 0
!
! ------ TSYS
!
         IF ( IVAR_FLAG == 1 ) THEN
!
! --------- Loop through the Tsys timesteps
!    
            TIM_CUR = 0.D0
            TIM_OLD = 0.D0
            ITCNT   = 0
            DO 510 J1 = 1, IVAR_CNT
!
               TIM_CUR = ANC%TTO(J1)%TIM
!
               IF ( J1 == 1 ) THEN
                  TIM_OLD = TIM_CUR
               END IF
!
               IF ( DABS(TIM_CUR - TIM_OLD) > DT_MAX .OR.       &
     &              J1 == IVAR_CNT                            ) THEN
!
! --------------- Array of index end points
!
                  IND_ARR(NS) = J1
!
! --------------- Update number of points in this scan to account
!                 for the end point
!
                  MP_SCA(NS) = MP_SCA(NS) + 1
!
                  NS = NS + 1
!     
! ------------ Number of points in the first scan 
!
               ELSE
                  MP_SCA(NS) = MP_SCA(NS) + 1
               END IF
               TIM_OLD = TIM_CUR
 510        CONTINUE
!
! ------ PCAL
!
         ELSEIF ( IVAR_FLAG == 2 ) THEN
            DO 511 J1 = 1, IVAR_CNT
!
               TIM_CUR = ANC%PCAL(J1)%TIM
!
               IF ( J1 == 1 ) THEN
                  TIM_OLD = TIM_CUR
               END IF
!
               IF ( DABS(TIM_CUR - TIM_OLD) > DT_MAX .OR.       &
     &              J1 == IVAR_CNT                            ) THEN
!
! --------------- Array of index end points
!
                  IND_ARR(NS) = J1
!
! --------------- Update number of points in this scan to account
!                 for the end point
!
                  MP_SCA(NS) = MP_SCA(NS) + 1
!
                  NS = NS + 1
!     
! ------------ Number of points in the first scan 
!
               ELSE
                  MP_SCA(NS) = MP_SCA(NS) + 1
               END IF
               TIM_OLD = TIM_CUR
 511        CONTINUE
!
! ------ TPI
!
         ELSEIF ( IVAR_FLAG == 3 ) THEN
            DO 512 J1 = 1, IVAR_CNT
!
               TIM_CUR = ANC%TPI(J1)%TIM
!
               IF ( J1 == 1 ) THEN
                  TIM_OLD = TIM_CUR
               END IF
!
               IF ( DABS(TIM_CUR - TIM_OLD) > DT_MAX .OR.       &
     &              J1 == IVAR_CNT                            ) THEN
!
! --------------- Array of index end points
!
                  IND_ARR(NS) = J1
!
! --------------- Update number of points in this scan to account
!                 for the end point
!
                  MP_SCA(NS) = MP_SCA(NS) + 1
!
                  NS = NS + 1
!     
! ------------ Number of points in the first scan 
!
               ELSE
                  MP_SCA(NS) = MP_SCA(NS) + 1
               END IF
               TIM_OLD = TIM_CUR
 512        CONTINUE
         END IF
!
! ------ Remove the extra scan
!
         NS = NS - 1
      END IF
! ---
      RETURN
      END SUBROUTINE !#!1
!     
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_TSYS_SCANS ( ANC, IND_FRQ, TIM, TSYS, MS, NP,     &
     &                             MP_SCA, IND_ARR, NS, IND_SCA,        &
     &                             NP_SCA, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_TSYS_SCANS                                                        *
! *   Scans as indexed in the filtered arrays.                                       *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC         =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                  *
! *          IND_FRQ     =  Frequency index                     { INT*4 }            *
! *                                                                                  *
! *          TIM         =  Filtered Time Array                 { REAL*8 } [s]       *
! *                                                                                  *
! *          TSYS        =  Filtered Tsys Array                 { REAL*8 } [K]       *
! *                                                                                  *
! *          MS          =  Maximum Number of scans             { INT*4 }            *
! *                                                                                  *
! *          NP          =  Number of filtered points           { INT*4 }            *
! *                                                                                  *
! *          MP_SCA      =  Maximum number of points per scan   { INT*4 }            *
! *                                                                                  *
! *          IND_ARR     =  scan end index (on derived type)    { INT*4 }   (MSx1)   *
! *                                                                                  *
! *          IUER        =  Error Handler                       { INT*4, OPT }       *
! *                         If IUER=0 no error message will be printed,  even in     *
! *                         the event of an error. However, for other possible       *
! *                         values, i.e. IUER=-1,-2, & -3, the error message will    *
! *                         print to screen. For the latter case, i.e., IUER = -3,   *
! *                         after printing the program will terminate.               *
! *                         Default, IUER = -1                                       *
! *                                                                                  *
! *   OUTPUT:                                                                        *
! *          NS          =  Number of scans for this index      { INT*4 }            *
! *                                                                                  *
! *          IND_SCA     =  indices where filtered scans end    { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          NP_SCA      =  Number of points in each scan       { INT*4 }   (NSx1)   *
! *                                                                                  *
! *                                                                                  *
! *   Copyright (c) 1975-2025 United States Government as represented by             *
! *   the Administrator of the National Aeronautics and Space                        *
! *   Administration. All Rights Reserved.                                           *
! *   License: NASA Open Source Software Agreement (NOSA).                           *
! *                                                                                  *
! * ###  07-DEC-2022   ATP_TSYS_SCANS         v2.0 (d)  N. Habana  24-NOV-2025   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, J4, J5, J6, I11
      REAL*8      DT_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NP_SCA(ANC__MEPC)
      INTEGER*4   IVAR_CNT, MS, MP_SCA(ANC__MEPC)
      REAL*8      TIM(ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8      EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Loop through the timesteps, but note that the indices are now 
!     defined on the filtered data.
!    
!@      NS  = 0
  !@    NP = 0
!
! --- Check format
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         IVAR_CNT = ANC%NUM_TSYS
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         IVAR_CNT = ANC%NUM_EPO_TTO
      ELSE
         CALL ERR_LOG ( 2508, IUER, 'ATP_TSYS_SCANS',                        &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
! --- Get scans 
!
      NP_SCA = 0
      J3 = 1
      J6 = 0
!
      DO 410 J1 = 1, MS 

         IF ( J1 == 1 ) THEN
            J4 = 1
            J5 = IND_ARR(J1)
         ELSE
            J4 = J5 + 1
            J5 = IND_ARR(J1)
         END IF
!
         DO 420 J2 = J3, NP
!     
! --------- Get the actual index of Tsys on filtered array
!
            IF ( (TIM(J2) .GE. ANC%TTO(J4)%TIM) .AND.                   &
     &           (TIM(J2) .LE. ANC%TTO(J5)%TIM)       ) THEN
!
! ------------ Number of points in this scan
!
               NP_SCA(J1) = NP_SCA(J1) + 1
!
! ------------ Array index of end of scan
!
               IND_SCA(J1) = J2
            ELSE
               J3 = J2
               EXIT
            END IF
 420     CONTINUE
!
         IF ( NP_SCA(J1) == 0 ) J6 = J6 + 1
!
 410  CONTINUE
!
! --- Number of scans for this index
!
      NS = MS - J6
!
      RETURN
      END SUBROUTINE !#!#!2
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_PCAL_SCANS ( ANC, IND_FRQ, TIM, PCAL, MS, NP,     &
     &                             MP_SCA, IND_ARR, NS, IND_SCA,        &
     &                             NP_SCA, IUER )
!
! *************************************************************************************
! *                                                                                   *
! *   Routine  ATP_PCAL_SCANS                                                         *
! *   Scans as indexed in the filtered arrays.                                        *
! *                                                                                   *
! *   INPUT:                                                                          *
! *          ANC        =  Parsed Antenna Callibration file   { DERIVED TYPE }        *
! *                                                                                   *
! *          IND_FRQ    =  Frequency index                    { INT*4 }               *
! *                                                                                   *
! *          TIM        =  Filtered Time array                { REAL*8 } (LPx1) [s]   *
! *                                                                                   *
! *          PCAL       =  Filtered Pcal array                { CMPLX*8 } (NPx1) []   *
! *                                                                                   *
! *          MS         =  Maximum Number of scans            { INT*4 }               *
! *                                                                                   *
! *          NP         =  Number of filtered points          { INT*4 }               *
! *                                                                                   *
! *          MP_SCA     =  Maximum number of points per scan  { INT*4 }               *
! *                                                                                   *
! *          IND_ARR    =  scan end index (on derived type)   { INT*4 }   (MSx1)      *
! *                                                                                   *
! *          IUER       =  Error Handler                      { INT*4, OPT }          *
! *                           If IUER=0 no error message will be printed,  even in    *
! *                           the event of an error. However, for other possible      *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will   *
! *                           print to screen. For the latter case, i.e., IUER = -3,  *
! *                           after printing the program will terminate.              *
! *                           Default, IUER = -1                                      *
! *                                                                                   *
! *   OUTPUT:                                                                         *
! *          NS        =  Total number of scans                { INT*4 }              *
! *                                                                                   *
! *          IND_SCA   =  indices where scans end              { INT*4 }   (NSx1)     *
! *                                                                                   *
! *          NP_SCA    =  Number of points in each scan        { INT*4 }              *
! *                                                                                   *
! *   Copyright (c) 1975-2025 United States Government as represented by              *
! *   the Administrator of the National Aeronautics and Space                         *
! *   Administration. All Rights Reserved.                                            *
! *   License: NASA Open Source Software Agreement (NOSA).                            *
! *                                                                                   *
! * ###  09-MAR-2023   ATP_PCAL_SCANS          v1.0 (d)  N. Habana  25-NOV-2025 ###   *
! *                                                                                   *
! *************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, J4, J5, J6, I11
      REAL*8      DT_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NP_SCA(ANC__MEPC), NPC
      INTEGER*4   IVAR_CNT, MS, MP_SCA(ANC__MEPC)
      REAL*8      TIM(ANC__MEPC)
      COMPLEX*8   PCAL(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Check format
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         IVAR_CNT = ANC%NUM_PCAL
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         IVAR_CNT = ANC%NUM_EPO_PCAL
      ELSE
         CALL ERR_LOG ( 2508, IUER, 'ATP_PCAL_SCANS',                   &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
! --- Get scans 
!
      NP_SCA = 0
      J3 = 1
      J6 = 0
      DO 410 J1 = 1, MS 

         IF ( J1 == 1 ) THEN
            J4 = 1
            J5 = IND_ARR(J1)
         ELSE
            J4 = J5 + 1
            J5 = IND_ARR(J1)
         END IF
!
         DO 420 J2 = J3, NP
!     
! --------- Get the actual index of Tsys on filtered array
!
            IF ( (TIM(J2) .GE. ANC%PCAL(J4)%TIM) .AND.                   &
     &           (TIM(J2) .LE. ANC%PCAL(J5)%TIM)       ) THEN
!
! ------------ Number of points in this scan
!
               NP_SCA(J1) = NP_SCA(J1) + 1
!
! ------------ Array index of end of scan
!
               IND_SCA(J1) = J2
            ELSE
               J3 = J2
               EXIT
            END IF
 420     CONTINUE
!
         IF ( NP_SCA(J1) == 0 ) J6 = J6 + 1
!
 410  CONTINUE
!     
! --- Number of scans for this index
!
      NS = MS - J6
!
      RETURN
      END SUBROUTINE !#!#!#!3
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_SEFD_SCANS ( ANC, IND_FRQ, DT_MAX, TIM,      &
     &                             SEFD, NS, NP, IND_SCA, NP_SCA,      &
     &                             IND_ARR, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_SEFD_SCANS                                                        *
! *   Scans as indexed in the filtered arrays.                                       *
! *                                                                                  *
! *   N.B: More often than not, NS == 1                                              *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          DT_MAX   = Time difference between scans        { REAL*8 } [s]     *
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
! *          NS         =  Total number of scans                { INT*4 }            *
! *                                                                                  *
! *          IND_SCA    =  indices where scans end              { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          NP_SCA    =  Number of points in each scan        { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          IND_ARR    =  scan end index (on derived type)     { INT*4 }   (NSx1)   *
! *                                                                                  *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  07-DEC-2022   ATP_TSYS_SCANS         v1.0 (d)  N. Habana  07-DEC-2022   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, I11
      REAL*8      DT_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NP_SCA(ANC__MEPC), NPC
      REAL*8      TIM(ANC__MEPC), SEFD(ANC__MEPC)
      REAL*8      EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Loop through the timesteps, but note that the indices are now 
!     defined on the filtered data.
!    
      NS  = 0
      NP = 0
! ---
      DO 410 J1 = 1, ANC%NUM_SEFD
! ------
         TIM_CUR = ANC%SEFD(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = TIM_CUR
         END IF
! ------
         IF ( .NOT. IS_R8_NAN (ANC%SEFD(J1)%TIM)            .AND.       &
     &        .NOT. IS_R8_NAN (ANC%SEFD(J1)%SEFD(IND_FRQ))    ) THEN
! ---------
            IF ( (ANC%SEFD(J1)%SEFD(IND_FRQ) .LT. ANC__SEFD_MAX) .AND.  &
                 (ANC%SEFD(J1)%SEFD(IND_FRQ) .GT. ANC__SEFD_MIN) ) THEN
! ------------
               NP = NP + 1
!
! ------------ Is the time diff. enough to define a scan?
!
            
               IF ( DABS(TIM_CUR - TIM_OLD) > DT_MAX .OR.           &
     &              J1 == ANC%NUM_SEFD                         ) THEN
! ---------------
                  NS = NS + 1                                   ! Update number of scans
!
! --------------- Array of indices of each start point of a new scan
!
                  IF ( J1 > 1 ) THEN
                     IND_ARR(NS) = J1-1
                  ELSE
                     IND_ARR(NS) = J1
                  END IF
                  IND_SCA(NS) = NP
!
! --------------- Number of points in scan
!
                  IF ( NS == 1 ) THEN
                     I11 = NP
                     NP_SCA(NS) = NP
                  ELSE
                     NP_SCA(NS) = NP - I11
                     I11 = NP
                  END IF
               END IF
            END IF 
         END IF 

         TIM_OLD = ANC%SEFD(J1)%TIM
 410  CONTINUE
!
! --- Bug fix for cases where there is one scan detected with
!     end point at the first index
!
      IF ( NS == 1 .AND. IND_SCA(1) == 1 ) THEN
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NP_SCA(NS) = NP
      END IF
!
! --- We looped through all points, but no scan was defined.
! --- N.B: This is likely because the last point is an empty value.
!
      IF ( NS == 0 ) THEN
         NS = NS + 1
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NP_SCA(NS) = NP
      END IF
!
! --- Do we have more scans than in the derived type?
!
      CALL ERR_LOG ( 0, IUER )
      IF ( NS .GT. ANC%NUM_DOO ) THEN 
         CALL IINCH ( NS, STR1 )
         CALL IINCH ( ANC%NUM_DOO, STR2 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_SEFD_SCANS',                 &
     &           'Scans Counted '//TRIM(STR1)//' .ne. '//               &
     &           'Scans Declared '//TRIM(STR2) )
      END IF
! ---
      RETURN
      END SUBROUTINE !#!#!3
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_TPI_SCANS ( ANC, IND_FRQ, TIM, TPION, TPIOFF,     &
     &                            MS, NP, MP_SCA, IND_ARR, NS, IND_SCA, &
     &                            NP_SCA, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_TPI_SCANS                                                         *
! *   Scans as indexed in the filtered arrays.                                       *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC         =  Parsed Antenna Callibration file  { DERIVED TYPE }       *
! *                                                                                  *
! *          IND_FRQ     =  Frequency index                   { INT*4 }              *
! *                                                                                  *
! *          TIM         =  Filtered Time Array               { REAL*8 } [s]         *
! *                                                                                  *
! *          TPION       =  Filtered TPION array              { INT*4 }  (NPx1) [K]  *
! *                                                                                  *
! *          TPIOFF      =  Filtered Tpioff array             { INT*4 }  (NPx1) [K]  *
! *                                                                                  *
! *          MS          =  Maximum Number of scans           { INT*4 }              *
! *                                                                                  *
! *          NP          =  Number of filtered points         { INT*4 }              *
! *                                                                                  *
! *          MP_SCA      =  Maximum number of points per scan { INT*4 }              *
! *                                                                                  *
! *          IND_ARR     =  scan end index (on derived type)    { INT*4 }   (MSx1)   *
! *                                                                                  *
! *          IUER        =  Error Handler                       { INT*4, OPT }     *
! *                           If IUER=0 no error message will be printed,  even in   *
! *                           the event of an error. However, for other possible     *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will  *
! *                           print to screen. For the latter case, i.e., IUER = -3, *
! *                           after printing the program will terminate.             *
! *                           Default, IUER = -1                                     *
! *                                                                                  *
! *   OUTPUT:                                                                        *
! *          NS         =  Total number of scans                { INT*4 }            *
! *                                                                                  *
! *          IND_SCA    =  indices where scans end              { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          NP_SCA    =  Number of points in each scan        { INT*4 }   (NSx1)   *
! *                                                                                  *
! *   Copyright (c) 1975-2025 United States Government as represented by             *
! *   the Administrator of the National Aeronautics and Space                        *
! *   Administration. All Rights Reserved.                                           *
! *   License: NASA Open Source Software Agreement (NOSA).                           *
! *                                                                                  *
! * ###  10-SEP-2025   ATP_TPI_SCANS         v1.0 (d)  N. Habana  25-NOV-2025   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, J4, J5, J6, I11
      REAL*8      DT_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NP_SCA(ANC__MEPC), NPC
      INTEGER*4   IVAR_CNT, MS, MP_SCA(ANC__MEPC)
      REAL*8      TIM(ANC__MEPC)
      INTEGER*4   TPION(ANC__MEPC), TPIOFF(ANC__MEPC)
      REAL*8      EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Loop through the timesteps, but note that the indices are now 
!     defined on the filtered data.
!    
!@      NS  = 0
  !@    NP = 0
!
! --- Check format
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         IVAR_CNT = ANC%NUM_TPI
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         IVAR_CNT = ANC%NUM_EPO_TPI
      ELSE
         CALL ERR_LOG ( 2508, IUER, 'ATP_TPI_SCANS',                        &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
! --- Get scans 
!
      NP_SCA = 0
      J3 = 1
      J6 = 0
      DO 410 J1 = 1, MS 

         IF ( J1 == 1 ) THEN
            J4 = 1
            J5 = IND_ARR(J1)
         ELSE
            J4 = J5 + 1
            J5 = IND_ARR(J1)
         END IF
!
         DO 420 J2 = J3, NP
!     
! --------- Get the actual index of TPI on filtered array
!
            IF ( (TIM(J2) .GE. ANC%TPI(J4)%TIM) .AND.                   &
     &           (TIM(J2) .LE. ANC%TPI(J5)%TIM)       ) THEN
!
! ------------ Number of points in this scan
!
               NP_SCA(J1) = NP_SCA(J1) + 1
!
! ------------ Array index of end of scan
!
               IND_SCA(J1) = J2
            ELSE
               J3 = J2
               EXIT
            END IF
 420     CONTINUE
!
         IF ( NP_SCA(J1) == 0 ) J6 = J6 + 1
!
 410  CONTINUE

!
! --- Number of scans for this index
!
      NS = MS - J6
!
      RETURN
      END SUBROUTINE !#!#!4
