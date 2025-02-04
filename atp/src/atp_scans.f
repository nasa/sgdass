      SUBROUTINE  ATP_SCANS ( ANC, TIM_DIF_MAX, IVAR_FLAG, NS,          &
     &                        IND_ARR, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_SCANS                                                             *
! *   Scans as indexed in the derived type (before filtering)                        *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          TIM_DIF_MAX   =  Time difference between scans       { REAL*8 } [s]     *
! *                                                                                  *
! *          IVAR_FLAG     =  Variable of interest flag           { INT*4 }          *
! *                           if == 1 --> Tsys                                       *
! *                           if == 2 --> Phas | Ampl_phas                           *
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
! * ###  25-AUG-2022   ATP_SCANS              v2.0 (c)  N. Habana  23-OCT-2023   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_ARR(ANC__MEPC), IVAR_FLAG, NS
      INTEGER*4   J1, I11
      INTEGER*4   K1
      REAL*8      TIM_OLD, TIM_CUR, TIM_DIF_MAX
      CHARACTER   STR1*128, STR2*128
!
! --- Check if the flag is correct
!
      IF ( IVAR_FLAG == 1 .OR. IVAR_FLAG == 2 ) THEN
         CONTINUE
      ELSE
         CALL IINCH ( IVAR_FLAG, STR1 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_SCANS',                        &
     &           'Incorrect variable flag '//TRIM(STR1)//'. '//         &
     &           'Expected 1 (for Tsys) or 2 (for phas or ampl)' )
      END IF

      IF ( IVAR_FLAG == 1 ) THEN
!
! ------ Loop through the Tsys timesteps
!    
         NS      = 0
         TIM_CUR = 0.D0
         TIM_OLD = 0.D0
         DO 410 J1 = 1, ANC%NUM_TSYS
! ---------
            TIM_CUR = ANC%TSYS(J1)%TIM
! ---------
            IF ( J1 == 1 ) THEN
               TIM_OLD = TIM_CUR
            END IF
!
! --------- Is the time diff. enough to define a scan?
!
            IF ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX .OR.            &
     &           J1 == ANC%NUM_TSYS                          ) THEN
! ------------
               NS = NS + 1 ! Update number of scans
!
! ------------ Array of indices of each start point of a new scan
!
               IF ( J1 > 1 ) THEN
                  IND_ARR(NS) = J1-1
               ELSE
                  IND_ARR(NS) = J1
               END IF
            END IF 
            TIM_OLD = TIM_CUR
 410     CONTINUE
      ELSEIF ( IVAR_FLAG == 2 ) THEN
!
! ------ Loop through the PCal timesteps
!    
         NS      = 0
         TIM_CUR = 0.D0
         TIM_OLD = 0.D0
         DO 411 J1 = 1, ANC%NUM_PCAL
! ---------
            TIM_CUR = ANC%PCAL(J1)%TIM
! ---------
            IF ( J1 == 1 ) THEN
               TIM_OLD = TIM_CUR
            END IF
!
! --------- Is the time diff. enough to define a scan?
!
            IF ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX .OR.            &
     &           J1 == ANC%NUM_PCAL                          ) THEN
! ------------
               NS = NS + 1 ! Update number of scans
!
! ------------ Array of indices of each start point of a new scan
!
               IF ( J1 > 1 ) THEN
                  IND_ARR(NS) = J1-1
               ELSE
                  IND_ARR(NS) = J1
               END IF
            END IF 
            TIM_OLD = TIM_CUR
 411     CONTINUE
      END IF
!
! --- Do we have more scans than in the derived type?
!
      CALL ERR_LOG ( 0, IUER )
      IF ( NS .GT. ANC%NUM_DOO ) THEN 
         CALL CLRCH ( STR1 )
         CALL CLRCH ( STR2 )
         CALL IINCH ( NS, STR1 )
         CALL IINCH ( ANC%NUM_DOO, STR2 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_SCANS',                        &
     &           'Scans Counted '//TRIM(STR1)//' > '//                  &
     &           'Scans Declared '//TRIM(STR2) )
      END IF
! ---
      RETURN
      END SUBROUTINE !#!1
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_TSYS_SCANS ( ANC, IND_FRQ, TIM_DIF_MAX, TIM,      &
     &                             TSYS, NS, NP, IND_SCA, NUM_SCA,      &
     &                             IND_ARR, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine  ATP_TSYS_SCANS                                                      *
! *   Scans as indexed in the filtered arrays.                                       *
! *                                                                                  *
! *   INPUT:                                                                         *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }   *
! *                                                                                  *
! *          TIM_DIF_MAX   = Time difference between scans        { REAL*8 } [s]     *
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
! *          NUM_SCA    =  Number of points in each scan        { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          IND_ARR    =  scan end index (on derived type)     { INT*4 }   (NSx1)   *
! *                                                                                  *
! * ###  07-DEC-2022   ATP_TSYS_SCANS         v1.0 (c)  N. Habana  07-DEC-2022   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, I11
      REAL*8      TIM_DIF_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NUM_SCA(ANC__MEPC), NPC
      REAL*8      TIM(ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8      EL_ARR(ANC__MEPC), AZ_ARR(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Loop through the timesteps, but note that the indices are now 
!     defined on the filtered data.
!    
      NS  = 0
      NP = 0
! ---
      DO 410 J1 = 1, ANC%NUM_TSYS
! ------
         TIM_CUR = ANC%TSYS(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = TIM_CUR
         END IF
! ------
         IF ( .NOT. IS_R8_NAN (ANC%TSYS(J1)%TIM)            .AND.       &
     &        .NOT. IS_R8_NAN (ANC%TSYS(J1)%TSYS(IND_FRQ))    ) THEN
! ---------
            IF ( (ANC%TSYS(J1)%TSYS(IND_FRQ) .LT. ANC__TSYS_MAX) .AND.  &
                 (ANC%TSYS(J1)%TSYS(IND_FRQ) .GT. ANC__TSYS_MIN) ) THEN
! ------------
               NP = NP + 1
!
! ------------ Is the time diff. enough to define a scan?
!
            
               IF ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX .OR.           &
     &              J1 == ANC%NUM_TSYS                         ) THEN
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
                     NUM_SCA(NS) = NP
                  ELSE
                     NUM_SCA(NS) = NP - I11
                     I11 = NP
                  END IF
               END IF
            END IF 
         END IF 

         TIM_OLD = ANC%TSYS(J1)%TIM
 410  CONTINUE
!
! --- Bug fix for cases where there is one scan detected with
!     end point at the first index
!
      IF ( NS == 1 .AND. IND_SCA(1) == 1 ) THEN
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NUM_SCA(NS) = NP
      END IF
!     
! --- We looped through all points, but no scan was defined.
! --- N.B: This is likely because the last point is an empty value.
!
      IF ( NS == 0 ) THEN
         NS = NS + 1
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NUM_SCA(NS) = NP
      END IF
!
! --- Do we have more scans than in the derived type?
!
      CALL ERR_LOG ( 0, IUER )
      IF ( NS .GT. ANC%NUM_DOO ) THEN 
         CALL IINCH ( NS, STR1 )
         CALL IINCH ( ANC%NUM_DOO, STR2 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_TSYS_SCANS',                 &
     &           'Scans Counted '//TRIM(STR1)//' .ne. '//               &
     &           'Scans Declared '//TRIM(STR2) )
      END IF
! ---
      RETURN
      END SUBROUTINE !#!#!2
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_PCAL_SCANS ( ANC, IND_FRQ, TIM_DIF_MAX, TIM,      &
     &                             PCAL, NS, NP, IND_SCA, NUM_SCA,      &
     &                             IND_ARR, IUER )
!
! *************************************************************************************
! *                                                                                   *
! *   Routine  ATP_PCAL_SCANS                                                         *
! *   Scans as indexed in the filtered arrays.                                        *
! *                                                                                   *
! *   INPUT:                                                                          *
! *          ANC           =  Parsed Antenna Callibration file  { DERIVED TYPE }      *
! *                                                                                   *
! *          IND_FRQ       =  Frequency index                   { INT*4 }             *
! *                                                                                   *
! *          TIM_DIF_MAX   =  Time difference between scans     { REAL*8 } [s]        *
! *                                                                                   *
! *          TIM           =  Time array                        { REAL*8 } (LPx1) [s] *
! *                                                                                   *
! *          PCAL          =  Pcal array                        { CMPLX*8 } (NPx1) [] *
! *                                                                                   *
! *          IUER          =  Error Handler                     { INT*4, OPT }        *
! *                           If IUER=0 no error message will be printed,  even in    *
! *                           the event of an error. However, for other possible      *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will   *
! *                           print to screen. For the latter case, i.e., IUER = -3,  *
! *                           after printing the program will terminate.              *
! *                           Default, IUER = -1                                      *
! *                                                                                   *
! *   OUTPUT:                                                                         *
! *          NS         =  Total number of scans                { INT*4 }             *
! *                                                                                   *
! *          NP         =  Number of points                     { INT*4 }             *
! *                                                                                   *
! *          IND_SCA    =  indices where scans end              { INT*4 }   (NSx1)    *
! *                                                                                   *
! *          NUM_SCA    =  Number of points in each scan        { INT*4 }             *
! *                                                                                   *
! *          IND_ARR    =                { INT*4 }   (NSx1)    *
! *                                                                                   *
! * ###  09-MAR-2023   ATP_PCAL_SCANS          v1.0 (c)  N. Habana  09-MAR-2023 ###   *
! *                                                                                   *
! *************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, I11
      REAL*8      TIM_DIF_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NUM_SCA(ANC__MEPC), NPC
      REAL*8      TIM(ANC__MEPC)
      COMPLEX*8   PCAL(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Loop through the timesteps, but note that the indices are now 
!     defined on the filtered data.
!    
      NS  = 0
      NP = 0
! ---
      DO 410 J1 = 1, ANC%NUM_PCAL
! ------
         TIM_CUR = ANC%PCAL(J1)%TIM
! ------
         IF ( J1 == 1 ) THEN
            TIM_OLD = TIM_CUR
         END IF
! ------
         IF ( .NOT. IS_R8_NAN (ANC%PCAL(J1)%TIM) ) THEN
! ---------
            IF ( ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)) < ANC__AMP_MAX    &
     &           .AND.                                                  &
     &           ABS(ANC%PCAL(J1)%PCAL_CMPL(IND_FRQ)) > ANC__AMP_MIN    &
     &         ) THEN
! ------------
               NP = NP + 1
!
! ------------ Is the time diff. enough to define a scan?
!
            
               IF ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX .OR.          &
     &              J1 == ANC%NUM_PCAL                         ) THEN
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
                     NUM_SCA(NS) = NP
                  ELSE
                     NUM_SCA(NS) = NP - I11
                     I11 = NP
                  END IF
               END IF
            END IF 
         END IF 

         TIM_OLD = ANC%PCAL(J1)%TIM
 410  CONTINUE
!
! --- Bug fix for cases where there is one scan detected with
!     end point at the first index
!
      IF ( NS == 1 .AND. IND_SCA(1) == 1 ) THEN
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NUM_SCA(NS) = NP
      END IF
!     
! --- We looped through all points, but no scan was defined.
! --- N.B: This is likely because the last point is an empty value.
!
      IF ( NS == 0 ) THEN
         NS = NS + 1
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NUM_SCA(NS) = NP
      END IF
!
! --- Do we have more scans than in the derived type?
!
      CALL ERR_LOG ( 0, IUER )
      IF ( NS .GT. ANC%NUM_DOO ) THEN 
         CALL IINCH ( NS, STR1 )
         CALL IINCH ( ANC%NUM_DOO, STR2 )
         IUER = -1
         CALL ERR_LOG ( 2508, IUER, 'ATP_PCAL_SCANS',                   &
     &           'Scans Counted '//TRIM(STR1)//' .ne. '//               &
     &           'Scans Declared '//TRIM(STR2) )
      END IF
! ---
      RETURN
      END SUBROUTINE !#!#!#!3
!
! -------------------------------------------------------------------------------------
!
      SUBROUTINE  ATP_SEFD_SCANS ( ANC, IND_FRQ, TIM_DIF_MAX, TIM,      &
     &                             SEFD, NS, NP, IND_SCA, NUM_SCA,      &
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
! *          TIM_DIF_MAX   = Time difference between scans        { REAL*8 } [s]     *
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
! *          NUM_SCA    =  Number of points in each scan        { INT*4 }   (NSx1)   *
! *                                                                                  *
! *          IND_ARR    =  scan end index (on derived type)     { INT*4 }   (NSx1)   *
! *                                                                                  *
! * ###  07-DEC-2022   ATP_TSYS_SCANS         v1.0 (c)  N. Habana  07-DEC-2022   ### *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'atp.i'
      TYPE        ( ANC__TYP ) :: ANC
      INTEGER*4   IUER, IND_SCA(ANC__MEPC), NS, IND_ARR(ANC__MEPC)
      INTEGER*4   J1, J2, J3, I11
      REAL*8      TIM_DIF_MAX, TIM_OLD, TIM_CUR
      CHARACTER   STR1*128, STR2*128
      INTEGER*4   IND_FRQ, NP, NUM_SCA(ANC__MEPC), NPC
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
            
               IF ( DABS(TIM_CUR - TIM_OLD) > TIM_DIF_MAX .OR.           &
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
                     NUM_SCA(NS) = NP
                  ELSE
                     NUM_SCA(NS) = NP - I11
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
         NUM_SCA(NS) = NP
      END IF
!
! --- We looped through all points, but no scan was defined.
! --- N.B: This is likely because the last point is an empty value.
!
      IF ( NS == 0 ) THEN
         NS = NS + 1
         IND_ARR(NS) = J1 - 1
         IND_SCA(NS) = NP
         NUM_SCA(NS) = NP
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
      END SUBROUTINE !#!#!2
