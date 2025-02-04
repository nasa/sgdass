      SUBROUTINE EOP_FCS ( MALO, EOP, NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine EOP_FCS pefromes EOP forecast.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   MALO ( MALO__TYPE     ) -- Object with internal parameters of      *
! *                              MALO package.                           *
! *   EOP  ( MALO_EOP__TYPE ) -- Object that gathers variables and       *
! *                              arrays related to the EOP forecast.     *
! *   NERS ( NERS__TYPE     ) -- Object with internal parameters of      *
! *                              NERS package.                           *
! *                                                                      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 07-MAR-2016     EOP_FCS  v2.8 (c)  L. Petrov  21-APR-2023  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
!
      TYPE NERS__SER_TYPE
           INTEGER*4  N_EOP
           REAL*8,    POINTER :: EOP_TIM(:)     => NULL()
           REAL*8,    POINTER :: EOP_VAL(:,:,:) => NULL()
           REAL*8,    POINTER :: EOP_SIG(:,:,:) => NULL()
           REAL*8,    POINTER :: EOP_WEI(:,:)   => NULL()
      END  TYPE NERS__SER_TYPE
      TYPE     ( NERS__SER_TYPE ) :: SER
      TYPE     ( MALO__TYPE     ), POINTER :: MALO
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      TYPE     ( NERS__TYPE     ) :: NERS
      INTEGER*4  IUER
      INTEGER*4  MP, DEG, NM_12, NM_3
      PARAMETER  ( MP    = 32*1024 )
      PARAMETER  ( DEG   =       3 )
      PARAMETER  ( NM_12 =       6 )
      PARAMETER  ( NM_3  =       6 )
      REAL*8     TIM(MP,M__EOPA), E(MP,3,2,M__EOPA), SIG(MP,3,2,M__EOPA), WEI(MP,3), &
     &           TIMA(MP), AAM(MP,M__AAM), TIM_BEG, &
     &           SPL_C(MP,3), TMP_ARR(MP), TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR
      REAL*8     SHIFT(3,2,M__EOPA), RMS(3,2,M__EOPA), ADD_WEI(3,2,M__EOPA), &
     &           RC, SIGMA, TIM_STEP, TIM_EXTR, UTC_M_TAI, TIM_LAST(M__EOPS)
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EQU_VEC(:)
      INTEGER*4, ALLOCATABLE :: IND_EQU(:)
      REAL*8,    ALLOCATABLE :: TIM_RES(:), VAL_RES(:), &
     &                          TIM3_MOD(:),  E3_MOD(:),    E3_MOD_SPL(:), &
     &                          TIM12_MOD(:), E12_MOD(:,:), E12_MOD_SPL(:,:)
      REAL*8       TIM_STEP_12, TIM_STEP_3, TIM_C04_BEG, &
     &             TAI_C04_BEG, TIM_EPS, VAL, DER, DR2, SCL, WRMS_C(3), WRMS_L(3)
      REAL*8       CNS_E3D, CNS_E3DR, CNS_E3D2, CNS_E3RR, &
     &             CNS_C04_E3_SIG, CNS_C04_E3R_SIG, CNS_C04_E12_SIG, CNS_C04_E12R_SIG, &
     &             SCL_CNS_E12, SCL_CNS_E12R, SCL_CNS_E12A, &
     &             CNS_E12D, CNS_E12DR, CNS_E12RR
      REAL*8       CNS_VAL_SIG_C, CNS_DER_SIG_C, CNS_DR2_SIG_C, EOP_STEP_C, ARG_EPS
      PARAMETER  ( CNS_VAL_SIG_C = 0.0D0 )
      PARAMETER  ( CNS_DER_SIG_C = 1.0D2 )
      PARAMETER  ( CNS_DR2_SIG_C = 1.0D2 )
      PARAMETER  ( CNS_C04_E3_SIG   =  1.D-10 )
      PARAMETER  ( CNS_C04_E3R_SIG  =  1.D-14 )
!
      PARAMETER  ( CNS_C04_E12_SIG  =  2.D-10  )
      PARAMETER  ( CNS_C04_E12R_SIG =  5.D-15 )
      PARAMETER  ( EOP_STEP_C    = 86400.0D0 )
      INTEGER*4    NP_E3_RR, NP_E12_RR, NL_OVR
      REAL*8       SCL_SIG_E3_I, SCL_SIG_E3_J, SCL_SIG_E3R_R, SCL_SIG_E3R_U, &
     &             SCL_SIG_E3R_A, SIG_E3_C, SIG_E12_C, CNS12_DER, CNS12_DR2, &
     &             WEI12_SCLV, WEI12_SCLR, WEI3_SCLV, WEI3_SCLR, &
     &             CNS3_DER, CNS3_DR2, EOPS__MAX_AC, TIM_L_BEG, TIM_L_STEP, TIM_PAST_C
      CHARACTER    EANG_MOD*24
      INTEGER*4    KM_12, KM_3
      PARAMETER  ( EANG_MOD = 'EANG v  2.7   2021.08.24' )
      PARAMETER  ( TIM_STEP_12 = 36.0D0*3600.0D0 ) 
      PARAMETER  ( TIM_STEP_3  = 36.0D0*3600.0D0 ) 
      PARAMETER  ( TIM_EPS     = 10.0D0 )
      PARAMETER  ( SCL         = 1.D9    )
      PARAMETER  ( CNS_E3D       =  1.0D-7  )
      PARAMETER  ( CNS_E3DR      =  1.0D-10 )
      PARAMETER  ( CNS_E3D2      =  5.0D-19 )
!
      PARAMETER  ( CNS_E3RR      =  5.0D-19 )
      PARAMETER  ( NP_E3_RR      =  8       ) ! number of knots for constraints on E3 accelration
      PARAMETER  ( SCL_SIG_E3_I  =  0.5D0   )
      PARAMETER  ( SCL_SIG_E3_J  =  1.5D0   )
      PARAMETER  ( SCL_SIG_E3R_R =  2.0D0   )
      PARAMETER  ( SCL_SIG_E3R_U =  0.7D0   )
      PARAMETER  ( SCL_SIG_E3R_A =  5.0D0   )
      PARAMETER  ( WEI3_SCLV     =  1.0D0   )
      PARAMETER  ( WEI3_SCLR     = 10.0D0   )
      PARAMETER  ( CNS3_DER      =  1.0D-14 )
      PARAMETER  ( CNS3_DR2      =  7.0D-21 )
      PARAMETER  ( SIG_E3_C      =  1.0D-9  )
      PARAMETER  ( KM_3          =  3       )
      PARAMETER  ( EOPS__MAX_AC  = 72.0D0*86400.0D0 ) ! maximim allowed interval between the end of C04 and AAM
!!      PARAMETER  ( TIM_PAST_C    = 1.0D0*YEAR__TO__SEC )
      PARAMETER  ( TIM_PAST_C    = 90.0D0*86400.0D0 ) 
!
      PARAMETER  ( SCL_CNS_E12   = 4.0D0    )
      PARAMETER  ( SCL_CNS_E12R  = 4.0D0    )
      PARAMETER  ( SCL_CNS_E12A  = 1.0D2    )
      PARAMETER  ( CNS_E12RR     = 2.0D-18  )
      PARAMETER  ( NP_E12_RR     =   24     ) ! number of knots for constraints on E12 accelration
      PARAMETER  ( CNS_E12D      = 1.D-7    ) ! deterministic model
      PARAMETER  ( CNS_E12DR     = 1.D-10   ) ! deterministic model rate
      PARAMETER  ( SIG_E12_C     = 3.0D-10  )
      PARAMETER  ( CNS12_DER     = 1.0D-14  )
      PARAMETER  ( CNS12_DR2     = 7.0D-21  )
      PARAMETER  ( WEI12_SCLV    = 1.0      )
      PARAMETER  ( WEI12_SCLR    = 3.0      )
      PARAMETER  ( KM_12         = 12       )
      PARAMETER  ( NL_OVR        = 10       )
!
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, &
     &           J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, J37, J38, &
     &           J39, J40, J41, J42, J43, J44, J45, J46, &
     &           IL, LD, IK, IM, L_EQU, IND, KN, &
     &           MJD, NP(M__EOPA), L_PAR, L_PA2, IPL, NL, &
     &           IND_EOP_12(M__EOPA), IND_EOP_3(M__EOPA), NC_EOP_12, NC_EOP_3, &
     &           NT, MJD_C04_BEG, IER
      LOGICAL*1  FL_HOLE 
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, FSPL8, DSPL8, &
     &                       D2SPL8, GET_UTC, EBSPL_VAL_R8, EBSPL_DER_R8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
! --- Initialization
!
      IPL = 0
      NC_EOP_12 = 5
      IND_EOP_12(1) = EOPS__C
      IND_EOP_12(2) = EOPS__U
      IND_EOP_12(3) = EOPS__R
      IND_EOP_12(4) = EOPS__A
      IND_EOP_12(5) = EOPS__L
!
      NC_EOP_3 = 7
      IND_EOP_3(1) = EOPS__C
      IND_EOP_3(2) = EOPS__I
      IND_EOP_3(3) = EOPS__J
      IND_EOP_3(4) = EOPS__R
      IND_EOP_3(5) = EOPS__U
      IND_EOP_3(6) = EOPS__A
      IND_EOP_3(7) = EOPS__L
!
! --- Set the initial date of C04. We discard the dates before that
!
      CALL DATE_TO_TIME ( '1976.01.01_00:00:00.0', MJD_C04_BEG, TAI_C04_BEG, IER )
      TIM_C04_BEG = (MJD_C04_BEG - J2000__MJD)*86400.0D0 - TAI_C04_BEG
      SER%N_EOP = 0
!
! --- Check, is there a hole in the C04 series
!
      FL_HOLE = .FALSE.
      DO 410 J1=1,EOP%EOPS(EOPS__C)%NP
         IF ( EOP%EOPS(EOPS__C)%SER(J1)%TIM .GE. TIM_C04_BEG  ) THEN
              SER%N_EOP = SER%N_EOP + 1
         END IF
         IF ( J1 > 1 ) THEN
              IF ( DABS ( EOP%EOPS(EOPS__C)%SER(J1)%TIM - EOP%EOPS(EOPS__C)%SER(J1-1)%TIM - 86400.0D0 ) > 0.01D0 ) THEN
!
! ---------------- Shows the hole
!
                   WRITE ( 6, * ) ' J1= ',J1, ' DIF = ', EOP%EOPS(EOPS__C)%SER(J1)%TIM - EOP%EOPS(EOPS__C)%SER(J1-1)%TIM
                   FL_HOLE = .TRUE.
              END IF
         END IF
 410  CONTINUE 
      IF ( FL_HOLE ) THEN
           CALL ERR_LOG ( 6661, IUER, 'EOP_FCS', 'There is a hole in the EOP series C04' )
           RETURN 
      END IF
!
      IF ( EOP%EOPS(EOPS__C)%SER(EOP%EOPS(EOPS__C)%NP)%TIM < &
     &     EOP%AAM%TIM(EOP%AAM%NP) - EOPS__MAX_AC            ) THEN
           CALL ERR_LOG ( 6662, IUER, 'EOP_FCS', 'EOP series C04 ends too early' )
           RETURN 
      END IF
!
      IF ( EOP%EOPS(EOPS__L)%SER(EOP%EOPS(EOPS__L)%NP)%TIM < &
     &     EOP%AAM%TIM(EOP%AAM%NP) - EOPS__MAX_AC            ) THEN
           CALL ERR_LOG ( 6663, IUER, 'EOP_FCS', 'EOP series LTP ends too early' )
           WRITE ( 6, * ) 'a1= ', EOP%EOPS(EOPS__L)%SER(EOP%EOPS(EOPS__L)%NP)%TIM, &
     &                    ' date: ', TIM_TO_DATE ( EOP%EOPS(EOPS__L)%SER(EOP%EOPS(EOPS__L)%NP)%TIM, IER )
           WRITE ( 6, * ) 'a2= ', EOP%AAM%TIM(EOP%AAM%NP), ' date: ', TIM_TO_DATE ( EOP%AAM%TIM(EOP%AAM%NP), IER )
           WRITE ( 6, * ) 'a3= ', EOPS__MAX_AC
           WRITE ( 6, * ) 'a4= ', EOP%EOPS(EOPS__L)%NP
           WRITE ( 6, * ) 'a5= ', TRIM(EOP%CONF%FIL_EOP(EOPS__L))
           RETURN 
      END IF
!
! --- Allocate memory
!
      ALLOCATE ( SER%EOP_TIM(SER%N_EOP),     STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6664, IUER, 'EOP_FCS', 'Error in allocating '// &
     &         'dynamic memory for array SER%EOP_TIM' )
           RETURN 
      END IF
      ALLOCATE ( SER%EOP_VAL(SER%N_EOP,3,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6665, IUER, 'EOP_FCS', 'Error in allocating '// &
     &         'dynamic memory for array SER%EOP_VAL' )
           RETURN 
      END IF
      ALLOCATE ( SER%EOP_SIG(SER%N_EOP,3,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6666, IUER, 'EOP_FCS', 'Error in allocating '// &
     &         'dynamic memory for array SER%EOP_SIG' )
           RETURN 
      END IF
      ALLOCATE ( SER%EOP_WEI(SER%N_EOP,3),   STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6667, IUER, 'EOP_FCS', 'Error in allocating '// &
     &         'dynamic memory for array SER%EOP_WEI' )
           RETURN 
      END IF
!
! --- Compute the number of knots for C04 B-spline expansion
! --- The sequence of knots is equidistant with the step 1 day -- the same as IERS C04
! ---            with the following exceptions:
! ---            1) The first knot is at NERS__ARG_EPS before the first EOP point
! ---            2) The gap between the 1nd and last knot is twice larger plus NERS__ARG_EPS 
! ---            3) The gap between the last but one and the and last knot 
! ---               is twice larger plus NERS__ARG_EPS
! ---            4) The last knot is at NERS__ARG_EPS after the last EOP point
!
      NERS%FCS%NC = NINT ( (EOP%EOPS(EOPS__C)%SER(EOP%EOPS(EOPS__C)%NP)%TIM - TIM_C04_BEG)/EOP_STEP_C ) - 1
      ALLOCATE ( NERS%FCS%ARG_C(NERS%FCS%NC),          STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6668, IUER, 'EOP_FCS', 'Errror in allocating '// &
     &         'dynamic memory for array NERS%FCS%ARG_C' )
           RETURN 
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6669, IUER, 'EOP_FCS', 'Errror in allocating '// &
     &         'dynamic memory for array NERS%FCS%BSPL_C' )
           RETURN 
      END IF
!
! --- Fill arrauys of nodes for C04 interpolation
!
      KN = 0
      DO 420 J2=1,NERS%FCS%NC + 2
         IF ( J2 == 2               ) GOTO 420
         IF ( J2 == NERS%FCS%NC + 1 ) GOTO 420
         KN = KN + 1
         NERS%FCS%ARG_C(KN) = TIM_C04_BEG + (J2-1)*EOP_STEP_C
 420  CONTINUE 
      NERS%FCS%ARG_C(1)           = NERS%FCS%ARG_C(1)           - NERS__ARG_EPS
      NERS%FCS%ARG_C(NERS%FCS%NC) = NERS%FCS%ARG_C(NERS%FCS%NC) + NERS__ARG_EPS
!
! --- Load EOPs, their rate and uncertainties into arrays E, SIG
!
      TIM_BEG = EOP%EOPS(EOPS__C)%SER(EOP%EOPS(EOPS__C)%NP)%TIM - TIM_PAST_C
      NP = 0
      IND = 0
!
! --- Cycle over EOP time series. AAM series is considered as the EOP in this context
!
      DO 430 J3=1,M__EOPS
         IF ( EOP%EOPS(J3)%NP > 0 ) THEN
              TIM_LAST(J3) = EOP%EOPS(J3)%SER(EOP%EOPS(J3)%NP)%TIM
            ELSE 
              TIM_LAST(J3) = 0.0D0
         END IF
         DO 440 J4=1,EOP%EOPS(J3)%NP
            IF ( EOP%EOPS(J3)%SER(J4)%TIM  .GE. TIM_BEG ) THEN
                 NP(J3)  = NP(J3) + 1
                 TIM(NP(J3),J3) = EOP%EOPS(J3)%SER(J4)%TIM 
                 DO 450 J5=1,3
                    E(NP(J3),J5,1,J3)   = EOP%EOPS(J3)%SER(J4)%E(J5)
                    E(NP(J3),J5,2,J3)   = EOP%EOPS(J3)%SER(J4)%ER(J5)  ! Time derivative
                    SIG(NP(J3),J5,1,J3) = EOP%EOPS(J3)%SER(J4)%DE(J5)
                    SIG(NP(J3),J5,2,J3) = EOP%EOPS(J3)%SER(J4)%DER(J5) ! Error of time derivative
 450             CONTINUE 
!
! -------------- Subtract the contribution of zonal tides
!
                 MJD = TIM(NP(J3),J3)/86400.0D0 + J2000__MJD
                 TAI = TIM(NP(J3),J3) - (MJD - J2000__MJD)*86400.0D0
                 IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_D93 ) THEN
                       CALL E3ZT_DICKMAN1993  ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
                    ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_RE2014 ) THEN
                       CALL E3ZT_RE2014 ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
                    ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_NONE   ) THEN
                       E3_HAR     = 0.0D0
                       E3_DOT_HAR = 0.0D0
                       E3_DT2_HAR = 0.0D0
                 END IF
                 E(NP(J3),3,1,J3) = E(NP(J3),3,1,J3) - E3_HAR
                 E(NP(J3),3,2,J3) = E(NP(J3),3,2,J3) - E3_DOT_HAR
            END IF
            IF ( J3 == EOPS__C  .AND.  EOP%EOPS(J3)%SER(J4)%TIM .GE. TIM_C04_BEG  ) THEN
!
! -------------- Load C04 in a separate slot
!
                 IND = IND + 1
                 SER%EOP_TIM(IND) = EOP%EOPS(J3)%SER(J4)%TIM 
                 DO 460 J6=1,3
                    SER%EOP_VAL(IND,J6,1) = EOP%EOPS(J3)%SER(J4)%E(J6)
                    SER%EOP_VAL(IND,J6,2) = EOP%EOPS(J3)%SER(J4)%ER(J6)  ! Time derivative
                    SER%EOP_SIG(IND,J6,1) = EOP%EOPS(J3)%SER(J4)%DE(J6)
                    SER%EOP_SIG(IND,J6,2) = EOP%EOPS(J3)%SER(J4)%DER(J6) ! Error of time derivative
                    SER%EOP_WEI(IND,J6)   = 1.0D0/SER%EOP_SIG(IND,J6,1) 
 460             CONTINUE 
            END IF
 440     CONTINUE 
         IF ( NP(J3) < 3 ) THEN
              CALL CLRCH ( STR  ) 
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( NP(J3), STR )
              STR1 = TIM_TO_DATE ( TIM_BEG,  IER )
!@              CALL ERR_LOG ( 6670, IUER, 'EOP_FCS', 'EOP series '// &
!@     &             EOPS__NAME(J3)//' from file '//TRIM(EOP%CONF%FIL_EOP(J3))// &
!@     &             ' are too short: it has '//TRIM(STR)//' points after '//STR1 )
!@              RETURN 
         END IF
 430  CONTINUE 
!
! --- Load AAM into arrays E, SIG, in slots of time derivatives.
! --- Formal uncertainties put in SIG are wild guesses
!
      NP(EOPS__A) = 0
      DO 470 J7=1,EOP%AAM%NP
         IF ( EOP%AAM%TIM(J7) .GE. TIM_BEG ) THEN
              NP(EOPS__A) = NP(EOPS__A) + 1
              TIMA(NP(EOPS__A))        = EOP%AAM%TIM(J7)
              TIM(NP(EOPS__A),EOPS__A) = EOP%AAM%TIM(J7)
              DO 480 J8=1,M__AAM
                 AAM(NP(EOPS__A),J8)  = EOP%AAM%VAL(J7,J8)
                 IF ( J8 == MALO__XI1_IB ) THEN
!
! ------------------- At the moment we ignore wind term and use only matter term for E1 component
!
                      E(NP(EOPS__A),1,2,EOPS__A)   = MALO__X1_MAT_COEF*EOP%AAM%VAL(J7,MALO__I31_IB)*OM__EAR ! mass term
                      SIG(NP(EOPS__A),1,1,EOPS__A) = 0.0D0
                      SIG(NP(EOPS__A),1,2,EOPS__A) = 1.D-15
                    ELSE IF ( J8 == MALO__XI2_IB ) THEN
!
! ------------------- At the moment we ignore wind term and use only matter term for E2 component
!
                      E(NP(EOPS__A),2,2,EOPS__A)   = MALO__X1_MAT_COEF*EOP%AAM%VAL(J7,MALO__I32_IB)*OM__EAR ! mass term
                      SIG(NP(EOPS__A),1,1,EOPS__A) = 0.0D0
                      SIG(NP(EOPS__A),2,2,EOPS__A) = 1.D-15
                    ELSE IF ( J8 == MALO__XI3_IB ) THEN
!
! ------------------- Use both wind and matter term for E3 component
!
                      E(NP(EOPS__A),3,2,EOPS__A)   = EOP%AAM%VAL(J7,J8)*OM__EAR
                      SIG(NP(EOPS__A),3,1,EOPS__A) = 0.0D0
                      SIG(NP(EOPS__A),3,2,EOPS__A) = 3.5D-14
                 END IF
 480          CONTINUE 
         END IF
 470  CONTINUE 
!
! --- Compute spline coefficients for 3 components of  EOPS__C.
! --- We do it two times: the first time for the time range of the 
! --- forcast ( 1 year ), the second time for the full time range.
! --- The first spline coefficients are for internal use only
! 
      DO 490 J9=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,J9,1,EOPS__C), &
     &                      0.0D0, 0.0D0, SPL_C(1,J9), TMP_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6671, IUER, 'EOP_FCS', 'Error in computing '// &
     &            'coeficients of spline interpolation of EOPS_C' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL EBSPL_WLSQ_CNS3 ( SER%N_EOP, SER%EOP_TIM, &
     &                          SER%EOP_VAL(1,J9,1), SER%EOP_WEI(1,J9), &
     &                          NERS%FCS%NC, NERS__MDEG, NERS%FCS%ARG_C,       &
     &                          NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,J9), &
     &                          CNS_VAL_SIG_C, CNS_DER_SIG_C, CNS_DR2_SIG_C, &
     &                          WRMS_C(J9), IER )
!
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J9, STR )
              CALL ERR_LOG ( 6672, IUER, 'EOP_FCS', 'Error in computing '// &
     &            'coeficients of spline interpolation of EOP C04 comp '//TRIM(STR) )
              RETURN 
         END IF
 490  CONTINUE 
!
! --- Compute time derivatives of E1, E2 from C04
!
      DO 5100 J10=1,SER%N_EOP
         IF ( J10 == 1 ) THEN
              SER%EOP_VAL(J10,1,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10) + TIM_EPS, &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,1) )
              SER%EOP_VAL(J10,2,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10) + TIM_EPS, &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,2) )
            ELSE IF ( J10 == SER%N_EOP ) THEN
              SER%EOP_VAL(J10,1,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10) - TIM_EPS, &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,1) )
              SER%EOP_VAL(J10,2,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10) - TIM_EPS, &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,2) )
            ELSE
              SER%EOP_VAL(J10,1,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10), &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,1) )
              SER%EOP_VAL(J10,2,2) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, SER%EOP_TIM(J10), &
     &                                              NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,2) )
         END IF
         EOP%EOPS(EOPS__C)%SER(J10)%ER(1) = SER%EOP_VAL(J10,1,2)
         EOP%EOPS(EOPS__C)%SER(J10)%ER(2) = SER%EOP_VAL(J10,2,2)
 5100 CONTINUE 
!
      DO 5110 J11=1,NP(EOPS__C)
         IF ( J11 == 1 ) THEN
              IM = J11
              E(J11,1,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C) + TIM_EPS, NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,1,1,EOPS__C), IM, SPL_C(1,1) )
              E(J11,2,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C) + TIM_EPS, NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,2,1,EOPS__C), IM, SPL_C(1,2) )
            ELSE IF ( J11 == NP(EOPS__C) ) THEN
              IM = J11 - 1
              E(J11,1,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C) - TIM_EPS, NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,1,1,EOPS__C), IM, SPL_C(1,1) )
              E(J11,2,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C) - TIM_EPS, NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,2,1,EOPS__C), IM, SPL_C(1,2) )
            ELSE
              IM = J11
              E(J11,1,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C), NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,1,1,EOPS__C), IM, SPL_C(1,1) )
              E(J11,2,2,EOPS__C) = DSPL8 ( TIM(J11,EOPS__C), NP(EOPS__C), TIM(1,EOPS__C), &
     &                                     E(1,2,1,EOPS__C), IM, SPL_C(1,2) )
         END IF
 5110 CONTINUE 
!
! --- Compare EOP and AAM against EOPC. Compute shift, rms, and additive weights
!
      ADD_WEI(1:3,1:2,EOPS__C) = 0.0D0
      SHIFT(1:3,1:2,EOPS__C)   = 0.0D0
      RMS(1:3,1:2,EOPS__C)     = 0.0D0
!!      IPL = EOP%IVRB
      IPL = 0
!
! --- R-series, 1st component
!
      CALL EOP_CMPR ( 1, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,1,1,EOPS__R), SIG(1,1,1,EOPS__R), &
     &                   SHIFT(1,1,EOPS__R), RMS(1,1,EOPS__R), ADD_WEI(1,1,EOPS__R), &
     &                   EOPS__R, IPL )
!
! ---           2nd component
!
      CALL EOP_CMPR ( 2, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,2,1,EOPS__R), SIG(1,2,1,EOPS__R), &
     &                   SHIFT(2,1,EOPS__R), RMS(2,1,EOPS__R), ADD_WEI(2,1,EOPS__R), &
     &                   EOPS__R, IPL )
!
! ---           3rd component
!
      CALL EOP_CMPR ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,3,1,EOPS__R), SIG(1,3,1,EOPS__R), &
     &                   SHIFT(3,1,EOPS__R), RMS(3,1,EOPS__R), ADD_WEI(3,1,EOPS__R), &
     &                   EOPS__R, IPL )
!
! ---           time derivative of the 1st component
!
      CALL EOP_CMPR ( 4, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,1,2,EOPS__R), SIG(1,1,2,EOPS__R), &
     &                   SHIFT(1,2,EOPS__R), RMS(1,2,EOPS__R), ADD_WEI(1,2,EOPS__R), &
     &                   EOPS__R, IPL )
!
! ---           time derivative of the 2nd component
!
      CALL EOP_CMPR ( 5, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,2,2,EOPS__R), SIG(1,2,2,EOPS__R), &
     &                   SHIFT(2,2,EOPS__R), RMS(2,2,EOPS__R), ADD_WEI(2,2,EOPS__R), &
     &                   EOPS__R, IPL )
!
! ---           time derivative of the 3rd component
!
      CALL EOP_CMPR ( 6, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__R), TIM(1,EOPS__R), E(1,3,2,EOPS__R), SIG(1,3,2,EOPS__R), &
     &                   SHIFT(3,2,EOPS__R), RMS(3,2,EOPS__R), ADD_WEI(3,2,EOPS__R), &
     &                   EOPS__R, IPL )
!
! --- U-series, 1st component
!
      CALL EOP_CMPR ( 1, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,1,1,EOPS__U), SIG(1,1,1,EOPS__U), &
     &                   SHIFT(1,1,EOPS__U), RMS(1,1,EOPS__U), ADD_WEI(1,1,EOPS__U), &
     &                   EOPS__U, IPL )
!
! ---           2nd component
!
      CALL EOP_CMPR ( 2, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,2,1,EOPS__U), SIG(1,2,1,EOPS__U), &
     &                   SHIFT(2,1,EOPS__U), RMS(2,1,EOPS__U), ADD_WEI(2,1,EOPS__U), &
     &                   EOPS__U, IPL )
!
! ---           3rd component
!
      CALL EOP_CMPR ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,3,1,EOPS__U), SIG(1,3,1,EOPS__U), &
     &                   SHIFT(3,1,EOPS__U), RMS(3,1,EOPS__U), ADD_WEI(3,1,EOPS__U), &
     &                   EOPS__U, IPL )
!
! ---           time derivative of the 1st component
!
      CALL EOP_CMPR ( 4, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,1,2,EOPS__U), SIG(1,1,2,EOPS__U), &
     &                   SHIFT(1,2,EOPS__U), RMS(1,2,EOPS__U), ADD_WEI(1,2,EOPS__U), &
     &                   EOPS__U, IPL )
!
! ---           time derivative of the 2nd component
!
      CALL EOP_CMPR ( 5, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,2,2,EOPS__U), SIG(1,2,2,EOPS__U), &
     &                   SHIFT(2,2,EOPS__U), RMS(2,2,EOPS__U), ADD_WEI(2,2,EOPS__U), &
     &                   EOPS__U, IPL )
!
! ---           time derivative of the 3rd component
!
      CALL EOP_CMPR ( 6, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__U), TIM(1,EOPS__U), E(1,3,2,EOPS__U), SIG(1,3,2,EOPS__U), &
     &                   SHIFT(3,2,EOPS__U), RMS(3,2,EOPS__U), ADD_WEI(3,2,EOPS__U), &
     &                   EOPS__U, IPL )
!
! --- F-series, 1st component
!
      CALL EOP_CMPR ( 1, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,1,1,EOPS__F), SIG(1,1,1,EOPS__F), &
     &                   SHIFT(1,1,EOPS__F), RMS(1,1,EOPS__F), ADD_WEI(1,1,EOPS__F), &
     &                   EOPS__F, IPL )
!
! ---           2nd comonent
!
      CALL EOP_CMPR ( 2, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,2,1,EOPS__F), SIG(1,2,1,EOPS__F), &
     &                   SHIFT(2,1,EOPS__F), RMS(2,1,EOPS__F), ADD_WEI(2,1,EOPS__F), &
     &                   EOPS__F, IPL )
!
! ---           3rd component
!
      CALL EOP_CMPR ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,3,1,EOPS__F), SIG(1,3,1,EOPS__F), &
     &                   SHIFT(3,1,EOPS__F), RMS(3,1,EOPS__F), ADD_WEI(3,1,EOPS__F), &
     &                   EOPS__F, IPL )
!
! ---           time derivative of the 1st component
!
      CALL EOP_CMPR ( 4, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,1,2,EOPS__F), SIG(1,1,2,EOPS__F), &
     &                   SHIFT(1,2,EOPS__F), RMS(1,2,EOPS__F), ADD_WEI(1,2,EOPS__F), &
     &                   EOPS__F, IPL )
!
! ---           time derivative of the 2nd component
!
      CALL EOP_CMPR ( 5, NP(EOPS__C), TIM(1,EOPS__C), E(1,2,1,EOPS__C), SPL_C(1,2), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,2,2,EOPS__F), SIG(1,2,2,EOPS__F), &
     &                   SHIFT(2,2,EOPS__F), RMS(2,2,EOPS__F), ADD_WEI(2,2,EOPS__F), &
     &                   EOPS__F, IPL )
!
! ---           time derivative of the 3rd component
!
      CALL EOP_CMPR ( 6, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__F), TIM(1,EOPS__F), E(1,3,2,EOPS__F), SIG(1,3,2,EOPS__F), &
     &                   SHIFT(3,2,EOPS__F), RMS(3,2,EOPS__F), ADD_WEI(3,2,EOPS__F), &
     &                   EOPS__F, IPL )
!
! --- I-series, 3rd component
!
      CALL EOP_CMPR ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                   NP(EOPS__I), TIM(1,EOPS__I), E(1,3,1,EOPS__I), SIG(1,3,1,EOPS__I), &
     &                   SHIFT(3,1,EOPS__I), RMS(3,1,EOPS__I), ADD_WEI(3,1,EOPS__I), &
     &                   EOPS__I, IPL )
!
! --- J-series, 3rd component
!
!!  ipl = 32
      IF ( NP(EOPS__J) > 0 ) THEN
           CALL EOP_CMPR ( 3, NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), SPL_C(1,3), &
     &                     NP(EOPS__J), TIM(1,EOPS__J), E(1,3,1,EOPS__J), SIG(1,3,1,EOPS__J), &
     &                     SHIFT(3,1,EOPS__J), RMS(3,1,EOPS__J), ADD_WEI(3,1,EOPS__J), &
     &                     EOPS__J, IPL )
         ELSE 
            SHIFT(3,1,EOPS__J)   = 0.0D0
            RMS(3,1,EOPS__J)     = 0.0D0
            ADD_WEI(3,1,EOPS__J) = 0.0D0
      END IF
!!  ipl = 0
!
! --- AAM-seris, time derivative, 1st component
!
      CALL AAM_CMPR ( 1, MP, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__A), TIM(1,EOPS__A), E(1,1,2,EOPS__A), &
     &                   SHIFT(1,2,EOPS__A), RMS(1,2,EOPS__A), ADD_WEI(1,2,EOPS__A), &
     &                   IPL )
!
! ---            time derivative, 2nd component
!
      CALL AAM_CMPR ( 2, MP, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__A), TIM(1,EOPS__A), E(1,1,2,EOPS__A), &
     &                   SHIFT(2,2,EOPS__A), RMS(2,2,EOPS__A), ADD_WEI(2,2,EOPS__A), &
     &                   IPL )
!
! ---            time derivative, 3rd component
!
      CALL AAM_CMPR ( 3, MP, NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), SPL_C(1,1), &
     &                   NP(EOPS__A), TIM(1,EOPS__A), E(1,1,2,EOPS__A), &
     &                   SHIFT(3,2,EOPS__A), RMS(3,2,EOPS__A), ADD_WEI(3,2,EOPS__A), &
     &                   IPL )
      IF ( EOP%IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) 'E1 ', EOPS__NAME(EOPS__U), NP(EOPS__U), SHIFT(1,1,EOPS__U), RMS(1,1,EOPS__U), ADD_WEI(1,1,EOPS__U)
           WRITE ( 6, 210 ) 'E1 ', EOPS__NAME(EOPS__R), NP(EOPS__R), SHIFT(1,1,EOPS__R), RMS(1,1,EOPS__R), ADD_WEI(1,1,EOPS__R)
           WRITE ( 6, 210 ) 'E1R', EOPS__NAME(EOPS__A), NP(EOPS__A), SHIFT(1,2,EOPS__A), RMS(1,2,EOPS__A), ADD_WEI(1,2,EOPS__A)
           WRITE ( 6, 210 ) ' '
!
           WRITE ( 6, 210 ) 'E2 ', EOPS__NAME(EOPS__U), NP(EOPS__U), SHIFT(2,1,EOPS__U), RMS(2,1,EOPS__U), ADD_WEI(2,1,EOPS__U)
           WRITE ( 6, 210 ) 'E2 ', EOPS__NAME(EOPS__R), NP(EOPS__R), SHIFT(2,1,EOPS__R), RMS(2,1,EOPS__R), ADD_WEI(2,1,EOPS__R)
           WRITE ( 6, 210 ) 'E2R', EOPS__NAME(EOPS__A), NP(EOPS__A), SHIFT(2,2,EOPS__A), RMS(2,2,EOPS__A), ADD_WEI(2,2,EOPS__A)
           WRITE ( 6, 210 ) ' '
!
           WRITE ( 6, 210 ) 'E3 ', EOPS__NAME(EOPS__J), NP(EOPS__J), SHIFT(3,1,EOPS__J), RMS(3,1,EOPS__J), ADD_WEI(3,1,EOPS__J)
           WRITE ( 6, 210 ) 'E3 ', EOPS__NAME(EOPS__I), NP(EOPS__I), SHIFT(3,1,EOPS__I), RMS(3,1,EOPS__I), ADD_WEI(3,1,EOPS__I)
           WRITE ( 6, 210 ) 'E3 ', EOPS__NAME(EOPS__U), NP(EOPS__U), SHIFT(3,1,EOPS__U), RMS(3,1,EOPS__U), ADD_WEI(3,1,EOPS__U)
           WRITE ( 6, 210 ) 'E3 ', EOPS__NAME(EOPS__R), NP(EOPS__R), SHIFT(3,1,EOPS__R), RMS(3,1,EOPS__R), ADD_WEI(3,1,EOPS__R)
!
           WRITE ( 6, 210 ) 'E3R', EOPS__NAME(EOPS__U), NP(EOPS__U), SHIFT(3,2,EOPS__U), RMS(3,2,EOPS__U), ADD_WEI(3,2,EOPS__U)
           WRITE ( 6, 210 ) 'E3R', EOPS__NAME(EOPS__R), NP(EOPS__R), SHIFT(3,2,EOPS__R), RMS(3,2,EOPS__R), ADD_WEI(3,2,EOPS__R)
           WRITE ( 6, 210 ) ' '
!
           CALL FLUSH ( 6 )
 210       FORMAT ( 'EOP_FCS: ', A, 2X, A, 2X, I4, ' Shift: ', 1PD12.4, ' rms: ', 1PD12.4, ' add: ', 1PD12.4 )
!
           WRITE ( 6, * ) 'EOP_C   LAST=     ', TIM(NP(EOPS__C),EOPS__C), TIM_TO_DATE ( TIM(NP(EOPS__c),EOPS__C), IER )
           WRITE ( 6, * ) 'EOP_U   LAST=     ', TIM(NP(EOPS__U),EOPS__U), TIM_TO_DATE ( TIM(NP(EOPS__U),EOPS__U), IER )
           WRITE ( 6, * ) 'AAM_TIM_LAST=     ', EOP%AAM%TIM(EOP%AAM%NP),  TIM_TO_DATE ( EOP%AAM%TIM(EOP%AAM%NP),  IER )
      END IF
!
! --- Compute array of arguments for spline coefficients of E3 
! --- for forecast constituents
!
      NERS%FCS%NK_3 = NINT ( (TIMA(NP(EOPS__A)) - TIM_BEG)/TIM_STEP_3 )
      ALLOCATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3) )
      DO 4100 J10=1,NERS%FCS%NK_3
         NERS%FCS%ARG_3(J10) = TIM_BEG + (J10-1)*TIM_STEP_3
 4100 CONTINUE 
      NERS%FCS%ARG_3(NERS%FCS%NK_3) = TIMA(NP(EOPS__A))
!
! --- Allocate dynamic memory for normal matrix and normal vector
! --- or the EOP forecast
!
      L_PAR = NERS%FCS%NK_3 + DEG - 1
      L_PA2 = (L_PAR*(L_PAR+1))/2
      ALLOCATE ( NOR_MAT(L_PA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PA2, STR )
           CALL ERR_LOG ( 6673, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NOR_MAT' )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6674, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NOR_VEC' )
           RETURN 
      END IF
      ALLOCATE ( EQU_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6675, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array EQU_VEC' )
           RETURN 
      END IF
      ALLOCATE ( IND_EQU(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6676, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array IND_VEC' )
           RETURN 
      END IF
!
! --- Allocate memory for the final results: B-spline for the 3rd component
! --- of the EOP forecast
!
      ALLOCATE ( NERS%FCS%BSPL_E3(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6677, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NERS%FCS%BSPL_E3' )
           RETURN 
      END IF
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
! --- Allocate memory for the the deterministic model of the E3 component
!
      ALLOCATE ( TIM3_MOD(NM_3),  STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_3, STR )
           CALL ERR_LOG ( 6678, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array '// &
     &          'TIM3_MOD' )
           RETURN 
      END IF
      ALLOCATE ( E3_MOD(NM_3),  STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_3, STR )
           CALL ERR_LOG ( 6679, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array E3_MOD' )
           RETURN 
      END IF
      ALLOCATE ( E3_MOD_SPL(NM_3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_3, STR )
           CALL ERR_LOG ( 6680, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array E3R_MOD_SPL' )
           RETURN 
      END IF
!
! --- Compute the deterministic model for the E3 component. The model is
! --- represented in a form of spline
!
      CALL ERR_PASS   ( IUER, IER )
      IF ( NP(EOPS__J) > 0 ) THEN
           CALL EOP_E3_MOD ( NP(EOPS__J), TIM(1,EOPS__J), E(1,3,1,EOPS__J), SIG(1,3,1,EOPS__J), &
     &                       NP(EOPS__U), TIM(1,EOPS__U), E(1,3,2,EOPS__U), SIG(1,3,2,EOPS__U), &
     &                       NM_3,        TIM3_MOD,       E3_MOD,           E3_MOD_SPL, &
     &                       TIM_BEG,     TIM(NP(EOPS__A),EOPS__A), &
     &                       WEI3_SCLV, WEI3_SCLR, CNS3_DER, CNS3_DR2, EOP%IVRB, IER )
         ELSE 
           CALL EOP_E3_MOD ( NP(EOPS__I), TIM(1,EOPS__I), E(1,3,1,EOPS__I), SIG(1,3,1,EOPS__I), &
     &                       NP(EOPS__U), TIM(1,EOPS__U), E(1,3,2,EOPS__U), SIG(1,3,2,EOPS__U), &
     &                       NM_3,        TIM3_MOD,       E3_MOD,           E3_MOD_SPL, &
     &                       TIM_BEG,     TIM(NP(EOPS__A),EOPS__A), &
     &                       WEI3_SCLV, WEI3_SCLR, CNS3_DER, CNS3_DR2, EOP%IVRB, IER )
      END IF
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6681, IUER, 'EOP_FCS', 'Failure in computing '// &
     &         'deterministic model for E3 using EOPS__C series' )
           RETURN 
      END IF
!
      IF ( EOP%IVRB == 53 ) THEN
           CALL PLOT_EOP_MOD ( 3, NP(EOPS__I), TIM(1,EOPS__I), E(1,3,1,EOPS__I), SIG(1,3,1,EOPS__I), &
     &                         NM_3,  TIM3_MOD, E3_MOD, E3_MOD_SPL, IER )
      END IF
!
! --- Build the normal system of equations for computation of the coefficients
! --- for expansion of the 3rd component of the Euler angle vector into B-spline 
! --- basis
!
      DO 4110 J11=1,NC_EOP_3
         DO 4120 J12=1,NP(IND_EOP_3(J11))
            IF ( J12 == 1 ) THEN
                 IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J12,IND_EOP_3(J11)) + TIM_EPS )
               ELSE IF ( J12 == NP(IND_EOP_3(J11)) ) THEN
                 IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J12,IND_EOP_3(J11)) - TIM_EPS )
               ELSE
                 IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J12,IND_EOP_3(J11)) )
            END IF
            IF ( IK .LT. 1             ) GOTO 4120
            IF ( IK .GE. NERS%FCS%NK_3 ) GOTO 4120
            IF ( IND_EOP_3(J11) .EQ. EOPS__C  .OR. &
     &           IND_EOP_3(J11) .EQ. EOPS__J  .OR. &
     &           IND_EOP_3(J11) .EQ. EOPS__I       ) THEN
!
! -------------- Form equation for the E3 angle
!
                 L_EQU = 0
                 DO 4130 J13=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J13, TIM(J12,IND_EOP_3(J11)) )
                    IND_EQU(L_EQU) = IK + J13 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J11= ', INT2(J11), ' J12= ', J12, &
     &                                  ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                  ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                         CALL ERR_LOG ( 6682, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4130            CONTINUE 
                 IF ( IND_EOP_3(J11) .EQ. EOPS__C ) THEN
                      SIGMA = SIG_E3_C
                   ELSE IF ( IND_EOP_3(J11) .EQ. EOPS__I ) THEN
                      SIGMA = SCL_SIG_E3_I * DSQRT ( SIG(J12,3,1,IND_EOP_3(J11))**2 + ADD_WEI(3,1,IND_EOP_3(J11))**2 )
                   ELSE IF ( IND_EOP_3(J11) .EQ. EOPS__J ) THEN
                      SIGMA = SCL_SIG_E3_J * DSQRT ( SIG(J12,3,1,IND_EOP_3(J11))**2 + ADD_WEI(3,1,IND_EOP_3(J11))**2 )
                 END IF
!
! -------------- NB: we remove bias of the series on the fly
!
                 VAL = E(J12,3,1,IND_EOP_3(J11)) - SHIFT(3,1,IND_EOP_3(J11))
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU, EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT  )
            END IF
!
            IF ( IND_EOP_3(J11) .EQ. EOPS__R .OR. &
     &           IND_EOP_3(J11) .EQ. EOPS__U .OR. &
     &           IND_EOP_3(J11) .EQ. EOPS__A      ) THEN
!
! -------------- Form equation for the E3-rate
!
                 L_EQU = 0
                 DO 4140 J14=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J14, TIM(J12,IND_EOP_3(J11)) )
                    IND_EQU(L_EQU) = IK + J14 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J11= ', INT2(J11), ' J14= ', J14, &
     &                                  ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                  ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR)
                         WRITE ( 6, * ) 'IK= ', IK, ' NERS%FCS%NK_3 = ', NERS%FCS%NK_3
                         WRITE ( 6, * ) 'T1= ', NERS%FCS%ARG_3(NERS%FCS%NK_3), ' T2= ', TIM(J12,IND_EOP_3(J11)), ' T3= ', TIM(NP(IND_EOP_3(J11)),IND_EOP_3(J11))
                         WRITE ( 6, * ) 'J12= ', J12, ' NP(IND_EOP_3(J11))= ', NP(IND_EOP_3(J11))
                         IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J12,IND_EOP_3(J11)) - TIM_EPS )
                         WRITE ( 6, * ) 'Test_tim = ', TIM(J12,IND_EOP_3(J11)) - TIM_EPS, ' New IK= ', IK
                         CALL ERR_LOG ( 6683, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4140            CONTINUE 
                 IF ( IND_EOP_3(J11) .EQ. EOPS__R .OR. &
     &                IND_EOP_3(J11) .EQ. EOPS__U      ) THEN
                      VAL = E(J12,3,2,IND_EOP_3(J11)) - SHIFT(3,2,IND_EOP_3(J11))
                    ELSE IF ( IND_EOP_3(J11) .EQ. EOPS__A ) THEN
                      VAL = E(J12,3,2,IND_EOP_3(J11)) - SHIFT(3,2,IND_EOP_3(J11))
                 END IF
                 IF ( IND_EOP_3(J11) .EQ. EOPS__R ) THEN
                      SIGMA = SCL_SIG_E3R_R * DSQRT ( SIG(J12,3,2,IND_EOP_3(J11))**2 + &
     &                                                ADD_WEI(3,2,IND_EOP_3(J11))**2 )
                    ELSE IF ( IND_EOP_3(J11) .EQ. EOPS__U ) THEN
                      SIGMA = SCL_SIG_E3R_U * DSQRT ( SIG(J12,3,2,IND_EOP_3(J11))**2 + &
     &                                                ADD_WEI(3,2,IND_EOP_3(J11))**2 )
                    ELSE IF ( IND_EOP_3(J11) .EQ. EOPS__A ) THEN
                      SIGMA = SCL_SIG_E3R_A * DSQRT ( SIG(J12,3,2,IND_EOP_3(J11))**2 + &
     &                                                ADD_WEI(3,2,IND_EOP_3(J11))**2 )
                 END IF
!
! -------------- NB: no bias correction for EOP rate
!
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU,     EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT )
            END IF
 4120    CONTINUE 
 4110 CONTINUE 
!
      IF ( CNS_E3D > 0.0D0  .OR.  CNS_E3DR > 0.0D0 .OR.  CNS_E3D2 > 0.0D0 ) THEN
!
! -------- Add the contribution of constraint equations to the normal matrix
! -------- to make the spline smoother within the end of EOP interval
!
           DO 4150 J15=1,NP(EOPS__A)
              IF ( TIM(J15,EOPS__A) < TIM3_MOD(1) ) GOTO 4150
!
! ----------- Constraints on E3 value
!
              IF ( J15 == 1 ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J15,EOPS__A) + TIM_EPS )
                 ELSE IF ( J15 == NP(EOPS__A) ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J15,EOPS__A) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J15,EOPS__A) )
              END IF
              IF ( IK < 1 ) GOTO 4150
!
              IF ( J15 == 1 ) THEN
                   IM = IXMN8 ( NM_3, TIM3_MOD, TIM(J15,EOPS__A) + TIM_EPS )
                 ELSE IF ( J15 == NP(EOPS__A) ) THEN
                   IM = IXMN8 ( NM_3, TIM3_MOD, TIM(J15,EOPS__A) - TIM_EPS )
                 ELSE
                   IM = IXMN8 ( NM_3, TIM3_MOD, TIM(J15,EOPS__A) )
              END IF
              IF ( IM < 1 ) GOTO 4150
!
!!    write ( 6, * ) 'j15= ', j15, ' tim= ', TIM(J15,EOPS__A), ' dat= ', (tim(j15,eops__a) - tim(np(eops__a),eops__a))/86400.0d0  ! %%%%
              L_EQU = 0
              DO 4160 J16=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J16, TIM(J15,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J16 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J15= ', INT2(J15), ' J16= ', INT2(J16), &
     &                               ' IND_EQU = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6684, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4160         CONTINUE 
!
! ----------- NB: we constraint the value of the 3rd EOP component to the 
! ----------- deterministic model.
!
              IF ( CNS_E3D > 0.0D0 ) THEN
                   VAL = FSPL8 ( TIM(J15,EOPS__A), NM_3, TIM3_MOD,  E3_MOD, IM, E3_MOD_SPL )
                   SIGMA = CNS_E3D
                   CALL ADD_TRG ( SCL*VAL, &
     &                            SCL*SIGMA, &
     &                            L_EQU,  IND_EQU, EQU_VEC, &
     &                            L_PAR,  NOR_VEC, NOR_MAT  )
              END IF
!
! ----------- Constraints on E3 rate
!
              L_EQU = 0
              DO 4170 J17=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J17, TIM(J15,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J17 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J15= ', INT2(J15), ' J17= ', INT2(J17), &
     &                               ' IND_EQU = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6685, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4170         CONTINUE 
              IF ( CNS_E3DR > 0.0D0 ) THEN
                   DER = DSPL8 ( TIM(J15,EOPS__A), NM_3, TIM3_MOD,  E3_MOD, IM, E3_MOD_SPL )
                   SIGMA = CNS_E3DR
                   CALL ADD_TRG ( SCL*DER, &
     &                            SCL*SIGMA, &
     &                            L_EQU,  IND_EQU, EQU_VEC, &
     &                            L_PAR,  NOR_VEC, NOR_MAT  )
              END IF
!
! ----------- Constraints on E3 acceleration
!
              L_EQU = 0
              DO 5180 J18=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DR2 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J18, TIM(J15,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J18 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J15= ', INT2(J15), ' J18= ', INT2(J18), &
     &                               ' IND_EQU= ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6686, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 5180         CONTINUE 
              IF ( CNS_E3D2 > 0.0D0 ) THEN
                   DR2 = D2SPL8 ( TIM(J15,EOPS__A), NM_3, TIM3_MOD, E3_MOD, IM, E3_MOD_SPL )
!!                   DR2 = 0.0D0
                   SIGMA = CNS_E3D2
                   CALL ADD_TRG ( SCL*DR2, &
     &                            SCL*SIGMA, &
     &                            L_EQU,  IND_EQU, EQU_VEC, &
     &                            L_PAR,  NOR_VEC, NOR_MAT  )
!%%    write ( 6, * ) 'DR2 j15= ', int2(j15), ' ik= ', int2(ik), ' tim= ', TIM(J15,EOPS__A), ' dat= ', (tim(j15,eops__a) - tim(np(eops__a),eops__a))/86400.0d0  ! %%%%
              END IF
 4150      CONTINUE 
      END IF
!
      IF ( CNS_E3RR > 0.0D0 ) THEN
           DO 4180 J18=NP(EOPS__A)-NP_E3_RR,NP(EOPS__A)
!!   write ( 6, * ) ' j18 = ', j18 , ' tim = ', tim(j18,eops__a) ; call flush ( 6 ) ! %%%%%%%%
!
! ----------- Constraints on E3 acceleration to zero
!
              IF ( J18 == NP(EOPS__A) ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J18,EOPS__A) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J18,EOPS__A) )
              END IF
!
              L_EQU = 0
              DO 4190 J19=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DR2 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J19, TIM(J18,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J19 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J18= ', INT2(J18), ' J19= ', INT2(J19), &
     &                               ' IND_EQU = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6687, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4190         CONTINUE 
              SIGMA = CNS_E3RR
              VAL   = 0.0D0
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
 4180      CONTINUE 
      END IF
!
! --- Add constraints to the end of C04 series in order to ensure a smooth
! --- transition between the C04 and forecast for the E3 component.
! --- We computed for the number of C04 epochs 3 times greater NERS__EDG_NODES,
! --- i.e. 9 days
!
      DO 4200 J20=NP(EOPS__C)-3*NERS__EDG_NODES+1,NP(EOPS__C)
         IK = IXMN8 ( NERS%FCS%NK_3, NERS%FCS%ARG_3, TIM(J20,EOPS__C) - TIM_EPS )
!
! ------ Constraint on value that forces the forecast to be close to 
! ------ the end of the C04 interval
!
         L_EQU = 0
         DO 4210 J21=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J21, TIM(J20,EOPS__C)  )
            IND_EQU(L_EQU) = IK + J21 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J20= ', INT2(J20), ' J21= ', J21, &
     &                           ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                           ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6688, IUER, 'EOP_FCS', 'Trap of '// &
     &               'internal control: wrong index' )
                 RETURN 
            END IF
 4210    CONTINUE 
         SIGMA = CNS_C04_E3_SIG
         IF ( SIGMA > 0.0 ) THEN
              VAL   = E(J20,3,1,EOPS__C)
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
!
! ------ Constraint on rate of change
!
         L_EQU = 0
         DO 4220 J22=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_3, NERS%FCS%ARG_3, DEG, IK+J21, TIM(J20,EOPS__C) )
            IND_EQU(L_EQU) = IK + J22 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J20= ', INT2(J20), ' J22= ', J22, &
     &                           ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                           ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6689, IUER, 'EOP_FCS', 'Trap of '// &
     &               'internal control: wrong index' )
                 RETURN 
            END IF
 4220    CONTINUE 
         SIGMA = CNS_C04_E3R_SIG
         IF ( SIGMA > 0.0D0 ) THEN
              VAL   = E(J20,3,2,EOPS__C)
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
 4200 CONTINUE 
!
! --- Invert the normal matrix...
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6690, IUER, 'EOP_FCS', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           DEALLOCATE ( NERS%FCS%BSPL_E3 )
           DEALLOCATE ( IND_EQU )
           RETURN 
      END IF
!
! --- ... and find vector of the estimates of the EOP forecast parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, NERS%FCS%BSPL_E3, IER )
      NERS%FCS%BSPL_E3 = NERS%FCS%BSPL_E3/SCL
!
! === Processing pole coordinates
!
      DEALLOCATE ( NOR_MAT    ) 
      DEALLOCATE ( NOR_VEC    ) 
      DEALLOCATE ( EQU_VEC    ) 
      DEALLOCATE ( IND_EQU    ) 
!
! --- Compute array of arguments for spline coefficients of E12
!
      NERS%FCS%NK_12 = NINT ( (TIMA(NP(EOPS__A)) - TIM_BEG)/TIM_STEP_12 )
      ALLOCATE ( NERS%FCS%ARG_12(NERS%FCS%NK_12) )
      DO 4230 J23=1,NERS%FCS%NK_12
         NERS%FCS%ARG_12(J23) = TIM_BEG + (J23-1)*TIM_STEP_12
 4230 CONTINUE 
      NERS%FCS%ARG_12(NERS%FCS%NK_12) = TIMA(NP(EOPS__A))
!
! --- Allocate dynamic memory for normal matrix
!
      L_PAR = 2*(NERS%FCS%NK_12 + DEG - 1)
      L_PA2 = (L_PAR*(L_PAR+1))/2
      ALLOCATE ( NOR_MAT(L_PA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PA2, STR )
           CALL ERR_LOG ( 6691, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NOR_MAT' )
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6692, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NOR_VEC' )
           RETURN 
      END IF
      ALLOCATE ( NERS%FCS%BSPL_E12(L_PAR/2,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6693, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array NERS%FCS%BSPL_E3' )
           RETURN 
      END IF
      ALLOCATE ( EQU_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6694, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array EQU_VEC' )
           RETURN 
      END IF
      ALLOCATE ( IND_EQU(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 6695, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array IND_VEC' )
           RETURN 
      END IF
!
! --- Allocate dynamic memory for 1-2 components of the Euler angles
!
      ALLOCATE ( TIM12_MOD(NM_12),  STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_12, STR )
           CALL ERR_LOG ( 6696, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array '// &
     &          'TIM12_MOD' )
           RETURN 
      END IF
      ALLOCATE ( E12_MOD(NM_12,2),  STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_12, STR )
           CALL ERR_LOG ( 6697, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array '// &
     &          'E12_MOD' )
           RETURN 
      END IF
      ALLOCATE ( E12_MOD_SPL(NM_12,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NM_12, STR )
           CALL ERR_LOG ( 6698, IUER, 'EOP_FCS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array '// &
     &          'E12_MOD_SPL' )
           RETURN 
      END IF
!
! --- Compute the deterministic model for 1-2 components of Euler angles
!
      CALL ERR_PASS    ( IUER, IER )
      CALL EOP_E12_MOD ( MP, NP(EOPS__U), TIM(1,EOPS__U), E(1,1,1,EOPS__U), SIG(1,1,1,EOPS__U), &
     &                                                    E(1,1,2,EOPS__U), SIG(1,1,2,EOPS__U), &
     &                   NM_12, TIM12_MOD, E12_MOD,          E12_MOD_SPL, &
     &                   TIM_BEG, TIM(NP(EOPS__A),EOPS__A), &
     &                   WEI12_SCLV, WEI12_SCLR, CNS12_DER, CNS12_DR2, EOP%IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6699, IUER, 'EOP_FCS', 'Failure in computing '// &
     &         'deterministic model for E12 using EOPS__U series' )
           RETURN 
      END IF
!
      IF ( EOP%IVRB == 51 ) THEN
           CALL PLOT_EOP_MOD ( 1, NP(EOPS__U), TIM(1,EOPS__U), E(1,1,1,EOPS__U), SIG(1,1,1,EOPS__U), &
     &                            NM_12, TIM12_MOD, E12_MOD(1,1),     E12_MOD_SPL(1,1), IER )
           CALL PLOT_EOP_MOD ( 2, NP(EOPS__U), TIM(1,EOPS__U), E(1,2,1,EOPS__U), SIG(1,2,1,EOPS__U), &
     &                            NM_12, TIM12_MOD, E12_MOD(1,2),     E12_MOD_SPL(1,2), IER )
        ELSE IF ( EOP%IVRB == 52 ) THEN
           CALL PLOT_EOP_MOD ( 4, NP(EOPS__U), TIM(1,EOPS__U), E(1,1,2,EOPS__U), SIG(1,1,2,EOPS__U), &
     &                            NM_12, TIM12_MOD, E12_MOD(1,1),     E12_MOD_SPL(1,1), IER )
           CALL PLOT_EOP_MOD ( 5, NP(EOPS__U), TIM(1,EOPS__U), E(1,2,2,EOPS__U), SIG(1,2,2,EOPS__U), &
     &                            NM_12, TIM12_MOD, E12_MOD(1,2),     E12_MOD_SPL(1,2), IER )
      END IF
!
! --- Build the system of normal equations for computing spline coefficients
! --- for the 12 components of the Euler angles
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 4240 J24=1,NC_EOP_12
         DO 4250 J25=1,NP(IND_EOP_12(J24))
            IF ( J25 == 1 ) THEN
                 IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J25,IND_EOP_12(J24)) + TIM_EPS )
               ELSE IF ( J25 == NP(IND_EOP_12(J24)) ) THEN
                 IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J25,IND_EOP_12(J24)) - TIM_EPS )
               ELSE
                 IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J25,IND_EOP_12(J24)) )
            END IF
            IF ( IK .LT. 1              ) GOTO 4250
            IF ( IK .GE. NERS%FCS%NK_12 ) GOTO 4250
            IF ( IND_EOP_12(J24) .EQ. EOPS__C  .OR. &
     &           IND_EOP_12(J24) .EQ. EOPS__R  .OR. &
     &           IND_EOP_12(J24) .EQ. EOPS__U       ) THEN
!
! -------------- Form equation for the E1 angle
!
                 L_EQU = 0
                 DO 4260 J26=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J26, TIM(J25,IND_EOP_12(J24)) )
                    IND_EQU(L_EQU) = IK + J26 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J24= ', INT2(J24), ' J25= ', J25, &
     &                                ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                         CALL ERR_LOG ( 6700, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4260            CONTINUE 
                 IF ( IND_EOP_12(J24) .EQ. EOPS__C ) THEN
                      SIGMA = SIG_E12_C
                   ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                      SIGMA = SCL_CNS_E12*DSQRT ( SIG(J25,1,1,IND_EOP_12(J24))**2 + ADD_WEI(1,1,IND_EOP_12(J24))**2 )
                   ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__U ) THEN
                      SIGMA = SCL_CNS_E12*DSQRT ( SIG(J25,1,1,IND_EOP_12(J24))**2 + ADD_WEI(1,1,IND_EOP_12(J24))**2 )
                 END IF
!
! -------------- NB: we remove the bias
!
                 VAL = E(J25,1,1,IND_EOP_12(J24)) - SHIFT(1,1,IND_EOP_12(J24))
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU, EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT  )
!
! -------------- Form equation for the E2 angle
!
                 L_EQU = 0
                 DO 4270 J27=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J27, TIM(J25,IND_EOP_12(J24)) )
                    IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J27 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J24= ', INT2(J24), ' J27= ', J27, &
     &                                ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                         CALL ERR_LOG ( 6701, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4270            CONTINUE 
                 IF ( IND_EOP_12(J24) .EQ. EOPS__C ) THEN
                      SIGMA = SIG_E12_C
                   ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                      SIGMA = SCL_CNS_E12*DSQRT ( SIG(J25,2,1,IND_EOP_12(J24))**2 + ADD_WEI(2,1,IND_EOP_12(J24))**2 )
                   ELSE IF ( IND_EOP_12(J24) .NE. EOPS__U ) THEN
                      SIGMA = SCL_CNS_E12*DSQRT ( SIG(J25,2,1,IND_EOP_12(J24))**2 + ADD_WEI(2,1,IND_EOP_12(J24))**2 )
                 END IF
!
! -------------- Again: we remove the bias
!
                 VAL = E(J25,2,1,IND_EOP_12(J24)) - SHIFT(2,1,IND_EOP_12(J24))
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU, EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT  )
            END IF
!
            IF ( IND_EOP_12(J24) .EQ. EOPS__R .OR. &
     &           IND_EOP_12(J24) .EQ. EOPS__U      ) THEN
!
! -------------- Form equation for the E1-rate
!
                 L_EQU = 0
                 DO 4280 J28=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J28, TIM(J25,IND_EOP_12(J24)) )
                    IND_EQU(L_EQU) = IK + J28 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J24= ', INT2(J24), ' J28= ', J28, &
     &                                ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                         CALL ERR_LOG ( 6702, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4280            CONTINUE 
                 IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                      VAL = E(J25,1,2,IND_EOP_12(J24)) - SHIFT(1,2,IND_EOP_12(J24))
                   ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__U ) THEN
                      VAL = E(J25,1,2,IND_EOP_12(J24)) - SHIFT(1,2,IND_EOP_12(J24))
                   ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__A ) THEN
                      VAL = E(J25,1,2,IND_EOP_12(J24)) - SHIFT(1,2,IND_EOP_12(J24))
                 END IF
!
                 IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                       SIGMA = SCL_CNS_E12R*DSQRT ( SIG(J25,1,2,IND_EOP_12(J24))**2 + ADD_WEI(1,2,IND_EOP_12(J24))**2 )
                    ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__U ) THEN
                       SIGMA = SCL_CNS_E12A*DSQRT ( SIG(J25,1,2,IND_EOP_12(J24))**2 + ADD_WEI(1,2,IND_EOP_12(J24))**2 )
                 END IF
!
! -------------- No bias for E1-rate
!
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU, EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT  )
!
! -------------- Form equation for the E2-rate
!
                 L_EQU = 0
                 DO 4290 J29=-DEG,0
                    L_EQU = L_EQU + 1
                    EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J29, TIM(J25,IND_EOP_12(J24)) )
                    IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J29 + DEG
                    IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                         WRITE ( 6, * ) 'EOP_FCS J24= ', INT2(J24), ' J29= ', J29, &
     &                                ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                                ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                         CALL ERR_LOG ( 6703, IUER, 'EOP_FCS', 'Trap of '// &
     &                       'internal control: wrong index' )
                         RETURN 
                    END IF
 4290            CONTINUE 
                 IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                      VAL = E(J25,2,2,IND_EOP_12(J24)) - SHIFT(2,2,IND_EOP_12(J24))
                    ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__U ) THEN
                      VAL = E(J25,2,2,IND_EOP_12(J24)) - SHIFT(2,2,IND_EOP_12(J24))
                 END IF
                 IF ( IND_EOP_12(J24) .EQ. EOPS__R ) THEN
                       SIGMA = SCL_CNS_E12R*DSQRT ( SIG(J25,2,2,IND_EOP_12(J24))**2 + ADD_WEI(2,2,IND_EOP_12(J24))**2 )
                    ELSE IF ( IND_EOP_12(J24) .EQ. EOPS__U ) THEN
                       SIGMA = SCL_CNS_E12A*DSQRT ( SIG(J25,2,2,IND_EOP_12(J24))**2 + ADD_WEI(2,2,IND_EOP_12(J24))**2 )
                 END IF
!
! -------------- No bias for E2-rate neither
!
                 CALL ADD_TRG ( SCL*VAL, &
     &                          SCL*SIGMA, &
     &                          L_EQU,  IND_EQU, EQU_VEC, &
     &                          L_PAR,  NOR_VEC, NOR_MAT  )
            END IF
 4250    CONTINUE 
 4240 CONTINUE 
!
      IF ( SCL_CNS_E12 > 0.0D0 .OR. SCL_CNS_E12R > 0.0D0 ) THEN
!
! -------- Add the contribution of constraint equations to the normal matrix
!
           DO 4300 J30=1,NP(EOPS__A)
              IF ( TIM(J30,EOPS__A) < TIM12_MOD(1) ) GOTO 4300
!
! ----------- Constraints on E1 value
!
              IF ( J30 == 1 ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J30,EOPS__A) + TIM_EPS )
                 ELSE IF ( J30 == NP(EOPS__A) ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J30,EOPS__A) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J30,EOPS__A) )
              END IF
              IF ( IK < 1 ) GOTO 4300
!
              IF ( J30 == 1 ) THEN
                   IM = IXMN8 ( NM_12, TIM12_MOD, TIM(J30,EOPS__A) + TIM_EPS )
                 ELSE IF ( J30 == NP(EOPS__A) ) THEN
                   IM = IXMN8 ( NM_12, TIM12_MOD, TIM(J30,EOPS__A) - TIM_EPS )
                 ELSE
                   IM = IXMN8 ( NM_12, TIM12_MOD, TIM(J30,EOPS__A) )
              END IF
              L_EQU = 0
              DO 4310 J31=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J31, TIM(J30,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J31 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J30= ', INT2(J30), ' J31= ', J31, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6704, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4310         CONTINUE 
!
! ----------- We constaint E1 to the vaule of the deterministic model
!
              VAL = FSPL8 ( TIM(J30,EOPS__A), NM_12, TIM12_MOD, &
     &                      E12_MOD(1,1), IM, E12_MOD_SPL(1,1) )
              SIGMA = CNS_E12D
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
!
! ----------- Constaint on E2 value
!
              L_EQU = 0
              DO 4320 J32=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J32, TIM(J30,EOPS__A) )
                 IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J32 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J30= ', INT2(J30), ' J32= ', J32, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6705, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4320         CONTINUE 
!
! ----------- We constaint E2 to the vaule of the deterministic model
!
              VAL = FSPL8 ( TIM(J30,EOPS__A), NM_12, TIM12_MOD, &
     &                      E12_MOD(1,2), IM, E12_MOD_SPL(1,2) )
              SIGMA = CNS_E12D
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
!
! ----------- Constraints on E1 rate
!
              L_EQU = 0
              DO 4330 J33=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J33, TIM(J30,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J33 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J30= ', INT2(J30), ' J33= ', J33, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6706, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4330         CONTINUE 
!
! ----------- We constaint E1 rate to the rate of the deterministic model
!
              DER = DSPL8 ( TIM(J30,EOPS__A), NM_12, TIM12_MOD, &
     &                      E12_MOD(1,1), IM, E12_MOD_SPL(1,1) )
              SIGMA = CNS_E12DR
              CALL ADD_TRG ( SCL*DER, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
!
! ----------- We constaint E2 rate to the rate of the deterministic model
!
              L_EQU = 0
              DO 4340 J34=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J34, TIM(J30,EOPS__A) )
                 IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J34 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J30= ', INT2(J30), ' J34= ', J34, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6707, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4340         CONTINUE 
              DER = DSPL8 ( TIM(J30,EOPS__A), NM_12, TIM12_MOD, &
     &                      E12_MOD(1,2), IM, E12_MOD_SPL(1,2) )
              SIGMA = CNS_E12DR
              CALL ADD_TRG ( SCL*DER, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
 4300      CONTINUE 
      END IF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      write ( 6, * ) 'eop_fcs-1404 tim-range = ', ners%fcs%arg_c(1), ners%fcs%arg_c(ners%fcs%nc) ! %%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF ( CNS_E3RR > 0.0D0 ) THEN
           DO 4350 J35=NP(EOPS__A)-NP_E12_RR,NP(EOPS__A)
!
! ----------- Constraints on E1 acceleration
!
              IF ( J35 == 1 ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J35,EOPS__A) + TIM_EPS )
                 ELSE IF ( J35 == NP(EOPS__A) ) THEN
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J35,EOPS__A) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J35,EOPS__A) )
              END IF
!
              L_EQU = 0
              DO 4360 J36=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DR2 ( NERS%FCS%NK_3, NERS%FCS%ARG_12, DEG, IK+J36, TIM(J35,EOPS__A) )
                 IND_EQU(L_EQU) = IK + J36 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J35= ', INT2(J35), ' J36= ', J36, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6708, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4360         CONTINUE 
              SIGMA = CNS_E12RR
              VAL   = 0.0D0
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
!
! ----------- Constraints on E2 acceleration
!
              L_EQU = 0
              DO 4370 J37=-DEG,0
                 L_EQU = L_EQU + 1
                 EQU_VEC(L_EQU) = BSPL_DR2 ( NERS%FCS%NK_3, NERS%FCS%ARG_12, DEG, IK+J37, TIM(J35,EOPS__A) )
                 IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J37 + DEG
                 IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                      WRITE ( 6, * ) 'EOP_FCS J35= ', INT2(J35), ' J37= ', J37, &
     &                               ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                               ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                      CALL ERR_LOG ( 6709, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                      RETURN 
                 END IF
 4370         CONTINUE 
              SIGMA = CNS_E12RR
              VAL   = 0.0D0
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
 4350      CONTINUE 
      END IF
!
! --- Add constraints to the end of C04 series in order to ensure a smooth
! --- transition between the C04 and the forecast for the E1/E2 components
!
      DO 4380 J38=NP(EOPS__C)-3*NERS__EDG_NODES,NP(EOPS__C)
         IF ( J38 == NP(EOPS__C) ) THEN
              IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J38,EOPS__C) - TIM_EPS )
            ELSE 
              IK = IXMN8 ( NERS%FCS%NK_12, NERS%FCS%ARG_12, TIM(J38,EOPS__C)           )
         END IF
!
! ------ Constraint on E1 value
!
         L_EQU = 0
         DO 4390 J39=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J39, TIM(J38,EOPS__C) )
            IND_EQU(L_EQU) = IK + J39 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J38= ', INT2(J38), ' J39= ', J39, &
     &                          ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                          ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6710, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                 RETURN 
            END IF
 4390    CONTINUE 
         SIGMA = CNS_C04_E12_SIG
         IF ( TIM(J38,EOPS__C) < NERS%FCS%ARG_C(NERS%FCS%NC) .AND. SIGMA > 0.0D0 ) THEN
              VAL   = E(J38,1,1,EOPS__C)
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
!
! ------ Constraint on E1 rate of change
!
         L_EQU = 0
         DO 4400 J40=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J40, TIM(J38,EOPS__C) )
            IND_EQU(L_EQU) = IK + J40 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J38= ', INT2(J38), ' J40= ', J40, &
     &                          ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                          ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6711, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                 RETURN 
            END IF
 4400    CONTINUE 
         SIGMA = CNS_C04_E12R_SIG
         IF ( TIM(J38,EOPS__C) < NERS%FCS%ARG_C(NERS%FCS%NC) .AND. SIGMA > 0.0D0 ) THEN
              VAL   = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, TIM(J38,EOPS__C), &
     &                               NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,1) )
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
!
! ------ Constraint on E2 value
!
         L_EQU = 0
         DO 4410 J41=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_VAL ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J41, TIM(J38,EOPS__C) )
            IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J41 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J38= ', INT2(J38), ' J41= ', J41, &
     &                          ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                          ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6712, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                 RETURN 
            END IF
 4410    CONTINUE 
         SIGMA = CNS_C04_E12_SIG
         VAL   = E(J38,2,1,EOPS__C)
         IF ( TIM(J38,EOPS__C) < NERS%FCS%ARG_C(NERS%FCS%NC) .AND. SIGMA > 0.0D0 ) THEN
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
!
! ------ Constraint on E2 rate
!
         L_EQU = 0
         DO 4420 J42=-DEG,0
            L_EQU = L_EQU + 1
            EQU_VEC(L_EQU) = BSPL_DER ( NERS%FCS%NK_12, NERS%FCS%ARG_12, DEG, IK+J42, TIM(J38,EOPS__C) )
            IND_EQU(L_EQU) = (NERS%FCS%NK_12 + DEG - 1) + IK + J42 + DEG
            IF ( IND_EQU(L_EQU) < 1 .OR. IND_EQU(L_EQU) > L_PAR ) THEN
                 WRITE ( 6, * ) 'EOP_FCS J38= ', INT2(J38), ' J42= ', J42, &
     &                          ' IND_EQU(L_EQU) = ', IND_EQU(L_EQU), &
     &                          ' L_EQU= ', INT2(L_EQU), ' L_PAR= ', INT2(L_PAR) ! %%%
                 CALL ERR_LOG ( 6713, IUER, 'EOP_FCS', 'Trap of '// &
     &                    'internal control: wrong index' )
                 RETURN 
            END IF
 4420    CONTINUE 
         SIGMA = CNS_C04_E12R_SIG
         IF ( TIM(J38,EOPS__C) < NERS%FCS%ARG_C(NERS%FCS%NC) .AND. SIGMA > 0.0D0 ) THEN
              VAL   = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, TIM(J38,EOPS__C), &
     &                               NERS%FCS%ARG_C, NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,2) )
              CALL ADD_TRG ( SCL*VAL, &
     &                       SCL*SIGMA, &
     &                       L_EQU,  IND_EQU, EQU_VEC, &
     &                       L_PAR,  NOR_VEC, NOR_MAT  )
         END IF
 4380 CONTINUE 
!
! --- Invert the normal matrix...
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6714, IUER, 'EOP_FCS', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC )
           DEALLOCATE ( NOR_MAT )
           DEALLOCATE ( EQU_VEC )
           DEALLOCATE ( NERS%FCS%BSPL_E3 )
           DEALLOCATE ( IND_EQU )
           RETURN 
      END IF
!
! --- Get B-spline
!
      IER=-1
      CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, NERS%FCS%BSPL_E12, IER )
      NERS%FCS%BSPL_E12 = NERS%FCS%BSPL_E12/SCL
!
      DEALLOCATE ( NOR_MAT    ) 
      DEALLOCATE ( NOR_VEC    ) 
      DEALLOCATE ( EQU_VEC    ) 
      DEALLOCATE ( IND_EQU    ) 
!
! --- Prepare the array of arugments for plotting in the 
!
      NT = (TIMA(NP(EOPS__A)) - TIM_BEG)/3600.0D0
      ALLOCATE ( TIM_RES(NT) )
      ALLOCATE ( VAL_RES(NT) )
      DO 4430 J43=1,NT
         TIM_RES(J43) = TIM_BEG + (J43-1)*3600.0D0
 4430 CONTINUE 
!
! --- Compute the start and stop time of EOP prediction.
! --- We stet the start date NL_OVR epocs befoe the end of C04
!
      TIM_L_STEP = TIM(2,EOPS__L) - TIM(1,EOPS__L) 
      TIM_L_BEG  = TIM(NP(EOPS__C),EOPS__C) - NL_OVR*TIM_L_STEP
      NL = IDNINT( (TIM(NP(EOPS__L),EOPS__L) - TIM_L_BEG)/TIM_L_STEP ) + 1
      IF ( MOD(NL,2) == 0 ) THEN
           TIM_L_BEG  = TIM_L_BEG - TIM_L_STEP
           NL = NL + 1
      END IF 
!
! --- The time step for knots for long-tern prediction is twice more than
! --- for the original long-term predictgoin series
!
      NERS%FCS%NL = NL/2 + 1
!
! --- Move the used section of the LTP series to the beg
!
      E(1:NL,1:3,1:2,EOPS__L)   = E(NP(EOPS__L)-NL+1:NP(EOPS__L),1:3,1:2,EOPS__L)
      TIM(1:NL,EOPS__L)         = TIM(NP(EOPS__L)-NL+1:NP(EOPS__L),EOPS__L)
      SIG(1:NL,1:3,1:2,EOPS__L) = SIG(NP(EOPS__L)-NL+1:NP(EOPS__L),1:3,1:2,EOPS__L)
      NP(EOPS__L) = NL
!
! --- Allocate memory for the long-term prediction
!
      ALLOCATE ( NERS%FCS%ARG_L(NERS%FCS%NL),          STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6715, IUER, 'EOP_FCS', 'Errror in allocating '// &
     &         'dynamic memory for array NERS%FCS%ARG_C' )
           RETURN 
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_L(1-NERS__MDEG:NERS%FCS%NL-1,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6716, IUER, 'EOP_FCS', 'Errror in allocating '// &
     &         'dynamic memory for array NERS%FCS%BSPL_L' )
           RETURN 
      END IF
!
! --- Cycle over epochs of long-term EOP prediction
!
      DO 4440 J44=1,NP(EOPS__L)
!
! ------ Subtract the contribution of zonal tides
!
         WEI(J44,1) = 1.0D0/SIG(J44,1,1,EOPS__L)
         WEI(J44,2) = 1.0D0/SIG(J44,2,1,EOPS__L)
         WEI(J44,3) = 1.0D0/SIG(J44,3,1,EOPS__L)
         MJD = TIM(J44,EOPS__L)/86400.0D0 + J2000__MJD
         TAI = TIM(J44,EOPS__L) - (MJD - J2000__MJD)*86400.0D0
         IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_D93 ) THEN
              CALL E3ZT_DICKMAN1993  ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
            ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_RE2014 ) THEN
              CALL E3ZT_RE2014 ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
            ELSE IF ( EOP%CONF%E3Z_MOD == NERS__E3Z_NONE   ) THEN
               E3_HAR     = 0.0D0
               E3_DOT_HAR = 0.0D0
               E3_DT2_HAR = 0.0D0
         END IF
         E(J44,3,1,EOPS__L) = E(J44,3,1,EOPS__L) - E3_HAR
         E(J44,3,2,EOPS__L) = E(J44,3,2,EOPS__L) - E3_DOT_HAR
 4440 CONTINUE 
!
! --- Set the array of arguments for the long-term EOP prediction
!
      DO 4450 J45=1,NERS%FCS%NL
         NERS%FCS%ARG_L(J45) = TIM_L_BEG + 2*(J45-1)*TIM_L_STEP     
 4450 CONTINUE 
!
! --- Compute the B-spline expansion coefficients for the long-term prediction
!
      DO 4460 J46=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL EBSPL_WLSQ_CNS3 ( NP(EOPS__L), TIM(1,EOPS__L), E(1,J46,1,EOPS__L), WEI(1,J46), &
     &                          NERS%FCS%NL, NERS__MDEG, NERS%FCS%ARG_L,         &
     &                          NERS%FCS%BSPL_L(1-NERS__MDEG:NERS%FCS%NL-1,J46), &
     &                          0.0D0, 0.0D0, 0.0D0, WRMS_L(J46), IER )
!
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J46, STR )
              CALL ERR_LOG ( 6717, IUER, 'EOP_FCS', 'Error in computing '// &
     &            'coefficients of B-spline expansion of the EOP long-term '// &
     &            'prediction component '//TRIM(STR) )
              RETURN 
         END IF
 4460 CONTINUE 
!!
!   write ( 6, * ) 'EOP_FCS-1748: ser%n_eop= ', ser%n_eop, ser%eop_tim(1), ser%eop_tim(ser%n_eop)  ! %%%%%
!   write ( 6, * ) 'EOP_FCS-1749: np(eops__c) = ',np(eops__c), ' tim= ', tim(1,eops__c), tim(np(eops__c),eops__c) ! %%%%%%%
!   write ( 6, * ) 'EOP_FCS-1750: np(eops__u) = ',np(eops__u), ' tim= ', tim(1,eops__u), tim(np(eops__u),eops__u) ! %%%%%%%
!   call diagi_1 ( np(eops__c), tim(1,eops__c), e(1,1,2,eops__c), iuer ) ! %%%%%
!   call diagi_2 ( np(eops__u), tim(1,eops__u), e(1,1,2,eops__u), ser%n_eop, ser%eop_tim(1), e(1,1,2,eops__c), ier ) ! %%%
!!
      IF ( EOP%IVRB .EQ. 31 ) THEN
!
! -------- Make plot of E1 and E1 rate in the debugging mode
                                !
           L_PAR = NERS%FCS%NK_12 + DEG - 1
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 1, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,1,1,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1,1,1,EOPS__A), &
     &                         NM_12,       TIM12_MOD,  E12_MOD, E12_MOD_SPL, &
     &                         NERS%FCS%NK_12, NERS%FCS%ARG_12, L_PAR, NERS%FCS%BSPL_E12, &
     &                         NT, TIM_RES, VAL_RES, IER )
!
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 4, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,1,2,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,1,2,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1,1,2,EOPS__A), &
     &                         NM_12,       TIM12_MOD,  E12_MOD, E12_MOD_SPL, &
     &                         NERS%FCS%NK_12, NERS%FCS%ARG_12, L_PAR, NERS%FCS%BSPL_E12, &
     &                         NT, TIM_RES, VAL_RES, IER )
      END IF
      IF ( EOP%IVRB .EQ. 32 ) THEN
!
! -------- Make plot of E2 and E2 rate in the debugging mode
!
           L_PAR = NERS%FCS%NK_12 + DEG - 1
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 2, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,1,1,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,1,1,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1,1,1,EOPS__A), &
     &                         NM_12,       TIM12_MOD,  E12_MOD, E12_MOD_SPL, &
     &                         NERS%FCS%NK_12, NERS%FCS%ARG_12, L_PAR, NERS%FCS%BSPL_E12, &
     &                         NT, TIM_RES, VAL_RES, IER )
!
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 5, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,1,2,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,1,2,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1,1,2,EOPS__A), &
     &                         NM_12,       TIM12_MOD,  E12_MOD, E12_MOD_SPL, &
     &                         NERS%FCS%NK_12, NERS%FCS%ARG_12, L_PAR, NERS%FCS%BSPL_E12, &
     &                         NT, TIM_RES, VAL_RES, IER )
      END IF
!
      IF ( EOP%IVRB .EQ. 33 ) THEN
!
! -------- Make plot of E3 and E3 rate in the debugging mode
!
           L_PAR = NERS%FCS%NK_3 + DEG - 1
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 3, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,3,1,EOPS__C), &
     &                         NP(EOPS__I), TIM(1,EOPS__I), E(1,3,1,EOPS__I), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1,3,1,EOPS__A), &
     &                         NM_3, TIM3_MOD,  E3_MOD, E3_MOD_SPL, &
     &                         NERS%FCS%NK_3, NERS%FCS%ARG_3, L_PAR, NERS%FCS%BSPL_E3, &
     &                         NT, TIM_RES, VAL_RES, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 6, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,3,2,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,3,2,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1:NP(EOPS__A),3,2,EOPS__A) - SHIFT(3,2,EOPS__A) , &
     &                         NM_3, TIM3_MOD,  E3_MOD, E3_MOD_SPL, &
     &                         NERS%FCS%NK_3, NERS%FCS%ARG_3, L_PAR, NERS%FCS%BSPL_E3, &
     &                         NT, TIM_RES, VAL_RES, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_EOP_FCS ( 2, 9, MP, &
     &                         NP(EOPS__C), TIM(1,EOPS__C), E(1,3,2,EOPS__C), &
     &                         NP(EOPS__R), TIM(1,EOPS__R), E(1,3,2,EOPS__R), &
     &                         NP(EOPS__A), TIM(1,EOPS__A), E(1:NP(EOPS__A),3,2,EOPS__A) - SHIFT(3,2,EOPS__A) , &
     &                         NM_3, TIM3_MOD,  E3_MOD, E3_MOD_SPL, &
     &                         NERS%FCS%NK_3, NERS%FCS%ARG_3, L_PAR, NERS%FCS%BSPL_E3, &
     &                         NT, TIM_RES, VAL_RES, IER )
!
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL EOP_NERS_UTCMTAI ( GET_UTC(), NERS, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6718, IUER, 'EOP_FCS', 'Failure in computing '// &
     &         'UTC minus TAI on the current moment of time' )
           RETURN 
      END IF
!
! --- Collect dates of last value in EOP series
!
      NERS%FCS%TAI_LAST_EOPS_C = TIM(NP(EOPS__C),EOPS__C) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_L = TIM(NP(EOPS__L),EOPS__L) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_U = TIM(NP(EOPS__U),EOPS__U) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_R = TIM(NP(EOPS__R),EOPS__R) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_I = TIM(NP(EOPS__I),EOPS__I) - UTC_M_TAI
      IF ( NP(EOPS__J) > 0 ) THEN
           NERS%FCS%TAI_LAST_EOPS_J = TIM(NP(EOPS__J),EOPS__J) - UTC_M_TAI
         ELSE
           NERS%FCS%TAI_LAST_EOPS_J = TIM_LAST(EOPS__J) - UTC_M_TAI
      END IF
      NERS%FCS%TAI_LAST_EOPS_S = TIM(NP(EOPS__S),EOPS__S) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_F = TIM(NP(EOPS__F),EOPS__F) - UTC_M_TAI
      NERS%FCS%TAI_LAST_EOPS_A = TIM(NP(EOPS__A),EOPS__A) - UTC_M_TAI
!
      IF ( EOP%IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'TAI_LAST_EOPS_C=  ', NERS%FCS%TAI_LAST_EOPS_C, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_C, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_L=  ', NERS%FCS%TAI_LAST_EOPS_L, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_L, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_U=  ', NERS%FCS%TAI_LAST_EOPS_U, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_U, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_R=  ', NERS%FCS%TAI_LAST_EOPS_R, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_R, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_I=  ', NERS%FCS%TAI_LAST_EOPS_I, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_I, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_J=  ', NERS%FCS%TAI_LAST_EOPS_J, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_J, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_S=  ', NERS%FCS%TAI_LAST_EOPS_S, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_S, IER )
           WRITE ( 6, * ) 'TAI_LAST_EOPS_F=  ', NERS%FCS%TAI_LAST_EOPS_F, TIM_TO_DATE ( NERS%FCS%TAI_LAST_EOPS_F, IER )
           WRITE ( 6, * ) 'ARG_3_FIRST =     ', NERS%FCS%ARG_3(1),  TIM_TO_DATE ( NERS%FCS%ARG_3(1),  IER )
           WRITE ( 6, * ) 'ARG_3_LAST  =     ', NERS%FCS%ARG_3(NERS%FCS%NK_3),   TIM_TO_DATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3),   IER )
           WRITE ( 6, * ) 'ARG_12_FIRST=     ', NERS%FCS%ARG_12(1), TIM_TO_DATE ( NERS%FCS%ARG_12(1), IER )
           WRITE ( 6, * ) 'ARG_12_LAST=      ', NERS%FCS%ARG_12(NERS%FCS%NK_12), TIM_TO_DATE ( NERS%FCS%ARG_12(NERS%FCS%NK_12), IER )
           WRITE ( 6, * ) 'EOP%CONF%E3Z_MOD= ', EOP%CONF%E3Z_MOD
      END IF
!
      NERS%FCS%NERS_FMT     = NERS__BIN_FMT  
      NERS%FCS%TAI_GEN      = GET_UTC() - UTC_M_TAI
      NERS%FCS%NERS_URL     = EOP%CONF%NERS_URL
      NERS%FCS%EOP_FCS_VERS = EOP_FCS__LABEL
      NERS%FCS%NUT_APR_MOD  = EOP%CONF%NUT_APR_MOD
      NERS%FCS%PRC_APR_MOD  = EOP%CONF%PRC_APR_MOD 
      NERS%FCS%E3Z_APR_MOD  = EOP%CONF%E3Z_MOD
      NERS%FCS%HEO_MOD      = EOP%CONF%HEO_MOD
      NERS%FCS%HEO_ID       = EOP%CONF%HEO_ID
      NERS%FCS%EANG_MOD     = EANG_MOD
      NERS%FCS%LTP_MOD      = EOP%CONF%LTP_MOD
!
      NERS%FCS%URL_C = EOP%CONF%URL_EOP(EOPS__C)
      NERS%FCS%URL_U = EOP%CONF%URL_EOP(EOPS__U)
      NERS%FCS%URL_R = EOP%CONF%URL_EOP(EOPS__R)
      NERS%FCS%URL_I = EOP%CONF%URL_EOP(EOPS__I)
      NERS%FCS%URL_J = EOP%CONF%URL_EOP(EOPS__J)
      NERS%FCS%URL_S = EOP%CONF%URL_EOP(EOPS__S)
      NERS%FCS%URL_F = EOP%CONF%URL_EOP(EOPS__F)
      NERS%FCS%URL_L = EOP%CONF%URL_EOP(EOPS__L)
      NERS%FCS%URL_A = EOP%CONF%URL_AAM_SER
      CALL CLRCH ( NERS%FCS%URL_RESERVED )
!
      DEALLOCATE ( SER%EOP_WEI ) 
      DEALLOCATE ( SER%EOP_SIG ) 
      DEALLOCATE ( SER%EOP_VAL ) 
      DEALLOCATE ( SER%EOP_TIM ) 
!
      DEALLOCATE ( TIM3_MOD    ) 
      DEALLOCATE ( E3_MOD      ) 
      DEALLOCATE ( E3_MOD_SPL  ) 
      DEALLOCATE ( E12_MOD_SPL ) 
      DEALLOCATE ( TIM12_MOD   ) 
      DEALLOCATE ( E12_MOD     ) 
!
      NERS%FCS_STATUS = NERS__COMP 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_FCS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_CMPR ( ICMP, NPR, TIMR, ER, SPLR, &
     &                            NPC, TIMC, EC, SIGC, SHIFT, WRMS, &
     &                            ADD_WEI, IEOP, IVRB )
! ************************************************************************
! *                                                                      *
! *   Routine EOP_CMPR compares the time series
! *   TIMR, ER -- reference EOP series
! *   TIMC, EC -- the series to compare
! *                                                                      *
! *  ### 08-MAR-2016    EOP_CMPR   v1.0 (c)  L. Petrov  08-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INTEGER*4  ICMP, NPR, NPC, IEOP, IVRB
      REAL*8     TIMR(NPR), ER(NPR), SPLR(NPR), TIMC(NPC), EC(NPC), SIGC(NPC), &
     &           SHIFT, WRMS, ADD_WEI, CNST_SHIFT
      REAL*8     SHR, NSIG, NSIG_F, NSIG_I, TIM_MIN_DIF
      PARAMETER  ( SHR    =  0.4D0 )
      PARAMETER  ( NSIG_F = 40.0D0 )
      PARAMETER  ( NSIG_I =  5.0D0 )
      PARAMETER  ( TIM_MIN_DIF = 4.0*3600.0D0 )
      REAL*8     WW, ADD_WEI_MIN, ADD_WEI_MAX, MAX_OUT, CHI, CHI_MIN, R1, R2, &
     &           PLOT_SHIFT, PLOT_TREND
      LOGICAL*1, ALLOCATABLE :: FL_USE(:)
      REAL*8,    ALLOCATABLE :: VALC(:), TDIF(:), DIFC(:), DIFE(:), &
     &                          VALR(:), ERRR(:), PLOC(:)
      INTEGER*4  M_AWI, M_ASI
      PARAMETER  ( M_ASI =  8 )
      PARAMETER  ( M_AWI = 16 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IP_LAST, NPM, &
     &           IND_CHI_MIN, ND, IP, KP, ITER, NOUT, IND_OUT, IER
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8
!
      IF ( IEOP == EOPS__I .OR. IEOP == EOPS__J ) THEN
           NSIG = NSIG_I
         ELSE 
           NSIG = NSIG_F
      END IF
!
      ALLOCATE ( FL_USE(NPC) )
      ALLOCATE ( VALC(NPC) )
      KP = 0
      IP_LAST = 1
      DO 410 J1=1,NPC
!
! ------ Find the closest epoch for the referemce series
!
         IP = IXMN8 ( NPR, TIMR, TIMC(J1) )
         IF ( IP > 0 ) THEN
              KP = KP + 1
              FL_USE(J1) = .TRUE.
            ELSE
              FL_USE(J1) = .FALSE.
         END IF
         IF ( ICMP == 6 .AND. IEOP == EOPS__A ) THEN
              EC(J1) = EC(J1)*OM__EAR
         END IF
         IF ( J1 > 1 ) THEN
              IF ( TIMC(J1) - TIMC(J1-1) < TIM_MIN_DIF ) THEN
                   IF ( SIGC(J1) > SIGC(J1-1) ) THEN
                        FL_USE(J1) = .FALSE.
                      ELSE
                        FL_USE(J1-1) = .FALSE.
                   END IF
              END IF
         END IF
         IF ( FL_USE(J1) ) THEN
              IP_LAST = MAX ( IP_LAST, IP )
         END IF
 410  CONTINUE 
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6 ,* )'EOP_FCS: KP = ', KP, ' IP_LAST= ', IP_LAST, ' TIMR_LAST= ', TIMR(IP_LAST)
           CALL FLUSH ( 6 )
      END IF
!
      ITER = KP*SHR
      NOUT = 0
      DO 420 J2=1,ITER
         SHIFT = 0.0D0
         WW = 0.0D0
         NPM = 0
         DO 430 J3=1,NPC
            IF ( FL_USE(J3) ) THEN
                 IP = IXMN8 ( NPR, TIMR, TIMC(J3) )
                 IF ( ICMP .LE. 3 ) THEN
                      VALC(J3) = FSPL8 ( TIMC(J3), NPR, TIMR, ER, IP, SPLR )
                    ELSE
                      VALC(J3) = DSPL8 ( TIMC(J3), NPR, TIMR, ER, IP, SPLR )
                 END IF
                 WW = WW + 1.D0/SIGC(J3)
                 SHIFT = SHIFT + (EC(J3) - VALC(J3))/SIGC(J3)
                 NPM = J3
            END IF
 430     CONTINUE 
         SHIFT = SHIFT/WW
         WW = 0.0D0
         WRMS = 0.0D0
         DO 440 J4=1,NPC
            IF ( FL_USE(J4) ) THEN
                 WW = WW + 1.D0/SIGC(J4)
                 WRMS = WRMS + (EC(J4) - VALC(J4) - SHIFT)**2/SIGC(J4)
            END IF
 440     CONTINUE 
         WRMS = DSQRT ( WRMS/WW )
         MAX_OUT = -1.0D0
         IND_OUT =  0
         DO 450 J5=1,NPC
            IF ( FL_USE(J5) ) THEN
                 IF ( DABS(EC(J5) - VALC(J5) - SHIFT)/SIGC(J5) > MAX_OUT ) THEN
                      MAX_OUT = DABS(EC(J5) - VALC(J5) - SHIFT)/SIGC(J5)
                      IND_OUT = J5
                 END IF
            END IF
 450     CONTINUE 
         IF ( MAX_OUT > NSIG ) THEN
              NOUT = NOUT + 1
              FL_USE(IND_OUT) = .FALSE.
           ELSE   
              GOTO 820
         END IF
 420  CONTINUE 
 820  CONTINUE 
!
! --- Compute additive weight correction
!
      ADD_WEI_MIN = 0.0D0
      ADD_WEI_MAX = 8.*WRMS
      DO 460 J6=1,M_ASI  ! external loop
         WW = 0.0D0
         CHI_MIN = 1.0D8
         IND_CHI_MIN = -1
         DO 470 J7=1,M_AWI  ! internal loop
            ADD_WEI = ADD_WEI_MIN + (J7-1)*(ADD_WEI_MAX - ADD_WEI_MIN)/(M_AWI-1)
            KP = 0
            CHI = 0.0D0
            DO 480 J8=1,NPC
               IF ( FL_USE(J8) ) THEN
                    CHI = CHI + (EC(J8) - VALC(J8) - SHIFT)**2/(SIGC(J8)**2 + ADD_WEI**2)
                    KP = KP + 1
               END IF
 480        CONTINUE 
            CHI = CHI/(KP-1)
            IF ( ABS(CHI - 1.0D0) < ABS(CHI_MIN - 1.0D0) ) THEN
                 CHI_MIN = CHI
                 IND_CHI_MIN = J7
            END IF
 470     CONTINUE 
         ADD_WEI = ADD_WEI_MIN + (IND_CHI_MIN-1)*(ADD_WEI_MAX - ADD_WEI_MIN)/(M_AWI-1)
         R1 = ADD_WEI - (ADD_WEI_MAX - ADD_WEI_MIN)/4.0D0
         IF ( R1 < 0.0D0 ) R1 = 0.0D0
         R2 = ADD_WEI + (ADD_WEI_MAX - ADD_WEI_MIN)/4.0D0
         ADD_WEI_MIN = R1
         ADD_WEI_MAX = R2
 460  CONTINUE 
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 110 ) EOPS__NAME(IEOP), ICMP, SHIFT, WRMS, ADD_WEI, KP, NOUT
 110       FORMAT ( 'EOP_CMPR  ', A, ' Icmp: ', I1, ' Shift: ', 1PD14.7, &
     &              ' wmrs: ', 1PD11.4, ' add: ', 1PD11.4, ' KP: ', I6, ' Nout: ', I6 )
      END IF
      IF ( IVRB == 32 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 2 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 4 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 0 )
           CALL DIAGI_2 ( NPC, TIMC, EC, NPM, TIMC, VALC, IER )
      END IF
      IF ( IVRB == 41 ) THEN
           ALLOCATE ( TDIF(NPC) )
           ALLOCATE ( DIFC(NPC) )
           ALLOCATE ( PLOC(NPC) )
           ALLOCATE ( DIFE(NPC) )
           ALLOCATE ( VALR(NPC) )
           ALLOCATE ( ERRR(NPC) )
           ND = 0
           PLOT_SHIFT = EC(1) 
           PLOT_TREND = (EC(NPC) - EC(1))/(TIMC(NPC) - TIMC(1))
           DO 490 J9=1,NPC
              IF ( FL_USE(J9) ) THEN
                   ND = ND + 1
                   TDIF(ND) = TIMC(J9)/(365.25*86400.0D0) + 2000.0D0
                   DIFC(ND) = EC(J9) - VALC(J9) - SHIFT
                   DIFE(ND) = DSQRT( SIGC(J9)**2 + ADD_WEI**2)
                   VALR(ND) = EC(J9)   - ( PLOT_SHIFT + PLOT_TREND*(TIMC(J9) - TIMC(1)) )
                   PLOC(ND) = VALC(J9) - ( PLOT_SHIFT + PLOT_TREND*(TIMC(J9) - TIMC(1)) )
                   ERRR(ND) = 1.D-11
              END IF
 490       CONTINUE 
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 3 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 4 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 2 )
           CALL DIAGI_2E ( ND, TDIF, VALR, ERRR, ND, TDIF, PLOC, DIFE, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 5 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 2 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 1 )
           CALL DIAGI_1E ( ND, TDIF, DIFC, DIFE, IER )
           DEALLOCATE ( TDIF )
           DEALLOCATE ( DIFC )
           DEALLOCATE ( DIFE )
           DEALLOCATE ( VALR )
           DEALLOCATE ( ERRR )
           DEALLOCATE ( PLOC )
        ELSE IF ( IVRB == 42 ) THEN
          ALLOCATE ( TDIF(NPC) )
          ALLOCATE ( PLOC(NPC) )
          KP = 0
          DO 4100 J10=1,NPC
             IF ( FL_USE(J10) ) THEN
                  KP = KP + 1
                  IP = IXMN8 ( NPR, TIMR, TIMC(J10) )
                  VALC(KP) = D2SPL8 ( TIMC(J10), NPR, TIMR, ER, IP, SPLR )
                  TDIF(KP) = (TIMC(J10) - TIMC(1))/86400.0D0
             END IF
 4100     CONTINUE 
          IF ( ICMP == 1 ) THEN
               CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E1 total acceleration' )
               CALL DIAGI_1 ( KP, TDIF, VALC, IER )
             ELSE IF ( ICMP == 2 ) THEN
               CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E2 total acceleration' )
               CALL DIAGI_1 ( KP, TDIF, VALC, IER )
             ELSE IF ( ICMP == 3 ) THEN
               CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E3 total acceleration' )
               CALL DIAGI_1 ( KP, TDIF, VALC, IER )
          END IF
          DEALLOCATE ( TDIF )
          DEALLOCATE ( PLOC )
      END IF
      DEALLOCATE ( VALC   )
      DEALLOCATE ( FL_USE )
      RETURN
      END  SUBROUTINE  EOP_CMPR  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE AAM_CMPR ( ICMP, MP, NPR, TIMR, ER, SPLR, &
     &                                NPA, TIMA, EA, SHIFT, WRMS, &
     &                                ADD_WEI, IVRB )
! ************************************************************************
! *                                                                      *
! *   Routine AAM_CMPR
! *                                                                      *
! *  ### 08-MAR-2016    AAM_CMPR   v1.0 (c)  L. Petrov  08-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INTEGER*4  ICMP, MP, NPR, NPA, IVRB
      REAL*8     TIMR(NPR), ER(MP,3), SPLR(MP,3), TIMA(NPA), EA(MP,3), &
     &           SHIFT, WRMS, ADD_WEI
      REAL*8     SHR, NSIG, NSIG_F, NSIG_I
      PARAMETER  ( SHR    =  0.4D0 )
      PARAMETER  ( NSIG_F = 40.0D0 )
      PARAMETER  ( NSIG_I =  3.5D0 )
      REAL*8     WW, ADD_WEI_MIN, ADD_WEI_MAX, MAX_OUT, CHI, CHI_MIN, &
     &           VAL1, DER1, VAL2, DER2, R1, R2
      LOGICAL*1, ALLOCATABLE :: FL_USE(:)
      REAL*8,    ALLOCATABLE :: VALC(:), TDIF(:), DIFC(:), DIFE(:)
      REAL*8       CHAN_FREQ, CHAN_DAMP
      PARAMETER  ( CHAN_FREQ = PI2/(86400.0D0*430.3D0) )
      PARAMETER  ( CHAN_DAMP = 100.0D0 )
      INTEGER*4  M_AWI, M_ASI
      PARAMETER  ( M_ASI =  8 )
      PARAMETER  ( M_AWI = 16 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           IND_CHI_MIN, ND, IP, KP, ITER, NOUT, IND_OUT, IER
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: FSPL8, DSPL8
!
      NSIG = NSIG_I
!
      ALLOCATE ( FL_USE(NPA) )
      ALLOCATE ( VALC(NPA) )
      KP = 0
      DO 410 J1=1,NPA
         IP = IXMN8 ( NPR, TIMR, TIMA(J1) )
         IF ( IP > 0 ) THEN
              KP = KP + 1
              FL_USE(J1) = .TRUE.
            ELSE
              FL_USE(J1) = .FALSE.
         END IF
 410  CONTINUE 
!
      ITER = KP*SHR
      NOUT = 0
      SHIFT = 0.0D0
      WW = 0.0D0
      DO 430 J3=1,NPA
         IF ( FL_USE(J3) ) THEN
              IP = IXMN8 ( NPR, TIMR, TIMA(J3) )
              IF ( ICMP .EQ. 1 ) THEN
                   VAL1 = FSPL8 ( TIMA(J3), NPR, TIMR, ER(1,1), IP, SPLR(1,1) )
                   DER1 = DSPL8 ( TIMA(J3), NPR, TIMR, ER(1,1), IP, SPLR(1,1) )
                   VAL2 = FSPL8 ( TIMA(J3), NPR, TIMR, ER(1,2), IP, SPLR(1,2) )
                   DER2 = DSPL8 ( TIMA(J3), NPR, TIMR, ER(1,2), IP, SPLR(1,2) )
                   VALC(J3) =  VAL2 + DER1/CHAN_FREQ + DER2/(2.0D0*CHAN_DAMP*CHAN_FREQ)
                ELSE IF ( ICMP .EQ. 2 ) THEN
                   VAL1 = FSPL8 ( TIMA(J3), NPR, TIMR, ER(1,1), IP, SPLR(1,1) )
                   DER1 = DSPL8 ( TIMA(J3), NPR, TIMR, ER(1,1), IP, SPLR(1,1) )
                   VAL2 = FSPL8 ( TIMA(J3), NPR, TIMR, ER(1,2), IP, SPLR(1,2) )
                   DER2 = DSPL8 ( TIMA(J3), NPR, TIMR, ER(1,2), IP, SPLR(1,2) )
                   VALC(J3) = -VAL1 + DER2/CHAN_FREQ - DER1/(2.0D0*CHAN_DAMP*CHAN_FREQ)
                ELSE IF ( ICMP .EQ. 3 ) THEN
                   VALC(J3) = DSPL8 ( TIMA(J3), NPR, TIMR, ER(1,ICMP), IP, SPLR(1,ICMP) ) 
              END IF
              WW = WW + 1.0D0
              SHIFT = SHIFT + (EA(J3,ICMP) - VALC(J3))
         END IF
 430  CONTINUE 
      IF ( WW > 1.D-20 ) THEN
           SHIFT = SHIFT/WW
      END IF
      WW = 0.0D0
      WRMS = 0.0D0
      DO 440 J4=1,NPA
         IF ( FL_USE(J4) ) THEN
              WW = WW + 1.0D0
              WRMS = WRMS + (EA(J4,ICMP) - VALC(J4) - SHIFT)**2
         END IF
 440  CONTINUE 
      WRMS = DSQRT ( WRMS/WW )
      ADD_WEI = WRMS
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 110 ) EOPS__NAME(EOPS__A), ICMP, SHIFT, WRMS, KP, NOUT
 110       FORMAT ( 'AAM_CMPR  ', A, ' Icmp: ', I1, ' Shift: ', 1PD14.7, &
     &              ' wmrs: ', 1PD14.7, ' KP: ', I6, ' Nout: ', I6 )
      END IF
      IF ( IVRB .GE. 32 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 2 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 4 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 0 )
           CALL DIAGI_2 ( NPA, TIMA/86400.d0, EA(1,ICMP), NPA, TIMA/86400.0d0, VALC + SHIFT, IER )
      END IF
      IF ( IVRB .GE. 33 ) THEN
           ALLOCATE ( TDIF(NPA) )
           ALLOCATE ( DIFC(NPA) )
           ND = 0
           DO 490 J9=1,NPA
              IF ( FL_USE(J9) ) THEN
                   ND = ND + 1
                   TDIF(ND) = TIMA(J9)
                   DIFC(ND) = EA(J9,ICMP) - VALC(J9) - SHIFT
              END IF
 490       CONTINUE 
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 1 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 5 )
           CALL DIAGI_1 ( ND, TDIF, DIFC, IER )
           DEALLOCATE ( TDIF )
           DEALLOCATE ( DIFC )
      END IF
      DEALLOCATE ( VALC   )
      DEALLOCATE ( FL_USE )
      RETURN
      END  SUBROUTINE  AAM_CMPR  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TEST_E3_PLOT ( EOP, MODE, &
     &                       NPI, TIMI, EI, SIGI, &
     &                       NPJ, TIMJ, EJ, SIGJ, &
     &                       NPC, TIMC, EC, SIGC, &
     &                       NPM, TIMM, EM, ESPL, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      INTEGER*4  NPI, NPJ, NPC, NPM, IUER
      REAL*8     TIMI(NPI), EI(NPI), SIGI(NPI)
      REAL*8     TIMJ(NPJ), EJ(NPJ), SIGJ(NPJ)
      REAL*8     TIMC(NPC), EC(NPC), SIGC(NPC)
      REAL*8     TIMM(NPM), EM(NPM), ESPL(NPM), MOD 
      REAL*8,    ALLOCATABLE :: TI(:), TJ(:), TC(:), &
     &                          VI(:), VJ(:), VC(:), &
     &                          SI(:), SJ(:), SC(:)
      INTEGER*4  J1, J2, J3, IP, KI, KJ, KC, MODE, IER
      REAL*8,    EXTERNAL :: FSPL8
      INTEGER*4, EXTERNAL :: IXMN8
!
      ALLOCATE ( TI(NPI) )
      ALLOCATE ( VI(NPI) )
      ALLOCATE ( SI(NPI) )
      ALLOCATE ( TJ(NPJ) )
      ALLOCATE ( VJ(NPJ) )
      ALLOCATE ( SJ(NPJ) )
      ALLOCATE ( TC(NPC) )
      ALLOCATE ( VC(NPC) )
      ALLOCATE ( SC(NPC) )
!
      KI = 0
      DO 410 J1=1,NPI
         IP = IXMN8 ( NPM, TIMM, TIMI(J1) )
         IF ( IP > 1 ) THEN
              KI = KI + 1 
              TI(KI) = TIMI(J1)
              MOD = FSPL8 ( TI(KI), NPM, TIMM, EM, IP, ESPL )
              TI(KI) = (TI(KI) - TIMC(1))/86400.0D0
              VI(KI) = EI(J1) - MOD
              SI(KI) = SIGI(J1)
         END IF
 410  CONTINUE 
!
      KJ = 0
      DO 420 J2=1,NPJ
         IP = IXMN8 ( NPM, TIMM, TIMJ(J2) )
         IF ( IP > 1 ) THEN
              KJ = KJ + 1 
              TJ(KJ) = TIMJ(J2)
              MOD = FSPL8 ( TJ(KJ), NPM, TIMM, EM, IP, ESPL )
              TJ(KJ) = (TJ(KJ) - TIMC(1))/86400.0D0
              VJ(KJ) = EJ(J2) - MOD
              SJ(KJ) = SIGJ(J2)
         END IF
 420  CONTINUE 
!
      KC = 0
      DO 430 J3=1,NPC
         IP = IXMN8 ( NPM, TIMM, TIMC(J3) )
         IF ( IP > 1 ) THEN
              KC = KC + 1 
              TC(KC) = TIMC(J3)
              MOD = FSPL8 ( TC(KC), NPM, TIMM, EM, IP, ESPL )
              TC(KC) = (TC(KC) - TIMC(1))/86400.0D0
              VC(KC) = EC(J3) - MOD
              SC(KC) = SIGC(J3)
         END IF
 430  CONTINUE 
!        call diagi_1 ( npc, timc, ec, ier ) ! %%%%%%%%%%
!        call diagi_1 (  kc, tc, vc, ier ) ! %%%%%%%%%%
!
      CALL DIAGI_3E ( KI, TI, VI, SI, KJ, TJ, VJ, SJ, KC, TC, VC, SC, IER )
!!      CALL DIAGI_2E ( KI, TI, VI, SI, KC, TC, VC, SC, IER )
!!      CALL DIAGI_2E ( NPI, TIMI, EI, SIGI, NPC, TIMC, EC, SIGC, IER )
!
      DEALLOCATE ( TI )
      DEALLOCATE ( VI )
      DEALLOCATE ( SI )
      DEALLOCATE ( TJ )
      DEALLOCATE ( VJ )
      DEALLOCATE ( SJ )
      DEALLOCATE ( TC )
      DEALLOCATE ( VC )
      DEALLOCATE ( SC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TEST_E3_PLOT  !#!#
