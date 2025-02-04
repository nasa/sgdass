      SUBROUTINE VTD_LOAD_OBJ_NZO ( NZO_NAME, OBJ_TYPE, OBJ_USED, NZO_REF,  &
     &                              TIM_CODE, VTD, L_ARR, MJD_ARR, TAI_ARR, &
     &                              POS_ARR, L_NOD, L_DEG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_OBJ_NZO  computes coefficients of the B-spline   *
! *   expansion of coordinates of the near zone object and loads them    *
! *   in the appropriate fields of the VTD object. Coordinates of the    *
! *   near-zone objects are supposed to be read by VTD_READ_NZO. These   *
! *   coordinates and time tags are passed to VTD_LOAD_OBJ_NZO.          *
! *                                                                      *
! *   The coefficients of the expansion will be used by other routines   *
! *   of the VTD library for computation of position, velocity and       *
! *   acceleration of the near zone object for calculation of VLBI       *
! *   path delay and/or Doppler frequency shift.                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * NZO_NAME ( CHARACTER ) -- Name of the near zone object.              *
! * OBJ_TYPE ( CHARACTER ) -- Type of the near zone object.              *
! *                           Support types:                             *
! *                           VTD__MG  -- Extra-galactic object.         *
! *                           VTD__GAL -- Galactic object.               *
! *                           VTD__SS  -- Solar system object.           *
! *                           VTD__ES  -- Near Earth object.             *
! * OBJ_USED ( CHARACTER ) -- Type of the near zone object.              *
! *                           VTD__EM  -- Observed emitter.              *
! *                           VTD__OR  -- Observing orbiting antenna.    *
! *                           VTD__ES  -- Observed Earth saetellite.     *
! * TIM_CODE ( INTEGER*4 ) -- Time code of time tags. One of             *
! *                           VTD__TDB, VTD__TDT, VTD__UTC, VTD__TAI.    *
! *    L_ARR ( INTEGER*4 ) -- The number of points in the array of       *
! *                           the near zone object positions.            *
! *  MJD_ARR ( INTEGER*4 ) -- Array of MJD dates of position arrays of   *
! *                           near zone objects. Dimension: L_ARR.       *
! *  TAI_ARR ( REAL*8    ) -- Array of TAU time tags of dates of         *
! *                           position arrays of near zone objects.      *
! *                           Dimension: L_ARR.                          *
! *  POS_ARR ( REAL*8    ) -- Array of positions of the near zone        *
! *                           object. Units: meters. Dimension: L_ARR,3. *
! *    L_NOD ( INTEGER*4 ) -- The number of nodes for the B-spline basis *
! *                           of the expansion.                          *
! *    L_DEG ( INTEGER*4 ) -- The degree of the B-spline basis.          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 01-JAN-2006  VTD_LOAD_OBJ_NZO v3.3 (c) L. Petrov 02-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  TIM_CODE, L_ARR, L_NOD, L_DEG, MJD_ARR(L_ARR), NZO_REF, IUER
      CHARACTER  NZO_NAME*(*), OBJ_TYPE*1, OBJ_USED*1
      REAL*8     TAI_ARR(L_ARR), POS_ARR(VTD__MEPH,3)
      CHARACTER  STR*128, STR1*128, STR2*128, VTD_DIFX_COMPAT*3
      REAL*8,    ALLOCATABLE :: EQU_MAT(:,:), NOR_MAT(:), NOR_VEC(:,:), &
     &                          TARG_ARR(:), MOON_POT(:), RLT_ARR(:)
      REAL*8     TIM_ARG, TIM_STEP, TAI_M_TDB, TAI, RCOND, &
     &           MOON_COO(3), MOON_VEL(3), MOON_ACC(3), &
     &           EARTH_COO(3), EARTH_VEL(3), EARTH_ACC(3), VEL_VEC(3), &
     &           MOON_DIST, EARTH_DIST, EARTH_POT, ORB_TO_MOON(3), &
     &           TIM_INP_STEP, TDB_ARG, UTC_M_TAI
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           KNOT, IND_NZO, L_PAR, LL_PAR, MJD_ARG, IER
      REAL*8     TIM__MARGIN
      PARAMETER  ( TIM__MARGIN = 0.1D0 )
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V
      LOGICAL*1  FL_COMPAT_UTC, FL_COMPAT_MOON
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_COMPAT_UTC  = .FALSE.
      CALL GETENVAR ( 'VTD_DIFX_COMPAT', VTD_DIFX_COMPAT )
      IF ( VTD_DIFX_COMPAT == 'yes' .OR. VTD_DIFX_COMPAT == 'YES' ) THEN
           FL_COMPAT_MOON = .TRUE.
         ELSE 
           FL_COMPAT_MOON = .FALSE.
      END IF
      IF ( OBJ_TYPE == VTD__ES .OR. OBJ_TYPE == VTD__SS ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 3721, IUER, 'VTD_LOAD_OBJ_NZO', 'Unsupported '// &
     &         'OBJ_TYPE parameter: '//OBJ_TYPE )
           RETURN 
      END IF
!
      IND_NZO = 0
      IF ( VTD%L_NZO > 0 ) THEN
!
! -------- Find the index of the near zone object in the NZO list 
!
           DO 410 J1=1,VTD%L_SOU
              IF ( NZO_NAME == VTD%NZO(J1)%NAME ) IND_NZO = J1
 410       CONTINUE 
      END IF
!
! --- Did not find?  Add the new object in the list
!
      IF ( IND_NZO == 0 ) THEN
           VTD%L_NZO = VTD%L_NZO + 1
           IND_NZO   = VTD%L_NZO 
      END IF
      IF ( VTD%L_NZO + 1 > VTD__M_NZO ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( VTD__M_NZO, STR )
           CALL ERR_LOG ( 3721, IUER, 'VTD_LOAD_OBJ_NZO', 'Too many near '// &
     &         'zone objects -- exceeded the maximum VTD__M_NZO '//STR )
           RETURN 
      END IF
      ALLOCATE ( VTD%NZO(IND_NZO)%TIM_ARR(1-VTD__M_SPL:L_NOD),             &
     &           VTD%NZO(IND_NZO)%SPL_ARR(1-VTD__M_SPL:L_NOD,3),           &
     &           VTD%NZO(IND_NZO)%SPL_RLT_ARR(1-VTD__M_SPL:L_NOD),         &
     &           VTD%NZO(IND_NZO)%PHASE_WINDUP(VTD%L_STA,VTD%L_STA),       & 
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( VTD__M_NZO, STR )
           CALL ERR_LOG ( 3721, IUER, 'VTD_LOAD_OBJ_NZO', 'Error in '// &
     &         'an attempt to allocate dynamic memory for arrays with '// &
     &         'the ephemeride' )
           RETURN 
      END IF
      VTD%NZO(IND_NZO)%PHASE_WINDUP = 0.0D0 
!
      VTD%NZO(IND_NZO)%NAME = NZO_NAME ! store the object name
      VTD%NZO(IND_NZO)%TIM_CODE = TIM_CODE
!!      VTD%NZO(IND_NZO)%NZO_COO_SYS    = 
!!      VTD%NZO(IND_NZO)%NZO_COO_ORIGIN = 
      VTD%NZO(IND_NZO)%OBJ_TYPE = OBJ_TYPE
      VTD%NZO(IND_NZO)%OBJ_USED = OBJ_USED
      VTD%NZO(IND_NZO)%TIM_ARR  = 0.0D0
      VTD%NZO(IND_NZO)%SPL_ARR  = 0.0D0
      VTD%NZO(IND_NZO)%SPL_RLT_ARR  = 0.0D0
!
      IF ( L_NOD .NE. L_ARR .AND. L_ARR < L_NOD+L_DEG-1 ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           CALL CLRCH ( STR2 ) 
           CALL INCH  ( L_ARR, STR  )
           CALL INCH  ( L_DEG, STR1 )
           CALL INCH  ( L_NOD, STR2 )
           CALL ERR_LOG ( 3722, IUER, 'VTD_LOAD_OBJ_NZO', 'Too few points '// &
     &         'in the input array of positions of the new zone object '// &
     &          NZO_NAME(1:I_LEN(NZO_NAME))//' only '//STR(1:I_LEN(STR))// &
     &         ' which is not sufficient for a B-spline of degree '// &
     &          STR1(1:I_LEN(STR1))//' at '//STR2(1:I_LEN(STR2))//' nodes' )
           RETURN 
      END IF   
!
! --- Allocate memory for temporary arrays needed for computation of the 
! --- expansion over B-spline basis
!
      L_PAR = L_DEG-1+L_NOD
      LL_PAR = (L_PAR*(L_PAR+1))/2
      ALLOCATE ( TARG_ARR(L_ARR) )
      ALLOCATE ( MOON_POT(L_ARR) )
      ALLOCATE ( RLT_ARR(L_ARR)  )
      IF ( L_NOD < L_ARR ) THEN
           ALLOCATE ( EQU_MAT(L_PAR,L_ARR) )
           ALLOCATE ( NOR_VEC(L_PAR,3) )
           ALLOCATE ( NOR_MAT(LL_PAR) )
           CALL NOUT_R8 ( LL_PAR,  NOR_MAT )
           CALL NOUT_R8 ( L_PAR*3, NOR_VEC )
      END IF
!
      IF ( L_ARR > 1 ) THEN
           TIM_INP_STEP = (MJD_ARR(2) - MJD_ARR(1))*86400.0D0 + &
     &                    (TAI_ARR(2) - TAI_ARR(1))
      END IF
      DO 420 J2=1,L_ARR
         MJD_ARG = MJD_ARR(J2)
         TIM_ARG = TAI_ARR(J2)
!
! ------ Correct time argument for using different time scales.
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_UTC_TO_TAI ( VTD, MJD_ARG, TIM_ARG, TAI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3725, IUER, 'VTD_LOAD_OBJ_NZO', 'Error in '// &
     &            'an attempt to get UTC minus TAI difference' )
              RETURN 
         END IF
         UTC_M_TAI = TIM_ARG - TAI
!
         CALL TDB_TO_TAI ( MJD_ARG, TIM_ARG, TDB_ARG )
!
! ------ Convert time tag to TAI
!         
         IF ( TIM_CODE == VTD__UTC ) THEN
              TIM_ARG = TIM_ARG - UTC_M_TAI
           ELSE IF ( TIM_CODE == VTD__TDT ) THEN
              TIM_ARG = TIM_ARG - 32.184D0
           ELSE IF ( TIM_CODE == VTD__TDB ) THEN
              TIM_ARG = TDB_ARG
              CONTINUE 
           ELSE IF ( TIM_CODE == VTD__TAI ) THEN
              CONTINUE 
         END IF 
!
         IF ( J2 == 1 ) THEN
              VTD%NZO(IND_NZO)%MJD_BEG = MJD_ARG
              VTD%NZO(IND_NZO)%TIM_BEG = TIM_ARG
         END IF
!
! ------ Store time arguments
!
         TARG_ARR(J2) = ( TIM_ARG - VTD%NZO(IND_NZO)%TIM_BEG ) + &
     &                  ( MJD_ARG - VTD%NZO(IND_NZO)%MJD_BEG )*86400.0D0
!
         IF ( FL_COMPAT_UTC .AND. MJD_ARG .GE. 56110 ) TAI = TAI - 1.D0 ! %%%%
         IF ( J2 > 2 ) THEN
              IF ( DABS( TARG_ARR(J2) - TARG_ARR(J2-1) - TIM_INP_STEP ) > &
     &             TIM__MARGIN*TIM_INP_STEP                               ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   WRITE ( UNIT=STR1, FMT='(1PD14.6)' ) TARG_ARR(J2) - TARG_ARR(J2-1)
                   WRITE ( UNIT=STR2, FMT='(1PD14.6)' ) TIM_INP_STEP
                   CALL ERR_LOG ( 3726, IUER, 'VTD_LOAD_OBJ_NZO', 'Trap of '// &
     &                 'internal control: there is a gap of '// &
     &                  STR1(1:I_LEN(STR1))//' seconds after the record '// &
     &                  STR(1:I_LEN(STR))//' in the input file '// &
     &                  'with emeperide while a gap of '//STR2(1:I_LEN(STR2))// &
     &                  ' was expected. Please fix the gap in the '// &
     &                  'ephemeride file and try again' )
                  RETURN 
              END IF
         END IF
!
         IF ( OBJ_USED == VTD__OR  .AND.  NZO_REF == VTD__EME ) THEN
              CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_ARG, TAI, 'MOON    ', &
     &                              MOON_COO, MOON_VEL, MOON_ACC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 3727, IUER, 'VTD_MOMENT', 'Error in an '// &
     &                 'attempt to compute position, velocity and accelration '// &
     &                 'of the Moon on TAI moment of time '// &
     &                  MJDSEC_TO_DATE( MJD_ARG, TAI, IER ) )
                   RETURN
              END IF
!
              CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_ARG, TAI, 'EARTH   ', &
     &                              EARTH_COO, EARTH_VEL, EARTH_ACC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 3728, IUER, 'VTD_MOMENT', 'Error in an '// &
     &                 'attempt to compute position, velocity and accelration '// &
     &                 'of the Earth on TAI moment of time '// &
     &                  MJDSEC_TO_DATE( MJD_ARG, TAI, IER ) )
                   RETURN
              END IF
              MOON_COO = MOON_COO - EARTH_COO
              MOON_VEL = MOON_VEL - EARTH_VEL
              MOON_ACC = MOON_ACC - EARTH_ACC
              ORB_TO_MOON(1) = MOON_COO(1) - POS_ARR(J2,1)
              ORB_TO_MOON(2) = MOON_COO(2) - POS_ARR(J2,2)
              ORB_TO_MOON(3) = MOON_COO(3) - POS_ARR(J2,3)
              MOON_DIST = DSQRT ( DP_VV_V ( 3, ORB_TO_MOON, ORB_TO_MOON ) )
              MOON_POT(J2) = -VTD__GM(VTD__EART)/VTD%DE_EPH%EMRAT/VTD__C**2/MOON_DIST
              IF ( FL_COMPAT_MOON ) THEN
                   MOON_POT(J2) = 0.0D0
              END IF
         END IF
 420  CONTINUE 
!
! --- Set array of node epochs for the B-spline basis.
! --- First, set epochs for the first mutiple node.
!
      DO 440 J4=1-L_DEG,0
         VTD%NZO(IND_NZO)%TIM_ARR(J4) = TARG_ARR(1)
 440  CONTINUE 
!
! --- Second, set epochs for other nodes
!
      TIM_STEP = (TARG_ARR(L_ARR) - TARG_ARR(1))/(L_NOD-1)
      DO 450 J5=1,L_NOD
         VTD%NZO(IND_NZO)%TIM_ARR(J5) = TARG_ARR(1) + TIM_STEP*(J5-1)
 450  CONTINUE 
      IF ( L_NOD < L_ARR ) THEN
!
! -------- Now build the system of normal equations for all three components
!
           DO 460 J6=1,L_ARR
!
! ----------- Create an equation of conditions
!
              DO 470 J7=1-L_DEG,L_NOD-1
                 EQU_MAT(J7+L_DEG,J6) = BSPL_VAL ( L_NOD, &
     &                                             VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                             L_DEG, J7, TARG_ARR(J6) )
                 DO 480 J8=1,3
                    NOR_VEC(J7+L_DEG,J8) = NOR_VEC(J7+L_DEG,J8) + &
     &                                     EQU_MAT(J7+L_DEG,J6)*POS_ARR(J8,J6)
 480             CONTINUE 
 470          CONTINUE 
!
! ----------- Update the normal equation
!
              CALL DIAD_CVT_S ( 1.D0, L_PAR, EQU_MAT(1,J6), EQU_MAT(1,J6), NOR_MAT )
 460       CONTINUE 
!
! -------- Invert the matrix of normal equations
!
           CALL ERR_PASS ( IUER, IER )
           CALL INVS ( L_PAR, NOR_MAT, RCOND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3729, IUER, 'VTD_LOAD_OBJ_NZO', 'Error in '// &
     &                  'an attempt to invert normal matrix' )
                RETURN
           END IF
      END IF
!
! --- Get solution and sort it in the SPL_ARR field
!
      DO 490 J9=1,3
         IF ( L_NOD < L_ARR ) THEN
              CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC(1,J9), L_PAR, &
     &                           VTD%NZO(IND_NZO)%SPL_ARR(1-L_DEG,J9), IER )
            ELSE
              VTD%NZO(IND_NZO)%SPL_ARR(1:L_ARR,J9) = POS_ARR(1:L_ARR,J9)
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL_1D_CMP ( L_DEG, 0, L_ARR, VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                           VTD%NZO(IND_NZO)%SPL_ARR(1-L_DEG,J9), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 3729, IUER, 'VTD_LOAD_OBJ_NZO', 'Error in an '// &
     &                 'attempt to expand the orbit over B-spline basis' )
                   RETURN
              END IF
         END IF
 490  CONTINUE 
      VTD%NZO(IND_NZO)%DEG_SPL = L_DEG
      VTD%NZO(IND_NZO)%NOD_SPL = L_NOD
!
      IF ( OBJ_USED == VTD__OR  .AND.  NZO_REF == VTD__EME ) THEN
!
! -------- Compute clock rate due to velocity and grav potential
!
           NOR_VEC = 0.0D0
           DO 4100 J10=1,L_ARR
              IF ( J10 == 1 ) THEN
                   TIM_ARG = TARG_ARR(1) + (TARG_ARR(2) - TARG_ARR(1))*1.D-6
                 ELSE IF ( J10 == L_ARR ) THEN
                   TIM_ARG = TARG_ARR(L_ARR) - (TARG_ARR(L_ARR) - TARG_ARR(L_ARR-1))*1.D-6
                 ELSE
                   TIM_ARG = TARG_ARR(J10)
              END IF 
              KNOT = IXMN8 ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                       VTD%NZO(IND_NZO)%TIM_ARR(1), TIM_ARG )
              DO 4110 J11=1,3
                 VEL_VEC(J11) = 0.0D0
                 DO 4120 J12=KNOT-L_DEG,KNOT
                    VEL_VEC(J11) = VEL_VEC(J11) + VTD%NZO(IND_NZO)%SPL_ARR(J12,J11)* & 
     &                             BSPL_DER ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                        VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                        VTD%NZO(IND_NZO)%DEG_SPL, J12, TIM_ARG )
 4120            CONTINUE 
 4110         CONTINUE 
              EARTH_DIST   = DSQRT ( POS_ARR(J10,1)**2 + POS_ARR(J10,2)**2 + POS_ARR(J10,3)**2 )
              EARTH_POT    = VTD__GM(VTD__EART)/VTD__C**2*(1.D0/EARTH_DIST - 1.D0/VTD__REA)
!
! ----------- Take into account differences in geopotential 
! ----------- at the oribiting satellite and at the geoid
!
              EARTH_POT    = VTD__GM(VTD__EART)/VTD__C**2/EARTH_DIST - 6.969290134D-10 
              RLT_ARR(J10) =   DP_VV_V ( 3, VEL_VEC, VEL_VEC )/VTD__C**2/2.0D0 &
     &                       + EARTH_POT + MOON_POT(J10)
              IF ( L_NOD < L_ARR ) THEN
!
! ---------------- Update normal vector
!
                   DO 4130 J13=1-L_DEG,L_NOD-1
                      NOR_VEC(J13+L_DEG,1) = NOR_VEC(J13+L_DEG,1) + &
     &                                       EQU_MAT(J13+L_DEG,J10)*RLT_ARR(J10)
 4130              CONTINUE 
              END IF
 4100      CONTINUE 
!
           IF ( L_NOD < L_ARR ) THEN
!
! ------------- Solve equation using the previosly found the normal matix invert
!
                CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, &
     &                             VTD%NZO(IND_NZO)%SPL_RLT_ARR, IER )
              ELSE
                VTD%NZO(IND_NZO)%SPL_RLT_ARR(1:L_ARR) = RLT_ARR(1:L_ARR)
                CALL ERR_PASS ( IUER, IER )
                CALL BSPL_1D_CMP ( L_DEG, 0, L_ARR, VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                             VTD%NZO(IND_NZO)%SPL_RLT_ARR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 3730, IUER, 'VTD_LOAD_OBJ_NZO', 'Error in an '// &
     &                   'attempt to expand the orbit over B-spline basis' )
                     RETURN
                END IF
           END IF
      END IF
!
! --- Deallocate dynamic memory
!
      DEALLOCATE ( TARG_ARR )
      DEALLOCATE ( MOON_POT )
      DEALLOCATE ( RLT_ARR  )
      IF ( L_NOD < L_ARR ) THEN
           DEALLOCATE ( EQU_MAT  )
           DEALLOCATE ( NOR_VEC  )
           DEALLOCATE ( NOR_MAT  )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_OBJ_NZO  !#!#
