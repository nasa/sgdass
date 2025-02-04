      SUBROUTINE NERS_AZELHA_COMP ( NERS, TIM_TAI, COO_TRS, RA, DEC, &
     &                              REFR_MODE, AZ, EL, HA, &
     &                              AZ_RATE, EL_RATE, HA_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_AZELHA_COMP computes azimuth, elevation, hour angle, *
! *   and their time derivatives at moment of time TIM_TAI for station   *
! *   with coordinates in the terrestrial coordinate system COO_TRS that *
! *   observes a source with coordinates in the barycentric celestial    *
! *   coordinate system RA, DEC. both annual and diurnal aberration is   *
! *   taken into account. Elevation is computed with respect to the      *
! *   normal to the reference ellipsoid. Vertical deflection is ignored. *
! *   Optionally the elevation can be corrected for refraction.          *
! *                                                                      *
! *   This routine can be used only for computation of azimuth and       *
! *   elevations of an object beyond the Solar system, such a as a star  *
! *   or a galaxy.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM_TAI   ( REAL*8    ) -- Time for which the Earth orientation      *
! *                            parameter(s) is to be computed elapsed    *
! *                            since 2000.01.01_00:00:00.0 TAI. Unit:    *
! *                            sec. If TIM_TAI .LE. -1.0D14, then the    *
! *                            azimuth and elevation at the current      *
! *                            moment of time will be computed.          *
! * COO_TRS   ( REAL*8    ) -- Vector of station coordinates in the      *
! *                            rotating crust-fixed coordinate system.   *
! *                            Units: m.                                 *
! * RA        ( REAL*8    ) -- Source right ascension in the inertial    *
! *                            barycenter coordinate system. Units: rad. *
! * DEC       ( REAL*8    ) -- Source declination in the inertial        *
! *                            barycenter coordinate system. Units: rad. *
! * REFR_MODE ( CHARACTER ) -- Refraction mode. Supported values:        *
! *                            none  -- refractivity is not accounted.   *
! *                            optic -- formula Bennett for optic range  *
! *                                     is used ( Bennett, G.G. (1982).  *
! *                                     "The Calculation of Astronomical *
! *                                     Refraction in Marine Navigation".*
! *                                     Journal of Navigation, 35(2),    *
! *                                     255-259.                         *
! *                            radio -- 3.13D-4/tg(el) expression        *
! *                                     suitable for radio waves is used.*
! *                            both formulae have a floor of 3 deg, i.e. *
! *                            refraction at elevations below 3 deg is   *
! *                            considered to be equal to refraction at   *
! *                            3 deg.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * AZ        ( REAL*8    ) -- Azimuth in rad.                           *
! * EL        ( REAL*8    ) -- Elevation in rad.                         *
! * HA        ( REAL*8    ) -- Hour angle in rad.                        *
! * AZ_RATE   ( REAL*8    ) -- Time derivative of azimuth   in rad/s.    *
! * EL_RATE   ( REAL*8    ) -- Time derivative of elevation in rad/s.    *
! * HA_RATE   ( REAL*8    ) -- Time derivative of hour angle in rad/s.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 06-JUN-2018 NERS_AZEL_COMP  v2.1 (c)  L. Petrov  03-JUL-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      REAL*8     TIM_TAI, COO_TRS(3), RA, DEC, AZ, EL, HA, &
     &           AZ_RATE, EL_RATE, HA_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MP
      PARAMETER  ( MP = 18 ) 
      CHARACTER  STR*128, STR1*128, STR2*128
      INTEGER*4  L_PAR, IK, J1, IER 
      REAL*8     MATR(3,3,3), LONG, PP, RAD, LAT_GCN, LAT_GDT, MU, HEI_ELL, &
     &           COO_CRS(3), UEN_TO_TRS(3,3), UP_UEN(3), UP_TRS(3), UP_CRS(3), &
     &           UP_CRS_RATE(3), NORTH_TRS(3), NORTH_CRS(3), NORTH_CRS_RATE(3), &
     &           VEL_CRS(3), VEC_PROJ_EN(3), VAL, VEC1(3), VEC2(3), &
     &           EAST_CRS(3), EAST_CRS_RATE(3), N_PROJ, E_PROJ, N_PROJ_RATE, &
     &           E_PROJ_RATE, CF, SF, CL, SL, NORTH_UEN(3), VEC_PE_RATE(3), &
     &           VEL_EA(3), ACC_EA(3), SOU_CRS(3), S_APP(3), S_APP_RATE(3), &
     &           ACC_CRS(3), SV, SA, VEL_BAR_CRS(3), ACC_BAR_CRS(3), S_APP_MAG, &
     &           EL_USE, REFR_ANG
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      REAL*8,    EXTERNAL :: ATAN_CS, DP_VV_V, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: IXMN8 
!
      IF ( REFR_MODE == NERS__REFR_NONE ) THEN
           CONTINUE 
         ELSE IF ( REFR_MODE == NERS__REFR_OPTIC ) THEN
           CONTINUE 
         ELSE IF ( REFR_MODE == NERS__REFR_RADIO ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 4381, IUER, 'NERS_AZEL_COMP', 'Wrong site '// &
    &          'refractivity mode: '//REFR_MODE//' while none, optic, '// &
    &          'or radio were expected' )
           RETURN
      END IF 
      IF ( IS_R8_NAN(TIM_TAI) ) THEN
           CALL ERR_LOG ( 4382, IUER, 'NERS_AZEL_COMP', 'Argument TIM_TAI '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(1)) ) THEN
           CALL ERR_LOG ( 4383, IUER, 'NERS_AZEL_COMP', 'Argument COO_TRS(1) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(2)) ) THEN
           CALL ERR_LOG ( 4384, IUER, 'NERS_AZEL_COMP', 'Argument COO_TRS(2) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(3)) ) THEN
           CALL ERR_LOG ( 4385, IUER, 'NERS_AZEL_COMP', 'Argument COO_TRS(3) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(RA) ) THEN
           CALL ERR_LOG ( 4386, IUER, 'NERS_AZEL_COMP', 'Argument RA '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(DEC) ) THEN
           CALL ERR_LOG ( 4387, IUER, 'NERS_AZEL_COMP', 'Argument DEC '// &
     &         'is not a number' )
           RETURN 
      END IF
!
      IF ( NERS%EPH_STATUS .NE. NERS__LOAD ) THEN
!
! -------- Read Earth ephemerides for accounting anual aberration.
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_READ_EARTH_EPHE ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4388, IUER, 'NERS_AZEL_COMP', 'Error in an '// &
     &              'attempt to read the Earth ephemerides file' )
                RETURN
           END IF
      END IF
!
      IK = IXMN8 ( NERS%EPH%L_TIM, NERS%EPH%TIM, TIM_TAI )
      IF ( IK < 1 ) THEN
           STR  = TIM_TO_DATE ( TIM_TAI, IER )
           STR1 = TIM_TO_DATE ( NERS%EPH%TIM(1), IER )
           STR2 = TIM_TO_DATE ( NERS%EPH%TIM(NERS%EPH%L_TIM), IER )
           CALL ERR_LOG ( 4389, IUER, 'NERS_AZEL_COMP', 'Trap of internal '// &
     &         'control: time epoch TIM_TAI '//TRIM(STR)//' is out of '// &
     &         'range for the Earth ephemeride: [ '//TRIM(STR1)//' , '// &
     &          TRIM(STR2)//' ]' )
           RETURN
      END IF
!
! --- Compute vector of the geocenter velocity and acceleration in the barycentric
! --- coordinate system
!
      DO 410 J1=1,3
         VEL_EA(J1) = DSPL8  ( TIM_TAI, NERS%EPH%L_TIM, NERS%EPH%TIM, &
     &                         NERS%EPH%COO_EARTH_VAL(1,J1), IK, NERS%EPH%COO_EARTH_SPL(1,J1) )
         ACC_EA(J1) = D2SPL8 ( TIM_TAI, NERS%EPH%L_TIM, NERS%EPH%TIM, &
     &                         NERS%EPH%COO_EARTH_VAL(1,J1), IK, NERS%EPH%COO_EARTH_SPL(1,J1) )
 410  CONTINUE 
!%   vel_ea = 0.0 ! %%%%%%%%
!%   acc_ea = 0.0 ! %%%%%%%%
!
      SOU_CRS(1) = DCOS(DEC)*DCOS(RA)
      SOU_CRS(2) = DCOS(DEC)*DSIN(RA)
      SOU_CRS(3) = DSIN(DEC)
!
! --- Compute longitude
!
      LONG = ATAN_CS ( COO_TRS(1), COO_TRS(2)  )
      IF ( LONG .LT. 0.0D0 ) THEN
           LONG = PI2 + LONG
      END IF
!
! --- Compute geodetic latitude
!
      PP  = DSQRT ( COO_TRS(1)**2 + COO_TRS(2)**2 )
      IF ( DABS(PP) .LT. NERS__ANG_EPS ) PP = NERS__ANG_EPS 
      RAD = DSQRT ( COO_TRS(3)**2 + PP**2 )
      IF ( DABS ( RAD - NERS__REA ) .GT. NERS__HEIGHT_MIN .AND. &
           DABS ( RAD - NERS__REA ) .LT. NERS__HEIGHT_MAX       ) THEN
!
           CALL CHASHL ( STR  )
           WRITE ( UNIT=STR, FMT='(3(F15.3,2X))' ) COO_TRS
           CALL ERR_LOG ( 4390, IUER, 'NERS_AZEL_COMP', 'Wrong site '// &
     &                   'positions  -- '//TRIM(STR)//' -- they are '// &
     &                   'not on the surface of our planet' )
           RETURN
      END IF
!
      LAT_GCN = DATAN( COO_TRS(3)/PP )
!
! --- Computation of geodetic latitude
!
      MU = DATAN ( COO_TRS(3)/PP * ( (1.D0 - NERS__FE) + NERS__EXC_SQ*NERS__REA/RAD ) )  
!
      LAT_GDT = DATAN( ( (1.D0 - NERS__FE)*COO_TRS(3) + &
     &                    NERS__EXC_SQ*NERS__REA*DSIN(MU)**3 ) / &
     &                   ( (1.D0 - NERS__FE)* &
     &                   ( PP  - NERS__EXC_SQ*NERS__REA*DCOS(MU)**3 )) )
!
! ---- Computation of height above the ellipsoid
!
      HEI_ELL =   PP*DCOS(LAT_GDT)         &
     &          + COO_TRS(3)*DSIN(LAT_GDT) &
     &          - NERS__REA* DSQRT( 1.D0 - NERS__EXC_SQ*DSIN(LAT_GDT)**2 )
!
! --- Calculation matrix of transformation from UEN (local topocentric,
! --- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
      CF = DCOS(LAT_GDT)
      SF = DSIN(LAT_GDT)
      CL = DCOS(LONG)
      SL = DSIN(LONG)
!
      UEN_TO_TRS(1,1) = CF*CL
      UEN_TO_TRS(2,1) = CF*SL
      UEN_TO_TRS(3,1) = SF
!
      UEN_TO_TRS(1,2) = -SL
      UEN_TO_TRS(2,2) =  CL
      UEN_TO_TRS(3,2) =  0.D0
!
      UEN_TO_TRS(1,3) = -SF*CL
      UEN_TO_TRS(2,3) = -SF*SL
      UEN_TO_TRS(3,3) =  CF
!
      CALL ERR_PASS ( IUER, IER )       
      CALL NERS_GET_EOP ( NERS, TIM_TAI, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           STR = TIM_TO_DATE ( TIM_TAI, IER )
           CALL ERR_LOG ( 4391, IUER, 'NERS_AZEL_COMP', 'Error in '//&
     &                   'computing the Earth rotation matrtix on epoch '// &
     &                    STR )
           RETURN
      END IF
!
! --- Compute the vector of local zenith in CRS and then
! --- rate of its change
!
      UP_UEN(1) = 1.0D0
      UP_UEN(2) = 0.0D0
      UP_UEN(3) = 0.0D0
      CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS,  3, UP_UEN, 3, UP_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), 3, UP_TRS, 3, UP_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), 3, UP_TRS, 3, UP_CRS_RATE, IER )
!
      NORTH_UEN(1) = 0.0D0
      NORTH_UEN(2) = 0.0D0
      NORTH_UEN(3) = 1.0D0
      CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS,  3, NORTH_UEN, 3, NORTH_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), 3, NORTH_TRS, 3, NORTH_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), 3, NORTH_TRS, 3, NORTH_CRS_RATE, IER )
!
! --- Compute velocity of the station
! --- First: TRS_TO_CRS' * R_VEC
!
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, COO_TRS, &
     &                      3, COO_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), &
     &                      3, COO_TRS, &
     &                      3, VEL_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,3), &
     &                      3, COO_TRS, &
     &                      3, ACC_CRS, IER )
!
! --- Compute S_APP -- source vector corrected for annual and diurnal aberration
!
      CALL ADDC_VV  ( 3, 1.D0, VEL_EA, 1.D0, VEL_CRS, VEL_BAR_CRS )
      SV = DP_VV_V  ( 3, SOU_CRS, VEL_BAR_CRS )
      CALL ADDC_VV  ( 3, 1.0D0/NERS__C, VEL_BAR_CRS, &
     &                   (1.0D0 - SV/NERS__C), SOU_CRS, S_APP )
      CALL NORM_VEC ( 3, S_APP, S_APP_MAG )
!
! --- Compute geometric elevation angle (no refraction, just aberration)
!
      EL = DASIN ( DP_VV_V ( 3, UP_CRS, S_APP ) )
!
! ----------- ... and its rate
!
      CALL ADDC_VV ( 3, 1.D0, ACC_EA, 1.D0, ACC_CRS, ACC_BAR_CRS )
      SA = DP_VV_V ( 3, SOU_CRS, ACC_BAR_CRS )
      CALL ADDC_VV ( 3, 1.0D0/NERS__C/S_APP_MAG, &
     &                  ACC_BAR_CRS, -SA/NERS__C/S_APP_MAG, &
     &                  SOU_CRS, S_APP_RATE )
      EL_RATE =  ( DP_VV_V( 3, UP_CRS_RATE, S_APP) + DP_VV_V( 3, UP_CRS, S_APP_RATE) )/ &
     &             DSQRT ( 1.0D0 - DP_VV_V ( 3, UP_CRS, S_APP )**2 )
!
! --- Now let us compute azimuth. In order to do it, first compute
! --- the projection of the source vector to the horizontal plane
!
      CALL ADDC_VV  ( 3, 1.0D0, S_APP, -DP_VV_V( 3, UP_CRS, S_APP ), &
     &                UP_CRS, VEC_PROJ_EN )
      CALL NORM_VEC ( 3, VEC_PROJ_EN, VAL )
      VEC_PE_RATE =    S_APP_RATE &
     &               - DP_VV_V ( 3, S_APP,      UP_CRS      )*UP_CRS_RATE &
     &               - DP_VV_V ( 3, S_APP_RATE, UP_CRS      )*UP_CRS &
     &               - DP_VV_V ( 3, S_APP,      UP_CRS_RATE )*UP_CRS
      VEC_PE_RATE = (  VEC_PE_RATE &
     &               - DP_VV_V ( 3, VEC_PROJ_EN, VEC_PE_RATE )* &
     &                 VEC_PROJ_EN )/VAL
!
! --- Then compute the north projection of that projection ...
!
      N_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS )
!
! --- ... and east projection of that projection
!
      CALL VM83 ( NORTH_CRS, UP_CRS, EAST_CRS )
      E_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS  )
!
! --- ... and its time derivative.
!
      N_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, NORTH_CRS ) + &
     &              DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS_RATE )
      CALL VM83 ( NORTH_CRS_RATE, UP_CRS, VEC1 )
      CALL VM83 ( NORTH_CRS, UP_CRS_RATE, VEC2 )
      EAST_CRS_RATE = VEC1 + VEC2
      E_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, EAST_CRS ) + &
     &              DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS_RATE )
!
! --- From these two projections we get the azimuth. Ugh!
!
      AZ = ATAN_CS ( N_PROJ, E_PROJ )
      AZ_RATE  = ( E_PROJ_RATE*N_PROJ - E_PROJ*N_PROJ_RATE   )/ &
     &           ( E_PROJ**2 + N_PROJ**2 )
!
      IF ( REFR_MODE == NERS__REFR_OPTIC ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 2.909D-4/DTAN(EL_USE + 2.126D-3/(EL_USE + 0.0768))
           EL = EL + REFR_ANG
         ELSE IF ( REFR_MODE == NERS__REFR_RADIO ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 3.13D-4/DTAN(EL_USE)
           EL = EL + REFR_ANG
      END IF
      HA = ATAN_CS ( COO_CRS(1), COO_CRS(2) ) - RA
      IF ( HA >  PI__NUM ) HA = HA - PI2
      IF ( HA < -PI__NUM ) HA = HA + PI2
      HA_RATE = ( COO_CRS(1)*VEL_CRS(2) - VEL_CRS(1)*COO_CRS(2) )/ &
     &          ( COO_CRS(1)**2 + COO_CRS(2)**2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE NERS_AZELHA_COMP  !#!#
