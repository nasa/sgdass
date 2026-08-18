      SUBROUTINE NERS_RADEC_COMP ( NERS, TIM_TAI, COO_TRS, AZ, EL, REFR_MODE, &
     &                             RA, DEC, RA_RATE, DEC_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_RADEC_COMP computes right ascension, declination      *
! *   and their time derivatives in the barycentric coordinate system    *
! *   at moment of time TIM_TAI for station with coordinates in the      *
! *   terrestrial coordinate system COO_TRS that observes a source with  *
! *   given azimuth and elevation. Both annual and diurnal aberration is *
! *   taken into account. Elevation is counted with respect to the       *
! *   normal to the reference ellipsoid. Vertical deflection is ignored. *
! *   Elevation may or may not account for the a contribution due        *
! *   refraction depending on parameter REFR_MOD.                        *
! *                                                                      *
! *   This routine can be used only for computation of right ascension   *
! *   and declination of an object beyond the Solar system, such as      *
! *   a star or a galaxy.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM_TAI   ( REAL*8    ) -- Time for which the Earth orientation      *
! *                            parameter(s) is to be computed elapsed    *
! *                            since 2000.01.01_00:00:00.0 TAI. Unit:    *
! *                            sec. If TIM_TAI .LE. -1.0D14, then the    *
! *                            right ascension and declination at the    *
! *                            current moment of time will be computed.  *
! * COO_TRS   ( REAL*8    ) -- Vector of station coordinates in the      *
! *                            rotating crust-fixed coordinate system.   *
! *                            Units: m.                                 *
! * AZ        ( REAL*8    ) -- Azimuth in rad.                           *
! * EL        ( REAL*8    ) -- Elevation in rad.                         *
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
! * RA        ( REAL*8    ) -- Source right ascension in the inertial    *
! *                            barycentric coordinate system. Units: rad.*
! * DEC       ( REAL*8    ) -- Source declination in the inertial        *
! *                            barycentric coordinate system. Units: rad.*
! * RA_RATE   ( REAL*8    ) -- Rate of change of right ascension in the  *
! *                            inertial barycentric coordinate system.   *
! *                            Units: rad/s.                             *
! * DEC_RATE  ( REAL*8    ) -- Rate of change of declination in the      *
! *                            inertial barycentric coordinate system.   *
! *                            Units: rad/s.                             *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 21-JUL-2025 NERS_RADEC_COMP v1.0 (d) L. Petrov  22-JUL-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM_TAI, COO_TRS(3), AZ, EL, RA, DEC, RA_RATE, DEC_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      INTEGER*4  MP
      PARAMETER  ( MP = 18 ) 
      CHARACTER  STR*128, STR1*128, STR2*128
      INTEGER*4  L_PAR, IK, J1, IER 
!
      REAL*8     MATR(3,3,3), LONG, PP, RAD, LAT_GCN, LAT_GDT, MU, HEI_ELL, &
     &           COO_CRS(3), UEN_TO_TRS(3,3), VEL_CRS(3), CF, SF, CL, SL, &
     &           VEL_EA(3), ACC_EA(3), SOU_UEN(3), SOU_TRS(3), SOU_CRS(3), &
     &           SOU_CRS_RATE(3), S_APP(3), S_APP_RATE(3), ACC_CRS(3), &
     &           SV, SA, VEL_BAR_CRS(3), ACC_BAR_CRS(3), S_APP_MAG, &
     &           SOU_CRS_APP_RATE(3), EL_USE, REFR_ANG
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
           CALL ERR_LOG ( 4451, IUER, 'NERS_RADEC_COMP', 'Wrong site '// &
    &          'refractivity mode: '//REFR_MODE//' while none, optic, '// &
    &          'or radio were expected' )
           RETURN
      END IF 
!
! --- Check whether input parameters are sane
!
      IF ( IS_R8_NAN(TIM_TAI) ) THEN
           CALL ERR_LOG ( 4452, IUER, 'NERS_RADEC_COMP', 'Argument TIM_TAI '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(1)) ) THEN
           CALL ERR_LOG ( 4453, IUER, 'NERS_RADEC_COMP', 'Argument COO_TRS(1) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(2)) ) THEN
           CALL ERR_LOG ( 4454, IUER, 'NERS_RADEC_COMP', 'Argument COO_TRS(2) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(COO_TRS(3)) ) THEN
           CALL ERR_LOG ( 4455, IUER, 'NERS_RADEC_COMP', 'Argument COO_TRS(3) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(AZ) ) THEN
           CALL ERR_LOG ( 4456, IUER, 'NERS_RADEC_COMP', 'Argument AZ '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(EL) ) THEN
           CALL ERR_LOG ( 4457, IUER, 'NERS_RADEC_COMP', 'Argument EL '// &
     &         'is not a number' )
           RETURN 
      END IF
!
      IF ( NERS%EPH_STATUS .NE. NERS__LOAD ) THEN
!
! -------- Read Earth ephemerides for accounting annual aberration.
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_READ_EARTH_EPHE ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4458, IUER, 'NERS_RADEC_COMP', 'Error in an '// &
     &              'attempt to read the Earth ephemerides file' )
                RETURN
           END IF
      END IF
!
! --- Get the knot of the ephemeride
!
      IK = IXMN8 ( NERS%EPH%L_TIM, NERS%EPH%TIM, TIM_TAI )
      IF ( IK < 1 ) THEN
           STR  = TIM_TO_DATE ( TIM_TAI, IER )
           STR1 = TIM_TO_DATE ( NERS%EPH%TIM(1), IER )
           STR2 = TIM_TO_DATE ( NERS%EPH%TIM(NERS%EPH%L_TIM), IER )
           CALL ERR_LOG ( 4459, IUER, 'NERS_RADEC_COMP', 'Trap of internal '// &
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
!
! --- Compute longitude
!
      LONG = ATAN_CS ( COO_TRS(1), COO_TRS(2)  )
      IF ( LONG .LT. 0.0D0 ) THEN
           LONG = PI2 + LONG
      END IF
!
! --- Compute geocentric latitude
!
      PP  = DSQRT ( COO_TRS(1)**2 + COO_TRS(2)**2 )
      IF ( DABS(PP) .LT. NERS__ANG_EPS ) PP = NERS__ANG_EPS 
      LAT_GCN = DATAN( COO_TRS(3)/PP )
      RAD = DSQRT ( COO_TRS(3)**2 + PP**2 )
      IF ( DABS ( RAD - NERS__REA ) .GT. NERS__HEIGHT_MIN .AND. &
           DABS ( RAD - NERS__REA ) .LT. NERS__HEIGHT_MAX       ) THEN
!
           CALL CHASHL ( STR  )
           WRITE ( UNIT=STR, FMT='(3(F15.3,2X))' ) COO_TRS
           CALL ERR_LOG ( 4460, IUER, 'NERS_RADEC_COMP', 'Wrong site '// &
     &                   'positions  -- '//TRIM(STR)//' -- they are '// &
     &                   'not on the surface of our planet' )
           RETURN
      END IF
!
! --- Compute geodetic latitude
!
      MU = DATAN ( COO_TRS(3)/PP * ( (1.D0 - NERS__FE) + NERS__EXC_SQ*NERS__REA/RAD ) )  
      LAT_GDT = DATAN( ( (1.D0 - NERS__FE)*COO_TRS(3) + &
     &                    NERS__EXC_SQ*NERS__REA*DSIN(MU)**3 ) / &
     &                   ( (1.D0 - NERS__FE)* &
     &                   ( PP  - NERS__EXC_SQ*NERS__REA*DCOS(MU)**3 )) )
!
! --- Computate the height of the station above the ellipsoid
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
! --- Get the matrix of the transformation from the terrestrical
! --- to the celestial coordinate system and its first and second
! --- time derivatives
!
      CALL ERR_PASS ( IUER, IER )       
      CALL NERS_GET_EOP ( NERS, TIM_TAI, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           STR = TIM_TO_DATE ( TIM_TAI, IER )
           CALL ERR_LOG ( 4461, IUER, 'NERS_AZEL_COMP', 'Error in '//&
     &                   'computing the Earth rotation matrtix on epoch '// &
     &                    STR )
           RETURN
      END IF
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
! --- Compute the refractivity angle
!
      IF ( REFR_MODE == NERS__REFR_OPTIC ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 2.909D-4/DTAN(EL_USE + 2.126D-3/(EL_USE + 0.0768))
         ELSE IF ( REFR_MODE == NERS__REFR_RADIO ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 3.13D-4/DTAN(EL_USE)
         ELSE 
           REFR_ANG = 0.0D0
      END IF
!
! --- Compute the station position in the local topocentric coordinate system
!
      SOU_UEN(3) = DCOS(AZ)*DCOS(EL - REFR_ANG)
      SOU_UEN(2) = DSIN(AZ)*DCOS(EL - REFR_ANG)
      SOU_UEN(1) = DSIN(EL-REFR_ANG)
!
! --- Transform the source positoin first to the non-rotating terrstrial 
! --- coordinate suystem then the the celestiacla coordinate system.
! --- Get also the first derivative
!
      CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS,  3, SOU_UEN, 3, SOU_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), 3, SOU_TRS, 3, S_APP, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), 3, SOU_TRS, 3, S_APP_RATE, IER )
!
! --- Compute S_APP -- the apparent source vector affected by the 
! --- annual and diurnal aberration
!
!
! --- Now get true source position with the contribition from the 
! --- aberration removed
!
      CALL ADDC_VV  ( 3, 1.D0, VEL_EA, 1.D0, VEL_CRS, VEL_BAR_CRS )
      CALL ADDC_VV  ( 3, 1.D0, ACC_EA, 1.D0, ACC_CRS, ACC_BAR_CRS )
      SV = DP_VV_V  ( 3, S_APP, VEL_BAR_CRS )
      CALL ADDC_VV  ( 3, 1.0D0/(1.0D0 - SV/NERS__C), S_APP, -1.0D0/(NERS__C*(1.0D0 - SV/NERS__C)), VEL_BAR_CRS, SOU_CRS )
      CALL NORM_VEC ( 3, SOU_CRS, S_APP_MAG )
      SA = DP_VV_V  ( 3, SOU_CRS, ACC_BAR_CRS )
      CALL ADDC_VV  ( 3, 1.0D0/(1.0D0 - SV/NERS__C), S_APP_RATE, -1.0D0/(NERS__C*(1.0D0 - SV/NERS__C)), ACC_BAR_CRS, SOU_CRS_RATE )
      CALL ADDC_VV  ( 3, 1.D0, SOU_CRS_RATE, ( DP_VV_V ( 3, SOU_CRS_RATE, VEL_BAR_CRS )/NERS__C + &
     &                                         DP_VV_V ( 3, SOU_CRS,      ACC_BAR_CRS )/NERS__C   ), S_APP, SOU_CRS_RATE )
!
! --- Convert SOU_CRS to the spherical coordintate system and 
! --- get time derivatives of right ascension and declination
!
      DEC = DASIN ( SOU_CRS(3) )
      IF ( DEC > P2I - NERS__ANG_EPS ) THEN
!
! -------- Declination is very close to the North Pole
!
           RA = ATAN_CS ( SOU_CRS(1)/DCOS(P2I-NERS__ANG_EPS), SOU_CRS(2)/DCOS(P2I-NERS__ANG_EPS) )
           DEC_RATE = SOU_CRS_RATE(3)/DCOS(P2I-NERS__ANG_EPS)
           RA_RATE  = (SOU_CRS_RATE(2)*DCOS(RA) - SOU_CRS_RATE(1)*DSIN(RA))/DCOS(P2I-NERS__ANG_EPS)
        ELSE IF ( DEC < -P2I + NERS__ANG_EPS ) THEN
!
! -------- Declination is very close to the South Pole
!
           RA = ATAN_CS ( SOU_CRS(1)/DCOS(-P2I+NERS__ANG_EPS), SOU_CRS(2)/DCOS(-P2I+NERS__ANG_EPS) )
           DEC_RATE = SOU_CRS_RATE(3)/DCOS(-P2I+NERS__ANG_EPS)
           RA_RATE  = (SOU_CRS_RATE(2)*DCOS(RA) - SOU_CRS_RATE(1)*DSIN(RA))/DCOS(-P2I+NERS__ANG_EPS)
        ELSE
!
! -------- Normal declination
!
           RA = ATAN_CS ( SOU_CRS(1)/DCOS(DEC), SOU_CRS(2)/DCOS(DEC) )
           DEC_RATE =  SOU_CRS_RATE(3)/DCOS(DEC)
           RA_RATE  = (SOU_CRS_RATE(2)*DCOS(RA) - SOU_CRS_RATE(1)*DSIN(RA) )/DCOS(DEC)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE NERS_RADEC_COMP  !#!  
