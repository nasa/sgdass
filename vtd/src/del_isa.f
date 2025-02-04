      FUNCTION   DEL_ISA ( EL )
! ************************************************************************
! *                                                                      *
! *   Function DEL_ISA computes the path delay as a function of          *
! *   elevation for the ISA ISO standard atmosphere at the geoid at      *
! *   the place with the geodetic attitude 45 deg.                       *
! *                                                                      *
! *   Computation speed at Core Duo, 2.8 MHz: 110 ns.                    *
! *   Error of approximation is better than 8.d-6                        *
! *                                                                      *
! *  ### 14-SEP-2008    DEL_ISA    v1.0 (c)  L. Petrov  14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     EL
      REAL*8     DEL_ISA
      INTEGER*4  MD
      PARAMETER  ( MD = 12 ) 
      REAL*8     DEL_ZEN, E0, ARG_MIN, ARG_MAX, ISA_MAP_CHE(0:MD)
      PARAMETER  ( DEL_ZEN = 7.6707375D-09 ) 
      PARAMETER  ( E0 = -0.052D0 )
      PARAMETER  ( ARG_MIN =  1.0D0 ) 
      PARAMETER  ( ARG_MAX = 27.1711640769597D0 ) 
      DATA ISA_MAP_CHE   &
    &          / &
    &            2.3496276D+01,  &  !   0
    &            2.3756641D+01,  &  !   1
    &            2.8960727D-01,  &  !   2
    &           -8.8489567D-01,  &  !   3
    &            1.4212949D-01,  &  !   4
    &            3.4080806D-02,  &  !   5
    &           -2.1776292D-02,  &  !   6
    &            2.6966697D-03,  &  !   7
    &            1.3182015D-03,  &  !   8
    &           -9.2308499D-04,  &  !   9
    &            6.8354478D-05,  &  !  10
    &            1.3502649D-05,  &  !  11
    &           -1.7105554D-05   &  !  12
    &          /
      REAL*8   ARG_ISA
      REAL*8,  EXTERNAL :: CHEB_VAL
!
      ARG_ISA = 1.D0/DSIN( (1.D0 + E0/P2I)*EL - E0 )
      DEL_ISA = DEL_ZEN * CHEB_VAL ( MD, ARG_MIN, ARG_MAX, ARG_ISA, ISA_MAP_CHE )
      RETURN
      END  FUNCTION  DEL_ISA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DEL_ISA_DER ( EL )
! ************************************************************************
! *                                                                      *
! *   Function DEL_ISA_DER computes the derivative with respoect to EL   *
! *   of path delay as a function of elevation for the ISA ISO standard  *
! *   atmosphere at the geoid at the place with the geodetic attitude    *
! *   45 deg.                                                            *
! *                                                                      *
! *  ### 12-AUG-2009   DEL_ISA_DER   v1.0 (c) L. Petrov 14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     EL
      REAL*8     DEL_ISA_DER
      INTEGER*4  MD
      PARAMETER  ( MD = 12 ) 
      REAL*8     DEL_ZEN, E0, ARG_MIN, ARG_MAX, ISA_MAP_CHE(0:MD)
      PARAMETER  ( DEL_ZEN = 7.6707375D-09 ) 
      PARAMETER  ( E0 = -0.052D0 )
      PARAMETER  ( ARG_MIN =  1.0D0 ) 
      PARAMETER  ( ARG_MAX = 27.1711640769597D0 ) 
      DATA ISA_MAP_CHE   &
    &          / &
    &            2.3496276D+01,  &  !   0
    &            2.3756641D+01,  &  !   1
    &            2.8960727D-01,  &  !   2
    &           -8.8489567D-01,  &  !   3
    &            1.4212949D-01,  &  !   4
    &            3.4080806D-02,  &  !   5
    &           -2.1776292D-02,  &  !   6
    &            2.6966697D-03,  &  !   7
    &            1.3182015D-03,  &  !   8
    &           -9.2308499D-04,  &  !   9
    &            6.8354478D-05,  &  !  10
    &            1.3502649D-05,  &  !  11
    &           -1.7105554D-05   &  !  12
    &          /
      REAL*8   ARG_ISA, ARG_DER_ISA 
      REAL*8,  EXTERNAL :: CHEB_DER
!
      ARG_ISA = 1.D0/DSIN( (1.D0 + E0/P2I)*EL - E0 )
      ARG_DER_ISA = -(1.D0 + E0/P2I)* &
     &               DCOS( (1.D0 + E0/P2I)*EL - E0 )/ &
     &               DSIN( (1.D0 + E0/P2I)*EL - E0 )**2
      DEL_ISA_DER = DEL_ZEN * &
     &              CHEB_DER ( MD, ARG_MIN, ARG_MAX, ARG_ISA, ISA_MAP_CHE ) * &
     &              ARG_DER_ISA
      RETURN
      END  FUNCTION  DEL_ISA_DER  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION INV_MAP_ISA ( MAP )
! ************************************************************************
! *                                                                      *
! *   Finction INV_MAP_ISA computes the elevation engle for a given      *
! *   value of the ISA ISO mapping function.                             *
! *   Computation time: 160 nsec.                                        *
! *                                                                      *
! *   Error of approximation around zenith is better than 0.005 rad.     *
! *   Computation speed at Core Duo, 2.8 MHz: 110 ns.                    *
! *                                                                      *
! *  ### 14-SEP-2008  INV_MAP_ISA  v2.0 (c)  L. Petrov  12-AUG-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     MAP
      REAL*8     INV_MAP_ISA 
      INTEGER*4  MD
      PARAMETER  ( MD = 12 )
      REAL*8     E0, ISA_INV_MAP_CHE(0:MD), MAP_MIN, MAP_MAX, EPS
      PARAMETER  ( E0 = -0.052D0                 )
!!      PARAMETER  ( MAP_MIN =   1.0D0             )
      PARAMETER  ( MAP_MIN =   1.0D0 - 2.0D-16 )
      PARAMETER  ( MAP_MAX =  46.8152143128350D0 )
      PARAMETER  ( EPS     =  1.D-14 )
      DATA       ISA_INV_MAP_CHE &
    &     /    &
!
    &          1.433399D+01,  &  !  0
    &          1.255039D+01,  &  !  1
    &         -1.159927D-01,  &  !  2
    &          4.812282D-01,  &  !  3
    &         -1.142513D-01,  &  !  4
    &          4.717299D-02,  &  !  5
    &         -1.584444D-02,  &  !  6
    &          5.871918D-03,  &  !  7
    &         -1.949435D-03,  &  !  8
    &          7.900266D-04,  &  !  9
    &         -2.467537D-04,  &  ! 10
    &          9.892615D-05,  &  ! 11
    &         -1.280538D-04   &  ! 12
!
    &     /
      REAL*8, EXTERNAL :: CHEB_VAL
!
      IF ( MAP < MAP_MIN - EPS  .OR. &
     &     MAP > MAP_MAX + EPS       ) THEN
           INV_MAP_ISA = -1.D308
        ELSE IF ( DABS(MAP-MAP_MIN) < 1.D-6 ) THEN
           INV_MAP_ISA = P2I
        ELSE 
           INV_MAP_ISA = (DASIN ( 1.D0/CHEB_VAL ( MD, MAP_MIN, MAP_MAX, MAP, &
     &                            ISA_INV_MAP_CHE, -2 ) ) + E0)/ &
     &                   (1.0D0 + E0/P2I)
      END IF
      RETURN
      END  FUNCTION INV_MAP_ISA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DEL_ISA_OLD ( EL )
! ************************************************************************
! *                                                                      *
! *   Function DEL_ISA_OLD computes the path delay as a function of      *
! *   elevation for the ISA ISO standard atmosphere at the geoid at      *
! *   the place with the geodetic attitude 45 deg.                       *
! *                                                                      *
! *  ### 14-SEP-2008  DEL_ISA_OLD  v1.0 (c)  L. Petrov  14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EL
      REAL*8     DEL_ISA_OLD
      REAL*8     DEL_ZEN, A, B, C
!
      DEL_ZEN = 7.688244D-09
      A = 1.2273836D-03
      B = 2.4857151D-03
      C = 4.7857806D-02
!
      DEL_ISA_OLD = DEL_ZEN*( 1.D0 + A + B + C + A*C )/( 1.0D0 + B + C ) * &
     &                  ( DSIN(EL)**2 + C*DSIN(EL) + B )/ &
     &                  ( DSIN(EL)**3 + C*DSIN(EL)**2 + (A+B)*DSIN(EL) + A*C )
      RETURN
      END  FUNCTION  DEL_ISA_OLD  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   DEL_ISA_DER_OLD ( EL )
! ************************************************************************
! *                                                                      *
! *   Function DEL_ISA computes the derivative of the path delay with    *
! *   respect of elevation angle as a function of elevation for the      *
! *   ISA ISO standard atmosphere at the geoid at the place with         *
! *   the geodetic attitude 45 deg.                                      *
! *                                                                      *
! *  ### 14-SEP-2008  DEL_ISA_DER_OLD   v1.0 (c) L. Petrov  14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EL
      REAL*8     DEL_ISA_DER_OLD
      REAL*8     DEL_ZEN, A, B, C
!
      DEL_ZEN = 7.688244D-09
      A = 1.2273836D-03
      B = 2.4857151D-03
      C = 4.7857806D-02
!
      DEL_ISA_DER_OLD = DEL_ZEN * ( 1.D0 + A/(1.0D0 + B/(1.0D0 + C) ) )* &
     &              ( - DSIN(EL)**4 &
     &                - 2.0D0*C*DSIN(EL)**3 &
     &                + (A - 2.0D0*B - C**2)*DSIN(EL)**2 &
     &                + 2.0D0*(A-B)*C * DSIN(EL) &
     &                + A*C**2 - A*B - B**2 )/ &
     &              ( DSIN(EL)**3 + C*DSIN(EL)**2 + (A+B)*DSIN(EL) + A*C )**2* &
     &              DCOS(EL)
      RETURN
      END  FUNCTION  DEL_ISA_DER_OLD  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION INV_MAP_ISA_OLD ( MAP )
! ************************************************************************
! *                                                                      *
! *   Finction INV_MAP_ISA computes the elevation engle for a given      *
! *   value of the ISA ISO mapping function.                             *
! *   Computation time: 100 nsec.                                        *
! *                                                                      *
! *  ### 14-SEP-2008  INV_MAP_ISA  v1.0 (c)  L. Petrov  14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     MAP
      REAL*8     INV_MAP_ISA_OLD
      INTEGER*4  MD
      PARAMETER  ( MD = 12 )
      REAL*8     CHE(0:MD), MAP_MIN, MAP_MAX, EPS
      PARAMETER  ( MAP_MIN =   1.0D0             )
      PARAMETER  ( MAP_MAX =  25.3074549885792D0 )
      PARAMETER  ( EPS     =  1.D-14 )
      DATA CHE &
    &     /    &
    &       2.238156019895D+01, &   !  0
    &       2.637178441157D+01, &   !  1
    &       6.439028513384D+00, &   !  2
    &       1.713329428696D+00, &   !  3
    &       3.152893125481D-01, &   !  4
    &       6.031295779214D-02, &   !  5
    &       1.249351094806D-02, &   !  6
    &       3.754213487007D-03, &   !  7
    &       9.563743394633D-04, &   !  8
    &       1.596430646125D-04, &   !  9
    &       1.359534343137D-05, &   ! 10
    &       2.782504720522D-06, &   ! 11
    &       2.704591133051D-06  &   ! 12
    &     /
      REAL*8, EXTERNAL :: CHEB_VAL
!
      IF ( MAP < MAP_MIN - EPS  .OR. &
     &     MAP > MAP_MAX + EPS       ) THEN
           INV_MAP_ISA_OLD = -1.D308
        ELSE 
           INV_MAP_ISA_OLD = DASIN ( 1.D0/CHEB_VAL ( MD, MAP_MIN, MAP_MAX, MAP, CHE, -2 ) )
      END IF
      RETURN
      END  FUNCTION  INV_MAP_ISA_OLD  !#!#
