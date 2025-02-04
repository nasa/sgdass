      SUBROUTINE GET_ECLCOO ( ALPHA, DELTA, ECL_B, ECL_L )
! ************************************************************************
! *                                                                      *
! *   Rouine GET_ECLCOO computes galactic latitude ECL_B and galactic    *
! *   longitude CAL_L for given right ascension ALPHA and declination    *
! *   DELTA.                                                             *
! *                                                                      *
! *  ### 17-JAN-2007   GET_ECLCOO  v1.0 (c)  L. Petrov  17-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     ALPHA, DELTA, ECL_B, ECL_L
      REAL*8      PI, PI2, P2I, EPSILON_0 
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi number
      PARAMETER ( EPSILON_0 = 0.4090928041D0 )  ! rad
      REAL*8,     EXTERNAL :: ATAN_CS
!
      ECL_B = DASIN ( DSIN(DELTA)*DCOS(EPSILON_0) - &
     &                DSIN(ALPHA)*DCOS(DELTA)*DSIN(EPSILON_0) )
      ECL_L = ATAN_CS ( DCOS(DELTA)*DCOS(ALPHA), &
     &                  DSIN(DELTA)*DSIN(EPSILON_0) + &
     &                  DSIN(ALPHA)*DCOS(DELTA)*DCOS(EPSILON_0) )
      IF ( ECL_L .GE. PI2 ) ECL_L = ECL_L - PI2
!
      RETURN
      END  !#!  GET_ECLCOO  #!#
