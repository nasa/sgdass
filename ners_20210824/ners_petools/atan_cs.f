      FUNCTION ATAN_CS ( C, S )
! ************************************************************************
! *                                                                      *
! *   Function ATAN_CS  finds angle in the range [0, pi2) given sinus    *
! *   and cosinus of that angle.                                         *
! *                                                                      *
! *  ###  13-AUG-98     ATAN_CS    v1.0  (c)  L. Petrov  13-AUG-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8      ATAN_CS, C, S
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.0D0*PI, P2I=PI/2.0D0 ) !
      REAL*8      EPS
      PARAMETER ( EPS=1.D-30 )
!
      IF ( C .GT. EPS ) THEN
           ATAN_CS = DATAN2(S,C)
           IF ( S .LT. 0.0D0 ) ATAN_CS = ATAN_CS + PI2
         ELSE IF ( C .LT. -EPS ) THEN
           ATAN_CS = DATAN2(S,C)
           IF ( S .LT. 0.0D0 ) ATAN_CS = ATAN_CS + PI2
         ELSE ! -EPS < C < EPS
           ATAN_CS = P2I
           IF ( S .LT. 0.0D0 ) ATAN_CS = ATAN_CS + PI
      END IF
!
      RETURN
      END  FUNCTION  ATAN_CS  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION ATAN_CS_R4 ( C, S )
! ************************************************************************
! *                                                                      *
! *   Function ATAN_CS_R4  finds angle in the range [0, pi2) given sinus *
! *   and cosuins of that angle.                                         *
! *                                                                      *
! *  ###  13-AUG-98   ATAN_CS_R4    v1.0  (c)  L. Petrov  13-AUG-98  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*4      ATAN_CS_R4, C, S
      REAL*4      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793E0, PI2=2.0E0*PI, P2I=PI/2.0E0 ) !
      REAL*4      EPS
      PARAMETER ( EPS=1.E-30 )
!
      IF ( C .GT. EPS ) THEN
           ATAN_CS_R4 = ATAN2(S,C)
           IF ( S .LT. 0.0E0 ) ATAN_CS_R4 = ATAN_CS_R4 + PI2
         ELSE IF ( C .LT. -EPS ) THEN
           ATAN_CS_R4 = ATAN2(S,C)
           IF ( S .LT. 0.0E0 ) ATAN_CS_R4 = ATAN_CS_R4 + PI2
         ELSE ! -EPS < C < EPS
           ATAN_CS_R4 = P2I
           IF ( S .LT. 0.0E0 ) ATAN_CS_R4 = ATAN_CS_R4 + PI
      END IF
!
      RETURN
      END  FUNCTION  ATAN_CS_R4  !#!#
