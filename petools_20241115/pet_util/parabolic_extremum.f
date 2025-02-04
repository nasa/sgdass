      SUBROUTINE PARABOLIC_EXTREMUM ( X, Y, X_EXTR, Y_EXTR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PARABOLIC_EXTREMUM fits the parabolic line into *
! *   array 3 point long with argumetns X and values Y and fits the      *
! *   extremum of the parabolic fit.                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      X ( REAL*8     ) --  Array of arguments. Dimension: 3.          *
! *      Y ( REAL*8     ) --  Array of values. Dimension: 3.             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * X_EXTR ( REAL*8     ) -- Argument of the extremum.                   *
! * Y_EXTR ( REAL*8     ) -- Value of the extremum.                      *
! *                                                                      *
! * ### 26-JAN-2017 PARABOLIC_EXTREMUM v1.0 (c) L. Petrov 26-JAN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     X(3), Y(3), X_EXTR, Y_EXTR
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-30 )
      REAL*8     A, C, X0, Y0
!
      IF ( DABS(Y(2) - Y(3)) > EPS ) THEN
           C  = (Y(1) - Y(2))/(Y(2) - Y(3))
           X0 = (-X(1)**2 + X(2)**2 + C*(X(2)**2 - X(3)**2))/ &
     &          (2.0*(-X(1) + X(2) + C*X(2) - C*X(3)))
           A  = (Y(1)-Y(2))/((X(1) - X0)**2 - (X(2) - X0)**2)
           Y0 = Y(1) - A*(X(1) - X0)**2
         ELSE IF ( DABS(Y(1) - Y(3)) > EPS ) THEN
           C  = (Y(2) - Y(1))/(Y(1) - Y(3))
           X0 = (-X(2)**2 + X(1)**2 + C*(X(1)**2 - X(3)**2))/ &
     &          (2.0*(-X(2) + X(1) + C*X(1) - C*X(3)))
           A  = (Y(2)-Y(1))/((X(2) - X0)**2 - (X(1) - X0)**2)
           Y0 = Y(2) - A*(X(2) - X0)**2
         ELSE 
           X0 = X(2)
           Y0 = Y(2)
      END IF
      X_EXTR = X0
      Y_EXTR = Y0
!
      RETURN
      END  SUBROUTINE  PARABOLIC_EXTREMUM  !#!  
