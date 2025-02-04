      SUBROUTINE ERROR_ELLIPSE ( SIG1, SIG2, CORR, SIG_MAJ, SIG_MIN, TETA )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ERROR_ELLIPSE computes parameters of the error        *
! *   ellipse: major semi-axis, minor semi-axis, position angle using    *
! *   given standard deviations of two estimates and their correlation.  *
! *   Position angle is defined as a counter-clockwise rotation from     *
! *   axis along SIG1 to the major semi-axis of the ellipse. Position    *
! *   angle is defined in the range [-pi, pi].                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    SIG1 ( REAL*8     ) -- First standard deviation.                  *
! *    SIG2 ( REAL*8     ) -- Second standard deviation.                 *
! *    CORR ( REAL*8     ) -- Correlation coefficient.                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * SIG_MAJ ( REAL*8     ) -- Semi-major axis of the error ellipse.      *
! * SIG_MIN ( REAL*8     ) -- Semi-minor axis of the error ellipse.      *
! *    TETA ( REAL*8     ) -- Position angle of the  semi-major axis.    *
! *                           Counted from the axis along SIG1 in        *
! *                           counter clock-wise direction towards the   *
! *                           major semi-axis.                           *
! *                                                                      *
! * ### 08-AUG-2001  ERROR_ELLIPSE  v2.0 (c) L. Petrov  18-JUL-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     SIG1, SIG2, CORR, SIG_MAJ, SIG_MIN, TETA
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-32 )
!
      IF ( DABS(SIG1**2 - SIG2**2) .LT. EPS ) THEN
           TETA = 0.0
           SIG_MAJ = MAX ( SIG1, SIG2 )
           SIG_MIN = MIN ( SIG1, SIG2 )
         ELSE
!!!
!           teta = atan ( corr*sig1*sig2/(sig1**2 - sig2**2) ) / 2.0d0
!           sig_maj = dsqrt ( ( sig1**2 + sig2**2 + &
!     &         dsqrt ( (sig1**2 - sig2**2)**2 + (corr*sig1*sig2)**2 ) )/2.0d0 )
!           sig_min = dsqrt ( ( sig1**2 + sig2**2 - &
!     &         dsqrt ( (sig1**2 - sig2**2)**2 + (corr*sig1*sig2)**2 ) )/2.0d0 )
!!!
           TETA = ATAN ( 2.0D0*CORR*SIG1*SIG2/(SIG1**2 - SIG2**2) ) / 2.0D0
           SIG_MAJ = DSQRT ( ( SIG1**2 + SIG2**2 + &
     &         DSQRT ( (SIG1**2 - SIG2**2)**2 + 4.0D0*(CORR*SIG1*SIG2)**2 ) )/2.0D0 )
           SIG_MIN = DSQRT ( ( SIG1**2 + SIG2**2 - &
     &         DSQRT ( (SIG1**2 - SIG2**2)**2 + 4.0D0*(CORR*SIG1*SIG2)**2 ) )/2.0D0 )
      ENDIF
!
      RETURN
      END  SUBROUTINE  ERROR_ELLIPSE  !#!#
