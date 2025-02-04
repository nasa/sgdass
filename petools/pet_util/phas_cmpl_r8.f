      FUNCTION   PHAS_CMPL_R8 ( CMPL_R8 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PHAS_CMPL_R8 computes the phase of the complex  *
! *   number CMPL_R8 in the range [-pi, +pi]                             *
! *                                                                      *
! * ### 01-FEB-2009  PHAS_CMPL_R8  v1.0 (c)  L. Petrov  01-FEB-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8      PI__NUM, PI2, P2I
      PARAMETER ( PI__NUM=3.141592653589793D0, PI2=2.0D0*PI__NUM, P2I=PI__NUM/2.0D0 ) !
      REAL*8     PHAS_CMPL_R8 
      COMPLEX*16 CMPL_R8
      REAL*8,    EXTERNAL :: ATAN_CS
      PHAS_CMPL_R8 = ATAN_CS ( REAL(CMPL_R8), IMAG(CMPL_R8) )
      IF ( PHAS_CMPL_R8 > PI__NUM ) PHAS_CMPL_R8 = PHAS_CMPL_R8 - PI2
      RETURN
      END  FUNCTION  PHAS_CMPL_R8  !#!  
