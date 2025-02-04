      FUNCTION   PHAS_CMPL_R4 ( CMPL_R4 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PHAS_CMPL_R4 computes the phase of the complex  *
! *   number CMPL_R4 in the range [-pi, +pi]                             *
! *                                                                      *
! * ### 01-FEB-2009  PHAS_CMPL_R4  v1.0 (c)  L. Petrov  01-FEB-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8      PI__NUM, PI2, P2I
      PARAMETER ( PI__NUM=3.141592653589793D0, PI2=2.0D0*PI__NUM, P2I=PI__NUM/2.0D0 ) !
      REAL*4     PHAS_CMPL_R4 
      COMPLEX*8  CMPL_R4
      REAL*4,    EXTERNAL :: ATAN_CS_R4
      PHAS_CMPL_R4 = ATAN_CS_R4 ( REAL(CMPL_R4), IMAG(CMPL_R4) )
      IF ( PHAS_CMPL_R4 > PI__NUM ) PHAS_CMPL_R4 = PHAS_CMPL_R4 - PI2
      RETURN
      END  FUNCTION  PHAS_CMPL_R4  !#!  
