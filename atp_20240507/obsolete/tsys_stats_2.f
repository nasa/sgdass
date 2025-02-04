      SUBROUTINE  TSYS_AVER ( NP, TIM, TSYS, TIM_AVR, TSYS_AVR,         &
     &                        TSYS_STD, IUER )
!     
! ***************************************************************************
! *                                                                         *
! *   This routine computes the average and rms of a TSYS array             *
! *                                                                         *
! *   INPUT:                                                                *
! *         NP        =  No. of input points      { INT }                   *
! *                                                                         *
! *         TIM       =  Time array               { REAL }   [NPx1]         *
! *                                                                         *
! *         TSYS      =  Tsys array               { REAL }   [NPx1]         *
! *                                                                         *
! *         IUER      =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         TIM_AVR   =  Average Time             { REAL }                  *
! *                                                                         *
! *         TSYS_AVR  =  Tsys average             { REAL }                  *
! *                                                                         *
! *         TSYS_STD  =  Tsys standard deviation  { REAL }                  *
! *                                                                         *
! * ### 28-JUL-2022   TSYS_AVER   v1.0  (c)    L. Petrov   28-JUL-2022 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INTEGER*4  NP, KP, IUER
      REAL*8     TIM(NP), TSYS(NP), TIM_AVR, TSYS_AVR, TSYS_STD
      INTEGER*4  J1, J2, J3, J4, J5
!
! --- Initial values
!
      TIM_AVR  = 0.0D0
      TSYS_AVR = 0.0D0
      TSYS_STD = 0.0D0
!
! --- Sum up the values
!
      DO 410 J1 = 1, NP
! ------
         TIM_AVR  = TIM_AVR + TIM(J1)
         TSYS_AVR = TSYS_AVR + TSYS(J1)
 410  CONTINUE
!
! --- Compute the averages
!
      TIM_AVR   = TIM_AVR/NP
      TSYS_AVR  = TSYS_AVR/NP
!     
! --- Compute the squared differences
!
      DO 420 J1 = 1, NP
         TSYS_STD = TSYS_STD + ( TSYS(J1) - TSYS_AVR )**2.D0
 420  CONTINUE
!
      TSYS_STD = DSQRT( TSYS_STD/NP)
!
      RETURN
      END SUBROUTINE !#!
