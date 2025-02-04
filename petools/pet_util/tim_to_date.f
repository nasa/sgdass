      FUNCTION   TIM_TO_DATE ( TIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TIM_TO_DATE  returns a string with date corresponds to    *
! *   time elapsed since 2000.01.01_00:00:00 in seconds.                 *
! *                                                                      *
! *  ### 04-MAY-2016  TIM_TO_DATE  v1.0 (c)  L. Petrov  04-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  TIM_TO_DATE*23
      REAL*8     TIM
      INTEGER*4  IUER
      REAL*8     SEC
      INTEGER*4  MJD
      INTEGER*4  J2000__MJD  
      PARAMETER  ( J2000__MJD =   51544     ) ! 2000.01.01_00:00:00
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      MJD = TIM/86400.0D0 + J2000__MJD
      SEC = TIM - (MJD - J2000__MJD)*86400.0D0
      TIM_TO_DATE = MJDSEC_TO_DATE ( MJD, SEC, IUER )
      RETURN 
      END  FUNCTION  TIM_TO_DATE  !#!  
