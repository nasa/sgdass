      FUNCTION   MJDSEC_TO_TIM ( MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Function  MJDSEC_TO_TIM  transforms the date specified by a pair   *
! *   of MJD and SEC to the amount of time in seconds elapsed from the   *
! *   fundamental epoch J2000: 01 January 2000, 12:00                    *
! *   
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *              MJD ( INTEGER*4 ) -- Integer MJD at the midnight of the *
! *                                   date under consideration.          *
! *              SEC ( REAL*8    ) -- Time in seconds from the midnight. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <MJDSEC_TO_TIM> ( REAL*8     ) -- amount of time in seconds elapsed  *
! *                                   from the fundamental epoch J2000:  *
! *                                   01 January 2000, 12:00.            *
! *                                                                      *
! * ### 16-NOV-2006  MJDSEC_TO_TIM   v2.0 (c)  L. Petrov 16-NOV-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     MJDSEC_TO_TIM
      INTEGER*4  MJD
      REAL*8     SEC, J2000__JD   
      INTEGER*4  IDAY
      INTEGER*4  J2000__MJD  
      PARAMETER  ( J2000__MJD =   51544     ) ! 2000.01.01_00:00:00
!
      MJDSEC_TO_TIM = (MJD - J2000__MJD)*86400.0D0 + (SEC - 43200.0D0)
!
      RETURN
      END  FUNCTION  MJDSEC_TO_TIM  !#!#
