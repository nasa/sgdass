      SUBROUTINE TIM_TO_MJDSEC ( TIM, MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Routine  TIM_TO_MJDSEC Transforms the amount of time in seconds    *
! *   elapsed from the fundamental epoch J2000.0, 01 January 2000, 12:00 *
! *   to a pair MJD,SEC                                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      TIM ( REAL*8    ) -- amount of time in seconds elapsed from     *
! *                           the fundamental epoch J2000.0,             *
! *                           01 January 2000, 12:00 
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      MJD ( INTEGER*4 ) -- Modified Julian date: integer number of    *
! *                           days elapsed from 0 UT January 01, 2000    *
! *                           plus magical number 51544                  *
! *      SEC ( REAL*8    ) -- Time in seconds elapsed from midnight.     *
! *                                                                      *
! *  ###  16-NOV-2006 TIM_TO_MJDSEC  v1.0 (c) L. Petrov 16-NOV-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MJD 
      REAL*8     TIM, SEC
      INTEGER*4  J2000__MJD  
      PARAMETER  ( J2000__MJD =   51544     ) ! 2000.01.01_00:00:00
      INTEGER*4  IDAY_FROM_MJD0 
!
      IDAY_FROM_MJD0 = (TIM + 43200.0D0)/86400.0D0
      MJD = J2000__MJD + IDAY_FROM_MJD0 
      SEC = (TIM+43200.0D0) - IDAY_FROM_MJD0*86400.0D0
!
      IF ( SEC < 0.D0 ) THEN
           SEC = SEC + 86400.0D0
           MJD = MJD - 1
      END IF
!
      RETURN
      END  SUBROUTINE  TIM_TO_MJDSEC  !#!#
