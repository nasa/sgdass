      FUNCTION SPD_CLIENT_VERSION()
! ************************************************************************
! *                                                                      *
! *   Function SPD_CLIENT_VERSION returns the version date.              *
! *                                                                      *
! * ## 16-SEP-2014 SPD_CLIENT_VERSION v1.0 (c)  L. Petrov 16-SEP-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  SPD_CLIENT_VERSION*8
      CHARACTER  SPD_CLIENT_VERSION_STR*8
      PARAMETER  ( SPD_CLIENT_VERSION_STR = '20200501' )
      SPD_CLIENT_VERSION = SPD_CLIENT_VERSION_STR
      RETURN
      END  FUNCTION SPD_CLIENT_VERSION  !#!#
