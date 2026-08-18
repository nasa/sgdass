      SUBROUTINE SPD_CLIENT_VERSION ( INQ, ANS )
! ************************************************************************
! *                                                                      *
! *   Function SPD_CLIENT_VERSION returns the version date.              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 16-SEP-2014 SPD_CLIENT_VERSION v2.0 (d)  L. Petrov 05-NOV-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER  INQ*(*), ANS*(*)
      CHARACTER  SPD_CLIENT_VERSION_STR*8
      PARAMETER  ( SPD_CLIENT_VERSION_STR = '@SPD_CLIENT_VERSION_STR@' )
      IF ( INQ == 'SPD_CLIENT__LABEL' ) THEN
           ANS = SPD_CLIENT__LABEL 
         ELSE IF ( INQ == 'SPD_CLIENT_VERSION' ) THEN
           ANS = SPD_CLIENT_VERSION_STR
         ELSE 
           ANS = 'Unknown inquire: '//INQ
      END IF

      RETURN
      END  SUBROUTINE  SPD_CLIENT_VERSION  !#!#
