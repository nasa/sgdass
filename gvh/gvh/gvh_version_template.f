      SUBROUTINE GVH_VERSION ( INQ, ANS )
! ************************************************************************
! *                                                                      *
! *   Function GVH_VERSION returns the version or package label.         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 05-NOV-2020   GVH_VERSION   v1.0 (d)  L. Petrov 05-NOV-2020  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      CHARACTER  INQ*(*), ANS*(*)
      CHARACTER  GVH_VERSION_STR*8
      PARAMETER  ( GVH_VERSION_STR = '@GVH_VERSION_STR@' )
      IF ( INQ == 'GVH__LABEL' ) THEN
           ANS = GVH__LABEL 
         ELSE IF ( INQ == 'GVH_VERSION' ) THEN
           ANS = GVH_VERSION_STR  
         ELSE 
           ANS = 'Unknown inquire: '//INQ
      END IF

      RETURN
      END  SUBROUTINE  GVH_VERSION  !#!#
