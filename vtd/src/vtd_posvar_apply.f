      SUBROUTINE VTD_POSVAR_APPLY ( VTD, ISTA, I_PSV, PSV_REN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_POSVAR_APPLY 
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 29-JAN-2004  VTD_POSVAR_APPLY v1.0 (d) L. Petrov 29-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  ISTA, I_PSV, IUER
      REAL*8     PSV_REN(3)
!
      PSV_REN(1) = 0.0D0
      PSV_REN(2) = 0.0D0
      PSV_REN(3) = 0.0D0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_POSVAR_APPLY
