      SUBROUTINE CHECK_SUPCAT ( SUPNAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Rouitine  CHECK_SUPCAT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 02-AUG-2000  CHECK_SUPCAT  1.0 (d)  L. Petrov  02-AUG-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SUPNAM*(*)
      INTEGER*4  IUER
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CHECK_SUPCAT  #!#
