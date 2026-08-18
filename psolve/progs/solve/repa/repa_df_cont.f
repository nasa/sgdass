      FUNCTION   REPA_DF_CONT ()
! ************************************************************************
! *                                                                      *
! *   The auxiliary function REPA_DF_CONT does nothing.                  *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 13-DEC-2004  REPA_DF_CONT  v1.0 (d) L. Petrov  13-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INTEGER*4  REPA_DF_CONT
      REPA_DF_CONT = DIAGI__CONT   
      RETURN
      END  FUNCTION  REPA_DF_CONT
