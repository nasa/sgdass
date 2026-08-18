      SUBROUTINE VCAT_RESOLVE_DBNAME ( VCAT, DB_NAME, M_NAME, L_NAME, C_NAME, &
     &                                 IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VCAT_RESOLVE_DBNAME returns
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 14-AUG-2003 VCAT_RESOLVE_DBNAME v1.0 (d) L. Petrov 14-AUG-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'vcat.i'
      TYPE     ( VCAT__STRU ) :: VCAT
      CHARACTER  DB_NAME*(*)
      INTEGER*4  M_NAME, L_NAME, IUER
      CHARACTER  C_NAME(M_NAME)*(*)
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VCAT_RESOLVE_DBNAME  #!#
